open Core_kernel
open Helpers

(** The interface of an intra analysis *)
module type INTRA = sig
  (** The state of the analysis *)
  type state

  (** States should be comparable *)
  val equal_state : state -> state -> bool

  (** The annotations expected on the CFG for analysis *)
  type annot_expected

  (** Analyze a method (represented by its CFG) from the module *)
  val analyze : Wasm_module.t -> annot_expected Cfg.t -> state Cfg.t

(*  (** [TODO] the summary is a shorter representation of the analysis results for a function*)
  type summary
    val get_summary : state Cfg.t -> summary *)
end

module Make (Transfer : Transfer.TRANSFER) (* : INTRA *) = struct
  (* Include transfer to get a definition for state, equal_state, and annot_expected *)
  include Transfer

  (** The result of applying the transfer function. *)
  type result =
    | Uninitialized (** Meaning it has not been computed yet *)
    | Simple of state (** A single successor *)
    | Branch of state * state (** Upon a `brif`, there are two successor states: one where the condition holds, and where where it does not hold. This is used to model that. *)
  [@@deriving compare]

  (** The results of an intra analysis are a mapping from indices (block or instructions) to their in and out values *)
  type intra_results = (result * result) IntMap.t

  (** Converts a result to a state. May require joining output states in case of branching *)
  let result_to_state (cfg : annot_expected Cfg.t) (r : result) : state = match r with
    | Uninitialized -> bottom_state cfg
    | Simple s -> s
    | Branch (s1, s2) -> join_state s1 s2

  let result_to_string (r : result) : string = match r with
    | Uninitialized -> "uninit"
    | Simple s -> Printf.sprintf "simple: %s" (state_to_string s)
    | Branch (s1, s2) -> Printf.sprintf "branch: %s\nand: %s" (state_to_string s1) (state_to_string s2)

  (** Analyzes a CFG. Returns the final state after computing the transfer of the entire function. That final state is a pair where the first element are the results per block, and the second element are the results per instructions.
      @param module_ is the overall WebAssembly module, needed to access type information, tables, etc.
      @param cfg is the CFG to analyze *)
  let analyze (module_ : Wasm_module.t) (cfg : annot_expected Cfg.t) : state Cfg.t =
    let bottom = Uninitialized in
    (* Data of the analysis, per block *)
    let block_data : intra_results ref = ref (IntMap.of_alist_exn (List.map (IntMap.keys cfg.basic_blocks)
                                                 ~f:(fun idx ->
                                                      (idx, (bottom, bottom))))) in
    (* Data of the analysis, per instruction *)
    let instr_data : intra_results ref = ref IntMap.empty (* initially empty *) in

    (* Applies the transfer function to an entire block *)
    let transfer (b : 'a Basic_block.t) (state : Transfer.state) : result =
      match b.content with
      | Data instrs ->
        Simple (List.fold_left instrs ~init:state ~f:(fun prestate instr ->
            let poststate = Transfer.data_instr_transfer module_ cfg instr prestate in
            instr_data := IntMap.set !instr_data ~key:instr.label ~data:(Simple prestate, Simple poststate);
            poststate))
      | Control instr ->
        let poststate = match Transfer.control_instr_transfer module_ cfg instr state with
          | `Simple s -> Simple s
          | `Branch (s1, s2) -> Branch (s1, s2)
        in
        instr_data := IntMap.set !instr_data ~key:instr.label ~data:(Simple state, poststate);
        poststate
      | ControlMerge -> Simple state in

    (* Analyzes one block, returning the pre and post states *)
    let analyze_block (block_idx : int) : Transfer.state * result =
      (* Printf.printf "Analyzing block %d\n" block_idx; *)
      (* The block to analyze *)
      let block = Cfg.find_block_exn cfg block_idx in
      let predecessors = Cfg.predecessors cfg block_idx in
      (* in_state is the join of all the the out_state of the predecessors.
         Special case: if the out_state of a predecessor is not a simple one, that means we are the target of a break.
         If this is the case, we pick the right branch, according to the edge data *)
      let pred_states = (List.map predecessors ~f:(fun (idx, d) -> match (snd (IntMap.find_exn !block_data idx), d) with
          | Simple s, _ -> (idx, s)
          | Branch (t, _), Some true -> (idx, t)
          | Branch (_, f), Some false -> (idx, f)
          | Branch _, None -> failwith "invalid branch state"
          | Uninitialized, _ -> (idx, Transfer.bottom_state cfg))) in
      let in_state = Transfer.merge_flows module_ cfg block pred_states in
      (* We analyze it *)
      Printf.printf "state before analysis: %s\n" (state_to_string in_state);
      let result = transfer block in_state in
      (in_state, result)
    in
    let join_result  (r1 : result) (r2 : result) =
    match (r1, r2) with
    | Uninitialized, _ -> r2
    | _, Uninitialized -> r1
    | Simple st1, Simple st2 ->
      let joined = Transfer.join_state st1 st2 in
      Simple joined
    | Branch (st1, st2), Branch (st1', st2') ->
      Branch (Transfer.join_state st1 st1',
              Transfer.join_state st2 st2')
    | _ -> failwith "Cannot join results" in
    let widen_result (r1 : result) (r2 : result) =
      match (r1, r2) with
      | Uninitialized, _ -> r2
      | _, Uninitialized -> r1
      | Simple st1, Simple st2 ->
        Simple (Transfer.widen st1 st2)
      | Branch (st1, st2), Branch (st1', st2') ->
        Branch (Transfer.widen st1 st1',
                Transfer.widen st2 st2')
      | _ -> failwith "Cannot widen results" in
    let rec fixpoint (worklist : IntSet.t) (iteration : int) : unit =
      if IntSet.is_empty worklist then
        () (* No more elements to consider. We can stop here *)
      else
        let block_idx = IntSet.min_elt_exn worklist in
        Printf.printf "-----------------------\n Analyzing block %d\n" block_idx;
        let (in_state, out_state) = analyze_block block_idx in
        (* Has out state changed? *)
        let previous_out_state = snd (IntMap.find_exn !block_data block_idx) in
        match previous_out_state with
        | st when compare_result out_state st = 0 ->
          (* Didn't change, we can safely ignore the successors *)
          fixpoint (IntSet.remove worklist block_idx) (iteration+1)
        | _ ->
          (* Update the out state in the analysis results.
             We join with the previous results *)
          Printf.printf "joining states at block %d\n" block_idx;
          Printf.printf "previous state was: %s\n" (result_to_string previous_out_state);
          Printf.printf "current state is: %s\n" (result_to_string out_state);
          let new_out_state =
            if IntSet.mem cfg.loop_heads block_idx then
              widen_result previous_out_state (join_result previous_out_state out_state)
            else
              join_result previous_out_state out_state
          in
          Printf.printf "result: %s\n" (result_to_string new_out_state);
          block_data := IntMap.set !block_data ~key:block_idx ~data:(Simple in_state, new_out_state);
          (* And recurse by adding all successors *)
          let successors = Cfg.successors cfg block_idx in
          fixpoint (IntSet.union (IntSet.remove worklist block_idx) (IntSet.of_list successors)) (iteration+1)
    in
    (* Performs narrowing by re-analyzing once each block *)
    let rec _narrow (blocks : int list) : unit = match blocks with
      | [] -> ()
      | block_idx :: blocks ->
        let (in_state, out_state) = analyze_block block_idx in
        block_data := IntMap.set !block_data ~key:block_idx ~data:(Simple in_state, out_state);
        _narrow blocks
    in
    fixpoint (IntSet.singleton cfg.entry_block) 1;
    (* _narrow (IntMap.keys cfg.basic_blocks); *)
    Cfg.annotate cfg (IntMap.map !instr_data ~f:(fun (before, after) -> (result_to_state cfg before, result_to_state cfg after)))

(*  (** Extract the out state from intra-procedural results *)
  let out_state (cfg : 'a Cfg.t) (results : intra_results * intra_results) : Transfer.state =
    match snd (IntMap.find_exn (fst results) cfg.exit_block) with
    | Simple s -> s
    | _ -> failwith "Multiple exits for function? This should not happen"

  let extract_spec (results : intra_results) : (state * state) IntMap.t =
    IntMap.filter_map results ~f:(function
        | Uninitialized, _ -> None
        | Simple s, Simple s' -> Some (s, s')
        | Simple s, Branch (s1, s2) ->
          Some (s, join_state s1 s2)
        | _ -> failwith "invalid spec") *)
end
