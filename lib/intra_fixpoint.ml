open Core_kernel
open Helpers

module type TRANSFER = sig
  (** The state of the analysis *)
  type state

  (** States should be comparable *)
  val compare_state : state -> state -> int

  (** The initial state *)
  val init_state : Cfg.t -> state

  (** Convert a state to its textual representation *)
  val state_to_string : state -> string

  (** Joins two states of the analysis *)
  val join_state : state -> state -> state

  (** The result of applying the transfer function. *)
  type result =
    | Uninitialized (** Meaning it has not been computed yet *)
    | Simple of state (** A single successor *)
    | Branch of state * state (** Upon a `brif`, there are two successor states: one where the condition holds, and where where it does not hold. This is used to model that. *)
  [@@deriving compare]

  (** Transfer function for control instructions *)
  val control_instr_transfer : Wasm_module.t -> Cfg.t -> Instr.control Instr.labelled -> state -> result

  (** Transfer function for data instructions *)
  val data_instr_transfer : Wasm_module.t -> Cfg.t -> Instr.data Instr.labelled -> state -> state

  (** Merges flows for blocks that have multiple predecessors *)
  val merge_flows : Wasm_module.t -> Cfg.t -> Basic_block.t -> (int * state) list -> state
end

module Make = functor (Transfer : TRANSFER) -> struct

  let result_to_string (r : Transfer.result) : string = match r with
    | Uninitialized -> "uninitialized"
    | Simple st -> Transfer.state_to_string st
    | Branch (st1, st2) -> Printf.sprintf "branch:\n%s\n%s" (Transfer.state_to_string st1) (Transfer.state_to_string st2)

  let join_result (r1 : Transfer.result) (r2 : Transfer.result) =
    match (r1, r2) with
    | Uninitialized, _ -> r2
    | _, Uninitialized -> r1
    | Simple st1, Simple st2 -> Simple (Transfer.join_state st1 st2)
    | Branch (st1, st2), Branch (st1', st2') -> Branch (Transfer.join_state st1 st1', Transfer.join_state st2 st2')
    | _ -> failwith "Cannot join results"

  (** The results of an intra analysis are a mapping from indices (block or instructions) to their in and out values *)
  type intra_results = (Transfer.result * Transfer.result) IntMap.t

  (** Analyzes a CFG. Returns the final state after computing the transfer of the entire function. That final state is a pair where the first element are the results per block, and the second element are the results per instructions.
      @param module_ is the overall WebAssembly module, needed to access type information, tables, etc.
      @param cfg is the CFG to analyze *)
  let analyze (module_ : Wasm_module.t) (cfg : Cfg.t) : intra_results * intra_results =
    let bottom = Transfer.Uninitialized in
    let init = Transfer.init_state cfg in
    (* Data of the analysis, per block *)
    let block_data : intra_results ref = ref (IntMap.of_alist_exn (List.map (IntMap.keys cfg.basic_blocks)
                                                 ~f:(fun idx ->
                                                      (idx, (bottom, bottom))))) in
    (* Data of the analysis, per instruction *)
    let instr_data : intra_results ref = ref IntMap.empty (* initially empty *) in

    (* Applies the transfer function to an entire block *)
    let transfer (b : Basic_block.t) (state : Transfer.state) : Transfer.result =
      match b.content with
      | Data instrs ->
        Simple (List.fold_left instrs ~init:state ~f:(fun prestate instr ->
            let poststate = Transfer.data_instr_transfer module_ cfg instr prestate in
            instr_data := IntMap.set !instr_data ~key:instr.label ~data:(Simple prestate, Simple poststate);
            poststate))
      | Control instr ->
        let poststate = Transfer.control_instr_transfer module_ cfg instr state in
        instr_data := IntMap.set !instr_data ~key:instr.label ~data:(Simple state, poststate);
        poststate
      | Nothing -> Simple state
      | ControlMerge -> Simple state in

    (* Analyzes one block, returning the pre and post states *)
    let analyze_block (block_idx : int) : Transfer.state * Transfer.result =
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
          | Uninitialized, _ -> (idx, init))) in
      let in_state = Transfer.merge_flows module_ cfg block pred_states in
      (* We analyze it *)
      let result = transfer block in_state in
      Printf.printf "block: %s\n" (Basic_block.to_string block);
      Printf.printf "block %d results: %s\n" block_idx (result_to_string result);
      (in_state, result)
    in
    let rec fixpoint (worklist : IntSet.t) (iteration : int) : unit =
      if IntSet.is_empty worklist then
        () (* No more elements to consider. We can stop here *)
      else
        let block_idx = IntSet.min_elt_exn worklist in
        let (in_state, out_state) = analyze_block block_idx in
        (* Has out state changed? *)
        let previous_out_state = snd (IntMap.find_exn !block_data block_idx) in
        match previous_out_state with
        | st when Transfer.compare_result out_state st = 0 ->
          (* Didn't change, we can safely ignore the successors *)
          fixpoint (IntSet.remove worklist block_idx) (iteration+1)
        | _ ->
          (* Update the out state in the analysis results.
             We join with the previous results *)
          let new_out_state = join_result out_state previous_out_state in
          block_data := IntMap.set !block_data ~key:block_idx ~data:(Simple in_state, new_out_state);
          (* And recurse by adding all successors *)
          let successors = Cfg.successors cfg block_idx in
          fixpoint (IntSet.union (IntSet.remove worklist block_idx) (IntSet.of_list successors)) (iteration+1)
    in
    (* Performs narrowing by re-analyzing once each block *)
    let rec _narrow (blocks : int list) : unit = match blocks with
      | [] -> ()
      | block_idx :: blocks ->
        Printf.printf "narrowing block %d\n" block_idx;
        let (in_state, out_state) = analyze_block block_idx in
        block_data := IntMap.set !block_data ~key:block_idx ~data:(Simple in_state, out_state);
        _narrow blocks
    in
    fixpoint (IntSet.singleton cfg.entry_block) 1;
    (* _narrow (IntMap.keys cfg.basic_blocks); *)
    (!block_data, !instr_data)

  (* Extract the out state from intra-procedural results *)
  let out_state (cfg : Cfg.t) (results : intra_results * intra_results) : Transfer.state =
    match snd (IntMap.find_exn (fst results) cfg.exit_block) with
    | Simple s -> s
    | _ -> failwith "Multiple exits for function? This should not happen"
end
