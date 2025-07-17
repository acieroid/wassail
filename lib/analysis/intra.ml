open Core
open Helpers

let narrow_option = ref false

(** The interface of an intra analysis *)
module type INTRA = sig
  (** The state of the analysis *)
  type state

  (** The type of summaries used by the analysis (can be unit) *)
  type summary

  (** States should be comparable *)
  val equal_state : state -> state -> bool

  (** The annotations expected on the CFG for analysis *)
  type annot_expected

  (** Analyze a method (represented by its CFG) from the module *)
  val analyze : Wasm_module.t -> annot_expected Cfg.t -> summary Int32Map.t -> state Cfg.t * summary

  (** Similar to analyze, but keep previous annotations *)
  val analyze_keep : Wasm_module.t -> annot_expected Cfg.t -> summary Int32Map.t -> (annot_expected * state) Cfg.t * summary
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

  (** The results of an intra analysis are a mapping from instruction labels to their in and out values *)
  type intra_results = (result * result) Instr.Label.Map.t

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
  let analyze_ (module_ : Wasm_module.t) (cfg : annot_expected Cfg.t) (summaries : summary Int32Map.t) : intra_results =
    let bottom = Uninitialized in
    (* Data of the analysis, per block *)
    let block_data : (result * result) IntMap.t ref =
      ref (IntMap.of_alist_exn (List.map (IntSet.to_list (Cfg.all_block_indices cfg))
                                  ~f:(fun idx -> (idx, (bottom, bottom))))) in
    (* Data of the analysis, per instruction *)
    let instr_data : intra_results ref =
      ref (Instr.Label.Map.of_alist_exn (List.map (Instr.Label.Set.to_list (Cfg.all_instruction_labels cfg))
                                  ~f:(fun idx -> (idx, (bottom, bottom))))) in

    (* Applies the transfer function to an entire block *)
    let transfer (b : 'a Basic_block.t) (state : Transfer.state) : result =
      match b.content with
      | Data instrs ->
        Simple (List.fold_left instrs ~init:state ~f:(fun prestate instr ->
            let poststate = Transfer.data_instr_transfer module_ cfg instr prestate in
            instr_data := Instr.Label.Map.set !instr_data ~key:instr.label ~data:(Simple prestate, Simple poststate);
            poststate))
      | Control instr ->
        let poststate = match Transfer.control_instr_transfer module_ summaries cfg instr state with
          | `Simple s -> Simple s
          | `Branch (s1, s2) -> Branch (s1, s2)
        in
        instr_data := Instr.Label.Map.set !instr_data ~key:instr.label ~data:(Simple state, poststate);
        poststate in

    (* Analyzes one block, returning the pre and post states *)
    let analyze_block (block_idx : int) : Transfer.state * result =
      (* The block to analyze *)
      let block = Cfg.find_block_exn cfg block_idx in
      let incoming = Cfg.incoming_edges cfg block_idx in
      (* in_state is the join of all the the out_state of the predecessors.
         Special case: if the out_state of a predecessor is not a simple one, that means we are the target of a break.
         If this is the case, we pick the right branch, according to the edge data *)
      let pred_states = (List.map incoming ~f:(fun (idx, d) -> match (snd (IntMap.find_exn !block_data idx), d) with
          | Simple s, _ -> (idx, s)
          | Branch (t, _), Some true -> (idx, t)
          | Branch (_, f), Some false -> (idx, f)
          | Branch _, None -> failwith (Printf.sprintf "invalid branch state at block %d, from block %d" block_idx idx)
          | Uninitialized, _ -> (idx, Transfer.bottom_state cfg))) in
      let in_state = Transfer.merge_flows module_ cfg block pred_states in
      (* We analyze it *)
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
        Simple (Transfer.widen_state st1 st2)
      | Branch (st1, st2), Branch (st1', st2') ->
        Branch (Transfer.widen_state st1 st1',
                Transfer.widen_state st2 st2')
      | _ -> failwith "Cannot widen results" in
    let rec fixpoint (worklist : IntSet.t) (iteration : int) : unit =
      if IntSet.is_empty worklist then
        () (* No more elements to consider. We can stop here *)
      else
        let block_idx = IntSet.min_elt_exn worklist in
        Log.debug (Printf.sprintf "-----------------------\n Analyzing block %d\n" block_idx);
        let (in_state, out_state) = analyze_block block_idx in
        Log.debug (Printf.sprintf "out_state is: %s\n" (result_to_string out_state));
        (* Has out state changed? *)
        let previous_out_state = snd (IntMap.find_exn !block_data block_idx) in
        match previous_out_state with
        | st when compare_result out_state st = 0 ->
          (* Didn't change, we can safely ignore the successors *)
          fixpoint (IntSet.remove worklist block_idx) (iteration+1)
        | _ ->
          (* Update the out state in the analysis results.
             We join with the previous results *)
          let new_out_state =
            (* TODO: Join may not be necessary here, as long as out_state is greater than previous_out_state *)
            if IntSet.mem cfg.loop_heads block_idx then
              widen_result previous_out_state (join_result previous_out_state out_state)
            else
              join_result previous_out_state out_state
          in
          block_data := IntMap.set !block_data ~key:block_idx ~data:(Simple in_state, new_out_state);
          (* And recurse by adding all successors *)
          let successors = Cfg.successors cfg block_idx in
          fixpoint (IntSet.union (IntSet.remove worklist block_idx) (IntSet.of_list successors)) (iteration+1)
    in
    (* Performs narrowing by re-analyzing once each block *)
    let rec narrow (blocks : int list) : unit = match blocks with
      | [] -> ()
      | block_idx :: blocks ->
        let (in_state, out_state) = analyze_block block_idx in
        block_data := IntMap.set !block_data ~key:block_idx ~data:(Simple in_state, out_state);
        narrow blocks
    in
    fixpoint (IntSet.singleton cfg.entry_block) 1;
    if !narrow_option then narrow (IntMap.keys cfg.basic_blocks);
    !instr_data

  let analyze (module_ : Wasm_module.t) (cfg : annot_expected Cfg.t) (summaries : summary Int32Map.t): state Cfg.t * summary =
    let to_state (results_pair : result * result) : (state * state) =
      (result_to_state cfg (fst results_pair), result_to_state cfg (snd results_pair)) in
    let instr_data = analyze_ module_ cfg summaries in
    let analyzed_cfg = Cfg.map_annotations cfg
        ~f:(fun i -> to_state (Instr.Label.Map.find_exn instr_data (Instr.label i))) in
    let summary = Transfer.extract_summary cfg analyzed_cfg in
    analyzed_cfg, summary

  let analyze_keep (module_ : Wasm_module.t) (cfg : annot_expected Cfg.t) (summaries : summary Int32Map.t) : (annot_expected * state) Cfg.t * summary =
    let to_state_keep (previous : annot_expected * annot_expected) (results_pair : result * result) : ((annot_expected * state) * (annot_expected * state)) =
      ((fst previous, result_to_state cfg (fst results_pair)),
       (snd previous, result_to_state cfg (snd results_pair))) in
    let to_state (results_pair : result * result) : (state * state) =
      (result_to_state cfg (fst results_pair), result_to_state cfg (snd results_pair)) in
    let instr_data = analyze_ module_ cfg summaries in
    let analyzed_cfg = Cfg.map_annotations cfg
        ~f:(fun i -> to_state (Instr.Label.Map.find_exn instr_data (Instr.label i))) in
    let analyzed_keep_cfg = Cfg.map_annotations cfg
        ~f:(fun i -> to_state_keep (Instr.annotation_before i, Instr.annotation_after i) (Instr.Label.Map.find_exn instr_data (Instr.label i))) in
    let summary = Transfer.extract_summary cfg analyzed_cfg in
    analyzed_keep_cfg, summary

  (** Extract the out state from intra-procedural results *)
  let final_state (cfg_before : annot_expected Cfg.t) (cfg : state Cfg.t) : state =
    Cfg.state_after_block cfg cfg.exit_block (Transfer.init_state cfg_before)

  let final_state_kept (_cfg : (annot_expected * state) Cfg.t) : state =
    failwith "TODO, was: snd (Cfg.state_after_block cfg cfg.exit_block)"
end
