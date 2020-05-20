open Core_kernel
open Helpers

(** Analyzes a CFG. Returns a map where each basic block is mappped to its input state and the result of applying the transfer function to it *)
let analyze (cfg : Cfg.t) (args : Value.t list) (globals : Globals.t) (memory : Memory.t) (summaries : Summary.t IntMap.t) : (Domain.state * Transfer.result) IntMap.t =
  let bottom = Transfer.Uninitialized in
  assert (List.length args = (fst cfg.arity)); (* Given number of arguments should match the in arity of the function *)
  let init = Domain.init args cfg.nlocals globals memory in
  let data = ref (IntMap.of_alist_exn (List.map (IntMap.keys cfg.basic_blocks)
                                         ~f:(fun idx ->
                                             (idx, (bottom, bottom))))) in
  let rec fixpoint (worklist : IntSet.t) (iteration : int) : unit =
    if IntSet.is_empty worklist then
      () (* No more elements to consider. We can stop here *)
    else
      let block_idx = IntSet.min_elt_exn worklist in
      (* The block to analyze *)
      let block = Cfg.find_block_exn cfg block_idx in
      let predecessors = Cfg.predecessors cfg block_idx in
      (* in_state is the join of all the the out_state of the predecessors.
         Special case: if the out_state of a predecessor is not a simple one, that means we are the target of a break.
         If this is the case, we pick the right branch, according to the edge data *)
      let pred_states = (List.map predecessors ~f:(fun (idx, d) -> (snd (IntMap.find_exn !data idx)), d)) in
      let in_state = match (List.fold_left pred_states ~init:bottom ~f:(fun acc res ->
          Transfer.join_result acc (match res with
              | (Branch (t, _), Some true) -> Simple t
              | (Branch (_, f), Some false) -> Simple f
              | (Branch _, None) -> failwith "should not happen"
              | (s, _) -> s)))
        with
        | Simple r -> r
        | Uninitialized -> init
        | _ -> failwith "should not happen" in
      (* We analyze it *)
      let out_state = Transfer.transfer block in_state summaries in
      (* Has out state changed? *)
      let previous_out_state = snd (IntMap.find_exn !data block_idx) in
      match previous_out_state with
      | st when Transfer.compare_result out_state st = 0 ->
        (* Didn't change, we can safely ignore the successors *)
        (* TODO: make sure that this is true. If not, maybe we just have to put all blocks on the worklist for the first iteration(s) *)
        fixpoint (IntSet.remove worklist block_idx) (iteration+1)
      | _ ->
        (* Update the out state in the analysis results, joining it with the previous one *)
        let new_out_state = Transfer.join_result out_state previous_out_state in
        data := IntMap.set !data ~key:block_idx ~data:(Simple in_state, new_out_state);
        (* And recurse by adding all successors *)
        let successors = Cfg.successors cfg block_idx in
        fixpoint (IntSet.union (IntSet.remove worklist block_idx) (IntSet.of_list successors)) (iteration+1)
  in
  fixpoint (IntSet.singleton cfg.entry_block) 1;
  IntMap.map !data ~f:(fun (in_state, out_state) -> (match in_state with
      | Simple s -> s
      | _ -> failwith "TODO"), out_state)

(* Similar to analyze, but only return the out state for a CFG *)
let analyze_coarse (cfg : Cfg.t) (args : Value.t list) (globals : Globals.t) (memory : Memory.t) (summaries : Summary.t IntMap.t) : Domain.state =
  let results = analyze cfg args globals memory summaries in
  match snd (IntMap.find_exn results cfg.exit_block) with
  | Simple s -> s
  | _ -> failwith "TODO"
