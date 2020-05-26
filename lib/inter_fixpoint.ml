open Core_kernel
open Helpers

type inter_results = ((((Transfer.result * Transfer.result) IntMap.t) option) IntMap.t)
(* The results of the inter analysis *)
let data : inter_results ref = ref IntMap.empty

(* Analyze multiple CFGS, returns a map from CFG id to out_state for each CFG *)
let analyze (cfgs : Cfg.t IntMap.t) (nglobals : int) (module_ : Wasm_module.t) : unit =
  data := (IntMap.of_alist_exn (List.map (IntMap.keys cfgs)
                                  ~f:(fun idx ->
                                      (idx, None))));
  (* The fixpoint loop, with a worklist, and the different domain values *)
  let rec fixpoint (worklist : IntSet.t)
      (globals : Globals.t) (memory : Memory.t)
      (summaries : Summary.t IntMap.t) =
    if IntSet.is_empty worklist then
      () (* empty worklist, analysis finished *)
    else
      (* Next CFG to analyze *)
      let cfg_idx = IntSet.min_elt_exn worklist in
      let cfg = IntMap.find_exn cfgs cfg_idx in
      (* The arguments to this CFG. They all map to top because we perform a compositional analysis *)
      let args = List.mapi cfg.arg_types ~f:(fun i t -> Value.parameter t i) in
      Printf.printf "Analyzing cfg %d with globals: [%s] and args: [%s]\n" cfg_idx (Globals.to_string globals) (Value.list_to_string args);
      (* Perform intra-procedural analysis *)
      let results = Intra_fixpoint.analyze cfg args globals memory summaries module_ in
      let out_state = Intra_fixpoint.out_state cfg results in
      (* Check difference with previous state, if there was any *)
      let previous_results = IntMap.find_exn !data cfg_idx in
      match previous_results with
      | Some res when Domain.compare_state out_state (Intra_fixpoint.out_state cfg res) = 0 ->
        (* Same results as before, we can just recurse without having to recompute globals nor memory *)
        fixpoint (IntSet.remove worklist cfg_idx) globals memory summaries
      | _ ->
        (* Result differed, we have to add all callees and callers to the worklist.
           Callers because the analyzed function could have modified globals/memory that will be read by the caller.
           Callees for the same reason. *)
        let callees = Cfg.callees cfg in
        let callers = Cfg.callers cfgs cfg in
        let summary = Summary.make cfg out_state in
        Printf.printf "Final summary is: %s\n" (Summary.to_string summary);
        let new_summaries = IntMap.set summaries ~key:cfg.idx ~data:summary in
        (* Update data of the analysis and recurse *)
        data := IntMap.set !data ~key:cfg_idx ~data:(Some results);
        fixpoint (IntSet.union (IntSet.remove worklist cfg_idx) (IntSet.union callees callers))
          (* These should always remain the same, and not be necessary anymore *)
          globals memory new_summaries
  in
  (* Initial summaries are all empty *)
  let summaries0 = IntMap.map cfgs ~f:(fun cfg -> Summary.bottom nglobals cfg) in
  (* Globals are symbolic variables, values are top *)
  let globals = List.init nglobals ~f:Value.global in
  fixpoint (IntSet.of_list (IntMap.keys cfgs)) globals Memory.initial summaries0 ;
