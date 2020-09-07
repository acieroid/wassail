open Core_kernel
open Helpers

module type INTER = sig
  type annot_expected
  type state
  val analyze : Wasm_module.t -> annot_expected Cfg.t IntMap.t -> state Cfg.t IntMap.t
end

module Make (Intra : Intra.INTRA) (*: INTER*) = struct
  type annot_expected = Intra.annot_expected
  type state = Intra.state

  (** Analyze multiple CFGS, returns a map from CFG id to out_state for each CFG *)
  let analyze (module_ : Wasm_module.t) (cfgs : 'a Cfg.t IntMap.t) : state Cfg.t IntMap.t =
    let deps : IntSet.t IntMap.t =
      (* The dependencies are a map from CFGs indices to the indices of their callers and callees *)
      IntMap.map cfgs ~f:(fun cfg ->
          let callees = Cfg.callees cfg in
          let callers = Cfg.callers cfgs cfg in
          (* Only the callers and callees that are in the set of CFGs to analyze are kept.
             This is because the inter analysis runs on an SCC of the call graph: we only need to fixpoint over that SCC *)
          IntSet.filter (IntSet.union callees callers) ~f:(fun idx -> IntMap.mem cfgs idx)) in
    (* The fixpoint loop, using a worklist algorithm, and the different domain values *)
    let rec fixpoint (worklist : IntSet.t) (acc : state Cfg.t IntMap.t) : state Cfg.t IntMap.t =
      if IntSet.is_empty worklist then
        (* Worklist is empty, produce the results *)
        acc
          (* was: IntMap.mapi cfgs ~f:(fun ~key:idx ~data:cfg ->
             Intra.summary cfg (Intra.out_state cfg (IntMap.find_exn acc idx))) *)
      else
        (* Next CFG to analyze *)
        let cfg_idx = IntSet.min_elt_exn worklist in
        if cfg_idx < module_.nimports then begin
          (* Should not happen *)
          Printf.printf "Not analyzing cfg %d (it is an imported function)" cfg_idx;
          fixpoint (IntSet.remove worklist cfg_idx) acc
        end else begin
          let cfg = IntMap.find_exn cfgs cfg_idx in
          Printf.printf "Analyzing cfg %d (name: %s)\n" cfg_idx cfg.name;
          (* Perform intra-procedural analysis *)
          let results = Intra.analyze module_ cfg (* !summaries *) in
          (* Check difference with previous state, if there was any *)
          let previous_results = IntMap.find acc cfg_idx in
          match previous_results with
          | Some res when Cfg.equal Intra.equal_state results res ->
            (* Same results as before, we can just recurse without having do anything *)
            fixpoint (IntSet.remove worklist cfg_idx) acc
        | _ ->
          (* Result differed, we have to add all callees and callers to the worklist.
             Callers because the analyzed function could have modified globals/memory that will be read by the caller.
             Callees for the same reason.
             The caller/callee information is encoded in the deppendencies *)
          let to_add = match IntMap.find deps cfg_idx with
            | Some ds -> ds
            | None -> IntSet.empty in
          (* Update data of the analysis and recurse *)
          fixpoint (IntSet.union (IntSet.remove worklist cfg_idx) to_add) (IntMap.set acc ~key:cfg_idx ~data:results)
      end
  in
  (* Run the analysis *)
  fixpoint
    (IntSet.of_list (IntMap.keys cfgs)) (* All CFGs are scheduled for analysis *)
    IntMap.empty (* No results have been produced yet *)
end
