open Core_kernel
open Helpers

module Make (Intra : Intra_fixpoint.INTRA) = struct

  (** Results of the inter-analysis are stored in the form of a map from function id to (optional) results from the intra-analysis *)
  type inter_results = Intra.full_results IntMap.t

  (** Analyze multiple CFGS, returns a map from CFG id to out_state for each CFG *)
  let analyze (cfgs : Cfg.t IntMap.t) (module_ : Wasm_module.t) : Intra.summary IntMap.t =
    let deps : IntSet.t IntMap.t =
      (* The dependencies are a map from CFGs indices to the indices of their callers and callees *)
      IntMap.map cfgs ~f:(fun cfg ->
          let callees = Cfg.callees cfg in
          let callers = Cfg.callers cfgs cfg in
          (* Only the callers and callees that are in the set of CFGs to analyze are kept.
             This is because the inter analysis runs on an SCC of the call graph: we only need to fixpoint over that SCC *)
          IntSet.filter (IntSet.union callees callers) ~f:(fun idx -> IntMap.mem cfgs idx)) in
    (* The fixpoint loop, using a worklist algorithm, and the different domain values *)
    let rec fixpoint (worklist : IntSet.t) (data : inter_results) : Intra.summary IntMap.t =
      if IntSet.is_empty worklist then
        IntMap.mapi cfgs ~f:(fun ~key:idx ~data:cfg ->
            Intra.summary cfg (Intra.out_state cfg (IntMap.find_exn data idx)))
      else
        (* Next CFG to analyze *)
        let cfg_idx = IntSet.min_elt_exn worklist in
        if cfg_idx < module_.nimports then begin
          (* Should not happen *)
          Printf.printf "Not analyzing cfg %d (it is an imported function)" cfg_idx;
          fixpoint (IntSet.remove worklist cfg_idx) data
        end else begin
          let cfg = IntMap.find_exn cfgs cfg_idx in
          Printf.printf "Analyzing cfg %d (name: %s)\n" cfg_idx cfg.name;
          (* Perform intra-procedural analysis *)
          let results = Intra.analyze module_ cfg (* !summaries *) in
          let out_state = Intra.out_state cfg results in
          (* Check difference with previous state, if there was any *)
          let previous_results = IntMap.find data cfg_idx in
          match previous_results with
          | Some res when Intra.equal_state out_state (Intra.out_state cfg res) ->
            (* Same results as before, we can just recurse without having do anything *)
            fixpoint (IntSet.remove worklist cfg_idx) data
        | _ ->
          (* Result differed, we have to add all callees and callers to the worklist.
             Callers because the analyzed function could have modified globals/memory that will be read by the caller.
             Callees for the same reason.
             The caller/callee information is encoded in the deppendencies *)
          let to_add = match IntMap.find deps cfg_idx with
            | Some ds -> ds
            | None -> IntSet.empty in
          (* Update data of the analysis and recurse *)
          fixpoint (IntSet.union (IntSet.remove worklist cfg_idx) to_add) (IntMap.set data ~key:cfg_idx ~data:results)
      end
  in
  (* Run the analysis *)
  fixpoint (IntSet.of_list (IntMap.keys cfgs)) IntMap.empty
end
