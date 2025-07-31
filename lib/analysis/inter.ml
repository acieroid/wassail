open Core
open Helpers

module type INTER = sig
  type annot_expected
  type state
  val analyze : Wasm_module.t -> annot_expected Cfg.t IntMap.t -> state Cfg.t IntMap.t
end

module Make (Intra : Intra.INTRA) (*: INTER*) = struct
  type annot_expected = Intra.annot_expected
  type state = Intra.state
  type summary = Intra.summary

  (** Analyze multiple CFGs, returns a map of the analyzed CFG and their summary. Relies on summaries produced for the depended-upon functions. *)
  let analyze (module_ : Wasm_module.t) (cfgs : annot_expected Cfg.t Int32Map.t) (summaries : summary Int32Map.t): (state Cfg.t * summary) Int32Map.t =
    let deps : Int32Set.t Int32Map.t =
      (* The dependencies are a map from CFGs indices to the indices of their callers and callees *)
      Int32Map.map cfgs ~f:(fun cfg ->
          let callees = Cfg.callees cfg in
          let callers = Cfg.callers cfgs cfg in
          (* Only the callers and callees that are in the set of CFGs to analyze are kept.
             This is because the inter analysis runs on an SCC of the call graph: we only need to fixpoint over that SCC *)
          Int32Set.filter (Int32Set.union callees callers) ~f:(fun idx -> Int32Map.mem cfgs idx)) in
    (* The fixpoint loop, using a worklist algorithm, and the different domain values *)
    let rec fixpoint (worklist : Int32Set.t) (annotated_cfgs : state Cfg.t Int32Map.t) (summaries : summary Int32Map.t) : (state Cfg.t * summary) Int32Map.t =
      if Int32Set.is_empty worklist then
        (* Worklist is empty, produce the results *)
        Int32Map.merge annotated_cfgs summaries ~f:(fun ~key:_fid -> function
            | `Both (cfg, summary) -> Some (cfg, summary)
            | `Left _cfg -> failwith "Unexpected: a summary is missing during inter analysis"
            | `Right _summary -> None (* we haven't analyzed that CFG *))
          (* was: Int32Map.mapi cfgs ~f:(fun ~key:idx ~data:cfg ->
             Intra.summary cfg (Intra.out_state cfg (Int32Map.find_exn acc idx))) *)
      else
        (* Next CFG to analyze *)
        let cfg_idx = Int32Set.min_elt_exn worklist in
        if Int32.(cfg_idx < module_.nfuncimports) then begin
          (* Should not happen *)
          Log.info
            (Printf.sprintf "Not analyzing cfg %s (it is an imported function)" (Int32.to_string cfg_idx));
          fixpoint (Int32Set.remove worklist cfg_idx) annotated_cfgs summaries
        end else begin
          let cfg = match Int32Map.find cfgs cfg_idx with
            | Some r -> r
            | None -> failwith "Inter: can't find CFG" in
          Log.info
            (Printf.sprintf "Analyzing cfg %s (name: %s)\n" (Int32.to_string cfg_idx) cfg.name);
          let () =
            let oc = Out_channel.create ~append:true "store_types.txt" in
            Out_channel.output_string oc (Printf.sprintf "==================================(function %s: %s)" (Int32.to_string cfg_idx) cfg.name ^ "\n");
            Out_channel.close oc;
          in
          (* Perform intra-procedural analysis *)
          let (results, summary) = Intra.analyze module_ cfg summaries in
          (* Check difference with previous state, if there was any *)
          let previous_results = Int32Map.find annotated_cfgs cfg_idx in
          match previous_results with
          | Some res when Cfg.equal Intra.equal_state results res ->
            (* Same results as before, we can just recurse without having do anything *)
            fixpoint (Int32Set.remove worklist cfg_idx) annotated_cfgs summaries
          | _ ->
            (* Result differed, we have to add all callees and callers to the worklist.
               Callers because the analyzed function could have modified globals/memory that will be read by the caller.
               Callees for the same reason.
               The caller/callee information is encoded in the deppendencies *)
            let to_add = match Int32Map.find deps cfg_idx with
              | Some ds -> ds
              | None -> Int32Set.empty in
            (* Update data of the analysis and recurse *)
            fixpoint
              (Int32Set.union (Int32Set.remove worklist cfg_idx) to_add)
              (Int32Map.set annotated_cfgs ~key:cfg_idx ~data:results)
              (Int32Map.set summaries ~key:cfg_idx ~data:summary)
        end
  in
  (* Run the analysis *)
  fixpoint
    (Int32Set.of_list (Int32Map.keys cfgs)) (* All CFGs are scheduled for analysis *)
    Int32Map.empty (* No results have been produced yet *)
    summaries
end
