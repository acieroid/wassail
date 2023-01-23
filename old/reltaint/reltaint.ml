open Core
open Helpers

let analyze_intra : Wasm_module.t -> Int32.t list -> (Relational.Summary.t * Taint.Summary.t) Int32Map.t =
  Analysis_helpers.mk_intra
    (fun cfgs wasm_mod ->
       Int32Map.merge
         (Relational.Summary.initial_summaries cfgs wasm_mod `Top)
         (Taint.Summary.initial_summaries cfgs wasm_mod `Top)
         ~f:(fun ~key:_k -> function
             | `Both (s1, s2) -> Some (s1, s2)
             | _ -> failwith "unexpected"))
    (fun summaries wasm_mod cfg ->
       Relational.Options.ignore_memory := false;
       Log.info  "---------- Relational analysis ----------";
       let relational_summaries = Int32Map.map summaries ~f:fst in
       let taint_summaries = Int32Map.map summaries ~f:snd in
       let relational_cfg, relational_summary = Relational.Intra.analyze_keep wasm_mod cfg relational_summaries in
       (* Run the taint analysis *)
       Log.info "---------- Taint analysis ----------";
       Taint.Options.use_relational := true;
       let _taint_cfg, taint_summary = Taint.Intra.analyze wasm_mod relational_cfg taint_summaries in
       relational_summary, taint_summary)
