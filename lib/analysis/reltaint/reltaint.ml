open Core_kernel
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
       Relational.Intra.init_summaries (Int32Map.map summaries ~f:fst);
       Taint.Intra.init_summaries (Int32Map.map summaries ~f:snd);
       Relational.Options.ignore_memory := false;
       Log.info  "---------- Relational analysis ----------";
       let relational_cfg = Relational.Intra.analyze_keep wasm_mod cfg in
       let out_state = Relational.Intra.final_state_kept relational_cfg in
       let relational_summary = Relational.Intra.summary cfg out_state in
       (* Run the taint analysis *)
       Log.info "---------- Taint analysis ----------";
       Taint.Options.use_relational := true;
       let result_cfg = Taint.Intra.analyze wasm_mod relational_cfg in
       let out_state = Taint.Intra.final_state relational_cfg result_cfg in
       let taint_summary = Taint.Intra.summary relational_cfg out_state in
       (relational_summary, taint_summary))
