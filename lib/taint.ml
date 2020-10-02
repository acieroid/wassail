open Core_kernel
open Helpers

module Options = Taint_options
module Domain = Taint_domain
module Transfer = Taint_transfer.Make
module Summary = Taint_summary
module Intra = Intra.Make(Transfer)
module Inter = Inter.Make(Intra)

let analyze_intra : string -> int list -> Summary.t IntMap.t =
  Analysis_helpers.mk_intra
    (fun cfgs wasm_mod -> Summary.initial_summaries cfgs wasm_mod `Bottom)
    (fun summaries wasm_mod cfg ->
       Logging.info (Printf.sprintf "---------- Taint analysis of function %d ----------" cfg.idx);
       (* Run the taint analysis *)
       Options.use_relational := false;
       Intra.init_summaries summaries;
       let annotated_cfg = Relational.Transfer.dummy_annotate cfg in
       let result_cfg = Intra.analyze wasm_mod annotated_cfg in
       let final_state = Intra.final_state result_cfg in
       let taint_summary = Intra.summary annotated_cfg final_state in
       taint_summary)

let analyze_inter : string -> int list list -> Summary.t IntMap.t =
  Analysis_helpers.mk_inter
    (fun cfgs wasm_mod -> Summary.initial_summaries cfgs wasm_mod `Bottom)
    (fun wasm_mod scc ->
       Logging.info (Printf.sprintf "---------- Taint analysis of SCC {%s} ----------"
                       (String.concat ~sep:"," (List.map (IntMap.keys scc) ~f:string_of_int)));
       (* Run the taint analysis *)
       Options.use_relational := false;
       let annotated_scc = IntMap.map scc ~f:Relational.Transfer.dummy_annotate in
       let analyzed_cfgs = Inter.analyze wasm_mod annotated_scc in
       IntMap.map analyzed_cfgs ~f:(fun cfg -> Intra.summary (IntMap.find_exn annotated_scc cfg.idx) (Intra.final_state cfg)))
