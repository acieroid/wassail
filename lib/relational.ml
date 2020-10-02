open Helpers

module Domain = Relational_domain
module Transfer = Relational_transfer
module Summary = Relational_summary
module Spec = Relational_spec

module Intra = Intra.Make(Transfer)

let analyze_intra : string -> int list -> Summary.t IntMap.t =
  Analysis_helpers.mk_intra
    (fun cfgs wasm_mod -> Summary.initial_summaries cfgs wasm_mod `Top)
    (fun summaries wasm_mod cfg ->
       Intra.init_summaries summaries;
       Transfer.ignore_memory := false;
       let result_cfg = Intra.analyze wasm_mod cfg in
       let out_state = Intra.final_state result_cfg in
       Intra.summary cfg out_state)
