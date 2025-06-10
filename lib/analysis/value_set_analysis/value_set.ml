open Core
open Helpers

(* module : Options = Value_set_options *)
module Domain = Abstract_store_domain
module Transfer = Value_set_transfer.Make
module Summary = Value_set_summary
module Intra = Intra.Make(Transfer)
module Inter = Inter.Make(Intra)

let analyze_intra : Wasm_module.t -> Int32.t list -> (Summary.t * Domain.t Cfg.t option) Int32Map.t =
  Analysis_helpers.mk_intra
    (fun cfgs wasm_mod ->
      (Int32Map.map ~f:(fun x -> (x, None)) (Summary.initial_summaries cfgs wasm_mod `Bottom)))
    (fun data wasm_mod cfg ->
      Log.info
        (Printf.sprintf "---------- Value-set analysis of function %s ----------" (Int32.to_string cfg.idx));
      (* Run the value-set analysis *)
      let annotated_cfg = (* Relational.Transfer.dummy_annotate  *) cfg in
      let summaries = Int32Map.map data ~f:fst in
      let (result_cfg, value_set_summary) = Intra.analyze wasm_mod annotated_cfg summaries in
      (value_set_summary, Some result_cfg))

let annotate (wasm_mod : Wasm_module.t) (summaries : Summary.t Int32Map.t) (spec_cfg : Spec.t Cfg.t) : Domain.t Cfg.t =
  let rel_cfg = (* Relational.Transfer.dummy_annotate *) spec_cfg in
  fst (Intra.analyze wasm_mod rel_cfg summaries)

let check (expected : Summary.t) (actual : Summary.t) : bool =
  if Summary.subsumes actual expected then
    if Summary.equal actual expected then
      true
    else begin
      Printf.printf "\n[IMPRECISION] summaries not equal:\nexpected: %s\nactual: %s\n" (Summary.to_string expected) (Summary.to_string actual);
      true (* not equal, but it does subsume so the test does not fail *)
    end
  else begin
    Printf.printf "\nsummaries does not subsume:\nexpected: %s\nactual: %s\n" (Summary.to_string expected) (Summary.to_string actual);
    false
  end