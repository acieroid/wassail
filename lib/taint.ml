open Core_kernel
open Helpers

module Options = Taint_options
module Domain = Taint_domain
module Transfer = Taint_transfer.Make
module Summary = Taint_summary
module Intra = Intra.Make(Transfer)
module Inter = Inter.Make(Intra)

let analyze_intra : Wasm_module.t -> int list -> Summary.t IntMap.t =
  Analysis_helpers.mk_intra
    (fun cfgs wasm_mod -> Summary.initial_summaries cfgs wasm_mod `Bottom)
    (fun summaries wasm_mod cfg ->
       Logging.info !Taint_options.verbose
         (Printf.sprintf "---------- Taint analysis of function %d ----------" cfg.idx);
       (* Run the taint analysis *)
       Options.use_relational := false;
       Intra.init_summaries summaries;
       let annotated_cfg = Relational.Transfer.dummy_annotate cfg in
       let result_cfg = Intra.analyze wasm_mod annotated_cfg in
       let final_state = Intra.final_state result_cfg in
       let taint_summary = Intra.summary annotated_cfg final_state in
       taint_summary)

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

let%test "simple function has no taint" =
  let module_ = Wasm_module.of_string "(module
  (type (;0;) (func (param i32) (result i32)))
  (func (;test;) (type 0) (param i32) (result i32)
    i32.const 256
    i32.const 512
    i32.const 0
    select)
  (table (;0;) 1 1 funcref)
  (memory (;0;) 2)
  (global (;0;) (mut i32) (i32.const 66560)))" in
  let actual = IntMap.find_exn (analyze_intra module_ [0]) 0 in
  let expected = Summary.{ ret = Some Domain.Taint.bottom; mem = Domain.Taint.bottom; globals = [Domain.Taint.taint (Var.Global 0)] } in
  check expected actual

let%test "test store" =
  let module_ = Wasm_module.of_string "(module
  (type (;0;) (func (param i32) (result i32)))
  (func (;test;) (type 0) (param i32) (result i32)
    global.get 0
    local.get 0
    i32.store
    local.get 0)
  (table (;0;) 1 1 funcref)
  (memory (;0;) 2)
  (global (;0;) (mut i32) (i32.const 66560)))" in
  let actual = IntMap.find_exn (analyze_intra module_ [0]) 0 in
  let expected = Summary.{ ret = Some (Domain.Taint.taint (Var.Local 0)); mem = Domain.Taint.taint (Var.Local 0); globals = [Domain.Taint.taint (Var.Global 0)] } in
  check expected actual

let analyze_inter : Wasm_module.t -> int list list -> Summary.t IntMap.t =
  Analysis_helpers.mk_inter
    (fun cfgs wasm_mod -> Summary.initial_summaries cfgs wasm_mod `Bottom)
    (fun wasm_mod scc ->
       Logging.info !Taint_options.verbose
         (Printf.sprintf "---------- Taint analysis of SCC {%s} ----------"
            (String.concat ~sep:"," (List.map (IntMap.keys scc) ~f:string_of_int)));
       (* Run the taint analysis *)
       Options.use_relational := false;
       let annotated_scc = IntMap.map scc ~f:Relational.Transfer.dummy_annotate in
       let analyzed_cfgs = Inter.analyze wasm_mod annotated_scc in
       IntMap.map analyzed_cfgs ~f:(fun cfg -> Intra.summary (IntMap.find_exn annotated_scc cfg.idx) (Intra.final_state cfg)))
