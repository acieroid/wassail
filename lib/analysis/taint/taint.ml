open Core_kernel
open Helpers

module Options = Taint_options
module Domain = Taint_domain
module Transfer = Taint_transfer.Make
module Summary = Taint_summary
module Intra = Intra.Make(Transfer)
module Inter = Inter.Make(Intra)

let analyze_intra : Wasm_module.t -> Int32.t list -> Summary.t Int32Map.t =
  Analysis_helpers.mk_intra
    (fun cfgs wasm_mod -> Summary.initial_summaries cfgs wasm_mod `Bottom)
    (fun summaries wasm_mod cfg ->
       Log.info
         (Printf.sprintf "---------- Taint analysis of function %s ----------" (Int32.to_string cfg.idx));
       (* Run the taint analysis *)
       Options.use_relational := false;
       Intra.init_summaries summaries;
       let annotated_cfg = Relational.Transfer.dummy_annotate cfg in
       let result_cfg = Intra.analyze wasm_mod annotated_cfg in
       let final_state = Intra.final_state annotated_cfg result_cfg  in
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

let analyze_inter : Wasm_module.t -> Int32.t list list -> Summary.t Int32Map.t =
  Analysis_helpers.mk_inter
    (fun cfgs wasm_mod -> Summary.initial_summaries cfgs wasm_mod `Bottom)
    (fun wasm_mod scc ->
       Log.info
         (Printf.sprintf "---------- Taint analysis of SCC {%s} ----------"
            (String.concat ~sep:"," (List.map (Int32Map.keys scc) ~f:Int32.to_string)));
       (* Run the taint analysis *)
       Options.use_relational := false;
       let annotated_scc = Int32Map.map scc ~f:Relational.Transfer.dummy_annotate in
       let analyzed_cfgs = Inter.analyze wasm_mod annotated_scc in
       Int32Map.mapi analyzed_cfgs ~f:(fun ~key:idx ~data:cfg ->
           Intra.summary (Int32Map.find_exn annotated_scc cfg.idx)
             (Intra.final_state (Int32Map.find_exn annotated_scc idx) cfg)))

module Test = struct
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
    let actual = Int32Map.find_exn (analyze_intra module_ [0l]) 0l in
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
    let actual = Int32Map.find_exn (analyze_intra module_ [0l]) 0l in
    let expected = Summary.{ ret = Some (Domain.Taint.taint (Var.Local 0)); mem = Domain.Taint.taint (Var.Local 0); globals = [Domain.Taint.taint (Var.Global 0)] } in
    check expected actual
end
