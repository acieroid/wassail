open Helpers

module Domain = Relational_domain
module Transfer = Relational_transfer
module Summary = Relational_summary
module Spec = Relational_spec
module Options = Relational_options

module Intra = Intra.Make(Transfer)

let analyze_intra : Wasm_module.t -> Int32.t list -> Summary.t Int32Map.t =
  Analysis_helpers.mk_intra
    (fun cfgs wasm_mod -> Summary.initial_summaries cfgs wasm_mod `Top)
    (fun summaries wasm_mod cfg ->
       Intra.init_summaries summaries;
       Options.ignore_memory := false;
       let result_cfg = Intra.analyze wasm_mod cfg in
       let out_state = Intra.final_state result_cfg in
       Intra.summary cfg out_state)

let check (expected : Summary.t) (actual : Summary.t) : bool =
  if Summary.equal expected actual then begin
    true
  end else begin
    Printf.printf "summaries not equal:\nexpected: %s\nactual: %s\n" (Summary.to_string expected) (Summary.to_string actual);
    false
  end

module Test = struct
  let%test "local.get 0 relational summary" =
    let module_ = Wasm_module.of_string "(module
  (type (;0;) (func (param i32) (result i32)))
  (func (;test;) (type 0) (param i32) (result i32)
    local.get 0)
  (table (;0;) 1 1 funcref)
  (memory (;0;) 2)
  (global (;0;) (mut i32) (i32.const 66560)))" in
    let actual = Int32Map.find_exn (analyze_intra module_ [0l]) 0l in
    let expected = Summary.{
        in_arity = 1;
        params = [Var.Local 0];
        return = Some Var.Return;
        mem_pre = [];
        mem_post = [];
        globals_pre = [Var.Global 0];
        globals_post = [Var.Global 0];
        state = Relational_domain.of_equality_constraints (Var.Set.of_list [Var.Global 0; Var.Local 0; Var.Return])
            [(Var.Local 0, Var.Return)]
      } in
    check expected actual

  let%test "call correctly propagates summaries" =
    let module_ = Wasm_module.of_string "(module
  (type (;0;) (func (param i32) (result i32)))
  (func (;test;) (type 0) (param i32) (result i32)
    local.get 0)
  (func (;test-call;) (type 0) (param i32) (result i32)
    local.get 0
    call 0)
  (table (;0;) 1 1 funcref)
  (memory (;0;) 2)
  (global (;0;) (mut i32) (i32.const 66560)))" in
    let actual = Int32Map.find_exn (analyze_intra module_ [0l; 1l]) 1l in
    let expected = Summary.{
        in_arity = 1;
        params = [Var.Local 0];
        return = Some Var.Return;
        mem_pre = [];
        mem_post = [];
        globals_pre = [Var.Global 0];
        globals_post = [Var.Global 0];
        state = Relational_domain.of_equality_constraints (Var.Set.of_list [Var.Global 0; Var.Local 0; Var.Return])
            [(Var.Local 0, Var.Return)]
      } in
    check expected actual

  let%test "memory is supported" =
    let module_ = Wasm_module.of_string "(module
  (type (;0;) (func (param i32) (result i32)))
  (func (;test;) (type 0) (param i32) (result i32)
    global.get 0 ;; [g0]
    local.get 0  ;; [l0, g0]
    i32.store    ;; [], [g0 -> l0] in the memory
    global.get 0 ;; [g0]
    i32.load     ;; [l0]
  )
  (table (;0;) 1 1 funcref)
  (memory (;0;) 2)
  (global (;0;) (mut i32) (i32.const 66560)))" in
    Relational_options.ignore_memory := false;
    let actual = Int32Map.find_exn (analyze_intra module_ [0l]) 0l in
    let expected = Summary.{
        in_arity = 1;
        params = [Var.Local 0];
        return = Some Var.Return;
        mem_pre = [];
        mem_post = [Var.Global 0; Var.Local 0];
        globals_pre = [Var.Global 0];
        globals_post = [Var.Global 0];
        state = Relational_domain.of_equality_constraints (Var.Set.of_list [Var.Global 0; Var.Local 0; Var.Return])
            [(Var.Local 0, Var.Return)]
      } in
    check expected actual
end
