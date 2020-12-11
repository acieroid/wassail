open Helpers

let analyze_intra : Wasm_module.t -> int list -> Spec.t Cfg.t IntMap.t =
  Analysis_helpers.mk_intra
    (fun _cfgs _wasm_mod -> IntMap.empty)
    (fun _summaries _wasm_mod cfg -> cfg)

let analyze_intra1 (module_ : Wasm_module.t) (idx : int) : Spec.t Cfg.t =
  let results = analyze_intra module_ [idx] in
  IntMap.find_exn results idx

let%test_unit "spec analysis does not error" =
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
  let _ : Spec.t Cfg.t = analyze_intra1 module_ 0 in
  ()
