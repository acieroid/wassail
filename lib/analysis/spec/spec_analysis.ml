open Helpers

let analyze_intra : Wasm_module.t -> Int32.t list -> Spec.t Cfg.t Int32Map.t =
  Analysis_helpers.mk_intra
    (fun _cfgs _wasm_mod -> Int32Map.empty)
    (fun _summaries _wasm_mod cfg -> cfg)

let analyze_intra1 (module_ : Wasm_module.t) (idx : Int32.t) : Spec.t Cfg.t =
  let results = analyze_intra module_ [idx] in
  match Int32Map.find results idx with
  | Some res -> res
  | None -> failwith "Spec_analysis.analyze_intra did not actually analyze"


module Test = struct
  let%test_unit "spec analysis does not error on trivial code" =
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
    let _ : Spec.t Cfg.t = analyze_intra1 module_ 0l in
    ()

  let%test_unit "spec analysis suceeds on simple program with if and globals" =
    let module_ = Wasm_module.of_string "(module
  (type (;0;) (func))
  (func (;test;) (type 0)
    global.get 0
    if
      global.get 0
      i32.const 1
      i32.sub
      global.set 0
    end)
  (table (;0;) 1 1 funcref)
  (memory (;0;) 2)
  (global (;0;) (mut i32) (i32.const 66560)))" in
    let _ : Spec.t Cfg.t = analyze_intra1 module_ 0l in
    ()

  let%test_unit "spec analysis suceeds with imported globals" =
    let module_ = Wasm_module.of_string "(module
  (type (;0;) (func))
  (import \"env\" \"DYNAMICTOP_PTR\" (global (;0;) i32))
  (func (;test;) (type 0)
    global.get 1
    global.set 0)
  (table (;0;) 1 1 funcref)
  (memory (;0;) 2)
  (global (;0;) (mut i32) (i32.const 66560)))" in
    let _ : Spec.t Cfg.t = analyze_intra1 module_ 0l in
    ()
end
