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
  let does_not_fail (module_str : string) : unit =
    let module_ = Wasm_module.of_string module_str in
    let _ : Spec.t Cfg.t = analyze_intra1 module_ 0l in
    ()

  let%test_unit "spec analysis does not error on trivial code" =
    does_not_fail "(module
  (type (;0;) (func (param i32) (result i32)))
  (func (;test;) (type 0) (param i32) (result i32)
    i32.const 256
    i32.const 512
    i32.const 0
    select)
  (table (;0;) 1 1 funcref)
  (memory (;0;) 2)
  (global (;0;) (mut i32) (i32.const 66560)))"

  let%test_unit "spec analysis suceeds on simple program with if and globals" =
    does_not_fail "(module
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
  (global (;0;) (mut i32) (i32.const 66560)))"

  let%test_unit "spec analysis suceeds with imported globals" =
    does_not_fail "(module
  (type (;0;) (func))
  (import \"env\" \"DYNAMICTOP_PTR\" (global (;0;) i32))
  (func (;test;) (type 0)
    global.get 1
    global.set 0)
  (table (;0;) 1 1 funcref)
  (memory (;0;) 2)
  (global (;0;) (mut i32) (i32.const 66560)))"

  let%test_unit "spec analysis succeeds with blocks" =
    does_not_fail "(module
  (type (;0;) (func (param i32) (result i32)))
  (func (;test;) (type 0) (param i32) (result i32)
    block
      i32.const 1 ;; This is a branch condition
      br_if 0     ;; The condition depends on var 'Const 1', and this block has index 3
      i32.const 2
      local.get 0
      i32.add
      drop
    end
    local.get 0))"

  let%test_unit "spec analysis succeeds even in the presence of unreachable code" =
    does_not_fail "(module
(type (;0;) (func (param i32) (result i32)))
(func (;0;) (type 0) (param i32) (result i32)
    block
      local.get 0
      br_if 0
      unreachable ;; stack length here is 0, and it is connected to the exit of the function
    end
    local.get 0 ;; stack length here is 1, hence there is a length mismatch
))"

  let%test_unit "spec analysis succeeds even in the presence of unreachable code" =
    does_not_fail "(module
(type (;0;) (func (param i32) (result i32)))
(func (;0;) (type 0) (param i32) (result i32)
    (local i32 i32)
    block
      i32.const 3
      local.get 0
      local.set 1
      br 0
      i32.const 1 ;; unreachable
      local.set 0 ;; also unreachable
    end
    local.get 1))"

  let%test_unit "spec analysis suceeds even in the presence of stack-polymorphic instructions" =
    does_not_fail  "(module
(type (;0;) (func (param i32) (result i32)))
(func (;0;) (type 0) (param i32) (result i32)
    (local i32 i32)
    block (result i32)
      i32.const 0
      if
        i32.const 1
        i32.const 2
        i32.const 3
        br 0
      else
      end
      i32.const 4
    end))"

end
