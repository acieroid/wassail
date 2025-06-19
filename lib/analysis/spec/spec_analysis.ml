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
  let does_not_fail (module_str : string) (fidx : int32) : unit =
    let module_ = Wasm_module.of_string module_str in
    let _ : Spec.t Cfg.t = analyze_intra1 module_ fidx in
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
  (global (;0;) (mut i32) (i32.const 66560)))" 0l

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
  (global (;0;) (mut i32) (i32.const 66560)))" 0l

  let%test_unit "spec analysis suceeds with imported globals" =
    does_not_fail "(module
  (type (;0;) (func))
  (import \"env\" \"DYNAMICTOP_PTR\" (global (;0;) i32))
  (func (;test;) (type 0)
    global.get 1
    global.set 0)
  (table (;0;) 1 1 funcref)
  (memory (;0;) 2)
  (global (;0;) (mut i32) (i32.const 66560)))" 0l

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
    local.get 0))" 0l

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
))" 0l

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
    local.get 1))" 0l

  let%test_unit "spec analysis succeeds even in the presence of stack-polymorphic instructions" =
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
    end))" 0l

  let%test_unit "spec analysis succeeds on example from the wild" =
    does_not_fail "(module
(type (;0;) (func (param i32 i32) (result i32)))
(func (;0;) (type 0) (param i32 i32) (result i32)
    (local i32)
    local.get 0 ;; [_]
    if (result i32) ;; [] INSTR 1
      block (result i32) ;; INSTR 2
        i32.const 8 ;; [_]
        local.tee 2 ;; [_]
        drop ;; []
        local.get 2 ;; [_]
        local.tee 1  ;; [_]
      end
      if (result i32) ;; [] ;; INSTR 8
        local.get 0 ;; [_]
        local.get 1 ;; [_, _]
        i32.store ;; []
        i32.const 0 ;; [_]
      else
        i32.const -16 ;; [_]
      end
    else
      i32.const -10420289 ;; [_]
    end)
)" 0l

  let%test_unit "spec analysis succeeds on word count" =
    does_not_fail "(module
  (type (;0;) (func))
  (type (;1;) (func (result i32)))
  (func (;0;) (type 1) ;; char getchar()
    i32.const 0)
  (func (;1;) (type 0) ;; void main()
    (local i32 i32 i32 i32 i32)
    ;; Local 0: c
    ;; Local 1: nl
    ;; Local 2: nw
    ;; Local 3: nc
    ;; Local 4: inword
    ;; EOF = -1
    ;; '\\n' = 10
    ;; ' ' = 32
    ;; '\\t' = 9
    call 0 ;; getchar();
    local.tee 0 ;; c = result of getchar();
    i32.const 0 ;; EOF
    i32.ne ;; c != EOF
    if ;; label = @1
      loop ;; label = @2
        local.get 3
        i32.const 1
        i32.add
        local.set 3 ;; nc = nc + 1
        local.get 0
        i32.const 10
        i32.eq ;; c = '\\n'
        if
          local.get 1
          i32.const 1
          i32.add
          local.set 1 ;; nl = nl + 1
        end
        local.get 0
        i32.const 32
        i32.eq ;; c == ' '
        ;; In the original program, the condition is c == ' ' || c == '\\n' || c = '\\t'
        if
          i32.const 0
          local.set 4 ;; inword = NO
        else
          local.get 4
          if ;; inword == NO
            i32.const 1
            local.set 4 ;; inword = YES
            local.get 2
            i32.const 1
            i32.add
            local.set 2 ;; nw = nw + 1
          end
        end
        call 0
        local.tee 0
        i32.const 0 ;; EOF
        i32.ne ;; c != EOF
        br_if 0
      end
    end
    local.get 0 ;; c
    drop
    local.get 1 ;; nl
    drop
    local.get 2 ;; nw
    drop
    local.get 3 ;; nc
    drop
    local.get 4 ;; inword
    drop))" 1l

  let%test_unit "spec inference on slice of word count should not fail" =
    does_not_fail "(module
(type (;0;) (func))
(type (;1;) (func (result i32)))
(func (;0;) (type 1)
  i32.const 0)
(func (;1;) (type 0)
(local i32 i32 i32 i32 i32)
  call 0  ;; getchar()
  local.tee 0  ;; c = getchar();
  i32.const 0
  i32.ne ;; c != EOF
  if
    loop
      call 0 ;; getchar()
      local.tee 0 ;; c = getchar()
      i32.const 0
      i32.ne ;; c != EOF
      br_if 0
    end
  end
  local.get 0 ;; c
  drop
))" 1l

  let%test_unit "spec inference on call in block should not fail" =
    let program = "(module
(type (;0;) (func))
(type (;1;) (func (result i32)))
(func (;0;) (type 1)
  i32.const 0)
(func (;1;) (type 0)
(local i32 i32 i32 i32 i32)
  block
    call 0
    br_if 0
  end
))" in
    let module_ = Wasm_module.of_string program in
    let cfg = Cfg_builder.build module_ 1l in
    let _ = Spec_inference.Intra.analyze module_ cfg () in
    ()


end
