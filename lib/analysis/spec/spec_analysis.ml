open Core
open Helpers

let analyze_intra : Wasm_module.t -> Int32.t list -> Spec_domain.t Cfg.t Int32Map.t =
  Analysis_helpers.mk_intra
    (fun _cfgs _wasm_mod -> Int32Map.empty)
    (fun _summaries _wasm_mod cfg -> cfg)

let analyze_intra1 (module_ : Wasm_module.t) (idx : Int32.t) : Spec_domain.t Cfg.t =
  let results = analyze_intra module_ [idx] in
  match Int32Map.find results idx with
  | Some res -> res
  | None -> failwith "Spec_analysis.analyze_intra did not actually analyze"

let analyze_inter_classical (module_ : Wasm_module.t) (entry : Int32.t) : Spec_domain.t Icfg.t =
  Analysis_helpers.mk_inter_classical module_ entry

module TestIntra = struct
  let does_not_fail (module_str : string) (fidx : int32) : unit =
    let module_ = Wasm_module.of_string module_str in
    let _ : Spec_domain.t Cfg.t = analyze_intra1 module_ fidx in
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
    global.get 0
    global.set 1)
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
  (memory (;0;) 2))" 0l

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

module TestInter = struct
  let does_not_fail (module_str : string) (fidx : int32) : unit =
    let module_ = Wasm_module.of_string module_str in
    let icfg = analyze_inter_classical module_ fidx in
    ignore icfg;
    Printf.printf "---\n%s\n---\n" (Icfg.to_dot ~annot_str:Spec_domain.to_dot_string icfg);
    ()

  let does_not_fail_on_file (path : string) (start : Int32.t) : unit =
    let module_ = Wasm_module.of_file path in
    let _ = analyze_inter_classical module_ start in
    ()

  let%test_unit "interprocedural spec analysis does not fail on trivial code" =
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

  let%test_unit "interprocedural spec analysis does not fail with function call" =
    does_not_fail "(module
  (type (;0;) (func (param i32) (result i32)))
  (func (;0;) (type 0) (param i32) (result i32)
    ;; locals: [p0], globals: []
    local.get 0 ;; [l0]
    i32.const 2 ;; [2, l0]
    i32.mul    ;; [iX]

    call 1 ;; [i2]
  )
  (func (;1;) (type 0) (param i32) (result i32)
    ;; []
    local.get 0 ;; [p0]
    i32.const 0 ;; [i1_1, p0]
    i32.add) ;; [i1_2]
  )" 0l

  let%test_unit "interprocedural spec analysis does not fail with function call and a mix of locals and params" =
    does_not_fail "(module
  (type (;0;) (func (param i32) (result i32)))
  (func (;0;) (type 0) (param i32) (result i32)
    ;; locals: [p0], globals: []
    local.get 0 ;; [l0]
    i32.const 2 ;; [2, l0]
    i32.mul    ;; [iX]

    call 1 ;; [i2]
  )
  (func (;1;) (type 0) (param i32) (result i32)
    (local i32 i32)
    ;; []
    local.get 0 ;; [p0]
    local.get 2 ;; [i1_1, p0]
    i32.add) ;; [i1_2]
  )" 0l

  let%test_unit "interprocedural spec analysis works even with imported functions" =
    does_not_fail "(module
  (type (;0;) (func (param i32) (result i32)))
  (import \"foo\" \"somefun\" (func (;0;) (type 0)))
  (func (;1;) (type 0) (param i32) (result i32)
    (local i32)
    local.get 1 ;; [l1]
    call 0 ;; [i2]
  ))" 1l

  let%test_unit "interprocedural spec analysis works with multiple calls to the same function with different locals" =
    does_not_fail "(module
  (type (;0;) (func (param i32) (result i32)))
  (func (;0;) (type 0) (param i32) (result i32)
    local.get 0)
  (func (;1;) (type 0) (param i32) (result i32)
    (local i32)
    local.get 1
    call 0)
  (func (;2;) (type 0) (param i32) (result i32)
    (local i32 i32)
    local.get 2
    call 0)
  (func (;3;) (type 0) (param i32) (result i32)
    i32.const 1
    call 1
    call 2))" 3l

  let%test_unit "interprocedural works on spectral-norm" =
    does_not_fail_on_file "../../../benchmarks/benchmarksgame/spectral-norm.wat" 1l

  let%test_unit "interprocedural spec works on all benchmarks" =
    List.iter [
      (* ("../../../benchmarks/benchmarksgame/binarytrees.wat", 1l); *)
      ("../../../benchmarks/benchmarksgame/fankuchredux.wat", 1l);
      ("../../../benchmarks/benchmarksgame/fasta.wat", 5l);
      (* ("../../../benchmarks/benchmarksgame/k-nucleotide.wat", 4l); *)
      ("../../../benchmarks/benchmarksgame/mandelbrot.wat", 1l);
      ("../../../benchmarks/benchmarksgame/nbody.wat", 1l);
      (* ("../../../benchmarks/benchmarksgame/reverse-complement.wat", 6l); *)
      ("../../../benchmarks/benchmarksgame/spectral-norm.wat", 1l);
      (* ("../../../benchmarks/polybench-clang/2mm.wat", 5l);
      ("../../../benchmarks/polybench-clang/3mm.wat", 5l);
      ("../../../benchmarks/polybench-clang/adi.wat", 5l);
      ("../../../benchmarks/polybench-clang/atax.wat", 5l);
      ("../../../benchmarks/polybench-clang/bicg.wat", 5l);
      ("../../../benchmarks/polybench-clang/cholesky.wat", 5l);
      ("../../../benchmarks/polybench-clang/correlation.wat", 5l);
      ("../../../benchmarks/polybench-clang/covariance.wat", 5l);
      ("../../../benchmarks/polybench-clang/deriche.wat", 5l);
      ("../../../benchmarks/polybench-clang/doitgen.wat", 5l);
      ("../../../benchmarks/polybench-clang/durbin.wat", 5l);
      ("../../../benchmarks/polybench-clang/fdtd-2d.wat", 5l);
      ("../../../benchmarks/polybench-clang/floyd-warshall.wat", 5l);
      ("../../../benchmarks/polybench-clang/gemm.wat", 5l);
      ("../../../benchmarks/polybench-clang/gemver.wat", 5l);
      ("../../../benchmarks/polybench-clang/gesummv.wat", 5l);
      ("../../../benchmarks/polybench-clang/gramschmidt.wat", 5l);
      ("../../../benchmarks/polybench-clang/heat-3d.wat", 5l);
      ("../../../benchmarks/polybench-clang/jacobi-1d.wat", 5l);
      ("../../../benchmarks/polybench-clang/jacobi-2d.wat", 5l);
      ("../../../benchmarks/polybench-clang/ludcmp.wat", 5l);
      ("../../../benchmarks/polybench-clang/lu.wat", 5l);
      ("../../../benchmarks/polybench-clang/mvt.wat", 5l);
      ("../../../benchmarks/polybench-clang/nussinov.wat", 5l);
      ("../../../benchmarks/polybench-clang/seidel-2d.wat", 5l);
      ("../../../benchmarks/polybench-clang/symm.wat", 5l);
      ("../../../benchmarks/polybench-clang/syr2k.wat", 5l);
      ("../../../benchmarks/polybench-clang/syrk.wat", 5l);
      ("../../../benchmarks/polybench-clang/trisolv.wat", 5l);
      ("../../../benchmarks/polybench-clang/trmm.wat", 5l);
      ("../../../test/element-section-func.wat", 5l); *)
    ] ~f:(fun (program, entry) ->
        try
          does_not_fail_on_file program entry
        with e -> failwith (Printf.sprintf "Inter spec failed on %s: %s" program (Exn.to_string e)))

end
