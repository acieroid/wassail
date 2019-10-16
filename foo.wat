(module
  (type $t0 (func (param i32) (result i32)))
  (type $t1 (func))
  (type $t2 (func (result i32)))
  (import "env" "sink" (func $sink (type $t0)))
  (func $__wasm_call_ctors (type $t1))
  (func $identity_noopt (export "identity_noopt") (type $t0) (param $p0 i32) (result i32)
    (local $l0 i32)
    get_global $g0
    i32.const 16
    i32.sub
    tee_local $l0
    get_local $p0
    i32.store offset=12
    get_local $l0
    i32.load offset=12)
  (func $identity (export "identity") (type $t0) (param $p0 i32) (result i32)
    get_local $p0)
  (func $foo (export "foo") (type $t0) (param $p0 i32) (result i32)
    block $B0
      get_local $p0
      i32.const 0
      i32.le_s
      br_if $B0
      get_local $p0
      i32.const 1
      i32.add
      return
    end
    get_local $p0
    call $sink)
  (func $fact (export "fact") (type $t0) (param $p0 i32) (result i32)
    (local $l0 i32) (local $l1 i32)
    block $B0
      get_local $p0
      i32.eqz
      br_if $B0
      i32.const 1
      set_local $l0
      loop $L1
        get_local $p0
        get_local $l0
        i32.mul
        set_local $l0
        get_local $p0
        i32.const -1
        i32.add
        tee_local $l1
        set_local $p0
        get_local $l1
        br_if $L1
      end
      get_local $l0
      return
    end
    i32.const 1)
  (func $main (export "main") (type $t2) (result i32)
    i32.const 42)
  (table $T0 1 1 anyfunc)
  (memory $memory (export "memory") 2)
  (global $g0 (mut i32) (i32.const 66560))
  (global $__heap_base (export "__heap_base") i32 (i32.const 66560))
  (global $__data_end (export "__data_end") i32 (i32.const 1024)))
