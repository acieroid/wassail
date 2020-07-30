(module
  (type (;0;) (func (param i32 i32)))
  (type (;1;) (func))
  (type (;2;) (func (param i32)))
  (type (;3;) (func (result i32)))
  (import "wasi_snapshot_preview1" "proc_exit" (func (;0;) (type 2)))
  (func (;1;) (type 1)
    (local i32)
    call 4
    local.tee 0
    if  ;; label = @1
      local.get 0
      call 0
      unreachable
    end)
  (func (;2;) (type 0) (param i32 i32)
    (local i32 i32 i32 i32 i32 i32 f64)
    loop  ;; label = @1
      local.get 1
      local.get 2
      i32.const 3
      i32.shl
      i32.add
      local.tee 6
      i64.const 0
      i64.store
      local.get 2
      i32.const 1
      i32.add
      local.set 5
      f64.const 0x0p+0 (;=0;)
      local.set 8
      local.get 0
      local.set 3
      i32.const 0
      local.set 4
      loop  ;; label = @2
        local.get 6
        local.get 8
        f64.const 0x1p+0 (;=1;)
        local.get 5
        local.get 2
        local.get 4
        i32.add
        local.tee 7
        i32.const 1
        i32.add
        local.get 7
        i32.mul
        i32.const 1
        i32.shr_u
        i32.add
        f64.convert_i32_s
        f64.div
        local.get 3
        f64.load
        f64.mul
        f64.add
        local.tee 8
        f64.store
        local.get 3
        i32.const 8
        i32.add
        local.set 3
        local.get 4
        i32.const 1
        i32.add
        local.tee 4
        i32.const 2000
        i32.ne
        br_if 0 (;@2;)
      end
      local.get 5
      local.tee 2
      i32.const 2000
      i32.ne
      br_if 0 (;@1;)
    end)
  (func (;3;) (type 0) (param i32 i32)
    (local i32 i32 i32 i32 i32 i32 i32 i32 f64)
    i32.const 2
    local.set 2
    loop  ;; label = @1
      local.get 1
      local.get 7
      i32.const 3
      i32.shl
      i32.add
      local.tee 9
      i64.const 0
      i64.store
      f64.const 0x0p+0 (;=0;)
      local.set 10
      local.get 8
      local.set 3
      local.get 2
      local.set 4
      local.get 0
      local.set 5
      i32.const 0
      local.set 6
      loop  ;; label = @2
        local.get 9
        local.get 10
        f64.const 0x1p+0 (;=1;)
        local.get 3
        i32.const 1
        i32.shr_u
        local.get 6
        i32.add
        i32.const 1
        i32.add
        f64.convert_i32_s
        f64.div
        local.get 5
        f64.load
        f64.mul
        f64.add
        local.tee 10
        f64.store
        local.get 3
        local.get 4
        i32.add
        local.set 3
        local.get 5
        i32.const 8
        i32.add
        local.set 5
        local.get 4
        i32.const 2
        i32.add
        local.set 4
        local.get 6
        i32.const 1
        i32.add
        local.tee 6
        i32.const 2000
        i32.ne
        br_if 0 (;@2;)
      end
      local.get 2
      local.get 8
      i32.add
      local.set 8
      local.get 2
      i32.const 2
      i32.add
      local.set 2
      local.get 7
      i32.const 1
      i32.add
      local.tee 7
      i32.const 2000
      i32.ne
      br_if 0 (;@1;)
    end)
  (func (;4;) (type 3) (result i32)
    (local i32 i32)
    global.get 0
    i32.const 48000
    i32.sub
    local.tee 0
    global.set 0
    loop  ;; label = @1
      local.get 0
      i32.const 16000
      i32.add
      local.get 1
      i32.add
      i64.const 4607182418800017408
      i64.store
      local.get 1
      i32.const 8
      i32.add
      local.tee 1
      i32.const 16000
      i32.ne
      br_if 0 (;@1;)
    end
    i32.const 10
    local.set 1
    loop  ;; label = @1
      local.get 0
      i32.const 16000
      i32.add
      local.get 0
      i32.const 32000
      i32.add
      call 2
      local.get 0
      i32.const 32000
      i32.add
      local.get 0
      call 3
      local.get 0
      local.get 0
      i32.const 32000
      i32.add
      call 2
      local.get 0
      i32.const 32000
      i32.add
      local.get 0
      i32.const 16000
      i32.add
      call 3
      local.get 1
      i32.const -1
      i32.add
      local.tee 1
      br_if 0 (;@1;)
    end
    local.get 0
    i32.const 48000
    i32.add
    global.set 0
    i32.const 0)
  (memory (;0;) 2)
  (global (;0;) (mut i32) (i32.const 66560))
  (export "memory" (memory 0))
  (export "_start" (func 1)))
