(module
  (type (;0;) (func))
  (type (;1;) (func (result i32)))
  (type (;2;) (func (param i32)))
  (type (;3;) (func (param i32) (result i32)))
  (import "wasi_snapshot_preview1" "proc_exit" (func (;0;) (type 2)))
  (func (;1;) (type 0)
    (local i32)
    call 4
    local.tee 0
    if  ;; label = @1
      local.get 0
      call 0
      unreachable
    end)
  (func (;2;) (type 1) (result i32)
    (local i32 i32 i32 i32 i32)
    i32.const 1168
    i32.load
    local.tee 1
    if  ;; label = @1
      loop  ;; label = @2
        local.get 0
        i32.const 1040
        i32.add
        local.get 0
        i32.const 1104
        i32.add
        i32.load
        i32.store
        local.get 0
        i32.const 4
        i32.add
        local.set 0
        local.get 1
        i32.const -1
        i32.add
        local.tee 1
        br_if 0 (;@2;)
      end
    end
    i32.const 1040
    i32.load
    local.set 0
    i32.const 1
    local.set 3
    loop  ;; label = @1
      local.get 3
      i32.const 1
      i32.add
      local.set 3
      local.get 0
      i32.const 1
      i32.ge_s
      if  ;; label = @2
        i32.const 1040
        local.get 0
        i32.const 2
        i32.shl
        local.tee 1
        i32.const 1040
        i32.add
        local.tee 2
        i32.load
        i32.store
        local.get 2
        local.get 0
        i32.store
        local.get 1
        i32.const 1036
        i32.add
        local.tee 0
        i32.const 1044
        i32.gt_u
        if  ;; label = @3
          i32.const 1048
          local.set 1
          loop  ;; label = @4
            local.get 1
            i32.const -4
            i32.add
            local.tee 2
            i32.load
            local.set 4
            local.get 2
            local.get 0
            i32.load
            i32.store
            local.get 0
            local.get 4
            i32.store
            local.get 1
            local.get 0
            i32.const -4
            i32.add
            local.tee 0
            i32.lt_u
            local.get 1
            i32.const 4
            i32.add
            local.set 1
            br_if 0 (;@4;)
          end
        end
        i32.const 1040
        i32.load
        local.set 0
      end
      local.get 0
      i32.const 2
      i32.shl
      i32.const 1040
      i32.add
      i32.load
      br_if 0 (;@1;)
    end
    local.get 3)
  (func (;3;) (type 0)
    (local i32 i32 i32 i32 i32)
    global.get 0
    i32.const -64
    i32.add
    local.tee 0
    global.set 0
    local.get 0
    call 5
    local.set 4
    loop  ;; label = @1
      i32.const 1104
      i32.load
      local.set 2
      local.get 1
      i32.const 1
      i32.ge_s
      if  ;; label = @2
        i32.const 1104
        local.set 3
        local.get 1
        local.set 0
        loop  ;; label = @3
          local.get 3
          local.get 3
          i32.const 4
          i32.add
          local.tee 3
          i32.load
          i32.store
          local.get 0
          i32.const -1
          i32.add
          local.tee 0
          br_if 0 (;@3;)
        end
      end
      local.get 1
      i32.const 2
      i32.shl
      local.tee 0
      i32.const 1104
      i32.add
      local.get 2
      i32.store
      block (result i32)  ;; label = @2
        local.get 0
        local.get 4
        i32.add
        local.tee 2
        i32.load
        local.tee 0
        local.get 1
        i32.ge_s
        if  ;; label = @3
          local.get 2
          i32.const 0
          i32.store
          local.get 1
          i32.const 1
          i32.add
          br 1 (;@2;)
        end
        local.get 2
        local.get 0
        i32.const 1
        i32.add
        i32.store
        i32.const 1028
        i32.const 1028
        i32.load
        i32.const -1
        i32.xor
        i32.store
        i32.const 1
        i32.const 1104
        i32.load
        local.tee 0
        i32.eqz
        br_if 0 (;@2;)
        drop
        block (result i32)  ;; label = @3
          i32.const 1
          local.get 0
          i32.const 2
          i32.shl
          i32.const 1104
          i32.add
          i32.load
          i32.eqz
          br_if 0 (;@3;)
          drop
          call 2
        end
        local.tee 0
        i32.const 1024
        i32.load
        i32.gt_s
        if  ;; label = @3
          i32.const 1024
          local.get 0
          i32.store
        end
        i32.const 1032
        i32.const 1032
        i32.load
        i32.const 0
        local.get 0
        i32.sub
        local.get 0
        i32.const 1028
        i32.load
        select
        i32.add
        i32.store
        i32.const 1
      end
      local.tee 1
      i32.const 8
      i32.lt_s
      br_if 0 (;@1;)
    end
    local.get 4
    i32.const -64
    i32.sub
    global.set 0)
  (func (;4;) (type 1) (result i32)
    (local i32 i32)
    i32.const 1168
    i32.const 8
    i32.store
    i32.const 1104
    local.set 1
    loop  ;; label = @1
      local.get 1
      local.get 0
      i32.store
      local.get 1
      i32.const 4
      i32.add
      local.set 1
      local.get 0
      i32.const 1
      i32.add
      local.tee 0
      i32.const 8
      i32.ne
      br_if 0 (;@1;)
    end
    call 3
    i32.const 0)
  (func (;5;) (type 3) (param i32) (result i32)
    (local i32 i32 i32)
    local.get 0
    i32.const 0
    i32.store8
    local.get 0
    i32.const -64
    i32.sub
    local.tee 1
    i32.const -1
    i32.add
    i32.const 0
    i32.store8
    local.get 0
    i32.const 0
    i32.store8 offset=2
    local.get 0
    i32.const 0
    i32.store8 offset=1
    local.get 1
    i32.const -3
    i32.add
    i32.const 0
    i32.store8
    local.get 1
    i32.const -2
    i32.add
    i32.const 0
    i32.store8
    local.get 0
    i32.const 0
    i32.store8 offset=3
    local.get 1
    i32.const -4
    i32.add
    i32.const 0
    i32.store8
    local.get 0
    i32.const 0
    local.get 0
    i32.sub
    i32.const 3
    i32.and
    local.tee 2
    i32.add
    local.tee 1
    i32.const 0
    i32.store
    local.get 1
    i32.const 64
    local.get 2
    i32.sub
    i32.const -4
    i32.and
    local.tee 3
    i32.add
    local.tee 2
    i32.const -4
    i32.add
    i32.const 0
    i32.store
    block  ;; label = @1
      local.get 3
      i32.const 9
      i32.lt_u
      br_if 0 (;@1;)
      local.get 1
      i32.const 0
      i32.store offset=8
      local.get 1
      i32.const 0
      i32.store offset=4
      local.get 2
      i32.const -8
      i32.add
      i32.const 0
      i32.store
      local.get 2
      i32.const -12
      i32.add
      i32.const 0
      i32.store
      local.get 3
      i32.const 25
      i32.lt_u
      br_if 0 (;@1;)
      local.get 1
      i32.const 0
      i32.store offset=24
      local.get 1
      i32.const 0
      i32.store offset=20
      local.get 1
      i32.const 0
      i32.store offset=16
      local.get 1
      i32.const 0
      i32.store offset=12
      local.get 2
      i32.const -16
      i32.add
      i32.const 0
      i32.store
      local.get 2
      i32.const -20
      i32.add
      i32.const 0
      i32.store
      local.get 2
      i32.const -24
      i32.add
      i32.const 0
      i32.store
      local.get 2
      i32.const -28
      i32.add
      i32.const 0
      i32.store
      local.get 3
      local.get 1
      i32.const 4
      i32.and
      i32.const 24
      i32.or
      local.tee 3
      i32.sub
      local.tee 2
      i32.const 32
      i32.lt_u
      br_if 0 (;@1;)
      local.get 1
      local.get 3
      i32.add
      local.set 1
      loop  ;; label = @2
        local.get 1
        i64.const 0
        i64.store
        local.get 1
        i32.const 24
        i32.add
        i64.const 0
        i64.store
        local.get 1
        i32.const 16
        i32.add
        i64.const 0
        i64.store
        local.get 1
        i32.const 8
        i32.add
        i64.const 0
        i64.store
        local.get 1
        i32.const 32
        i32.add
        local.set 1
        local.get 2
        i32.const -32
        i32.add
        local.tee 2
        i32.const 31
        i32.gt_u
        br_if 0 (;@2;)
      end
    end
    local.get 0)
  (memory (;0;) 2)
  (global (;0;) (mut i32) (i32.const 66720))
  (export "memory" (memory 0))
  (export "_start" (func 1)))
