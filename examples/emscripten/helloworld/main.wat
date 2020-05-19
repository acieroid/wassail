(module
  (type (;0;) (func (param i32 i32 i32) (result i32)))
  (type (;1;) (func))
  (type (;2;) (func (param i32) (result i32)))
  (type (;3;) (func (param i32 i32) (result i32)))
  (type (;4;) (func (param i32)))
  (type (;5;) (func (result i32)))
  (type (;6;) (func (param i32 i32 i32)))
  (type (;7;) (func (param i32 i32 i32 i32) (result i32)))
  (type (;8;) (func (param i32 i64 i32) (result i64)))
  (import "wasi_snapshot_preview1" "fd_write" (func (;0;) (type 7)))
  (import "wasi_snapshot_preview1" "proc_exit" (func (;1;) (type 4)))
  (func (;2;) (type 2) (param i32) (result i32)
    (local i32)
    local.get 0
    local.get 0
    i32.load8_u offset=74
    local.tee 1
    i32.const -1
    i32.add
    local.get 1
    i32.or
    i32.store8 offset=74
    local.get 0
    i32.load
    local.tee 1
    i32.const 8
    i32.and
    if  ;; label = @1
      local.get 0
      local.get 1
      i32.const 32
      i32.or
      i32.store
      i32.const -1
      return
    end
    local.get 0
    i64.const 0
    i64.store offset=4 align=4
    local.get 0
    local.get 0
    i32.load offset=44
    local.tee 1
    i32.store offset=28
    local.get 0
    local.get 1
    i32.store offset=20
    local.get 0
    local.get 1
    local.get 0
    i32.load offset=48
    i32.add
    i32.store offset=16
    i32.const 0)
  (func (;3;) (type 2) (param i32) (result i32)
    local.get 0
    i32.eqz
    if  ;; label = @1
      i32.const 0
      return
    end
    i32.const 1200
    local.get 0
    i32.store
    i32.const -1)
  (func (;4;) (type 3) (param i32 i32) (result i32)
    call 7)
  (func (;5;) (type 3) (param i32 i32) (result i32)
    (local i32 i32 i32 i32 i32)
    i32.const 1024
    local.set 4
    block  ;; label = @1
      local.get 1
      i32.load offset=16
      local.tee 2
      if (result i32)  ;; label = @2
        local.get 2
      else
        local.get 1
        call 2
        br_if 1 (;@1;)
        local.get 1
        i32.load offset=16
      end
      local.get 1
      i32.load offset=20
      local.tee 5
      i32.sub
      local.get 0
      i32.lt_u
      if  ;; label = @2
        local.get 1
        i32.const 1024
        local.get 0
        local.get 1
        i32.load offset=36
        call_indirect (type 0)
        return
      end
      block  ;; label = @2
        local.get 1
        i32.load8_s offset=75
        i32.const 0
        i32.lt_s
        br_if 0 (;@2;)
        local.get 0
        local.set 3
        loop  ;; label = @3
          local.get 3
          local.tee 2
          i32.eqz
          br_if 1 (;@2;)
          local.get 2
          i32.const -1
          i32.add
          local.tee 3
          i32.const 1024
          i32.add
          i32.load8_u
          i32.const 10
          i32.ne
          br_if 0 (;@3;)
        end
        local.get 1
        i32.const 1024
        local.get 2
        local.get 1
        i32.load offset=36
        call_indirect (type 0)
        local.tee 3
        local.get 2
        i32.lt_u
        br_if 1 (;@1;)
        local.get 0
        local.get 2
        i32.sub
        local.set 0
        local.get 2
        i32.const 1024
        i32.add
        local.set 4
        local.get 1
        i32.load offset=20
        local.set 5
        local.get 2
        local.set 6
      end
      local.get 5
      local.get 4
      local.get 0
      call 6
      drop
      local.get 1
      local.get 1
      i32.load offset=20
      local.get 0
      i32.add
      i32.store offset=20
      local.get 0
      local.get 6
      i32.add
      local.set 3
    end
    local.get 3)
  (func (;6;) (type 0) (param i32 i32 i32) (result i32)
    (local i32 i32 i32)
    local.get 2
    i32.const 512
    i32.ge_u
    if  ;; label = @1
      local.get 0
      local.get 1
      local.get 2
      call 13
      local.get 0
      return
    end
    local.get 0
    local.get 2
    i32.add
    local.set 3
    block  ;; label = @1
      local.get 0
      local.get 1
      i32.xor
      i32.const 3
      i32.and
      i32.eqz
      if  ;; label = @2
        block  ;; label = @3
          local.get 2
          i32.const 1
          i32.lt_s
          if  ;; label = @4
            local.get 0
            local.set 2
            br 1 (;@3;)
          end
          local.get 0
          i32.const 3
          i32.and
          i32.eqz
          if  ;; label = @4
            local.get 0
            local.set 2
            br 1 (;@3;)
          end
          local.get 0
          local.set 2
          loop  ;; label = @4
            local.get 2
            local.get 1
            i32.load8_u
            i32.store8
            local.get 1
            i32.const 1
            i32.add
            local.set 1
            local.get 2
            i32.const 1
            i32.add
            local.tee 2
            local.get 3
            i32.ge_u
            br_if 1 (;@3;)
            local.get 2
            i32.const 3
            i32.and
            br_if 0 (;@4;)
          end
        end
        block  ;; label = @3
          local.get 3
          i32.const -4
          i32.and
          local.tee 4
          i32.const 64
          i32.lt_u
          br_if 0 (;@3;)
          local.get 2
          local.get 4
          i32.const -64
          i32.add
          local.tee 5
          i32.gt_u
          br_if 0 (;@3;)
          loop  ;; label = @4
            local.get 2
            local.get 1
            i32.load
            i32.store
            local.get 2
            local.get 1
            i32.load offset=4
            i32.store offset=4
            local.get 2
            local.get 1
            i32.load offset=8
            i32.store offset=8
            local.get 2
            local.get 1
            i32.load offset=12
            i32.store offset=12
            local.get 2
            local.get 1
            i32.load offset=16
            i32.store offset=16
            local.get 2
            local.get 1
            i32.load offset=20
            i32.store offset=20
            local.get 2
            local.get 1
            i32.load offset=24
            i32.store offset=24
            local.get 2
            local.get 1
            i32.load offset=28
            i32.store offset=28
            local.get 2
            local.get 1
            i32.load offset=32
            i32.store offset=32
            local.get 2
            local.get 1
            i32.load offset=36
            i32.store offset=36
            local.get 2
            local.get 1
            i32.load offset=40
            i32.store offset=40
            local.get 2
            local.get 1
            i32.load offset=44
            i32.store offset=44
            local.get 2
            local.get 1
            i32.load offset=48
            i32.store offset=48
            local.get 2
            local.get 1
            i32.load offset=52
            i32.store offset=52
            local.get 2
            local.get 1
            i32.load offset=56
            i32.store offset=56
            local.get 2
            local.get 1
            i32.load offset=60
            i32.store offset=60
            local.get 1
            i32.const -64
            i32.sub
            local.set 1
            local.get 2
            i32.const -64
            i32.sub
            local.tee 2
            local.get 5
            i32.le_u
            br_if 0 (;@4;)
          end
        end
        local.get 2
        local.get 4
        i32.ge_u
        br_if 1 (;@1;)
        loop  ;; label = @3
          local.get 2
          local.get 1
          i32.load
          i32.store
          local.get 1
          i32.const 4
          i32.add
          local.set 1
          local.get 2
          i32.const 4
          i32.add
          local.tee 2
          local.get 4
          i32.lt_u
          br_if 0 (;@3;)
        end
        br 1 (;@1;)
      end
      local.get 3
      i32.const 4
      i32.lt_u
      if  ;; label = @2
        local.get 0
        local.set 2
        br 1 (;@1;)
      end
      local.get 3
      i32.const -4
      i32.add
      local.tee 4
      local.get 0
      i32.lt_u
      if  ;; label = @2
        local.get 0
        local.set 2
        br 1 (;@1;)
      end
      local.get 0
      local.set 2
      loop  ;; label = @2
        local.get 2
        local.get 1
        i32.load8_u
        i32.store8
        local.get 2
        local.get 1
        i32.load8_u offset=1
        i32.store8 offset=1
        local.get 2
        local.get 1
        i32.load8_u offset=2
        i32.store8 offset=2
        local.get 2
        local.get 1
        i32.load8_u offset=3
        i32.store8 offset=3
        local.get 1
        i32.const 4
        i32.add
        local.set 1
        local.get 2
        i32.const 4
        i32.add
        local.tee 2
        local.get 4
        i32.le_u
        br_if 0 (;@2;)
      end
    end
    local.get 2
    local.get 3
    i32.lt_u
    if  ;; label = @1
      loop  ;; label = @2
        local.get 2
        local.get 1
        i32.load8_u
        i32.store8
        local.get 1
        i32.const 1
        i32.add
        local.set 1
        local.get 2
        i32.const 1
        i32.add
        local.tee 2
        local.get 3
        i32.ne
        br_if 0 (;@2;)
      end
    end
    local.get 0)
  (func (;7;) (type 5) (result i32)
    call 15
    i32.const 0)
  (func (;8;) (type 1)
    nop)
  (func (;9;) (type 8) (param i32 i64 i32) (result i64)
    i64.const 0)
  (func (;10;) (type 2) (param i32) (result i32)
    i32.const 0)
  (func (;11;) (type 0) (param i32 i32 i32) (result i32)
    (local i32 i32 i32 i32 i32 i32)
    global.get 0
    i32.const 32
    i32.sub
    local.tee 3
    global.set 0
    local.get 3
    local.get 0
    i32.load offset=28
    local.tee 4
    i32.store offset=16
    local.get 0
    i32.load offset=20
    local.set 5
    local.get 3
    local.get 2
    i32.store offset=28
    local.get 3
    local.get 1
    i32.store offset=24
    local.get 3
    local.get 5
    local.get 4
    i32.sub
    local.tee 1
    i32.store offset=20
    local.get 1
    local.get 2
    i32.add
    local.set 4
    i32.const 2
    local.set 6
    local.get 3
    i32.const 16
    i32.add
    local.set 1
    block (result i32)  ;; label = @1
      block  ;; label = @2
        block  ;; label = @3
          local.get 0
          i32.load offset=60
          local.get 3
          i32.const 16
          i32.add
          i32.const 2
          local.get 3
          i32.const 12
          i32.add
          call 0
          call 3
          i32.eqz
          if  ;; label = @4
            loop  ;; label = @5
              local.get 4
              local.get 3
              i32.load offset=12
              local.tee 5
              i32.eq
              br_if 2 (;@3;)
              local.get 5
              i32.const -1
              i32.le_s
              br_if 3 (;@2;)
              local.get 1
              i32.const 8
              i32.add
              local.get 1
              local.get 5
              local.get 1
              i32.load offset=4
              local.tee 7
              i32.gt_u
              local.tee 8
              select
              local.tee 1
              local.get 5
              local.get 7
              i32.const 0
              local.get 8
              select
              i32.sub
              local.tee 7
              local.get 1
              i32.load
              i32.add
              i32.store
              local.get 1
              local.get 1
              i32.load offset=4
              local.get 7
              i32.sub
              i32.store offset=4
              local.get 4
              local.get 5
              i32.sub
              local.set 4
              local.get 0
              i32.load offset=60
              local.get 1
              local.get 6
              local.get 8
              i32.sub
              local.tee 6
              local.get 3
              i32.const 12
              i32.add
              call 0
              call 3
              i32.eqz
              br_if 0 (;@5;)
            end
          end
          local.get 3
          i32.const -1
          i32.store offset=12
          local.get 4
          i32.const -1
          i32.ne
          br_if 1 (;@2;)
        end
        local.get 0
        local.get 0
        i32.load offset=44
        local.tee 1
        i32.store offset=28
        local.get 0
        local.get 1
        i32.store offset=20
        local.get 0
        local.get 1
        local.get 0
        i32.load offset=48
        i32.add
        i32.store offset=16
        local.get 2
        br 1 (;@1;)
      end
      local.get 0
      i32.const 0
      i32.store offset=28
      local.get 0
      i64.const 0
      i64.store offset=16
      local.get 0
      local.get 0
      i32.load
      i32.const 32
      i32.or
      i32.store
      i32.const 0
      local.get 6
      i32.const 2
      i32.eq
      br_if 0 (;@1;)
      drop
      local.get 2
      local.get 1
      i32.load offset=4
      i32.sub
    end
    local.set 4
    local.get 3
    i32.const 32
    i32.add
    global.set 0
    local.get 4)
  (func (;12;) (type 1)
    (local i32)
    call 7
    local.tee 0
    if  ;; label = @1
      local.get 0
      call 1
      unreachable
    end)
  (func (;13;) (type 6) (param i32 i32 i32)
    (local i32)
    local.get 2
    if  ;; label = @1
      loop  ;; label = @2
        local.get 0
        local.get 1
        local.get 2
        i32.const 508
        local.get 2
        i32.const 508
        i32.lt_u
        select
        local.tee 3
        call 6
        local.set 0
        local.get 1
        i32.const 508
        i32.add
        local.set 1
        local.get 0
        i32.const 508
        i32.add
        local.set 0
        local.get 2
        local.get 3
        i32.sub
        local.tee 2
        br_if 0 (;@2;)
      end
    end)
  (func (;14;) (type 5) (result i32)
    (local i32 i32 i32)
    i32.const 1024
    local.set 1
    loop  ;; label = @1
      local.get 1
      local.tee 0
      i32.const 4
      i32.add
      local.set 1
      local.get 0
      i32.load
      local.tee 2
      i32.const -1
      i32.xor
      local.get 2
      i32.const -16843009
      i32.add
      i32.and
      i32.const -2139062144
      i32.and
      i32.eqz
      br_if 0 (;@1;)
    end
    local.get 2
    i32.const 255
    i32.and
    i32.eqz
    if  ;; label = @1
      local.get 0
      i32.const 1024
      i32.sub
      return
    end
    loop  ;; label = @1
      local.get 0
      i32.load8_u offset=1
      local.set 2
      local.get 0
      i32.const 1
      i32.add
      local.tee 1
      local.set 0
      local.get 2
      br_if 0 (;@1;)
    end
    local.get 1
    i32.const 1024
    i32.sub)
  (func (;15;) (type 1)
    (local i32 i32)
    i32.const 1040
    i32.load
    local.tee 0
    i32.load offset=76
    i32.const 0
    i32.ge_s
    if (result i32)  ;; label = @1
      i32.const 1
    else
      i32.const 0
    end
    drop
    block  ;; label = @1
      i32.const -1
      i32.const 0
      call 14
      local.tee 1
      local.get 0
      call 17
      local.get 1
      i32.ne
      select
      i32.const 0
      i32.lt_s
      br_if 0 (;@1;)
      block  ;; label = @2
        local.get 0
        i32.load8_u offset=75
        i32.const 10
        i32.eq
        br_if 0 (;@2;)
        local.get 0
        i32.load offset=20
        local.tee 1
        local.get 0
        i32.load offset=16
        i32.ge_u
        br_if 0 (;@2;)
        local.get 0
        local.get 1
        i32.const 1
        i32.add
        i32.store offset=20
        local.get 1
        i32.const 10
        i32.store8
        br 1 (;@1;)
      end
      local.get 0
      call 16
    end)
  (func (;16;) (type 4) (param i32)
    (local i32 i32 i32)
    global.get 0
    i32.const 16
    i32.sub
    local.tee 1
    global.set 0
    local.get 1
    i32.const 10
    i32.store8 offset=15
    block  ;; label = @1
      local.get 0
      i32.load offset=16
      local.tee 2
      i32.eqz
      if  ;; label = @2
        local.get 0
        call 2
        br_if 1 (;@1;)
        local.get 0
        i32.load offset=16
        local.set 2
      end
      block  ;; label = @2
        local.get 0
        i32.load offset=20
        local.tee 3
        local.get 2
        i32.ge_u
        br_if 0 (;@2;)
        local.get 0
        i32.load8_s offset=75
        i32.const 10
        i32.eq
        br_if 0 (;@2;)
        local.get 0
        local.get 3
        i32.const 1
        i32.add
        i32.store offset=20
        local.get 3
        i32.const 10
        i32.store8
        br 1 (;@1;)
      end
      local.get 0
      local.get 1
      i32.const 15
      i32.add
      i32.const 1
      local.get 0
      i32.load offset=36
      call_indirect (type 0)
      i32.const 1
      i32.ne
      br_if 0 (;@1;)
      local.get 1
      i32.load8_u offset=15
      drop
    end
    local.get 1
    i32.const 16
    i32.add
    global.set 0)
  (func (;17;) (type 3) (param i32 i32) (result i32)
    block (result i32)  ;; label = @1
      local.get 1
      i32.load offset=76
      i32.const -1
      i32.le_s
      if  ;; label = @2
        local.get 0
        local.get 1
        call 5
        br 1 (;@1;)
      end
      local.get 0
      local.get 1
      call 5
    end
    local.tee 1
    local.get 0
    i32.eq
    if  ;; label = @1
      local.get 0
      return
    end
    local.get 1)
  (table (;0;) 6 6 funcref)
  (memory (;0;) 256 256)
  (global (;0;) (mut i32) (i32.const 5245632))
  (export "memory" (memory 0))
  (export "main" (func 4))
  (export "_start" (func 12))
  (elem (;0;) (i32.const 1) 8 4 10 11 9)
  (data (;0;) (i32.const 1024) "hello, world!\00\00\00\18\04\00\00\00\00\00\00\05")
  (data (;1;) (i32.const 1060) "\03")
  (data (;2;) (i32.const 1084) "\04\00\00\00\05\00\00\00\c8\04\00\00\00\04")
  (data (;3;) (i32.const 1108) "\01")
  (data (;4;) (i32.const 1123) "\0a\ff\ff\ff\ff")
  (data (;5;) (i32.const 2752) "`\0bP"))
