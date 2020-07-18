(module
  (type (;0;) (func (param i32 i32 i32) (result i32)))
  (type (;1;) (func (param i32 i64 i32) (result i64)))
  (type (;2;) (func (param i32) (result i32)))
  (type (;3;) (func))
  (type (;4;) (func (param i32)))
  (type (;5;) (func (param i32 i32 i32 i32) (result i32)))
  (type (;6;) (func (param i32 i32)))
  (type (;7;) (func (param i32 i32 i32)))
  (type (;8;) (func (result i32)))
  (type (;9;) (func (param i32 i32) (result i32)))
  (type (;10;) (func (param i32 i64 i32 i32) (result i32)))
  (import "wasi_snapshot_preview1" "proc_exit" (func (;0;) (type 4)))
  (import "wasi_snapshot_preview1" "fd_close" (func (;1;) (type 2)))
  (import "wasi_snapshot_preview1" "fd_seek" (func (;2;) (type 10)))
  (import "wasi_snapshot_preview1" "fd_write" (func (;3;) (type 5)))
  (import "wasi_snapshot_preview1" "fd_fdstat_get" (func (;4;) (type 9)))
  (func (;5;) (type 3)
    (local i32)
    call 6
    local.set 0
    call 18
    local.get 0
    if  ;; label = @1
      local.get 0
      call 0
      unreachable
    end)
  (func (;6;) (type 8) (result i32)
    (local i32 i32)
    global.get 0
    i32.const 448
    i32.sub
    local.tee 0
    global.set 0
    i32.const 1024
    i32.const 1568
    i32.load
    local.tee 1
    call 9
    local.get 0
    i32.const 160
    i32.add
    i32.const 1056
    i32.const 288
    call 20
    drop
    local.get 0
    i32.const 160
    i32.add
    call 7
    i32.const 1716
    i32.const 0
    i32.store
    i32.const 1344
    local.get 1
    call 9
    local.get 0
    i32.const 32
    i32.add
    i32.const 1376
    i32.const 120
    call 20
    drop
    local.get 0
    i32.const 32
    i32.add
    i32.const 15
    i32.const 7500
    call 8
    i32.const 1496
    local.get 1
    call 9
    local.get 0
    i32.const 24
    i32.add
    i32.const 1560
    i64.load
    i64.store
    local.get 0
    i32.const 16
    i32.add
    i32.const 1552
    i64.load
    i64.store
    local.get 0
    i32.const 1544
    i64.load
    i64.store offset=8
    local.get 0
    i32.const 1536
    i64.load
    i64.store
    local.get 0
    i32.const 4
    i32.const 12500
    call 8
    local.get 0
    i32.const 448
    i32.add
    global.set 0
    i32.const 0)
  (func (;7;) (type 4) (param i32)
    (local i32 i32 i32 i32 i32 i32 i32)
    global.get 0
    i32.const -64
    i32.add
    local.tee 1
    local.set 4
    local.get 1
    global.set 0
    local.get 1
    local.get 0
    call 21
    local.tee 3
    i32.const 75
    i32.add
    i32.const -16
    i32.and
    i32.sub
    local.tee 5
    global.set 0
    local.get 3
    i32.const -59
    i32.ge_s
    if  ;; label = @1
      local.get 3
      i32.const 60
      i32.add
      local.set 2
      i32.const 0
      local.set 1
      loop  ;; label = @2
        local.get 1
        local.get 5
        i32.add
        local.get 0
        local.get 1
        local.get 3
        i32.rem_s
        i32.add
        i32.load8_u
        i32.store8
        local.get 1
        i32.const 1
        i32.add
        local.tee 1
        local.get 2
        i32.lt_s
        br_if 0 (;@2;)
      end
    end
    local.get 4
    i32.const 10
    i32.store8 offset=60
    i32.const 1568
    i32.load
    local.set 6
    i32.const 5000
    local.set 0
    i32.const 0
    local.set 2
    loop  ;; label = @1
      i32.const 60
      local.set 1
      local.get 4
      local.get 2
      local.get 5
      i32.add
      local.get 0
      i32.const 59
      i32.le_s
      if  ;; label = @2
        local.get 0
        local.get 4
        i32.add
        i32.const 10
        i32.store8
        local.get 0
        local.set 1
      end
      local.get 1
      call 20
      local.tee 7
      local.get 1
      i32.const 1
      i32.add
      i32.const 1
      local.get 6
      call 10
      drop
      local.get 1
      local.get 2
      i32.add
      local.tee 2
      local.get 3
      i32.const 0
      local.get 2
      local.get 3
      i32.gt_s
      select
      i32.sub
      local.set 2
      local.get 0
      local.get 1
      i32.sub
      local.tee 0
      i32.const 0
      i32.gt_s
      br_if 0 (;@1;)
    end
    local.get 7
    i32.const -64
    i32.sub
    global.set 0)
  (func (;8;) (type 7) (param i32 i32 i32)
    (local i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 f32 f32)
    global.get 0
    i32.const 308224
    i32.sub
    local.tee 3
    local.set 7
    local.get 3
    global.set 0
    local.get 3
    local.get 1
    i32.const 2
    i32.shl
    i32.const 15
    i32.add
    i32.const -16
    i32.and
    i32.sub
    local.tee 10
    global.set 0
    local.get 1
    i32.const 1
    i32.lt_s
    local.tee 11
    i32.eqz
    if  ;; label = @1
      local.get 0
      i32.const 4
      i32.add
      local.set 3
      local.get 10
      local.set 5
      local.get 1
      local.set 6
      loop  ;; label = @2
        local.get 5
        block (result i32)  ;; label = @3
          local.get 15
          local.get 3
          f32.load
          f32.add
          local.tee 15
          f32.const 0x1.116p+17 (;=139968;)
          f32.mul
          local.tee 14
          f32.const 0x1p+32 (;=4.29497e+09;)
          f32.lt
          local.get 14
          f32.const 0x0p+0 (;=0;)
          f32.ge
          i32.and
          if  ;; label = @4
            local.get 14
            i32.trunc_f32_u
            br 1 (;@3;)
          end
          i32.const 0
        end
        i32.const 1
        i32.add
        i32.store
        local.get 3
        i32.const 8
        i32.add
        local.set 3
        local.get 5
        i32.const 4
        i32.add
        local.set 5
        local.get 6
        i32.const -1
        i32.add
        local.tee 6
        br_if 0 (;@2;)
      end
    end
    i32.const 1712
    local.get 2
    i32.store
    i32.const 1720
    i32.const 0
    i32.store
    i32.const 1716
    i32.const 0
    i32.store
    i32.const 1568
    i32.load
    local.set 12
    i32.const 0
    local.set 3
    loop  ;; label = @1
      i32.const 1580
      i32.load
      i32.const 1
      i32.gt_s
      local.set 4
      block  ;; label = @2
        block  ;; label = @3
          loop  ;; label = @4
            i32.const -1
            local.set 2
            local.get 3
            i32.eqz
            if  ;; label = @5
              i32.const 1716
              local.get 4
              i32.store
              i32.const 1712
              i32.const 1712
              i32.load
              local.tee 2
              local.get 2
              i32.const 61440
              local.get 2
              i32.const 61440
              i32.lt_s
              select
              local.tee 2
              i32.sub
              i32.store
              local.get 2
              i32.eqz
              br_if 2 (;@3;)
              i32.const 1576
              i32.load
              local.set 5
              local.get 7
              local.set 3
              local.get 2
              local.set 6
              loop  ;; label = @6
                local.get 3
                local.get 5
                i32.const 3877
                i32.mul
                i32.const 29573
                i32.add
                i32.const 139968
                i32.rem_u
                local.tee 5
                i32.store
                local.get 3
                i32.const 4
                i32.add
                local.set 3
                local.get 6
                i32.const -1
                i32.add
                local.tee 6
                br_if 0 (;@6;)
              end
              i32.const 1576
              local.get 5
              i32.store
              local.get 4
              local.set 3
            end
            local.get 2
            i32.const -1
            i32.eq
            br_if 0 (;@4;)
          end
          local.get 2
          i32.eqz
          br_if 0 (;@3;)
          block  ;; label = @4
            local.get 2
            i32.const 1
            i32.lt_s
            if  ;; label = @5
              local.get 7
              i32.const 245760
              i32.add
              local.set 4
              br 1 (;@4;)
            end
            i32.const 0
            local.set 9
            local.get 7
            i32.const 245760
            i32.add
            local.set 4
            i32.const 0
            local.set 8
            loop  ;; label = @5
              i32.const 0
              local.set 5
              local.get 11
              i32.eqz
              if  ;; label = @6
                local.get 7
                local.get 9
                i32.const 2
                i32.shl
                i32.add
                i32.load
                local.set 13
                local.get 10
                local.set 3
                local.get 1
                local.set 6
                loop  ;; label = @7
                  local.get 5
                  local.get 3
                  i32.load
                  local.get 13
                  i32.le_u
                  i32.add
                  local.set 5
                  local.get 3
                  i32.const 4
                  i32.add
                  local.set 3
                  local.get 6
                  i32.const -1
                  i32.add
                  local.tee 6
                  br_if 0 (;@7;)
                end
              end
              local.get 4
              local.get 0
              local.get 5
              i32.const 3
              i32.shl
              i32.add
              i32.load8_u
              i32.store8
              block (result i32)  ;; label = @6
                local.get 8
                i32.const 59
                i32.lt_s
                if  ;; label = @7
                  local.get 8
                  i32.const 1
                  i32.add
                  local.set 8
                  local.get 4
                  i32.const 1
                  i32.add
                  br 1 (;@6;)
                end
                local.get 4
                i32.const 10
                i32.store8 offset=1
                i32.const 0
                local.set 8
                local.get 4
                i32.const 2
                i32.add
              end
              local.set 4
              local.get 9
              i32.const 1
              i32.add
              local.tee 9
              local.get 2
              i32.ne
              br_if 0 (;@5;)
            end
            local.get 8
            i32.eqz
            br_if 0 (;@4;)
            local.get 4
            i32.const 10
            i32.store8
            local.get 4
            i32.const 1
            i32.add
            local.set 4
          end
          local.get 4
          local.get 7
          i32.const 245760
          i32.add
          i32.sub
          local.set 2
          loop  ;; label = @4
            i32.const 1720
            i32.load
            if (result i32)  ;; label = @5
              i32.const -1
            else
              i32.const 1720
              i32.const 1584
              i32.load
              i32.const 1
              i32.gt_s
              i32.store
              local.get 7
              i32.const 245760
              i32.add
              local.get 2
              i32.const 1
              local.get 12
              call 10
            end
            i32.const 1
            i32.add
            local.tee 3
            i32.const 1
            i32.gt_u
            br_if 2 (;@2;)
            local.get 3
            i32.const 1
            i32.sub
            br_if 0 (;@4;)
          end
          call 18
          i32.const 1
          call 0
          unreachable
        end
        local.get 7
        i32.const 308224
        i32.add
        global.set 0
        return
      end
      i32.const 1716
      i32.load
      local.set 3
      br 0 (;@1;)
    end
    unreachable)
  (func (;9;) (type 6) (param i32 i32)
    (local i32)
    local.get 0
    call 21
    local.tee 2
    local.get 0
    i32.const 1
    local.get 2
    local.get 1
    call 10
    i32.ne
    drop)
  (func (;10;) (type 5) (param i32 i32 i32 i32) (result i32)
    (local i32 i32 i32 i32 i32 i32 i32)
    local.get 1
    local.get 2
    i32.mul
    local.set 5
    block  ;; label = @1
      local.get 3
      i32.load offset=16
      local.tee 4
      if (result i32)  ;; label = @2
        local.get 4
      else
        i32.const 0
        local.set 4
        local.get 3
        call 19
        br_if 1 (;@1;)
        local.get 3
        i32.load offset=16
      end
      local.get 3
      i32.load offset=20
      local.tee 8
      i32.sub
      local.get 5
      i32.lt_u
      if  ;; label = @2
        local.get 3
        local.get 0
        local.get 5
        local.get 3
        i32.load offset=32
        call_indirect (type 0)
        local.set 4
        br 1 (;@1;)
      end
      block (result i32)  ;; label = @2
        local.get 5
        local.get 3
        i32.load offset=64
        i32.const 0
        i32.lt_s
        br_if 0 (;@2;)
        drop
        local.get 0
        local.get 5
        i32.add
        local.set 9
        i32.const 0
        local.set 4
        loop  ;; label = @3
          local.get 5
          local.get 4
          local.get 5
          i32.add
          i32.eqz
          br_if 1 (;@2;)
          drop
          local.get 4
          local.get 9
          i32.add
          local.get 4
          i32.const -1
          i32.add
          local.tee 7
          local.set 4
          i32.const -1
          i32.add
          i32.load8_u
          i32.const 10
          i32.ne
          br_if 0 (;@3;)
        end
        local.get 3
        local.get 0
        local.get 5
        local.get 7
        i32.add
        i32.const 1
        i32.add
        local.tee 6
        local.get 3
        i32.load offset=32
        call_indirect (type 0)
        local.tee 4
        local.get 6
        i32.lt_u
        br_if 1 (;@1;)
        local.get 7
        local.get 9
        i32.add
        i32.const 1
        i32.add
        local.set 0
        local.get 3
        i32.load offset=20
        local.set 8
        local.get 7
        i32.const -1
        i32.xor
      end
      local.set 4
      local.get 8
      local.get 0
      local.get 4
      call 20
      drop
      local.get 3
      local.get 3
      i32.load offset=20
      local.get 4
      i32.add
      i32.store offset=20
      local.get 4
      local.get 6
      i32.add
      local.set 4
    end
    local.get 4
    local.get 5
    i32.eq
    if  ;; label = @1
      local.get 2
      i32.const 0
      local.get 1
      select
      return
    end
    local.get 4
    local.get 1
    i32.div_u)
  (func (;11;) (type 2) (param i32) (result i32)
    block (result i32)  ;; label = @1
      i32.const 0
      local.get 0
      i32.load offset=56
      call 1
      local.tee 0
      i32.eqz
      br_if 0 (;@1;)
      drop
      i32.const 1724
      local.get 0
      i32.store
      i32.const -1
    end)
  (func (;12;) (type 1) (param i32 i64 i32) (result i64)
    (local i32)
    global.get 0
    i32.const 16
    i32.sub
    local.tee 3
    global.set 0
    block (result i64)  ;; label = @1
      local.get 0
      local.get 1
      local.get 2
      i32.const 255
      i32.and
      local.get 3
      i32.const 8
      i32.add
      call 2
      local.tee 0
      if  ;; label = @2
        i32.const 1724
        i32.const 70
        local.get 0
        local.get 0
        i32.const 76
        i32.eq
        select
        i32.store
        i64.const -1
        br 1 (;@1;)
      end
      local.get 3
      i64.load offset=8
    end
    local.get 3
    i32.const 16
    i32.add
    global.set 0)
  (func (;13;) (type 1) (param i32 i64 i32) (result i64)
    local.get 0
    i32.load offset=56
    local.get 1
    local.get 2
    call 12)
  (func (;14;) (type 0) (param i32 i32 i32) (result i32)
    (local i32 i32)
    global.get 0
    i32.const 16
    i32.sub
    local.tee 3
    global.set 0
    i32.const -1
    local.set 4
    block  ;; label = @1
      local.get 2
      i32.const -1
      i32.le_s
      if  ;; label = @2
        i32.const 1724
        i32.const 28
        i32.store
        br 1 (;@1;)
      end
      local.get 0
      local.get 1
      local.get 2
      local.get 3
      i32.const 12
      i32.add
      call 3
      local.tee 0
      if  ;; label = @2
        i32.const 1724
        local.get 0
        i32.store
        br 1 (;@1;)
      end
      local.get 3
      i32.load offset=12
      local.set 4
    end
    local.get 3
    i32.const 16
    i32.add
    global.set 0
    local.get 4)
  (func (;15;) (type 0) (param i32 i32 i32) (result i32)
    (local i32 i32 i32 i32 i32 i32)
    global.get 0
    i32.const 16
    i32.sub
    local.tee 3
    global.set 0
    local.get 3
    local.get 2
    i32.store offset=12
    local.get 3
    local.get 1
    i32.store offset=8
    local.get 3
    local.get 0
    i32.load offset=24
    local.tee 1
    i32.store
    local.get 3
    local.get 0
    i32.load offset=20
    local.get 1
    i32.sub
    local.tee 1
    i32.store offset=4
    i32.const 2
    local.set 6
    block (result i32)  ;; label = @1
      local.get 1
      local.get 2
      i32.add
      local.tee 7
      local.get 0
      i32.load offset=56
      local.get 3
      i32.const 2
      call 14
      local.tee 4
      i32.ne
      if  ;; label = @2
        local.get 3
        local.set 1
        loop  ;; label = @3
          local.get 4
          i32.const -1
          i32.le_s
          if  ;; label = @4
            local.get 0
            i32.const 0
            i32.store offset=24
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
            br_if 3 (;@1;)
            drop
            local.get 2
            local.get 1
            i32.load offset=4
            i32.sub
            br 3 (;@1;)
          end
          local.get 1
          i32.const 8
          i32.add
          local.get 1
          local.get 4
          local.get 1
          i32.load offset=4
          local.tee 8
          i32.gt_u
          local.tee 5
          select
          local.tee 1
          local.get 4
          local.get 8
          i32.const 0
          local.get 5
          select
          i32.sub
          local.tee 8
          local.get 1
          i32.load
          i32.add
          i32.store
          local.get 1
          local.get 1
          i32.load offset=4
          local.get 8
          i32.sub
          i32.store offset=4
          local.get 7
          local.get 4
          i32.sub
          local.set 7
          local.get 0
          i32.load offset=56
          local.get 1
          local.get 6
          local.get 5
          i32.sub
          local.tee 6
          call 14
          local.tee 5
          local.set 4
          local.get 5
          local.get 7
          i32.ne
          br_if 0 (;@3;)
        end
      end
      local.get 0
      local.get 0
      i32.load offset=40
      local.tee 1
      i32.store offset=24
      local.get 0
      local.get 1
      i32.store offset=20
      local.get 0
      local.get 1
      local.get 0
      i32.load offset=44
      i32.add
      i32.store offset=16
      local.get 2
    end
    local.get 3
    i32.const 16
    i32.add
    global.set 0)
  (func (;16;) (type 2) (param i32) (result i32)
    (local i32)
    global.get 0
    i32.const 32
    i32.sub
    local.tee 1
    global.set 0
    block (result i32)  ;; label = @1
      block  ;; label = @2
        local.get 0
        local.get 1
        i32.const 8
        i32.add
        call 4
        local.tee 0
        br_if 0 (;@2;)
        i32.const 59
        local.set 0
        local.get 1
        i32.load8_u offset=8
        i32.const 2
        i32.ne
        br_if 0 (;@2;)
        local.get 1
        i32.load8_u offset=16
        i32.const 36
        i32.and
        br_if 0 (;@2;)
        i32.const 1
        br 1 (;@1;)
      end
      i32.const 1724
      local.get 0
      i32.store
      i32.const 0
    end
    local.get 1
    i32.const 32
    i32.add
    global.set 0)
  (func (;17;) (type 0) (param i32 i32 i32) (result i32)
    local.get 0
    i32.const 4
    i32.store offset=32
    block  ;; label = @1
      local.get 0
      i32.load8_u
      i32.const 64
      i32.and
      br_if 0 (;@1;)
      local.get 0
      i32.load offset=56
      call 16
      br_if 0 (;@1;)
      local.get 0
      i32.const -1
      i32.store offset=64
    end
    local.get 0
    local.get 1
    local.get 2
    call 15)
  (func (;18;) (type 3)
    (local i32 i32 i32)
    i32.const 2760
    i32.load
    local.tee 0
    if  ;; label = @1
      loop  ;; label = @2
        local.get 0
        i32.load offset=20
        local.get 0
        i32.load offset=24
        i32.ne
        if  ;; label = @3
          local.get 0
          i32.const 0
          i32.const 0
          local.get 0
          i32.load offset=32
          call_indirect (type 0)
          drop
        end
        local.get 0
        i32.load offset=4
        local.tee 1
        local.get 0
        i32.load offset=8
        local.tee 2
        i32.ne
        if  ;; label = @3
          local.get 0
          local.get 1
          local.get 2
          i32.sub
          i64.extend_i32_s
          i32.const 1
          local.get 0
          i32.load offset=36
          call_indirect (type 1)
          drop
        end
        local.get 0
        i32.load offset=52
        local.tee 0
        br_if 0 (;@2;)
      end
    end
    block  ;; label = @1
      i32.const 2764
      i32.load
      local.tee 0
      i32.eqz
      br_if 0 (;@1;)
      local.get 0
      i32.load offset=20
      local.get 0
      i32.load offset=24
      i32.ne
      if  ;; label = @2
        local.get 0
        i32.const 0
        i32.const 0
        local.get 0
        i32.load offset=32
        call_indirect (type 0)
        drop
      end
      local.get 0
      i32.load offset=4
      local.tee 1
      local.get 0
      i32.load offset=8
      local.tee 2
      i32.eq
      br_if 0 (;@1;)
      local.get 0
      local.get 1
      local.get 2
      i32.sub
      i64.extend_i32_s
      i32.const 1
      local.get 0
      i32.load offset=36
      call_indirect (type 1)
      drop
    end
    block  ;; label = @1
      i32.const 1704
      i32.load
      local.tee 0
      i32.eqz
      br_if 0 (;@1;)
      local.get 0
      i32.load offset=20
      local.get 0
      i32.load offset=24
      i32.ne
      if  ;; label = @2
        local.get 0
        i32.const 0
        i32.const 0
        local.get 0
        i32.load offset=32
        call_indirect (type 0)
        drop
      end
      local.get 0
      i32.load offset=4
      local.tee 1
      local.get 0
      i32.load offset=8
      local.tee 2
      i32.eq
      br_if 0 (;@1;)
      local.get 0
      local.get 1
      local.get 2
      i32.sub
      i64.extend_i32_s
      i32.const 1
      local.get 0
      i32.load offset=36
      call_indirect (type 1)
      drop
    end
    block  ;; label = @1
      i32.const 2764
      i32.load
      local.tee 0
      i32.eqz
      br_if 0 (;@1;)
      local.get 0
      i32.load offset=20
      local.get 0
      i32.load offset=24
      i32.ne
      if  ;; label = @2
        local.get 0
        i32.const 0
        i32.const 0
        local.get 0
        i32.load offset=32
        call_indirect (type 0)
        drop
      end
      local.get 0
      i32.load offset=4
      local.tee 1
      local.get 0
      i32.load offset=8
      local.tee 2
      i32.eq
      br_if 0 (;@1;)
      local.get 0
      local.get 1
      local.get 2
      i32.sub
      i64.extend_i32_s
      i32.const 1
      local.get 0
      i32.load offset=36
      call_indirect (type 1)
      drop
    end)
  (func (;19;) (type 2) (param i32) (result i32)
    (local i32)
    local.get 0
    local.get 0
    i32.load offset=60
    local.tee 1
    i32.const -1
    i32.add
    local.get 1
    i32.or
    i32.store offset=60
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
    i32.load offset=40
    local.tee 1
    i32.store offset=24
    local.get 0
    local.get 1
    i32.store offset=20
    local.get 0
    local.get 1
    local.get 0
    i32.load offset=44
    i32.add
    i32.store offset=16
    i32.const 0)
  (func (;20;) (type 0) (param i32 i32 i32) (result i32)
    (local i32 i32 i32 i32 i32 i32 i32 i32)
    block  ;; label = @1
      local.get 2
      i32.eqz
      local.get 1
      i32.const 3
      i32.and
      i32.eqz
      i32.or
      i32.eqz
      if  ;; label = @2
        local.get 0
        local.set 3
        loop  ;; label = @3
          local.get 3
          local.get 1
          i32.load8_u
          i32.store8
          local.get 2
          i32.const -1
          i32.add
          local.set 4
          local.get 3
          i32.const 1
          i32.add
          local.set 3
          local.get 1
          i32.const 1
          i32.add
          local.set 1
          local.get 2
          i32.const 1
          i32.eq
          br_if 2 (;@1;)
          local.get 4
          local.set 2
          local.get 1
          i32.const 3
          i32.and
          br_if 0 (;@3;)
        end
        br 1 (;@1;)
      end
      local.get 2
      local.set 4
      local.get 0
      local.set 3
    end
    block  ;; label = @1
      local.get 3
      i32.const 3
      i32.and
      local.tee 2
      i32.eqz
      if  ;; label = @2
        block  ;; label = @3
          local.get 4
          i32.const 16
          i32.lt_u
          if  ;; label = @4
            local.get 4
            local.set 2
            br 1 (;@3;)
          end
          local.get 4
          i32.const -16
          i32.add
          local.set 2
          loop  ;; label = @4
            local.get 3
            local.get 1
            i32.load
            i32.store
            local.get 3
            i32.const 4
            i32.add
            local.get 1
            i32.const 4
            i32.add
            i32.load
            i32.store
            local.get 3
            i32.const 8
            i32.add
            local.get 1
            i32.const 8
            i32.add
            i32.load
            i32.store
            local.get 3
            i32.const 12
            i32.add
            local.get 1
            i32.const 12
            i32.add
            i32.load
            i32.store
            local.get 3
            i32.const 16
            i32.add
            local.set 3
            local.get 1
            i32.const 16
            i32.add
            local.set 1
            local.get 4
            i32.const -16
            i32.add
            local.tee 4
            i32.const 15
            i32.gt_u
            br_if 0 (;@4;)
          end
        end
        local.get 2
        i32.const 8
        i32.and
        if  ;; label = @3
          local.get 3
          local.get 1
          i64.load align=4
          i64.store align=4
          local.get 3
          i32.const 8
          i32.add
          local.set 3
          local.get 1
          i32.const 8
          i32.add
          local.set 1
        end
        local.get 2
        i32.const 4
        i32.and
        if  ;; label = @3
          local.get 3
          local.get 1
          i32.load
          i32.store
          local.get 3
          i32.const 4
          i32.add
          local.set 3
          local.get 1
          i32.const 4
          i32.add
          local.set 1
        end
        local.get 2
        i32.const 2
        i32.and
        if  ;; label = @3
          local.get 3
          local.get 1
          i32.load8_u
          i32.store8
          local.get 3
          local.get 1
          i32.load8_u offset=1
          i32.store8 offset=1
          local.get 3
          i32.const 2
          i32.add
          local.set 3
          local.get 1
          i32.const 2
          i32.add
          local.set 1
        end
        local.get 2
        i32.const 1
        i32.and
        i32.eqz
        br_if 1 (;@1;)
        local.get 3
        local.get 1
        i32.load8_u
        i32.store8
        local.get 0
        return
      end
      block  ;; label = @2
        local.get 4
        i32.const 32
        i32.lt_u
        br_if 0 (;@2;)
        local.get 2
        i32.const -1
        i32.add
        local.tee 2
        i32.const 2
        i32.gt_u
        br_if 0 (;@2;)
        block  ;; label = @3
          block  ;; label = @4
            block  ;; label = @5
              local.get 2
              i32.const 1
              i32.sub
              br_table 1 (;@4;) 2 (;@3;) 0 (;@5;)
            end
            local.get 3
            local.get 1
            i32.load8_u offset=1
            i32.store8 offset=1
            local.get 3
            local.get 1
            i32.load
            local.tee 5
            i32.store8
            local.get 3
            local.get 1
            i32.load8_u offset=2
            i32.store8 offset=2
            local.get 4
            i32.const -3
            i32.add
            local.set 8
            local.get 3
            i32.const 3
            i32.add
            local.set 9
            local.get 4
            i32.const -20
            i32.add
            i32.const -16
            i32.and
            local.set 10
            i32.const 0
            local.set 2
            loop  ;; label = @5
              local.get 2
              local.get 9
              i32.add
              local.tee 3
              local.get 1
              local.get 2
              i32.add
              local.tee 6
              i32.const 4
              i32.add
              i32.load
              local.tee 7
              i32.const 8
              i32.shl
              local.get 5
              i32.const 24
              i32.shr_u
              i32.or
              i32.store
              local.get 3
              i32.const 4
              i32.add
              local.get 6
              i32.const 8
              i32.add
              i32.load
              local.tee 5
              i32.const 8
              i32.shl
              local.get 7
              i32.const 24
              i32.shr_u
              i32.or
              i32.store
              local.get 3
              i32.const 8
              i32.add
              local.get 6
              i32.const 12
              i32.add
              i32.load
              local.tee 7
              i32.const 8
              i32.shl
              local.get 5
              i32.const 24
              i32.shr_u
              i32.or
              i32.store
              local.get 3
              i32.const 12
              i32.add
              local.get 6
              i32.const 16
              i32.add
              i32.load
              local.tee 5
              i32.const 8
              i32.shl
              local.get 7
              i32.const 24
              i32.shr_u
              i32.or
              i32.store
              local.get 2
              i32.const 16
              i32.add
              local.set 2
              local.get 8
              i32.const -16
              i32.add
              local.tee 8
              i32.const 16
              i32.gt_u
              br_if 0 (;@5;)
            end
            local.get 2
            local.get 9
            i32.add
            local.set 3
            local.get 1
            local.get 2
            i32.add
            i32.const 3
            i32.add
            local.set 1
            local.get 4
            local.get 10
            i32.sub
            i32.const -19
            i32.add
            local.set 4
            br 2 (;@2;)
          end
          local.get 3
          local.get 1
          i32.load
          local.tee 5
          i32.store8
          local.get 3
          local.get 1
          i32.load8_u offset=1
          i32.store8 offset=1
          local.get 4
          i32.const -2
          i32.add
          local.set 8
          local.get 3
          i32.const 2
          i32.add
          local.set 9
          local.get 4
          i32.const -20
          i32.add
          i32.const -16
          i32.and
          local.set 10
          i32.const 0
          local.set 2
          loop  ;; label = @4
            local.get 2
            local.get 9
            i32.add
            local.tee 3
            local.get 1
            local.get 2
            i32.add
            local.tee 6
            i32.const 4
            i32.add
            i32.load
            local.tee 7
            i32.const 16
            i32.shl
            local.get 5
            i32.const 16
            i32.shr_u
            i32.or
            i32.store
            local.get 3
            i32.const 4
            i32.add
            local.get 6
            i32.const 8
            i32.add
            i32.load
            local.tee 5
            i32.const 16
            i32.shl
            local.get 7
            i32.const 16
            i32.shr_u
            i32.or
            i32.store
            local.get 3
            i32.const 8
            i32.add
            local.get 6
            i32.const 12
            i32.add
            i32.load
            local.tee 7
            i32.const 16
            i32.shl
            local.get 5
            i32.const 16
            i32.shr_u
            i32.or
            i32.store
            local.get 3
            i32.const 12
            i32.add
            local.get 6
            i32.const 16
            i32.add
            i32.load
            local.tee 5
            i32.const 16
            i32.shl
            local.get 7
            i32.const 16
            i32.shr_u
            i32.or
            i32.store
            local.get 2
            i32.const 16
            i32.add
            local.set 2
            local.get 8
            i32.const -16
            i32.add
            local.tee 8
            i32.const 17
            i32.gt_u
            br_if 0 (;@4;)
          end
          local.get 2
          local.get 9
          i32.add
          local.set 3
          local.get 1
          local.get 2
          i32.add
          i32.const 2
          i32.add
          local.set 1
          local.get 4
          local.get 10
          i32.sub
          i32.const -18
          i32.add
          local.set 4
          br 1 (;@2;)
        end
        local.get 3
        local.get 1
        i32.load
        local.tee 5
        i32.store8
        local.get 4
        i32.const -1
        i32.add
        local.set 8
        local.get 3
        i32.const 1
        i32.add
        local.set 9
        local.get 4
        i32.const -20
        i32.add
        i32.const -16
        i32.and
        local.set 10
        i32.const 0
        local.set 2
        loop  ;; label = @3
          local.get 2
          local.get 9
          i32.add
          local.tee 3
          local.get 1
          local.get 2
          i32.add
          local.tee 6
          i32.const 4
          i32.add
          i32.load
          local.tee 7
          i32.const 24
          i32.shl
          local.get 5
          i32.const 8
          i32.shr_u
          i32.or
          i32.store
          local.get 3
          i32.const 4
          i32.add
          local.get 6
          i32.const 8
          i32.add
          i32.load
          local.tee 5
          i32.const 24
          i32.shl
          local.get 7
          i32.const 8
          i32.shr_u
          i32.or
          i32.store
          local.get 3
          i32.const 8
          i32.add
          local.get 6
          i32.const 12
          i32.add
          i32.load
          local.tee 7
          i32.const 24
          i32.shl
          local.get 5
          i32.const 8
          i32.shr_u
          i32.or
          i32.store
          local.get 3
          i32.const 12
          i32.add
          local.get 6
          i32.const 16
          i32.add
          i32.load
          local.tee 5
          i32.const 24
          i32.shl
          local.get 7
          i32.const 8
          i32.shr_u
          i32.or
          i32.store
          local.get 2
          i32.const 16
          i32.add
          local.set 2
          local.get 8
          i32.const -16
          i32.add
          local.tee 8
          i32.const 18
          i32.gt_u
          br_if 0 (;@3;)
        end
        local.get 2
        local.get 9
        i32.add
        local.set 3
        local.get 1
        local.get 2
        i32.add
        i32.const 1
        i32.add
        local.set 1
        local.get 4
        local.get 10
        i32.sub
        i32.const -17
        i32.add
        local.set 4
      end
      local.get 4
      i32.const 16
      i32.and
      if  ;; label = @2
        local.get 3
        local.get 1
        i32.load16_u align=1
        i32.store16 align=1
        local.get 3
        local.get 1
        i32.load8_u offset=2
        i32.store8 offset=2
        local.get 3
        local.get 1
        i32.load8_u offset=3
        i32.store8 offset=3
        local.get 3
        local.get 1
        i32.load8_u offset=4
        i32.store8 offset=4
        local.get 3
        local.get 1
        i32.load8_u offset=5
        i32.store8 offset=5
        local.get 3
        local.get 1
        i32.load8_u offset=6
        i32.store8 offset=6
        local.get 3
        local.get 1
        i32.load8_u offset=7
        i32.store8 offset=7
        local.get 3
        local.get 1
        i32.load8_u offset=8
        i32.store8 offset=8
        local.get 3
        local.get 1
        i32.load8_u offset=9
        i32.store8 offset=9
        local.get 3
        local.get 1
        i32.load8_u offset=10
        i32.store8 offset=10
        local.get 3
        local.get 1
        i32.load8_u offset=11
        i32.store8 offset=11
        local.get 3
        local.get 1
        i32.load8_u offset=12
        i32.store8 offset=12
        local.get 3
        local.get 1
        i32.load8_u offset=13
        i32.store8 offset=13
        local.get 3
        local.get 1
        i32.load8_u offset=14
        i32.store8 offset=14
        local.get 3
        local.get 1
        i32.load8_u offset=15
        i32.store8 offset=15
        local.get 3
        i32.const 16
        i32.add
        local.set 3
        local.get 1
        i32.const 16
        i32.add
        local.set 1
      end
      local.get 4
      i32.const 8
      i32.and
      if  ;; label = @2
        local.get 3
        local.get 1
        i32.load8_u
        i32.store8
        local.get 3
        local.get 1
        i32.load8_u offset=1
        i32.store8 offset=1
        local.get 3
        local.get 1
        i32.load8_u offset=2
        i32.store8 offset=2
        local.get 3
        local.get 1
        i32.load8_u offset=3
        i32.store8 offset=3
        local.get 3
        local.get 1
        i32.load8_u offset=4
        i32.store8 offset=4
        local.get 3
        local.get 1
        i32.load8_u offset=5
        i32.store8 offset=5
        local.get 3
        local.get 1
        i32.load8_u offset=6
        i32.store8 offset=6
        local.get 3
        local.get 1
        i32.load8_u offset=7
        i32.store8 offset=7
        local.get 3
        i32.const 8
        i32.add
        local.set 3
        local.get 1
        i32.const 8
        i32.add
        local.set 1
      end
      local.get 4
      i32.const 4
      i32.and
      if  ;; label = @2
        local.get 3
        local.get 1
        i32.load8_u
        i32.store8
        local.get 3
        local.get 1
        i32.load8_u offset=1
        i32.store8 offset=1
        local.get 3
        local.get 1
        i32.load8_u offset=2
        i32.store8 offset=2
        local.get 3
        local.get 1
        i32.load8_u offset=3
        i32.store8 offset=3
        local.get 3
        i32.const 4
        i32.add
        local.set 3
        local.get 1
        i32.const 4
        i32.add
        local.set 1
      end
      local.get 4
      i32.const 2
      i32.and
      if  ;; label = @2
        local.get 3
        local.get 1
        i32.load8_u
        i32.store8
        local.get 3
        local.get 1
        i32.load8_u offset=1
        i32.store8 offset=1
        local.get 3
        i32.const 2
        i32.add
        local.set 3
        local.get 1
        i32.const 2
        i32.add
        local.set 1
      end
      local.get 4
      i32.const 1
      i32.and
      i32.eqz
      br_if 0 (;@1;)
      local.get 3
      local.get 1
      i32.load8_u
      i32.store8
    end
    local.get 0)
  (func (;21;) (type 2) (param i32) (result i32)
    (local i32 i32 i32)
    block  ;; label = @1
      block  ;; label = @2
        block  ;; label = @3
          local.get 0
          local.tee 1
          i32.const 3
          i32.and
          i32.eqz
          br_if 0 (;@3;)
          local.get 0
          i32.load8_u
          i32.eqz
          if  ;; label = @4
            i32.const 0
            return
          end
          local.get 0
          i32.const 1
          i32.add
          local.set 1
          loop  ;; label = @4
            local.get 1
            i32.const 3
            i32.and
            i32.eqz
            br_if 1 (;@3;)
            local.get 1
            i32.load8_u
            local.get 1
            i32.const 1
            i32.add
            local.tee 3
            local.set 1
            br_if 0 (;@4;)
          end
          br 1 (;@2;)
        end
        local.get 1
        i32.const -4
        i32.add
        local.set 1
        loop  ;; label = @3
          local.get 1
          i32.const 4
          i32.add
          local.tee 1
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
          br_if 0 (;@3;)
        end
        local.get 2
        i32.const 255
        i32.and
        i32.eqz
        if  ;; label = @3
          local.get 1
          local.get 0
          i32.sub
          return
        end
        loop  ;; label = @3
          local.get 1
          i32.load8_u offset=1
          local.get 1
          i32.const 1
          i32.add
          local.tee 2
          local.set 1
          br_if 0 (;@3;)
        end
        br 1 (;@1;)
      end
      local.get 3
      i32.const -1
      i32.add
      local.set 2
    end
    local.get 2
    local.get 0
    i32.sub)
  (table (;0;) 5 5 funcref)
  (memory (;0;) 2)
  (global (;0;) (mut i32) (i32.const 68304))
  (export "memory" (memory 0))
  (export "_start" (func 5))
  (elem (;0;) (i32.const 1) 11 17 13 15)
  (data (;0;) (i32.const 1024) ">ONE Homo sapiens alu\0a")
  (data (;1;) (i32.const 1056) "GGCCGGGCGCGGTGGCTCACGCCTGTAATCCCAGCACTTTGGGAGGCCGAGGCGGGCGGATCACCTGAGGTCAGGAGTTCGAGACCAGCCTGGCCAACATGGTGAAACCCCGTCTCTACTAAAAATACAAAAATTAGCCGGGCGTGGTGGCGCGCGCCTGTAATCCCAGCTACTCGGGAGGCTGAGGCAGGAGAATCGCTTGAACCCGGGAGGCGGAGGTTGCAGTGAGCCGAGATCGCGCCACTGCACTCCAGCCTGGGCGACAGAGCGAGACTCCGTCTCAAAAA\00>TWO IUB ambiguity codes\0a\00\00\00\00\00\00\00a\00\00\00q=\8a>c\00\00\00\8f\c2\f5=g\00\00\00\8f\c2\f5=t\00\00\00q=\8a>B\00\00\00\0a\d7\a3<D\00\00\00\0a\d7\a3<H\00\00\00\0a\d7\a3<K\00\00\00\0a\d7\a3<M\00\00\00\0a\d7\a3<N\00\00\00\0a\d7\a3<R\00\00\00\0a\d7\a3<S\00\00\00\0a\d7\a3<V\00\00\00\0a\d7\a3<W\00\00\00\0a\d7\a3<Y\00\00\00\0a\d7\a3<>THREE Homo sapiens frequency\0a")
  (data (;2;) (i32.const 1536) "a\00\00\00\e9\1c\9b>c\00\00\00r\bdJ>g\00\00\00\d7IJ>t\00\00\00r_\9a>8\06")
  (data (;3;) (i32.const 1576) "*\00\00\00\01\00\00\00\01\00\00\00\00\00\00\00\05")
  (data (;4;) (i32.const 1604) "\01")
  (data (;5;) (i32.const 1624) "\02\00\00\00\03\00\00\00\c8\06\00\00\00\04")
  (data (;6;) (i32.const 1648) "\01\00\00\00\00\00\00\00\0a")
  (data (;7;) (i32.const 1704) "8\06"))
