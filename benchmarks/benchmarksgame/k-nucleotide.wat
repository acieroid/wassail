(module
  (type (;0;) (func (param i32 i32 i32) (result i32)))
  (type (;1;) (func (param i32) (result i32)))
  (type (;2;) (func (param i32 i32) (result i32)))
  (type (;3;) (func (param i32 i64 i32) (result i64)))
  (type (;4;) (func))
  (type (;5;) (func (param i32)))
  (type (;6;) (func (param i32 i32)))
  (type (;7;) (func (param i32 i32 i32)))
  (type (;8;) (func (result i32)))
  (type (;9;) (func (param i32 i32 i32 i32 i32)))
  (type (;10;) (func (param i32 i32 i32 i32) (result i32)))
  (type (;11;) (func (param i32 i64 i32) (result i32)))
  (type (;12;) (func (param i32 i64 i32 i32) (result i32)))
  (import "wasi_snapshot_preview1" "proc_exit" (func (;0;) (type 5)))
  (import "wasi_snapshot_preview1" "fd_close" (func (;1;) (type 1)))
  (import "wasi_snapshot_preview1" "fd_read" (func (;2;) (type 10)))
  (import "wasi_snapshot_preview1" "fd_seek" (func (;3;) (type 12)))
  (func (;4;) (type 4)
    (local i32)
    call 5
    local.set 0
    call 24
    local.get 0
    if  ;; label = @1
      local.get 0
      call 0
      unreachable
    end)
  (func (;5;) (type 8) (result i32)
    (local i32 i32 i32 i32 i32 i32 i32 i32 i32)
    global.get 0
    i32.const 4096
    i32.sub
    local.tee 2
    global.set 0
    i32.const 1088
    i32.load
    local.set 4
    loop  ;; label = @1
      local.get 2
      local.get 4
      call 17
      if  ;; label = @2
        local.get 2
        call 28
        br_if 1 (;@1;)
      end
    end
    i32.const 1048576
    call 11
    local.set 3
    block  ;; label = @1
      local.get 2
      local.get 4
      call 17
      i32.eqz
      br_if 0 (;@1;)
      local.get 2
      i32.load8_u
      local.tee 0
      i32.const 62
      i32.eq
      br_if 0 (;@1;)
      local.get 2
      i32.const 1
      i32.or
      local.set 7
      i32.const 1048576
      local.set 5
      loop  ;; label = @2
        local.get 7
        local.set 6
        loop  ;; label = @3
          local.get 0
          local.tee 8
          i32.const 10
          i32.ne
          if  ;; label = @4
            local.get 8
            i32.eqz
            if  ;; label = @5
              local.get 5
              local.get 1
              i32.sub
              i32.const 4096
              i32.lt_u
              if  ;; label = @6
                local.get 3
                local.get 5
                i32.const 1
                i32.shl
                local.tee 5
                call 14
                local.set 3
              end
              local.get 2
              local.get 4
              call 17
              i32.eqz
              br_if 4 (;@1;)
              local.get 2
              i32.load8_u
              local.tee 0
              i32.const 62
              i32.ne
              br_if 3 (;@2;)
              br 4 (;@1;)
            end
            local.get 1
            local.get 3
            i32.add
            local.get 0
            i32.const 7
            i32.and
            i32.const 1031
            i32.add
            i32.load8_u
            i32.store8
            local.get 1
            i32.const 1
            i32.add
            local.set 1
          end
          local.get 6
          i32.load8_u
          local.set 0
          local.get 6
          i32.const 1
          i32.add
          local.set 6
          br 0 (;@3;)
        end
        unreachable
      end
      unreachable
    end
    local.get 3
    local.get 1
    call 14
    local.tee 0
    local.get 1
    i32.const 1040
    call 6
    local.get 0
    local.get 1
    i32.const 1059
    call 6
    local.get 0
    local.get 1
    i32.const 1072
    call 6
    local.get 0
    local.get 1
    i32.const 1079
    call 6
    local.get 0
    local.get 1
    i32.const 1084
    call 6
    local.get 0
    local.get 1
    i32.const 2
    call 7
    local.get 0
    local.get 1
    i32.const 1
    call 7
    local.get 0
    call 12
    local.get 2
    i32.const 4096
    i32.add
    global.set 0
    i32.const 0)
  (func (;6;) (type 7) (param i32 i32 i32)
    (local i32 i32 i32 i32 i32 i32 i64 i64)
    global.get 0
    i32.const 16
    i32.sub
    local.tee 8
    global.set 0
    local.get 2
    call 31
    local.tee 4
    i32.const -1
    i32.add
    local.set 5
    call 13
    local.set 3
    i64.const -1
    local.get 4
    i32.const 1
    i32.shl
    i64.extend_i32_u
    i64.shl
    i64.const -1
    i64.xor
    local.set 10
    local.get 4
    i32.const 2
    i32.ge_s
    if  ;; label = @1
      local.get 0
      local.set 7
      local.get 5
      local.set 6
      loop  ;; label = @2
        local.get 7
        i64.load8_s
        local.get 9
        i64.const 2
        i64.shl
        local.get 10
        i64.and
        i64.or
        local.set 9
        local.get 7
        i32.const 1
        i32.add
        local.set 7
        local.get 6
        i32.const -1
        i32.add
        local.tee 6
        br_if 0 (;@2;)
      end
    end
    i32.const 0
    local.set 7
    local.get 4
    local.get 1
    i32.le_s
    if  ;; label = @1
      loop  ;; label = @2
        local.get 3
        local.get 0
        local.get 5
        i32.add
        i64.load8_s
        local.get 9
        i64.const 2
        i64.shl
        local.get 10
        i64.and
        i64.or
        local.tee 9
        local.get 8
        i32.const 12
        i32.add
        call 8
        local.set 6
        local.get 3
        i32.load offset=24
        local.tee 7
        local.get 6
        i32.const 2
        i32.shl
        i32.add
        local.tee 6
        local.get 8
        i32.load offset=12
        if (result i32)  ;; label = @3
          i32.const 1
        else
          local.get 6
          i32.load
          i32.const 1
          i32.add
        end
        i32.store
        local.get 5
        i32.const 1
        i32.add
        local.tee 5
        local.get 1
        i32.lt_s
        br_if 0 (;@2;)
      end
    end
    i64.const 0
    local.set 9
    local.get 4
    i32.const 1
    i32.ge_s
    if  ;; label = @1
      loop  ;; label = @2
        local.get 2
        i32.load8_u
        i32.const 7
        i32.and
        i32.const 1031
        i32.add
        i64.load8_s
        local.get 9
        i64.const 2
        i64.shl
        i64.or
        local.set 9
        local.get 2
        i32.const 1
        i32.add
        local.set 2
        local.get 4
        i32.const -1
        i32.add
        local.tee 4
        br_if 0 (;@2;)
      end
    end
    block  ;; label = @1
      block  ;; label = @2
        local.get 3
        i32.load
        local.tee 0
        i32.eqz
        br_if 0 (;@2;)
        local.get 3
        i32.load offset=16
        local.set 1
        i32.const 1
        local.set 5
        local.get 0
        i32.const -1
        i32.add
        local.tee 0
        local.get 9
        i64.const 7
        i64.shr_u
        local.get 9
        i64.xor
        i32.wrap_i64
        i32.and
        local.tee 6
        local.set 2
        loop  ;; label = @3
          local.get 1
          local.get 2
          i32.const 2
          i32.shr_u
          i32.const 1073741820
          i32.and
          i32.add
          i32.load
          local.get 2
          i32.const 1
          i32.shl
          i32.const 30
          i32.and
          i32.shr_u
          local.tee 4
          i32.const 2
          i32.and
          i32.eqz
          if  ;; label = @4
            local.get 4
            i32.const 1
            i32.and
            i32.eqz
            if  ;; label = @5
              local.get 3
              i32.load offset=20
              local.get 2
              i32.const 3
              i32.shl
              i32.add
              i64.load
              local.get 9
              i64.eq
              br_if 3 (;@2;)
            end
            local.get 2
            local.get 5
            i32.add
            local.set 2
            local.get 5
            i32.const 1
            i32.add
            local.set 5
            local.get 0
            local.get 2
            i32.and
            local.tee 2
            local.get 6
            i32.ne
            br_if 1 (;@3;)
          end
        end
        local.get 3
        i32.eqz
        br_if 1 (;@1;)
      end
      local.get 3
      i32.load offset=20
      call 12
      local.get 3
      i32.load offset=16
      call 12
      local.get 7
      call 12
      local.get 3
      call 12
    end
    local.get 8
    i32.const 16
    i32.add
    global.set 0)
  (func (;7;) (type 7) (param i32 i32 i32)
    (local i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i64 i64)
    global.get 0
    i32.const 16
    i32.sub
    local.tee 7
    global.set 0
    local.get 2
    i32.const -1
    i32.add
    local.set 4
    call 13
    local.set 5
    i64.const -1
    local.get 2
    i32.const 1
    i32.shl
    i64.extend_i32_u
    i64.shl
    i64.const -1
    i64.xor
    local.set 15
    local.get 2
    i32.const 2
    i32.ge_s
    if  ;; label = @1
      local.get 0
      local.set 3
      local.get 4
      local.set 6
      loop  ;; label = @2
        local.get 3
        i64.load8_s
        local.get 14
        i64.const 2
        i64.shl
        local.get 15
        i64.and
        i64.or
        local.set 14
        local.get 3
        i32.const 1
        i32.add
        local.set 3
        local.get 6
        i32.const -1
        i32.add
        local.tee 6
        br_if 0 (;@2;)
      end
    end
    local.get 2
    local.get 1
    i32.gt_s
    if (result i32)  ;; label = @1
      i32.const 0
    else
      loop  ;; label = @2
        local.get 5
        local.get 0
        local.get 4
        i32.add
        i64.load8_s
        local.get 14
        i64.const 2
        i64.shl
        local.get 15
        i64.and
        i64.or
        local.tee 14
        local.get 7
        i32.const 12
        i32.add
        call 8
        local.set 3
        local.get 5
        i32.load offset=24
        local.tee 10
        local.get 3
        i32.const 2
        i32.shl
        i32.add
        local.tee 3
        local.get 7
        i32.load offset=12
        if (result i32)  ;; label = @3
          i32.const 1
        else
          local.get 3
          i32.load
          i32.const 1
          i32.add
        end
        i32.store
        local.get 4
        i32.const 1
        i32.add
        local.tee 4
        local.get 1
        i32.lt_s
        br_if 0 (;@2;)
      end
      local.get 5
      i32.load offset=4
    end
    local.tee 8
    i32.const 4
    i32.shl
    call 11
    local.set 0
    local.get 5
    i32.load
    local.tee 11
    if  ;; label = @1
      local.get 5
      i32.load offset=16
      local.set 12
      i32.const 0
      local.set 4
      i32.const 0
      local.set 6
      local.get 10
      local.set 1
      i32.const 0
      local.set 3
      loop  ;; label = @2
        local.get 12
        local.get 3
        i32.const 2
        i32.shr_u
        i32.const 1073741820
        i32.and
        i32.add
        i32.load
        local.get 4
        i32.const 30
        i32.and
        i32.shr_u
        i32.const 3
        i32.and
        i32.eqz
        if  ;; label = @3
          local.get 5
          i32.load offset=20
          local.get 6
          i32.add
          i64.load
          local.set 14
          local.get 0
          local.get 9
          i32.const 4
          i32.shl
          i32.add
          local.tee 13
          local.get 1
          i32.load
          i32.store offset=8
          local.get 13
          local.get 14
          i64.store
          local.get 9
          i32.const 1
          i32.add
          local.set 9
        end
        local.get 4
        i32.const 2
        i32.add
        local.set 4
        local.get 6
        i32.const 8
        i32.add
        local.set 6
        local.get 1
        i32.const 4
        i32.add
        local.set 1
        local.get 11
        local.get 3
        i32.const 1
        i32.add
        local.tee 3
        i32.ne
        br_if 0 (;@2;)
      end
    end
    local.get 5
    i32.load offset=20
    call 12
    local.get 5
    i32.load offset=16
    call 12
    local.get 10
    call 12
    local.get 5
    call 12
    local.get 0
    local.get 8
    call 32
    local.get 8
    i32.const 1
    i32.ge_s
    if  ;; label = @1
      local.get 2
      i32.const 1
      i32.lt_s
      local.set 1
      i32.const 0
      local.set 3
      loop  ;; label = @2
        local.get 1
        i32.eqz
        if  ;; label = @3
          local.get 0
          local.get 3
          i32.const 4
          i32.shl
          i32.add
          local.tee 6
          i64.load
          local.set 14
          local.get 2
          local.set 4
          loop  ;; label = @4
            local.get 14
            i64.const 2
            i64.shr_u
            local.set 14
            local.get 4
            i32.const -1
            i32.add
            local.tee 4
            i32.const 0
            i32.gt_s
            br_if 0 (;@4;)
          end
          local.get 6
          local.get 14
          i64.store
        end
        local.get 3
        i32.const 1
        i32.add
        local.tee 3
        local.get 8
        i32.ne
        br_if 0 (;@2;)
      end
    end
    local.get 0
    call 12
    local.get 7
    i32.const 16
    i32.add
    global.set 0)
  (func (;8;) (type 11) (param i32 i64 i32) (result i32)
    (local i32 i32 i32 i32 i32 i32 i32 i32 i32)
    block  ;; label = @1
      block  ;; label = @2
        local.get 0
        i32.load offset=8
        local.get 0
        i32.load offset=12
        i32.lt_u
        br_if 0 (;@2;)
        local.get 0
        i32.load
        local.tee 4
        local.get 0
        i32.load offset=4
        i32.const 1
        i32.shl
        i32.gt_u
        if  ;; label = @3
          local.get 0
          local.get 4
          i32.const -1
          i32.add
          call 10
          i32.const -1
          i32.gt_s
          br_if 1 (;@2;)
          br 2 (;@1;)
        end
        local.get 0
        local.get 4
        i32.const 1
        i32.add
        call 10
        i32.const -1
        i32.gt_s
        br_if 0 (;@2;)
        br 1 (;@1;)
      end
      block  ;; label = @2
        local.get 0
        i32.load offset=16
        local.tee 9
        local.get 0
        i32.load
        local.tee 7
        i32.const -1
        i32.add
        local.tee 11
        local.get 1
        i64.const 7
        i64.shr_u
        local.get 1
        i64.xor
        i32.wrap_i64
        i32.and
        local.tee 4
        i32.const 2
        i32.shr_u
        i32.const 1073741820
        i32.and
        i32.add
        i32.load
        local.get 4
        i32.const 1
        i32.shl
        i32.const 30
        i32.and
        i32.shr_u
        i32.const 2
        i32.and
        if  ;; label = @3
          local.get 4
          local.set 3
          br 1 (;@2;)
        end
        i32.const 1
        local.set 6
        local.get 4
        local.set 5
        local.get 7
        local.set 3
        block (result i32)  ;; label = @3
          block  ;; label = @4
            loop  ;; label = @5
              local.get 9
              local.get 5
              i32.const 2
              i32.shr_u
              i32.const 1073741820
              i32.and
              i32.add
              i32.load
              local.get 5
              i32.const 1
              i32.shl
              i32.const 30
              i32.and
              i32.shr_u
              local.tee 10
              i32.const 2
              i32.and
              br_if 1 (;@4;)
              local.get 10
              i32.const 1
              i32.and
              local.tee 8
              i32.eqz
              if  ;; label = @6
                local.get 0
                i32.load offset=20
                local.get 5
                i32.const 3
                i32.shl
                i32.add
                i64.load
                local.get 1
                i64.eq
                br_if 2 (;@4;)
              end
              local.get 5
              local.get 3
              local.get 8
              select
              local.set 3
              local.get 5
              local.get 6
              i32.add
              local.get 6
              i32.const 1
              i32.add
              local.set 6
              local.get 11
              i32.and
              local.tee 5
              local.get 4
              i32.ne
              br_if 0 (;@5;)
            end
            i32.const 1
            local.get 3
            local.get 7
            local.tee 6
            i32.eq
            br_if 1 (;@3;)
            drop
            br 2 (;@2;)
          end
          local.get 3
          local.set 6
          local.get 5
          local.set 4
          local.get 10
          i32.const 2
          i32.and
          i32.eqz
        end
        local.set 8
        local.get 4
        local.get 4
        local.get 6
        local.get 8
        select
        local.get 6
        local.get 7
        i32.eq
        select
        local.set 3
      end
      local.get 9
      local.get 3
      i32.const 2
      i32.shr_u
      i32.const 1073741820
      i32.and
      i32.add
      local.tee 6
      i32.load
      local.tee 5
      local.get 3
      i32.const 1
      i32.shl
      i32.const 30
      i32.and
      local.tee 7
      i32.shr_u
      local.tee 4
      i32.const 2
      i32.and
      if  ;; label = @2
        local.get 6
        local.get 5
        i32.const 3
        local.get 7
        i32.shl
        i32.const -1
        i32.xor
        i32.and
        i32.store
        local.get 0
        local.get 0
        i32.load offset=8
        i32.const 1
        i32.add
        i32.store offset=8
        local.get 0
        local.get 0
        i32.load offset=4
        i32.const 1
        i32.add
        i32.store offset=4
        local.get 0
        i32.load offset=20
        local.get 3
        i32.const 3
        i32.shl
        i32.add
        local.get 1
        i64.store
        local.get 2
        i32.const 1
        i32.store
        local.get 3
        return
      end
      local.get 4
      i32.const 1
      i32.and
      if  ;; label = @2
        local.get 6
        local.get 5
        i32.const 3
        local.get 7
        i32.shl
        i32.const -1
        i32.xor
        i32.and
        i32.store
        local.get 0
        local.get 0
        i32.load offset=4
        i32.const 1
        i32.add
        i32.store offset=4
        local.get 0
        i32.load offset=20
        local.get 3
        i32.const 3
        i32.shl
        i32.add
        local.get 1
        i64.store
        local.get 2
        i32.const 2
        i32.store
        local.get 3
        return
      end
      local.get 2
      i32.const 0
      i32.store
      local.get 3
      return
    end
    local.get 2
    i32.const -1
    i32.store
    local.get 0
    i32.load)
  (func (;9;) (type 2) (param i32 i32) (result i32)
    (local i32 i32)
    block (result i32)  ;; label = @1
      i32.const 1
      local.get 0
      i32.load offset=8
      local.tee 2
      local.get 1
      i32.load offset=8
      local.tee 3
      i32.lt_u
      br_if 0 (;@1;)
      drop
      i32.const -1
      local.get 2
      local.get 3
      i32.gt_u
      br_if 0 (;@1;)
      drop
      i32.const 1
      i32.const -1
      local.get 0
      i64.load
      local.get 1
      i64.load
      i64.gt_u
      select
    end)
  (func (;10;) (type 2) (param i32 i32) (result i32)
    (local i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i64 i64 f64)
    block  ;; label = @1
      block  ;; label = @2
        local.get 0
        i32.load offset=4
        block (result i32)  ;; label = @3
          local.get 1
          i32.const -1
          i32.add
          local.tee 1
          i32.const 1
          i32.shr_u
          local.get 1
          i32.or
          local.tee 1
          i32.const 2
          i32.shr_u
          local.get 1
          i32.or
          local.tee 1
          i32.const 4
          i32.shr_u
          local.get 1
          i32.or
          local.tee 1
          i32.const 8
          i32.shr_u
          local.get 1
          i32.or
          local.tee 1
          i32.const 16
          i32.shr_u
          local.get 1
          i32.or
          i32.const 1
          i32.add
          local.tee 1
          i32.const 4
          local.get 1
          i32.const 4
          i32.gt_u
          select
          local.tee 2
          f64.convert_i32_u
          f64.const 0x1.8a3d70a3d70a4p-1 (;=0.77;)
          f64.mul
          f64.const 0x1p-1 (;=0.5;)
          f64.add
          local.tee 19
          f64.const 0x1p+32 (;=4.29497e+09;)
          f64.lt
          local.get 19
          f64.const 0x0p+0 (;=0;)
          f64.ge
          i32.and
          if  ;; label = @4
            local.get 19
            i32.trunc_f64_u
            br 1 (;@3;)
          end
          i32.const 0
        end
        local.tee 16
        i32.lt_u
        if  ;; label = @3
          i32.const 4
          local.get 2
          i32.const 2
          i32.shr_u
          i32.const 1073741820
          i32.and
          local.get 2
          i32.const 16
          i32.lt_u
          select
          local.tee 1
          call 11
          local.tee 3
          i32.eqz
          br_if 2 (;@1;)
          local.get 3
          i32.const 170
          local.get 1
          call 30
          local.set 7
          local.get 0
          i32.load
          local.tee 1
          local.get 2
          i32.lt_u
          if  ;; label = @4
            local.get 0
            i32.load offset=20
            local.get 2
            i32.const 3
            i32.shl
            call 14
            local.tee 1
            i32.eqz
            br_if 2 (;@2;)
            local.get 0
            local.get 1
            i32.store offset=20
            local.get 0
            i32.load offset=24
            local.get 2
            i32.const 2
            i32.shl
            call 14
            local.tee 1
            i32.eqz
            br_if 2 (;@2;)
            local.get 0
            local.get 1
            i32.store offset=24
            local.get 0
            i32.load
            local.set 1
          end
          block  ;; label = @4
            local.get 1
            i32.eqz
            br_if 0 (;@4;)
            local.get 2
            i32.const -1
            i32.add
            local.set 10
            local.get 0
            i32.load offset=16
            local.set 11
            loop  ;; label = @5
              block  ;; label = @6
                local.get 11
                local.get 6
                i32.const 2
                i32.shr_u
                i32.const 1073741820
                i32.and
                i32.add
                local.tee 4
                i32.load
                local.tee 3
                local.get 6
                i32.const 1
                i32.shl
                i32.const 30
                i32.and
                local.tee 5
                i32.shr_u
                i32.const 3
                i32.and
                br_if 0 (;@6;)
                local.get 3
                i32.const 1
                local.get 5
                i32.shl
                i32.or
                local.set 1
                local.get 0
                i32.load offset=24
                local.tee 12
                local.get 6
                i32.const 2
                i32.shl
                i32.add
                i32.load
                local.set 3
                local.get 0
                i32.load offset=20
                local.tee 13
                local.get 6
                i32.const 3
                i32.shl
                i32.add
                i64.load
                local.set 17
                loop  ;; label = @7
                  local.get 4
                  local.get 1
                  i32.store
                  i32.const 2
                  local.get 10
                  local.get 17
                  i64.const 7
                  i64.shr_u
                  local.get 17
                  i64.xor
                  i32.wrap_i64
                  i32.and
                  local.tee 1
                  i32.const 1
                  i32.shl
                  i32.const 30
                  i32.and
                  local.tee 9
                  i32.shl
                  local.tee 5
                  local.get 7
                  local.get 1
                  i32.const 4
                  i32.shr_u
                  local.tee 8
                  i32.const 2
                  i32.shl
                  i32.add
                  local.tee 14
                  i32.load
                  local.tee 15
                  i32.and
                  i32.eqz
                  if  ;; label = @8
                    i32.const 1
                    local.set 4
                    loop  ;; label = @9
                      local.get 1
                      local.get 4
                      i32.add
                      local.set 1
                      local.get 4
                      i32.const 1
                      i32.add
                      local.set 4
                      i32.const 2
                      local.get 1
                      local.get 10
                      i32.and
                      local.tee 1
                      i32.const 1
                      i32.shl
                      i32.const 30
                      i32.and
                      local.tee 9
                      i32.shl
                      local.tee 5
                      local.get 7
                      local.get 1
                      i32.const 4
                      i32.shr_u
                      local.tee 8
                      i32.const 2
                      i32.shl
                      i32.add
                      local.tee 14
                      i32.load
                      local.tee 15
                      i32.and
                      i32.eqz
                      br_if 0 (;@9;)
                    end
                  end
                  local.get 14
                  local.get 15
                  local.get 5
                  i32.const -1
                  i32.xor
                  i32.and
                  i32.store
                  block  ;; label = @8
                    local.get 1
                    local.get 0
                    i32.load
                    i32.lt_u
                    if  ;; label = @9
                      local.get 11
                      local.get 8
                      i32.const 2
                      i32.shl
                      i32.add
                      local.tee 4
                      i32.load
                      local.get 9
                      i32.shr_u
                      i32.const 3
                      i32.and
                      i32.eqz
                      br_if 1 (;@8;)
                    end
                    local.get 12
                    local.get 1
                    i32.const 2
                    i32.shl
                    i32.add
                    local.get 3
                    i32.store
                    local.get 13
                    local.get 1
                    i32.const 3
                    i32.shl
                    i32.add
                    local.get 17
                    i64.store
                    local.get 0
                    i32.load
                    local.set 1
                    br 2 (;@6;)
                  end
                  local.get 12
                  local.get 1
                  i32.const 2
                  i32.shl
                  i32.add
                  local.tee 8
                  i32.load
                  local.get 8
                  local.get 3
                  i32.store
                  local.get 13
                  local.get 1
                  i32.const 3
                  i32.shl
                  i32.add
                  local.tee 1
                  i64.load
                  local.get 1
                  local.get 17
                  i64.store
                  local.get 4
                  i32.load
                  i32.const 1
                  local.get 9
                  i32.shl
                  i32.or
                  local.set 1
                  local.set 17
                  local.set 3
                  br 0 (;@7;)
                end
                unreachable
              end
              local.get 6
              i32.const 1
              i32.add
              local.tee 6
              local.get 1
              i32.ne
              br_if 0 (;@5;)
            end
            local.get 1
            local.get 2
            i32.le_u
            br_if 0 (;@4;)
            local.get 0
            local.get 0
            i32.load offset=20
            local.get 2
            i32.const 3
            i32.shl
            call 14
            i32.store offset=20
            local.get 0
            local.get 0
            i32.load offset=24
            local.get 2
            i32.const 2
            i32.shl
            call 14
            i32.store offset=24
          end
          local.get 0
          i32.load offset=16
          call 12
          local.get 0
          local.get 2
          i32.store
          local.get 0
          local.get 7
          i32.store offset=16
          local.get 0
          local.get 16
          i32.store offset=12
          local.get 0
          local.get 0
          i32.load offset=4
          i32.store offset=8
        end
        i32.const 0
        return
      end
      local.get 7
      call 12
    end
    i32.const -1)
  (func (;11;) (type 1) (param i32) (result i32)
    (local i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32)
    global.get 0
    i32.const 16
    i32.sub
    local.tee 11
    global.set 0
    block  ;; label = @1
      block  ;; label = @2
        block  ;; label = @3
          block  ;; label = @4
            block  ;; label = @5
              block  ;; label = @6
                block  ;; label = @7
                  block  ;; label = @8
                    block  ;; label = @9
                      block  ;; label = @10
                        block  ;; label = @11
                          local.get 0
                          i32.const 236
                          i32.le_u
                          if  ;; label = @12
                            i32.const 1216
                            i32.load
                            local.tee 5
                            i32.const 16
                            local.get 0
                            i32.const 19
                            i32.add
                            i32.const -16
                            i32.and
                            local.get 0
                            i32.const 11
                            i32.lt_u
                            select
                            local.tee 6
                            i32.const 3
                            i32.shr_u
                            local.tee 0
                            i32.shr_u
                            local.tee 1
                            i32.const 3
                            i32.and
                            if  ;; label = @13
                              local.get 1
                              i32.const 1
                              i32.and
                              local.get 0
                              i32.or
                              i32.const 1
                              i32.xor
                              local.tee 2
                              i32.const 3
                              i32.shl
                              local.tee 4
                              i32.const 1264
                              i32.add
                              i32.load
                              local.tee 1
                              i32.const 8
                              i32.add
                              local.set 0
                              block  ;; label = @14
                                local.get 1
                                i32.load offset=8
                                local.tee 3
                                local.get 4
                                i32.const 1256
                                i32.add
                                local.tee 4
                                i32.eq
                                if  ;; label = @15
                                  i32.const 1216
                                  local.get 5
                                  i32.const -2
                                  local.get 2
                                  i32.rotl
                                  i32.and
                                  i32.store
                                  br 1 (;@14;)
                                end
                                i32.const 1232
                                i32.load
                                drop
                                local.get 4
                                local.get 3
                                i32.store offset=8
                                local.get 3
                                local.get 4
                                i32.store offset=12
                              end
                              local.get 1
                              local.get 2
                              i32.const 3
                              i32.shl
                              local.tee 2
                              i32.const 3
                              i32.or
                              i32.store offset=4
                              local.get 1
                              local.get 2
                              i32.add
                              local.tee 1
                              local.get 1
                              i32.load offset=4
                              i32.const 1
                              i32.or
                              i32.store offset=4
                              br 12 (;@1;)
                            end
                            local.get 6
                            i32.const 1224
                            i32.load
                            local.tee 8
                            i32.le_u
                            br_if 1 (;@11;)
                            local.get 1
                            if  ;; label = @13
                              block  ;; label = @14
                                i32.const 2
                                local.get 0
                                i32.shl
                                local.tee 2
                                i32.const 0
                                local.get 2
                                i32.sub
                                i32.or
                                local.get 1
                                local.get 0
                                i32.shl
                                i32.and
                                local.tee 0
                                i32.const 0
                                local.get 0
                                i32.sub
                                i32.and
                                i32.const -1
                                i32.add
                                local.tee 0
                                local.get 0
                                i32.const 12
                                i32.shr_u
                                i32.const 16
                                i32.and
                                local.tee 0
                                i32.shr_u
                                local.tee 1
                                i32.const 5
                                i32.shr_u
                                i32.const 8
                                i32.and
                                local.tee 2
                                local.get 0
                                i32.or
                                local.get 1
                                local.get 2
                                i32.shr_u
                                local.tee 0
                                i32.const 2
                                i32.shr_u
                                i32.const 4
                                i32.and
                                local.tee 1
                                i32.or
                                local.get 0
                                local.get 1
                                i32.shr_u
                                local.tee 0
                                i32.const 1
                                i32.shr_u
                                i32.const 2
                                i32.and
                                local.tee 1
                                i32.or
                                local.get 0
                                local.get 1
                                i32.shr_u
                                local.tee 0
                                i32.const 1
                                i32.shr_u
                                i32.const 1
                                i32.and
                                local.tee 1
                                i32.or
                                local.get 0
                                local.get 1
                                i32.shr_u
                                i32.add
                                local.tee 2
                                i32.const 3
                                i32.shl
                                local.tee 3
                                i32.const 1264
                                i32.add
                                i32.load
                                local.tee 1
                                i32.load offset=8
                                local.tee 0
                                local.get 3
                                i32.const 1256
                                i32.add
                                local.tee 3
                                i32.eq
                                if  ;; label = @15
                                  i32.const 1216
                                  local.get 5
                                  i32.const -2
                                  local.get 2
                                  i32.rotl
                                  i32.and
                                  local.tee 5
                                  i32.store
                                  br 1 (;@14;)
                                end
                                i32.const 1232
                                i32.load
                                drop
                                local.get 3
                                local.get 0
                                i32.store offset=8
                                local.get 0
                                local.get 3
                                i32.store offset=12
                              end
                              local.get 1
                              i32.const 8
                              i32.add
                              local.set 0
                              local.get 1
                              local.get 6
                              i32.const 3
                              i32.or
                              i32.store offset=4
                              local.get 1
                              local.get 2
                              i32.const 3
                              i32.shl
                              local.tee 2
                              i32.add
                              local.get 2
                              local.get 6
                              i32.sub
                              local.tee 4
                              i32.store
                              local.get 1
                              local.get 6
                              i32.add
                              local.tee 6
                              local.get 4
                              i32.const 1
                              i32.or
                              i32.store offset=4
                              local.get 8
                              if  ;; label = @14
                                local.get 8
                                i32.const 3
                                i32.shr_u
                                local.tee 3
                                i32.const 3
                                i32.shl
                                i32.const 1256
                                i32.add
                                local.set 1
                                i32.const 1236
                                i32.load
                                local.set 2
                                block (result i32)  ;; label = @15
                                  local.get 5
                                  i32.const 1
                                  local.get 3
                                  i32.shl
                                  local.tee 3
                                  i32.and
                                  i32.eqz
                                  if  ;; label = @16
                                    i32.const 1216
                                    local.get 3
                                    local.get 5
                                    i32.or
                                    i32.store
                                    local.get 1
                                    br 1 (;@15;)
                                  end
                                  local.get 1
                                  i32.load offset=8
                                end
                                local.tee 3
                                local.get 2
                                i32.store offset=12
                                local.get 1
                                local.get 2
                                i32.store offset=8
                                local.get 2
                                local.get 1
                                i32.store offset=12
                                local.get 2
                                local.get 3
                                i32.store offset=8
                              end
                              i32.const 1236
                              local.get 6
                              i32.store
                              i32.const 1224
                              local.get 4
                              i32.store
                              br 12 (;@1;)
                            end
                            i32.const 1220
                            i32.load
                            local.tee 10
                            i32.eqz
                            br_if 1 (;@11;)
                            local.get 10
                            i32.const 0
                            local.get 10
                            i32.sub
                            i32.and
                            i32.const -1
                            i32.add
                            local.tee 0
                            local.get 0
                            i32.const 12
                            i32.shr_u
                            i32.const 16
                            i32.and
                            local.tee 0
                            i32.shr_u
                            local.tee 1
                            i32.const 5
                            i32.shr_u
                            i32.const 8
                            i32.and
                            local.tee 2
                            local.get 0
                            i32.or
                            local.get 1
                            local.get 2
                            i32.shr_u
                            local.tee 0
                            i32.const 2
                            i32.shr_u
                            i32.const 4
                            i32.and
                            local.tee 1
                            i32.or
                            local.get 0
                            local.get 1
                            i32.shr_u
                            local.tee 0
                            i32.const 1
                            i32.shr_u
                            i32.const 2
                            i32.and
                            local.tee 1
                            i32.or
                            local.get 0
                            local.get 1
                            i32.shr_u
                            local.tee 0
                            i32.const 1
                            i32.shr_u
                            i32.const 1
                            i32.and
                            local.tee 1
                            i32.or
                            local.get 0
                            local.get 1
                            i32.shr_u
                            i32.add
                            i32.const 2
                            i32.shl
                            i32.const 1520
                            i32.add
                            i32.load
                            local.tee 1
                            i32.load offset=4
                            i32.const -8
                            i32.and
                            local.get 6
                            i32.sub
                            local.set 2
                            local.get 1
                            local.set 4
                            loop  ;; label = @13
                              block  ;; label = @14
                                local.get 4
                                i32.load offset=16
                                local.tee 0
                                i32.eqz
                                if  ;; label = @15
                                  local.get 4
                                  i32.const 20
                                  i32.add
                                  i32.load
                                  local.tee 0
                                  i32.eqz
                                  br_if 1 (;@14;)
                                end
                                local.get 0
                                i32.load offset=4
                                i32.const -8
                                i32.and
                                local.get 6
                                i32.sub
                                local.tee 3
                                local.get 2
                                local.get 3
                                local.get 2
                                i32.lt_u
                                local.tee 3
                                select
                                local.set 2
                                local.get 0
                                local.get 1
                                local.get 3
                                select
                                local.set 1
                                local.get 0
                                local.set 4
                                br 1 (;@13;)
                              end
                            end
                            local.get 1
                            i32.load offset=24
                            local.set 9
                            local.get 1
                            local.get 1
                            i32.load offset=12
                            local.tee 3
                            i32.ne
                            if  ;; label = @13
                              i32.const 1232
                              i32.load
                              local.get 1
                              i32.load offset=8
                              local.tee 0
                              i32.le_u
                              if  ;; label = @14
                                local.get 0
                                i32.load offset=12
                                drop
                              end
                              local.get 3
                              local.get 0
                              i32.store offset=8
                              local.get 0
                              local.get 3
                              i32.store offset=12
                              br 11 (;@2;)
                            end
                            local.get 1
                            i32.const 20
                            i32.add
                            local.tee 4
                            i32.load
                            local.tee 0
                            i32.eqz
                            if  ;; label = @13
                              local.get 1
                              i32.load offset=16
                              local.tee 0
                              i32.eqz
                              br_if 3 (;@10;)
                              local.get 1
                              i32.const 16
                              i32.add
                              local.set 4
                            end
                            loop  ;; label = @13
                              local.get 4
                              local.set 7
                              local.get 0
                              local.tee 3
                              i32.const 20
                              i32.add
                              local.tee 4
                              i32.load
                              local.tee 0
                              br_if 0 (;@13;)
                              local.get 3
                              i32.const 16
                              i32.add
                              local.set 4
                              local.get 3
                              i32.load offset=16
                              local.tee 0
                              br_if 0 (;@13;)
                            end
                            local.get 7
                            i32.const 0
                            i32.store
                            br 10 (;@2;)
                          end
                          i32.const -1
                          local.set 6
                          local.get 0
                          i32.const -65
                          i32.gt_u
                          br_if 0 (;@11;)
                          local.get 0
                          i32.const 19
                          i32.add
                          local.tee 0
                          i32.const -16
                          i32.and
                          local.set 6
                          i32.const 1220
                          i32.load
                          local.tee 8
                          i32.eqz
                          br_if 0 (;@11;)
                          i32.const 0
                          local.get 6
                          i32.sub
                          local.set 4
                          block  ;; label = @12
                            block  ;; label = @13
                              block  ;; label = @14
                                block (result i32)  ;; label = @15
                                  i32.const 0
                                  local.get 0
                                  i32.const 8
                                  i32.shr_u
                                  local.tee 0
                                  i32.eqz
                                  br_if 0 (;@15;)
                                  drop
                                  i32.const 31
                                  local.get 6
                                  i32.const 16777215
                                  i32.gt_u
                                  br_if 0 (;@15;)
                                  drop
                                  local.get 0
                                  local.get 0
                                  i32.const 1048320
                                  i32.add
                                  i32.const 16
                                  i32.shr_u
                                  i32.const 8
                                  i32.and
                                  local.tee 0
                                  i32.shl
                                  local.tee 1
                                  local.get 1
                                  i32.const 520192
                                  i32.add
                                  i32.const 16
                                  i32.shr_u
                                  i32.const 4
                                  i32.and
                                  local.tee 1
                                  i32.shl
                                  local.tee 2
                                  local.get 2
                                  i32.const 245760
                                  i32.add
                                  i32.const 16
                                  i32.shr_u
                                  i32.const 2
                                  i32.and
                                  local.tee 2
                                  i32.shl
                                  i32.const 15
                                  i32.shr_u
                                  local.get 0
                                  local.get 1
                                  i32.or
                                  local.get 2
                                  i32.or
                                  i32.sub
                                  local.tee 0
                                  i32.const 1
                                  i32.shl
                                  local.get 6
                                  local.get 0
                                  i32.const 21
                                  i32.add
                                  i32.shr_u
                                  i32.const 1
                                  i32.and
                                  i32.or
                                  i32.const 28
                                  i32.add
                                end
                                local.tee 7
                                i32.const 2
                                i32.shl
                                i32.const 1520
                                i32.add
                                i32.load
                                local.tee 2
                                i32.eqz
                                if  ;; label = @15
                                  i32.const 0
                                  local.set 0
                                  br 1 (;@14;)
                                end
                                local.get 6
                                i32.const 0
                                i32.const 25
                                local.get 7
                                i32.const 1
                                i32.shr_u
                                i32.sub
                                local.get 7
                                i32.const 31
                                i32.eq
                                select
                                i32.shl
                                local.set 1
                                i32.const 0
                                local.set 0
                                loop  ;; label = @15
                                  block  ;; label = @16
                                    local.get 2
                                    i32.load offset=4
                                    i32.const -8
                                    i32.and
                                    local.get 6
                                    i32.sub
                                    local.tee 5
                                    local.get 4
                                    i32.ge_u
                                    br_if 0 (;@16;)
                                    local.get 2
                                    local.set 3
                                    local.get 5
                                    local.tee 4
                                    br_if 0 (;@16;)
                                    i32.const 0
                                    local.set 4
                                    local.get 2
                                    local.set 0
                                    br 3 (;@13;)
                                  end
                                  local.get 0
                                  local.get 2
                                  i32.const 20
                                  i32.add
                                  i32.load
                                  local.tee 5
                                  local.get 5
                                  local.get 2
                                  local.get 1
                                  i32.const 29
                                  i32.shr_u
                                  i32.const 4
                                  i32.and
                                  i32.add
                                  i32.const 16
                                  i32.add
                                  i32.load
                                  local.tee 2
                                  i32.eq
                                  select
                                  local.get 0
                                  local.get 5
                                  select
                                  local.set 0
                                  local.get 1
                                  local.get 2
                                  i32.const 0
                                  i32.ne
                                  i32.shl
                                  local.set 1
                                  local.get 2
                                  br_if 0 (;@15;)
                                end
                              end
                              local.get 0
                              local.get 3
                              i32.or
                              i32.eqz
                              if  ;; label = @14
                                i32.const 2
                                local.get 7
                                i32.shl
                                local.tee 0
                                i32.const 0
                                local.get 0
                                i32.sub
                                i32.or
                                local.get 8
                                i32.and
                                local.tee 0
                                i32.eqz
                                br_if 3 (;@11;)
                                local.get 0
                                i32.const 0
                                local.get 0
                                i32.sub
                                i32.and
                                i32.const -1
                                i32.add
                                local.tee 0
                                local.get 0
                                i32.const 12
                                i32.shr_u
                                i32.const 16
                                i32.and
                                local.tee 0
                                i32.shr_u
                                local.tee 1
                                i32.const 5
                                i32.shr_u
                                i32.const 8
                                i32.and
                                local.tee 2
                                local.get 0
                                i32.or
                                local.get 1
                                local.get 2
                                i32.shr_u
                                local.tee 0
                                i32.const 2
                                i32.shr_u
                                i32.const 4
                                i32.and
                                local.tee 1
                                i32.or
                                local.get 0
                                local.get 1
                                i32.shr_u
                                local.tee 0
                                i32.const 1
                                i32.shr_u
                                i32.const 2
                                i32.and
                                local.tee 1
                                i32.or
                                local.get 0
                                local.get 1
                                i32.shr_u
                                local.tee 0
                                i32.const 1
                                i32.shr_u
                                i32.const 1
                                i32.and
                                local.tee 1
                                i32.or
                                local.get 0
                                local.get 1
                                i32.shr_u
                                i32.add
                                i32.const 2
                                i32.shl
                                i32.const 1520
                                i32.add
                                i32.load
                                local.set 0
                              end
                              local.get 0
                              i32.eqz
                              br_if 1 (;@12;)
                            end
                            loop  ;; label = @13
                              local.get 0
                              i32.load offset=4
                              i32.const -8
                              i32.and
                              local.get 6
                              i32.sub
                              local.tee 5
                              local.get 4
                              i32.lt_u
                              local.set 1
                              local.get 5
                              local.get 4
                              local.get 1
                              select
                              local.set 4
                              local.get 0
                              local.get 3
                              local.get 1
                              select
                              local.set 3
                              local.get 0
                              i32.load offset=16
                              local.tee 2
                              if (result i32)  ;; label = @14
                                local.get 2
                              else
                                local.get 0
                                i32.const 20
                                i32.add
                                i32.load
                              end
                              local.tee 0
                              br_if 0 (;@13;)
                            end
                          end
                          local.get 3
                          i32.eqz
                          br_if 0 (;@11;)
                          local.get 4
                          i32.const 1224
                          i32.load
                          local.get 6
                          i32.sub
                          i32.ge_u
                          br_if 0 (;@11;)
                          local.get 3
                          i32.load offset=24
                          local.set 7
                          local.get 3
                          local.get 3
                          i32.load offset=12
                          local.tee 1
                          i32.ne
                          if  ;; label = @12
                            i32.const 1232
                            i32.load
                            local.get 3
                            i32.load offset=8
                            local.tee 0
                            i32.le_u
                            if  ;; label = @13
                              local.get 0
                              i32.load offset=12
                              drop
                            end
                            local.get 1
                            local.get 0
                            i32.store offset=8
                            local.get 0
                            local.get 1
                            i32.store offset=12
                            br 9 (;@3;)
                          end
                          local.get 3
                          i32.const 20
                          i32.add
                          local.tee 2
                          i32.load
                          local.tee 0
                          i32.eqz
                          if  ;; label = @12
                            local.get 3
                            i32.load offset=16
                            local.tee 0
                            i32.eqz
                            br_if 3 (;@9;)
                            local.get 3
                            i32.const 16
                            i32.add
                            local.set 2
                          end
                          loop  ;; label = @12
                            local.get 2
                            local.set 5
                            local.get 0
                            local.tee 1
                            i32.const 20
                            i32.add
                            local.tee 2
                            i32.load
                            local.tee 0
                            br_if 0 (;@12;)
                            local.get 1
                            i32.const 16
                            i32.add
                            local.set 2
                            local.get 1
                            i32.load offset=16
                            local.tee 0
                            br_if 0 (;@12;)
                          end
                          local.get 5
                          i32.const 0
                          i32.store
                          br 8 (;@3;)
                        end
                        i32.const 1224
                        i32.load
                        local.tee 1
                        local.get 6
                        i32.ge_u
                        if  ;; label = @11
                          i32.const 1236
                          i32.load
                          local.set 0
                          block  ;; label = @12
                            local.get 1
                            local.get 6
                            i32.sub
                            local.tee 2
                            i32.const 16
                            i32.ge_u
                            if  ;; label = @13
                              local.get 0
                              local.get 6
                              i32.add
                              local.tee 3
                              local.get 2
                              i32.const 1
                              i32.or
                              i32.store offset=4
                              i32.const 1224
                              local.get 2
                              i32.store
                              i32.const 1236
                              local.get 3
                              i32.store
                              local.get 0
                              local.get 1
                              i32.add
                              local.get 2
                              i32.store
                              local.get 0
                              local.get 6
                              i32.const 3
                              i32.or
                              i32.store offset=4
                              br 1 (;@12;)
                            end
                            local.get 0
                            local.get 1
                            i32.const 3
                            i32.or
                            i32.store offset=4
                            local.get 0
                            local.get 1
                            i32.add
                            local.tee 1
                            local.get 1
                            i32.load offset=4
                            i32.const 1
                            i32.or
                            i32.store offset=4
                            i32.const 1236
                            i32.const 0
                            i32.store
                            i32.const 1224
                            i32.const 0
                            i32.store
                          end
                          local.get 0
                          i32.const 8
                          i32.add
                          local.set 0
                          br 10 (;@1;)
                        end
                        i32.const 1228
                        i32.load
                        local.tee 1
                        local.get 6
                        i32.gt_u
                        if  ;; label = @11
                          i32.const 1240
                          i32.load
                          local.tee 0
                          local.get 6
                          i32.add
                          local.tee 2
                          local.get 1
                          local.get 6
                          i32.sub
                          local.tee 1
                          i32.const 1
                          i32.or
                          i32.store offset=4
                          i32.const 1228
                          local.get 1
                          i32.store
                          i32.const 1240
                          local.get 2
                          i32.store
                          local.get 0
                          local.get 6
                          i32.const 3
                          i32.or
                          i32.store offset=4
                          local.get 0
                          i32.const 8
                          i32.add
                          local.set 0
                          br 10 (;@1;)
                        end
                        i32.const 0
                        local.set 0
                        local.get 6
                        i32.const 71
                        i32.add
                        local.tee 4
                        block (result i32)  ;; label = @11
                          i32.const 1688
                          i32.load
                          if  ;; label = @12
                            i32.const 1696
                            i32.load
                            br 1 (;@11;)
                          end
                          i32.const 1700
                          i64.const -1
                          i64.store align=4
                          i32.const 1692
                          i64.const 281474976776192
                          i64.store align=4
                          i32.const 1688
                          local.get 11
                          i32.const 12
                          i32.add
                          i32.const -16
                          i32.and
                          i32.const 1431655768
                          i32.xor
                          i32.store
                          i32.const 1708
                          i32.const 0
                          i32.store
                          i32.const 1660
                          i32.const 0
                          i32.store
                          i32.const 65536
                        end
                        local.tee 2
                        i32.add
                        local.tee 5
                        i32.const 0
                        local.get 2
                        i32.sub
                        local.tee 7
                        i32.and
                        local.tee 2
                        local.get 6
                        i32.le_u
                        if  ;; label = @11
                          i32.const 1712
                          i32.const 48
                          i32.store
                          br 10 (;@1;)
                        end
                        block  ;; label = @11
                          i32.const 1656
                          i32.load
                          local.tee 0
                          i32.eqz
                          br_if 0 (;@11;)
                          i32.const 1648
                          i32.load
                          local.tee 3
                          local.get 2
                          i32.add
                          local.tee 8
                          local.get 3
                          i32.gt_u
                          i32.const 0
                          local.get 8
                          local.get 0
                          i32.le_u
                          select
                          br_if 0 (;@11;)
                          i32.const 0
                          local.set 0
                          i32.const 1712
                          i32.const 48
                          i32.store
                          br 10 (;@1;)
                        end
                        i32.const 1660
                        i32.load8_u
                        i32.const 4
                        i32.and
                        br_if 4 (;@6;)
                        block  ;; label = @11
                          block  ;; label = @12
                            i32.const 1240
                            i32.load
                            local.tee 3
                            if  ;; label = @13
                              i32.const 1664
                              local.set 0
                              loop  ;; label = @14
                                local.get 0
                                i32.load
                                local.tee 8
                                local.get 3
                                i32.le_u
                                if  ;; label = @15
                                  local.get 8
                                  local.get 0
                                  i32.load offset=4
                                  i32.add
                                  local.get 3
                                  i32.gt_u
                                  br_if 3 (;@12;)
                                end
                                local.get 0
                                i32.load offset=8
                                local.tee 0
                                br_if 0 (;@14;)
                              end
                            end
                            i32.const 0
                            call 16
                            local.tee 1
                            i32.const -1
                            i32.eq
                            br_if 5 (;@7;)
                            local.get 2
                            local.set 5
                            i32.const 1692
                            i32.load
                            local.tee 0
                            i32.const -1
                            i32.add
                            local.tee 3
                            local.get 1
                            i32.and
                            if  ;; label = @13
                              local.get 2
                              local.get 1
                              i32.sub
                              local.get 1
                              local.get 3
                              i32.add
                              i32.const 0
                              local.get 0
                              i32.sub
                              i32.and
                              i32.add
                              local.set 5
                            end
                            local.get 5
                            local.get 6
                            i32.le_u
                            local.get 5
                            i32.const 2147483646
                            i32.gt_u
                            i32.or
                            br_if 5 (;@7;)
                            i32.const 1656
                            i32.load
                            local.tee 0
                            if  ;; label = @13
                              i32.const 1648
                              i32.load
                              local.tee 3
                              local.get 5
                              i32.add
                              local.tee 7
                              local.get 3
                              i32.le_u
                              local.get 7
                              local.get 0
                              i32.gt_u
                              i32.or
                              br_if 6 (;@7;)
                            end
                            local.get 5
                            call 16
                            local.tee 0
                            local.get 1
                            i32.ne
                            br_if 1 (;@11;)
                            br 7 (;@5;)
                          end
                          local.get 5
                          local.get 1
                          i32.sub
                          local.get 7
                          i32.and
                          local.tee 5
                          i32.const 2147483646
                          i32.gt_u
                          br_if 4 (;@7;)
                          local.get 5
                          call 16
                          local.tee 1
                          local.get 0
                          i32.load
                          local.get 0
                          i32.load offset=4
                          i32.add
                          i32.eq
                          br_if 3 (;@8;)
                          local.get 1
                          local.set 0
                        end
                        local.get 6
                        i32.const 72
                        i32.add
                        local.get 5
                        i32.le_u
                        local.get 5
                        i32.const 2147483646
                        i32.gt_u
                        i32.or
                        local.get 0
                        local.tee 1
                        i32.const -1
                        i32.eq
                        i32.or
                        i32.eqz
                        if  ;; label = @11
                          i32.const 1696
                          i32.load
                          local.tee 0
                          local.get 4
                          local.get 5
                          i32.sub
                          i32.add
                          i32.const 0
                          local.get 0
                          i32.sub
                          i32.and
                          local.tee 0
                          i32.const 2147483646
                          i32.gt_u
                          br_if 6 (;@5;)
                          local.get 0
                          call 16
                          i32.const -1
                          i32.ne
                          if  ;; label = @12
                            local.get 0
                            local.get 5
                            i32.add
                            local.set 5
                            br 7 (;@5;)
                          end
                          i32.const 0
                          local.get 5
                          i32.sub
                          call 16
                          drop
                          br 4 (;@7;)
                        end
                        local.get 1
                        i32.const -1
                        i32.ne
                        br_if 5 (;@5;)
                        br 3 (;@7;)
                      end
                      i32.const 0
                      local.set 3
                      br 7 (;@2;)
                    end
                    i32.const 0
                    local.set 1
                    br 5 (;@3;)
                  end
                  local.get 1
                  i32.const -1
                  i32.ne
                  br_if 2 (;@5;)
                end
                i32.const 1660
                i32.const 1660
                i32.load
                i32.const 4
                i32.or
                i32.store
              end
              local.get 2
              i32.const 2147483646
              i32.gt_u
              br_if 1 (;@4;)
              local.get 2
              call 16
              local.tee 1
              i32.const 0
              call 16
              local.tee 0
              i32.ge_u
              local.get 1
              i32.const -1
              i32.eq
              i32.or
              local.get 0
              i32.const -1
              i32.eq
              i32.or
              br_if 1 (;@4;)
              local.get 0
              local.get 1
              i32.sub
              local.tee 5
              local.get 6
              i32.const 56
              i32.add
              i32.le_u
              br_if 1 (;@4;)
            end
            i32.const 1648
            i32.const 1648
            i32.load
            local.get 5
            i32.add
            local.tee 0
            i32.store
            local.get 0
            i32.const 1652
            i32.load
            i32.gt_u
            if  ;; label = @5
              i32.const 1652
              local.get 0
              i32.store
            end
            block  ;; label = @5
              block  ;; label = @6
                block  ;; label = @7
                  i32.const 1240
                  i32.load
                  local.tee 7
                  if  ;; label = @8
                    i32.const 1664
                    local.set 0
                    loop  ;; label = @9
                      local.get 1
                      local.get 0
                      i32.load
                      local.tee 2
                      local.get 0
                      i32.load offset=4
                      local.tee 3
                      i32.add
                      i32.eq
                      br_if 2 (;@7;)
                      local.get 0
                      i32.load offset=8
                      local.tee 0
                      br_if 0 (;@9;)
                    end
                    br 2 (;@6;)
                  end
                  i32.const 1232
                  i32.load
                  local.tee 0
                  i32.const 0
                  local.get 1
                  local.get 0
                  i32.ge_u
                  select
                  i32.eqz
                  if  ;; label = @8
                    i32.const 1232
                    local.get 1
                    i32.store
                  end
                  i32.const 0
                  local.set 0
                  i32.const 1668
                  local.get 5
                  i32.store
                  i32.const 1664
                  local.get 1
                  i32.store
                  i32.const 1248
                  i32.const -1
                  i32.store
                  i32.const 1252
                  i32.const 1688
                  i32.load
                  i32.store
                  i32.const 1676
                  i32.const 0
                  i32.store
                  loop  ;; label = @8
                    local.get 0
                    i32.const 1264
                    i32.add
                    local.get 0
                    i32.const 1256
                    i32.add
                    local.tee 2
                    i32.store
                    local.get 0
                    i32.const 1268
                    i32.add
                    local.get 2
                    i32.store
                    local.get 0
                    i32.const 8
                    i32.add
                    local.tee 0
                    i32.const 256
                    i32.ne
                    br_if 0 (;@8;)
                  end
                  local.get 1
                  i32.const -8
                  local.get 1
                  i32.sub
                  i32.const 15
                  i32.and
                  i32.const 0
                  local.get 1
                  i32.const 8
                  i32.add
                  i32.const 15
                  i32.and
                  select
                  local.tee 0
                  i32.add
                  local.tee 2
                  local.get 5
                  i32.const -56
                  i32.add
                  local.tee 3
                  local.get 0
                  i32.sub
                  local.tee 0
                  i32.const 1
                  i32.or
                  i32.store offset=4
                  i32.const 1244
                  i32.const 1704
                  i32.load
                  i32.store
                  i32.const 1228
                  local.get 0
                  i32.store
                  i32.const 1240
                  local.get 2
                  i32.store
                  local.get 1
                  local.get 3
                  i32.add
                  i32.const 56
                  i32.store offset=4
                  br 2 (;@5;)
                end
                local.get 0
                i32.load8_u offset=12
                i32.const 8
                i32.and
                local.get 1
                local.get 7
                i32.le_u
                i32.or
                local.get 2
                local.get 7
                i32.gt_u
                i32.or
                br_if 0 (;@6;)
                local.get 7
                i32.const -8
                local.get 7
                i32.sub
                i32.const 15
                i32.and
                i32.const 0
                local.get 7
                i32.const 8
                i32.add
                i32.const 15
                i32.and
                select
                local.tee 1
                i32.add
                local.tee 2
                i32.const 1228
                i32.load
                local.get 5
                i32.add
                local.tee 4
                local.get 1
                i32.sub
                local.tee 1
                i32.const 1
                i32.or
                i32.store offset=4
                local.get 0
                local.get 3
                local.get 5
                i32.add
                i32.store offset=4
                i32.const 1244
                i32.const 1704
                i32.load
                i32.store
                i32.const 1228
                local.get 1
                i32.store
                i32.const 1240
                local.get 2
                i32.store
                local.get 4
                local.get 7
                i32.add
                i32.const 56
                i32.store offset=4
                br 1 (;@5;)
              end
              local.get 1
              i32.const 1232
              i32.load
              local.tee 3
              i32.lt_u
              if  ;; label = @6
                i32.const 1232
                local.get 1
                i32.store
                local.get 1
                local.set 3
              end
              local.get 1
              local.get 5
              i32.add
              local.set 2
              i32.const 1664
              local.set 0
              block  ;; label = @6
                block  ;; label = @7
                  block  ;; label = @8
                    block  ;; label = @9
                      block  ;; label = @10
                        block  ;; label = @11
                          loop  ;; label = @12
                            local.get 2
                            local.get 0
                            i32.load
                            i32.ne
                            if  ;; label = @13
                              local.get 0
                              i32.load offset=8
                              local.tee 0
                              br_if 1 (;@12;)
                              br 2 (;@11;)
                            end
                          end
                          local.get 0
                          i32.load8_u offset=12
                          i32.const 8
                          i32.and
                          i32.eqz
                          br_if 1 (;@10;)
                        end
                        i32.const 1664
                        local.set 0
                        loop  ;; label = @11
                          local.get 0
                          i32.load
                          local.tee 2
                          local.get 7
                          i32.le_u
                          if  ;; label = @12
                            local.get 2
                            local.get 0
                            i32.load offset=4
                            i32.add
                            local.tee 3
                            local.get 7
                            i32.gt_u
                            br_if 3 (;@9;)
                          end
                          local.get 0
                          i32.load offset=8
                          local.set 0
                          br 0 (;@11;)
                        end
                        unreachable
                      end
                      local.get 0
                      local.get 1
                      i32.store
                      local.get 0
                      local.get 0
                      i32.load offset=4
                      local.get 5
                      i32.add
                      i32.store offset=4
                      local.get 1
                      i32.const -8
                      local.get 1
                      i32.sub
                      i32.const 15
                      i32.and
                      i32.const 0
                      local.get 1
                      i32.const 8
                      i32.add
                      i32.const 15
                      i32.and
                      select
                      i32.add
                      local.tee 8
                      local.get 6
                      i32.const 3
                      i32.or
                      i32.store offset=4
                      local.get 2
                      i32.const -8
                      local.get 2
                      i32.sub
                      i32.const 15
                      i32.and
                      i32.const 0
                      local.get 2
                      i32.const 8
                      i32.add
                      i32.const 15
                      i32.and
                      select
                      i32.add
                      local.tee 1
                      local.get 8
                      i32.sub
                      local.get 6
                      i32.sub
                      local.set 0
                      local.get 6
                      local.get 8
                      i32.add
                      local.set 4
                      local.get 1
                      local.get 7
                      i32.eq
                      if  ;; label = @10
                        i32.const 1240
                        local.get 4
                        i32.store
                        i32.const 1228
                        i32.const 1228
                        i32.load
                        local.get 0
                        i32.add
                        local.tee 0
                        i32.store
                        local.get 4
                        local.get 0
                        i32.const 1
                        i32.or
                        i32.store offset=4
                        br 3 (;@7;)
                      end
                      local.get 1
                      i32.const 1236
                      i32.load
                      i32.eq
                      if  ;; label = @10
                        i32.const 1236
                        local.get 4
                        i32.store
                        i32.const 1224
                        i32.const 1224
                        i32.load
                        local.get 0
                        i32.add
                        local.tee 0
                        i32.store
                        local.get 4
                        local.get 0
                        i32.const 1
                        i32.or
                        i32.store offset=4
                        local.get 0
                        local.get 4
                        i32.add
                        local.get 0
                        i32.store
                        br 3 (;@7;)
                      end
                      local.get 1
                      i32.load offset=4
                      local.tee 6
                      i32.const 3
                      i32.and
                      i32.const 1
                      i32.eq
                      if  ;; label = @10
                        local.get 6
                        i32.const -8
                        i32.and
                        local.set 9
                        block  ;; label = @11
                          local.get 6
                          i32.const 255
                          i32.le_u
                          if  ;; label = @12
                            local.get 1
                            i32.load offset=8
                            local.tee 3
                            local.get 6
                            i32.const 3
                            i32.shr_u
                            local.tee 6
                            i32.const 3
                            i32.shl
                            i32.const 1256
                            i32.add
                            i32.ne
                            drop
                            local.get 3
                            local.get 1
                            i32.load offset=12
                            local.tee 2
                            i32.eq
                            if  ;; label = @13
                              i32.const 1216
                              i32.const 1216
                              i32.load
                              i32.const -2
                              local.get 6
                              i32.rotl
                              i32.and
                              i32.store
                              br 2 (;@11;)
                            end
                            local.get 2
                            local.get 3
                            i32.store offset=8
                            local.get 3
                            local.get 2
                            i32.store offset=12
                            br 1 (;@11;)
                          end
                          local.get 1
                          i32.load offset=24
                          local.set 7
                          block  ;; label = @12
                            local.get 1
                            local.get 1
                            i32.load offset=12
                            local.tee 5
                            i32.ne
                            if  ;; label = @13
                              local.get 3
                              local.get 1
                              i32.load offset=8
                              local.tee 2
                              i32.le_u
                              if  ;; label = @14
                                local.get 2
                                i32.load offset=12
                                drop
                              end
                              local.get 5
                              local.get 2
                              i32.store offset=8
                              local.get 2
                              local.get 5
                              i32.store offset=12
                              br 1 (;@12;)
                            end
                            block  ;; label = @13
                              local.get 1
                              i32.const 20
                              i32.add
                              local.tee 2
                              i32.load
                              local.tee 6
                              br_if 0 (;@13;)
                              local.get 1
                              i32.const 16
                              i32.add
                              local.tee 2
                              i32.load
                              local.tee 6
                              br_if 0 (;@13;)
                              i32.const 0
                              local.set 5
                              br 1 (;@12;)
                            end
                            loop  ;; label = @13
                              local.get 2
                              local.set 3
                              local.get 6
                              local.tee 5
                              i32.const 20
                              i32.add
                              local.tee 2
                              i32.load
                              local.tee 6
                              br_if 0 (;@13;)
                              local.get 5
                              i32.const 16
                              i32.add
                              local.set 2
                              local.get 5
                              i32.load offset=16
                              local.tee 6
                              br_if 0 (;@13;)
                            end
                            local.get 3
                            i32.const 0
                            i32.store
                          end
                          local.get 7
                          i32.eqz
                          br_if 0 (;@11;)
                          block  ;; label = @12
                            local.get 1
                            local.get 1
                            i32.load offset=28
                            local.tee 2
                            i32.const 2
                            i32.shl
                            i32.const 1520
                            i32.add
                            local.tee 3
                            i32.load
                            i32.eq
                            if  ;; label = @13
                              local.get 3
                              local.get 5
                              i32.store
                              local.get 5
                              br_if 1 (;@12;)
                              i32.const 1220
                              i32.const 1220
                              i32.load
                              i32.const -2
                              local.get 2
                              i32.rotl
                              i32.and
                              i32.store
                              br 2 (;@11;)
                            end
                            local.get 7
                            i32.const 16
                            i32.const 20
                            local.get 7
                            i32.load offset=16
                            local.get 1
                            i32.eq
                            select
                            i32.add
                            local.get 5
                            i32.store
                            local.get 5
                            i32.eqz
                            br_if 1 (;@11;)
                          end
                          local.get 5
                          local.get 7
                          i32.store offset=24
                          local.get 1
                          i32.load offset=16
                          local.tee 2
                          if  ;; label = @12
                            local.get 5
                            local.get 2
                            i32.store offset=16
                            local.get 2
                            local.get 5
                            i32.store offset=24
                          end
                          local.get 1
                          i32.load offset=20
                          local.tee 2
                          i32.eqz
                          br_if 0 (;@11;)
                          local.get 5
                          i32.const 20
                          i32.add
                          local.get 2
                          i32.store
                          local.get 2
                          local.get 5
                          i32.store offset=24
                        end
                        local.get 1
                        local.get 9
                        i32.add
                        local.set 1
                        local.get 0
                        local.get 9
                        i32.add
                        local.set 0
                      end
                      local.get 1
                      local.get 1
                      i32.load offset=4
                      i32.const -2
                      i32.and
                      i32.store offset=4
                      local.get 0
                      local.get 4
                      i32.add
                      local.get 0
                      i32.store
                      local.get 4
                      local.get 0
                      i32.const 1
                      i32.or
                      i32.store offset=4
                      local.get 0
                      i32.const 255
                      i32.le_u
                      if  ;; label = @10
                        local.get 0
                        i32.const 3
                        i32.shr_u
                        local.tee 1
                        i32.const 3
                        i32.shl
                        i32.const 1256
                        i32.add
                        local.set 0
                        block (result i32)  ;; label = @11
                          i32.const 1216
                          i32.load
                          local.tee 2
                          i32.const 1
                          local.get 1
                          i32.shl
                          local.tee 1
                          i32.and
                          i32.eqz
                          if  ;; label = @12
                            i32.const 1216
                            local.get 1
                            local.get 2
                            i32.or
                            i32.store
                            local.get 0
                            br 1 (;@11;)
                          end
                          local.get 0
                          i32.load offset=8
                        end
                        local.tee 2
                        local.get 4
                        i32.store offset=12
                        local.get 0
                        local.get 4
                        i32.store offset=8
                        local.get 4
                        local.get 0
                        i32.store offset=12
                        local.get 4
                        local.get 2
                        i32.store offset=8
                        br 3 (;@7;)
                      end
                      local.get 4
                      block (result i32)  ;; label = @10
                        i32.const 0
                        local.get 0
                        i32.const 8
                        i32.shr_u
                        local.tee 1
                        i32.eqz
                        br_if 0 (;@10;)
                        drop
                        i32.const 31
                        local.get 0
                        i32.const 16777215
                        i32.gt_u
                        br_if 0 (;@10;)
                        drop
                        local.get 1
                        local.get 1
                        i32.const 1048320
                        i32.add
                        i32.const 16
                        i32.shr_u
                        i32.const 8
                        i32.and
                        local.tee 1
                        i32.shl
                        local.tee 2
                        local.get 2
                        i32.const 520192
                        i32.add
                        i32.const 16
                        i32.shr_u
                        i32.const 4
                        i32.and
                        local.tee 2
                        i32.shl
                        local.tee 3
                        local.get 3
                        i32.const 245760
                        i32.add
                        i32.const 16
                        i32.shr_u
                        i32.const 2
                        i32.and
                        local.tee 3
                        i32.shl
                        i32.const 15
                        i32.shr_u
                        local.get 1
                        local.get 2
                        i32.or
                        local.get 3
                        i32.or
                        i32.sub
                        local.tee 1
                        i32.const 1
                        i32.shl
                        local.get 0
                        local.get 1
                        i32.const 21
                        i32.add
                        i32.shr_u
                        i32.const 1
                        i32.and
                        i32.or
                        i32.const 28
                        i32.add
                      end
                      local.tee 2
                      i32.store offset=28
                      local.get 4
                      i64.const 0
                      i64.store offset=16 align=4
                      local.get 2
                      i32.const 2
                      i32.shl
                      i32.const 1520
                      i32.add
                      local.set 1
                      i32.const 1220
                      i32.load
                      local.tee 3
                      i32.const 1
                      local.get 2
                      i32.shl
                      local.tee 6
                      i32.and
                      i32.eqz
                      if  ;; label = @10
                        local.get 1
                        local.get 4
                        i32.store
                        i32.const 1220
                        local.get 3
                        local.get 6
                        i32.or
                        i32.store
                        local.get 4
                        local.get 1
                        i32.store offset=24
                        local.get 4
                        local.get 4
                        i32.store offset=8
                        local.get 4
                        local.get 4
                        i32.store offset=12
                        br 3 (;@7;)
                      end
                      local.get 0
                      i32.const 0
                      i32.const 25
                      local.get 2
                      i32.const 1
                      i32.shr_u
                      i32.sub
                      local.get 2
                      i32.const 31
                      i32.eq
                      select
                      i32.shl
                      local.set 2
                      local.get 1
                      i32.load
                      local.set 1
                      loop  ;; label = @10
                        local.get 1
                        local.tee 3
                        i32.load offset=4
                        i32.const -8
                        i32.and
                        local.get 0
                        i32.eq
                        br_if 2 (;@8;)
                        local.get 2
                        i32.const 29
                        i32.shr_u
                        local.set 1
                        local.get 2
                        i32.const 1
                        i32.shl
                        local.set 2
                        local.get 3
                        local.get 1
                        i32.const 4
                        i32.and
                        i32.add
                        i32.const 16
                        i32.add
                        local.tee 6
                        i32.load
                        local.tee 1
                        br_if 0 (;@10;)
                      end
                      local.get 6
                      local.get 4
                      i32.store
                      local.get 4
                      local.get 3
                      i32.store offset=24
                      local.get 4
                      local.get 4
                      i32.store offset=12
                      local.get 4
                      local.get 4
                      i32.store offset=8
                      br 2 (;@7;)
                    end
                    local.get 1
                    i32.const -8
                    local.get 1
                    i32.sub
                    i32.const 15
                    i32.and
                    i32.const 0
                    local.get 1
                    i32.const 8
                    i32.add
                    i32.const 15
                    i32.and
                    select
                    local.tee 0
                    i32.add
                    local.tee 4
                    local.get 5
                    i32.const -56
                    i32.add
                    local.tee 2
                    local.get 0
                    i32.sub
                    local.tee 0
                    i32.const 1
                    i32.or
                    i32.store offset=4
                    local.get 1
                    local.get 2
                    i32.add
                    i32.const 56
                    i32.store offset=4
                    local.get 7
                    local.get 3
                    i32.const 55
                    local.get 3
                    i32.sub
                    i32.const 15
                    i32.and
                    i32.const 0
                    local.get 3
                    i32.const -55
                    i32.add
                    i32.const 15
                    i32.and
                    select
                    i32.add
                    i32.const -63
                    i32.add
                    local.tee 2
                    local.get 2
                    local.get 7
                    i32.const 16
                    i32.add
                    i32.lt_u
                    select
                    local.tee 2
                    i32.const 35
                    i32.store offset=4
                    i32.const 1244
                    i32.const 1704
                    i32.load
                    i32.store
                    i32.const 1228
                    local.get 0
                    i32.store
                    i32.const 1240
                    local.get 4
                    i32.store
                    local.get 2
                    i32.const 16
                    i32.add
                    i32.const 1672
                    i64.load align=4
                    i64.store align=4
                    local.get 2
                    i32.const 1664
                    i64.load align=4
                    i64.store offset=8 align=4
                    i32.const 1672
                    local.get 2
                    i32.const 8
                    i32.add
                    i32.store
                    i32.const 1668
                    local.get 5
                    i32.store
                    i32.const 1664
                    local.get 1
                    i32.store
                    i32.const 1676
                    i32.const 0
                    i32.store
                    local.get 2
                    i32.const 36
                    i32.add
                    local.set 0
                    loop  ;; label = @9
                      local.get 0
                      i32.const 7
                      i32.store
                      local.get 3
                      local.get 0
                      i32.const 4
                      i32.add
                      local.tee 0
                      i32.gt_u
                      br_if 0 (;@9;)
                    end
                    local.get 2
                    local.get 7
                    i32.eq
                    br_if 3 (;@5;)
                    local.get 2
                    local.get 2
                    i32.load offset=4
                    i32.const -2
                    i32.and
                    i32.store offset=4
                    local.get 2
                    local.get 2
                    local.get 7
                    i32.sub
                    local.tee 3
                    i32.store
                    local.get 7
                    local.get 3
                    i32.const 1
                    i32.or
                    i32.store offset=4
                    local.get 3
                    i32.const 255
                    i32.le_u
                    if  ;; label = @9
                      local.get 3
                      i32.const 3
                      i32.shr_u
                      local.tee 1
                      i32.const 3
                      i32.shl
                      i32.const 1256
                      i32.add
                      local.set 0
                      block (result i32)  ;; label = @10
                        i32.const 1216
                        i32.load
                        local.tee 2
                        i32.const 1
                        local.get 1
                        i32.shl
                        local.tee 1
                        i32.and
                        i32.eqz
                        if  ;; label = @11
                          i32.const 1216
                          local.get 1
                          local.get 2
                          i32.or
                          i32.store
                          local.get 0
                          br 1 (;@10;)
                        end
                        local.get 0
                        i32.load offset=8
                      end
                      local.tee 4
                      local.get 7
                      i32.store offset=12
                      local.get 0
                      local.get 7
                      i32.store offset=8
                      local.get 7
                      local.get 0
                      i32.store offset=12
                      local.get 7
                      local.get 4
                      i32.store offset=8
                      br 4 (;@5;)
                    end
                    local.get 7
                    i64.const 0
                    i64.store offset=16 align=4
                    local.get 7
                    i32.const 28
                    i32.add
                    block (result i32)  ;; label = @9
                      i32.const 0
                      local.get 3
                      i32.const 8
                      i32.shr_u
                      local.tee 1
                      i32.eqz
                      br_if 0 (;@9;)
                      drop
                      i32.const 31
                      local.get 3
                      i32.const 16777215
                      i32.gt_u
                      br_if 0 (;@9;)
                      drop
                      local.get 1
                      local.get 1
                      i32.const 1048320
                      i32.add
                      i32.const 16
                      i32.shr_u
                      i32.const 8
                      i32.and
                      local.tee 0
                      i32.shl
                      local.tee 1
                      local.get 1
                      i32.const 520192
                      i32.add
                      i32.const 16
                      i32.shr_u
                      i32.const 4
                      i32.and
                      local.tee 1
                      i32.shl
                      local.tee 2
                      local.get 2
                      i32.const 245760
                      i32.add
                      i32.const 16
                      i32.shr_u
                      i32.const 2
                      i32.and
                      local.tee 2
                      i32.shl
                      i32.const 15
                      i32.shr_u
                      local.get 0
                      local.get 1
                      i32.or
                      local.get 2
                      i32.or
                      i32.sub
                      local.tee 0
                      i32.const 1
                      i32.shl
                      local.get 3
                      local.get 0
                      i32.const 21
                      i32.add
                      i32.shr_u
                      i32.const 1
                      i32.and
                      i32.or
                      i32.const 28
                      i32.add
                    end
                    local.tee 0
                    i32.store
                    local.get 0
                    i32.const 2
                    i32.shl
                    i32.const 1520
                    i32.add
                    local.set 1
                    i32.const 1220
                    i32.load
                    local.tee 2
                    i32.const 1
                    local.get 0
                    i32.shl
                    local.tee 4
                    i32.and
                    i32.eqz
                    if  ;; label = @9
                      local.get 1
                      local.get 7
                      i32.store
                      i32.const 1220
                      local.get 2
                      local.get 4
                      i32.or
                      i32.store
                      local.get 7
                      i32.const 24
                      i32.add
                      local.get 1
                      i32.store
                      local.get 7
                      local.get 7
                      i32.store offset=8
                      local.get 7
                      local.get 7
                      i32.store offset=12
                      br 4 (;@5;)
                    end
                    local.get 3
                    i32.const 0
                    i32.const 25
                    local.get 0
                    i32.const 1
                    i32.shr_u
                    i32.sub
                    local.get 0
                    i32.const 31
                    i32.eq
                    select
                    i32.shl
                    local.set 0
                    local.get 1
                    i32.load
                    local.set 1
                    loop  ;; label = @9
                      local.get 1
                      local.tee 2
                      i32.load offset=4
                      i32.const -8
                      i32.and
                      local.get 3
                      i32.eq
                      br_if 3 (;@6;)
                      local.get 0
                      i32.const 29
                      i32.shr_u
                      local.set 1
                      local.get 0
                      i32.const 1
                      i32.shl
                      local.set 0
                      local.get 2
                      local.get 1
                      i32.const 4
                      i32.and
                      i32.add
                      i32.const 16
                      i32.add
                      local.tee 4
                      i32.load
                      local.tee 1
                      br_if 0 (;@9;)
                    end
                    local.get 4
                    local.get 7
                    i32.store
                    local.get 7
                    i32.const 24
                    i32.add
                    local.get 2
                    i32.store
                    local.get 7
                    local.get 7
                    i32.store offset=12
                    local.get 7
                    local.get 7
                    i32.store offset=8
                    br 3 (;@5;)
                  end
                  local.get 3
                  i32.load offset=8
                  local.set 0
                  local.get 3
                  local.get 4
                  i32.store offset=8
                  local.get 0
                  local.get 4
                  i32.store offset=12
                  local.get 4
                  i32.const 0
                  i32.store offset=24
                  local.get 4
                  local.get 0
                  i32.store offset=8
                  local.get 4
                  local.get 3
                  i32.store offset=12
                end
                local.get 8
                i32.const 8
                i32.add
                local.set 0
                br 5 (;@1;)
              end
              local.get 2
              i32.load offset=8
              local.set 0
              local.get 2
              local.get 7
              i32.store offset=8
              local.get 0
              local.get 7
              i32.store offset=12
              local.get 7
              i32.const 24
              i32.add
              i32.const 0
              i32.store
              local.get 7
              local.get 0
              i32.store offset=8
              local.get 7
              local.get 2
              i32.store offset=12
            end
            i32.const 1228
            i32.load
            local.tee 1
            local.get 6
            i32.le_u
            br_if 0 (;@4;)
            i32.const 1240
            i32.load
            local.tee 0
            local.get 6
            i32.add
            local.tee 2
            local.get 1
            local.get 6
            i32.sub
            local.tee 1
            i32.const 1
            i32.or
            i32.store offset=4
            i32.const 1228
            local.get 1
            i32.store
            i32.const 1240
            local.get 2
            i32.store
            local.get 0
            local.get 6
            i32.const 3
            i32.or
            i32.store offset=4
            local.get 0
            i32.const 8
            i32.add
            local.set 0
            br 3 (;@1;)
          end
          i32.const 0
          local.set 0
          i32.const 1712
          i32.const 48
          i32.store
          br 2 (;@1;)
        end
        block  ;; label = @3
          local.get 7
          i32.eqz
          br_if 0 (;@3;)
          block  ;; label = @4
            local.get 3
            i32.load offset=28
            local.tee 0
            i32.const 2
            i32.shl
            i32.const 1520
            i32.add
            local.tee 2
            i32.load
            local.get 3
            i32.eq
            if  ;; label = @5
              local.get 2
              local.get 1
              i32.store
              local.get 1
              br_if 1 (;@4;)
              i32.const 1220
              local.get 8
              i32.const -2
              local.get 0
              i32.rotl
              i32.and
              local.tee 8
              i32.store
              br 2 (;@3;)
            end
            local.get 7
            i32.const 16
            i32.const 20
            local.get 7
            i32.load offset=16
            local.get 3
            i32.eq
            select
            i32.add
            local.get 1
            i32.store
            local.get 1
            i32.eqz
            br_if 1 (;@3;)
          end
          local.get 1
          local.get 7
          i32.store offset=24
          local.get 3
          i32.load offset=16
          local.tee 0
          if  ;; label = @4
            local.get 1
            local.get 0
            i32.store offset=16
            local.get 0
            local.get 1
            i32.store offset=24
          end
          local.get 3
          i32.const 20
          i32.add
          i32.load
          local.tee 0
          i32.eqz
          br_if 0 (;@3;)
          local.get 1
          i32.const 20
          i32.add
          local.get 0
          i32.store
          local.get 0
          local.get 1
          i32.store offset=24
        end
        block  ;; label = @3
          local.get 4
          i32.const 15
          i32.le_u
          if  ;; label = @4
            local.get 3
            local.get 4
            local.get 6
            i32.add
            local.tee 0
            i32.const 3
            i32.or
            i32.store offset=4
            local.get 0
            local.get 3
            i32.add
            local.tee 0
            local.get 0
            i32.load offset=4
            i32.const 1
            i32.or
            i32.store offset=4
            br 1 (;@3;)
          end
          local.get 3
          local.get 6
          i32.add
          local.tee 5
          local.get 4
          i32.const 1
          i32.or
          i32.store offset=4
          local.get 3
          local.get 6
          i32.const 3
          i32.or
          i32.store offset=4
          local.get 4
          local.get 5
          i32.add
          local.get 4
          i32.store
          local.get 4
          i32.const 255
          i32.le_u
          if  ;; label = @4
            local.get 4
            i32.const 3
            i32.shr_u
            local.tee 1
            i32.const 3
            i32.shl
            i32.const 1256
            i32.add
            local.set 0
            block (result i32)  ;; label = @5
              i32.const 1216
              i32.load
              local.tee 2
              i32.const 1
              local.get 1
              i32.shl
              local.tee 1
              i32.and
              i32.eqz
              if  ;; label = @6
                i32.const 1216
                local.get 1
                local.get 2
                i32.or
                i32.store
                local.get 0
                br 1 (;@5;)
              end
              local.get 0
              i32.load offset=8
            end
            local.tee 2
            local.get 5
            i32.store offset=12
            local.get 0
            local.get 5
            i32.store offset=8
            local.get 5
            local.get 0
            i32.store offset=12
            local.get 5
            local.get 2
            i32.store offset=8
            br 1 (;@3;)
          end
          local.get 5
          block (result i32)  ;; label = @4
            i32.const 0
            local.get 4
            i32.const 8
            i32.shr_u
            local.tee 1
            i32.eqz
            br_if 0 (;@4;)
            drop
            i32.const 31
            local.get 4
            i32.const 16777215
            i32.gt_u
            br_if 0 (;@4;)
            drop
            local.get 1
            local.get 1
            i32.const 1048320
            i32.add
            i32.const 16
            i32.shr_u
            i32.const 8
            i32.and
            local.tee 0
            i32.shl
            local.tee 1
            local.get 1
            i32.const 520192
            i32.add
            i32.const 16
            i32.shr_u
            i32.const 4
            i32.and
            local.tee 1
            i32.shl
            local.tee 2
            local.get 2
            i32.const 245760
            i32.add
            i32.const 16
            i32.shr_u
            i32.const 2
            i32.and
            local.tee 2
            i32.shl
            i32.const 15
            i32.shr_u
            local.get 0
            local.get 1
            i32.or
            local.get 2
            i32.or
            i32.sub
            local.tee 0
            i32.const 1
            i32.shl
            local.get 4
            local.get 0
            i32.const 21
            i32.add
            i32.shr_u
            i32.const 1
            i32.and
            i32.or
            i32.const 28
            i32.add
          end
          local.tee 0
          i32.store offset=28
          local.get 5
          i64.const 0
          i64.store offset=16 align=4
          local.get 0
          i32.const 2
          i32.shl
          i32.const 1520
          i32.add
          local.set 1
          local.get 8
          i32.const 1
          local.get 0
          i32.shl
          local.tee 2
          i32.and
          i32.eqz
          if  ;; label = @4
            local.get 1
            local.get 5
            i32.store
            i32.const 1220
            local.get 2
            local.get 8
            i32.or
            i32.store
            local.get 5
            local.get 1
            i32.store offset=24
            local.get 5
            local.get 5
            i32.store offset=8
            local.get 5
            local.get 5
            i32.store offset=12
            br 1 (;@3;)
          end
          local.get 4
          i32.const 0
          i32.const 25
          local.get 0
          i32.const 1
          i32.shr_u
          i32.sub
          local.get 0
          i32.const 31
          i32.eq
          select
          i32.shl
          local.set 0
          local.get 1
          i32.load
          local.set 6
          block  ;; label = @4
            loop  ;; label = @5
              local.get 6
              local.tee 1
              i32.load offset=4
              i32.const -8
              i32.and
              local.get 4
              i32.eq
              br_if 1 (;@4;)
              local.get 0
              i32.const 29
              i32.shr_u
              local.set 2
              local.get 0
              i32.const 1
              i32.shl
              local.set 0
              local.get 1
              local.get 2
              i32.const 4
              i32.and
              i32.add
              i32.const 16
              i32.add
              local.tee 2
              i32.load
              local.tee 6
              br_if 0 (;@5;)
            end
            local.get 2
            local.get 5
            i32.store
            local.get 5
            local.get 1
            i32.store offset=24
            local.get 5
            local.get 5
            i32.store offset=12
            local.get 5
            local.get 5
            i32.store offset=8
            br 1 (;@3;)
          end
          local.get 1
          i32.load offset=8
          local.set 0
          local.get 1
          local.get 5
          i32.store offset=8
          local.get 0
          local.get 5
          i32.store offset=12
          local.get 5
          i32.const 0
          i32.store offset=24
          local.get 5
          local.get 0
          i32.store offset=8
          local.get 5
          local.get 1
          i32.store offset=12
        end
        local.get 3
        i32.const 8
        i32.add
        local.set 0
        br 1 (;@1;)
      end
      block  ;; label = @2
        local.get 9
        i32.eqz
        br_if 0 (;@2;)
        block  ;; label = @3
          local.get 1
          i32.load offset=28
          local.tee 0
          i32.const 2
          i32.shl
          i32.const 1520
          i32.add
          local.tee 4
          i32.load
          local.get 1
          i32.eq
          if  ;; label = @4
            local.get 4
            local.get 3
            i32.store
            local.get 3
            br_if 1 (;@3;)
            i32.const 1220
            local.get 10
            i32.const -2
            local.get 0
            i32.rotl
            i32.and
            i32.store
            br 2 (;@2;)
          end
          local.get 9
          i32.const 16
          i32.const 20
          local.get 9
          i32.load offset=16
          local.get 1
          i32.eq
          select
          i32.add
          local.get 3
          i32.store
          local.get 3
          i32.eqz
          br_if 1 (;@2;)
        end
        local.get 3
        local.get 9
        i32.store offset=24
        local.get 1
        i32.load offset=16
        local.tee 0
        if  ;; label = @3
          local.get 3
          local.get 0
          i32.store offset=16
          local.get 0
          local.get 3
          i32.store offset=24
        end
        local.get 1
        i32.const 20
        i32.add
        i32.load
        local.tee 0
        i32.eqz
        br_if 0 (;@2;)
        local.get 3
        i32.const 20
        i32.add
        local.get 0
        i32.store
        local.get 0
        local.get 3
        i32.store offset=24
      end
      block  ;; label = @2
        local.get 2
        i32.const 15
        i32.le_u
        if  ;; label = @3
          local.get 1
          local.get 2
          local.get 6
          i32.add
          local.tee 0
          i32.const 3
          i32.or
          i32.store offset=4
          local.get 0
          local.get 1
          i32.add
          local.tee 0
          local.get 0
          i32.load offset=4
          i32.const 1
          i32.or
          i32.store offset=4
          br 1 (;@2;)
        end
        local.get 1
        local.get 6
        i32.add
        local.tee 7
        local.get 2
        i32.const 1
        i32.or
        i32.store offset=4
        local.get 1
        local.get 6
        i32.const 3
        i32.or
        i32.store offset=4
        local.get 2
        local.get 7
        i32.add
        local.get 2
        i32.store
        local.get 8
        if  ;; label = @3
          local.get 8
          i32.const 3
          i32.shr_u
          local.tee 3
          i32.const 3
          i32.shl
          i32.const 1256
          i32.add
          local.set 0
          i32.const 1236
          i32.load
          local.set 4
          block (result i32)  ;; label = @4
            i32.const 1
            local.get 3
            i32.shl
            local.tee 3
            local.get 5
            i32.and
            i32.eqz
            if  ;; label = @5
              i32.const 1216
              local.get 3
              local.get 5
              i32.or
              i32.store
              local.get 0
              br 1 (;@4;)
            end
            local.get 0
            i32.load offset=8
          end
          local.tee 3
          local.get 4
          i32.store offset=12
          local.get 0
          local.get 4
          i32.store offset=8
          local.get 4
          local.get 0
          i32.store offset=12
          local.get 4
          local.get 3
          i32.store offset=8
        end
        i32.const 1236
        local.get 7
        i32.store
        i32.const 1224
        local.get 2
        i32.store
      end
      local.get 1
      i32.const 8
      i32.add
      local.set 0
    end
    local.get 11
    i32.const 16
    i32.add
    global.set 0
    local.get 0)
  (func (;12;) (type 5) (param i32)
    (local i32 i32 i32 i32 i32 i32 i32)
    block  ;; label = @1
      local.get 0
      i32.eqz
      br_if 0 (;@1;)
      local.get 0
      i32.const -8
      i32.add
      local.tee 3
      local.get 0
      i32.const -4
      i32.add
      i32.load
      local.tee 1
      i32.const -8
      i32.and
      local.tee 0
      i32.add
      local.set 5
      block  ;; label = @2
        local.get 1
        i32.const 1
        i32.and
        br_if 0 (;@2;)
        local.get 1
        i32.const 3
        i32.and
        i32.eqz
        br_if 1 (;@1;)
        local.get 3
        local.get 3
        i32.load
        local.tee 2
        i32.sub
        local.tee 3
        i32.const 1232
        i32.load
        local.tee 4
        i32.lt_u
        br_if 1 (;@1;)
        local.get 0
        local.get 2
        i32.add
        local.set 0
        local.get 3
        i32.const 1236
        i32.load
        i32.ne
        if  ;; label = @3
          local.get 2
          i32.const 255
          i32.le_u
          if  ;; label = @4
            local.get 3
            i32.load offset=8
            local.tee 4
            local.get 2
            i32.const 3
            i32.shr_u
            local.tee 2
            i32.const 3
            i32.shl
            i32.const 1256
            i32.add
            i32.ne
            drop
            local.get 4
            local.get 3
            i32.load offset=12
            local.tee 1
            i32.eq
            if  ;; label = @5
              i32.const 1216
              i32.const 1216
              i32.load
              i32.const -2
              local.get 2
              i32.rotl
              i32.and
              i32.store
              br 3 (;@2;)
            end
            local.get 1
            local.get 4
            i32.store offset=8
            local.get 4
            local.get 1
            i32.store offset=12
            br 2 (;@2;)
          end
          local.get 3
          i32.load offset=24
          local.set 6
          block  ;; label = @4
            local.get 3
            local.get 3
            i32.load offset=12
            local.tee 1
            i32.ne
            if  ;; label = @5
              local.get 4
              local.get 3
              i32.load offset=8
              local.tee 2
              i32.le_u
              if  ;; label = @6
                local.get 2
                i32.load offset=12
                drop
              end
              local.get 1
              local.get 2
              i32.store offset=8
              local.get 2
              local.get 1
              i32.store offset=12
              br 1 (;@4;)
            end
            block  ;; label = @5
              local.get 3
              i32.const 20
              i32.add
              local.tee 2
              i32.load
              local.tee 4
              br_if 0 (;@5;)
              local.get 3
              i32.const 16
              i32.add
              local.tee 2
              i32.load
              local.tee 4
              br_if 0 (;@5;)
              i32.const 0
              local.set 1
              br 1 (;@4;)
            end
            loop  ;; label = @5
              local.get 2
              local.set 7
              local.get 4
              local.tee 1
              i32.const 20
              i32.add
              local.tee 2
              i32.load
              local.tee 4
              br_if 0 (;@5;)
              local.get 1
              i32.const 16
              i32.add
              local.set 2
              local.get 1
              i32.load offset=16
              local.tee 4
              br_if 0 (;@5;)
            end
            local.get 7
            i32.const 0
            i32.store
          end
          local.get 6
          i32.eqz
          br_if 1 (;@2;)
          block  ;; label = @4
            local.get 3
            local.get 3
            i32.load offset=28
            local.tee 2
            i32.const 2
            i32.shl
            i32.const 1520
            i32.add
            local.tee 4
            i32.load
            i32.eq
            if  ;; label = @5
              local.get 4
              local.get 1
              i32.store
              local.get 1
              br_if 1 (;@4;)
              i32.const 1220
              i32.const 1220
              i32.load
              i32.const -2
              local.get 2
              i32.rotl
              i32.and
              i32.store
              br 3 (;@2;)
            end
            local.get 6
            i32.const 16
            i32.const 20
            local.get 6
            i32.load offset=16
            local.get 3
            i32.eq
            select
            i32.add
            local.get 1
            i32.store
            local.get 1
            i32.eqz
            br_if 2 (;@2;)
          end
          local.get 1
          local.get 6
          i32.store offset=24
          local.get 3
          i32.load offset=16
          local.tee 2
          if  ;; label = @4
            local.get 1
            local.get 2
            i32.store offset=16
            local.get 2
            local.get 1
            i32.store offset=24
          end
          local.get 3
          i32.load offset=20
          local.tee 2
          i32.eqz
          br_if 1 (;@2;)
          local.get 1
          i32.const 20
          i32.add
          local.get 2
          i32.store
          local.get 2
          local.get 1
          i32.store offset=24
          br 1 (;@2;)
        end
        local.get 5
        i32.load offset=4
        local.tee 1
        i32.const 3
        i32.and
        i32.const 3
        i32.ne
        br_if 0 (;@2;)
        local.get 5
        local.get 1
        i32.const -2
        i32.and
        i32.store offset=4
        i32.const 1224
        local.get 0
        i32.store
        local.get 0
        local.get 3
        i32.add
        local.get 0
        i32.store
        local.get 3
        local.get 0
        i32.const 1
        i32.or
        i32.store offset=4
        return
      end
      local.get 5
      local.get 3
      i32.le_u
      br_if 0 (;@1;)
      local.get 5
      i32.load offset=4
      local.tee 1
      i32.const 1
      i32.and
      i32.eqz
      br_if 0 (;@1;)
      block  ;; label = @2
        local.get 1
        i32.const 2
        i32.and
        i32.eqz
        if  ;; label = @3
          local.get 5
          i32.const 1240
          i32.load
          i32.eq
          if  ;; label = @4
            i32.const 1240
            local.get 3
            i32.store
            i32.const 1228
            i32.const 1228
            i32.load
            local.get 0
            i32.add
            local.tee 0
            i32.store
            local.get 3
            local.get 0
            i32.const 1
            i32.or
            i32.store offset=4
            local.get 3
            i32.const 1236
            i32.load
            i32.ne
            br_if 3 (;@1;)
            i32.const 1224
            i32.const 0
            i32.store
            i32.const 1236
            i32.const 0
            i32.store
            return
          end
          local.get 5
          i32.const 1236
          i32.load
          i32.eq
          if  ;; label = @4
            i32.const 1236
            local.get 3
            i32.store
            i32.const 1224
            i32.const 1224
            i32.load
            local.get 0
            i32.add
            local.tee 0
            i32.store
            local.get 3
            local.get 0
            i32.const 1
            i32.or
            i32.store offset=4
            local.get 0
            local.get 3
            i32.add
            local.get 0
            i32.store
            return
          end
          local.get 1
          i32.const -8
          i32.and
          local.get 0
          i32.add
          local.set 0
          block  ;; label = @4
            local.get 1
            i32.const 255
            i32.le_u
            if  ;; label = @5
              local.get 5
              i32.load offset=12
              local.set 2
              local.get 5
              i32.load offset=8
              local.tee 4
              local.get 1
              i32.const 3
              i32.shr_u
              local.tee 1
              i32.const 3
              i32.shl
              i32.const 1256
              i32.add
              local.tee 7
              i32.ne
              if  ;; label = @6
                i32.const 1232
                i32.load
                drop
              end
              local.get 2
              local.get 4
              i32.eq
              if  ;; label = @6
                i32.const 1216
                i32.const 1216
                i32.load
                i32.const -2
                local.get 1
                i32.rotl
                i32.and
                i32.store
                br 2 (;@4;)
              end
              local.get 2
              local.get 7
              i32.ne
              if  ;; label = @6
                i32.const 1232
                i32.load
                drop
              end
              local.get 2
              local.get 4
              i32.store offset=8
              local.get 4
              local.get 2
              i32.store offset=12
              br 1 (;@4;)
            end
            local.get 5
            i32.load offset=24
            local.set 6
            block  ;; label = @5
              local.get 5
              local.get 5
              i32.load offset=12
              local.tee 1
              i32.ne
              if  ;; label = @6
                i32.const 1232
                i32.load
                local.get 5
                i32.load offset=8
                local.tee 2
                i32.le_u
                if  ;; label = @7
                  local.get 2
                  i32.load offset=12
                  drop
                end
                local.get 1
                local.get 2
                i32.store offset=8
                local.get 2
                local.get 1
                i32.store offset=12
                br 1 (;@5;)
              end
              block  ;; label = @6
                local.get 5
                i32.const 20
                i32.add
                local.tee 2
                i32.load
                local.tee 4
                br_if 0 (;@6;)
                local.get 5
                i32.const 16
                i32.add
                local.tee 2
                i32.load
                local.tee 4
                br_if 0 (;@6;)
                i32.const 0
                local.set 1
                br 1 (;@5;)
              end
              loop  ;; label = @6
                local.get 2
                local.set 7
                local.get 4
                local.tee 1
                i32.const 20
                i32.add
                local.tee 2
                i32.load
                local.tee 4
                br_if 0 (;@6;)
                local.get 1
                i32.const 16
                i32.add
                local.set 2
                local.get 1
                i32.load offset=16
                local.tee 4
                br_if 0 (;@6;)
              end
              local.get 7
              i32.const 0
              i32.store
            end
            local.get 6
            i32.eqz
            br_if 0 (;@4;)
            block  ;; label = @5
              local.get 5
              local.get 5
              i32.load offset=28
              local.tee 2
              i32.const 2
              i32.shl
              i32.const 1520
              i32.add
              local.tee 4
              i32.load
              i32.eq
              if  ;; label = @6
                local.get 4
                local.get 1
                i32.store
                local.get 1
                br_if 1 (;@5;)
                i32.const 1220
                i32.const 1220
                i32.load
                i32.const -2
                local.get 2
                i32.rotl
                i32.and
                i32.store
                br 2 (;@4;)
              end
              local.get 6
              i32.const 16
              i32.const 20
              local.get 6
              i32.load offset=16
              local.get 5
              i32.eq
              select
              i32.add
              local.get 1
              i32.store
              local.get 1
              i32.eqz
              br_if 1 (;@4;)
            end
            local.get 1
            local.get 6
            i32.store offset=24
            local.get 5
            i32.load offset=16
            local.tee 2
            if  ;; label = @5
              local.get 1
              local.get 2
              i32.store offset=16
              local.get 2
              local.get 1
              i32.store offset=24
            end
            local.get 5
            i32.load offset=20
            local.tee 2
            i32.eqz
            br_if 0 (;@4;)
            local.get 1
            i32.const 20
            i32.add
            local.get 2
            i32.store
            local.get 2
            local.get 1
            i32.store offset=24
          end
          local.get 0
          local.get 3
          i32.add
          local.get 0
          i32.store
          local.get 3
          local.get 0
          i32.const 1
          i32.or
          i32.store offset=4
          local.get 3
          i32.const 1236
          i32.load
          i32.ne
          br_if 1 (;@2;)
          i32.const 1224
          local.get 0
          i32.store
          return
        end
        local.get 5
        local.get 1
        i32.const -2
        i32.and
        i32.store offset=4
        local.get 0
        local.get 3
        i32.add
        local.get 0
        i32.store
        local.get 3
        local.get 0
        i32.const 1
        i32.or
        i32.store offset=4
      end
      local.get 0
      i32.const 255
      i32.le_u
      if  ;; label = @2
        local.get 0
        i32.const 3
        i32.shr_u
        local.tee 1
        i32.const 3
        i32.shl
        i32.const 1256
        i32.add
        local.set 0
        block (result i32)  ;; label = @3
          i32.const 1216
          i32.load
          local.tee 2
          i32.const 1
          local.get 1
          i32.shl
          local.tee 1
          i32.and
          i32.eqz
          if  ;; label = @4
            i32.const 1216
            local.get 1
            local.get 2
            i32.or
            i32.store
            local.get 0
            br 1 (;@3;)
          end
          local.get 0
          i32.load offset=8
        end
        local.tee 2
        local.get 3
        i32.store offset=12
        local.get 0
        local.get 3
        i32.store offset=8
        local.get 3
        local.get 0
        i32.store offset=12
        local.get 3
        local.get 2
        i32.store offset=8
        return
      end
      local.get 3
      i64.const 0
      i64.store offset=16 align=4
      local.get 3
      i32.const 28
      i32.add
      block (result i32)  ;; label = @2
        i32.const 0
        local.get 0
        i32.const 8
        i32.shr_u
        local.tee 1
        i32.eqz
        br_if 0 (;@2;)
        drop
        i32.const 31
        local.get 0
        i32.const 16777215
        i32.gt_u
        br_if 0 (;@2;)
        drop
        local.get 1
        local.get 1
        i32.const 1048320
        i32.add
        i32.const 16
        i32.shr_u
        i32.const 8
        i32.and
        local.tee 1
        i32.shl
        local.tee 2
        local.get 2
        i32.const 520192
        i32.add
        i32.const 16
        i32.shr_u
        i32.const 4
        i32.and
        local.tee 2
        i32.shl
        local.tee 4
        local.get 4
        i32.const 245760
        i32.add
        i32.const 16
        i32.shr_u
        i32.const 2
        i32.and
        local.tee 4
        i32.shl
        i32.const 15
        i32.shr_u
        local.get 1
        local.get 2
        i32.or
        local.get 4
        i32.or
        i32.sub
        local.tee 1
        i32.const 1
        i32.shl
        local.get 0
        local.get 1
        i32.const 21
        i32.add
        i32.shr_u
        i32.const 1
        i32.and
        i32.or
        i32.const 28
        i32.add
      end
      local.tee 2
      i32.store
      local.get 2
      i32.const 2
      i32.shl
      i32.const 1520
      i32.add
      local.set 1
      block  ;; label = @2
        i32.const 1220
        i32.load
        local.tee 4
        i32.const 1
        local.get 2
        i32.shl
        local.tee 7
        i32.and
        i32.eqz
        if  ;; label = @3
          local.get 1
          local.get 3
          i32.store
          i32.const 1220
          local.get 4
          local.get 7
          i32.or
          i32.store
          local.get 3
          i32.const 24
          i32.add
          local.get 1
          i32.store
          local.get 3
          local.get 3
          i32.store offset=8
          local.get 3
          local.get 3
          i32.store offset=12
          br 1 (;@2;)
        end
        local.get 0
        i32.const 0
        i32.const 25
        local.get 2
        i32.const 1
        i32.shr_u
        i32.sub
        local.get 2
        i32.const 31
        i32.eq
        select
        i32.shl
        local.set 2
        local.get 1
        i32.load
        local.set 1
        block  ;; label = @3
          loop  ;; label = @4
            local.get 1
            local.tee 4
            i32.load offset=4
            i32.const -8
            i32.and
            local.get 0
            i32.eq
            br_if 1 (;@3;)
            local.get 2
            i32.const 29
            i32.shr_u
            local.set 1
            local.get 2
            i32.const 1
            i32.shl
            local.set 2
            local.get 4
            local.get 1
            i32.const 4
            i32.and
            i32.add
            i32.const 16
            i32.add
            local.tee 7
            i32.load
            local.tee 1
            br_if 0 (;@4;)
          end
          local.get 7
          local.get 3
          i32.store
          local.get 3
          i32.const 24
          i32.add
          local.get 4
          i32.store
          local.get 3
          local.get 3
          i32.store offset=12
          local.get 3
          local.get 3
          i32.store offset=8
          br 1 (;@2;)
        end
        local.get 4
        i32.load offset=8
        local.set 0
        local.get 4
        local.get 3
        i32.store offset=8
        local.get 0
        local.get 3
        i32.store offset=12
        local.get 3
        i32.const 24
        i32.add
        i32.const 0
        i32.store
        local.get 3
        local.get 0
        i32.store offset=8
        local.get 3
        local.get 4
        i32.store offset=12
      end
      i32.const 1248
      i32.const 1248
      i32.load
      i32.const -1
      i32.add
      local.tee 0
      i32.store
      local.get 0
      br_if 0 (;@1;)
      i32.const 1672
      local.set 3
      loop  ;; label = @2
        local.get 3
        i32.load
        local.tee 0
        i32.const 8
        i32.add
        local.set 3
        local.get 0
        br_if 0 (;@2;)
      end
      i32.const 1248
      i32.const -1
      i32.store
    end)
  (func (;13;) (type 8) (result i32)
    (local i32)
    block  ;; label = @1
      i32.const 28
      call 11
      local.tee 0
      i32.eqz
      br_if 0 (;@1;)
      local.get 0
      i32.const -4
      i32.add
      i32.load8_u
      i32.const 3
      i32.and
      i32.eqz
      br_if 0 (;@1;)
      local.get 0
      i32.const 0
      i32.const 28
      call 30
      drop
    end
    local.get 0)
  (func (;14;) (type 2) (param i32 i32) (result i32)
    (local i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32)
    local.get 0
    i32.eqz
    if  ;; label = @1
      local.get 1
      call 11
      return
    end
    local.get 1
    i32.const -64
    i32.ge_u
    if  ;; label = @1
      i32.const 1712
      i32.const 48
      i32.store
      i32.const 0
      return
    end
    local.get 0
    i32.const -8
    i32.add
    local.set 6
    i32.const 1232
    i32.load
    local.set 11
    local.get 0
    i32.const -4
    i32.add
    local.tee 7
    i32.load
    local.tee 8
    i32.const 3
    i32.and
    local.tee 5
    i32.const 1
    i32.eq
    local.get 8
    i32.const -8
    i32.and
    local.tee 2
    i32.const 1
    i32.lt_s
    i32.or
    drop
    i32.const 16
    local.get 1
    i32.const 19
    i32.add
    i32.const -16
    i32.and
    local.get 1
    i32.const 11
    i32.lt_u
    select
    local.set 3
    block  ;; label = @1
      block  ;; label = @2
        local.get 5
        i32.eqz
        if  ;; label = @3
          local.get 3
          i32.const 256
          i32.lt_u
          local.get 2
          local.get 3
          i32.const 4
          i32.or
          i32.lt_u
          i32.or
          br_if 1 (;@2;)
          local.get 2
          local.get 3
          i32.sub
          i32.const 1696
          i32.load
          i32.const 1
          i32.shl
          i32.le_u
          br_if 2 (;@1;)
          br 1 (;@2;)
        end
        local.get 2
        local.get 6
        i32.add
        local.set 4
        local.get 2
        local.get 3
        i32.ge_u
        if  ;; label = @3
          local.get 2
          local.get 3
          i32.sub
          local.tee 1
          i32.const 16
          i32.lt_u
          br_if 2 (;@1;)
          local.get 7
          local.get 3
          local.get 8
          i32.const 1
          i32.and
          i32.or
          i32.const 2
          i32.or
          i32.store
          local.get 3
          local.get 6
          i32.add
          local.tee 2
          local.get 1
          i32.const 3
          i32.or
          i32.store offset=4
          local.get 4
          local.get 4
          i32.load offset=4
          i32.const 1
          i32.or
          i32.store offset=4
          local.get 2
          local.get 1
          call 15
          local.get 0
          return
        end
        local.get 4
        i32.const 1240
        i32.load
        i32.eq
        if  ;; label = @3
          i32.const 1228
          i32.load
          local.get 2
          i32.add
          local.tee 2
          local.get 3
          i32.le_u
          br_if 1 (;@2;)
          local.get 7
          local.get 3
          local.get 8
          i32.const 1
          i32.and
          i32.or
          i32.const 2
          i32.or
          i32.store
          i32.const 1240
          local.get 3
          local.get 6
          i32.add
          local.tee 1
          i32.store
          i32.const 1228
          local.get 2
          local.get 3
          i32.sub
          local.tee 2
          i32.store
          local.get 1
          local.get 2
          i32.const 1
          i32.or
          i32.store offset=4
          local.get 0
          return
        end
        local.get 4
        i32.const 1236
        i32.load
        i32.eq
        if  ;; label = @3
          i32.const 1224
          i32.load
          local.get 2
          i32.add
          local.tee 2
          local.get 3
          i32.lt_u
          br_if 1 (;@2;)
          block  ;; label = @4
            local.get 2
            local.get 3
            i32.sub
            local.tee 1
            i32.const 16
            i32.ge_u
            if  ;; label = @5
              local.get 7
              local.get 3
              local.get 8
              i32.const 1
              i32.and
              i32.or
              i32.const 2
              i32.or
              i32.store
              local.get 3
              local.get 6
              i32.add
              local.tee 5
              local.get 1
              i32.const 1
              i32.or
              i32.store offset=4
              local.get 2
              local.get 6
              i32.add
              local.tee 2
              local.get 1
              i32.store
              local.get 2
              local.get 2
              i32.load offset=4
              i32.const -2
              i32.and
              i32.store offset=4
              br 1 (;@4;)
            end
            local.get 7
            local.get 8
            i32.const 1
            i32.and
            local.get 2
            i32.or
            i32.const 2
            i32.or
            i32.store
            local.get 2
            local.get 6
            i32.add
            local.tee 1
            local.get 1
            i32.load offset=4
            i32.const 1
            i32.or
            i32.store offset=4
            i32.const 0
            local.set 1
            i32.const 0
            local.set 5
          end
          i32.const 1236
          local.get 5
          i32.store
          i32.const 1224
          local.get 1
          i32.store
          local.get 0
          return
        end
        local.get 4
        i32.load offset=4
        local.tee 5
        i32.const 2
        i32.and
        br_if 0 (;@2;)
        local.get 5
        i32.const -8
        i32.and
        local.get 2
        i32.add
        local.tee 9
        local.get 3
        i32.lt_u
        br_if 0 (;@2;)
        local.get 9
        local.get 3
        i32.sub
        local.set 12
        block  ;; label = @3
          local.get 5
          i32.const 255
          i32.le_u
          if  ;; label = @4
            local.get 4
            i32.load offset=8
            local.tee 2
            local.get 5
            i32.const 3
            i32.shr_u
            local.tee 5
            i32.const 3
            i32.shl
            i32.const 1256
            i32.add
            i32.ne
            drop
            local.get 2
            local.get 4
            i32.load offset=12
            local.tee 1
            i32.eq
            if  ;; label = @5
              i32.const 1216
              i32.const 1216
              i32.load
              i32.const -2
              local.get 5
              i32.rotl
              i32.and
              i32.store
              br 2 (;@3;)
            end
            local.get 1
            local.get 2
            i32.store offset=8
            local.get 2
            local.get 1
            i32.store offset=12
            br 1 (;@3;)
          end
          local.get 4
          i32.load offset=24
          local.set 10
          block  ;; label = @4
            local.get 4
            local.get 4
            i32.load offset=12
            local.tee 2
            i32.ne
            if  ;; label = @5
              local.get 11
              local.get 4
              i32.load offset=8
              local.tee 1
              i32.le_u
              if  ;; label = @6
                local.get 1
                i32.load offset=12
                drop
              end
              local.get 2
              local.get 1
              i32.store offset=8
              local.get 1
              local.get 2
              i32.store offset=12
              br 1 (;@4;)
            end
            block  ;; label = @5
              local.get 4
              i32.const 20
              i32.add
              local.tee 1
              i32.load
              local.tee 5
              br_if 0 (;@5;)
              local.get 4
              i32.const 16
              i32.add
              local.tee 1
              i32.load
              local.tee 5
              br_if 0 (;@5;)
              i32.const 0
              local.set 2
              br 1 (;@4;)
            end
            loop  ;; label = @5
              local.get 1
              local.set 11
              local.get 5
              local.tee 2
              i32.const 20
              i32.add
              local.tee 1
              i32.load
              local.tee 5
              br_if 0 (;@5;)
              local.get 2
              i32.const 16
              i32.add
              local.set 1
              local.get 2
              i32.load offset=16
              local.tee 5
              br_if 0 (;@5;)
            end
            local.get 11
            i32.const 0
            i32.store
          end
          local.get 10
          i32.eqz
          br_if 0 (;@3;)
          block  ;; label = @4
            local.get 4
            local.get 4
            i32.load offset=28
            local.tee 1
            i32.const 2
            i32.shl
            i32.const 1520
            i32.add
            local.tee 5
            i32.load
            i32.eq
            if  ;; label = @5
              local.get 5
              local.get 2
              i32.store
              local.get 2
              br_if 1 (;@4;)
              i32.const 1220
              i32.const 1220
              i32.load
              i32.const -2
              local.get 1
              i32.rotl
              i32.and
              i32.store
              br 2 (;@3;)
            end
            local.get 10
            i32.const 16
            i32.const 20
            local.get 10
            i32.load offset=16
            local.get 4
            i32.eq
            select
            i32.add
            local.get 2
            i32.store
            local.get 2
            i32.eqz
            br_if 1 (;@3;)
          end
          local.get 2
          local.get 10
          i32.store offset=24
          local.get 4
          i32.load offset=16
          local.tee 1
          if  ;; label = @4
            local.get 2
            local.get 1
            i32.store offset=16
            local.get 1
            local.get 2
            i32.store offset=24
          end
          local.get 4
          i32.load offset=20
          local.tee 1
          i32.eqz
          br_if 0 (;@3;)
          local.get 2
          i32.const 20
          i32.add
          local.get 1
          i32.store
          local.get 1
          local.get 2
          i32.store offset=24
        end
        local.get 12
        i32.const 15
        i32.le_u
        if  ;; label = @3
          local.get 7
          local.get 8
          i32.const 1
          i32.and
          local.get 9
          i32.or
          i32.const 2
          i32.or
          i32.store
          local.get 6
          local.get 9
          i32.add
          local.tee 1
          local.get 1
          i32.load offset=4
          i32.const 1
          i32.or
          i32.store offset=4
          local.get 0
          return
        end
        local.get 7
        local.get 3
        local.get 8
        i32.const 1
        i32.and
        i32.or
        i32.const 2
        i32.or
        i32.store
        local.get 3
        local.get 6
        i32.add
        local.tee 1
        local.get 12
        i32.const 3
        i32.or
        i32.store offset=4
        local.get 6
        local.get 9
        i32.add
        local.tee 2
        local.get 2
        i32.load offset=4
        i32.const 1
        i32.or
        i32.store offset=4
        local.get 1
        local.get 12
        call 15
        local.get 0
        return
      end
      local.get 1
      call 11
      local.tee 2
      i32.eqz
      if  ;; label = @2
        i32.const 0
        return
      end
      local.get 2
      local.get 0
      local.get 7
      i32.load
      local.tee 2
      i32.const -8
      i32.and
      i32.const 4
      i32.const 8
      local.get 2
      i32.const 3
      i32.and
      select
      i32.sub
      local.tee 2
      local.get 1
      local.get 2
      local.get 1
      i32.lt_u
      select
      call 29
      local.get 0
      call 12
      local.set 0
    end
    local.get 0)
  (func (;15;) (type 6) (param i32 i32)
    (local i32 i32 i32 i32 i32 i32)
    local.get 0
    local.get 1
    i32.add
    local.set 5
    block  ;; label = @1
      block  ;; label = @2
        local.get 0
        i32.load offset=4
        local.tee 2
        i32.const 1
        i32.and
        br_if 0 (;@2;)
        local.get 2
        i32.const 3
        i32.and
        i32.eqz
        br_if 1 (;@1;)
        local.get 0
        i32.load
        local.tee 3
        local.get 1
        i32.add
        local.set 1
        local.get 0
        local.get 3
        i32.sub
        local.tee 0
        i32.const 1236
        i32.load
        i32.ne
        if  ;; label = @3
          i32.const 1232
          i32.load
          local.set 4
          local.get 3
          i32.const 255
          i32.le_u
          if  ;; label = @4
            local.get 0
            i32.load offset=8
            local.tee 4
            local.get 3
            i32.const 3
            i32.shr_u
            local.tee 3
            i32.const 3
            i32.shl
            i32.const 1256
            i32.add
            i32.ne
            drop
            local.get 4
            local.get 0
            i32.load offset=12
            local.tee 2
            i32.eq
            if  ;; label = @5
              i32.const 1216
              i32.const 1216
              i32.load
              i32.const -2
              local.get 3
              i32.rotl
              i32.and
              i32.store
              br 3 (;@2;)
            end
            local.get 2
            local.get 4
            i32.store offset=8
            local.get 4
            local.get 2
            i32.store offset=12
            br 2 (;@2;)
          end
          local.get 0
          i32.load offset=24
          local.set 6
          block  ;; label = @4
            local.get 0
            local.get 0
            i32.load offset=12
            local.tee 2
            i32.ne
            if  ;; label = @5
              local.get 4
              local.get 0
              i32.load offset=8
              local.tee 3
              i32.le_u
              if  ;; label = @6
                local.get 3
                i32.load offset=12
                drop
              end
              local.get 2
              local.get 3
              i32.store offset=8
              local.get 3
              local.get 2
              i32.store offset=12
              br 1 (;@4;)
            end
            block  ;; label = @5
              local.get 0
              i32.const 20
              i32.add
              local.tee 3
              i32.load
              local.tee 4
              br_if 0 (;@5;)
              local.get 0
              i32.const 16
              i32.add
              local.tee 3
              i32.load
              local.tee 4
              br_if 0 (;@5;)
              i32.const 0
              local.set 2
              br 1 (;@4;)
            end
            loop  ;; label = @5
              local.get 3
              local.set 7
              local.get 4
              local.tee 2
              i32.const 20
              i32.add
              local.tee 3
              i32.load
              local.tee 4
              br_if 0 (;@5;)
              local.get 2
              i32.const 16
              i32.add
              local.set 3
              local.get 2
              i32.load offset=16
              local.tee 4
              br_if 0 (;@5;)
            end
            local.get 7
            i32.const 0
            i32.store
          end
          local.get 6
          i32.eqz
          br_if 1 (;@2;)
          block  ;; label = @4
            local.get 0
            local.get 0
            i32.load offset=28
            local.tee 3
            i32.const 2
            i32.shl
            i32.const 1520
            i32.add
            local.tee 4
            i32.load
            i32.eq
            if  ;; label = @5
              local.get 4
              local.get 2
              i32.store
              local.get 2
              br_if 1 (;@4;)
              i32.const 1220
              i32.const 1220
              i32.load
              i32.const -2
              local.get 3
              i32.rotl
              i32.and
              i32.store
              br 3 (;@2;)
            end
            local.get 6
            i32.const 16
            i32.const 20
            local.get 6
            i32.load offset=16
            local.get 0
            i32.eq
            select
            i32.add
            local.get 2
            i32.store
            local.get 2
            i32.eqz
            br_if 2 (;@2;)
          end
          local.get 2
          local.get 6
          i32.store offset=24
          local.get 0
          i32.load offset=16
          local.tee 3
          if  ;; label = @4
            local.get 2
            local.get 3
            i32.store offset=16
            local.get 3
            local.get 2
            i32.store offset=24
          end
          local.get 0
          i32.load offset=20
          local.tee 3
          i32.eqz
          br_if 1 (;@2;)
          local.get 2
          i32.const 20
          i32.add
          local.get 3
          i32.store
          local.get 3
          local.get 2
          i32.store offset=24
          br 1 (;@2;)
        end
        local.get 5
        i32.load offset=4
        local.tee 2
        i32.const 3
        i32.and
        i32.const 3
        i32.ne
        br_if 0 (;@2;)
        local.get 5
        local.get 2
        i32.const -2
        i32.and
        i32.store offset=4
        i32.const 1224
        local.get 1
        i32.store
        local.get 5
        local.get 1
        i32.store
        local.get 0
        local.get 1
        i32.const 1
        i32.or
        i32.store offset=4
        return
      end
      block  ;; label = @2
        local.get 5
        i32.load offset=4
        local.tee 2
        i32.const 2
        i32.and
        i32.eqz
        if  ;; label = @3
          local.get 5
          i32.const 1240
          i32.load
          i32.eq
          if  ;; label = @4
            i32.const 1240
            local.get 0
            i32.store
            i32.const 1228
            i32.const 1228
            i32.load
            local.get 1
            i32.add
            local.tee 1
            i32.store
            local.get 0
            local.get 1
            i32.const 1
            i32.or
            i32.store offset=4
            local.get 0
            i32.const 1236
            i32.load
            i32.ne
            br_if 3 (;@1;)
            i32.const 1224
            i32.const 0
            i32.store
            i32.const 1236
            i32.const 0
            i32.store
            return
          end
          local.get 5
          i32.const 1236
          i32.load
          i32.eq
          if  ;; label = @4
            i32.const 1236
            local.get 0
            i32.store
            i32.const 1224
            i32.const 1224
            i32.load
            local.get 1
            i32.add
            local.tee 1
            i32.store
            local.get 0
            local.get 1
            i32.const 1
            i32.or
            i32.store offset=4
            local.get 0
            local.get 1
            i32.add
            local.get 1
            i32.store
            return
          end
          i32.const 1232
          i32.load
          local.set 3
          local.get 2
          i32.const -8
          i32.and
          local.get 1
          i32.add
          local.set 1
          block  ;; label = @4
            local.get 2
            i32.const 255
            i32.le_u
            if  ;; label = @5
              local.get 5
              i32.load offset=8
              local.tee 4
              local.get 2
              i32.const 3
              i32.shr_u
              local.tee 2
              i32.const 3
              i32.shl
              i32.const 1256
              i32.add
              i32.ne
              drop
              local.get 4
              local.get 5
              i32.load offset=12
              local.tee 3
              i32.eq
              if  ;; label = @6
                i32.const 1216
                i32.const 1216
                i32.load
                i32.const -2
                local.get 2
                i32.rotl
                i32.and
                i32.store
                br 2 (;@4;)
              end
              local.get 3
              local.get 4
              i32.store offset=8
              local.get 4
              local.get 3
              i32.store offset=12
              br 1 (;@4;)
            end
            local.get 5
            i32.load offset=24
            local.set 6
            block  ;; label = @5
              local.get 5
              local.get 5
              i32.load offset=12
              local.tee 2
              i32.ne
              if  ;; label = @6
                local.get 3
                local.get 5
                i32.load offset=8
                local.tee 3
                i32.le_u
                if  ;; label = @7
                  local.get 3
                  i32.load offset=12
                  drop
                end
                local.get 2
                local.get 3
                i32.store offset=8
                local.get 3
                local.get 2
                i32.store offset=12
                br 1 (;@5;)
              end
              block  ;; label = @6
                local.get 5
                i32.const 20
                i32.add
                local.tee 3
                i32.load
                local.tee 4
                br_if 0 (;@6;)
                local.get 5
                i32.const 16
                i32.add
                local.tee 3
                i32.load
                local.tee 4
                br_if 0 (;@6;)
                i32.const 0
                local.set 2
                br 1 (;@5;)
              end
              loop  ;; label = @6
                local.get 3
                local.set 7
                local.get 4
                local.tee 2
                i32.const 20
                i32.add
                local.tee 3
                i32.load
                local.tee 4
                br_if 0 (;@6;)
                local.get 2
                i32.const 16
                i32.add
                local.set 3
                local.get 2
                i32.load offset=16
                local.tee 4
                br_if 0 (;@6;)
              end
              local.get 7
              i32.const 0
              i32.store
            end
            local.get 6
            i32.eqz
            br_if 0 (;@4;)
            block  ;; label = @5
              local.get 5
              local.get 5
              i32.load offset=28
              local.tee 3
              i32.const 2
              i32.shl
              i32.const 1520
              i32.add
              local.tee 4
              i32.load
              i32.eq
              if  ;; label = @6
                local.get 4
                local.get 2
                i32.store
                local.get 2
                br_if 1 (;@5;)
                i32.const 1220
                i32.const 1220
                i32.load
                i32.const -2
                local.get 3
                i32.rotl
                i32.and
                i32.store
                br 2 (;@4;)
              end
              local.get 6
              i32.const 16
              i32.const 20
              local.get 6
              i32.load offset=16
              local.get 5
              i32.eq
              select
              i32.add
              local.get 2
              i32.store
              local.get 2
              i32.eqz
              br_if 1 (;@4;)
            end
            local.get 2
            local.get 6
            i32.store offset=24
            local.get 5
            i32.load offset=16
            local.tee 3
            if  ;; label = @5
              local.get 2
              local.get 3
              i32.store offset=16
              local.get 3
              local.get 2
              i32.store offset=24
            end
            local.get 5
            i32.load offset=20
            local.tee 3
            i32.eqz
            br_if 0 (;@4;)
            local.get 2
            i32.const 20
            i32.add
            local.get 3
            i32.store
            local.get 3
            local.get 2
            i32.store offset=24
          end
          local.get 0
          local.get 1
          i32.add
          local.get 1
          i32.store
          local.get 0
          local.get 1
          i32.const 1
          i32.or
          i32.store offset=4
          local.get 0
          i32.const 1236
          i32.load
          i32.ne
          br_if 1 (;@2;)
          i32.const 1224
          local.get 1
          i32.store
          return
        end
        local.get 5
        local.get 2
        i32.const -2
        i32.and
        i32.store offset=4
        local.get 0
        local.get 1
        i32.add
        local.get 1
        i32.store
        local.get 0
        local.get 1
        i32.const 1
        i32.or
        i32.store offset=4
      end
      local.get 1
      i32.const 255
      i32.le_u
      if  ;; label = @2
        local.get 1
        i32.const 3
        i32.shr_u
        local.tee 2
        i32.const 3
        i32.shl
        i32.const 1256
        i32.add
        local.set 1
        block (result i32)  ;; label = @3
          i32.const 1216
          i32.load
          local.tee 3
          i32.const 1
          local.get 2
          i32.shl
          local.tee 2
          i32.and
          i32.eqz
          if  ;; label = @4
            i32.const 1216
            local.get 2
            local.get 3
            i32.or
            i32.store
            local.get 1
            br 1 (;@3;)
          end
          local.get 1
          i32.load offset=8
        end
        local.tee 3
        local.get 0
        i32.store offset=12
        local.get 1
        local.get 0
        i32.store offset=8
        local.get 0
        local.get 1
        i32.store offset=12
        local.get 0
        local.get 3
        i32.store offset=8
        return
      end
      local.get 0
      i64.const 0
      i64.store offset=16 align=4
      local.get 0
      i32.const 28
      i32.add
      block (result i32)  ;; label = @2
        i32.const 0
        local.get 1
        i32.const 8
        i32.shr_u
        local.tee 2
        i32.eqz
        br_if 0 (;@2;)
        drop
        i32.const 31
        local.get 1
        i32.const 16777215
        i32.gt_u
        br_if 0 (;@2;)
        drop
        local.get 2
        local.get 2
        i32.const 1048320
        i32.add
        i32.const 16
        i32.shr_u
        i32.const 8
        i32.and
        local.tee 2
        i32.shl
        local.tee 3
        local.get 3
        i32.const 520192
        i32.add
        i32.const 16
        i32.shr_u
        i32.const 4
        i32.and
        local.tee 3
        i32.shl
        local.tee 4
        local.get 4
        i32.const 245760
        i32.add
        i32.const 16
        i32.shr_u
        i32.const 2
        i32.and
        local.tee 4
        i32.shl
        i32.const 15
        i32.shr_u
        local.get 2
        local.get 3
        i32.or
        local.get 4
        i32.or
        i32.sub
        local.tee 2
        i32.const 1
        i32.shl
        local.get 1
        local.get 2
        i32.const 21
        i32.add
        i32.shr_u
        i32.const 1
        i32.and
        i32.or
        i32.const 28
        i32.add
      end
      local.tee 3
      i32.store
      local.get 3
      i32.const 2
      i32.shl
      i32.const 1520
      i32.add
      local.set 2
      i32.const 1220
      i32.load
      local.tee 4
      i32.const 1
      local.get 3
      i32.shl
      local.tee 7
      i32.and
      i32.eqz
      if  ;; label = @2
        local.get 2
        local.get 0
        i32.store
        i32.const 1220
        local.get 4
        local.get 7
        i32.or
        i32.store
        local.get 0
        i32.const 24
        i32.add
        local.get 2
        i32.store
        local.get 0
        local.get 0
        i32.store offset=8
        local.get 0
        local.get 0
        i32.store offset=12
        return
      end
      local.get 1
      i32.const 0
      i32.const 25
      local.get 3
      i32.const 1
      i32.shr_u
      i32.sub
      local.get 3
      i32.const 31
      i32.eq
      select
      i32.shl
      local.set 3
      local.get 2
      i32.load
      local.set 2
      block  ;; label = @2
        loop  ;; label = @3
          local.get 2
          local.tee 4
          i32.load offset=4
          i32.const -8
          i32.and
          local.get 1
          i32.eq
          br_if 1 (;@2;)
          local.get 3
          i32.const 29
          i32.shr_u
          local.set 2
          local.get 3
          i32.const 1
          i32.shl
          local.set 3
          local.get 4
          local.get 2
          i32.const 4
          i32.and
          i32.add
          i32.const 16
          i32.add
          local.tee 7
          i32.load
          local.tee 2
          br_if 0 (;@3;)
        end
        local.get 7
        local.get 0
        i32.store
        local.get 0
        i32.const 24
        i32.add
        local.get 4
        i32.store
        local.get 0
        local.get 0
        i32.store offset=12
        local.get 0
        local.get 0
        i32.store offset=8
        return
      end
      local.get 4
      i32.load offset=8
      local.set 1
      local.get 4
      local.get 0
      i32.store offset=8
      local.get 1
      local.get 0
      i32.store offset=12
      local.get 0
      i32.const 24
      i32.add
      i32.const 0
      i32.store
      local.get 0
      local.get 1
      i32.store offset=8
      local.get 0
      local.get 4
      i32.store offset=12
    end)
  (func (;16;) (type 1) (param i32) (result i32)
    local.get 0
    i32.eqz
    if  ;; label = @1
      memory.size
      i32.const 16
      i32.shl
      return
    end
    local.get 0
    i32.const 65535
    i32.and
    local.get 0
    i32.const -1
    i32.le_s
    i32.or
    i32.eqz
    if  ;; label = @1
      local.get 0
      i32.const 16
      i32.shr_u
      memory.grow
      local.tee 0
      i32.const -1
      i32.eq
      if  ;; label = @2
        i32.const 1712
        i32.const 48
        i32.store
        i32.const -1
        return
      end
      local.get 0
      i32.const 16
      i32.shl
      return
    end
    unreachable)
  (func (;17;) (type 2) (param i32 i32) (result i32)
    (local i32 i32 i32 i32 i32)
    i32.const 4095
    local.set 4
    local.get 0
    local.set 3
    block  ;; label = @1
      loop  ;; label = @2
        block  ;; label = @3
          local.get 4
          i32.eqz
          br_if 0 (;@3;)
          block  ;; label = @4
            block  ;; label = @5
              local.get 1
              i32.load offset=4
              local.tee 2
              local.get 1
              i32.load offset=8
              local.tee 5
              i32.eq
              br_if 0 (;@5;)
              local.get 3
              local.get 2
              local.get 2
              local.get 5
              local.get 2
              i32.sub
              local.tee 3
              call 27
              local.tee 6
              local.get 2
              i32.sub
              i32.const 1
              i32.add
              local.get 3
              local.get 6
              select
              local.tee 3
              local.get 4
              local.get 3
              local.get 4
              i32.lt_u
              select
              local.tee 2
              call 29
              local.set 3
              local.get 1
              local.get 1
              i32.load offset=4
              local.get 2
              i32.add
              local.tee 5
              i32.store offset=4
              local.get 2
              local.get 3
              i32.add
              local.set 3
              local.get 6
              br_if 2 (;@3;)
              local.get 4
              local.get 2
              i32.sub
              local.tee 4
              i32.eqz
              br_if 2 (;@3;)
              local.get 5
              local.get 1
              i32.load offset=8
              i32.eq
              br_if 0 (;@5;)
              local.get 1
              local.get 5
              i32.const 1
              i32.add
              i32.store offset=4
              local.get 5
              i32.load8_u
              local.set 2
              br 1 (;@4;)
            end
            local.get 1
            call 26
            local.tee 2
            i32.const -1
            i32.gt_s
            br_if 0 (;@4;)
            i32.const 0
            local.set 2
            local.get 0
            local.get 3
            i32.eq
            br_if 3 (;@1;)
            local.get 1
            i32.load8_u
            i32.const 16
            i32.and
            i32.eqz
            br_if 3 (;@1;)
            br 1 (;@3;)
          end
          local.get 3
          local.get 2
          i32.store8
          local.get 3
          i32.const 1
          i32.add
          local.set 3
          local.get 4
          i32.const -1
          i32.add
          local.set 4
          local.get 2
          i32.const 255
          i32.and
          i32.const 10
          i32.ne
          br_if 1 (;@2;)
        end
      end
      local.get 0
      i32.eqz
      if  ;; label = @2
        i32.const 0
        return
      end
      local.get 3
      i32.const 0
      i32.store8
      local.get 0
      local.set 2
    end
    local.get 2)
  (func (;18;) (type 1) (param i32) (result i32)
    block (result i32)  ;; label = @1
      i32.const 0
      local.get 0
      i32.load offset=56
      call 1
      local.tee 0
      i32.eqz
      br_if 0 (;@1;)
      drop
      i32.const 1712
      local.get 0
      i32.store
      i32.const -1
    end)
  (func (;19;) (type 2) (param i32 i32) (result i32)
    (local i32 i32)
    global.get 0
    i32.const 16
    i32.sub
    local.tee 2
    global.set 0
    i32.const -1
    local.set 3
    block  ;; label = @1
      local.get 0
      local.get 1
      i32.const 2
      local.get 2
      i32.const 12
      i32.add
      call 2
      local.tee 0
      if  ;; label = @2
        i32.const 1712
        local.get 0
        i32.store
        br 1 (;@1;)
      end
      local.get 2
      i32.load offset=12
      local.set 3
    end
    local.get 2
    i32.const 16
    i32.add
    global.set 0
    local.get 3)
  (func (;20;) (type 0) (param i32 i32 i32) (result i32)
    (local i32)
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
    block (result i32)  ;; label = @1
      local.get 0
      local.get 3
      i32.const 8
      i32.add
      i32.const 1
      local.get 3
      i32.const 4
      i32.add
      call 2
      local.tee 0
      if  ;; label = @2
        i32.const 1712
        i32.const 8
        local.get 0
        local.get 0
        i32.const 76
        i32.eq
        select
        i32.store
        i32.const -1
        br 1 (;@1;)
      end
      local.get 3
      i32.load offset=4
    end
    local.get 3
    i32.const 16
    i32.add
    global.set 0)
  (func (;21;) (type 0) (param i32 i32 i32) (result i32)
    (local i32 i32 i32 i32 i32)
    global.get 0
    i32.const 16
    i32.sub
    local.tee 5
    global.set 0
    local.get 5
    local.get 1
    i32.store
    local.get 5
    local.get 0
    i32.load offset=44
    local.tee 3
    i32.store offset=12
    local.get 5
    local.get 0
    i32.load offset=40
    local.tee 6
    i32.store offset=8
    local.get 5
    local.get 2
    local.get 3
    i32.const 0
    i32.ne
    i32.sub
    local.tee 7
    i32.store offset=4
    local.get 0
    i32.load offset=56
    local.set 4
    block (result i32)  ;; label = @1
      local.get 7
      if  ;; label = @2
        local.get 4
        local.get 5
        call 19
        br 1 (;@1;)
      end
      local.get 4
      local.get 6
      local.get 3
      call 20
    end
    local.set 4
    i32.const 0
    local.set 3
    block  ;; label = @1
      local.get 4
      i32.const 0
      i32.le_s
      if  ;; label = @2
        local.get 0
        local.get 0
        i32.load
        i32.const 32
        i32.const 16
        local.get 4
        select
        i32.or
        i32.store
        br 1 (;@1;)
      end
      local.get 4
      local.get 5
      i32.load offset=4
      local.tee 6
      i32.le_u
      if  ;; label = @2
        local.get 4
        local.set 3
        br 1 (;@1;)
      end
      local.get 0
      local.get 0
      i32.load offset=40
      local.tee 3
      i32.store offset=4
      local.get 0
      local.get 3
      local.get 4
      local.get 6
      i32.sub
      i32.add
      i32.store offset=8
      local.get 0
      i32.load offset=44
      if  ;; label = @2
        local.get 0
        local.get 3
        i32.const 1
        i32.add
        i32.store offset=4
        local.get 1
        local.get 2
        i32.add
        i32.const -1
        i32.add
        local.get 3
        i32.load8_u
        i32.store8
      end
      local.get 2
      local.set 3
    end
    local.get 5
    i32.const 16
    i32.add
    global.set 0
    local.get 3)
  (func (;22;) (type 3) (param i32 i64 i32) (result i64)
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
      call 3
      local.tee 0
      if  ;; label = @2
        i32.const 1712
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
  (func (;23;) (type 3) (param i32 i64 i32) (result i64)
    local.get 0
    i32.load offset=56
    local.get 1
    local.get 2
    call 22)
  (func (;24;) (type 4)
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
          call_indirect (type 3)
          drop
        end
        local.get 0
        i32.load offset=52
        local.tee 0
        br_if 0 (;@2;)
      end
    end
    block  ;; label = @1
      i32.const 1208
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
      call_indirect (type 3)
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
      call_indirect (type 3)
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
      call_indirect (type 3)
      drop
    end)
  (func (;25;) (type 1) (param i32) (result i32)
    (local i32 i32)
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
    i32.load offset=20
    local.get 0
    i32.load offset=24
    i32.ne
    if  ;; label = @1
      local.get 0
      i32.const 0
      i32.const 0
      local.get 0
      i32.load offset=32
      call_indirect (type 0)
      drop
    end
    local.get 0
    i32.const 0
    i32.store offset=24
    local.get 0
    i64.const 0
    i64.store offset=16
    local.get 0
    i32.load
    local.tee 1
    i32.const 4
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
    local.get 0
    i32.load offset=40
    local.get 0
    i32.load offset=44
    i32.add
    local.tee 2
    i32.store offset=8
    local.get 0
    local.get 2
    i32.store offset=4
    local.get 1
    i32.const 27
    i32.shl
    i32.const 31
    i32.shr_s)
  (func (;26;) (type 1) (param i32) (result i32)
    (local i32 i32)
    global.get 0
    i32.const 16
    i32.sub
    local.tee 1
    global.set 0
    i32.const -1
    local.set 2
    block  ;; label = @1
      local.get 0
      call 25
      br_if 0 (;@1;)
      local.get 0
      local.get 1
      i32.const 15
      i32.add
      i32.const 1
      local.get 0
      i32.load offset=28
      call_indirect (type 0)
      i32.const 1
      i32.ne
      br_if 0 (;@1;)
      local.get 1
      i32.load8_u offset=15
      local.set 2
    end
    local.get 1
    i32.const 16
    i32.add
    global.set 0
    local.get 2)
  (func (;27;) (type 2) (param i32 i32) (result i32)
    (local i32 i32 i32)
    local.get 1
    i32.const 0
    i32.ne
    local.set 3
    block  ;; label = @1
      block  ;; label = @2
        block  ;; label = @3
          local.get 1
          i32.eqz
          if  ;; label = @4
            local.get 1
            local.set 2
            br 1 (;@3;)
          end
          local.get 0
          i32.const 3
          i32.and
          i32.eqz
          if  ;; label = @4
            local.get 1
            local.set 2
            br 1 (;@3;)
          end
          loop  ;; label = @4
            local.get 0
            i32.load8_u
            i32.const 10
            i32.eq
            if  ;; label = @5
              local.get 1
              local.set 2
              br 3 (;@2;)
            end
            local.get 1
            i32.const 1
            i32.ne
            local.set 3
            local.get 1
            i32.const -1
            i32.add
            local.set 2
            local.get 0
            i32.const 1
            i32.add
            local.set 0
            local.get 1
            i32.const 1
            i32.eq
            br_if 1 (;@3;)
            local.get 2
            local.set 1
            local.get 0
            i32.const 3
            i32.and
            br_if 0 (;@4;)
          end
        end
        local.get 3
        i32.eqz
        br_if 1 (;@1;)
      end
      block  ;; label = @2
        local.get 0
        i32.load8_u
        i32.const 10
        i32.eq
        local.get 2
        i32.const 4
        i32.lt_u
        i32.or
        br_if 0 (;@2;)
        local.get 2
        i32.const -4
        i32.add
        local.tee 3
        i32.const 3
        i32.and
        local.get 3
        i32.const -4
        i32.and
        local.get 0
        i32.add
        i32.const 4
        i32.add
        local.set 3
        loop  ;; label = @3
          local.get 0
          i32.load
          i32.const 168430090
          i32.xor
          local.tee 4
          i32.const -1
          i32.xor
          local.get 4
          i32.const -16843009
          i32.add
          i32.and
          i32.const -2139062144
          i32.and
          br_if 1 (;@2;)
          local.get 0
          i32.const 4
          i32.add
          local.set 0
          local.get 2
          i32.const -4
          i32.add
          local.tee 2
          i32.const 3
          i32.gt_u
          br_if 0 (;@3;)
        end
        local.set 2
        local.get 3
        local.set 0
      end
      local.get 2
      i32.eqz
      br_if 0 (;@1;)
      loop  ;; label = @2
        local.get 0
        i32.load8_u
        i32.const 10
        i32.eq
        if  ;; label = @3
          local.get 0
          return
        end
        local.get 0
        i32.const 1
        i32.add
        local.set 0
        local.get 2
        i32.const -1
        i32.add
        local.tee 2
        br_if 0 (;@2;)
      end
    end
    i32.const 0)
  (func (;28;) (type 1) (param i32) (result i32)
    (local i32 i32 i32 i32 i32)
    i32.const 6
    local.set 2
    i32.const 1024
    local.set 1
    block  ;; label = @1
      loop  ;; label = @2
        local.get 1
        i32.load8_u
        local.tee 3
        local.get 0
        i32.load8_u
        local.tee 4
        i32.eq
        if  ;; label = @3
          local.get 0
          i32.const 1
          i32.add
          local.set 0
          local.get 1
          i32.const 1
          i32.add
          local.set 1
          local.get 2
          i32.const -1
          i32.add
          local.tee 2
          br_if 1 (;@2;)
          br 2 (;@1;)
        end
      end
      local.get 3
      local.get 4
      i32.sub
      local.set 5
    end
    local.get 5)
  (func (;29;) (type 0) (param i32 i32 i32) (result i32)
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
  (func (;30;) (type 0) (param i32 i32 i32) (result i32)
    (local i32 i32 i64)
    block  ;; label = @1
      local.get 2
      i32.eqz
      br_if 0 (;@1;)
      local.get 0
      local.get 1
      i32.store8
      local.get 0
      local.get 2
      i32.add
      local.tee 3
      i32.const -1
      i32.add
      local.get 1
      i32.store8
      local.get 2
      i32.const 3
      i32.lt_u
      br_if 0 (;@1;)
      local.get 0
      local.get 1
      i32.store8 offset=2
      local.get 0
      local.get 1
      i32.store8 offset=1
      local.get 3
      i32.const -3
      i32.add
      local.get 1
      i32.store8
      local.get 3
      i32.const -2
      i32.add
      local.get 1
      i32.store8
      local.get 2
      i32.const 7
      i32.lt_u
      br_if 0 (;@1;)
      local.get 0
      local.get 1
      i32.store8 offset=3
      local.get 3
      i32.const -4
      i32.add
      local.get 1
      i32.store8
      local.get 2
      i32.const 9
      i32.lt_u
      br_if 0 (;@1;)
      local.get 0
      i32.const 0
      local.get 0
      i32.sub
      i32.const 3
      i32.and
      local.tee 4
      i32.add
      local.tee 3
      local.get 1
      i32.const 255
      i32.and
      i32.const 16843009
      i32.mul
      local.tee 1
      i32.store
      local.get 3
      local.get 2
      local.get 4
      i32.sub
      i32.const -4
      i32.and
      local.tee 4
      i32.add
      local.tee 2
      i32.const -4
      i32.add
      local.get 1
      i32.store
      local.get 4
      i32.const 9
      i32.lt_u
      br_if 0 (;@1;)
      local.get 3
      local.get 1
      i32.store offset=8
      local.get 3
      local.get 1
      i32.store offset=4
      local.get 2
      i32.const -8
      i32.add
      local.get 1
      i32.store
      local.get 2
      i32.const -12
      i32.add
      local.get 1
      i32.store
      local.get 4
      i32.const 25
      i32.lt_u
      br_if 0 (;@1;)
      local.get 3
      local.get 1
      i32.store offset=24
      local.get 3
      local.get 1
      i32.store offset=20
      local.get 3
      local.get 1
      i32.store offset=16
      local.get 3
      local.get 1
      i32.store offset=12
      local.get 2
      i32.const -16
      i32.add
      local.get 1
      i32.store
      local.get 2
      i32.const -20
      i32.add
      local.get 1
      i32.store
      local.get 2
      i32.const -24
      i32.add
      local.get 1
      i32.store
      local.get 2
      i32.const -28
      i32.add
      local.get 1
      i32.store
      local.get 4
      local.get 3
      i32.const 4
      i32.and
      i32.const 24
      i32.or
      local.tee 4
      i32.sub
      local.tee 2
      i32.const 32
      i32.lt_u
      br_if 0 (;@1;)
      local.get 1
      i64.extend_i32_u
      local.tee 5
      i64.const 32
      i64.shl
      local.get 5
      i64.or
      local.set 5
      local.get 3
      local.get 4
      i32.add
      local.set 1
      loop  ;; label = @2
        local.get 1
        local.get 5
        i64.store
        local.get 1
        i32.const 24
        i32.add
        local.get 5
        i64.store
        local.get 1
        i32.const 16
        i32.add
        local.get 5
        i64.store
        local.get 1
        i32.const 8
        i32.add
        local.get 5
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
  (func (;31;) (type 1) (param i32) (result i32)
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
  (func (;32;) (type 6) (param i32 i32)
    (local i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32)
    global.get 0
    i32.const 704
    i32.sub
    local.tee 2
    global.set 0
    local.get 2
    i64.const 1
    i64.store offset=8
    block  ;; label = @1
      local.get 1
      i32.const 4
      i32.shl
      local.tee 10
      i32.eqz
      br_if 0 (;@1;)
      local.get 2
      i32.const 16
      i32.store offset=16
      local.get 2
      i32.const 16
      i32.store offset=20
      local.get 2
      i32.const 16
      i32.add
      i32.const 8
      i32.or
      local.set 1
      i32.const 16
      local.tee 3
      local.set 4
      loop  ;; label = @2
        local.get 1
        local.get 3
        local.tee 5
        local.get 4
        i32.const 16
        i32.add
        i32.add
        local.tee 3
        i32.store
        local.get 1
        i32.const 4
        i32.add
        local.set 1
        local.get 5
        local.set 4
        local.get 3
        local.get 10
        i32.lt_u
        br_if 0 (;@2;)
      end
      block  ;; label = @2
        local.get 0
        local.get 10
        i32.add
        i32.const -16
        i32.add
        local.tee 12
        local.get 0
        i32.le_u
        if  ;; label = @3
          i32.const 1
          local.set 6
          br 1 (;@2;)
        end
        local.get 2
        i32.const 208
        i32.add
        i32.const 4
        i32.or
        local.set 10
        i32.const 1
        local.set 1
        i32.const 1
        local.set 6
        loop  ;; label = @3
          block (result i32)  ;; label = @4
            local.get 1
            i32.const 3
            i32.and
            i32.const 3
            i32.eq
            if  ;; label = @5
              local.get 2
              local.get 0
              i32.store offset=208
              block  ;; label = @6
                local.get 6
                i32.const 2
                i32.lt_s
                br_if 0 (;@6;)
                i32.const 1
                local.set 9
                local.get 10
                local.set 5
                local.get 0
                local.set 7
                local.get 0
                local.set 1
                local.get 6
                local.set 4
                block  ;; label = @7
                  loop  ;; label = @8
                    local.get 7
                    local.get 1
                    i32.const -16
                    i32.add
                    local.tee 8
                    local.get 2
                    i32.const 16
                    i32.add
                    local.get 4
                    i32.const -2
                    i32.add
                    local.tee 3
                    i32.const 2
                    i32.shl
                    i32.add
                    i32.load
                    i32.sub
                    local.tee 1
                    call 9
                    i32.const 0
                    i32.ge_s
                    if  ;; label = @9
                      local.get 7
                      local.get 8
                      call 9
                      i32.const -1
                      i32.gt_s
                      br_if 2 (;@7;)
                    end
                    block  ;; label = @9
                      local.get 1
                      local.get 8
                      call 9
                      i32.const 0
                      i32.ge_s
                      if  ;; label = @10
                        local.get 5
                        local.get 1
                        i32.store
                        local.get 4
                        i32.const -1
                        i32.add
                        local.set 3
                        br 1 (;@9;)
                      end
                      local.get 5
                      local.get 8
                      i32.store
                      local.get 8
                      local.set 1
                    end
                    local.get 3
                    i32.const 2
                    i32.ge_s
                    if  ;; label = @9
                      local.get 9
                      i32.const 1
                      i32.add
                      local.set 9
                      local.get 5
                      i32.const 4
                      i32.add
                      local.set 5
                      local.get 2
                      i32.load offset=208
                      local.set 7
                      local.get 3
                      local.set 4
                      br 1 (;@8;)
                    end
                  end
                  local.get 9
                  i32.const 1
                  i32.add
                  local.set 9
                end
                local.get 9
                i32.const 2
                i32.lt_s
                br_if 0 (;@6;)
                local.get 2
                i32.const 208
                i32.add
                local.get 9
                i32.const 2
                i32.shl
                i32.add
                local.tee 7
                local.get 2
                i32.const 448
                i32.add
                i32.store
                local.get 2
                i32.const 448
                i32.add
                local.set 1
                i32.const 16
                local.set 3
                loop  ;; label = @7
                  local.get 1
                  local.get 2
                  i32.load offset=208
                  local.tee 4
                  local.get 3
                  i32.const 256
                  local.get 3
                  i32.const 256
                  i32.lt_u
                  select
                  local.tee 8
                  call 29
                  drop
                  local.get 2
                  i32.const 208
                  i32.add
                  local.set 1
                  local.get 9
                  local.set 5
                  loop  ;; label = @8
                    local.get 1
                    local.get 4
                    local.get 1
                    i32.const 4
                    i32.add
                    local.tee 1
                    i32.load
                    local.tee 4
                    local.get 8
                    call 29
                    local.get 8
                    i32.add
                    i32.store
                    local.get 5
                    i32.const -1
                    i32.add
                    local.tee 5
                    br_if 0 (;@8;)
                  end
                  local.get 3
                  local.get 8
                  i32.sub
                  local.tee 3
                  i32.eqz
                  br_if 1 (;@6;)
                  local.get 7
                  i32.load
                  local.set 1
                  br 0 (;@7;)
                end
                unreachable
              end
              local.get 2
              local.get 2
              i32.load offset=12
              local.tee 1
              i32.const 2
              i32.shr_u
              i32.store offset=12
              local.get 2
              local.get 1
              i32.const 30
              i32.shl
              local.get 2
              i32.load offset=8
              i32.const 2
              i32.shr_u
              i32.or
              local.tee 1
              i32.store offset=8
              local.get 6
              i32.const 2
              i32.add
              br 1 (;@4;)
            end
            block  ;; label = @5
              local.get 2
              i32.const 16
              i32.add
              local.get 6
              i32.const -1
              i32.add
              local.tee 11
              i32.const 2
              i32.shl
              i32.add
              i32.load
              local.get 12
              local.get 0
              i32.sub
              i32.ge_u
              if  ;; label = @6
                local.get 0
                local.get 2
                i32.const 8
                i32.add
                local.get 6
                i32.const 0
                local.get 2
                i32.const 16
                i32.add
                call 33
                br 1 (;@5;)
              end
              local.get 2
              local.get 0
              i32.store offset=208
              local.get 6
              i32.const 2
              i32.lt_s
              br_if 0 (;@5;)
              i32.const 1
              local.set 9
              local.get 10
              local.set 5
              local.get 0
              local.set 7
              local.get 0
              local.set 1
              local.get 6
              local.set 4
              block  ;; label = @6
                loop  ;; label = @7
                  local.get 7
                  local.get 1
                  i32.const -16
                  i32.add
                  local.tee 8
                  local.get 2
                  i32.const 16
                  i32.add
                  local.get 4
                  i32.const -2
                  i32.add
                  local.tee 3
                  i32.const 2
                  i32.shl
                  i32.add
                  i32.load
                  i32.sub
                  local.tee 1
                  call 9
                  i32.const 0
                  i32.ge_s
                  if  ;; label = @8
                    local.get 7
                    local.get 8
                    call 9
                    i32.const -1
                    i32.gt_s
                    br_if 2 (;@6;)
                  end
                  block  ;; label = @8
                    local.get 1
                    local.get 8
                    call 9
                    i32.const 0
                    i32.ge_s
                    if  ;; label = @9
                      local.get 5
                      local.get 1
                      i32.store
                      local.get 4
                      i32.const -1
                      i32.add
                      local.set 3
                      br 1 (;@8;)
                    end
                    local.get 5
                    local.get 8
                    i32.store
                    local.get 8
                    local.set 1
                  end
                  local.get 3
                  i32.const 2
                  i32.ge_s
                  if  ;; label = @8
                    local.get 9
                    i32.const 1
                    i32.add
                    local.set 9
                    local.get 5
                    i32.const 4
                    i32.add
                    local.set 5
                    local.get 2
                    i32.load offset=208
                    local.set 7
                    local.get 3
                    local.set 4
                    br 1 (;@7;)
                  end
                end
                local.get 9
                i32.const 1
                i32.add
                local.set 9
              end
              local.get 9
              i32.const 2
              i32.lt_s
              br_if 0 (;@5;)
              local.get 2
              i32.const 208
              i32.add
              local.get 9
              i32.const 2
              i32.shl
              i32.add
              local.tee 7
              local.get 2
              i32.const 448
              i32.add
              i32.store
              local.get 2
              i32.const 448
              i32.add
              local.set 1
              i32.const 16
              local.set 3
              loop  ;; label = @6
                local.get 1
                local.get 2
                i32.load offset=208
                local.tee 4
                local.get 3
                i32.const 256
                local.get 3
                i32.const 256
                i32.lt_u
                select
                local.tee 8
                call 29
                drop
                local.get 2
                i32.const 208
                i32.add
                local.set 1
                local.get 9
                local.set 5
                loop  ;; label = @7
                  local.get 1
                  local.get 4
                  local.get 1
                  i32.const 4
                  i32.add
                  local.tee 1
                  i32.load
                  local.tee 4
                  local.get 8
                  call 29
                  local.get 8
                  i32.add
                  i32.store
                  local.get 5
                  i32.const -1
                  i32.add
                  local.tee 5
                  br_if 0 (;@7;)
                end
                local.get 3
                local.get 8
                i32.sub
                local.tee 3
                i32.eqz
                br_if 1 (;@5;)
                local.get 7
                i32.load
                local.set 1
                br 0 (;@6;)
              end
              unreachable
            end
            local.get 6
            i32.const 1
            i32.eq
            if  ;; label = @5
              local.get 2
              local.get 2
              i32.load offset=8
              local.tee 4
              i32.const 1
              i32.shl
              local.tee 1
              i32.store offset=8
              local.get 2
              local.get 2
              i32.load offset=12
              i32.const 1
              i32.shl
              local.get 4
              i32.const 31
              i32.shr_u
              i32.or
              i32.store offset=12
              i32.const 0
              br 1 (;@4;)
            end
            local.get 2
            block (result i32)  ;; label = @5
              local.get 11
              i32.const 31
              i32.le_u
              if  ;; label = @6
                local.get 2
                i32.load offset=12
                local.set 4
                local.get 2
                i32.load offset=8
                br 1 (;@5;)
              end
              local.get 2
              local.get 2
              i32.load offset=8
              local.tee 4
              i32.store offset=12
              local.get 2
              i32.const 0
              i32.store offset=8
              local.get 6
              i32.const -33
              i32.add
              local.set 11
              i32.const 0
            end
            local.tee 5
            local.get 11
            i32.shl
            local.tee 1
            i32.store offset=8
            local.get 2
            local.get 4
            local.get 11
            i32.shl
            local.get 5
            i32.const 32
            local.get 11
            i32.sub
            i32.shr_u
            i32.or
            i32.store offset=12
            i32.const 1
          end
          local.set 6
          local.get 2
          local.get 1
          i32.const 1
          i32.or
          local.tee 1
          i32.store offset=8
          local.get 0
          i32.const 16
          i32.add
          local.tee 0
          local.get 12
          i32.lt_u
          br_if 0 (;@3;)
        end
      end
      local.get 0
      local.get 2
      i32.const 8
      i32.add
      local.get 6
      i32.const 0
      local.get 2
      i32.const 16
      i32.add
      call 33
      local.get 0
      i32.const -16
      i32.add
      local.set 7
      loop  ;; label = @2
        block  ;; label = @3
          block  ;; label = @4
            local.get 6
            i32.const 1
            i32.ne
            local.get 2
            i32.load offset=8
            local.tee 1
            i32.const 1
            i32.ne
            i32.or
            i32.eqz
            if  ;; label = @5
              local.get 2
              i32.load offset=12
              br_if 1 (;@4;)
              br 4 (;@1;)
            end
            local.get 6
            i32.const 1
            i32.gt_s
            br_if 1 (;@3;)
          end
          block  ;; label = @4
            block  ;; label = @5
              local.get 1
              i32.const -1
              i32.add
              i32.ctz
              local.tee 4
              i32.eqz
              if  ;; label = @6
                local.get 2
                i32.load offset=12
                local.tee 5
                i32.ctz
                local.tee 0
                if  ;; label = @7
                  local.get 0
                  i32.const 32
                  i32.add
                  local.set 4
                  br 2 (;@5;)
                end
                i32.const 0
                local.set 4
                i32.const 0
                local.set 3
                br 2 (;@4;)
              end
              local.get 2
              i32.load offset=12
              local.set 5
              local.get 4
              local.tee 3
              i32.const 32
              i32.lt_u
              br_if 1 (;@4;)
            end
            local.get 5
            local.set 1
            i32.const 0
            local.set 5
            local.get 2
            i32.const 0
            i32.store offset=12
            local.get 2
            local.get 1
            i32.store offset=8
            local.get 4
            i32.const -32
            i32.add
            local.set 3
          end
          local.get 2
          local.get 5
          local.get 3
          i32.shr_u
          i32.store offset=12
          local.get 2
          local.get 5
          i32.const 32
          local.get 3
          i32.sub
          i32.shl
          local.get 1
          local.get 3
          i32.shr_u
          i32.or
          i32.store offset=8
          local.get 4
          local.get 6
          i32.add
          local.set 6
          local.get 7
          i32.const -16
          i32.add
          local.set 7
          br 1 (;@2;)
        end
        local.get 2
        local.get 1
        i32.const 30
        i32.shr_u
        local.tee 0
        local.get 2
        i32.load offset=12
        i32.const 2
        i32.shl
        i32.or
        i32.const 1
        i32.shr_u
        i32.store offset=12
        local.get 2
        local.get 1
        i32.const 1
        i32.shl
        i32.const 2147483646
        i32.and
        local.get 0
        i32.const 31
        i32.shl
        i32.or
        i32.const 3
        i32.xor
        i32.store offset=8
        local.get 6
        i32.const -1
        i32.add
        local.set 0
        local.get 7
        local.get 2
        i32.const 16
        i32.add
        local.get 6
        i32.const -2
        i32.add
        local.tee 6
        i32.const 2
        i32.shl
        i32.add
        i32.load
        i32.sub
        local.get 2
        i32.const 8
        i32.add
        local.get 0
        i32.const 1
        local.get 2
        i32.const 16
        i32.add
        call 33
        local.get 2
        local.get 2
        i32.load offset=12
        i32.const 1
        i32.shl
        local.get 2
        i32.load offset=8
        local.tee 0
        i32.const 31
        i32.shr_u
        i32.or
        i32.store offset=12
        local.get 2
        local.get 0
        i32.const 1
        i32.shl
        i32.const 1
        i32.or
        i32.store offset=8
        local.get 7
        local.get 2
        i32.const 8
        i32.add
        local.get 6
        i32.const 1
        local.get 2
        i32.const 16
        i32.add
        call 33
        local.get 7
        i32.const -16
        i32.add
        local.set 7
        br 0 (;@2;)
      end
      unreachable
    end
    local.get 2
    i32.const 704
    i32.add
    global.set 0)
  (func (;33;) (type 9) (param i32 i32 i32 i32 i32)
    (local i32 i32 i32 i32 i32 i32 i32 i32)
    i32.const 16
    local.set 11
    global.get 0
    i32.const 736
    i32.sub
    local.tee 6
    global.set 0
    local.get 6
    local.get 0
    i32.store
    local.get 1
    i32.load
    local.set 7
    block  ;; label = @1
      block  ;; label = @2
        block  ;; label = @3
          block  ;; label = @4
            block  ;; label = @5
              local.get 1
              i32.load offset=4
              local.tee 5
              br_if 0 (;@5;)
              i32.const 1
              local.set 10
              local.get 7
              i32.const 1
              i32.ne
              br_if 0 (;@5;)
              local.get 0
              local.set 8
              br 1 (;@4;)
            end
            i32.const 1
            local.set 10
            local.get 0
            local.get 4
            local.get 2
            i32.const 2
            i32.shl
            i32.add
            i32.load
            i32.sub
            local.tee 1
            local.get 0
            call 9
            i32.const 1
            i32.lt_s
            if  ;; label = @5
              local.get 0
              local.set 8
              br 1 (;@4;)
            end
            local.get 6
            i32.const 4
            i32.or
            local.set 12
            local.get 3
            i32.eqz
            local.set 9
            loop  ;; label = @5
              local.get 1
              local.set 8
              block  ;; label = @6
                local.get 9
                i32.const 1
                i32.and
                i32.eqz
                local.get 2
                i32.const 2
                i32.lt_s
                i32.or
                br_if 0 (;@6;)
                local.get 2
                i32.const 2
                i32.shl
                local.get 4
                i32.add
                i32.const -8
                i32.add
                i32.load
                local.set 3
                local.get 0
                i32.const -16
                i32.add
                local.tee 1
                local.get 8
                call 9
                i32.const -1
                i32.gt_s
                if  ;; label = @7
                  local.get 0
                  local.set 8
                  br 5 (;@2;)
                end
                local.get 1
                local.get 3
                i32.sub
                local.get 8
                call 9
                i32.const -1
                i32.le_s
                br_if 0 (;@6;)
                local.get 0
                local.set 8
                br 4 (;@2;)
              end
              local.get 12
              local.get 8
              i32.store
              block (result i32)  ;; label = @6
                block  ;; label = @7
                  local.get 7
                  i32.const -1
                  i32.add
                  i32.ctz
                  local.tee 1
                  i32.eqz
                  if  ;; label = @8
                    local.get 5
                    i32.ctz
                    local.tee 0
                    if  ;; label = @9
                      local.get 0
                      i32.const 32
                      i32.add
                      local.set 1
                      br 2 (;@7;)
                    end
                    i32.const 0
                    local.set 1
                    local.get 5
                    local.set 9
                    local.get 7
                    local.set 5
                    i32.const 0
                    br 2 (;@6;)
                  end
                  local.get 1
                  i32.const 32
                  i32.ge_u
                  br_if 0 (;@7;)
                  local.get 5
                  local.set 9
                  local.get 7
                  local.set 5
                  local.get 1
                  br 1 (;@6;)
                end
                i32.const 0
                local.set 9
                local.get 1
                i32.const -32
                i32.add
              end
              local.set 0
              local.get 9
              i32.const 32
              local.get 0
              i32.sub
              i32.shl
              local.get 5
              local.get 0
              i32.shr_u
              i32.or
              local.set 7
              local.get 1
              local.get 2
              i32.add
              local.set 2
              local.get 9
              local.get 0
              i32.shr_u
              local.tee 5
              i32.eqz
              i32.const 0
              local.get 7
              i32.const 1
              i32.eq
              select
              br_if 2 (;@3;)
              local.get 10
              i32.const 1
              i32.add
              local.set 10
              local.get 12
              i32.const 4
              i32.add
              local.set 12
              i32.const 0
              local.set 3
              i32.const 1
              local.set 9
              local.get 8
              local.tee 0
              local.get 4
              local.get 2
              i32.const 2
              i32.shl
              i32.add
              i32.load
              i32.sub
              local.tee 1
              local.get 6
              i32.load
              call 9
              i32.const 0
              i32.gt_s
              br_if 0 (;@5;)
            end
          end
          local.get 3
          i32.eqz
          br_if 1 (;@2;)
          br 2 (;@1;)
        end
        local.get 10
        i32.const 1
        i32.add
        local.set 10
      end
      block  ;; label = @2
        local.get 10
        i32.const 2
        i32.lt_u
        br_if 0 (;@2;)
        local.get 6
        local.get 10
        i32.const 2
        i32.shl
        i32.add
        local.tee 1
        local.get 6
        i32.const 480
        i32.add
        i32.store
        local.get 6
        i32.const 480
        i32.add
        local.set 0
        i32.const 16
        local.set 3
        loop  ;; label = @3
          local.get 0
          local.get 6
          i32.load
          local.tee 5
          local.get 3
          i32.const 256
          local.get 3
          i32.const 256
          i32.lt_u
          select
          local.tee 9
          call 29
          drop
          local.get 6
          local.set 0
          local.get 10
          local.set 7
          loop  ;; label = @4
            local.get 0
            local.get 5
            local.get 0
            i32.const 4
            i32.add
            local.tee 0
            i32.load
            local.tee 5
            local.get 9
            call 29
            local.get 9
            i32.add
            i32.store
            local.get 7
            i32.const -1
            i32.add
            local.tee 7
            br_if 0 (;@4;)
          end
          local.get 3
          local.get 9
          i32.sub
          local.tee 3
          i32.eqz
          br_if 1 (;@2;)
          local.get 1
          i32.load
          local.set 0
          br 0 (;@3;)
        end
        unreachable
      end
      local.get 6
      local.get 8
      i32.store offset=240
      local.get 2
      i32.const 1
      i32.le_s
      br_if 0 (;@1;)
      local.get 6
      i32.const 240
      i32.add
      i32.const 4
      i32.or
      local.set 7
      i32.const 1
      local.set 3
      local.get 8
      local.set 0
      block  ;; label = @2
        loop  ;; label = @3
          local.get 8
          local.get 0
          i32.const -16
          i32.add
          local.tee 5
          local.get 4
          local.get 2
          i32.const -2
          i32.add
          local.tee 1
          i32.const 2
          i32.shl
          i32.add
          i32.load
          i32.sub
          local.tee 0
          call 9
          i32.const 0
          i32.ge_s
          if  ;; label = @4
            local.get 8
            local.get 5
            call 9
            i32.const -1
            i32.gt_s
            br_if 2 (;@2;)
          end
          block  ;; label = @4
            local.get 0
            local.get 5
            call 9
            i32.const 0
            i32.ge_s
            if  ;; label = @5
              local.get 7
              local.get 0
              i32.store
              local.get 2
              i32.const -1
              i32.add
              local.set 1
              br 1 (;@4;)
            end
            local.get 7
            local.get 5
            i32.store
            local.get 5
            local.set 0
          end
          local.get 1
          i32.const 2
          i32.ge_s
          if  ;; label = @4
            local.get 3
            i32.const 1
            i32.add
            local.set 3
            local.get 7
            i32.const 4
            i32.add
            local.set 7
            local.get 6
            i32.load offset=240
            local.set 8
            local.get 1
            local.set 2
            br 1 (;@3;)
          end
        end
        local.get 3
        i32.const 1
        i32.add
        local.set 3
      end
      local.get 3
      i32.const 2
      i32.lt_s
      br_if 0 (;@1;)
      local.get 6
      i32.const 240
      i32.add
      local.get 3
      i32.const 2
      i32.shl
      i32.add
      local.tee 1
      local.get 6
      i32.const 480
      i32.add
      i32.store
      local.get 6
      i32.const 480
      i32.add
      local.set 0
      loop  ;; label = @2
        local.get 0
        local.get 6
        i32.load offset=240
        local.tee 5
        local.get 11
        i32.const 256
        local.get 11
        i32.const 256
        i32.lt_u
        select
        local.tee 2
        call 29
        drop
        local.get 6
        i32.const 240
        i32.add
        local.set 0
        local.get 3
        local.set 7
        loop  ;; label = @3
          local.get 0
          local.get 5
          local.get 0
          i32.const 4
          i32.add
          local.tee 0
          i32.load
          local.tee 5
          local.get 2
          call 29
          local.get 2
          i32.add
          i32.store
          local.get 7
          i32.const -1
          i32.add
          local.tee 7
          br_if 0 (;@3;)
        end
        local.get 11
        local.get 2
        i32.sub
        local.tee 11
        i32.eqz
        br_if 1 (;@1;)
        local.get 1
        i32.load
        local.set 0
        br 0 (;@2;)
      end
      unreachable
    end
    local.get 6
    i32.const 736
    i32.add
    global.set 0)
  (table (;0;) 5 5 funcref)
  (memory (;0;) 2)
  (global (;0;) (mut i32) (i32.const 68304))
  (export "memory" (memory 0))
  (export "_start" (func 4))
  (elem (;0;) (i32.const 1) 9 18 21 23)
  (data (;0;) (i32.const 1024) ">THREE\00 \00 \01\03  \02\00GGTATTTTAATTTATAGT\00GGTATTTTAATT\00GGTATT\00GGTA\00GGT\00H\04")
  (data (;1;) (i32.const 1096) "\09")
  (data (;2;) (i32.const 1108) "\02")
  (data (;3;) (i32.const 1124) "\03\00\00\00\00\00\00\00\04\00\00\00\c8\06\00\00\00\04")
  (data (;4;) (i32.const 1208) "H\04"))
