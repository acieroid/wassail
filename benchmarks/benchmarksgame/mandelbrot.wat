(module
  (type (;0;) (func))
  (type (;1;) (func (param i32)))
  (type (;2;) (func (param i32 i32)))
  (type (;3;) (func (result i32)))
  (import "wasi_snapshot_preview1" "proc_exit" (func (;0;) (type 1)))
  (func (;1;) (type 0)
    (local i32)
    call 2
    local.tee 0
    if  ;; label = @1
      local.get 0
      call 0
      unreachable
    end)
  (func (;2;) (type 3) (result i32)
    (local i32 i32 i32 i32 i32 i32 i32 i32 i32 f64 f64 f64 f64 f64)
    global.get 0
    i32.const 8320
    i32.sub
    local.tee 1
    global.set 0
    loop  ;; label = @1
      local.get 1
      i32.const 128
      i32.add
      local.get 0
      i32.add
      local.get 9
      f64.const 0x1p-8 (;=0.00390625;)
      f64.mul
      local.tee 10
      f64.const -0x1p+0 (;=-1;)
      f64.add
      f64.store
      local.get 1
      i32.const 4224
      i32.add
      local.get 0
      i32.add
      local.get 10
      f64.const -0x1.8p+0 (;=-1.5;)
      f64.add
      f64.store
      local.get 9
      f64.const 0x1p+0 (;=1;)
      f64.add
      local.set 9
      local.get 0
      i32.const 8
      i32.add
      local.tee 0
      i32.const 4096
      i32.ne
      br_if 0 (;@1;)
    end
    loop  ;; label = @1
      local.get 1
      i32.const 128
      i32.add
      local.get 2
      i32.const 3
      i32.shl
      i32.add
      f64.load
      local.set 10
      local.get 1
      i32.const 4224
      i32.add
      local.set 3
      i32.const 0
      local.set 4
      loop  ;; label = @2
        local.get 1
        i32.const -64
        i32.sub
        local.get 1
        i32.const 4224
        i32.add
        local.get 4
        i32.const 6
        i32.shl
        i32.add
        call 3
        i32.const 0
        local.set 0
        loop  ;; label = @3
          local.get 0
          local.get 1
          i32.add
          local.get 10
          f64.store
          local.get 0
          i32.const 8
          i32.add
          local.tee 0
          i32.const 64
          i32.ne
          br_if 0 (;@3;)
        end
        i32.const 50
        local.set 8
        i32.const 255
        local.set 5
        loop  ;; label = @3
          i32.const 128
          local.set 6
          i32.const 0
          local.set 0
          loop  ;; label = @4
            local.get 1
            i32.const -64
            i32.sub
            local.get 0
            i32.add
            local.tee 7
            local.get 0
            local.get 3
            i32.add
            f64.load
            local.get 7
            f64.load
            local.tee 9
            local.get 9
            f64.mul
            local.tee 12
            local.get 0
            local.get 1
            i32.add
            local.tee 7
            f64.load
            local.tee 11
            local.get 11
            f64.mul
            local.tee 13
            f64.sub
            f64.add
            f64.store
            local.get 7
            local.get 10
            local.get 11
            local.get 9
            local.get 9
            f64.add
            f64.mul
            f64.add
            f64.store
            local.get 6
            i32.const -1
            i32.xor
            i32.const -1
            local.get 12
            local.get 13
            f64.add
            f64.const 0x1p+2 (;=4;)
            f64.gt
            select
            local.get 5
            i32.and
            local.set 5
            local.get 6
            i32.const 254
            i32.and
            i32.const 1
            i32.shr_u
            local.set 6
            local.get 0
            i32.const 8
            i32.add
            local.tee 0
            i32.const 64
            i32.ne
            br_if 0 (;@4;)
          end
          local.get 5
          i32.const 255
          i32.and
          if  ;; label = @4
            local.get 8
            i32.const -1
            i32.add
            local.tee 8
            br_if 1 (;@3;)
          end
        end
        local.get 3
        i32.const -64
        i32.sub
        local.set 3
        local.get 4
        i32.const 1
        i32.add
        local.tee 4
        i32.const 64
        i32.ne
        br_if 0 (;@2;)
      end
      local.get 2
      i32.const 1
      i32.add
      local.tee 2
      i32.const 512
      i32.ne
      br_if 0 (;@1;)
    end
    local.get 1
    i32.const 8320
    i32.add
    global.set 0
    i32.const 0)
  (func (;3;) (type 2) (param i32 i32)
    (local i32 i32 i32 i32 i32 i32 i32 i32)
    i32.const 64
    local.set 2
    block  ;; label = @1
      local.get 1
      i32.const 3
      i32.and
      if  ;; label = @2
        loop  ;; label = @3
          local.get 0
          local.get 1
          i32.load8_u
          i32.store8
          local.get 2
          i32.const -1
          i32.add
          local.set 3
          local.get 0
          i32.const 1
          i32.add
          local.set 0
          local.get 1
          i32.const 1
          i32.add
          local.set 1
          local.get 2
          i32.const 1
          i32.eq
          br_if 2 (;@1;)
          local.get 3
          local.set 2
          local.get 1
          i32.const 3
          i32.and
          br_if 0 (;@3;)
        end
        br 1 (;@1;)
      end
      i32.const 64
      local.set 3
    end
    block  ;; label = @1
      local.get 0
      i32.const 3
      i32.and
      local.tee 2
      i32.eqz
      if  ;; label = @2
        block  ;; label = @3
          local.get 3
          i32.const 16
          i32.lt_u
          if  ;; label = @4
            local.get 3
            local.set 2
            br 1 (;@3;)
          end
          local.get 3
          i32.const -16
          i32.add
          local.set 2
          loop  ;; label = @4
            local.get 0
            local.get 1
            i32.load
            i32.store
            local.get 0
            i32.const 4
            i32.add
            local.get 1
            i32.const 4
            i32.add
            i32.load
            i32.store
            local.get 0
            i32.const 8
            i32.add
            local.get 1
            i32.const 8
            i32.add
            i32.load
            i32.store
            local.get 0
            i32.const 12
            i32.add
            local.get 1
            i32.const 12
            i32.add
            i32.load
            i32.store
            local.get 0
            i32.const 16
            i32.add
            local.set 0
            local.get 1
            i32.const 16
            i32.add
            local.set 1
            local.get 3
            i32.const -16
            i32.add
            local.tee 3
            i32.const 15
            i32.gt_u
            br_if 0 (;@4;)
          end
        end
        local.get 2
        i32.const 8
        i32.and
        if  ;; label = @3
          local.get 0
          local.get 1
          i64.load align=4
          i64.store align=4
          local.get 1
          i32.const 8
          i32.add
          local.set 1
          local.get 0
          i32.const 8
          i32.add
          local.set 0
        end
        local.get 2
        i32.const 4
        i32.and
        if  ;; label = @3
          local.get 0
          local.get 1
          i32.load
          i32.store
          local.get 1
          i32.const 4
          i32.add
          local.set 1
          local.get 0
          i32.const 4
          i32.add
          local.set 0
        end
        local.get 2
        i32.const 2
        i32.and
        if  ;; label = @3
          local.get 0
          local.get 1
          i32.load8_u
          i32.store8
          local.get 0
          local.get 1
          i32.load8_u offset=1
          i32.store8 offset=1
          local.get 1
          i32.const 2
          i32.add
          local.set 1
          local.get 0
          i32.const 2
          i32.add
          local.set 0
        end
        local.get 2
        i32.const 1
        i32.and
        i32.eqz
        br_if 1 (;@1;)
        local.get 0
        local.get 1
        i32.load8_u
        i32.store8
        return
      end
      block  ;; label = @2
        local.get 3
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
            local.get 0
            local.get 1
            i32.load8_u offset=1
            i32.store8 offset=1
            local.get 0
            local.get 1
            i32.load
            local.tee 4
            i32.store8
            local.get 0
            local.get 1
            i32.load8_u offset=2
            i32.store8 offset=2
            local.get 3
            i32.const -3
            i32.add
            local.set 7
            local.get 0
            i32.const 3
            i32.add
            local.set 8
            local.get 3
            i32.const -20
            i32.add
            i32.const -16
            i32.and
            local.set 9
            i32.const 0
            local.set 2
            loop  ;; label = @5
              local.get 2
              local.get 8
              i32.add
              local.tee 0
              local.get 1
              local.get 2
              i32.add
              local.tee 5
              i32.const 4
              i32.add
              i32.load
              local.tee 6
              i32.const 8
              i32.shl
              local.get 4
              i32.const 24
              i32.shr_u
              i32.or
              i32.store
              local.get 0
              i32.const 4
              i32.add
              local.get 5
              i32.const 8
              i32.add
              i32.load
              local.tee 4
              i32.const 8
              i32.shl
              local.get 6
              i32.const 24
              i32.shr_u
              i32.or
              i32.store
              local.get 0
              i32.const 8
              i32.add
              local.get 5
              i32.const 12
              i32.add
              i32.load
              local.tee 6
              i32.const 8
              i32.shl
              local.get 4
              i32.const 24
              i32.shr_u
              i32.or
              i32.store
              local.get 0
              i32.const 12
              i32.add
              local.get 5
              i32.const 16
              i32.add
              i32.load
              local.tee 4
              i32.const 8
              i32.shl
              local.get 6
              i32.const 24
              i32.shr_u
              i32.or
              i32.store
              local.get 2
              i32.const 16
              i32.add
              local.set 2
              local.get 7
              i32.const -16
              i32.add
              local.tee 7
              i32.const 16
              i32.gt_u
              br_if 0 (;@5;)
            end
            local.get 2
            local.get 8
            i32.add
            local.set 0
            local.get 1
            local.get 2
            i32.add
            i32.const 3
            i32.add
            local.set 1
            local.get 3
            local.get 9
            i32.sub
            i32.const -19
            i32.add
            local.set 3
            br 2 (;@2;)
          end
          local.get 0
          local.get 1
          i32.load
          local.tee 4
          i32.store8
          local.get 0
          local.get 1
          i32.load8_u offset=1
          i32.store8 offset=1
          local.get 3
          i32.const -2
          i32.add
          local.set 7
          local.get 0
          i32.const 2
          i32.add
          local.set 8
          local.get 3
          i32.const -20
          i32.add
          i32.const -16
          i32.and
          local.set 9
          i32.const 0
          local.set 2
          loop  ;; label = @4
            local.get 2
            local.get 8
            i32.add
            local.tee 0
            local.get 1
            local.get 2
            i32.add
            local.tee 5
            i32.const 4
            i32.add
            i32.load
            local.tee 6
            i32.const 16
            i32.shl
            local.get 4
            i32.const 16
            i32.shr_u
            i32.or
            i32.store
            local.get 0
            i32.const 4
            i32.add
            local.get 5
            i32.const 8
            i32.add
            i32.load
            local.tee 4
            i32.const 16
            i32.shl
            local.get 6
            i32.const 16
            i32.shr_u
            i32.or
            i32.store
            local.get 0
            i32.const 8
            i32.add
            local.get 5
            i32.const 12
            i32.add
            i32.load
            local.tee 6
            i32.const 16
            i32.shl
            local.get 4
            i32.const 16
            i32.shr_u
            i32.or
            i32.store
            local.get 0
            i32.const 12
            i32.add
            local.get 5
            i32.const 16
            i32.add
            i32.load
            local.tee 4
            i32.const 16
            i32.shl
            local.get 6
            i32.const 16
            i32.shr_u
            i32.or
            i32.store
            local.get 2
            i32.const 16
            i32.add
            local.set 2
            local.get 7
            i32.const -16
            i32.add
            local.tee 7
            i32.const 17
            i32.gt_u
            br_if 0 (;@4;)
          end
          local.get 2
          local.get 8
          i32.add
          local.set 0
          local.get 1
          local.get 2
          i32.add
          i32.const 2
          i32.add
          local.set 1
          local.get 3
          local.get 9
          i32.sub
          i32.const -18
          i32.add
          local.set 3
          br 1 (;@2;)
        end
        local.get 0
        local.get 1
        i32.load
        local.tee 4
        i32.store8
        local.get 3
        i32.const -1
        i32.add
        local.set 7
        local.get 0
        i32.const 1
        i32.add
        local.set 8
        local.get 3
        i32.const -20
        i32.add
        i32.const -16
        i32.and
        local.set 9
        i32.const 0
        local.set 2
        loop  ;; label = @3
          local.get 2
          local.get 8
          i32.add
          local.tee 0
          local.get 1
          local.get 2
          i32.add
          local.tee 5
          i32.const 4
          i32.add
          i32.load
          local.tee 6
          i32.const 24
          i32.shl
          local.get 4
          i32.const 8
          i32.shr_u
          i32.or
          i32.store
          local.get 0
          i32.const 4
          i32.add
          local.get 5
          i32.const 8
          i32.add
          i32.load
          local.tee 4
          i32.const 24
          i32.shl
          local.get 6
          i32.const 8
          i32.shr_u
          i32.or
          i32.store
          local.get 0
          i32.const 8
          i32.add
          local.get 5
          i32.const 12
          i32.add
          i32.load
          local.tee 6
          i32.const 24
          i32.shl
          local.get 4
          i32.const 8
          i32.shr_u
          i32.or
          i32.store
          local.get 0
          i32.const 12
          i32.add
          local.get 5
          i32.const 16
          i32.add
          i32.load
          local.tee 4
          i32.const 24
          i32.shl
          local.get 6
          i32.const 8
          i32.shr_u
          i32.or
          i32.store
          local.get 2
          i32.const 16
          i32.add
          local.set 2
          local.get 7
          i32.const -16
          i32.add
          local.tee 7
          i32.const 18
          i32.gt_u
          br_if 0 (;@3;)
        end
        local.get 2
        local.get 8
        i32.add
        local.set 0
        local.get 1
        local.get 2
        i32.add
        i32.const 1
        i32.add
        local.set 1
        local.get 3
        local.get 9
        i32.sub
        i32.const -17
        i32.add
        local.set 3
      end
      local.get 3
      i32.const 16
      i32.and
      if  ;; label = @2
        local.get 0
        local.get 1
        i32.load16_u align=1
        i32.store16 align=1
        local.get 0
        local.get 1
        i32.load8_u offset=2
        i32.store8 offset=2
        local.get 0
        local.get 1
        i32.load8_u offset=3
        i32.store8 offset=3
        local.get 0
        local.get 1
        i32.load8_u offset=4
        i32.store8 offset=4
        local.get 0
        local.get 1
        i32.load8_u offset=5
        i32.store8 offset=5
        local.get 0
        local.get 1
        i32.load8_u offset=6
        i32.store8 offset=6
        local.get 0
        local.get 1
        i32.load8_u offset=7
        i32.store8 offset=7
        local.get 0
        local.get 1
        i32.load8_u offset=8
        i32.store8 offset=8
        local.get 0
        local.get 1
        i32.load8_u offset=9
        i32.store8 offset=9
        local.get 0
        local.get 1
        i32.load8_u offset=10
        i32.store8 offset=10
        local.get 0
        local.get 1
        i32.load8_u offset=11
        i32.store8 offset=11
        local.get 0
        local.get 1
        i32.load8_u offset=12
        i32.store8 offset=12
        local.get 0
        local.get 1
        i32.load8_u offset=13
        i32.store8 offset=13
        local.get 0
        local.get 1
        i32.load8_u offset=14
        i32.store8 offset=14
        local.get 0
        local.get 1
        i32.load8_u offset=15
        i32.store8 offset=15
        local.get 1
        i32.const 16
        i32.add
        local.set 1
        local.get 0
        i32.const 16
        i32.add
        local.set 0
      end
      local.get 3
      i32.const 8
      i32.and
      if  ;; label = @2
        local.get 0
        local.get 1
        i32.load8_u
        i32.store8
        local.get 0
        local.get 1
        i32.load8_u offset=1
        i32.store8 offset=1
        local.get 0
        local.get 1
        i32.load8_u offset=2
        i32.store8 offset=2
        local.get 0
        local.get 1
        i32.load8_u offset=3
        i32.store8 offset=3
        local.get 0
        local.get 1
        i32.load8_u offset=4
        i32.store8 offset=4
        local.get 0
        local.get 1
        i32.load8_u offset=5
        i32.store8 offset=5
        local.get 0
        local.get 1
        i32.load8_u offset=6
        i32.store8 offset=6
        local.get 0
        local.get 1
        i32.load8_u offset=7
        i32.store8 offset=7
        local.get 1
        i32.const 8
        i32.add
        local.set 1
        local.get 0
        i32.const 8
        i32.add
        local.set 0
      end
      local.get 3
      i32.const 4
      i32.and
      if  ;; label = @2
        local.get 0
        local.get 1
        i32.load8_u
        i32.store8
        local.get 0
        local.get 1
        i32.load8_u offset=1
        i32.store8 offset=1
        local.get 0
        local.get 1
        i32.load8_u offset=2
        i32.store8 offset=2
        local.get 0
        local.get 1
        i32.load8_u offset=3
        i32.store8 offset=3
        local.get 1
        i32.const 4
        i32.add
        local.set 1
        local.get 0
        i32.const 4
        i32.add
        local.set 0
      end
      local.get 3
      i32.const 2
      i32.and
      if  ;; label = @2
        local.get 0
        local.get 1
        i32.load8_u
        i32.store8
        local.get 0
        local.get 1
        i32.load8_u offset=1
        i32.store8 offset=1
        local.get 1
        i32.const 2
        i32.add
        local.set 1
        local.get 0
        i32.const 2
        i32.add
        local.set 0
      end
      local.get 3
      i32.const 1
      i32.and
      i32.eqz
      br_if 0 (;@1;)
      local.get 0
      local.get 1
      i32.load8_u
      i32.store8
    end)
  (memory (;0;) 2)
  (global (;0;) (mut i32) (i32.const 66560))
  (export "memory" (memory 0))
  (export "_start" (func 1)))
