(module
  (type (;0;) (func (param i32 i32 i32) (result i32)))
  (type (;1;) (func (param i32) (result i32)))
  (type (;2;) (func (param i32 i32 i32)))
  (type (;3;) (func (param i32 i64 i32) (result i64)))
  (type (;4;) (func (param i32 i32) (result i32)))
  (type (;5;) (func (param i32)))
  (type (;6;) (func))
  (type (;7;) (func (result i32)))
  (type (;8;) (func (param i32 i32 i32 i32) (result i32)))
  (type (;9;) (func (param i32 i32)))
  (type (;10;) (func (param i32 i32 i32 i32 i32) (result i32)))
  (type (;11;) (func (param i32 i64 i32 i32) (result i32)))
  (type (;12;) (func (param i64) (result i32)))
  (type (;13;) (func (param f64 i32) (result f64)))
  (import "wasi_snapshot_preview1" "proc_exit" (func (;0;) (type 5))) ;; 0
  (import "wasi_snapshot_preview1" "fd_close" (func (;1;) (type 1))) ;; 1
  (import "wasi_snapshot_preview1" "fd_seek" (func (;2;) (type 11))) ;; 2
  (import "wasi_snapshot_preview1" "fd_write" (func (;3;) (type 8))) ;; 3
  (import "wasi_snapshot_preview1" "fd_fdstat_get" (func (;4;) (type 4))) ;; 4
  (func (;5;) (type 6)
    (local i32)
    call 7
    local.set 0
    call 26
    local.get 0
    if  ;; label = @1
      local.get 0
      call 0
      unreachable
    end)
  (func (;6;) (type 12) (param i64) (result i32)
    (local i32 i32)
    global.get 0
    i32.const 16
    i32.sub
    local.tee 1
    global.set 0
    local.get 1
    i32.const 0
    i32.store offset=12
    block  ;; label = @1
      local.get 1
      i32.const 12
      i32.add
      local.get 0
      i32.wrap_i64
      i32.const 3
      i32.shl
      call 11
      i32.eqz
      if  ;; label = @2
        local.get 1
        i32.load offset=12
        local.tee 2
        br_if 1 (;@1;)
      end
      i32.const 1024
      i32.const 50
      i32.const 1
      i32.const 1164
      i32.load
      call 17
      drop
      call 26
      i32.const 1
      call 0
      unreachable
    end
    local.get 1
    i32.const 16
    i32.add
    global.set 0
    local.get 2)
  (func (;7;) (type 7) (result i32)
    (local i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 f64)
    global.get 0
    i32.const 48
    i32.sub
    local.tee 7
    global.set 0
    i64.const 34200
    call 6
    local.set 10
    i64.const 37800
    call 6
    local.set 11
    i64.const 39900
    call 6
    local.set 12
    i64.const 41800
    call 6
    local.set 13
    i64.const 39600
    call 6
    local.set 9
    local.get 11
    local.set 2
    loop  ;; label = @1
      i32.const 0
      local.set 1
      i32.const 1
      local.set 0
      loop  ;; label = @2
        local.get 1
        local.get 2
        i32.add
        local.get 0
        local.get 0
        i32.const 180
        i32.div_u
        i32.const 180
        i32.mul
        i32.sub
        f64.convert_i32_s
        f64.const 0x1.68p+7 (;=180;)
        f64.div
        f64.store
        local.get 0
        local.get 3
        i32.add
        local.set 0
        local.get 1
        i32.const 8
        i32.add
        local.tee 1
        i32.const 1680
        i32.ne
        br_if 0 (;@2;)
      end
      local.get 2
      i32.const 1680
      i32.add
      local.set 2
      local.get 3
      i32.const 1
      i32.add
      local.tee 3
      i32.const 180
      i32.ne
      br_if 0 (;@1;)
    end
    i32.const 0
    local.set 3
    local.get 12
    local.set 2
    loop  ;; label = @1
      local.get 3
      local.set 0
      i32.const 0
      local.set 1
      loop  ;; label = @2
        local.get 1
        local.get 2
        i32.add
        local.get 0
        local.get 0
        i32.const 190
        i32.div_u
        i32.const 190
        i32.mul
        i32.sub
        f64.convert_i32_s
        f64.const 0x1.7cp+7 (;=190;)
        f64.div
        f64.store
        local.get 0
        local.get 3
        i32.add
        local.set 0
        local.get 1
        i32.const 8
        i32.add
        local.tee 1
        i32.const 1520
        i32.ne
        br_if 0 (;@2;)
      end
      local.get 2
      i32.const 1520
      i32.add
      local.set 2
      local.get 3
      i32.const 1
      i32.add
      local.tee 3
      i32.const 210
      i32.ne
      br_if 0 (;@1;)
    end
    i32.const 0
    local.set 3
    i32.const 1
    local.set 4
    local.get 13
    local.set 2
    loop  ;; label = @1
      local.get 4
      local.set 0
      i32.const 0
      local.set 1
      loop  ;; label = @2
        local.get 1
        local.get 2
        i32.add
        local.get 0
        local.get 0
        i32.const 220
        i32.div_u
        i32.const 220
        i32.mul
        i32.sub
        f64.convert_i32_s
        f64.const 0x1.b8p+7 (;=220;)
        f64.div
        f64.store
        local.get 0
        local.get 3
        i32.add
        local.set 0
        local.get 1
        i32.const 8
        i32.add
        local.tee 1
        i32.const 1760
        i32.ne
        br_if 0 (;@2;)
      end
      local.get 4
      i32.const 3
      i32.add
      local.set 4
      local.get 2
      i32.const 1760
      i32.add
      local.set 2
      local.get 3
      i32.const 1
      i32.add
      local.tee 3
      i32.const 190
      i32.ne
      br_if 0 (;@1;)
    end
    i32.const 0
    local.set 4
    local.get 9
    local.set 3
    i32.const 0
    local.set 2
    loop  ;; label = @1
      local.get 4
      local.set 0
      i32.const 0
      local.set 1
      loop  ;; label = @2
        local.get 1
        local.get 3
        i32.add
        local.get 0
        local.get 0
        i32.const 210
        i32.div_u
        i32.const 210
        i32.mul
        i32.sub
        f64.convert_i32_s
        f64.const 0x1.a4p+7 (;=210;)
        f64.div
        f64.store
        local.get 0
        local.get 2
        i32.add
        local.set 0
        local.get 1
        i32.const 8
        i32.add
        local.tee 1
        i32.const 1760
        i32.ne
        br_if 0 (;@2;)
      end
      local.get 4
      i32.const 2
      i32.add
      local.set 4
      local.get 3
      i32.const 1760
      i32.add
      local.set 3
      local.get 2
      i32.const 1
      i32.add
      local.tee 2
      i32.const 180
      i32.ne
      br_if 0 (;@1;)
    end
    local.get 11
    local.set 4
    loop  ;; label = @1
      local.get 12
      local.set 3
      i32.const 0
      local.set 2
      loop  ;; label = @2
        local.get 10
        local.get 5
        i32.const 1520
        i32.mul
        i32.add
        local.get 2
        i32.const 3
        i32.shl
        i32.add
        local.tee 6
        i64.const 0
        i64.store
        f64.const 0x0p+0 (;=0;)
        local.set 14
        i32.const 0
        local.set 0
        local.get 3
        local.set 1
        loop  ;; label = @3
          local.get 6
          local.get 14
          local.get 0
          local.get 4
          i32.add
          f64.load
          f64.const 0x1.8p+0 (;=1.5;)
          f64.mul
          local.get 1
          f64.load
          f64.mul
          f64.add
          local.tee 14
          f64.store
          local.get 1
          i32.const 1520
          i32.add
          local.set 1
          local.get 0
          i32.const 8
          i32.add
          local.tee 0
          i32.const 1680
          i32.ne
          br_if 0 (;@3;)
        end
        local.get 3
        i32.const 8
        i32.add
        local.set 3
        local.get 2
        i32.const 1
        i32.add
        local.tee 2
        i32.const 190
        i32.ne
        br_if 0 (;@2;)
      end
      local.get 4
      i32.const 1680
      i32.add
      local.set 4
      local.get 5
      i32.const 1
      i32.add
      local.tee 5
      i32.const 180
      i32.ne
      br_if 0 (;@1;)
    end
    i32.const 0
    local.set 5
    local.get 10
    local.set 6
    loop  ;; label = @1
      local.get 13
      local.set 2
      i32.const 0
      local.set 4
      loop  ;; label = @2
        local.get 9
        local.get 5
        i32.const 1760
        i32.mul
        i32.add
        local.get 4
        i32.const 3
        i32.shl
        i32.add
        local.tee 3
        local.get 3
        f64.load
        f64.const 0x1.3333333333333p+0 (;=1.2;)
        f64.mul
        local.tee 14
        f64.store
        i32.const 0
        local.set 0
        local.get 2
        local.set 1
        loop  ;; label = @3
          local.get 3
          local.get 14
          local.get 0
          local.get 6
          i32.add
          f64.load
          local.get 1
          f64.load
          f64.mul
          f64.add
          local.tee 14
          f64.store
          local.get 1
          i32.const 1760
          i32.add
          local.set 1
          local.get 0
          i32.const 8
          i32.add
          local.tee 0
          i32.const 1520
          i32.ne
          br_if 0 (;@3;)
        end
        local.get 2
        i32.const 8
        i32.add
        local.set 2
        local.get 4
        i32.const 1
        i32.add
        local.tee 4
        i32.const 220
        i32.ne
        br_if 0 (;@2;)
      end
      local.get 6
      i32.const 1520
      i32.add
      local.set 6
      local.get 5
      i32.const 1
      i32.add
      local.tee 5
      i32.const 180
      i32.ne
      br_if 0 (;@1;)
    end
    i32.const 0
    local.set 6
    i32.const 1075
    i32.const 22
    i32.const 1
    i32.const 1164
    i32.load
    local.tee 8
    call 17
    drop
    local.get 7
    i32.const 1113
    i32.store offset=32
    local.get 8
    i32.const 1098
    local.get 7
    i32.const 32
    i32.add
    call 14
    local.get 9
    local.set 4
    i32.const 0
    local.set 5
    i32.const 0
    local.set 2
    loop  ;; label = @1
      i32.const 0
      local.set 0
      local.get 6
      local.set 1
      local.get 5
      local.set 3
      loop  ;; label = @2
        local.get 3
        local.get 1
        i32.const 20
        i32.div_u
        i32.const 20
        i32.mul
        i32.add
        i32.eqz
        if  ;; label = @3
          local.get 8
          call 15
        end
        local.get 7
        local.get 0
        local.get 4
        i32.add
        f64.load
        f64.store offset=16
        local.get 8
        i32.const 1115
        local.get 7
        i32.const 16
        i32.add
        call 14
        local.get 1
        i32.const 1
        i32.add
        local.set 1
        local.get 3
        i32.const -1
        i32.add
        local.set 3
        local.get 0
        i32.const 8
        i32.add
        local.tee 0
        i32.const 1760
        i32.ne
        br_if 0 (;@2;)
      end
      local.get 4
      i32.const 1760
      i32.add
      local.set 4
      local.get 6
      i32.const 180
      i32.add
      local.set 6
      local.get 5
      i32.const -180
      i32.add
      local.set 5
      local.get 2
      i32.const 1
      i32.add
      local.tee 2
      i32.const 180
      i32.ne
      br_if 0 (;@1;)
    end
    local.get 7
    i32.const 1113
    i32.store
    local.get 8
    i32.const 1123
    local.get 7
    call 14
    i32.const 1140
    i32.const 22
    i32.const 1
    local.get 8
    call 17
    drop
    local.get 10
    call 9
    local.get 11
    call 9
    local.get 12
    call 9
    local.get 13
    call 9
    local.get 9
    call 9
    local.get 7
    i32.const 48
    i32.add
    global.set 0
    i32.const 0)
  (func (;8;) (type 1) (param i32) (result i32)
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
                            i32.const 3760
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
                              i32.const 3808
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
                                i32.const 3800
                                i32.add
                                local.tee 4
                                i32.eq
                                if  ;; label = @15
                                  i32.const 3760
                                  local.get 5
                                  i32.const -2
                                  local.get 2
                                  i32.rotl
                                  i32.and
                                  i32.store
                                  br 1 (;@14;)
                                end
                                i32.const 3776
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
                            i32.const 3768
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
                                i32.const 3808
                                i32.add
                                i32.load
                                local.tee 1
                                i32.load offset=8
                                local.tee 0
                                local.get 3
                                i32.const 3800
                                i32.add
                                local.tee 3
                                i32.eq
                                if  ;; label = @15
                                  i32.const 3760
                                  local.get 5
                                  i32.const -2
                                  local.get 2
                                  i32.rotl
                                  i32.and
                                  local.tee 5
                                  i32.store
                                  br 1 (;@14;)
                                end
                                i32.const 3776
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
                                i32.const 3800
                                i32.add
                                local.set 1
                                i32.const 3780
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
                                    i32.const 3760
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
                              i32.const 3780
                              local.get 6
                              i32.store
                              i32.const 3768
                              local.get 4
                              i32.store
                              br 12 (;@1;)
                            end
                            i32.const 3764
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
                            i32.const 4064
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
                              i32.const 3776
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
                          i32.const 3764
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
                                i32.const 4064
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
                                i32.const 4064
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
                          i32.const 3768
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
                            i32.const 3776
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
                        i32.const 3768
                        i32.load
                        local.tee 1
                        local.get 6
                        i32.ge_u
                        if  ;; label = @11
                          i32.const 3780
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
                              i32.const 3768
                              local.get 2
                              i32.store
                              i32.const 3780
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
                            i32.const 3780
                            i32.const 0
                            i32.store
                            i32.const 3768
                            i32.const 0
                            i32.store
                          end
                          local.get 0
                          i32.const 8
                          i32.add
                          local.set 0
                          br 10 (;@1;)
                        end
                        i32.const 3772
                        i32.load
                        local.tee 1
                        local.get 6
                        i32.gt_u
                        if  ;; label = @11
                          i32.const 3784
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
                          i32.const 3772
                          local.get 1
                          i32.store
                          i32.const 3784
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
                          i32.const 4232
                          i32.load
                          if  ;; label = @12
                            i32.const 4240
                            i32.load
                            br 1 (;@11;)
                          end
                          i32.const 4244
                          i64.const -1
                          i64.store align=4
                          i32.const 4236
                          i64.const 281474976776192
                          i64.store align=4
                          i32.const 4232
                          local.get 11
                          i32.const 12
                          i32.add
                          i32.const -16
                          i32.and
                          i32.const 1431655768
                          i32.xor
                          i32.store
                          i32.const 4252
                          i32.const 0
                          i32.store
                          i32.const 4204
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
                          i32.const 4256
                          i32.const 48
                          i32.store
                          br 10 (;@1;)
                        end
                        block  ;; label = @11
                          i32.const 4200
                          i32.load
                          local.tee 0
                          i32.eqz
                          br_if 0 (;@11;)
                          i32.const 4192
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
                          i32.const 4256
                          i32.const 48
                          i32.store
                          br 10 (;@1;)
                        end
                        i32.const 4204
                        i32.load8_u
                        i32.const 4
                        i32.and
                        br_if 4 (;@6;)
                        block  ;; label = @11
                          block  ;; label = @12
                            i32.const 3784
                            i32.load
                            local.tee 3
                            if  ;; label = @13
                              i32.const 4208
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
                            call 13
                            local.tee 1
                            i32.const -1
                            i32.eq
                            br_if 5 (;@7;)
                            local.get 2
                            local.set 5
                            i32.const 4236
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
                            i32.const 4200
                            i32.load
                            local.tee 0
                            if  ;; label = @13
                              i32.const 4192
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
                            call 13
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
                          call 13
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
                          i32.const 4240
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
                          call 13
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
                          call 13
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
                i32.const 4204
                i32.const 4204
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
              call 13
              local.tee 1
              i32.const 0
              call 13
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
            i32.const 4192
            i32.const 4192
            i32.load
            local.get 5
            i32.add
            local.tee 0
            i32.store
            local.get 0
            i32.const 4196
            i32.load
            i32.gt_u
            if  ;; label = @5
              i32.const 4196
              local.get 0
              i32.store
            end
            block  ;; label = @5
              block  ;; label = @6
                block  ;; label = @7
                  i32.const 3784
                  i32.load
                  local.tee 7
                  if  ;; label = @8
                    i32.const 4208
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
                  i32.const 3776
                  i32.load
                  local.tee 0
                  i32.const 0
                  local.get 1
                  local.get 0
                  i32.ge_u
                  select
                  i32.eqz
                  if  ;; label = @8
                    i32.const 3776
                    local.get 1
                    i32.store
                  end
                  i32.const 0
                  local.set 0
                  i32.const 4212
                  local.get 5
                  i32.store
                  i32.const 4208
                  local.get 1
                  i32.store
                  i32.const 3792
                  i32.const -1
                  i32.store
                  i32.const 3796
                  i32.const 4232
                  i32.load
                  i32.store
                  i32.const 4220
                  i32.const 0
                  i32.store
                  loop  ;; label = @8
                    local.get 0
                    i32.const 3808
                    i32.add
                    local.get 0
                    i32.const 3800
                    i32.add
                    local.tee 2
                    i32.store
                    local.get 0
                    i32.const 3812
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
                  i32.const 3788
                  i32.const 4248
                  i32.load
                  i32.store
                  i32.const 3772
                  local.get 0
                  i32.store
                  i32.const 3784
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
                i32.const 3772
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
                i32.const 3788
                i32.const 4248
                i32.load
                i32.store
                i32.const 3772
                local.get 1
                i32.store
                i32.const 3784
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
              i32.const 3776
              i32.load
              local.tee 3
              i32.lt_u
              if  ;; label = @6
                i32.const 3776
                local.get 1
                i32.store
                local.get 1
                local.set 3
              end
              local.get 1
              local.get 5
              i32.add
              local.set 2
              i32.const 4208
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
                        i32.const 4208
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
                        i32.const 3784
                        local.get 4
                        i32.store
                        i32.const 3772
                        i32.const 3772
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
                      i32.const 3780
                      i32.load
                      i32.eq
                      if  ;; label = @10
                        i32.const 3780
                        local.get 4
                        i32.store
                        i32.const 3768
                        i32.const 3768
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
                            i32.const 3800
                            i32.add
                            i32.ne
                            drop
                            local.get 3
                            local.get 1
                            i32.load offset=12
                            local.tee 2
                            i32.eq
                            if  ;; label = @13
                              i32.const 3760
                              i32.const 3760
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
                            i32.const 4064
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
                              i32.const 3764
                              i32.const 3764
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
                        i32.const 3800
                        i32.add
                        local.set 0
                        block (result i32)  ;; label = @11
                          i32.const 3760
                          i32.load
                          local.tee 2
                          i32.const 1
                          local.get 1
                          i32.shl
                          local.tee 1
                          i32.and
                          i32.eqz
                          if  ;; label = @12
                            i32.const 3760
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
                      i32.const 4064
                      i32.add
                      local.set 1
                      i32.const 3764
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
                        i32.const 3764
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
                    i32.const 3788
                    i32.const 4248
                    i32.load
                    i32.store
                    i32.const 3772
                    local.get 0
                    i32.store
                    i32.const 3784
                    local.get 4
                    i32.store
                    local.get 2
                    i32.const 16
                    i32.add
                    i32.const 4216
                    i64.load align=4
                    i64.store align=4
                    local.get 2
                    i32.const 4208
                    i64.load align=4
                    i64.store offset=8 align=4
                    i32.const 4216
                    local.get 2
                    i32.const 8
                    i32.add
                    i32.store
                    i32.const 4212
                    local.get 5
                    i32.store
                    i32.const 4208
                    local.get 1
                    i32.store
                    i32.const 4220
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
                      i32.const 3800
                      i32.add
                      local.set 0
                      block (result i32)  ;; label = @10
                        i32.const 3760
                        i32.load
                        local.tee 2
                        i32.const 1
                        local.get 1
                        i32.shl
                        local.tee 1
                        i32.and
                        i32.eqz
                        if  ;; label = @11
                          i32.const 3760
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
                    i32.const 4064
                    i32.add
                    local.set 1
                    i32.const 3764
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
                      i32.const 3764
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
            i32.const 3772
            i32.load
            local.tee 1
            local.get 6
            i32.le_u
            br_if 0 (;@4;)
            i32.const 3784
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
            i32.const 3772
            local.get 1
            i32.store
            i32.const 3784
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
          i32.const 4256
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
            i32.const 4064
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
              i32.const 3764
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
            i32.const 3800
            i32.add
            local.set 0
            block (result i32)  ;; label = @5
              i32.const 3760
              i32.load
              local.tee 2
              i32.const 1
              local.get 1
              i32.shl
              local.tee 1
              i32.and
              i32.eqz
              if  ;; label = @6
                i32.const 3760
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
          i32.const 4064
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
            i32.const 3764
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
          i32.const 4064
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
            i32.const 3764
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
          i32.const 3800
          i32.add
          local.set 0
          i32.const 3780
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
              i32.const 3760
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
        i32.const 3780
        local.get 7
        i32.store
        i32.const 3768
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
  (func (;9;) (type 5) (param i32)
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
        i32.const 3776
        i32.load
        local.tee 4
        i32.lt_u
        br_if 1 (;@1;)
        local.get 0
        local.get 2
        i32.add
        local.set 0
        local.get 3
        i32.const 3780
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
            i32.const 3800
            i32.add
            i32.ne
            drop
            local.get 4
            local.get 3
            i32.load offset=12
            local.tee 1
            i32.eq
            if  ;; label = @5
              i32.const 3760
              i32.const 3760
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
            i32.const 4064
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
              i32.const 3764
              i32.const 3764
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
        i32.const 3768
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
          i32.const 3784
          i32.load
          i32.eq
          if  ;; label = @4
            i32.const 3784
            local.get 3
            i32.store
            i32.const 3772
            i32.const 3772
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
            i32.const 3780
            i32.load
            i32.ne
            br_if 3 (;@1;)
            i32.const 3768
            i32.const 0
            i32.store
            i32.const 3780
            i32.const 0
            i32.store
            return
          end
          local.get 5
          i32.const 3780
          i32.load
          i32.eq
          if  ;; label = @4
            i32.const 3780
            local.get 3
            i32.store
            i32.const 3768
            i32.const 3768
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
              i32.const 3800
              i32.add
              local.tee 7
              i32.ne
              if  ;; label = @6
                i32.const 3776
                i32.load
                drop
              end
              local.get 2
              local.get 4
              i32.eq
              if  ;; label = @6
                i32.const 3760
                i32.const 3760
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
                i32.const 3776
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
                i32.const 3776
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
              i32.const 4064
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
                i32.const 3764
                i32.const 3764
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
          i32.const 3780
          i32.load
          i32.ne
          br_if 1 (;@2;)
          i32.const 3768
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
        i32.const 3800
        i32.add
        local.set 0
        block (result i32)  ;; label = @3
          i32.const 3760
          i32.load
          local.tee 2
          i32.const 1
          local.get 1
          i32.shl
          local.tee 1
          i32.and
          i32.eqz
          if  ;; label = @4
            i32.const 3760
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
      i32.const 4064
      i32.add
      local.set 1
      block  ;; label = @2
        i32.const 3764
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
          i32.const 3764
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
      i32.const 3792
      i32.const 3792
      i32.load
      i32.const -1
      i32.add
      local.tee 0
      i32.store
      local.get 0
      br_if 0 (;@1;)
      i32.const 4216
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
      i32.const 3792
      i32.const -1
      i32.store
    end)
  (func (;10;) (type 9) (param i32 i32)
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
        i32.const 3780
        i32.load
        i32.ne
        if  ;; label = @3
          i32.const 3776
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
            i32.const 3800
            i32.add
            i32.ne
            drop
            local.get 4
            local.get 0
            i32.load offset=12
            local.tee 2
            i32.eq
            if  ;; label = @5
              i32.const 3760
              i32.const 3760
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
            i32.const 4064
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
              i32.const 3764
              i32.const 3764
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
        i32.const 3768
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
          i32.const 3784
          i32.load
          i32.eq
          if  ;; label = @4
            i32.const 3784
            local.get 0
            i32.store
            i32.const 3772
            i32.const 3772
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
            i32.const 3780
            i32.load
            i32.ne
            br_if 3 (;@1;)
            i32.const 3768
            i32.const 0
            i32.store
            i32.const 3780
            i32.const 0
            i32.store
            return
          end
          local.get 5
          i32.const 3780
          i32.load
          i32.eq
          if  ;; label = @4
            i32.const 3780
            local.get 0
            i32.store
            i32.const 3768
            i32.const 3768
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
          i32.const 3776
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
              i32.const 3800
              i32.add
              i32.ne
              drop
              local.get 4
              local.get 5
              i32.load offset=12
              local.tee 3
              i32.eq
              if  ;; label = @6
                i32.const 3760
                i32.const 3760
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
              i32.const 4064
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
                i32.const 3764
                i32.const 3764
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
          i32.const 3780
          i32.load
          i32.ne
          br_if 1 (;@2;)
          i32.const 3768
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
        i32.const 3800
        i32.add
        local.set 1
        block (result i32)  ;; label = @3
          i32.const 3760
          i32.load
          local.tee 3
          i32.const 1
          local.get 2
          i32.shl
          local.tee 2
          i32.and
          i32.eqz
          if  ;; label = @4
            i32.const 3760
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
      i32.const 4064
      i32.add
      local.set 2
      i32.const 3764
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
        i32.const 3764
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
  (func (;11;) (type 4) (param i32 i32) (result i32)
    block (result i32)  ;; label = @1
      i32.const 48
      i32.const -4160
      local.get 1
      i32.lt_u
      br_if 0 (;@1;)
      drop
      local.get 1
      call 12
      local.tee 1
      i32.eqz
      if  ;; label = @2
        i32.const 48
        return
      end
      local.get 0
      local.get 1
      i32.store
      i32.const 0
    end)
  (func (;12;) (type 1) (param i32) (result i32)
    (local i32 i32 i32 i32 i32 i32)
    block  ;; label = @1
      i32.const 4096
      local.set 1
      br 0 (;@1;)
    end
    i32.const -64
    local.get 1
    i32.sub
    local.get 0
    i32.le_u
    if  ;; label = @1
      i32.const 4256
      i32.const 48
      i32.store
      i32.const 0
      return
    end
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
    local.tee 3
    i32.const 12
    i32.or
    local.get 1
    i32.add
    call 8
    local.tee 2
    i32.eqz
    if  ;; label = @1
      i32.const 0
      return
    end
    local.get 2
    i32.const -8
    i32.add
    local.set 0
    block  ;; label = @1
      local.get 1
      i32.const -1
      i32.add
      local.get 2
      i32.and
      i32.eqz
      if  ;; label = @2
        local.get 0
        local.set 1
        br 1 (;@1;)
      end
      local.get 2
      i32.const -4
      i32.add
      local.tee 5
      i32.load
      local.tee 6
      i32.const -8
      i32.and
      local.get 1
      local.get 2
      i32.add
      i32.const -1
      i32.add
      i32.const 0
      local.get 1
      i32.sub
      i32.and
      i32.const -8
      i32.add
      local.tee 2
      local.get 1
      local.get 2
      i32.add
      local.get 2
      local.get 0
      i32.sub
      i32.const 15
      i32.gt_u
      select
      local.tee 1
      local.get 0
      i32.sub
      local.tee 2
      i32.sub
      local.set 4
      local.get 6
      i32.const 3
      i32.and
      i32.eqz
      if  ;; label = @2
        local.get 1
        local.get 4
        i32.store offset=4
        local.get 1
        local.get 0
        i32.load
        local.get 2
        i32.add
        i32.store
        br 1 (;@1;)
      end
      local.get 1
      local.get 4
      local.get 1
      i32.load offset=4
      i32.const 1
      i32.and
      i32.or
      i32.const 2
      i32.or
      i32.store offset=4
      local.get 1
      local.get 4
      i32.add
      local.tee 4
      local.get 4
      i32.load offset=4
      i32.const 1
      i32.or
      i32.store offset=4
      local.get 5
      local.get 2
      local.get 5
      i32.load
      i32.const 1
      i32.and
      i32.or
      i32.const 2
      i32.or
      i32.store
      local.get 1
      local.get 1
      i32.load offset=4
      i32.const 1
      i32.or
      i32.store offset=4
      local.get 0
      local.get 2
      call 10
    end
    block  ;; label = @1
      local.get 1
      i32.load offset=4
      local.tee 0
      i32.const 3
      i32.and
      i32.eqz
      br_if 0 (;@1;)
      local.get 0
      i32.const -8
      i32.and
      local.tee 2
      local.get 3
      i32.const 16
      i32.add
      i32.le_u
      br_if 0 (;@1;)
      local.get 1
      local.get 3
      local.get 0
      i32.const 1
      i32.and
      i32.or
      i32.const 2
      i32.or
      i32.store offset=4
      local.get 1
      local.get 3
      i32.add
      local.tee 0
      local.get 2
      local.get 3
      i32.sub
      local.tee 3
      i32.const 3
      i32.or
      i32.store offset=4
      local.get 1
      local.get 2
      i32.add
      local.tee 2
      local.get 2
      i32.load offset=4
      i32.const 1
      i32.or
      i32.store offset=4
      local.get 0
      local.get 3
      call 10
    end
    local.get 1
    i32.const 8
    i32.add)
  (func (;13;) (type 1) (param i32) (result i32)
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
        i32.const 4256
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
  (func (;14;) (type 2) (param i32 i32 i32)
    (local i32)
    global.get 0
    i32.const 16
    i32.sub
    local.tee 3
    global.set 0
    local.get 3
    local.get 2
    i32.store offset=12
    local.get 0
    local.get 1
    local.get 2
    call 29
    local.get 3
    i32.const 16
    i32.add
    global.set 0)
  (func (;15;) (type 5) (param i32)
    (local i32)
    block  ;; label = @1
      local.get 0
      i32.load offset=64
      i32.const 10
      i32.eq
      br_if 0 (;@1;)
      local.get 0
      i32.load offset=20
      local.tee 1
      local.get 0
      i32.load offset=16
      i32.eq
      br_if 0 (;@1;)
      local.get 0
      local.get 1
      i32.const 1
      i32.add
      i32.store offset=20
      local.get 1
      i32.const 10
      i32.store8
      return
    end
    local.get 0
    call 18)
  (func (;16;) (type 2) (param i32 i32 i32)
    (local i32 i32 i32 i32 i32)
    block  ;; label = @1
      local.get 2
      i32.load offset=16
      local.tee 3
      if (result i32)  ;; label = @2
        local.get 3
      else
        local.get 2
        call 27
        br_if 1 (;@1;)
        local.get 2
        i32.load offset=16
      end
      local.get 2
      i32.load offset=20
      local.tee 5
      i32.sub
      local.get 1
      i32.lt_u
      if  ;; label = @2
        local.get 2
        local.get 0
        local.get 1
        local.get 2
        i32.load offset=32
        call_indirect (type 0)
        drop
        return
      end
      block  ;; label = @2
        local.get 2
        i32.load offset=64
        i32.const 0
        i32.lt_s
        br_if 0 (;@2;)
        local.get 0
        local.set 3
        loop  ;; label = @3
          local.get 1
          local.get 4
          i32.eq
          br_if 1 (;@2;)
          local.get 4
          i32.const 1
          i32.add
          local.set 4
          local.get 1
          local.get 3
          i32.add
          local.get 3
          i32.const -1
          i32.add
          local.tee 7
          local.set 3
          i32.const -1
          i32.add
          i32.load8_u
          i32.const 10
          i32.ne
          br_if 0 (;@3;)
        end
        local.get 2
        local.get 0
        local.get 1
        local.get 4
        i32.sub
        i32.const 1
        i32.add
        local.tee 0
        local.get 2
        i32.load offset=32
        call_indirect (type 0)
        local.get 0
        i32.lt_u
        br_if 1 (;@1;)
        local.get 1
        local.get 7
        i32.add
        i32.const 1
        i32.add
        local.set 0
        local.get 2
        i32.load offset=20
        local.set 5
        local.get 4
        i32.const -1
        i32.add
        local.set 1
      end
      local.get 5
      local.get 0
      local.get 1
      call 32
      local.get 2
      local.get 2
      i32.load offset=20
      local.get 1
      i32.add
      i32.store offset=20
    end)
  (func (;17;) (type 8) (param i32 i32 i32 i32) (result i32)
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
        call 27
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
      call 32
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
  (func (;18;) (type 5) (param i32)
    (local i32 i32)
    global.get 0
    i32.const 16
    i32.sub
    local.tee 1
    global.set 0
    local.get 1
    i32.const 10
    i32.store8 offset=15
    block  ;; label = @1
      block  ;; label = @2
        local.get 0
        i32.load offset=16
        local.tee 2
        if (result i32)  ;; label = @3
          local.get 2
        else
          local.get 0
          call 27
          br_if 2 (;@1;)
          local.get 0
          i32.load offset=16
        end
        local.get 0
        i32.load offset=20
        local.tee 2
        i32.eq
        br_if 0 (;@2;)
        local.get 0
        i32.load offset=64
        i32.const 10
        i32.eq
        br_if 0 (;@2;)
        local.get 0
        local.get 2
        i32.const 1
        i32.add
        i32.store offset=20
        local.get 2
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
      i32.load offset=32
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
  (func (;19;) (type 1) (param i32) (result i32)
    block (result i32)  ;; label = @1
      i32.const 0
      local.get 0
      i32.load offset=56
      call 1
      local.tee 0
      i32.eqz
      br_if 0 (;@1;)
      drop
      i32.const 4256
      local.get 0
      i32.store
      i32.const -1
    end)
  (func (;20;) (type 3) (param i32 i64 i32) (result i64)
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
        i32.const 4256
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
  (func (;21;) (type 3) (param i32 i64 i32) (result i64)
    local.get 0
    i32.load offset=56
    local.get 1
    local.get 2
    call 20)
  (func (;22;) (type 0) (param i32 i32 i32) (result i32)
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
        i32.const 4256
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
        i32.const 4256
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
  (func (;23;) (type 0) (param i32 i32 i32) (result i32)
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
      call 22
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
          call 22
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
  (func (;24;) (type 1) (param i32) (result i32)
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
      i32.const 4256
      local.get 0
      i32.store
      i32.const 0
    end
    local.get 1
    i32.const 32
    i32.add
    global.set 0)
  (func (;25;) (type 0) (param i32 i32 i32) (result i32)
    local.get 0
    i32.const 2
    i32.store offset=32
    block  ;; label = @1
      local.get 0
      i32.load8_u
      i32.const 64
      i32.and
      br_if 0 (;@1;)
      local.get 0
      i32.load offset=56
      call 24
      br_if 0 (;@1;)
      local.get 0
      i32.const -1
      i32.store offset=64
    end
    local.get 0
    local.get 1
    local.get 2
    call 23)
  (func (;26;) (type 6)
    (local i32 i32 i32)
    i32.const 4260
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
      i32.const 5304
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
      i32.const 3744
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
      i32.const 3624
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
  (func (;27;) (type 1) (param i32) (result i32)
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
  (func (;28;) (type 1) (param i32) (result i32)
    (local i32 i32 i32 i32)
    i32.const 5332
    i32.load
    local.tee 2
    i32.eqz
    if  ;; label = @1
      i32.const 5332
      i32.const 5308
      i32.store
      i32.const 5308
      local.set 2
    end
    block  ;; label = @1
      block  ;; label = @2
        loop  ;; label = @3
          local.get 0
          local.get 1
          i32.const 1168
          i32.add
          i32.load8_u
          i32.ne
          if  ;; label = @4
            i32.const 77
            local.set 3
            local.get 1
            i32.const 1
            i32.add
            local.tee 1
            i32.const 77
            i32.ne
            br_if 1 (;@3;)
            br 2 (;@2;)
          end
        end
        local.get 1
        local.set 3
        local.get 1
        br_if 0 (;@2;)
        i32.const 1248
        local.set 0
        br 1 (;@1;)
      end
      i32.const 1248
      local.set 1
      loop  ;; label = @2
        local.get 1
        i32.load8_u
        local.get 1
        i32.const 1
        i32.add
        local.tee 0
        local.set 1
        br_if 0 (;@2;)
        local.get 0
        local.set 1
        local.get 3
        i32.const -1
        i32.add
        local.tee 3
        br_if 0 (;@2;)
      end
    end
    local.get 2
    i32.load offset=20
    drop
    local.get 0)
  (func (;29;) (type 2) (param i32 i32 i32)
    (local i32 i32 i32)
    global.get 0
    i32.const 208
    i32.sub
    local.tee 3
    global.set 0
    local.get 3
    local.get 2
    i32.store offset=204
    local.get 3
    i32.const 192
    i32.add
    i64.const 0
    i64.store
    local.get 3
    i32.const 184
    i32.add
    i64.const 0
    i64.store
    local.get 3
    i32.const 176
    i32.add
    i64.const 0
    i64.store
    local.get 3
    i64.const 0
    i64.store offset=168
    local.get 3
    i64.const 0
    i64.store offset=160
    local.get 3
    local.get 2
    i32.store offset=200
    i32.const 0
    local.get 1
    local.get 3
    i32.const 200
    i32.add
    local.get 3
    i32.const 80
    i32.add
    local.get 3
    i32.const 160
    i32.add
    call 30
    i32.const 0
    i32.ge_s
    if  ;; label = @1
      local.get 0
      i32.load
      local.set 4
      local.get 0
      i32.load offset=60
      i32.const 0
      i32.le_s
      if  ;; label = @2
        local.get 0
        local.get 4
        i32.const -33
        i32.and
        i32.store
      end
      block (result i32)  ;; label = @2
        block  ;; label = @3
          block  ;; label = @4
            local.get 0
            i32.load offset=44
            i32.eqz
            if  ;; label = @5
              local.get 0
              i32.const 80
              i32.store offset=44
              local.get 0
              i32.const 0
              i32.store offset=24
              local.get 0
              i64.const 0
              i64.store offset=16
              local.get 0
              i32.load offset=40
              local.set 5
              local.get 0
              local.get 3
              i32.store offset=40
              br 1 (;@4;)
            end
            local.get 0
            i32.load offset=16
            br_if 1 (;@3;)
          end
          i32.const -1
          local.get 0
          call 27
          br_if 1 (;@2;)
          drop
        end
        local.get 0
        local.get 1
        local.get 3
        i32.const 200
        i32.add
        local.get 3
        i32.const 80
        i32.add
        local.get 3
        i32.const 160
        i32.add
        call 30
      end
      local.set 2
      local.get 5
      if (result i32)  ;; label = @2
        local.get 0
        i32.const 0
        i32.const 0
        local.get 0
        i32.load offset=32
        call_indirect (type 0)
        drop
        local.get 0
        i32.const 0
        i32.store offset=44
        local.get 0
        local.get 5
        i32.store offset=40
        local.get 0
        i32.const 0
        i32.store offset=24
        local.get 0
        i32.const 0
        i32.store offset=16
        local.get 0
        i32.load offset=20
        local.set 1
        local.get 0
        i32.const 0
        i32.store offset=20
        i32.const 0
      else
        local.get 2
      end
      drop
      local.get 0
      local.get 0
      i32.load
      local.get 4
      i32.const 32
      i32.and
      i32.or
      i32.store
    end
    local.get 3
    i32.const 208
    i32.add
    global.set 0)
  (func (;30;) (type 10) (param i32 i32 i32 i32 i32) (result i32)
    (local i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i64 i64 f64 f64)
    global.get 0
    i32.const 880
    i32.sub
    local.tee 10
    global.set 0
    local.get 10
    i32.const 336
    i32.add
    i32.const 8
    i32.or
    local.set 31
    local.get 10
    i32.const 55
    i32.add
    local.set 32
    i32.const -338
    local.get 10
    i32.sub
    local.set 33
    local.get 10
    i32.const 336
    i32.add
    i32.const 9
    i32.or
    local.set 27
    local.get 10
    i32.const 656
    i32.add
    local.set 34
    local.get 10
    i32.const 336
    i32.add
    local.set 25
    i32.const 0
    local.get 10
    i32.const 336
    i32.add
    i32.sub
    local.set 30
    local.get 10
    i32.const 56
    i32.add
    local.set 21
    block  ;; label = @1
      block  ;; label = @2
        loop  ;; label = @3
          block  ;; label = @4
            local.get 1
            local.set 8
            local.get 5
            i32.const 2147483647
            local.get 18
            i32.sub
            i32.gt_s
            br_if 0 (;@4;)
            local.get 5
            local.get 18
            i32.add
            local.set 18
            block  ;; label = @5
              block  ;; label = @6
                block  ;; label = @7
                  local.get 8
                  i32.load8_u
                  local.tee 5
                  if  ;; label = @8
                    loop  ;; label = @9
                      block  ;; label = @10
                        block  ;; label = @11
                          local.get 5
                          i32.const 255
                          i32.and
                          local.tee 5
                          if  ;; label = @12
                            local.get 5
                            i32.const 37
                            i32.ne
                            br_if 2 (;@10;)
                            local.get 1
                            local.tee 6
                            local.set 5
                            loop  ;; label = @13
                              local.get 5
                              i32.const 1
                              i32.add
                              i32.load8_u
                              i32.const 37
                              i32.ne
                              if  ;; label = @14
                                local.get 5
                                local.set 1
                                br 3 (;@11;)
                              end
                              local.get 6
                              i32.const 1
                              i32.add
                              local.set 6
                              local.get 5
                              i32.load8_u offset=2
                              local.get 5
                              i32.const 2
                              i32.add
                              local.tee 1
                              local.set 5
                              i32.const 37
                              i32.eq
                              br_if 0 (;@13;)
                            end
                            br 1 (;@11;)
                          end
                          local.get 1
                          local.set 6
                        end
                        local.get 6
                        local.get 8
                        i32.sub
                        local.tee 5
                        i32.const 2147483647
                        local.get 18
                        i32.sub
                        local.tee 20
                        i32.gt_s
                        br_if 6 (;@4;)
                        block  ;; label = @11
                          local.get 0
                          i32.eqz
                          br_if 0 (;@11;)
                          local.get 0
                          i32.load8_u
                          i32.const 32
                          i32.and
                          br_if 0 (;@11;)
                          local.get 8
                          local.get 5
                          local.get 0
                          call 16
                        end
                        local.get 5
                        br_if 7 (;@3;)
                        local.get 1
                        i32.const 1
                        i32.add
                        local.set 5
                        block (result i32)  ;; label = @11
                          i32.const -1
                          local.get 1
                          i32.load8_s offset=1
                          local.tee 9
                          i32.const -48
                          i32.add
                          local.tee 7
                          i32.const 9
                          i32.gt_u
                          br_if 0 (;@11;)
                          drop
                          local.get 1
                          i32.const 3
                          i32.add
                          local.get 5
                          local.get 1
                          i32.load8_u offset=2
                          i32.const 36
                          i32.eq
                          local.tee 6
                          select
                          local.set 5
                          i32.const 1
                          local.get 26
                          local.get 6
                          select
                          local.set 26
                          local.get 1
                          i32.const 3
                          i32.const 1
                          local.get 6
                          select
                          i32.add
                          i32.load8_s
                          local.set 9
                          local.get 7
                          i32.const -1
                          local.get 6
                          select
                        end
                        local.set 7
                        i32.const 0
                        local.set 6
                        block  ;; label = @11
                          local.get 9
                          i32.const -32
                          i32.add
                          local.tee 1
                          i32.const 31
                          i32.gt_u
                          if  ;; label = @12
                            local.get 5
                            local.set 1
                            br 1 (;@11;)
                          end
                          i32.const 1
                          local.get 1
                          i32.shl
                          local.tee 13
                          i32.const 75913
                          i32.and
                          i32.eqz
                          if  ;; label = @12
                            local.get 5
                            local.set 1
                            br 1 (;@11;)
                          end
                          loop  ;; label = @12
                            local.get 5
                            i32.const 1
                            i32.add
                            local.set 1
                            local.get 6
                            local.get 13
                            i32.or
                            local.set 6
                            local.get 5
                            i32.load8_s offset=1
                            local.tee 9
                            i32.const -32
                            i32.add
                            local.tee 11
                            i32.const 31
                            i32.gt_u
                            br_if 1 (;@11;)
                            local.get 1
                            local.set 5
                            i32.const 1
                            local.get 11
                            i32.shl
                            local.tee 13
                            i32.const 75913
                            i32.and
                            br_if 0 (;@12;)
                          end
                        end
                        block  ;; label = @11
                          local.get 9
                          i32.const 42
                          i32.eq
                          if  ;; label = @12
                            block (result i32)  ;; label = @13
                              block  ;; label = @14
                                local.get 1
                                i32.load8_s offset=1
                                i32.const -48
                                i32.add
                                local.tee 5
                                i32.const 9
                                i32.gt_u
                                br_if 0 (;@14;)
                                local.get 1
                                i32.load8_u offset=2
                                i32.const 36
                                i32.ne
                                br_if 0 (;@14;)
                                local.get 4
                                local.get 5
                                i32.const 2
                                i32.shl
                                i32.add
                                i32.const 10
                                i32.store
                                local.get 1
                                i32.const 3
                                i32.add
                                local.set 11
                                i32.const 1
                                local.set 26
                                local.get 1
                                i32.load8_s offset=1
                                i32.const 3
                                i32.shl
                                local.get 3
                                i32.add
                                i32.const -384
                                i32.add
                                i32.load
                                br 1 (;@13;)
                              end
                              local.get 26
                              br_if 6 (;@7;)
                              local.get 1
                              i32.const 1
                              i32.add
                              local.set 11
                              local.get 0
                              i32.eqz
                              if  ;; label = @14
                                i32.const 0
                                local.set 26
                                i32.const 0
                                local.set 14
                                br 3 (;@11;)
                              end
                              local.get 2
                              local.get 2
                              i32.load
                              local.tee 1
                              i32.const 4
                              i32.add
                              i32.store
                              i32.const 0
                              local.set 26
                              local.get 1
                              i32.load
                            end
                            local.tee 14
                            i32.const -1
                            i32.gt_s
                            br_if 1 (;@11;)
                            i32.const 0
                            local.get 14
                            i32.sub
                            local.set 14
                            local.get 6
                            i32.const 8192
                            i32.or
                            local.set 6
                            br 1 (;@11;)
                          end
                          i32.const 0
                          local.set 14
                          local.get 9
                          i32.const -48
                          i32.add
                          local.tee 13
                          i32.const 9
                          i32.gt_u
                          if  ;; label = @12
                            local.get 1
                            local.set 11
                            br 1 (;@11;)
                          end
                          i32.const 0
                          local.set 5
                          loop  ;; label = @12
                            i32.const -1
                            local.set 14
                            local.get 1
                            i32.load8_s offset=1
                            local.get 1
                            i32.const 1
                            i32.add
                            local.tee 11
                            local.set 1
                            local.get 5
                            i32.const 214748364
                            i32.le_u
                            if  ;; label = @13
                              i32.const -1
                              local.get 5
                              i32.const 10
                              i32.mul
                              local.tee 5
                              local.get 13
                              i32.add
                              local.get 13
                              i32.const 2147483647
                              local.get 5
                              i32.sub
                              i32.gt_s
                              select
                              local.set 14
                            end
                            local.get 14
                            local.set 5
                            i32.const -48
                            i32.add
                            local.tee 13
                            i32.const 10
                            i32.lt_u
                            br_if 0 (;@12;)
                          end
                          local.get 14
                          i32.const 0
                          i32.lt_s
                          br_if 7 (;@4;)
                        end
                        i32.const 0
                        local.set 5
                        i32.const -1
                        local.set 9
                        block  ;; label = @11
                          local.get 11
                          i32.load8_u
                          i32.const 46
                          i32.ne
                          if  ;; label = @12
                            local.get 11
                            local.set 1
                            i32.const 0
                            local.set 17
                            br 1 (;@11;)
                          end
                          local.get 11
                          i32.load8_s offset=1
                          local.tee 9
                          i32.const 42
                          i32.eq
                          if  ;; label = @12
                            block (result i32)  ;; label = @13
                              block  ;; label = @14
                                local.get 11
                                i32.load8_s offset=2
                                i32.const -48
                                i32.add
                                local.tee 1
                                i32.const 9
                                i32.gt_u
                                br_if 0 (;@14;)
                                local.get 11
                                i32.load8_u offset=3
                                i32.const 36
                                i32.ne
                                br_if 0 (;@14;)
                                local.get 4
                                local.get 1
                                i32.const 2
                                i32.shl
                                i32.add
                                i32.const 10
                                i32.store
                                local.get 11
                                i32.const 4
                                i32.add
                                local.set 1
                                local.get 11
                                i32.load8_s offset=2
                                i32.const 3
                                i32.shl
                                local.get 3
                                i32.add
                                i32.const -384
                                i32.add
                                i32.load
                                br 1 (;@13;)
                              end
                              local.get 26
                              br_if 6 (;@7;)
                              local.get 11
                              i32.const 2
                              i32.add
                              local.set 1
                              i32.const 0
                              local.get 0
                              i32.eqz
                              br_if 0 (;@13;)
                              drop
                              local.get 2
                              local.get 2
                              i32.load
                              local.tee 9
                              i32.const 4
                              i32.add
                              i32.store
                              local.get 9
                              i32.load
                            end
                            local.tee 9
                            i32.const -1
                            i32.xor
                            i32.const 31
                            i32.shr_u
                            local.set 17
                            br 1 (;@11;)
                          end
                          local.get 11
                          i32.const 1
                          i32.add
                          local.set 1
                          local.get 9
                          i32.const -48
                          i32.add
                          local.tee 15
                          i32.const 9
                          i32.gt_u
                          if  ;; label = @12
                            i32.const 1
                            local.set 17
                            i32.const 0
                            local.set 9
                            br 1 (;@11;)
                          end
                          i32.const 0
                          local.set 11
                          local.get 1
                          local.set 13
                          loop  ;; label = @12
                            i32.const -1
                            local.set 9
                            local.get 11
                            i32.const 214748364
                            i32.le_u
                            if  ;; label = @13
                              i32.const -1
                              local.get 11
                              i32.const 10
                              i32.mul
                              local.tee 1
                              local.get 15
                              i32.add
                              local.get 15
                              i32.const 2147483647
                              local.get 1
                              i32.sub
                              i32.gt_s
                              select
                              local.set 9
                            end
                            i32.const 1
                            local.set 17
                            local.get 13
                            i32.load8_s offset=1
                            local.get 13
                            i32.const 1
                            i32.add
                            local.tee 1
                            local.set 13
                            local.get 9
                            local.set 11
                            i32.const -48
                            i32.add
                            local.tee 15
                            i32.const 10
                            i32.lt_u
                            br_if 0 (;@12;)
                          end
                        end
                        loop  ;; label = @11
                          local.get 5
                          local.set 12
                          local.get 1
                          i32.load8_s
                          i32.const -65
                          i32.add
                          local.tee 5
                          i32.const 57
                          i32.gt_u
                          br_if 4 (;@7;)
                          local.get 1
                          i32.const 1
                          i32.add
                          local.set 1
                          local.get 12
                          i32.const 58
                          i32.mul
                          local.get 5
                          i32.add
                          i32.const 2848
                          i32.add
                          i32.load8_u
                          local.tee 5
                          i32.const -1
                          i32.add
                          i32.const 8
                          i32.lt_u
                          br_if 0 (;@11;)
                        end
                        local.get 5
                        i32.eqz
                        br_if 3 (;@7;)
                        block  ;; label = @11
                          block  ;; label = @12
                            block  ;; label = @13
                              local.get 5
                              i32.const 27
                              i32.eq
                              if  ;; label = @14
                                local.get 7
                                i32.const -1
                                i32.le_s
                                br_if 1 (;@13;)
                                br 7 (;@7;)
                              end
                              local.get 7
                              i32.const 0
                              i32.lt_s
                              br_if 1 (;@12;)
                              local.get 4
                              local.get 7
                              i32.const 2
                              i32.shl
                              i32.add
                              local.get 5
                              i32.store
                              local.get 10
                              local.get 3
                              local.get 7
                              i32.const 3
                              i32.shl
                              i32.add
                              i64.load
                              i64.store offset=56
                            end
                            i32.const 0
                            local.set 5
                            local.get 0
                            i32.eqz
                            br_if 9 (;@3;)
                            br 1 (;@11;)
                          end
                          local.get 0
                          i32.eqz
                          if  ;; label = @12
                            i32.const 0
                            local.set 18
                            br 11 (;@1;)
                          end
                          local.get 10
                          i32.const 56
                          i32.add
                          local.get 5
                          local.get 2
                          call 31
                        end
                        local.get 6
                        i32.const -65537
                        i32.and
                        local.tee 7
                        local.get 6
                        local.get 6
                        i32.const 8192
                        i32.and
                        select
                        local.set 16
                        block  ;; label = @11
                          block  ;; label = @12
                            block  ;; label = @13
                              local.get 1
                              i32.const -1
                              i32.add
                              i32.load8_s
                              local.tee 5
                              i32.const -33
                              i32.and
                              local.get 5
                              local.get 5
                              i32.const 15
                              i32.and
                              i32.const 3
                              i32.eq
                              select
                              local.get 5
                              local.get 12
                              select
                              local.tee 11
                              i32.const -65
                              i32.add
                              local.tee 5
                              i32.const 55
                              i32.gt_u
                              br_if 0 (;@13;)
                              block  ;; label = @14
                                block  ;; label = @15
                                  block (result i32)  ;; label = @16
                                    block  ;; label = @17
                                      block  ;; label = @18
                                        block (result i32)  ;; label = @19
                                          block  ;; label = @20
                                            block  ;; label = @21
                                              block  ;; label = @22
                                                block  ;; label = @23
                                                  block (result i32)  ;; label = @24
                                                    block  ;; label = @25
                                                      block  ;; label = @26
                                                        block  ;; label = @27
                                                          block  ;; label = @28
                                                            block  ;; label = @29
                                                              block  ;; label = @30
                                                                local.get 5
                                                                i32.const 1
                                                                i32.sub
                                                                br_table 17 (;@13;) 13 (;@17;) 17 (;@13;) 16 (;@14;) 16 (;@14;) 16 (;@14;) 17 (;@13;) 17 (;@13;) 17 (;@13;) 17 (;@13;) 17 (;@13;) 17 (;@13;) 17 (;@13;) 17 (;@13;) 17 (;@13;) 17 (;@13;) 17 (;@13;) 12 (;@18;) 17 (;@13;) 17 (;@13;) 17 (;@13;) 17 (;@13;) 3 (;@27;) 17 (;@13;) 17 (;@13;) 17 (;@13;) 17 (;@13;) 17 (;@13;) 17 (;@13;) 17 (;@13;) 17 (;@13;) 16 (;@14;) 17 (;@13;) 8 (;@22;) 5 (;@25;) 16 (;@14;) 16 (;@14;) 16 (;@14;) 17 (;@13;) 5 (;@25;) 17 (;@13;) 17 (;@13;) 17 (;@13;) 9 (;@21;) 1 (;@29;) 4 (;@26;) 2 (;@28;) 17 (;@13;) 17 (;@13;) 10 (;@20;) 17 (;@13;) 0 (;@30;) 17 (;@13;) 17 (;@13;) 3 (;@27;) 16 (;@14;)
                                                              end
                                                              i32.const 0
                                                              local.set 12
                                                              local.get 10
                                                              i64.load offset=56
                                                              local.set 35
                                                              i32.const 2822
                                                              br 5 (;@24;)
                                                            end
                                                            i32.const 0
                                                            local.set 5
                                                            local.get 12
                                                            i32.const 255
                                                            i32.and
                                                            local.tee 6
                                                            i32.const 7
                                                            i32.gt_u
                                                            br_if 25 (;@3;)
                                                            block  ;; label = @29
                                                              block  ;; label = @30
                                                                block  ;; label = @31
                                                                  block  ;; label = @32
                                                                    block  ;; label = @33
                                                                      block  ;; label = @34
                                                                        block  ;; label = @35
                                                                          local.get 6
                                                                          i32.const 1
                                                                          i32.sub
                                                                          br_table 1 (;@34;) 2 (;@33;) 3 (;@32;) 4 (;@31;) 32 (;@3;) 5 (;@30;) 6 (;@29;) 0 (;@35;)
                                                                        end
                                                                        local.get 10
                                                                        i32.load offset=56
                                                                        local.get 18
                                                                        i32.store
                                                                        br 31 (;@3;)
                                                                      end
                                                                      local.get 10
                                                                      i32.load offset=56
                                                                      local.get 18
                                                                      i32.store
                                                                      br 30 (;@3;)
                                                                    end
                                                                    local.get 10
                                                                    i32.load offset=56
                                                                    local.get 18
                                                                    i64.extend_i32_s
                                                                    i64.store
                                                                    br 29 (;@3;)
                                                                  end
                                                                  local.get 10
                                                                  i32.load offset=56
                                                                  local.get 18
                                                                  i32.store16
                                                                  br 28 (;@3;)
                                                                end
                                                                local.get 10
                                                                i32.load offset=56
                                                                local.get 18
                                                                i32.store8
                                                                br 27 (;@3;)
                                                              end
                                                              local.get 10
                                                              i32.load offset=56
                                                              local.get 18
                                                              i32.store
                                                              br 26 (;@3;)
                                                            end
                                                            local.get 10
                                                            i32.load offset=56
                                                            local.get 18
                                                            i64.extend_i32_s
                                                            i64.store
                                                            br 25 (;@3;)
                                                          end
                                                          local.get 9
                                                          i32.const 8
                                                          local.get 9
                                                          i32.const 8
                                                          i32.gt_u
                                                          select
                                                          local.set 9
                                                          local.get 16
                                                          i32.const 8
                                                          i32.or
                                                          local.set 16
                                                          i32.const 120
                                                          local.set 11
                                                        end
                                                        i32.const 0
                                                        local.set 12
                                                        i32.const 2822
                                                        local.set 15
                                                        local.get 10
                                                        i64.load offset=56
                                                        local.tee 35
                                                        i64.eqz
                                                        if  ;; label = @27
                                                          local.get 21
                                                          local.set 8
                                                          br 4 (;@23;)
                                                        end
                                                        local.get 11
                                                        i32.const 32
                                                        i32.and
                                                        local.set 5
                                                        local.get 21
                                                        local.set 8
                                                        loop  ;; label = @27
                                                          local.get 8
                                                          i32.const -1
                                                          i32.add
                                                          local.tee 8
                                                          local.get 35
                                                          i32.wrap_i64
                                                          i32.const 15
                                                          i32.and
                                                          i32.const 3456
                                                          i32.add
                                                          i32.load8_u
                                                          local.get 5
                                                          i32.or
                                                          i32.store8
                                                          local.get 35
                                                          i64.const 4
                                                          i64.shr_u
                                                          local.tee 35
                                                          i64.const 0
                                                          i64.ne
                                                          br_if 0 (;@27;)
                                                        end
                                                        local.get 16
                                                        i32.const 8
                                                        i32.and
                                                        i32.eqz
                                                        br_if 3 (;@23;)
                                                        local.get 10
                                                        i64.load offset=56
                                                        i64.eqz
                                                        br_if 3 (;@23;)
                                                        local.get 11
                                                        i32.const 4
                                                        i32.shr_s
                                                        i32.const 2822
                                                        i32.add
                                                        local.set 15
                                                        i32.const 2
                                                        local.set 12
                                                        br 3 (;@23;)
                                                      end
                                                      local.get 21
                                                      local.set 8
                                                      local.get 10
                                                      i64.load offset=56
                                                      local.tee 35
                                                      i64.eqz
                                                      i32.eqz
                                                      if  ;; label = @26
                                                        loop  ;; label = @27
                                                          local.get 8
                                                          i32.const -1
                                                          i32.add
                                                          local.tee 8
                                                          local.get 35
                                                          i32.wrap_i64
                                                          i32.const 7
                                                          i32.and
                                                          i32.const 48
                                                          i32.or
                                                          i32.store8
                                                          local.get 35
                                                          i64.const 3
                                                          i64.shr_u
                                                          local.tee 35
                                                          i64.const 0
                                                          i64.ne
                                                          br_if 0 (;@27;)
                                                        end
                                                      end
                                                      i32.const 0
                                                      local.set 12
                                                      i32.const 2822
                                                      local.set 15
                                                      local.get 16
                                                      i32.const 8
                                                      i32.and
                                                      i32.eqz
                                                      br_if 2 (;@23;)
                                                      local.get 9
                                                      local.get 21
                                                      local.get 8
                                                      i32.sub
                                                      local.tee 5
                                                      i32.const 1
                                                      i32.add
                                                      local.get 9
                                                      local.get 5
                                                      i32.gt_s
                                                      select
                                                      local.set 9
                                                      br 2 (;@23;)
                                                    end
                                                    local.get 10
                                                    i64.load offset=56
                                                    local.tee 35
                                                    i64.const -1
                                                    i64.le_s
                                                    if  ;; label = @25
                                                      local.get 10
                                                      i64.const 0
                                                      local.get 35
                                                      i64.sub
                                                      local.tee 35
                                                      i64.store offset=56
                                                      i32.const 1
                                                      local.set 12
                                                      i32.const 2822
                                                      br 1 (;@24;)
                                                    end
                                                    local.get 16
                                                    i32.const 2048
                                                    i32.and
                                                    if  ;; label = @25
                                                      i32.const 1
                                                      local.set 12
                                                      i32.const 2823
                                                      br 1 (;@24;)
                                                    end
                                                    i32.const 2824
                                                    i32.const 2822
                                                    local.get 16
                                                    i32.const 1
                                                    i32.and
                                                    local.tee 12
                                                    select
                                                  end
                                                  local.set 15
                                                  block  ;; label = @24
                                                    local.get 35
                                                    i64.const 4294967296
                                                    i64.lt_u
                                                    if  ;; label = @25
                                                      local.get 35
                                                      local.set 36
                                                      local.get 21
                                                      local.set 8
                                                      br 1 (;@24;)
                                                    end
                                                    local.get 21
                                                    local.set 8
                                                    loop  ;; label = @25
                                                      local.get 8
                                                      i32.const -1
                                                      i32.add
                                                      local.tee 8
                                                      local.get 35
                                                      local.get 35
                                                      i64.const 10
                                                      i64.div_u
                                                      local.tee 36
                                                      i64.const 10
                                                      i64.mul
                                                      i64.sub
                                                      i32.wrap_i64
                                                      i32.const 48
                                                      i32.or
                                                      i32.store8
                                                      local.get 35
                                                      i64.const 42949672959
                                                      i64.gt_u
                                                      local.get 36
                                                      local.set 35
                                                      br_if 0 (;@25;)
                                                    end
                                                  end
                                                  local.get 36
                                                  i32.wrap_i64
                                                  local.tee 5
                                                  i32.eqz
                                                  br_if 0 (;@23;)
                                                  loop  ;; label = @24
                                                    local.get 8
                                                    i32.const -1
                                                    i32.add
                                                    local.tee 8
                                                    local.get 5
                                                    local.get 5
                                                    i32.const 10
                                                    i32.div_u
                                                    local.tee 6
                                                    i32.const 10
                                                    i32.mul
                                                    i32.sub
                                                    i32.const 48
                                                    i32.or
                                                    i32.store8
                                                    local.get 5
                                                    i32.const 9
                                                    i32.gt_u
                                                    local.get 6
                                                    local.set 5
                                                    br_if 0 (;@24;)
                                                  end
                                                end
                                                local.get 17
                                                i32.const 0
                                                local.get 9
                                                i32.const 0
                                                i32.lt_s
                                                select
                                                br_if 18 (;@4;)
                                                local.get 16
                                                i32.const -65537
                                                i32.and
                                                local.get 16
                                                local.get 17
                                                select
                                                local.set 16
                                                local.get 9
                                                local.get 10
                                                i64.load offset=56
                                                local.tee 35
                                                i64.eqz
                                                i32.eqz
                                                i32.or
                                                i32.eqz
                                                if  ;; label = @23
                                                  local.get 21
                                                  local.tee 8
                                                  local.set 5
                                                  i32.const 0
                                                  local.set 9
                                                  br 18 (;@5;)
                                                end
                                                local.get 9
                                                local.get 35
                                                i64.eqz
                                                local.get 21
                                                local.get 8
                                                i32.sub
                                                i32.add
                                                local.tee 5
                                                local.get 9
                                                local.get 5
                                                i32.gt_s
                                                select
                                                local.set 9
                                                br 10 (;@12;)
                                              end
                                              local.get 10
                                              local.get 10
                                              i64.load offset=56
                                              i64.store8 offset=55
                                              i32.const 0
                                              local.set 12
                                              i32.const 2822
                                              local.set 15
                                              i32.const 1
                                              local.set 9
                                              local.get 32
                                              local.set 8
                                              local.get 21
                                              local.set 5
                                              local.get 7
                                              local.set 16
                                              br 16 (;@5;)
                                            end
                                            i32.const 4256
                                            i32.load
                                            call 28
                                            br 1 (;@19;)
                                          end
                                          local.get 10
                                          i32.load offset=56
                                          local.tee 5
                                          i32.const 2832
                                          local.get 5
                                          select
                                        end
                                        local.set 8
                                        i32.const 0
                                        local.set 12
                                        local.get 8
                                        local.get 8
                                        i32.const 2147483647
                                        local.get 9
                                        local.get 9
                                        i32.const 0
                                        i32.lt_s
                                        select
                                        local.tee 5
                                        call 35
                                        local.tee 6
                                        local.get 8
                                        i32.sub
                                        local.get 5
                                        local.get 6
                                        select
                                        local.tee 6
                                        i32.add
                                        local.set 5
                                        i32.const 2822
                                        local.set 15
                                        local.get 9
                                        i32.const -1
                                        i32.le_s
                                        br_if 7 (;@11;)
                                        local.get 7
                                        local.set 16
                                        local.get 6
                                        local.set 9
                                        br 13 (;@5;)
                                      end
                                      local.get 10
                                      i32.load offset=56
                                      local.tee 8
                                      local.get 9
                                      br_if 1 (;@16;)
                                      drop
                                      i32.const 0
                                      local.set 5
                                      br 2 (;@15;)
                                    end
                                    local.get 10
                                    i32.const 0
                                    i32.store offset=12
                                    local.get 10
                                    local.get 10
                                    i64.load offset=56
                                    i64.store32 offset=8
                                    local.get 10
                                    local.get 10
                                    i32.const 8
                                    i32.add
                                    i32.store offset=56
                                    i32.const -1
                                    local.set 9
                                    local.get 10
                                    i32.const 8
                                    i32.add
                                  end
                                  local.set 8
                                  i32.const 0
                                  local.set 5
                                  local.get 8
                                  local.set 6
                                  block  ;; label = @16
                                    loop  ;; label = @17
                                      local.get 6
                                      i32.load
                                      local.tee 7
                                      i32.eqz
                                      br_if 1 (;@16;)
                                      local.get 10
                                      i32.const 4
                                      i32.add
                                      local.get 7
                                      call 37
                                      local.tee 7
                                      i32.const 0
                                      i32.lt_s
                                      local.tee 11
                                      local.get 7
                                      local.get 9
                                      local.get 5
                                      i32.sub
                                      i32.gt_u
                                      i32.or
                                      i32.eqz
                                      if  ;; label = @18
                                        local.get 6
                                        i32.const 4
                                        i32.add
                                        local.set 6
                                        local.get 9
                                        local.get 5
                                        local.get 7
                                        i32.add
                                        local.tee 5
                                        i32.gt_u
                                        br_if 1 (;@17;)
                                        br 2 (;@16;)
                                      end
                                    end
                                    local.get 11
                                    br_if 14 (;@2;)
                                  end
                                  local.get 5
                                  i32.const 0
                                  i32.lt_s
                                  br_if 11 (;@4;)
                                end
                                block  ;; label = @15
                                  local.get 16
                                  i32.const 73728
                                  i32.and
                                  local.tee 9
                                  local.get 14
                                  local.get 5
                                  i32.le_s
                                  i32.or
                                  br_if 0 (;@15;)
                                  local.get 10
                                  i32.const -64
                                  i32.sub
                                  i32.const 32
                                  local.get 14
                                  local.get 5
                                  i32.sub
                                  local.tee 15
                                  i32.const 256
                                  local.get 15
                                  i32.const 256
                                  i32.lt_u
                                  local.tee 7
                                  select
                                  call 33
                                  local.get 0
                                  i32.load
                                  local.tee 13
                                  i32.const 32
                                  i32.and
                                  local.set 6
                                  block  ;; label = @16
                                    local.get 7
                                    i32.eqz
                                    if  ;; label = @17
                                      local.get 6
                                      i32.eqz
                                      local.set 6
                                      local.get 15
                                      local.set 7
                                      loop  ;; label = @18
                                        local.get 6
                                        i32.const 1
                                        i32.and
                                        if  ;; label = @19
                                          local.get 10
                                          i32.const -64
                                          i32.sub
                                          i32.const 256
                                          local.get 0
                                          call 16
                                          local.get 0
                                          i32.load
                                          local.set 13
                                        end
                                        local.get 13
                                        i32.const 32
                                        i32.and
                                        local.tee 11
                                        i32.eqz
                                        local.set 6
                                        local.get 7
                                        i32.const -256
                                        i32.add
                                        local.tee 7
                                        i32.const 255
                                        i32.gt_u
                                        br_if 0 (;@18;)
                                      end
                                      local.get 11
                                      br_if 2 (;@15;)
                                      local.get 15
                                      i32.const 255
                                      i32.and
                                      local.set 15
                                      br 1 (;@16;)
                                    end
                                    local.get 6
                                    br_if 1 (;@15;)
                                  end
                                  local.get 10
                                  i32.const -64
                                  i32.sub
                                  local.get 15
                                  local.get 0
                                  call 16
                                end
                                block  ;; label = @15
                                  local.get 5
                                  i32.eqz
                                  br_if 0 (;@15;)
                                  i32.const 0
                                  local.set 6
                                  loop  ;; label = @16
                                    local.get 8
                                    i32.load
                                    local.tee 7
                                    i32.eqz
                                    br_if 1 (;@15;)
                                    local.get 10
                                    i32.const 4
                                    i32.add
                                    local.get 7
                                    call 37
                                    local.tee 7
                                    local.get 6
                                    i32.add
                                    local.tee 6
                                    local.get 5
                                    i32.gt_u
                                    br_if 1 (;@15;)
                                    local.get 0
                                    i32.load8_u
                                    i32.const 32
                                    i32.and
                                    i32.eqz
                                    if  ;; label = @17
                                      local.get 10
                                      i32.const 4
                                      i32.add
                                      local.get 7
                                      local.get 0
                                      call 16
                                    end
                                    local.get 8
                                    i32.const 4
                                    i32.add
                                    local.set 8
                                    local.get 6
                                    local.get 5
                                    i32.lt_u
                                    br_if 0 (;@16;)
                                  end
                                end
                                block  ;; label = @15
                                  local.get 9
                                  i32.const 8192
                                  i32.ne
                                  local.get 14
                                  local.get 5
                                  i32.le_s
                                  i32.or
                                  br_if 0 (;@15;)
                                  local.get 10
                                  i32.const -64
                                  i32.sub
                                  i32.const 32
                                  local.get 14
                                  local.get 5
                                  i32.sub
                                  local.tee 9
                                  i32.const 256
                                  local.get 9
                                  i32.const 256
                                  i32.lt_u
                                  local.tee 7
                                  select
                                  call 33
                                  local.get 0
                                  i32.load
                                  local.tee 8
                                  i32.const 32
                                  i32.and
                                  local.set 6
                                  block  ;; label = @16
                                    local.get 7
                                    i32.eqz
                                    if  ;; label = @17
                                      local.get 6
                                      i32.eqz
                                      local.set 6
                                      local.get 9
                                      local.set 7
                                      loop  ;; label = @18
                                        local.get 6
                                        i32.const 1
                                        i32.and
                                        if  ;; label = @19
                                          local.get 10
                                          i32.const -64
                                          i32.sub
                                          i32.const 256
                                          local.get 0
                                          call 16
                                          local.get 0
                                          i32.load
                                          local.set 8
                                        end
                                        local.get 8
                                        i32.const 32
                                        i32.and
                                        local.tee 11
                                        i32.eqz
                                        local.set 6
                                        local.get 7
                                        i32.const -256
                                        i32.add
                                        local.tee 7
                                        i32.const 255
                                        i32.gt_u
                                        br_if 0 (;@18;)
                                      end
                                      local.get 11
                                      br_if 2 (;@15;)
                                      local.get 9
                                      i32.const 255
                                      i32.and
                                      local.set 9
                                      br 1 (;@16;)
                                    end
                                    local.get 6
                                    br_if 1 (;@15;)
                                  end
                                  local.get 10
                                  i32.const -64
                                  i32.sub
                                  local.get 9
                                  local.get 0
                                  call 16
                                end
                                local.get 14
                                local.get 5
                                local.get 14
                                local.get 5
                                i32.gt_s
                                select
                                local.set 5
                                br 11 (;@3;)
                              end
                              local.get 9
                              i32.const -1
                              i32.le_s
                              i32.const 0
                              local.get 17
                              select
                              br_if 9 (;@4;)
                              local.get 10
                              f64.load offset=56
                              local.set 37
                              local.get 10
                              i32.const 0
                              i32.store offset=364
                              block (result i32)  ;; label = @14
                                local.get 37
                                i64.reinterpret_f64
                                i64.const -1
                                i64.le_s
                                if  ;; label = @15
                                  local.get 37
                                  f64.neg
                                  local.set 37
                                  i32.const 1
                                  local.set 19
                                  i32.const 3472
                                  br 1 (;@14;)
                                end
                                local.get 16
                                i32.const 2048
                                i32.and
                                if  ;; label = @15
                                  i32.const 1
                                  local.set 19
                                  i32.const 3475
                                  br 1 (;@14;)
                                end
                                i32.const 3478
                                i32.const 3473
                                local.get 16
                                i32.const 1
                                i32.and
                                local.tee 19
                                select
                              end
                              local.set 22
                              block  ;; label = @14
                                local.get 37
                                f64.abs
                                local.tee 38
                                f64.const inf (;=inf;)
                                f64.ne
                                local.get 38
                                local.get 38
                                f64.eq
                                i32.and
                                i32.eqz
                                if  ;; label = @15
                                  block  ;; label = @16
                                    local.get 16
                                    i32.const 8192
                                    i32.and
                                    local.get 14
                                    local.get 19
                                    i32.const 3
                                    i32.add
                                    local.tee 8
                                    i32.le_s
                                    i32.or
                                    br_if 0 (;@16;)
                                    local.get 10
                                    i32.const -64
                                    i32.sub
                                    i32.const 32
                                    local.get 14
                                    local.get 8
                                    i32.sub
                                    local.tee 9
                                    i32.const 256
                                    local.get 9
                                    i32.const 256
                                    i32.lt_u
                                    local.tee 6
                                    select
                                    call 33
                                    local.get 0
                                    i32.load
                                    local.tee 7
                                    i32.const 32
                                    i32.and
                                    local.set 5
                                    block  ;; label = @17
                                      local.get 6
                                      i32.eqz
                                      if  ;; label = @18
                                        local.get 5
                                        i32.eqz
                                        local.set 5
                                        local.get 9
                                        local.set 6
                                        loop  ;; label = @19
                                          local.get 5
                                          i32.const 1
                                          i32.and
                                          if  ;; label = @20
                                            local.get 10
                                            i32.const -64
                                            i32.sub
                                            i32.const 256
                                            local.get 0
                                            call 16
                                            local.get 0
                                            i32.load
                                            local.set 7
                                          end
                                          local.get 7
                                          i32.const 32
                                          i32.and
                                          local.tee 12
                                          i32.eqz
                                          local.set 5
                                          local.get 6
                                          i32.const -256
                                          i32.add
                                          local.tee 6
                                          i32.const 255
                                          i32.gt_u
                                          br_if 0 (;@19;)
                                        end
                                        local.get 12
                                        br_if 2 (;@16;)
                                        local.get 9
                                        i32.const 255
                                        i32.and
                                        local.set 9
                                        br 1 (;@17;)
                                      end
                                      local.get 5
                                      br_if 1 (;@16;)
                                    end
                                    local.get 10
                                    i32.const -64
                                    i32.sub
                                    local.get 9
                                    local.get 0
                                    call 16
                                  end
                                  local.get 0
                                  i32.load
                                  local.tee 5
                                  i32.const 32
                                  i32.and
                                  if (result i32)  ;; label = @16
                                    local.get 5
                                  else
                                    local.get 22
                                    local.get 19
                                    local.get 0
                                    call 16
                                    local.get 0
                                    i32.load
                                  end
                                  i32.const 32
                                  i32.and
                                  i32.eqz
                                  if  ;; label = @16
                                    i32.const 3499
                                    i32.const 3503
                                    local.get 11
                                    i32.const 32
                                    i32.and
                                    i32.const 5
                                    i32.shr_u
                                    local.tee 5
                                    select
                                    i32.const 3491
                                    i32.const 3495
                                    local.get 5
                                    select
                                    local.get 37
                                    local.get 37
                                    f64.ne
                                    select
                                    i32.const 3
                                    local.get 0
                                    call 16
                                  end
                                  block  ;; label = @16
                                    local.get 16
                                    i32.const 73728
                                    i32.and
                                    i32.const 8192
                                    i32.ne
                                    local.get 14
                                    local.get 8
                                    i32.le_s
                                    i32.or
                                    br_if 0 (;@16;)
                                    local.get 10
                                    i32.const -64
                                    i32.sub
                                    i32.const 32
                                    local.get 14
                                    local.get 8
                                    i32.sub
                                    local.tee 9
                                    i32.const 256
                                    local.get 9
                                    i32.const 256
                                    i32.lt_u
                                    local.tee 6
                                    select
                                    call 33
                                    local.get 0
                                    i32.load
                                    local.tee 7
                                    i32.const 32
                                    i32.and
                                    local.set 5
                                    block  ;; label = @17
                                      local.get 6
                                      i32.eqz
                                      if  ;; label = @18
                                        local.get 5
                                        i32.eqz
                                        local.set 5
                                        local.get 9
                                        local.set 6
                                        loop  ;; label = @19
                                          local.get 5
                                          i32.const 1
                                          i32.and
                                          if  ;; label = @20
                                            local.get 10
                                            i32.const -64
                                            i32.sub
                                            i32.const 256
                                            local.get 0
                                            call 16
                                            local.get 0
                                            i32.load
                                            local.set 7
                                          end
                                          local.get 7
                                          i32.const 32
                                          i32.and
                                          local.tee 11
                                          i32.eqz
                                          local.set 5
                                          local.get 6
                                          i32.const -256
                                          i32.add
                                          local.tee 6
                                          i32.const 255
                                          i32.gt_u
                                          br_if 0 (;@19;)
                                        end
                                        local.get 11
                                        br_if 2 (;@16;)
                                        local.get 9
                                        i32.const 255
                                        i32.and
                                        local.set 9
                                        br 1 (;@17;)
                                      end
                                      local.get 5
                                      br_if 1 (;@16;)
                                    end
                                    local.get 10
                                    i32.const -64
                                    i32.sub
                                    local.get 9
                                    local.get 0
                                    call 16
                                  end
                                  local.get 14
                                  local.get 8
                                  local.get 14
                                  local.get 8
                                  i32.gt_s
                                  select
                                  local.set 5
                                  br 1 (;@14;)
                                end
                                local.get 37
                                local.get 10
                                i32.const 364
                                i32.add
                                call 38
                                local.tee 37
                                local.get 37
                                f64.add
                                local.tee 37
                                f64.const 0x0p+0 (;=0;)
                                f64.ne
                                if  ;; label = @15
                                  local.get 10
                                  local.get 10
                                  i32.load offset=364
                                  i32.const -1
                                  i32.add
                                  i32.store offset=364
                                end
                                local.get 11
                                i32.const 32
                                i32.or
                                local.tee 13
                                i32.const 97
                                i32.eq
                                if  ;; label = @15
                                  local.get 22
                                  i32.const 9
                                  i32.add
                                  local.get 22
                                  local.get 11
                                  i32.const 32
                                  i32.and
                                  local.tee 12
                                  select
                                  local.set 13
                                  block  ;; label = @16
                                    i32.const 12
                                    local.get 9
                                    i32.sub
                                    i32.eqz
                                    local.get 9
                                    i32.const 11
                                    i32.gt_u
                                    i32.or
                                    br_if 0 (;@16;)
                                    local.get 9
                                    i32.const -12
                                    i32.add
                                    local.set 5
                                    f64.const 0x1p+4 (;=16;)
                                    local.set 38
                                    loop  ;; label = @17
                                      local.get 38
                                      f64.const 0x1p+4 (;=16;)
                                      f64.mul
                                      local.set 38
                                      local.get 5
                                      i32.const 1
                                      i32.add
                                      local.tee 6
                                      local.get 5
                                      i32.ge_u
                                      local.get 6
                                      local.set 5
                                      br_if 0 (;@17;)
                                    end
                                    local.get 13
                                    i32.load8_u
                                    i32.const 45
                                    i32.eq
                                    if  ;; label = @17
                                      local.get 38
                                      local.get 37
                                      f64.neg
                                      local.get 38
                                      f64.sub
                                      f64.add
                                      f64.neg
                                      local.set 37
                                      br 1 (;@16;)
                                    end
                                    local.get 37
                                    local.get 38
                                    f64.add
                                    local.get 38
                                    f64.sub
                                    local.set 37
                                  end
                                  local.get 25
                                  local.set 7
                                  block  ;; label = @16
                                    local.get 10
                                    i32.load offset=364
                                    local.tee 8
                                    local.get 8
                                    i32.const 31
                                    i32.shr_s
                                    local.tee 5
                                    i32.add
                                    local.get 5
                                    i32.xor
                                    local.tee 5
                                    if  ;; label = @17
                                      i32.const 0
                                      local.set 6
                                      loop  ;; label = @18
                                        local.get 6
                                        local.get 10
                                        i32.add
                                        i32.const 335
                                        i32.add
                                        local.get 5
                                        local.get 5
                                        i32.const 10
                                        i32.div_u
                                        local.tee 7
                                        i32.const 10
                                        i32.mul
                                        i32.sub
                                        i32.const 48
                                        i32.or
                                        i32.store8
                                        local.get 6
                                        i32.const -1
                                        i32.add
                                        local.set 6
                                        local.get 5
                                        i32.const 9
                                        i32.gt_u
                                        local.get 7
                                        local.set 5
                                        br_if 0 (;@18;)
                                      end
                                      local.get 6
                                      local.get 10
                                      i32.add
                                      i32.const 336
                                      i32.add
                                      local.set 7
                                      local.get 6
                                      br_if 1 (;@16;)
                                    end
                                    local.get 7
                                    i32.const -1
                                    i32.add
                                    local.tee 7
                                    i32.const 48
                                    i32.store8
                                  end
                                  local.get 19
                                  i32.const 2
                                  i32.or
                                  local.set 15
                                  local.get 7
                                  i32.const -2
                                  i32.add
                                  local.tee 17
                                  local.get 11
                                  i32.const 15
                                  i32.add
                                  i32.store8
                                  local.get 7
                                  i32.const -1
                                  i32.add
                                  i32.const 45
                                  i32.const 43
                                  local.get 8
                                  i32.const 0
                                  i32.lt_s
                                  select
                                  i32.store8
                                  local.get 16
                                  i32.const 8
                                  i32.and
                                  local.set 7
                                  local.get 10
                                  i32.const 336
                                  i32.add
                                  local.set 6
                                  loop  ;; label = @16
                                    local.get 6
                                    local.tee 5
                                    block (result i32)  ;; label = @17
                                      local.get 37
                                      f64.abs
                                      f64.const 0x1p+31 (;=2.14748e+09;)
                                      f64.lt
                                      if  ;; label = @18
                                        local.get 37
                                        i32.trunc_f64_s
                                        br 1 (;@17;)
                                      end
                                      i32.const -2147483648
                                    end
                                    local.tee 8
                                    i32.const 3456
                                    i32.add
                                    i32.load8_u
                                    local.get 12
                                    i32.or
                                    i32.store8
                                    local.get 5
                                    i32.const 1
                                    i32.add
                                    local.tee 6
                                    local.get 10
                                    i32.const 336
                                    i32.add
                                    i32.sub
                                    i32.const 1
                                    i32.ne
                                    local.get 7
                                    local.get 9
                                    i32.const 0
                                    i32.gt_s
                                    i32.or
                                    i32.eqz
                                    i32.const 0
                                    local.get 37
                                    local.get 8
                                    f64.convert_i32_s
                                    f64.sub
                                    f64.const 0x1p+4 (;=16;)
                                    f64.mul
                                    local.tee 37
                                    f64.const 0x0p+0 (;=0;)
                                    f64.eq
                                    select
                                    i32.or
                                    i32.eqz
                                    if  ;; label = @17
                                      local.get 5
                                      i32.const 46
                                      i32.store8 offset=1
                                      local.get 5
                                      i32.const 2
                                      i32.add
                                      local.set 6
                                    end
                                    local.get 37
                                    f64.const 0x0p+0 (;=0;)
                                    f64.ne
                                    br_if 0 (;@16;)
                                  end
                                  i32.const -1
                                  local.set 5
                                  i32.const 2147483645
                                  local.get 15
                                  local.get 25
                                  local.get 17
                                  i32.sub
                                  local.tee 19
                                  i32.add
                                  local.tee 7
                                  i32.sub
                                  local.get 9
                                  i32.lt_s
                                  br_if 1 (;@14;)
                                  block  ;; label = @16
                                    local.get 16
                                    i32.const 73728
                                    i32.and
                                    local.tee 16
                                    local.get 14
                                    local.get 7
                                    local.get 9
                                    i32.const 2
                                    i32.add
                                    local.get 6
                                    local.get 10
                                    i32.const 336
                                    i32.add
                                    i32.sub
                                    local.tee 12
                                    local.get 6
                                    local.get 33
                                    i32.add
                                    local.get 9
                                    i32.lt_s
                                    select
                                    local.get 12
                                    local.get 9
                                    select
                                    local.tee 9
                                    i32.add
                                    local.tee 8
                                    i32.le_s
                                    i32.or
                                    br_if 0 (;@16;)
                                    local.get 10
                                    i32.const -64
                                    i32.sub
                                    i32.const 32
                                    local.get 14
                                    local.get 8
                                    i32.sub
                                    local.tee 11
                                    i32.const 256
                                    local.get 11
                                    i32.const 256
                                    i32.lt_u
                                    local.tee 6
                                    select
                                    call 33
                                    local.get 0
                                    i32.load
                                    local.tee 7
                                    i32.const 32
                                    i32.and
                                    local.set 5
                                    block  ;; label = @17
                                      local.get 6
                                      i32.eqz
                                      if  ;; label = @18
                                        local.get 5
                                        i32.eqz
                                        local.set 5
                                        local.get 11
                                        local.set 6
                                        loop  ;; label = @19
                                          local.get 5
                                          i32.const 1
                                          i32.and
                                          if  ;; label = @20
                                            local.get 10
                                            i32.const -64
                                            i32.sub
                                            i32.const 256
                                            local.get 0
                                            call 16
                                            local.get 0
                                            i32.load
                                            local.set 7
                                          end
                                          local.get 7
                                          i32.const 32
                                          i32.and
                                          local.tee 22
                                          i32.eqz
                                          local.set 5
                                          local.get 6
                                          i32.const -256
                                          i32.add
                                          local.tee 6
                                          i32.const 255
                                          i32.gt_u
                                          br_if 0 (;@19;)
                                        end
                                        local.get 22
                                        br_if 2 (;@16;)
                                        local.get 11
                                        i32.const 255
                                        i32.and
                                        local.set 11
                                        br 1 (;@17;)
                                      end
                                      local.get 5
                                      br_if 1 (;@16;)
                                    end
                                    local.get 10
                                    i32.const -64
                                    i32.sub
                                    local.get 11
                                    local.get 0
                                    call 16
                                  end
                                  local.get 0
                                  i32.load8_u
                                  i32.const 32
                                  i32.and
                                  i32.eqz
                                  if  ;; label = @16
                                    local.get 13
                                    local.get 15
                                    local.get 0
                                    call 16
                                  end
                                  block  ;; label = @16
                                    local.get 16
                                    i32.const 65536
                                    i32.ne
                                    local.get 14
                                    local.get 8
                                    i32.le_s
                                    i32.or
                                    br_if 0 (;@16;)
                                    local.get 10
                                    i32.const -64
                                    i32.sub
                                    i32.const 48
                                    local.get 14
                                    local.get 8
                                    i32.sub
                                    local.tee 15
                                    i32.const 256
                                    local.get 15
                                    i32.const 256
                                    i32.lt_u
                                    local.tee 6
                                    select
                                    call 33
                                    local.get 0
                                    i32.load
                                    local.tee 7
                                    i32.const 32
                                    i32.and
                                    local.set 5
                                    block  ;; label = @17
                                      local.get 6
                                      i32.eqz
                                      if  ;; label = @18
                                        local.get 5
                                        i32.eqz
                                        local.set 5
                                        local.get 15
                                        local.set 6
                                        loop  ;; label = @19
                                          local.get 5
                                          i32.const 1
                                          i32.and
                                          if  ;; label = @20
                                            local.get 10
                                            i32.const -64
                                            i32.sub
                                            i32.const 256
                                            local.get 0
                                            call 16
                                            local.get 0
                                            i32.load
                                            local.set 7
                                          end
                                          local.get 7
                                          i32.const 32
                                          i32.and
                                          local.tee 11
                                          i32.eqz
                                          local.set 5
                                          local.get 6
                                          i32.const -256
                                          i32.add
                                          local.tee 6
                                          i32.const 255
                                          i32.gt_u
                                          br_if 0 (;@19;)
                                        end
                                        local.get 11
                                        br_if 2 (;@16;)
                                        local.get 15
                                        i32.const 255
                                        i32.and
                                        local.set 15
                                        br 1 (;@17;)
                                      end
                                      local.get 5
                                      br_if 1 (;@16;)
                                    end
                                    local.get 10
                                    i32.const -64
                                    i32.sub
                                    local.get 15
                                    local.get 0
                                    call 16
                                  end
                                  local.get 0
                                  i32.load8_u
                                  i32.const 32
                                  i32.and
                                  i32.eqz
                                  if  ;; label = @16
                                    local.get 10
                                    i32.const 336
                                    i32.add
                                    local.get 12
                                    local.get 0
                                    call 16
                                  end
                                  block  ;; label = @16
                                    local.get 9
                                    local.get 12
                                    i32.sub
                                    local.tee 9
                                    i32.const 1
                                    i32.lt_s
                                    br_if 0 (;@16;)
                                    local.get 10
                                    i32.const -64
                                    i32.sub
                                    i32.const 48
                                    local.get 9
                                    i32.const 256
                                    local.get 9
                                    i32.const 256
                                    i32.lt_u
                                    local.tee 6
                                    select
                                    call 33
                                    local.get 0
                                    i32.load
                                    local.tee 7
                                    i32.const 32
                                    i32.and
                                    local.set 5
                                    block  ;; label = @17
                                      local.get 6
                                      i32.eqz
                                      if  ;; label = @18
                                        local.get 5
                                        i32.eqz
                                        local.set 5
                                        local.get 9
                                        local.set 6
                                        loop  ;; label = @19
                                          local.get 5
                                          i32.const 1
                                          i32.and
                                          if  ;; label = @20
                                            local.get 10
                                            i32.const -64
                                            i32.sub
                                            i32.const 256
                                            local.get 0
                                            call 16
                                            local.get 0
                                            i32.load
                                            local.set 7
                                          end
                                          local.get 7
                                          i32.const 32
                                          i32.and
                                          local.tee 11
                                          i32.eqz
                                          local.set 5
                                          local.get 6
                                          i32.const -256
                                          i32.add
                                          local.tee 6
                                          i32.const 255
                                          i32.gt_u
                                          br_if 0 (;@19;)
                                        end
                                        local.get 11
                                        br_if 2 (;@16;)
                                        local.get 9
                                        i32.const 255
                                        i32.and
                                        local.set 9
                                        br 1 (;@17;)
                                      end
                                      local.get 5
                                      br_if 1 (;@16;)
                                    end
                                    local.get 10
                                    i32.const -64
                                    i32.sub
                                    local.get 9
                                    local.get 0
                                    call 16
                                  end
                                  local.get 0
                                  i32.load8_u
                                  i32.const 32
                                  i32.and
                                  i32.eqz
                                  if  ;; label = @16
                                    local.get 17
                                    local.get 19
                                    local.get 0
                                    call 16
                                  end
                                  block  ;; label = @16
                                    local.get 16
                                    i32.const 8192
                                    i32.ne
                                    local.get 14
                                    local.get 8
                                    i32.le_s
                                    i32.or
                                    br_if 0 (;@16;)
                                    local.get 10
                                    i32.const -64
                                    i32.sub
                                    i32.const 32
                                    local.get 14
                                    local.get 8
                                    i32.sub
                                    local.tee 9
                                    i32.const 256
                                    local.get 9
                                    i32.const 256
                                    i32.lt_u
                                    local.tee 6
                                    select
                                    call 33
                                    local.get 0
                                    i32.load
                                    local.tee 7
                                    i32.const 32
                                    i32.and
                                    local.set 5
                                    block  ;; label = @17
                                      local.get 6
                                      i32.eqz
                                      if  ;; label = @18
                                        local.get 5
                                        i32.eqz
                                        local.set 5
                                        local.get 9
                                        local.set 6
                                        loop  ;; label = @19
                                          local.get 5
                                          i32.const 1
                                          i32.and
                                          if  ;; label = @20
                                            local.get 10
                                            i32.const -64
                                            i32.sub
                                            i32.const 256
                                            local.get 0
                                            call 16
                                            local.get 0
                                            i32.load
                                            local.set 7
                                          end
                                          local.get 7
                                          i32.const 32
                                          i32.and
                                          local.tee 11
                                          i32.eqz
                                          local.set 5
                                          local.get 6
                                          i32.const -256
                                          i32.add
                                          local.tee 6
                                          i32.const 255
                                          i32.gt_u
                                          br_if 0 (;@19;)
                                        end
                                        local.get 11
                                        br_if 2 (;@16;)
                                        local.get 9
                                        i32.const 255
                                        i32.and
                                        local.set 9
                                        br 1 (;@17;)
                                      end
                                      local.get 5
                                      br_if 1 (;@16;)
                                    end
                                    local.get 10
                                    i32.const -64
                                    i32.sub
                                    local.get 9
                                    local.get 0
                                    call 16
                                  end
                                  local.get 14
                                  local.get 8
                                  local.get 14
                                  local.get 8
                                  i32.gt_s
                                  select
                                  local.set 5
                                  br 1 (;@14;)
                                end
                                local.get 9
                                i32.const 0
                                i32.lt_s
                                local.set 5
                                block  ;; label = @15
                                  local.get 37
                                  f64.const 0x0p+0 (;=0;)
                                  f64.eq
                                  if  ;; label = @16
                                    local.get 10
                                    i32.load offset=364
                                    local.set 8
                                    br 1 (;@15;)
                                  end
                                  local.get 10
                                  local.get 10
                                  i32.load offset=364
                                  i32.const -28
                                  i32.add
                                  local.tee 8
                                  i32.store offset=364
                                  local.get 37
                                  f64.const 0x1p+28 (;=2.68435e+08;)
                                  f64.mul
                                  local.set 37
                                end
                                i32.const 6
                                local.get 9
                                local.get 5
                                select
                                local.set 12
                                local.get 10
                                i32.const 368
                                i32.add
                                local.get 34
                                local.get 8
                                i32.const 0
                                i32.lt_s
                                select
                                local.tee 17
                                local.set 7
                                loop  ;; label = @15
                                  local.get 7
                                  block (result i32)  ;; label = @16
                                    local.get 37
                                    f64.const 0x1p+32 (;=4.29497e+09;)
                                    f64.lt
                                    local.get 37
                                    f64.const 0x0p+0 (;=0;)
                                    f64.ge
                                    i32.and
                                    if  ;; label = @17
                                      local.get 37
                                      i32.trunc_f64_u
                                      br 1 (;@16;)
                                    end
                                    i32.const 0
                                  end
                                  local.tee 5
                                  i32.store
                                  local.get 7
                                  i32.const 4
                                  i32.add
                                  local.set 7
                                  local.get 37
                                  local.get 5
                                  f64.convert_i32_u
                                  f64.sub
                                  f64.const 0x1.dcd65p+29 (;=1e+09;)
                                  f64.mul
                                  local.tee 37
                                  f64.const 0x0p+0 (;=0;)
                                  f64.ne
                                  br_if 0 (;@15;)
                                end
                                block  ;; label = @15
                                  local.get 8
                                  i32.const 1
                                  i32.lt_s
                                  if  ;; label = @16
                                    local.get 7
                                    local.set 5
                                    local.get 17
                                    local.set 6
                                    br 1 (;@15;)
                                  end
                                  local.get 17
                                  local.set 6
                                  loop  ;; label = @16
                                    local.get 8
                                    i32.const 29
                                    local.get 8
                                    i32.const 29
                                    i32.lt_s
                                    select
                                    local.set 8
                                    block  ;; label = @17
                                      local.get 7
                                      i32.const -4
                                      i32.add
                                      local.tee 5
                                      local.get 6
                                      i32.lt_u
                                      br_if 0 (;@17;)
                                      local.get 8
                                      i64.extend_i32_u
                                      local.set 36
                                      i64.const 0
                                      local.set 35
                                      loop  ;; label = @18
                                        local.get 5
                                        local.get 35
                                        i64.const 4294967295
                                        i64.and
                                        local.get 5
                                        i64.load32_u
                                        local.get 36
                                        i64.shl
                                        i64.add
                                        local.tee 35
                                        local.get 35
                                        i64.const 1000000000
                                        i64.div_u
                                        local.tee 35
                                        i64.const 1000000000
                                        i64.mul
                                        i64.sub
                                        i64.store32
                                        local.get 5
                                        i32.const -4
                                        i32.add
                                        local.tee 5
                                        local.get 6
                                        i32.ge_u
                                        br_if 0 (;@18;)
                                      end
                                      local.get 35
                                      i32.wrap_i64
                                      local.tee 5
                                      i32.eqz
                                      br_if 0 (;@17;)
                                      local.get 6
                                      i32.const -4
                                      i32.add
                                      local.tee 6
                                      local.get 5
                                      i32.store
                                    end
                                    loop  ;; label = @17
                                      local.get 7
                                      local.tee 5
                                      local.get 6
                                      i32.gt_u
                                      if  ;; label = @18
                                        local.get 5
                                        i32.const -4
                                        i32.add
                                        local.tee 7
                                        i32.load
                                        i32.eqz
                                        br_if 1 (;@17;)
                                      end
                                    end
                                    local.get 10
                                    local.get 10
                                    i32.load offset=364
                                    local.get 8
                                    i32.sub
                                    local.tee 8
                                    i32.store offset=364
                                    local.get 5
                                    local.set 7
                                    local.get 8
                                    i32.const 0
                                    i32.gt_s
                                    br_if 0 (;@16;)
                                  end
                                end
                                local.get 8
                                i32.const -1
                                i32.le_s
                                if  ;; label = @15
                                  local.get 12
                                  i32.const 25
                                  i32.add
                                  i32.const 9
                                  i32.div_u
                                  i32.const 1
                                  i32.add
                                  local.set 15
                                  loop  ;; label = @16
                                    i32.const 9
                                    i32.const 0
                                    local.get 8
                                    i32.sub
                                    local.get 8
                                    i32.const -9
                                    i32.lt_s
                                    select
                                    local.set 9
                                    block  ;; label = @17
                                      local.get 6
                                      local.get 5
                                      i32.ge_u
                                      if  ;; label = @18
                                        local.get 6
                                        local.get 6
                                        i32.const 4
                                        i32.add
                                        local.get 6
                                        i32.load
                                        select
                                        local.set 6
                                        br 1 (;@17;)
                                      end
                                      i32.const 1000000000
                                      local.get 9
                                      i32.shr_u
                                      local.set 20
                                      i32.const -1
                                      local.get 9
                                      i32.shl
                                      i32.const -1
                                      i32.xor
                                      local.set 23
                                      i32.const 0
                                      local.set 8
                                      local.get 6
                                      local.set 7
                                      loop  ;; label = @18
                                        local.get 7
                                        local.get 8
                                        local.get 7
                                        i32.load
                                        local.tee 24
                                        local.get 9
                                        i32.shr_u
                                        i32.add
                                        i32.store
                                        local.get 23
                                        local.get 24
                                        i32.and
                                        local.get 20
                                        i32.mul
                                        local.set 8
                                        local.get 7
                                        i32.const 4
                                        i32.add
                                        local.tee 7
                                        local.get 5
                                        i32.lt_u
                                        br_if 0 (;@18;)
                                      end
                                      local.get 6
                                      local.get 6
                                      i32.const 4
                                      i32.add
                                      local.get 6
                                      i32.load
                                      select
                                      local.set 6
                                      local.get 8
                                      i32.eqz
                                      br_if 0 (;@17;)
                                      local.get 5
                                      local.get 8
                                      i32.store
                                      local.get 5
                                      i32.const 4
                                      i32.add
                                      local.set 5
                                    end
                                    local.get 10
                                    local.get 10
                                    i32.load offset=364
                                    local.get 9
                                    i32.add
                                    local.tee 8
                                    i32.store offset=364
                                    local.get 17
                                    local.get 6
                                    local.get 13
                                    i32.const 102
                                    i32.eq
                                    select
                                    local.tee 7
                                    local.get 15
                                    i32.const 2
                                    i32.shl
                                    i32.add
                                    local.get 5
                                    local.get 5
                                    local.get 7
                                    i32.sub
                                    i32.const 2
                                    i32.shr_s
                                    local.get 15
                                    i32.gt_s
                                    select
                                    local.set 5
                                    local.get 8
                                    i32.const 0
                                    i32.lt_s
                                    br_if 0 (;@16;)
                                  end
                                end
                                i32.const 0
                                local.set 7
                                block  ;; label = @15
                                  local.get 6
                                  local.get 5
                                  i32.ge_u
                                  br_if 0 (;@15;)
                                  local.get 17
                                  local.get 6
                                  i32.sub
                                  i32.const 2
                                  i32.shr_s
                                  i32.const 9
                                  i32.mul
                                  local.set 7
                                  local.get 6
                                  i32.load
                                  local.tee 9
                                  i32.const 10
                                  i32.lt_u
                                  br_if 0 (;@15;)
                                  i32.const 10
                                  local.set 8
                                  loop  ;; label = @16
                                    local.get 7
                                    i32.const 1
                                    i32.add
                                    local.set 7
                                    local.get 9
                                    local.get 8
                                    i32.const 10
                                    i32.mul
                                    local.tee 8
                                    i32.ge_u
                                    br_if 0 (;@16;)
                                  end
                                end
                                local.get 12
                                i32.const 0
                                local.get 7
                                local.get 13
                                i32.const 102
                                i32.eq
                                select
                                local.tee 15
                                i32.sub
                                local.get 13
                                i32.const 103
                                i32.eq
                                local.tee 20
                                local.get 12
                                i32.const 0
                                i32.ne
                                i32.and
                                local.tee 13
                                i32.sub
                                local.tee 8
                                local.get 5
                                local.get 17
                                i32.sub
                                i32.const 2
                                i32.shr_s
                                i32.const 9
                                i32.mul
                                i32.const -9
                                i32.add
                                i32.lt_s
                                if  ;; label = @15
                                  local.get 8
                                  i32.const 9216
                                  i32.add
                                  local.tee 23
                                  i32.const 9
                                  i32.div_s
                                  local.tee 24
                                  i32.const 2
                                  i32.shl
                                  local.get 17
                                  i32.add
                                  local.tee 29
                                  i32.const -4092
                                  i32.add
                                  local.set 9
                                  i32.const 10
                                  local.set 8
                                  local.get 23
                                  local.get 24
                                  i32.const 9
                                  i32.mul
                                  local.tee 23
                                  i32.sub
                                  i32.const 7
                                  i32.le_s
                                  if  ;; label = @16
                                    local.get 12
                                    local.get 13
                                    i32.sub
                                    local.get 15
                                    i32.sub
                                    local.get 23
                                    i32.sub
                                    i32.const 9215
                                    i32.add
                                    local.set 13
                                    loop  ;; label = @17
                                      local.get 8
                                      i32.const 10
                                      i32.mul
                                      local.set 8
                                      local.get 13
                                      i32.const 1
                                      i32.add
                                      local.tee 13
                                      i32.const 7
                                      i32.lt_s
                                      br_if 0 (;@17;)
                                    end
                                  end
                                  block  ;; label = @16
                                    i32.const 0
                                    local.get 5
                                    local.get 9
                                    i32.const 4
                                    i32.add
                                    local.tee 23
                                    i32.eq
                                    local.get 9
                                    i32.load
                                    local.tee 15
                                    local.get 15
                                    local.get 8
                                    i32.div_u
                                    local.tee 24
                                    local.get 8
                                    i32.mul
                                    i32.sub
                                    local.tee 13
                                    select
                                    br_if 0 (;@16;)
                                    block  ;; label = @17
                                      local.get 24
                                      i32.const 1
                                      i32.and
                                      i32.eqz
                                      if  ;; label = @18
                                        f64.const 0x1p+53 (;=9.0072e+15;)
                                        local.set 37
                                        local.get 8
                                        i32.const 1000000000
                                        i32.ne
                                        local.get 9
                                        local.get 6
                                        i32.le_u
                                        i32.or
                                        br_if 1 (;@17;)
                                        local.get 9
                                        i32.const -4
                                        i32.add
                                        i32.load8_u
                                        i32.const 1
                                        i32.and
                                        i32.eqz
                                        br_if 1 (;@17;)
                                      end
                                      f64.const 0x1.0000000000001p+53 (;=9.0072e+15;)
                                      local.set 37
                                    end
                                    f64.const 0x1p-1 (;=0.5;)
                                    f64.const 0x1p+0 (;=1;)
                                    f64.const 0x1.8p+0 (;=1.5;)
                                    local.get 13
                                    local.get 8
                                    i32.const 1
                                    i32.shr_u
                                    local.tee 24
                                    i32.eq
                                    select
                                    f64.const 0x1.8p+0 (;=1.5;)
                                    local.get 5
                                    local.get 23
                                    i32.eq
                                    select
                                    local.get 13
                                    local.get 24
                                    i32.lt_u
                                    select
                                    local.set 38
                                    block  ;; label = @17
                                      local.get 19
                                      i32.eqz
                                      br_if 0 (;@17;)
                                      local.get 22
                                      i32.load8_u
                                      i32.const 45
                                      i32.ne
                                      br_if 0 (;@17;)
                                      local.get 38
                                      f64.neg
                                      local.set 38
                                      local.get 37
                                      f64.neg
                                      local.set 37
                                    end
                                    local.get 9
                                    local.get 15
                                    local.get 13
                                    i32.sub
                                    local.tee 13
                                    i32.store
                                    local.get 37
                                    local.get 38
                                    f64.add
                                    local.get 37
                                    f64.eq
                                    br_if 0 (;@16;)
                                    local.get 9
                                    local.get 8
                                    local.get 13
                                    i32.add
                                    local.tee 7
                                    i32.store
                                    local.get 7
                                    i32.const 1000000000
                                    i32.ge_u
                                    if  ;; label = @17
                                      local.get 29
                                      i32.const -4096
                                      i32.add
                                      local.set 7
                                      loop  ;; label = @18
                                        local.get 7
                                        i32.const 4
                                        i32.add
                                        i32.const 0
                                        i32.store
                                        local.get 7
                                        local.get 6
                                        i32.lt_u
                                        if  ;; label = @19
                                          local.get 6
                                          i32.const -4
                                          i32.add
                                          local.tee 6
                                          i32.const 0
                                          i32.store
                                        end
                                        local.get 7
                                        local.get 7
                                        i32.load
                                        i32.const 1
                                        i32.add
                                        local.tee 8
                                        i32.store
                                        local.get 7
                                        i32.const -4
                                        i32.add
                                        local.set 7
                                        local.get 8
                                        i32.const 999999999
                                        i32.gt_u
                                        br_if 0 (;@18;)
                                      end
                                      local.get 7
                                      i32.const 4
                                      i32.add
                                      local.set 9
                                    end
                                    local.get 17
                                    local.get 6
                                    i32.sub
                                    i32.const 2
                                    i32.shr_s
                                    i32.const 9
                                    i32.mul
                                    local.set 7
                                    local.get 6
                                    i32.load
                                    local.tee 13
                                    i32.const 10
                                    i32.lt_u
                                    br_if 0 (;@16;)
                                    i32.const 10
                                    local.set 8
                                    loop  ;; label = @17
                                      local.get 7
                                      i32.const 1
                                      i32.add
                                      local.set 7
                                      local.get 13
                                      local.get 8
                                      i32.const 10
                                      i32.mul
                                      local.tee 8
                                      i32.ge_u
                                      br_if 0 (;@17;)
                                    end
                                  end
                                  local.get 9
                                  i32.const 4
                                  i32.add
                                  local.tee 8
                                  local.get 5
                                  local.get 5
                                  local.get 8
                                  i32.gt_u
                                  select
                                  local.set 5
                                end
                                block (result i32)  ;; label = @15
                                  loop  ;; label = @16
                                    i32.const 0
                                    local.get 5
                                    local.tee 13
                                    local.get 6
                                    i32.le_u
                                    br_if 1 (;@15;)
                                    drop
                                    local.get 13
                                    i32.const -4
                                    i32.add
                                    local.tee 5
                                    i32.load
                                    i32.eqz
                                    br_if 0 (;@16;)
                                  end
                                  i32.const 1
                                end
                                local.set 23
                                block  ;; label = @15
                                  local.get 20
                                  i32.eqz
                                  if  ;; label = @16
                                    local.get 16
                                    i32.const 8
                                    i32.and
                                    local.set 15
                                    br 1 (;@15;)
                                  end
                                  local.get 7
                                  i32.const -1
                                  i32.xor
                                  i32.const -1
                                  local.get 12
                                  i32.const 1
                                  local.get 12
                                  select
                                  local.tee 5
                                  local.get 7
                                  i32.gt_s
                                  local.get 7
                                  i32.const -5
                                  i32.gt_s
                                  i32.and
                                  local.tee 8
                                  select
                                  local.get 5
                                  i32.add
                                  local.set 12
                                  i32.const -1
                                  i32.const -2
                                  local.get 8
                                  select
                                  local.get 11
                                  i32.add
                                  local.set 11
                                  local.get 16
                                  i32.const 8
                                  i32.and
                                  local.tee 15
                                  br_if 0 (;@15;)
                                  i32.const 9
                                  local.set 5
                                  block  ;; label = @16
                                    local.get 23
                                    i32.eqz
                                    br_if 0 (;@16;)
                                    local.get 13
                                    i32.const -4
                                    i32.add
                                    i32.load
                                    local.tee 9
                                    i32.eqz
                                    br_if 0 (;@16;)
                                    i32.const 0
                                    local.set 5
                                    local.get 9
                                    i32.const 10
                                    i32.rem_u
                                    br_if 0 (;@16;)
                                    i32.const 10
                                    local.set 8
                                    loop  ;; label = @17
                                      local.get 5
                                      i32.const 1
                                      i32.add
                                      local.set 5
                                      local.get 9
                                      local.get 8
                                      i32.const 10
                                      i32.mul
                                      local.tee 8
                                      i32.rem_u
                                      i32.eqz
                                      br_if 0 (;@17;)
                                    end
                                  end
                                  local.get 13
                                  local.get 17
                                  i32.sub
                                  i32.const 2
                                  i32.shr_s
                                  i32.const 9
                                  i32.mul
                                  i32.const -9
                                  i32.add
                                  local.set 8
                                  local.get 11
                                  i32.const 32
                                  i32.or
                                  i32.const 102
                                  i32.eq
                                  if  ;; label = @16
                                    i32.const 0
                                    local.set 15
                                    local.get 12
                                    local.get 8
                                    local.get 5
                                    i32.sub
                                    local.tee 5
                                    i32.const 0
                                    local.get 5
                                    i32.const 0
                                    i32.gt_s
                                    select
                                    local.tee 5
                                    local.get 12
                                    local.get 5
                                    i32.lt_s
                                    select
                                    local.set 12
                                    br 1 (;@15;)
                                  end
                                  i32.const 0
                                  local.set 15
                                  local.get 12
                                  local.get 7
                                  local.get 8
                                  i32.add
                                  local.get 5
                                  i32.sub
                                  local.tee 5
                                  i32.const 0
                                  local.get 5
                                  i32.const 0
                                  i32.gt_s
                                  select
                                  local.tee 5
                                  local.get 12
                                  local.get 5
                                  i32.lt_s
                                  select
                                  local.set 12
                                end
                                i32.const -1
                                local.set 5
                                local.get 12
                                i32.const 2147483645
                                i32.const 2147483646
                                local.get 12
                                local.get 15
                                i32.or
                                local.tee 24
                                select
                                i32.gt_s
                                br_if 0 (;@14;)
                                local.get 12
                                local.get 24
                                i32.const 0
                                i32.ne
                                i32.add
                                i32.const 1
                                i32.add
                                local.set 20
                                block  ;; label = @15
                                  local.get 11
                                  i32.const 32
                                  i32.or
                                  i32.const 102
                                  i32.ne
                                  local.tee 29
                                  i32.eqz
                                  if  ;; label = @16
                                    local.get 7
                                    i32.const 2147483647
                                    local.get 20
                                    i32.sub
                                    i32.gt_s
                                    br_if 2 (;@14;)
                                    local.get 7
                                    i32.const 0
                                    local.get 7
                                    i32.const 0
                                    i32.gt_s
                                    select
                                    local.set 7
                                    br 1 (;@15;)
                                  end
                                  local.get 25
                                  local.set 8
                                  local.get 7
                                  local.get 7
                                  i32.const 31
                                  i32.shr_s
                                  local.tee 5
                                  i32.add
                                  local.get 5
                                  i32.xor
                                  local.tee 5
                                  if  ;; label = @16
                                    loop  ;; label = @17
                                      local.get 8
                                      i32.const -1
                                      i32.add
                                      local.tee 8
                                      local.get 5
                                      local.get 5
                                      i32.const 10
                                      i32.div_u
                                      local.tee 9
                                      i32.const 10
                                      i32.mul
                                      i32.sub
                                      i32.const 48
                                      i32.or
                                      i32.store8
                                      local.get 5
                                      i32.const 9
                                      i32.gt_u
                                      local.get 9
                                      local.set 5
                                      br_if 0 (;@17;)
                                    end
                                  end
                                  local.get 25
                                  local.get 8
                                  i32.sub
                                  i32.const 1
                                  i32.le_s
                                  if  ;; label = @16
                                    local.get 8
                                    i32.const -1
                                    i32.add
                                    local.set 5
                                    loop  ;; label = @17
                                      local.get 5
                                      i32.const 48
                                      i32.store8
                                      local.get 25
                                      local.get 5
                                      i32.sub
                                      local.get 5
                                      i32.const -1
                                      i32.add
                                      local.tee 9
                                      local.set 5
                                      i32.const 2
                                      i32.lt_s
                                      br_if 0 (;@17;)
                                    end
                                    local.get 9
                                    i32.const 1
                                    i32.add
                                    local.set 8
                                  end
                                  local.get 8
                                  i32.const -2
                                  i32.add
                                  local.tee 28
                                  local.get 11
                                  i32.store8
                                  i32.const -1
                                  local.set 5
                                  local.get 8
                                  i32.const -1
                                  i32.add
                                  i32.const 45
                                  i32.const 43
                                  local.get 7
                                  i32.const 0
                                  i32.lt_s
                                  select
                                  i32.store8
                                  local.get 25
                                  local.get 28
                                  i32.sub
                                  local.tee 7
                                  i32.const 2147483647
                                  local.get 20
                                  i32.sub
                                  i32.gt_s
                                  br_if 1 (;@14;)
                                end
                                local.get 7
                                local.get 20
                                i32.add
                                local.tee 7
                                local.get 19
                                i32.const 2147483647
                                i32.xor
                                i32.gt_s
                                br_if 0 (;@14;)
                                block  ;; label = @15
                                  local.get 16
                                  i32.const 73728
                                  i32.and
                                  local.tee 20
                                  local.get 14
                                  local.get 7
                                  local.get 19
                                  i32.add
                                  local.tee 16
                                  i32.le_s
                                  i32.or
                                  br_if 0 (;@15;)
                                  local.get 10
                                  i32.const -64
                                  i32.sub
                                  i32.const 32
                                  local.get 14
                                  local.get 16
                                  i32.sub
                                  local.tee 11
                                  i32.const 256
                                  local.get 11
                                  i32.const 256
                                  i32.lt_u
                                  local.tee 7
                                  select
                                  call 33
                                  local.get 0
                                  i32.load
                                  local.tee 8
                                  i32.const 32
                                  i32.and
                                  local.set 5
                                  block  ;; label = @16
                                    local.get 7
                                    i32.eqz
                                    if  ;; label = @17
                                      local.get 5
                                      i32.eqz
                                      local.set 5
                                      local.get 11
                                      local.set 7
                                      loop  ;; label = @18
                                        local.get 5
                                        i32.const 1
                                        i32.and
                                        if  ;; label = @19
                                          local.get 10
                                          i32.const -64
                                          i32.sub
                                          i32.const 256
                                          local.get 0
                                          call 16
                                          local.get 0
                                          i32.load
                                          local.set 8
                                        end
                                        local.get 8
                                        i32.const 32
                                        i32.and
                                        local.tee 9
                                        i32.eqz
                                        local.set 5
                                        local.get 7
                                        i32.const -256
                                        i32.add
                                        local.tee 7
                                        i32.const 255
                                        i32.gt_u
                                        br_if 0 (;@18;)
                                      end
                                      local.get 9
                                      br_if 2 (;@15;)
                                      local.get 11
                                      i32.const 255
                                      i32.and
                                      local.set 11
                                      br 1 (;@16;)
                                    end
                                    local.get 5
                                    br_if 1 (;@15;)
                                  end
                                  local.get 10
                                  i32.const -64
                                  i32.sub
                                  local.get 11
                                  local.get 0
                                  call 16
                                end
                                local.get 0
                                i32.load8_u
                                i32.const 32
                                i32.and
                                i32.eqz
                                if  ;; label = @15
                                  local.get 22
                                  local.get 19
                                  local.get 0
                                  call 16
                                end
                                block  ;; label = @15
                                  local.get 20
                                  i32.const 65536
                                  i32.ne
                                  local.get 14
                                  local.get 16
                                  i32.le_s
                                  i32.or
                                  br_if 0 (;@15;)
                                  local.get 10
                                  i32.const -64
                                  i32.sub
                                  i32.const 48
                                  local.get 14
                                  local.get 16
                                  i32.sub
                                  local.tee 11
                                  i32.const 256
                                  local.get 11
                                  i32.const 256
                                  i32.lt_u
                                  local.tee 7
                                  select
                                  call 33
                                  local.get 0
                                  i32.load
                                  local.tee 8
                                  i32.const 32
                                  i32.and
                                  local.set 5
                                  block  ;; label = @16
                                    local.get 7
                                    i32.eqz
                                    if  ;; label = @17
                                      local.get 5
                                      i32.eqz
                                      local.set 5
                                      local.get 11
                                      local.set 7
                                      loop  ;; label = @18
                                        local.get 5
                                        i32.const 1
                                        i32.and
                                        if  ;; label = @19
                                          local.get 10
                                          i32.const -64
                                          i32.sub
                                          i32.const 256
                                          local.get 0
                                          call 16
                                          local.get 0
                                          i32.load
                                          local.set 8
                                        end
                                        local.get 8
                                        i32.const 32
                                        i32.and
                                        local.tee 9
                                        i32.eqz
                                        local.set 5
                                        local.get 7
                                        i32.const -256
                                        i32.add
                                        local.tee 7
                                        i32.const 255
                                        i32.gt_u
                                        br_if 0 (;@18;)
                                      end
                                      local.get 9
                                      br_if 2 (;@15;)
                                      local.get 11
                                      i32.const 255
                                      i32.and
                                      local.set 11
                                      br 1 (;@16;)
                                    end
                                    local.get 5
                                    br_if 1 (;@15;)
                                  end
                                  local.get 10
                                  i32.const -64
                                  i32.sub
                                  local.get 11
                                  local.get 0
                                  call 16
                                end
                                block  ;; label = @15
                                  local.get 29
                                  i32.eqz
                                  if  ;; label = @16
                                    local.get 17
                                    local.get 6
                                    local.get 6
                                    local.get 17
                                    i32.gt_u
                                    select
                                    local.tee 8
                                    local.set 9
                                    loop  ;; label = @17
                                      block  ;; label = @18
                                        local.get 9
                                        i32.load
                                        local.tee 5
                                        i32.eqz
                                        if  ;; label = @19
                                          i32.const 0
                                          local.set 6
                                          br 1 (;@18;)
                                        end
                                        i32.const 0
                                        local.set 6
                                        loop  ;; label = @19
                                          local.get 6
                                          local.get 31
                                          i32.add
                                          local.get 5
                                          local.get 5
                                          i32.const 10
                                          i32.div_u
                                          local.tee 7
                                          i32.const 10
                                          i32.mul
                                          i32.sub
                                          i32.const 48
                                          i32.or
                                          i32.store8
                                          local.get 6
                                          i32.const -1
                                          i32.add
                                          local.set 6
                                          local.get 5
                                          i32.const 9
                                          i32.gt_u
                                          local.get 7
                                          local.set 5
                                          br_if 0 (;@19;)
                                        end
                                      end
                                      local.get 6
                                      local.get 27
                                      i32.add
                                      local.set 5
                                      block  ;; label = @18
                                        local.get 8
                                        local.get 9
                                        i32.ne
                                        if  ;; label = @19
                                          local.get 5
                                          local.get 10
                                          i32.const 336
                                          i32.add
                                          i32.le_u
                                          br_if 1 (;@18;)
                                          local.get 10
                                          i32.const 336
                                          i32.add
                                          i32.const 48
                                          local.get 6
                                          i32.const 9
                                          i32.add
                                          call 33
                                          local.get 10
                                          i32.const 336
                                          i32.add
                                          local.set 5
                                          br 1 (;@18;)
                                        end
                                        local.get 6
                                        br_if 0 (;@18;)
                                        local.get 5
                                        i32.const -1
                                        i32.add
                                        local.tee 5
                                        i32.const 48
                                        i32.store8
                                      end
                                      local.get 0
                                      i32.load8_u
                                      i32.const 32
                                      i32.and
                                      i32.eqz
                                      if  ;; label = @18
                                        local.get 5
                                        local.get 27
                                        local.get 5
                                        i32.sub
                                        local.get 0
                                        call 16
                                      end
                                      local.get 9
                                      i32.const 4
                                      i32.add
                                      local.tee 9
                                      local.get 17
                                      i32.le_u
                                      br_if 0 (;@17;)
                                    end
                                    block  ;; label = @17
                                      local.get 24
                                      i32.eqz
                                      br_if 0 (;@17;)
                                      local.get 0
                                      i32.load8_u
                                      i32.const 32
                                      i32.and
                                      br_if 0 (;@17;)
                                      i32.const 3507
                                      i32.const 1
                                      local.get 0
                                      call 16
                                    end
                                    block  ;; label = @17
                                      local.get 12
                                      i32.const 1
                                      i32.lt_s
                                      if  ;; label = @18
                                        local.get 12
                                        local.set 5
                                        br 1 (;@17;)
                                      end
                                      local.get 9
                                      local.get 13
                                      i32.ge_u
                                      if  ;; label = @18
                                        local.get 12
                                        local.set 5
                                        br 1 (;@17;)
                                      end
                                      loop  ;; label = @18
                                        local.get 27
                                        local.set 5
                                        block  ;; label = @19
                                          local.get 9
                                          i32.load
                                          local.tee 6
                                          if  ;; label = @20
                                            loop  ;; label = @21
                                              local.get 5
                                              i32.const -1
                                              i32.add
                                              local.tee 5
                                              local.get 6
                                              local.get 6
                                              i32.const 10
                                              i32.div_u
                                              local.tee 7
                                              i32.const 10
                                              i32.mul
                                              i32.sub
                                              i32.const 48
                                              i32.or
                                              i32.store8
                                              local.get 6
                                              i32.const 9
                                              i32.gt_u
                                              local.get 7
                                              local.set 6
                                              br_if 0 (;@21;)
                                            end
                                            local.get 5
                                            local.get 10
                                            i32.const 336
                                            i32.add
                                            i32.le_u
                                            br_if 1 (;@19;)
                                          end
                                          local.get 10
                                          i32.const 336
                                          i32.add
                                          i32.const 48
                                          local.get 5
                                          local.get 30
                                          i32.add
                                          call 33
                                          loop  ;; label = @20
                                            local.get 5
                                            i32.const -1
                                            i32.add
                                            local.tee 5
                                            local.get 10
                                            i32.const 336
                                            i32.add
                                            i32.gt_u
                                            br_if 0 (;@20;)
                                          end
                                        end
                                        local.get 0
                                        i32.load8_u
                                        i32.const 32
                                        i32.and
                                        i32.eqz
                                        if  ;; label = @19
                                          local.get 5
                                          local.get 12
                                          i32.const 9
                                          local.get 12
                                          i32.const 9
                                          i32.lt_s
                                          select
                                          local.get 0
                                          call 16
                                        end
                                        local.get 12
                                        i32.const -9
                                        i32.add
                                        local.set 5
                                        local.get 12
                                        i32.const 10
                                        i32.lt_s
                                        br_if 1 (;@17;)
                                        local.get 5
                                        local.set 12
                                        local.get 9
                                        i32.const 4
                                        i32.add
                                        local.tee 9
                                        local.get 13
                                        i32.lt_u
                                        br_if 0 (;@18;)
                                      end
                                    end
                                    local.get 5
                                    i32.const 1
                                    i32.lt_s
                                    br_if 1 (;@15;)
                                    local.get 10
                                    i32.const -64
                                    i32.sub
                                    i32.const 48
                                    local.get 5
                                    i32.const 256
                                    local.get 5
                                    i32.const 256
                                    i32.lt_u
                                    local.tee 7
                                    select
                                    call 33
                                    local.get 0
                                    i32.load
                                    local.tee 8
                                    i32.const 32
                                    i32.and
                                    local.set 6
                                    block  ;; label = @17
                                      local.get 7
                                      i32.eqz
                                      if  ;; label = @18
                                        local.get 6
                                        i32.eqz
                                        local.set 6
                                        local.get 5
                                        local.set 7
                                        loop  ;; label = @19
                                          local.get 6
                                          i32.const 1
                                          i32.and
                                          if  ;; label = @20
                                            local.get 10
                                            i32.const -64
                                            i32.sub
                                            i32.const 256
                                            local.get 0
                                            call 16
                                            local.get 0
                                            i32.load
                                            local.set 8
                                          end
                                          local.get 8
                                          i32.const 32
                                          i32.and
                                          local.tee 9
                                          i32.eqz
                                          local.set 6
                                          local.get 7
                                          i32.const -256
                                          i32.add
                                          local.tee 7
                                          i32.const 255
                                          i32.gt_u
                                          br_if 0 (;@19;)
                                        end
                                        local.get 9
                                        br_if 3 (;@15;)
                                        local.get 5
                                        i32.const 255
                                        i32.and
                                        local.set 5
                                        br 1 (;@17;)
                                      end
                                      local.get 6
                                      br_if 2 (;@15;)
                                    end
                                    local.get 10
                                    i32.const -64
                                    i32.sub
                                    local.get 5
                                    local.get 0
                                    call 16
                                    br 1 (;@15;)
                                  end
                                  block  ;; label = @16
                                    local.get 12
                                    i32.const 0
                                    i32.lt_s
                                    br_if 0 (;@16;)
                                    local.get 13
                                    local.get 6
                                    i32.const 4
                                    i32.add
                                    local.get 23
                                    select
                                    local.set 11
                                    local.get 6
                                    local.set 9
                                    loop  ;; label = @17
                                      local.get 27
                                      local.set 8
                                      block  ;; label = @18
                                        local.get 9
                                        i32.load
                                        local.tee 5
                                        if  ;; label = @19
                                          i32.const 0
                                          local.set 7
                                          loop  ;; label = @20
                                            local.get 7
                                            local.get 10
                                            i32.add
                                            i32.const 344
                                            i32.add
                                            local.get 5
                                            local.get 5
                                            i32.const 10
                                            i32.div_u
                                            local.tee 8
                                            i32.const 10
                                            i32.mul
                                            i32.sub
                                            i32.const 48
                                            i32.or
                                            i32.store8
                                            local.get 7
                                            i32.const -1
                                            i32.add
                                            local.set 7
                                            local.get 5
                                            i32.const 9
                                            i32.gt_u
                                            local.get 8
                                            local.set 5
                                            br_if 0 (;@20;)
                                          end
                                          local.get 7
                                          local.get 10
                                          i32.add
                                          i32.const 345
                                          i32.add
                                          local.set 8
                                          local.get 7
                                          br_if 1 (;@18;)
                                        end
                                        local.get 8
                                        i32.const -1
                                        i32.add
                                        local.tee 8
                                        i32.const 48
                                        i32.store8
                                      end
                                      block  ;; label = @18
                                        local.get 6
                                        local.get 9
                                        i32.ne
                                        if  ;; label = @19
                                          local.get 8
                                          local.get 10
                                          i32.const 336
                                          i32.add
                                          i32.le_u
                                          br_if 1 (;@18;)
                                          local.get 10
                                          i32.const 336
                                          i32.add
                                          i32.const 48
                                          local.get 8
                                          local.get 30
                                          i32.add
                                          call 33
                                          loop  ;; label = @20
                                            local.get 8
                                            i32.const -1
                                            i32.add
                                            local.tee 8
                                            local.get 10
                                            i32.const 336
                                            i32.add
                                            i32.gt_u
                                            br_if 0 (;@20;)
                                          end
                                          br 1 (;@18;)
                                        end
                                        local.get 0
                                        i32.load8_u
                                        i32.const 32
                                        i32.and
                                        i32.eqz
                                        if  ;; label = @19
                                          local.get 8
                                          i32.const 1
                                          local.get 0
                                          call 16
                                        end
                                        local.get 8
                                        i32.const 1
                                        i32.add
                                        local.set 8
                                        local.get 15
                                        i32.eqz
                                        i32.const 0
                                        local.get 12
                                        i32.const 1
                                        i32.lt_s
                                        select
                                        br_if 0 (;@18;)
                                        local.get 0
                                        i32.load8_u
                                        i32.const 32
                                        i32.and
                                        br_if 0 (;@18;)
                                        i32.const 3507
                                        i32.const 1
                                        local.get 0
                                        call 16
                                      end
                                      local.get 27
                                      local.get 8
                                      i32.sub
                                      local.set 5
                                      local.get 0
                                      i32.load8_u
                                      i32.const 32
                                      i32.and
                                      i32.eqz
                                      if  ;; label = @18
                                        local.get 8
                                        local.get 5
                                        local.get 12
                                        local.get 12
                                        local.get 5
                                        i32.gt_s
                                        select
                                        local.get 0
                                        call 16
                                      end
                                      local.get 9
                                      i32.const 4
                                      i32.add
                                      local.tee 9
                                      local.get 11
                                      i32.lt_u
                                      i32.const 0
                                      local.get 12
                                      local.get 5
                                      i32.sub
                                      local.tee 12
                                      i32.const -1
                                      i32.gt_s
                                      select
                                      br_if 0 (;@17;)
                                    end
                                    local.get 12
                                    i32.const 1
                                    i32.lt_s
                                    br_if 0 (;@16;)
                                    local.get 10
                                    i32.const -64
                                    i32.sub
                                    i32.const 48
                                    local.get 12
                                    i32.const 256
                                    local.get 12
                                    i32.const 256
                                    i32.lt_u
                                    local.tee 6
                                    select
                                    call 33
                                    local.get 0
                                    i32.load
                                    local.tee 7
                                    i32.const 32
                                    i32.and
                                    local.set 5
                                    block  ;; label = @17
                                      local.get 6
                                      i32.eqz
                                      if  ;; label = @18
                                        local.get 5
                                        i32.eqz
                                        local.set 5
                                        local.get 12
                                        local.set 6
                                        loop  ;; label = @19
                                          local.get 5
                                          i32.const 1
                                          i32.and
                                          if  ;; label = @20
                                            local.get 10
                                            i32.const -64
                                            i32.sub
                                            i32.const 256
                                            local.get 0
                                            call 16
                                            local.get 0
                                            i32.load
                                            local.set 7
                                          end
                                          local.get 7
                                          i32.const 32
                                          i32.and
                                          local.tee 8
                                          i32.eqz
                                          local.set 5
                                          local.get 6
                                          i32.const -256
                                          i32.add
                                          local.tee 6
                                          i32.const 255
                                          i32.gt_u
                                          br_if 0 (;@19;)
                                        end
                                        local.get 8
                                        br_if 2 (;@16;)
                                        local.get 12
                                        i32.const 255
                                        i32.and
                                        local.set 12
                                        br 1 (;@17;)
                                      end
                                      local.get 5
                                      br_if 1 (;@16;)
                                    end
                                    local.get 10
                                    i32.const -64
                                    i32.sub
                                    local.get 12
                                    local.get 0
                                    call 16
                                  end
                                  local.get 0
                                  i32.load8_u
                                  i32.const 32
                                  i32.and
                                  br_if 0 (;@15;)
                                  local.get 28
                                  local.get 25
                                  local.get 28
                                  i32.sub
                                  local.get 0
                                  call 16
                                end
                                block  ;; label = @15
                                  local.get 20
                                  i32.const 8192
                                  i32.ne
                                  local.get 14
                                  local.get 16
                                  i32.le_s
                                  i32.or
                                  br_if 0 (;@15;)
                                  local.get 10
                                  i32.const -64
                                  i32.sub
                                  i32.const 32
                                  local.get 14
                                  local.get 16
                                  i32.sub
                                  local.tee 13
                                  i32.const 256
                                  local.get 13
                                  i32.const 256
                                  i32.lt_u
                                  local.tee 6
                                  select
                                  call 33
                                  local.get 0
                                  i32.load
                                  local.tee 7
                                  i32.const 32
                                  i32.and
                                  local.set 5
                                  block  ;; label = @16
                                    local.get 6
                                    i32.eqz
                                    if  ;; label = @17
                                      local.get 5
                                      i32.eqz
                                      local.set 5
                                      local.get 13
                                      local.set 6
                                      loop  ;; label = @18
                                        local.get 5
                                        i32.const 1
                                        i32.and
                                        if  ;; label = @19
                                          local.get 10
                                          i32.const -64
                                          i32.sub
                                          i32.const 256
                                          local.get 0
                                          call 16
                                          local.get 0
                                          i32.load
                                          local.set 7
                                        end
                                        local.get 7
                                        i32.const 32
                                        i32.and
                                        local.tee 8
                                        i32.eqz
                                        local.set 5
                                        local.get 6
                                        i32.const -256
                                        i32.add
                                        local.tee 6
                                        i32.const 255
                                        i32.gt_u
                                        br_if 0 (;@18;)
                                      end
                                      local.get 8
                                      br_if 2 (;@15;)
                                      local.get 13
                                      i32.const 255
                                      i32.and
                                      local.set 13
                                      br 1 (;@16;)
                                    end
                                    local.get 5
                                    br_if 1 (;@15;)
                                  end
                                  local.get 10
                                  i32.const -64
                                  i32.sub
                                  local.get 13
                                  local.get 0
                                  call 16
                                end
                                local.get 14
                                local.get 16
                                local.get 14
                                local.get 16
                                i32.gt_s
                                select
                                local.set 5
                              end
                              local.get 5
                              i32.const 0
                              i32.ge_s
                              br_if 10 (;@3;)
                              br 9 (;@4;)
                            end
                            i32.const 0
                            local.set 12
                            i32.const 2822
                            local.set 15
                          end
                          local.get 21
                          local.set 5
                          br 6 (;@5;)
                        end
                        local.get 7
                        local.set 16
                        local.get 6
                        local.set 9
                        local.get 5
                        i32.load8_u
                        i32.eqz
                        br_if 5 (;@5;)
                        br 6 (;@4;)
                      end
                      local.get 1
                      i32.load8_u offset=1
                      local.set 5
                      local.get 1
                      i32.const 1
                      i32.add
                      local.set 1
                      br 0 (;@9;)
                    end
                    unreachable
                  end
                  local.get 0
                  br_if 6 (;@1;)
                  local.get 26
                  i32.eqz
                  if  ;; label = @8
                    i32.const 0
                    local.set 18
                    br 7 (;@1;)
                  end
                  block (result i32)  ;; label = @8
                    i32.const 1
                    local.get 4
                    i32.load offset=4
                    local.tee 0
                    i32.eqz
                    br_if 0 (;@8;)
                    drop
                    local.get 3
                    i32.const 8
                    i32.add
                    local.get 0
                    local.get 2
                    call 31
                    i32.const 2
                    local.get 4
                    i32.load offset=8
                    local.tee 0
                    i32.eqz
                    br_if 0 (;@8;)
                    drop
                    local.get 3
                    i32.const 16
                    i32.add
                    local.get 0
                    local.get 2
                    call 31
                    i32.const 3
                    local.get 4
                    i32.load offset=12
                    local.tee 0
                    i32.eqz
                    br_if 0 (;@8;)
                    drop
                    local.get 3
                    i32.const 24
                    i32.add
                    local.get 0
                    local.get 2
                    call 31
                    i32.const 4
                    local.get 4
                    i32.load offset=16
                    local.tee 0
                    i32.eqz
                    br_if 0 (;@8;)
                    drop
                    local.get 3
                    i32.const 32
                    i32.add
                    local.get 0
                    local.get 2
                    call 31
                    i32.const 5
                    local.get 4
                    i32.load offset=20
                    local.tee 0
                    i32.eqz
                    br_if 0 (;@8;)
                    drop
                    local.get 3
                    i32.const 40
                    i32.add
                    local.get 0
                    local.get 2
                    call 31
                    i32.const 6
                    local.get 4
                    i32.load offset=24
                    local.tee 0
                    i32.eqz
                    br_if 0 (;@8;)
                    drop
                    local.get 3
                    i32.const 48
                    i32.add
                    local.get 0
                    local.get 2
                    call 31
                    i32.const 7
                    local.get 4
                    i32.load offset=28
                    local.tee 0
                    i32.eqz
                    br_if 0 (;@8;)
                    drop
                    local.get 3
                    i32.const 56
                    i32.add
                    local.get 0
                    local.get 2
                    call 31
                    i32.const 8
                    local.get 4
                    i32.load offset=32
                    local.tee 0
                    i32.eqz
                    br_if 0 (;@8;)
                    drop
                    local.get 3
                    i32.const -64
                    i32.sub
                    local.get 0
                    local.get 2
                    call 31
                    local.get 4
                    i32.load offset=36
                    local.tee 0
                    br_if 2 (;@6;)
                    i32.const 9
                  end
                  local.tee 0
                  i32.const -1
                  i32.add
                  local.set 5
                  local.get 4
                  local.get 0
                  i32.const 2
                  i32.shl
                  i32.add
                  local.set 1
                  loop  ;; label = @8
                    local.get 1
                    i32.load
                    br_if 1 (;@7;)
                    local.get 1
                    i32.const 4
                    i32.add
                    local.set 1
                    i32.const 1
                    local.set 18
                    local.get 5
                    i32.const 1
                    i32.add
                    local.tee 5
                    i32.const 8
                    i32.le_u
                    br_if 0 (;@8;)
                  end
                  br 6 (;@1;)
                end
                i32.const 4256
                i32.const 28
                i32.store
                br 4 (;@2;)
              end
              local.get 3
              i32.const 72
              i32.add
              local.get 0
              local.get 2
              call 31
              i32.const 1
              local.set 18
              br 4 (;@1;)
            end
            local.get 5
            local.get 8
            i32.sub
            local.tee 19
            local.get 9
            local.get 9
            local.get 19
            i32.lt_s
            select
            local.tee 22
            i32.const 2147483647
            local.get 12
            i32.sub
            i32.gt_s
            br_if 0 (;@4;)
            local.get 12
            local.get 22
            i32.add
            local.tee 17
            local.get 14
            local.get 14
            local.get 17
            i32.lt_s
            select
            local.tee 5
            local.get 20
            i32.gt_s
            br_if 0 (;@4;)
            block  ;; label = @5
              local.get 16
              i32.const 73728
              i32.and
              local.tee 16
              local.get 17
              local.get 14
              i32.ge_s
              i32.or
              br_if 0 (;@5;)
              local.get 10
              i32.const -64
              i32.sub
              i32.const 32
              local.get 5
              local.get 17
              i32.sub
              local.tee 11
              i32.const 256
              local.get 11
              i32.const 256
              i32.lt_u
              local.tee 7
              select
              call 33
              local.get 0
              i32.load
              local.tee 13
              i32.const 32
              i32.and
              local.set 6
              block  ;; label = @6
                local.get 7
                i32.eqz
                if  ;; label = @7
                  local.get 6
                  i32.eqz
                  local.set 6
                  local.get 11
                  local.set 7
                  loop  ;; label = @8
                    local.get 6
                    i32.const 1
                    i32.and
                    if  ;; label = @9
                      local.get 10
                      i32.const -64
                      i32.sub
                      i32.const 256
                      local.get 0
                      call 16
                      local.get 0
                      i32.load
                      local.set 13
                    end
                    local.get 13
                    i32.const 32
                    i32.and
                    local.tee 20
                    i32.eqz
                    local.set 6
                    local.get 7
                    i32.const -256
                    i32.add
                    local.tee 7
                    i32.const 255
                    i32.gt_u
                    br_if 0 (;@8;)
                  end
                  local.get 20
                  br_if 2 (;@5;)
                  local.get 11
                  i32.const 255
                  i32.and
                  local.set 11
                  br 1 (;@6;)
                end
                local.get 6
                br_if 1 (;@5;)
              end
              local.get 10
              i32.const -64
              i32.sub
              local.get 11
              local.get 0
              call 16
            end
            local.get 0
            i32.load8_u
            i32.const 32
            i32.and
            i32.eqz
            if  ;; label = @5
              local.get 15
              local.get 12
              local.get 0
              call 16
            end
            block  ;; label = @5
              local.get 16
              i32.const 65536
              i32.ne
              local.get 17
              local.get 14
              i32.ge_s
              i32.or
              br_if 0 (;@5;)
              local.get 10
              i32.const -64
              i32.sub
              i32.const 48
              local.get 5
              local.get 17
              i32.sub
              local.tee 12
              i32.const 256
              local.get 12
              i32.const 256
              i32.lt_u
              local.tee 7
              select
              call 33
              local.get 0
              i32.load
              local.tee 13
              i32.const 32
              i32.and
              local.set 6
              block  ;; label = @6
                local.get 7
                i32.eqz
                if  ;; label = @7
                  local.get 6
                  i32.eqz
                  local.set 6
                  local.get 12
                  local.set 7
                  loop  ;; label = @8
                    local.get 6
                    i32.const 1
                    i32.and
                    if  ;; label = @9
                      local.get 10
                      i32.const -64
                      i32.sub
                      i32.const 256
                      local.get 0
                      call 16
                      local.get 0
                      i32.load
                      local.set 13
                    end
                    local.get 13
                    i32.const 32
                    i32.and
                    local.tee 11
                    i32.eqz
                    local.set 6
                    local.get 7
                    i32.const -256
                    i32.add
                    local.tee 7
                    i32.const 255
                    i32.gt_u
                    br_if 0 (;@8;)
                  end
                  local.get 11
                  br_if 2 (;@5;)
                  local.get 12
                  i32.const 255
                  i32.and
                  local.set 12
                  br 1 (;@6;)
                end
                local.get 6
                br_if 1 (;@5;)
              end
              local.get 10
              i32.const -64
              i32.sub
              local.get 12
              local.get 0
              call 16
            end
            block  ;; label = @5
              local.get 19
              local.get 9
              i32.ge_s
              br_if 0 (;@5;)
              local.get 10
              i32.const -64
              i32.sub
              i32.const 48
              local.get 22
              local.get 19
              i32.sub
              local.tee 11
              i32.const 256
              local.get 11
              i32.const 256
              i32.lt_u
              local.tee 7
              select
              call 33
              local.get 0
              i32.load
              local.tee 13
              i32.const 32
              i32.and
              local.set 6
              block  ;; label = @6
                local.get 7
                i32.eqz
                if  ;; label = @7
                  local.get 6
                  i32.eqz
                  local.set 6
                  local.get 11
                  local.set 7
                  loop  ;; label = @8
                    local.get 6
                    i32.const 1
                    i32.and
                    if  ;; label = @9
                      local.get 10
                      i32.const -64
                      i32.sub
                      i32.const 256
                      local.get 0
                      call 16
                      local.get 0
                      i32.load
                      local.set 13
                    end
                    local.get 13
                    i32.const 32
                    i32.and
                    local.tee 9
                    i32.eqz
                    local.set 6
                    local.get 7
                    i32.const -256
                    i32.add
                    local.tee 7
                    i32.const 255
                    i32.gt_u
                    br_if 0 (;@8;)
                  end
                  local.get 9
                  br_if 2 (;@5;)
                  local.get 11
                  i32.const 255
                  i32.and
                  local.set 11
                  br 1 (;@6;)
                end
                local.get 6
                br_if 1 (;@5;)
              end
              local.get 10
              i32.const -64
              i32.sub
              local.get 11
              local.get 0
              call 16
            end
            local.get 0
            i32.load8_u
            i32.const 32
            i32.and
            i32.eqz
            if  ;; label = @5
              local.get 8
              local.get 19
              local.get 0
              call 16
            end
            local.get 16
            i32.const 8192
            i32.ne
            local.get 17
            local.get 14
            i32.ge_s
            i32.or
            br_if 1 (;@3;)
            local.get 10
            i32.const -64
            i32.sub
            i32.const 32
            local.get 5
            local.get 17
            i32.sub
            local.tee 14
            i32.const 256
            local.get 14
            i32.const 256
            i32.lt_u
            local.tee 7
            select
            call 33
            local.get 0
            i32.load
            local.tee 8
            i32.const 32
            i32.and
            local.set 6
            block  ;; label = @5
              local.get 7
              i32.eqz
              if  ;; label = @6
                local.get 6
                i32.eqz
                local.set 6
                local.get 14
                local.set 7
                loop  ;; label = @7
                  local.get 6
                  i32.const 1
                  i32.and
                  if  ;; label = @8
                    local.get 10
                    i32.const -64
                    i32.sub
                    i32.const 256
                    local.get 0
                    call 16
                    local.get 0
                    i32.load
                    local.set 8
                  end
                  local.get 8
                  i32.const 32
                  i32.and
                  local.tee 9
                  i32.eqz
                  local.set 6
                  local.get 7
                  i32.const -256
                  i32.add
                  local.tee 7
                  i32.const 255
                  i32.gt_u
                  br_if 0 (;@7;)
                end
                local.get 9
                br_if 3 (;@3;)
                local.get 14
                i32.const 255
                i32.and
                local.set 14
                br 1 (;@5;)
              end
              local.get 6
              br_if 2 (;@3;)
            end
            local.get 10
            i32.const -64
            i32.sub
            local.get 14
            local.get 0
            call 16
            br 1 (;@3;)
          end
        end
        i32.const 4256
        i32.const 61
        i32.store
      end
      i32.const -1
      local.set 18
    end
    local.get 10
    i32.const 880
    i32.add
    global.set 0
    local.get 18)
  (func (;31;) (type 2) (param i32 i32 i32)
    block  ;; label = @1
      block  ;; label = @2
        block  ;; label = @3
          local.get 1
          i32.const -9
          i32.add
          local.tee 1
          i32.const 17
          i32.le_u
          if  ;; label = @4
            block  ;; label = @5
              block  ;; label = @6
                block  ;; label = @7
                  block  ;; label = @8
                    block  ;; label = @9
                      block  ;; label = @10
                        local.get 1
                        i32.const 1
                        i32.sub
                        br_table 7 (;@3;) 8 (;@2;) 9 (;@1;) 7 (;@3;) 8 (;@2;) 0 (;@10;) 1 (;@9;) 2 (;@8;) 3 (;@7;) 9 (;@1;) 8 (;@2;) 9 (;@1;) 9 (;@1;) 7 (;@3;) 8 (;@2;) 9 (;@1;) 4 (;@6;) 5 (;@5;)
                      end
                      local.get 2
                      local.get 2
                      i32.load
                      local.tee 1
                      i32.const 4
                      i32.add
                      i32.store
                      local.get 0
                      local.get 1
                      i64.load16_s
                      i64.store
                      return
                    end
                    local.get 2
                    local.get 2
                    i32.load
                    local.tee 1
                    i32.const 4
                    i32.add
                    i32.store
                    local.get 0
                    local.get 1
                    i64.load16_u
                    i64.store
                    return
                  end
                  local.get 2
                  local.get 2
                  i32.load
                  local.tee 1
                  i32.const 4
                  i32.add
                  i32.store
                  local.get 0
                  local.get 1
                  i64.load8_s
                  i64.store
                  return
                end
                local.get 2
                local.get 2
                i32.load
                local.tee 1
                i32.const 4
                i32.add
                i32.store
                local.get 0
                local.get 1
                i64.load8_u
                i64.store
                return
              end
              call 34
              local.tee 0
              i32.const 3312
              i32.const 1
              local.get 0
              i32.const 3512
              call 17
              i32.ne
              drop
              unreachable
            end
            local.get 2
            local.get 2
            i32.load
            local.tee 1
            i32.const 4
            i32.add
            i32.store
            local.get 0
            local.get 1
            i32.load
            i32.store
          end
          return
        end
        local.get 2
        local.get 2
        i32.load
        local.tee 1
        i32.const 4
        i32.add
        i32.store
        local.get 0
        local.get 1
        i64.load32_s
        i64.store
        return
      end
      local.get 2
      local.get 2
      i32.load
      local.tee 1
      i32.const 4
      i32.add
      i32.store
      local.get 0
      local.get 1
      i64.load32_u
      i64.store
      return
    end
    local.get 2
    local.get 2
    i32.load
    i32.const 7
    i32.add
    i32.const -8
    i32.and
    local.tee 1
    i32.const 8
    i32.add
    i32.store
    local.get 0
    local.get 1
    i64.load
    i64.store)
  (func (;32;) (type 2) (param i32 i32 i32)
    (local i32 i32 i32 i32 i32 i32 i32)
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
      local.get 2
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
  (func (;33;) (type 2) (param i32 i32 i32)
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
      local.tee 0
      i32.store
      local.get 3
      local.get 2
      local.get 4
      i32.sub
      i32.const -4
      i32.and
      local.tee 2
      i32.add
      local.tee 1
      i32.const -4
      i32.add
      local.get 0
      i32.store
      local.get 2
      i32.const 9
      i32.lt_u
      br_if 0 (;@1;)
      local.get 3
      local.get 0
      i32.store offset=8
      local.get 3
      local.get 0
      i32.store offset=4
      local.get 1
      i32.const -8
      i32.add
      local.get 0
      i32.store
      local.get 1
      i32.const -12
      i32.add
      local.get 0
      i32.store
      local.get 2
      i32.const 25
      i32.lt_u
      br_if 0 (;@1;)
      local.get 3
      local.get 0
      i32.store offset=24
      local.get 3
      local.get 0
      i32.store offset=20
      local.get 3
      local.get 0
      i32.store offset=16
      local.get 3
      local.get 0
      i32.store offset=12
      local.get 1
      i32.const -16
      i32.add
      local.get 0
      i32.store
      local.get 1
      i32.const -20
      i32.add
      local.get 0
      i32.store
      local.get 1
      i32.const -24
      i32.add
      local.get 0
      i32.store
      local.get 1
      i32.const -28
      i32.add
      local.get 0
      i32.store
      local.get 2
      local.get 3
      i32.const 4
      i32.and
      i32.const 24
      i32.or
      local.tee 1
      i32.sub
      local.tee 2
      i32.const 32
      i32.lt_u
      br_if 0 (;@1;)
      local.get 0
      i64.extend_i32_u
      local.tee 5
      i64.const 32
      i64.shl
      local.get 5
      i64.or
      local.set 5
      local.get 1
      local.get 3
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
    end)
  (func (;34;) (type 7) (result i32)
    (local i32 i32 i32)
    block  ;; label = @1
      block  ;; label = @2
        block  ;; label = @3
          i32.const 3312
          local.tee 0
          i32.const 3
          i32.and
          i32.eqz
          br_if 0 (;@3;)
          i32.const 3312
          i32.load8_u
          i32.eqz
          if  ;; label = @4
            i32.const 0
            return
          end
          i32.const 3313
          local.set 0
          loop  ;; label = @4
            local.get 0
            i32.const 3
            i32.and
            i32.eqz
            br_if 1 (;@3;)
            local.get 0
            i32.load8_u
            local.get 0
            i32.const 1
            i32.add
            local.tee 2
            local.set 0
            br_if 0 (;@4;)
          end
          br 1 (;@2;)
        end
        local.get 0
        i32.const -4
        i32.add
        local.set 0
        loop  ;; label = @3
          local.get 0
          i32.const 4
          i32.add
          local.tee 0
          i32.load
          local.tee 1
          i32.const -1
          i32.xor
          local.get 1
          i32.const -16843009
          i32.add
          i32.and
          i32.const -2139062144
          i32.and
          i32.eqz
          br_if 0 (;@3;)
        end
        local.get 1
        i32.const 255
        i32.and
        i32.eqz
        if  ;; label = @3
          local.get 0
          i32.const 3312
          i32.sub
          return
        end
        loop  ;; label = @3
          local.get 0
          i32.load8_u offset=1
          local.get 0
          i32.const 1
          i32.add
          local.tee 1
          local.set 0
          br_if 0 (;@3;)
        end
        br 1 (;@1;)
      end
      local.get 2
      i32.const -1
      i32.add
      local.set 1
    end
    local.get 1
    i32.const 3312
    i32.sub)
  (func (;35;) (type 4) (param i32 i32) (result i32)
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
            i32.eqz
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
        i32.eqz
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
        i32.eqz
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
  (func (;36;) (type 4) (param i32 i32) (result i32)
    local.get 0
    if (result i32)  ;; label = @1
      local.get 1
      i32.const 127
      i32.le_u
      if  ;; label = @2
        local.get 0
        local.get 1
        i32.store8
        i32.const 1
        return
      end
      block  ;; label = @2
        i32.const 5308
        i32.load
        i32.eqz
        if  ;; label = @3
          local.get 1
          i32.const -128
          i32.and
          i32.const 57216
          i32.ne
          br_if 1 (;@2;)
          local.get 0
          local.get 1
          i32.store8
          i32.const 1
          return
        end
        local.get 1
        i32.const 2047
        i32.le_u
        if  ;; label = @3
          local.get 0
          local.get 1
          i32.const 63
          i32.and
          i32.const 128
          i32.or
          i32.store8 offset=1
          local.get 0
          local.get 1
          i32.const 6
          i32.shr_u
          i32.const 192
          i32.or
          i32.store8
          i32.const 2
          return
        end
        local.get 1
        i32.const 55296
        i32.ge_u
        i32.const 0
        local.get 1
        i32.const -8192
        i32.and
        i32.const 57344
        i32.ne
        select
        i32.eqz
        if  ;; label = @3
          local.get 0
          local.get 1
          i32.const 63
          i32.and
          i32.const 128
          i32.or
          i32.store8 offset=2
          local.get 0
          local.get 1
          i32.const 12
          i32.shr_u
          i32.const 224
          i32.or
          i32.store8
          local.get 0
          local.get 1
          i32.const 6
          i32.shr_u
          i32.const 63
          i32.and
          i32.const 128
          i32.or
          i32.store8 offset=1
          i32.const 3
          return
        end
        local.get 1
        i32.const -65536
        i32.add
        i32.const 1048575
        i32.le_u
        if  ;; label = @3
          local.get 0
          local.get 1
          i32.const 63
          i32.and
          i32.const 128
          i32.or
          i32.store8 offset=3
          local.get 0
          local.get 1
          i32.const 18
          i32.shr_u
          i32.const 240
          i32.or
          i32.store8
          local.get 0
          local.get 1
          i32.const 6
          i32.shr_u
          i32.const 63
          i32.and
          i32.const 128
          i32.or
          i32.store8 offset=2
          local.get 0
          local.get 1
          i32.const 12
          i32.shr_u
          i32.const 63
          i32.and
          i32.const 128
          i32.or
          i32.store8 offset=1
          i32.const 4
          return
        end
      end
      i32.const 4256
      i32.const 25
      i32.store
      i32.const -1
    else
      i32.const 1
    end)
  (func (;37;) (type 4) (param i32 i32) (result i32)
    local.get 0
    i32.eqz
    if  ;; label = @1
      i32.const 0
      return
    end
    local.get 0
    local.get 1
    call 36)
  (func (;38;) (type 13) (param f64 i32) (result f64)
    (local i32 i64)
    local.get 0
    i64.reinterpret_f64
    local.tee 3
    i64.const 52
    i64.shr_u
    i32.wrap_i64
    i32.const 2047
    i32.and
    local.tee 2
    i32.const 2047
    i32.ne
    if (result f64)  ;; label = @1
      local.get 2
      i32.eqz
      if  ;; label = @2
        local.get 0
        f64.const 0x0p+0 (;=0;)
        f64.eq
        if  ;; label = @3
          local.get 1
          i32.const 0
          i32.store
          local.get 0
          return
        end
        local.get 0
        f64.const 0x1p+64 (;=1.84467e+19;)
        f64.mul
        local.get 1
        call 38
        local.get 1
        local.get 1
        i32.load
        i32.const -64
        i32.add
        i32.store
        return
      end
      local.get 1
      local.get 2
      i32.const -1022
      i32.add
      i32.store
      local.get 3
      i64.const -9218868437227405313
      i64.and
      i64.const 4602678819172646912
      i64.or
      f64.reinterpret_i64
    else
      local.get 0
    end)
  (table (;0;) 5 5 funcref)
  (memory (;0;) 2)
  (global (;0;) (mut i32) (i32.const 70880))
  (export "memory" (memory 0))
  (export "_start" (func 5))
  (elem (;0;) (i32.const 1) 19 23 21 25)
  (data (;0;) (i32.const 1024) "[PolyBench] posix_memalign: cannot allocate memory\00==BEGIN DUMP_ARRAYS==\0a\00begin dump: %s\00D\00%0.2lf \00\0aend   dump: %s\0a\00==END   DUMP_ARRAYS==\0a\00\00\b8\0d\00\00\00\19\12D;\02?,G\14=30\0a\1b\06FKE7\0fI\0e\17\03@\1d<+6\1fJ-\1c\01 %)!\08\0c\15\16\22.\108>\0b41\18/A\099\11#C2B:\05\04&('\0d*\1e5\07\1aH\13$L\ff\00\00Success\00Illegal byte sequence\00Domain error\00Result not representable\00Not a tty\00Permission denied\00Operation not permitted\00No such file or directory\00No such process\00File exists\00Value too large for data type\00No space left on device\00Out of memory\00Resource busy\00Interrupted system call\00Resource temporarily unavailable\00Invalid seek\00Cross-device link\00Read-only file system\00Directory not empty\00Connection reset by peer\00Operation timed out\00Connection refused\00Host is unreachable\00Address in use\00Broken pipe\00I/O error\00No such device or address\00No such device\00Not a directory\00Is a directory\00Text file busy\00Exec format error\00Invalid argument\00Argument list too long\00Symbolic link loop\00Filename too long\00Too many open files in system\00No file descriptors available\00Bad file descriptor\00No child process\00Bad address\00File too large\00Too many links\00No locks available\00Resource deadlock would occur\00State not recoverable\00Previous owner died\00Operation canceled\00Function not implemented\00No message of desired type\00Identifier removed\00Link has been severed\00Protocol error\00Bad message\00Not a socket\00Destination address required\00Message too large\00Protocol wrong type for socket\00Protocol not available\00Protocol not supported\00Not supported\00Address family not supported by protocol\00Address not available\00Network is down\00Network unreachable\00Connection reset by network\00Connection aborted\00No buffer space available\00Socket is connected\00Socket not connected\00Operation already in progress\00Operation in progress\00Stale file handle\00Quota exceeded\00Multihop attempted\00Capabilities insufficient\00No error information\00\00-+   0X0x\00(null)")
  (data (;1;) (i32.const 2848) "\19\00\0a\00\19\19\19\00\00\00\00\05\00\00\00\00\00\00\09\00\00\00\00\0b\00\00\00\00\00\00\00\00\19\00\11\0a\19\19\19\03\0a\07\00\01\1b\09\0b\18\00\00\09\06\0b\00\00\0b\00\06\19\00\00\00\19\19\19")
  (data (;2;) (i32.const 2929) "\0e\00\00\00\00\00\00\00\00\19\00\0a\0d\19\19\19\00\0d\00\00\02\00\09\0e\00\00\00\09\00\0e\00\00\0e")
  (data (;3;) (i32.const 2987) "\0c")
  (data (;4;) (i32.const 2999) "\13\00\00\00\00\13\00\00\00\00\09\0c\00\00\00\00\00\0c\00\00\0c")
  (data (;5;) (i32.const 3045) "\10")
  (data (;6;) (i32.const 3057) "\0f\00\00\00\04\0f\00\00\00\00\09\10\00\00\00\00\00\10\00\00\10")
  (data (;7;) (i32.const 3103) "\12")
  (data (;8;) (i32.const 3115) "\11\00\00\00\00\11\00\00\00\00\09\12\00\00\00\00\00\12\00\00\12\00\00\1a\00\00\00\1a\1a\1a")
  (data (;9;) (i32.const 3170) "\1a\00\00\00\1a\1a\1a\00\00\00\00\00\00\09")
  (data (;10;) (i32.const 3219) "\14")
  (data (;11;) (i32.const 3231) "\17\00\00\00\00\17\00\00\00\00\09\14\00\00\00\00\00\14\00\00\14")
  (data (;12;) (i32.const 3277) "\16")
  (data (;13;) (i32.const 3289) "\15\00\00\00\00\15\00\00\00\00\09\16\00\00\00\00\00\16\00\00\16\00\00Support for formatting long double values is currently disabled.\0aTo enable it, add -lc-printscan-long-double to the link command.\0a")
  (data (;14;) (i32.const 3456) "0123456789ABCDEF-0X+0X 0X-0x+0x 0x\00inf\00INF\00nan\00NAN\00.")
  (data (;15;) (i32.const 3512) "\05")
  (data (;16;) (i32.const 3524) "\01")
  (data (;17;) (i32.const 3544) "\02\00\00\00\03\00\00\00\b0\10")
  (data (;18;) (i32.const 3568) "\02\00\00\00\00\00\00\00\ff\ff\ff\ff")
  (data (;19;) (i32.const 3624) "\b8\0d\00\00\00\00\00\00\05")
  (data (;20;) (i32.const 3644) "\01")
  (data (;21;) (i32.const 3664) "\04\00\00\00\03\00\00\00\b8\10\00\00\00\04")
  (data (;22;) (i32.const 3688) "\01\00\00\00\00\00\00\00\0a")
  (data (;23;) (i32.const 3744) "0\0e"))
