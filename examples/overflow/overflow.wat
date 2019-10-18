(module
  (type (;0;) (func (param i32 i32 i32) (result i32)))
  (type (;1;) (func (param i32) (result i32)))
  (type (;2;) (func (result i32)))
  (type (;3;) (func (param i32 i32) (result i32)))
  (func (;0;) (type 0) (param i32 i32 i32) (result i32)
    (local i32)
    block  ;; label = @1
      local.get 2
      i32.const 1
      i32.lt_s
      br_if 0 (;@1;)
      local.get 0
      local.set 3
      loop  ;; label = @2
        local.get 3
        local.get 1
        i32.load8_u
        i32.store8
        local.get 1
        i32.const 1
        i32.add
        local.set 1
        local.get 3
        i32.const 1
        i32.add
        local.set 3
        local.get 2
        i32.const -1
        i32.add
        local.tee 2
        br_if 0 (;@2;)
      end
    end
    local.get 0)
  (func (;1;) (type 1) (param i32) (result i32)
    (local i32 i32 i32 i32 i32)
    global.get 0
    i32.const 32
    i32.sub
    local.tee 1
    global.set 0
    block  ;; label = @1
      block  ;; label = @2
        local.get 0
        i32.load8_u
        br_if 0 (;@2;)
        i32.const 0
        local.set 2
        br 1 (;@1;)
      end
      local.get 0
      i32.const 1
      i32.add
      local.set 3
      i32.const 0
      local.set 4
      loop  ;; label = @2
        local.get 3
        local.get 4
        i32.add
        local.set 5
        local.get 4
        i32.const 1
        i32.add
        local.tee 2
        local.set 4
        local.get 5
        i32.load8_u
        br_if 0 (;@2;)
      end
    end
    local.get 1
    local.get 2
    i32.store offset=24
    local.get 1
    i32.const 8
    i32.add
    local.get 0
    local.get 2
    call 0
    drop
    local.get 1
    i32.load offset=24
    local.set 4
    local.get 1
    i32.const 32
    i32.add
    global.set 0
    local.get 4)
  (func (;2;) (type 2) (result i32)
    i32.const 2)
  (func (;3;) (type 3) (param i32 i32) (result i32)
    call 2)
  (table (;0;) 1 1 funcref)
  (memory (;0;) 2)
  (global (;0;) (mut i32) (i32.const 66560))
  (export "memory" (memory 0))
  (export "foo" (func 1))
  (export "main" (func 3))
  (export "__original_main" (func 2)))
