(module
  (type (;0;) (func (param i32 i32 i32)))
  (func (;0;) (type 0) (param i32 i32 i32)
    (local i32)
    global.get 0
    i32.const 16
    i32.sub
    local.set 3
    local.get 0
    local.get 1
    i32.store
    block  ;; label = @1
      local.get 2
      i32.const 1
      i32.ne
      br_if 0 (;@1;)
      local.get 3
      i32.const 0
      i32.store offset=12
      local.get 3
      local.get 1
      i32.store offset=8
      local.get 0
      local.get 3
      i32.const 8
      i32.add
      i32.store offset=4
      return
    end
    local.get 0
    i32.const 0
    i32.store offset=4)
  (table (;0;) 1 1 funcref)
  (memory (;0;) 2)
  (global (;0;) (mut i32) (i32.const 66560))
  (export "memory" (memory 0))
  (export "mklist" (func 0)))
