(module
  (type (;0;) (func (param i32)))
  (import "env" "print_string" (func (;0;) (type 0)))
  (func (;1;) (type 0) (param i32)
    (local i32)
    global.get 0
    i32.const 16
    i32.sub
    local.tee 1
    global.set 0
    local.get 1
    local.get 0
    i32.store offset=12
    local.get 1
    i32.load offset=12
    call 0
    local.get 1
    i32.const 16
    i32.add
    global.set 0)
  (func (;2;) (type 0) (param i32)
    (local i32)
    global.get 0
    i32.const 16
    i32.sub
    local.tee 1
    global.set 0
    local.get 1
    local.get 0
    i32.store offset=12
    i32.const 1024
    call 0
    local.get 1
    i32.const 16
    i32.add
    global.set 0)
  (table (;0;) 1 1 funcref)
  (memory (;0;) 2)
  (global (;0;) (mut i32) (i32.const 66576))
  (export "memory" (memory 0))
  (export "propagate_taint" (func 1))
  (export "ignore_arg" (func 2))
  (data (;0;) (i32.const 1024) "hello!\0a\00"))
