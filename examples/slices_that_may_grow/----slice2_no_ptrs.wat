(module
  (type (;0;) (func (param i32 i32 i32 i32 i32)))
  (type (;1;) (func (result i32)))
  (func (;0;) (type 0) (param i32 i32 i32 i32 i32)
    i32.const 1000
    i32.const 1234
    i32.store)
  (func (;1;) (type 1) (result i32)
    (local i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32)
    i32.const 10
    local.set 0
    i32.const 20
    local.set 1
    i32.const 30
    local.set 2
    i32.const 40
    local.set 3
    i32.const 0
    i32.const 42
    i32.store
    local.get 0
    i32.const 48
    i32.add      ;; arg 1
    local.get 1
    i32.const 9
    i32.add      ;; arg 2
    local.get 2
    i32.const 18
    i32.add      ;; arg 3 (kept alive by the next tee)
    local.tee 4 
    local.get 3
    local.get 0
    i32.sub      ;; arg 4
    local.get 1
    local.get 2
    i32.xor      ;; arg 5
    call 0       ;; requires 5 args
    i32.const 0
    i32.load
    local.get 4
    i32.add
    local.set 16
    local.get 16)
  (memory (;0;) 1)
  (export "main" (func 1))
)