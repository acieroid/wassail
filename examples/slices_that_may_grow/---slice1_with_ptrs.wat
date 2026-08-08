(module
  (type (;0;) (func (param i32 i32 i32 i32 i32)))
  (type (;1;) (func (result i32)))
  (func (;0;) (type 0) (param i32 i32 i32 i32 i32)
    i32.const 1000
    i32.const 1234
    i32.store)
  (func (;1;) (type 1) (result i32)
    (local i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32)
    i32.const 7
    local.set 1
    i32.const 0
    i32.const 42
    i32.store
    i32.const 0  ;; arg 1 is replaced with 0
    i32.const 0  ;; arg 2 is replaced with 0
    local.get 1
    i32.const 18
    i32.add      ;; arg 3 is kept intact, because needed later (its value is kept via the local.tee)
    local.tee 2
    drop         ;; arg 3 is dropped
    drop         ;; arg 2 is dropped
    drop         ;; arg 1 is dropped
                 ;; args 4 and 5 have been removed, as well as the function call
    i32.const 0
    i32.load
    local.get 2
    i32.add
    local.set 16
    local.get 16)
  (memory (;0;) 1)
  (export "main" (func 1))
)