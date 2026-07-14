(module
  (type (;0;) (func (param i32 i32 i32 i32 i32)))
  (type (;1;) (func (result i32)))
  (func (;0;) (type 0) (param i32 i32 i32 i32 i32)
    i32.const 1000
    i32.const 1234
    i32.store)
  (func (;1;) (type 1) (result i32)
    (local i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32)
    i32.const 30
    local.set 2
    i32.const 0
    i32.const 42
    i32.store  
                  ;; args 1 and 2 have been removed and replaced with the next 2 zeros: 
    i32.const 0   ;; replaces stack value for arg 1
    i32.const 0   ;; replaces stack value for arg 2
    local.get 2
    i32.const 18
    i32.add       ;; arg 3 is kept unchanged, because the following tee will make it relevant later on
    local.tee 4
    drop          ;; arg 3 is dropped
    drop          ;; arg 2 (its const 0 placeholder) is dropped 
    drop          ;; arg 1 (its const 0 placeholder) is dropped 
                  ;; args 4 and 5 are removed, as well as the unnecessary function call
    i32.const 0
    i32.load
    local.get 4
    i32.add
    local.set 16
    local.get 16)
  (memory (;0;) 1)
  (export "main" (func 1))
)