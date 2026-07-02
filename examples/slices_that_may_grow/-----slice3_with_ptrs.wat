(module
  (type (;0;) (func (param i32 i32 i32 i32 i32)))
  (type (;1;) (func (result i32)))
  (func (;0;) (type 0) (param i32 i32 i32 i32 i32)
    i32.const 1000
    i32.const 1234
    i32.store)
  (func (;1;) (type 1) (result i32)
    (local i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32)
    i32.const 0
    i32.const 42
    i32.store
                ;; the function call and all its arguments were all removed, which which is the expected behaviour, but not the bug we're trying to study
    i32.const 0
    i32.load
    local.set 16
    local.get 16)
  (memory (;0;) 1)
  (export "main" (func 1))
)