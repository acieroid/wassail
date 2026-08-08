(module
  (type (;0;) (func (param i32 i32 i32 i32 i32)))
  (type (;1;) (func (result i32)))
  (func (;0;) (type 0) (param i32 i32 i32 i32 i32)
    i32.const 1000
    i32.const 1234
    i32.store)
  (func (;1;) (type 1) (result i32)
    (local i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32)
    i32.const 0
    local.set 0
    i32.const 20
    local.set 20
    i32.const 8192
    local.set 26
    i32.const 4
    i32.const 42
    i32.store
    block
      block
        local.get 0    ;; arg 1
        i32.const 32   ;; arg 2
        local.get 20   ;; arg 3
        i32.const 0    ;; arg 4
        local.get 26   ;; arg 5
        call 0         ;; needs 5 args
        local.get 26
        i32.eqz
        br_if 1
      end
      local.get 0
      i32.const 48
      local.get 20
      i32.const 18
      i32.add
      i32.const 18
      i32.const 0
      call 0
    end
    i32.const 4
    i32.load
    local.set 16
    local.get 16)
  (memory (;0;) 1)
  (export "main" (func 1))
)