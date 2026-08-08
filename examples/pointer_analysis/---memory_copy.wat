(module
  (memory (export "mem") 1)

  ;; Copy 12 bytes from address 0 to address 32.
  (func (export "main") (result i32)
    ;; Initialize memory[0..3]
    i32.const 0
    i32.const 42
    i32.store

    i32.const 4
    i32.const 43
    i32.store

    i32.const 8
    i32.const 44
    i32.store

    i32.const 12
    i32.const 45
    i32.store

    ;; memory.copy(dest=8, src=0, len=4)
    i32.const 32
    i32.const 0
    i32.const 12
    memory.copy

    ;; load what's been copied
    i32.const 32
    i32.load
  )
)