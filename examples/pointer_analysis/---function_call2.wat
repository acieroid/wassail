(module
  (memory (export "memory") 1)

  ;; Function that stores 422 at address 42
  (func $store42
    i32.const 42
    i32.const 422
    i32.store)

  ;; Main function that increments global g and calls add42(10)
  (func $main
    i32.const 42
    i32.const 14
    i32.store          ;; storing 14 at address 42

    i32.const 10
    i32.const 36
    i32.store          ;; storing 36 at address 10

    call $store42)     ;; function overrides what was written at address 42

  ;; Export the main function
  (export "main" (func $main))
)