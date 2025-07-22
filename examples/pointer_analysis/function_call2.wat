(module
  ;; Define a mutable global variable initialized to 0
  ;; (global $g (mut i32) (i32.const 0))

  ;; Function that adds 42 to its argument
  (func $store42
    i32.const 42
    f32.const 422
    f32.store)

  ;; Main function that increments global g and calls add42(10)
  (func $main
    i32.const 42
    i32.const 14
    i32.store

    i32.const 10
    i32.const 36
    i32.store

    call $store42)

  ;; Export the main function
  (export "main" (func $main))
)