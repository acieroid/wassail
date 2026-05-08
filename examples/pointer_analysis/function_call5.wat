(module
  (memory (export "mem") 1)
  ;; Define a mutable global variable initialized to 0
  (global $g (mut i32) (i32.const 0))

  ;; Function that adds its arguments together and adds 42
  (func $add42 (param $x i32) (param $y i32) (result i32)
    local.get $x
    i32.const 90
    i32.add
    i32.load
    local.get $y
    i32.sub
    i32.const 42
    i32.add
    return)

  ;; Main function that increments global g and calls add42(10)
  (func $main (param $x i32) (result i32)
    i32.const 10
    ;; local.get $x
    i32.const 14
    call $add42
    return)

  ;; Export the main function
  (export "main" (func $main))
)