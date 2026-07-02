(module
  (memory (export "mem") 1)
  ;; Define a mutable global variable initialized to 0
  ;; (global $g (mut i32) (i32.const 0))

  ;; Function that adds 42 to its argument
  (func $f1 (param $x i32) (result i32)
    local.get $x
    i32.const 19
    i32.lt_u
    if (result i32)
      i32.const 100
    else
      i32.const 50
    end
    return)

  (func $f2 (param $x i32) (result i32)
    i32.const 34
    return)

  ;; Main function that increments global g and calls add42(10)
  (func $main (param $x i32) (result i32) (local $l0 i32)
    local.get $x
    if (result i32)
      i32.const 14
    else
      i32.const 27
    end
    call $f1
  )

  ;; Export the main function
  (export "main" (func $main))
)