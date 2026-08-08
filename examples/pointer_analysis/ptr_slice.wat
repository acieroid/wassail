(module
  (memory (export "mem") 1)
  ;; Define a mutable global variable initialized to 0
  (global $g (mut i32) (i32.const 0))

  ;; times 4
  (func $f (param $x i32) (result i32)
    local.get $x
    i32.const 4
    i32.mul
    return)

  (func $g (param $x i32)
    return)
  
  (func $main (result i32)
    i32.const 14
    call $f 
    ;; call $g

    ;; i32.const 45
    return)

  ;; Export the main function
  (export "main" (func $main))
)