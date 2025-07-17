(module
  ;; Define a mutable global variable initialized to 0
  (global $g (mut i32) (i32.const 0))

  ;; Function that adds 42 to its argument
  (func $add42 (param $x i32) (result i32)
    ;; Increment global g by 1
    global.get $g
    i32.const 1
    i32.add
    global.set $g

    ;; store 14 at address 42
    ;; i32.const 42
    global.get $g
    i32.const 14
    i32.store

    ;; Add 42 to the argument and return
    local.get $x
    i32.const 42
    i32.add
    return)

  ;; Main function that increments global g and calls add42(10)
  (func $main (result i32)
    ;; Increment global g by 1
    ;; i32.const 14
    ;; global.set $g

    ;; store some value at address 39 (spanning addresses 39, 40, 41 and 42)
    ;; i32.const 39
    global.get $g
    i32.const 99
    i32.store

    ;; store some value at address 35 (spanning addresses 35, 36, 37 and 38)
    i32.const 35
    i32.const 102
    i32.store

    ;; Call add42 with 10
    i32.const 10
    call $add42
    return)

  ;; Export the main function
  (export "main" (func $main))
)