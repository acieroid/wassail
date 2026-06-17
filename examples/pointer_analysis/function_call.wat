(module
        (memory (export "mem") 1)
        (global $g (mut i32) (i32.const 0))

        ;; Function that increments g by 166, stores 14 at address g, and adds 42 to its argument
        (func $add42 (param $x i32) (result i32)
          ;; Increment global g by 166
          global.get $g
          i32.const 166
          i32.add
          global.set $g

          ;; store 14 at address 14
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
          global.get $g
          i32.const 99
          i32.store

          ;; Call add42 with 10
          i32.const 10
          call $add42
          return)

        ;; Export the main function
        (export "main" (func $main))
      )