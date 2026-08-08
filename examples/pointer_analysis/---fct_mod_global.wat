(module
        (memory (export "mem") 1)
        (global $g (mut i32) (i32.const 0))

        ;; Function that increments g by 166, stores 14 at address g, and adds 42 to its argument
        (func $f
          i32.const 14
          global.set $g
        )

        ;; Main function that increments global g and calls add42(10)
        (func $main (result i32) (local $l0 i32)
          i32.const 42
          global.set $g

          call $f

          global.get $g
          local.set $l0
          i32.const 99
          global.get $g
          i32.store
          i32.const 99
          i32.load
        )

        ;; Export the main function
        (export "main" (func $main))
      )