(module
        (memory (export "mem") 1)
        (global $g (mut i32) (i32.const 0))

        (func $is_even (param $x i32) (result i32)
          local.get $x
          i32.eqz
          if (result i32)
            i32.const 1
          else
            local.get $x
            i32.const 1
            i32.sub
            call $is_odd
          end
        )

        (func $is_odd (param $x i32) (result i32)
          local.get $x
          i32.eqz
          if (result i32)
            i32.const 0
          else
            local.get $x
            i32.const 1
            i32.sub
            call $is_even
          end
        )

        (func $main (result i32)
          i32.const 14
          call $is_even)

        ;; Export the main function
        (export "main" (func $main))
      )