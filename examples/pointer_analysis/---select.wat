(module
        (memory (export "mem") 1)

        (func $main (export "main") (result i32)
          ;; true condition keeps first value
          i32.const 10
          i32.const 20
          i32.const 1
          select
          drop

          ;; false condition keeps second value
          i32.const 10
          i32.const 20
          i32.const 0
          select
        )
      )