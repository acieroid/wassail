(module
        (memory (export "mem") 1)

        (func $main (export "main") (result i32)
          unreachable

          i32.const 42
        )
      )