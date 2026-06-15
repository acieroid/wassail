(module
        (memory (export "mem") 1)

        (func $main (export "main") (result i32)
          i32.const 4
          i32.const 14
          i32.store

          i32.const 8
          i32.const 42
          i32.store8

          i32.const 4
          i32.load
        )
      )