(module
        (memory (export "mem") 1)

        (func $main (export "main") (result i32)
          i32.const 1
          memory.grow
        )
      )