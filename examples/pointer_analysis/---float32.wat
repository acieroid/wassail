(module
        (memory (export "mem") 1)

        (func $main (export "main") (result f32) (local $x f32)
          f32.const 3.1416
          local.set $x
          local.get $x
        )
      )