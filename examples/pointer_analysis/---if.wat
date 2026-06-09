(module
        (memory (export "mem") 1)

        (func $main (export "main") (result i32) (local $l0 i32)
          i32.const 14
          local.set $l0
        
          i32.const 1
          i32.eqz
          if (result i32)
            i32.const 10
          else
            i32.const 20
          end
        )
      )