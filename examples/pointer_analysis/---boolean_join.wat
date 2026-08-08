(module
        (memory (export "mem") 1)

        (func $main (export "main") (param $y i32) (result i32) (local $x i32)
          local.get $y
          if 
            i32.const 14
            local.set $x
          else
            i32.const 0
            local.set $x
          end

          local.get $y
          if (result i32)
            local.get $x
            i32.eqz
          else
            local.get $x
            if (result i32)
              i32.const 26
            else
              i32.const 3
            end
            local.get $x
            i32.lt_s
          end
        )
      )