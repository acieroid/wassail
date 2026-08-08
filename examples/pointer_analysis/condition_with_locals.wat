(module
        (memory (export "mem") 1)

        (func $main (export "main") (param $g i32)
          local.get $g
          if
          i32.const 0
          local.set $g
          else
          i32.const 6
          local.set $g
          end
          local.get $g
          i32.eqz
          if (result i32)
            local.get $g
            local.get $g
            i32.gt_s
            if (result i32)
              i32.const 1
            else
              i32.const 2
            end
          else
            i32.const 3
          end
          drop
        )
      )