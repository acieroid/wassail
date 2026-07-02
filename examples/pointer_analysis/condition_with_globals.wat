(module
        (memory (export "mem") 1)
        (global $g (mut i32) (i32.const 0))

        (func $main (export "main")
          global.get $g
          if
          i32.const 0
          global.set $g
          else
          i32.const 6
          global.set $g
          end
          global.get $g
          i32.eqz
          if (result i32)
            global.get $g
            i32.const 0
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