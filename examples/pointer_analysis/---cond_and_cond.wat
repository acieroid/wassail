(module
        (memory (export "mem") 1)

        (func $main (export "main") (param $a i32) (param $b i32) (result i32)
          (local $x i32)
          (local $gt_minus_one i32)
          (local $lt_one i32)

          ;; x = {-4,0,4} = 4[0,2]-4
          local.get $a
          if (result i32)
            i32.const -4
          else
            local.get $b
            if (result i32)
              i32.const 0
            else
              i32.const 4
            end
          end
          local.set $x

          ;; gt_minus_one = (-1 < x)
          i32.const -1
          local.get $x
          i32.lt_s
          local.set $gt_minus_one

          ;; lt_one = (x < 1)
          local.get $x
          i32.const 1
          i32.lt_s
          local.set $lt_one

          ;; (-1 < x) && (x < 1)
          local.get $gt_minus_one
          local.get $lt_one
          i32.and

          if (result i32)
            ;; Here x should be refined to 0.
            local.get $x
            i32.const 10
            i32.add
          else
            i32.const 100
          end
        )
      )