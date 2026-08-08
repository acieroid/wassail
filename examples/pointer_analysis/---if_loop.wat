(module
        (memory (export "mem") 1)

        (func $main (export "main") (result i32) (local $i i32) (local $res i32)
          ;; i = 0
          i32.const 0
          local.set $i

          block
            loop
              ;; if i == 0:
              ;;   res = 10
              ;; else:
              ;;   res = 20
              local.get $i
              i32.eqz
              if
                i32.const 10
                local.set $res
              else
                i32.const 20
                local.set $res
              end

              ;; i++
              local.get $i
              i32.const 1
              i32.add
              local.tee $i

              ;; continue while i < 2
              i32.const 2
              i32.lt_s
              br_if 0
            end
          end

          local.get $res
        )
      )