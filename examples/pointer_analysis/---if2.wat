(module
          (memory (export "mem") 1)

          (func $main (export "main") (result i32) (local $l1 i32) (local $l2 i32) (local $l3 i32)
            i32.const 0
            if (result i32)
              i32.const 99     ;; unreachable
              local.set $l1    ;; unreachable
              i32.const 1      ;; unreachable
            else
              i32.const 2
            end
                                                ;; [2]
            local.get $l1      ;; i0_7 = 0         [0; 2]
            local.set $l3                       ;; [2]

            local.tee $l1      ;; l1 = 2           [2]
            i32.const 2                         ;; [2, 2]
            i32.lt_u                            ;; [0]
            i32.const 5                         ;; [5; 0]
            local.tee $l2      ;; l2 = 5           [5; 0]
            i32.const 3                         ;; [3; 5; 0]
            i32.gt_u                            ;; [1; 0]
            i32.and                             ;; [0]
            if (result i32)
              i32.const 6   ;; unreqchable
            else
              i32.const 7                       ;; [7]   
            end
            return          ;; ret0_0 = 7
          )
        )