(module
(memory (export "mem") 1)
  (func $main (param $i i32) (result i32)
    
    local.get $i       ;; memory:[top] stack:[i]
    i32.const 43       ;; memory:[top] stack:[43; i]
    i32.lt_u           ;; memory:[top] stack:[[0,1]]
    if                 ;; memory:[top] stack:[]
      i32.const 10     ;; memory:[top] stack:[10]
      i32.const 0      ;; memory:[top] stack:[0; 10]
      i32.store        ;; memory:[10:0] stack:[1]
    else
      local.get $i     ;; memory:[top] stack:[i]
      i32.const 43     ;; memory:[top] stack:[43; i]
      i32.lt_u         ;; memory:[top] stack:[[0,1]]
      if               ;; memory:[top] stack:[]
        i32.const 10   ;; memory:[top] stack:[10]
        i32.const 15   ;; memory:[top] stack:[15; 10]
        i32.store      ;; memory:[10:15] stack:[]
      else
        local.get $i   ;; memory:[top] stack:[i]
        i32.const 43   ;; memory:[top] stack:[43; i]
        i32.lt_u       ;; memory:[top] stack:[[0,1]]
        if
          i32.const 10 ;; memory:[top] stack:[10]
          i32.const 30 ;; memory:[top] stack:[30; 10]
          i32.store    ;; memory:[10:30] stack:[]
        else
          i32.const 10 ;; memory:[top] stack:[10]
          i32.const 40 ;; memory:[top] stack:[30; 10]
          i32.store    ;; memory:[10:40] stack:[]
        end
      end
    end                ;; memory:[10:5[0,8]] stack:[]
    i32.const 10       ;; memory:[10:5[0,8]] stack:[10]
    i32.load           ;; memory:[10:5[0,8]] stack:[5[0,8]]                             i0_25=5[0,8]
    local.tee $i       ;; memory:[10:5[0,8]] stack:[5[0,8]]                i=5[0,8]
    i32.const 100      ;; memory:[10:5[0,8]] stack:[100; 5[0,8]]           i=5[0,8]
    i32.store          ;; memory:[10:5[0,20]] stack:[]                     i=5[0,8]
    i32.const 10       ;; memory:[10:5[0,20]] stack:[10]                   i=5[0,8]
    i32.load           ;; memory:[10:5[0,20]] stack:[5[0,20]]              i=5[0,8]     i0_30=5[0,20]
    i32.const 1001     ;; memory:[10:5[0,20]] stack:[1001; 5[0,20]]        i=5[0,8]     i0_30=5[0,20]
    i32.store          ;; memory:[10:[0,1001]] stack:[]                    i=5[0,8]     i0_30=5[0,20]
    i32.const 10       ;; memory:[10:[0,1001]] stack:[10]                  i=5[0,8]     i0_30=5[0,20]
    i32.load           ;; memory:[10:[0,1001]] stack:[[0,1001]]            i=5[0,8]     i0_30=5[0,20]    i0_34=[0,1001]
    i32.const 13       ;; memory:[10:[0,1001]] stack:[13; [0,1001]]        i=5[0,8]     i0_30=5[0,20]    i0_34=[0,1001]
    i32.store          ;; memory:[10:Top] stack:[]                         i=5[0,8]     i0_30=5[0,20]    i0_34=[0,1001]
    i32.const 10       ;; memory:[10:Top] stack:[10]                       i=5[0,8]     i0_30=5[0,20]    i0_34=[0,1001]
    i32.load           ;; memory:[10:Top] stack:[Top]                      i=5[0,8]     i0_30=5[0,20]    i0_34=[0,1001]
  )
  (export "main" (func $main))
)