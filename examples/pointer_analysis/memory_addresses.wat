(module
  (memory (export "mem") 1)
  ;; (global $g0 (mut i32) (i32.const 1024))

  (func $main (export "main") (param $x i32) (local $l1 i32) (local $l2 i32)
    local.get $x
    if (result i32 i32)
      i32.const 1
      i32.const 3
    else
      i32.const 9
      i32.const 11
    end
    local.set $l1   ;; 8[0,1]+1
    local.set $l2   ;; 8[0,1]+3



    local.get $l1   ;; 8[0,1]+1
    local.get $l2
    i32.store       ;; weak update
    
  )
)
