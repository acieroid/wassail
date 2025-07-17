(module
  (memory (export "mem") 1)
  ;;(global $g0 (mut i32) (i32.const 1024))

  (func $main (export "main") (param $l0 i32) (result i32) (local $l1 i32) (local $l2 i32)
    i32.const 1
    if
      i32.const 14
      local.set $l0
      i32.const 42
      local.set $l1
    else
      i32.const 100
      local.set $l0
      i32.const 36
      local.set $l1
    end
    local.get $l0
    local.get $l1
    i32.le_u
    ;; i32.gt_u
    if
      local.get $l0  
    else
      local.get $l0
    end
    i32.const 10
    i32.const 42
    i32.le_u
    if
      local.get $l0
    else
      local.get $l1
    end
    return
  )
)
