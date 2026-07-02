(module
  (memory (export "mem") 1)
  ;;(global $g0 (mut i32) (i32.const 1024))

  (func $main (export "main") (param $l0 i32) (param $l1 i32) (result i32)
    i32.const 14
    return
    local.get $l0
    i32.add
    i32.const 36
    local.get $l1
    i32.add
    i32.lt_u
    if (result i32)
      local.get $l0
    else
      local.get $l1
    end
    return
    i32.const 32 ;; this should not be permitted 
    return
  )
)
