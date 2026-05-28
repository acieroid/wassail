(module
  (memory (export "mem") 1)
  ;;(global $g0 (mut i32) (i32.const 1024))

  (func $main (export "main") (param $l0 i32) (result i32) (local $l1 i32)
    local.get $l0
    local.get $l1
    i32.lt_u
    if
      local.get $l0
    else
      local.get $l1
    end
    return
  )
)
