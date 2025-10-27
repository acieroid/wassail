(module
  (memory (export "mem") 1)

  (func $main (export "main") (param $l0 i32) (result i32) (local $l1 i32)
    local.get $l0
    local.get $l1
    i32.const 14
    local.set $l0
    local.get $l0
    local.set $l0
    local.set $l0
    local.set $l0
    local.get $l0
    return
  )
)

