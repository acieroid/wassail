(module
  (memory (export "mem") 1)
  (global $g0 (mut i32) (i32.const 1024))

  (func $main (export "main") (param $l0 f32) (param $l1 f64) (param $l2 i64)
    f32.const 42
    local.set $l0
    i32.const 14
    local.get $l0
    i32.store

    f64.const 42
    local.set $l1
    i32.const 36
    local.get $l1

    i64.const 66
    local.set 2
    local.get 2
    i32.const 99
    i32.store
  )
)