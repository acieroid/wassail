(module
  (memory (export "mem") 1)
  (global $g0 (mut i64) (i64.const 1024))
  (global $g1 (mut i32) (i32.const 14))

  (func $main (export "main") (param $x i64) (param $y i32) (result i64) (local $l2 i64) (local $l3 i32)
    global.get $g1
    drop
    global.get $g0
  )
)

