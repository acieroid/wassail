(module
  (memory (export "mem") 1)
  (global $g0 (mut i32) (i32.const 1024))

  (func $main (export "main") (param $l0 i32) (result i32) (local $l1 i32)
    i32.const 36
    global.get $g0
    i32.store offset=8
    global.get $g0
    i32.load offset=8
    i32.const 4
    i32.add
    global.get $g0
    i32.store offset=112
  )
)

