(module
  (memory (export "mem") 1)
  (global $g0 (mut i32) (i32.const 1024))

  (func $main (export "main") (param $l0 i32) (result i32) (local $l1 i32)
    i32.const 42
    local.tee $l0
    global.get $g0
    i32.sub
    global.get $g0
    i32.add
    return
  )
)

