(module
  (memory (export "mem") 1)
  (global $g (mut i32) (i32.const 0))

  (func $f 
    i32.const 14
    global.set $g
  )

  (func $main (export "main") (param $l0 i32) (result i32) (local $l1 i32)
    global.get $g
    local.set $l1

    i32.const 14
    global.set $g

    global.get $g
    local.set $l0

    call $f

    global.get $g
    local.set $l1

    i32.const 44
    global.set $g

    local.get $l0
  )
)

