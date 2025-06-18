(module
  (memory (export "mem") 1)
  (global $g0 (mut i32) (i32.const 1024))

  (func $main (export "main") (param $l0 i32) (result i32) (local $l1 i32)
    ;;i32.const 3000
    i32.const 1
    i32.const 2
    i32.const 3
    i32.const 14
    local.set $l0
    i32.const 124
    global.set $g0
    i32.const 42
    local.get $l0
    i32.lt_u
    if (result i32)
      i32.const 15
      local.set $l0
      i32.const 72 ;; then-branch result
      i32.const 7
      ;;global.set $g0
    else
      i32.const 132
      i32.const 13 ;; else-branch result
      ;;global.set $g0
    end
    global.set $g0
    global.set $g0
    global.get $g0
    return
  )
)
