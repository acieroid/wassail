(module
  (memory (export "mem") 1)
  (global $__stack_pointer (mut i32) (i32.const 1024))
  (global $part1Value i32 (i32.const 0))
  (global $part2Value i32 (i32.const 1))

  (func $main (export "main") (param $l0 i32) (result i32) (local $l1 i32)
    local.get $l0
    local.set $l1
    i32.const 42
    local.set $l0
    local.get $l0
    local.set $l1
    global.get $part1Value
    return
  )
)

