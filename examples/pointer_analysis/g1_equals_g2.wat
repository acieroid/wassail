(module
  (memory (export "mem") 1)
  (global $__stack_pointer (mut i32) (i32.const 1024))
  (global $part1Value i32 (i32.const 0))
  (global $part2Value i32 (i32.const 1))

  (func $main (export "main") (result i32)
    global.get $__stack_pointer
    global.set $part1Value
    i32.const 42
    global.set $__stack_pointer
    global.get $__stack_pointer
    global.set $part1Value
    global.get $part1Value
    return
  )
)

