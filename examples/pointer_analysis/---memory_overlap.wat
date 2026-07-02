(module
  (memory (export "mem") 1)

  (func $main (export "main") (param $x i32)
    local.get $x
    if
      i32.const 14
      i32.const 66
      i32.store
    else
      i32.const 15
      i32.const 99
      i32.store
    end
      i32.const 14
      i32.load
      drop
  )
)