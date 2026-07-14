(module
  (func $main (export "main") (param $x i32) (result i32)
    local.get $x
    if (result i32)
      i32.const 10
    else
      i32.const 2
    end
    i32.clz
  )
)