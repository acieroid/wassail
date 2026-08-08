(module
  (func $add (param $a i32) (param $b i32) (result i32)
    local.get 0
    i32.const 2
    i32.shl
    
    local.get 1
    i32.add)
  (export "add" (func $add))
  (func $main (result i32)
    i32.const 5
    i32.const 2
    call $add
  )
  (export "main" (func $main))
)