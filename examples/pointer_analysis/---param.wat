(module
(memory (export "mem") 1)
  (func $main (export "main") (param $x i32) (result i32) (local $y i32)
          local.get $x
          local.set $y

          i32.const 14
          local.set $x

          i32.const 44
          local.get $y
          local.tee $x
          i32.store

          local.get $x
        )
      )