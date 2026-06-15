(module
  (func $main (export "main") (param $x i32) (result i32) (local $y i32)
          local.get $x
          local.set $y

          i32.const 14
          local.set $x

          local.get $y
          local.set $x

          local.get $x
        )
      )