(module
        (func $main (export "main") (param $x i32) (result i32) (local $l1 i32) (local $l2 i32)
          local.get $x
          if 
            i32.const 14
            local.set $l1
          else     
            i32.const 42
            local.set $l2 
          end
          local.get $l1
          i32.eqz
          local.get $l2
          i32.eqz
          i32.add
        )
      )