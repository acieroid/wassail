(module
      (memory (export "mem") 1)
      (global $g (mut i64) (i64.const 0))

        (func $main (export "main") (result i64) (local $l0 i64)
          i64.const 14
          local.tee $l0
          global.set $g
          global.get $g
          local.get $l0
          i64.add
        )
      )