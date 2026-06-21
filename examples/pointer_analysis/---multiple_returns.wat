(module
        (memory (export "mem") 1)

        (func $f1 (export "f1") (result i32 i32)
          i32.const 42
          i32.const 14
          ;; return
        )
        (func $f2 (export "f2") (result i32 i32)
          i32.const 42
          i32.const 14
          ;; return
        )
        (func $main (export "main")
          call $f1
          i32.store
        )
      )