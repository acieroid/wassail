(module
  (memory (export "mem") 1)

  (func $main (export "main")
    i32.const 42
    i64.const 66
    i64.store

    i32.const 42
    i64.const 66
    i64.store8

    i32.const 42
    i64.const 66
    i64.store16

    i32.const 42
    i64.const 66
    i64.store32

    i32.const 42
    i32.const 36
    i32.store16

    i32.const 42
    i32.const 36
    i32.store8

    i32.const 42
    i32.const 36
    i32.store

    i32.const 42
    i32.load16_u

    i32.const 42
    i32.load

    i32.shl

    drop
  )
)