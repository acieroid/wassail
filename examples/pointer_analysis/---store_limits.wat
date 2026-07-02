(module
  (memory (export "mem") 65536)

  (func $main (export "main")
    i32.const 0x7FFFFFFF
    i32.const 14
    i32.store
    i32.const 0x7FFFFFFF
    i32.load
    drop

    i32.const 0x7FFFFFFE
    i32.const 26
    i32.store
    i32.const 0x80000000
    i32.const 42
    i32.store

    i32.const 0x7FFFFFFF
    i32.load
    drop
  )
)

