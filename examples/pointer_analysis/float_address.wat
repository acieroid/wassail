(module
  (memory (export "mem") 1)
  ;;(global $g0 (mut i32) (i32.const 1024))

  (func $main (export "main")
    i64.const 42
    i32.const 36
    i32.store
  )
)