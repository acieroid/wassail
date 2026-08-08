(module
  (memory (export "mem") 1)

  (func $main (export "main") (param $l0 i32) (result) (local $l1 i32)
    ;; global = param
    local.get $l0
    local.set $l1
    
    ;; global = const
    i32.const 14
    local.set $l1

    local.get $l1
    drop
  )
)

