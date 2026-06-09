(module
  (memory (export "mem") 1)
  (global $g0 (mut i32) (i32.const 1024))
  (global $g1 (mut i32) (i32.const 1024))

  (func $main (export "main") (param $l0 i32) (result) (local $l1 i32)
    ;; global = param
    local.get $l0
    global.set $g0
    
    ;; global = const
    i32.const 14
    global.set $g1
  )
)

