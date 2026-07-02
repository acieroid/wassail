(module
  (memory (export "mem") 1)
  (global $g0 (mut i32) (i32.const 1024))

  (func $main (export "main") (param $l0 i32) (result i32) (local $l1 i32)
    i32.const 42            ;; [42]
    local.tee $l0           ;; [42]        l0=42
    global.get $g0          ;; [g0; 42]
    i32.add                 ;; [g0+42]
    return
  )
)

