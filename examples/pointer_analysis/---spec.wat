(module
  (memory (export "mem") 1)
  (global $g (mut i32) (i32.const 0))

  (func $f 
    i32.const 42
    global.set $g
  )

  (func $main (export "main") (param $l0 i32) (result i32) (local $l1 i32) (local $l2 i32)
    global.get $g         ;; g=g0;  l0=l0;  l1=0;  l2=0
    local.set $l0         ;; g=g0;  l0=g0;  l1=0;  l2=0

    i32.const 14        
    global.set $g         ;; g=14;  l0=g0;  l1=0;  l2=0

    global.get $g
    local.set $l1         ;; g=14;  l0=g0;  l1=14;  l2=0

    call $f               ;; g=42;  l0=g0;  l1=14;  l2=0

    global.get $g
    local.set $l2         ;; g=42;  l0=g0;  l1=14;  l2=42

    i32.const 99
    global.set $g         ;; g=99;  l0=g0;  l1=14;  l2=42

    local.get $l0         ;; g=99;  l0=g0;  l1=14;  l2=42;  ret=g0
  )
)

