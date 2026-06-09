(module
  (memory (export "mem") 1)
  (global $g0 (mut i32) (i32.const 1024))

  (func $main (export "main") (param $l0 i32) (result i32) (local $l1 i32)
    ;; constant - constant
    i32.const 14
    i32.const 3
    i32.sub ;; 11
    drop

    ;; constant - (negative constant)
    i32.const 42
    i32.const -13
    i32.sub ;; 55
    drop

    ;; constant - global
    i32.const 26
    global.get $g0
    i32.sub ;; 26+negg0
    drop

    ;; constant - param
    i32.const 126
    local.get $l0
    i32.sub ;; 126+negl0
    drop

    ;; constant - local
    i32.const 1024
    local.get $l1
    i32.sub ;; 1024
    drop

    ;; global - cst
    global.get $g0
    i32.const 26
    i32.sub ;; g0 - 26
    drop

    ;; param - cst
    local.get $l0
    i32.const 126
    i32.sub ;; l0 - 126
    drop

    ;; local - cst
    local.get $l1
    i32.const 1024
    i32.sub ;; - 1024
    drop

    ;; global - param
    global.get $g0
    local.get $l0
    i32.sub ;; g0+negl0
  )
)

