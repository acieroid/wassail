(module
        (memory (export "mem") 1)
        (global $g0 (mut i32) (i32.const 1024))

        (func $main (export "main") (param $l0 i32) (result i32) (local $l1 i32)
    ;; add two constants:
          i32.const 42
          i32.const 14
          i32.add 
          drop

    ;; add a constant and a global:
          i32.const 99
          global.get $g0
          i32.add
          drop

    ;; add a constant and a parameter:
          i32.const 77
          local.get $l0
          i32.add
          drop

    ;; add a constant and a local
          i32.const 5
          local.get $l1
          i32.add
          drop

    ;; add a global and a parameter:
          global.get $g0
          local.get $l0
          i32.add
        )
      )