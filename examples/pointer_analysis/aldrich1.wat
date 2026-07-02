(module
  (memory (export "mem") 1)

  (func $main (export "main") (result i32)   

    ;; Code sippet:
    ;; z := 1
    ;; p := &z
    ;; *p := 2

    ;; z := 1
    i32.const 0 ;; &z
    i32.const 1
    i32.store

    ;; p := &z
    i32.const 4 ;; &p
    i32.const 0 ;; &z
    i32.store 

    ;; *p := 2
    i32.const 4 ;; &p
    i32.load    ;; &z
    i32.const 2
    i32.store 

    ;; *p := 22
    i32.const 4 ;; &p
    i32.load    ;; &z
    i32.const 22
    i32.store 

    i32.const 0
    return
  )
)

