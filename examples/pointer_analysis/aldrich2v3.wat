(module
  (memory (export "mem") 1)

  (func $main (export "main") (result i32)   

    ;; Code snippet:
    ;; y := 14
    ;; z := 1
    ;; if (cond) p := &y else p := &z
    ;; *p := 2

    ;; y := 14
    i32.const 0 ;; &y
    i32.const 14
    i32.store

    ;; z := 1
    i32.const 4 ;; &z
    i32.const 1
    i32.store

    
    (if
      (i32.const 1) ;; condition)
      (then
        ;; p := &y
        i32.const 8 ;;&p
        i32.const 0 ;; &y
        i32.store
      )
      (else
        ;; p := &z
        i32.const 8 ;; &p
        i32.const 4 ;; &z
        i32.store
      )
    )

    ;; *p := 2
    i32.const 8 ;; &p
    i32.load    ;; &y or &z
    i32.const 2 ;; new value
    i32.store   ;; store new value where p is pointing

    i32.const 500
    return
  )
)

