(module
  (memory (export "mem") 1)

  (func $main (export "main") (result i32)

    ;; y := 14
    i32.const 0 ;; &y
    i32.const 14
    i32.store

    ;; z := 1
    i32.const 4 ;; &z
    i32.const 1
    i32.store

    ;; if (cond) p := &y else p := &z
   
    block ;; outer block
      block ;; inner block
        i32.const 1 ;; condition
        i32.eqz       ;; if condition == 0
        br_if 0       ;; branch to label 1 (skip then, go to else)
        ;; then: p := &y
        i32.const 8   ;; &p
        i32.const 0   ;; &y
        i32.store
        br 1          ;; skip the else block
      end             ;; end inner block
      ;; else: p := &z
      i32.const 8     ;; &p
      i32.const 4     ;; &z
      i32.store
    end               ;; end outer block

    ;; *p := 2
    i32.const 8       ;; &p
    i32.load          ;; load value of p
    i32.const 2
    i32.store

    i32.const 500
    return
  )
)