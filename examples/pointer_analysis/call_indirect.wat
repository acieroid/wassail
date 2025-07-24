(module
  (type $t (func (param i32) (result i32))) ;; common type for all three functions
  (global $g0 (mut i32) (i32.const 1024))
  (global $g1 (mut i32) (i32.const 1024))
  (global $g2 (mut i32) (i32.const 1024))


  ;; Function $zero: always returns -1
  (func $zero (type $t) (param i32) (result i32)
    global.get $g0
    (i32.const -1)
    i32.store
    global.get $g0
    i32.load
    return)

  ;; Function $even: returns param / 2
  (func $even (type $t) (param i32) (result i32)
    global.get $g0
    (i32.const 2)
    i32.store
    global.get $g0
    i32.load
    return)

  ;; Function $odd: 
  (func $odd (type $t) (param i32) (result i32)
    global.get $g0
    (i32.const 3)
    i32.store
    global.get $g0
    i32.load
    return)

  ;; Table of functions for indirect call
  (table 3 funcref)
  (elem (i32.const 0) $zero $even $odd)

  ;; Main function: decides which function to call indirectly
  (func (export "main") (param $x i32) (result i32)
    (local $index i32)

    ;; if x <= 0 then index = 0 (zero)
    (local.get $x)
    (i32.const 0)
    (i32.le_s)
    (if (result i32)
      (then (i32.const 0)) ;; $zero
      (else
        (local.get $x)
        (i32.const 2)
        (i32.rem_u)
        (if (result i32)
          (then (i32.const 2)) ;; $odd
          (else (i32.const 1)) ;; $even
        )
      )
    )
    (local.set $index)

    ;; Perform indirect call using the index
    (local.get $x)        ;; argument
    (local.get $index)    ;; table index
    (call_indirect (type $t))
  )
)