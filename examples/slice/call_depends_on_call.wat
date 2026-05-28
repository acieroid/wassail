(module
  (memory (export "mem") 1)
  (type $t (func (param i32) (result i32)))
  (type $t2 (func (result i32)))
  (global $g0 (mut i32) (i32.const 1024))
  (global $g1 (mut i32) (i32.const 1024))
  (global $g2 (mut i32) (i32.const 1024))


  ;; Function $f0 (stores at address g0+14):
  (func $f0 (type $t) (param i32) (result i32)
    global.get $g0
    i32.const 14
    i32.add
    i32.const 1000
    i32.store
    i32.const 66)

  ;; Function $f1 (reads at address g0):
  (func $f1 (type $t) (param i32) (result i32)
    global.get $g0
    i32.load
    )

  ;; Function $f2 (reads at address g0+8): 
  (func $f2 (type $t2) (result i32)
    global.get $g0
    i32.const 8
    i32.add
    i32.load)

  ;; Table of functions for indirect call
  (table 3 funcref)
  (elem (i32.const 0) $f0 $f1 $f2)

  ;; Main function: decides which function to call indirectly
  (func (export "main") (result i32)
    i32.const 0
    call $f0
    drop

    i32.const 0
    call $f1 (; should not depend on call $f0;)
    drop

    i32.const 14
    global.get $g0
    i32.add
    global.set $g0
    i32.const 0
    call $f1 (; should depend on call $f0;)
    drop

    global.get $g0
    i32.const 8
    i32.sub
    global.set $g0
    call $f2 (; should depend on call $f1;)
    drop

    i32.const 50
    global.get $g0
    i32.add
    global.set $g0
    call  $f2 (; should not depend on call $f1;)


  )
)