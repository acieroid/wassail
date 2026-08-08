(module
  (memory (export "mem") 1)
  (type $t (func (param i32) (result i32)))
  (type $t2 (func (result i32)))
  (global $g0 (mut i32) (i32.const 1024))
  (global $g1 (mut i32) (i32.const 1024))
  (global $g2 (mut i32) (i32.const 1024))


  ;; Function $f0 (reads at address 14):
  (func $f0 (type $t) (param i32) (result i32)
    ;; local.get 0
    i32.const 14
    i32.load
    drop
    local.get 0
    global.set $g0
    i32.const 14)

  ;; Function $f1 (reads at address 99):
  (func $f1 (type $t) (param i32) (result i32)
    local.get 0
    i32.const 85
    i32.add
    i32.load
    drop
    local.get 0
    global.set $g1
    i32.const 66)

  ;; Function $f2: 
  (func $f2 (type $t2) (result i32)
    i32.const 99
    i32.const 99
    i32.store

    global.get $g1
    drop

    i32.const 26
    global.set $g2
    i32.const 26)

  ;; Table of functions for indirect call
  (table 3 funcref)
  (elem (i32.const 0) $f0 $f1 $f2)

  ;; Main function: decides which function to call indirectly
  (func (export "main") (result i32)
    i32.const 14
    global.get $g0
    (call_indirect (type $t))
    
  )
)