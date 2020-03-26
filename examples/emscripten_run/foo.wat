(module
  (type (;2;) (func (param i32) (result i32)))
  (func (;throughstore;) (type 0) (param i32) (result i32)
    (local i32)
    global.get 0 ;; [g0]
    i32.const 16 ;; [16, g0]
    i32.sub ;; [g0-16]
    local.tee 1 ;; locals: [p0, g0-16]
    local.get 0 ;; [p0]
    i32.store offset=12 ;; {g0-4: [b0,b1,b2,b3]} -> {g0-4: [p0.b0, p0.b1, p0.b2, p0.b3]}
    local.get 1 ;; [g0-16]
    i32.load offset=12) ;; p0
  (func (;direct;) (type 0) (param i32) (result i32)
    local.get 0) ;; Summary: [p0] -> [p0]
  (func (;global;) (type 0) (param i32) (result i32)
    global.get 0) ;; Summary [p0] -> [g0]
  (table (;0;) 1 1 funcref)
  (memory (;0;) 2)
  (global (;0;) (mut i32) (i32.const 66576))
  (export "memory" (memory 0))
  (export "throughstore" (func 1))
  (export "direct" (func 2))
  (export "global" (func 3))
  (data (;0;) (i32.const 1024) "foo\00"))
