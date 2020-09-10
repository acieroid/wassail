(module
  (type (;0;) (func (param i32)))
  (type (;1;) (func (param i32) (result i32)))
  (type (;2;) (func (result i32)))
  (type (;3;) (func (param i32 i32) (result i32)))
  (type (;4;) (func (param i32 i32 i32) (result i32)))
  (type (;5;) (func (param i64) (result i32)))
  (func (;test-id-localget;) (type 1) (param i32) (result i32) ;; 0
    local.get 0
    ;; summary: ret = l0
  )
  (func (;test-id-call;) (type 1) (param i32) (result i32) ;; 1
    local.get 0
    call 0
    ;; summary: ret = l0
  )
  (func (;test-localset;) (type 1) (param i32) (result i32) ;; 2
    (local i32)
    i32.const 256
    local.set 1
    local.get 1
    ;; summary: ret = 256
    )
  (func (;test-id-store-load;) (type 1) (param i32) (result i32) ;; 3
    global.get 0
    local.get 0
    i32.store
    global.get 0
    i32.load
    ;; summary: ret = l0
  )
  (func (;store-seq;) (type 1) (param i32) (result i32) ;; 4
    (local i32)
    global.get 0
    local.set 1  ;; l1 = g0
    local.get 0  ;; [l0]
    i32.const 1  ;; [1, l0]
    i32.store    ;; l0 -> 1
    local.get 0  ;; [l0]
    local.get 1  ;; [g0, l0]
    i32.const 16
    i32.sub      ;; [g0-16, l0]
    i32.const 8
    i32.add      ;; [g0-8, l0]
    i32.store offset=4 ;; l0+4 -> g0-8
    local.get 0
    i32.load
    ;; summary: ret = 1, mem: [l0 -> 1, l0+4 -> g0-8]
    )
  (func (;store-branch;) (type 1) (param i32) (result i32) ;; 5
    global.get 0
    local.get 0
    if (result i32)
      i32.const 0
    else

      i32.const 1
    end
    i32.store
    global.get 0
    i32.load
    ;; summary: ret = [0,1], mem: g0 -> [0,1]
  )
  (func (;store-different-branches;) (type 1) (param i32) (result i32) ;; 6
  ;; This example is where it is important to get the definitions right:
  ;; On one branch, we write to the store, on the other we do not.
  ;; Hence, after the if, there is a join which in theory should result in the store being []
  ;; And the return value is then top
    local.get 0
    if
      global.get 0
      i32.const 42
      i32.store
    else
      nop
    end
    global.get 0
    i32.load
    ;; summary: ret = top
  )
  (table (;0;) 1 1 funcref)
  (memory (;0;) 2)
  (global (;0;) (mut i32) (i32.const 66560))
  (export "_start" (func 0))
  (export "memory" (memory 0)))
