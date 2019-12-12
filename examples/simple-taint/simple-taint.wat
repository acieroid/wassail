(module
  (type (;0;) (func (param i32)))
  (type (;1;) (func (param i32) (result i32)))
  (func (;0;) (type 0) (param i32)
    ;; Summary:
    ;; vstack: [p0] -> []
    ;; globals: [g0] -> [g0]
    ;; heap: {g0-16: _} -> {g0-16: p0}
    global.get 0 ;; [g0]
    i32.const 16 ;; [16, g0]
    i32.sub ;; [g0-16]
    local.get 0 ;; [p0, g0-16]
    i32.store offset=12) ;; [] + {g0-16: _} -> {g0-16: p0}
  (func (;1;) (type 0) (param i32)
    global.get 0
    i32.const 16
    i32.sub
    local.get 0
    i32.store offset=12)
  (func (;2;) (type 1) (param i32) (result i32)
    (local i32)
    ;; Summary:
    ;; Vstack: [p0] -> [1]
    ;; Globals: [g0] -> [g0]
    ;; Heap: {g0-16: _ * g0-32: _ * emp} -> {g0-16: p0, g0-32: p0 * emp}
    global.get 0 ;; [g0]
    i32.const 16 ;; [16, g0]
    i32.sub ;; [g0-16]
    local.tee 1 ;; [g0-16]
    ;; locals: [p0, g0-16]
    global.set 0 ;; []
    ;; globals: [g0-16]
    local.get 1 ;; [g0-16]
    local.get 0 ;; [p0, g0-16]
    i32.store offset=12 ;; []
    ;; heap: {g0-16: _} -> {g0-16: p0}
    local.get 1 ;; [g0-16]
    i32.load offset=12 ;; [p0]
    call 0 ;; []
    ;; heap: {g0-32: _} -> {g0-32: p0}
    local.get 1 ;; [g0-16]
    i32.const 16 ;; [16, g0-16]
    i32.add ;; [g0]
    global.set 0 ;; []
    ;; global: [g0]
    i32.const 1) ;; [1]
  (func (;3;) (type 1) (param i32) (result i32)
    ;; SUMMARY:
    ;; vstack: [p0] -> [p0]
    ;; Heap: {g0-16: _ * emp} --> heap: {g0-16: p0 * emp}
    ;; globals: [g0] -> [g0]
    (local i32)
    ;; globals: [g0]
    ;; locals: [p0, 0]
    ;; stack: []
    global.get 0
    ;; globals: [g0]
    ;; locals: [p0, 0]
    ;; stack: [g0]
    i32.const 16
    ;; stack: [16, g0]
    i32.sub
    ;; stack: [g0-16]
    local.tee 1
    ;; locals: [p0, g0-16]
    local.get 0
    ;; stack: [p0, g0-16]
    i32.store offset=12
    ;; heap: {g0-16: p0}
    ;; stack: []
    local.get 1
    ;; stack: [g0-16]
    i32.load offset=12
    ;; stack: [p0]
    )
  (func (;4;) (type 0) (param i32)
    (local i32)
    global.get 0
    i32.const 16
    i32.sub
    local.tee 1
    global.set 0
    local.get 1
    local.get 0
    i32.store offset=12
    local.get 1
    i32.load offset=12
    call 1
    local.get 1
    i32.const 16
    i32.add
    global.set 0)
  (func (;5;) (type 1) (param i32) (result i32)
    (local i32)
    global.get 0
    i32.const 16
    i32.sub
    local.tee 1
    local.get 0
    i32.store offset=8
    block  ;; label = @1
      block  ;; label = @2
        local.get 1
        i32.load offset=8
        i32.const 2
        i32.rem_s
        br_if 0 (;@2;)
        local.get 1
        i32.const 1
        i32.store offset=12
        br 1 (;@1;)
      end
      local.get 1
      i32.const 0
      i32.store offset=12
    end
    local.get 1
    i32.load offset=12)
  (table (;0;) 1 1 funcref)
  (memory (;0;) 2)
  (global (;0;) (mut i32) (i32.const 66560))
  (export "memory" (memory 0))
  (export "safe" (func 2))
  (export "unsafe1" (func 3))
  (export "unsafe2" (func 4))
  (export "unsafe3" (func 5)))
