(module
  (type (;0;) (func (param i32)))
  (type (;1;) (func (param i32) (result i32)))
  (func (;test-select;) (type 1) (param i32) (result i32)
    i32.const 256
    i32.const 512
    i32.const 0
    select) ;; summary: returns 512
  (func (;test-store;) (type 1) (param i32) (result i32)
    global.get 0
    local.get 0
    i32.store offset=12
    local.get 0)
  (func (;test-load;) (type 1) (param i32) (result i32)
    global.get 0
    local.get 0
    i32.store offset=12
    global.get 0
    i32.load offset=12
    ;; Expected summary: return value is first argument
    )
  (func (;test-if;) (type 1) (param i32) (result i32)
    local.get 0
    i32.const 0
    i32.eq
    if (result i32)
      i32.const 1
    else
      i32.const 0
    end)
  (func (;test-join;) (type 1) (param i32) (result i32)
    local.get 0
    i32.eqz
    if  ;; label = @1
      i32.const 0
      return ;; at this point, we have memory M, vstack [1]
    end
    i32.const 1200
    local.get 0
    i32.store
    i32.const -1 ;; here we have memory M[1200: p0], vstack: [-1]
    ;; We expect the final summary to be: M, vstack[[-1,1]]
    )
  (table (;0;) 1 1 funcref)
  (memory (;0;) 2)
  (global (;0;) (mut i32) (i32.const 66560))
  (export "memory" (memory 0)))
