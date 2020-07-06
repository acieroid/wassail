(module
  (type (;0;) (func (param i32)))
  (type (;1;) (func (param i32) (result i32)))
  (type (;2;) (func (result i32)))
  (type (;3;) (func (param i32 i32) (result i32)))
  (type (;4;) (func (param i32 i32 i32) (result i32)))
  (func (;test-select;) (type 1) (param i32) (result i32) ;; 0
    i32.const 256
    i32.const 512
    i32.const 0
    select
    ;; summary: ret = 512
    )
  (func (;test-store;) (type 1) (param i32) (result i32) ;; 1
    global.get 0
    local.get 0
    i32.store offset=12
    local.get 0
    ;; summary: ret = l0, memory[g0] = l0
    )
  (func (;test-load;) (type 1) (param i32) (result i32) ;; 2
    global.get 0
    local.get 0
    i32.store offset=12
    global.get 0
    i32.load offset=12
    ;; ret = p0
    )
  (func (;test-if;) (type 1) (param i32) (result i32) ;; 3
    local.get 0
    i32.const 0
    i32.eq
    if (result i32)
      i32.const 1
    else
      i32.const 0
    end) ;; Expected summary: return value is in [0,1]
  (func (;test-if-return;) (type 1) (param i32) (result i32) ;; 4
    i32.const 5
    i32.const 0
    i32.eq
    if
      i32.const 6
      return
    end
    i32.const 0 ;; expected summary: [0, 6]
    )
  (func (;test-join;) (type 1) (param i32) (result i32) ;; 5
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
  (func (;test-merge-after-if;) (type 1) (param i32) (result i32) ;; 6
    local.get 0
    i32.const 0
    i32.ge_s
    if (result i32)  ;; label = @1
      i32.const 1
    else
      i32.const 0
    end
    drop
    i32.const 0)
  (func (;test-merge-locals-after-if;) (type 1) (param i32) (result i32) ;; 7
    (local i32 i32 i32)
    local.get 0
    i32.eqz
    if
      i32.const 1
      local.set 0
    else
      i32.const 2
      local.set 0
    end
    local.get 0) ;; ret = [1,2]
  (func (;test-merge-globals-after-if;) (type 1) (param i32) (result i32) ;; 8
    (local i32 i32 i32)
    local.get 0
    i32.eqz
    if
      i32.const 1
      global.set 0
    else
      i32.const 2
      global.set 0
    end
    local.get 0) ;; ret = p0
  (func (;test-loop;) (type 1) (param i32) (result i32) ;; 9
    (local i32 i32 i32)
    nop
    loop  ;; label = @1
      i32.const 4
      i32.eqz
      br_if 0 (;@1;)
    end
    i32.const 0)
  (func (;test-if-result;) (type 1) (param i32) (result i32) ;; 10
    (local i32 i32 i32 i32 i32)
    i32.const 0
    if (result i32)  ;; label = @2
      local.get 2
    else
      local.get 1
    end
    local.get 1
    i32.sub
    )
  (func (;test-if-in-block;) (type 1) (param i32) (result i32) ;; 11
    (local i32)
    block (result i32)  ;; label = @1 ;; vstack: []
      local.get 1 ;; vstack [l1]
      if  ;; label = @2
        local.get 0 ;; vstack: [l0]
        br 1 (;@1;)
      end
      local.get 0 ;; vstack: [l0]
    end
    ;; vstack: [l0]
    nop)
  (func (;test-if-return-loop;) (type 2) (result i32) ;; 12
    (local i32 i32 i32)
    local.get 2 ;; vstack: [0]
    if  ;; label = @1
      local.get 0 ;; vstack: [l0]
      return
    end
    loop  ;; label = @1
      local.get 2
      br_if 0 (;@1;)
    end
    local.get 1) ;; expected result: 0
  (func (;test-join-vstack;) (type 3) (param i32 i32) (result i32) ;; 13
    local.get 0
    if (result i32)
      local.get 0
    else
       local.get 1
    end)
  (func (;test-join-loop-if;) (type 4) (param i32 i32 i32) (result i32) ;; 14
    (local i32 i32 i32)
    loop  ;; label = @2
      local.get 2    ;; [p2]
      local.tee 0
      i32.eqz         ;; [Y]
      br_if 0 (;@2;) ;; []
    end
    local.get 0)     ;; [p0]
  (func (;test-multiple-input-states;) (type 4) (param i32 i32 i32) (result i32) ;; 15
    (local i32 i32 i32)
    block  ;; label = @1
      local.get 3
      local.tee 4
      drop
    end
    loop  ;; label = @2
      i32.const 0
      local.tee 2
      br_if 0 (;@2;)
    end
    local.get 0)
  (func (;test-spec-inference-loops;) (type 4) (param i32 i32 i32) (result i32) ;; 16
    (local i32 i32 i32)
    block  ;; label = @1
      local.get 0
      if  ;; label = @2
        block  ;; label = @3
          local.get 2
          local.tee 5
          br_if 0 (;@3;)
          loop  ;; label = @4
            local.get 2
            local.get 1
            i32.store
            local.get 2
            local.get 1
            i32.store
            i32.const -64
            local.tee 2
            br_if 0 (;@4;)
          end
        end
        loop  ;; label = @3
          local.get 2
          local.get 1
          i32.store
          local.get 1
          local.tee 2
          br_if 0 (;@3;)
        end
        br 1 (;@1;)
      end
    end
    local.get 0)
  (func (;test-identity;) (type 1) (param i32) (result i32) ;; 17
    local.get 0)
  (func (;test-if-0;) (type 2) (result i32) ;; 18
  (local i32 i32)
    local.get 0
    if (result i32)
      local.get 0
    else
       local.get 1
    end)
  (func (;test-simple-loop;) (type 4) (param i32 i32 i32) (result i32) ;; 19
    loop  ;; label = @2
      local.get 2    ;; [p2]
      br_if 0 (;@2;) ;; []
    end
    local.get 0)     ;; [p0]
  (table (;0;) 1 1 funcref)
  (memory (;0;) 2)
  (global (;0;) (mut i32) (i32.const 66560))
  (export "memory" (memory 0)))
