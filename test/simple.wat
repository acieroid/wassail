(module
  (type (;0;) (func (param i32)))
  (type (;1;) (func (param i32) (result i32)))
  (type (;2;) (func (result i32)))
  (type (;3;) (func (param i32 i32) (result i32)))
  (type (;4;) (func (param i32 i32 i32) (result i32)))
  (type (;5;) (func (param i64) (result i32)))
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
    ;; taint: ret tainted with l0, memory[g0] tainted with l0
    )
  (func (;test-load;) (type 1) (param i32) (result i32) ;; 2
    global.get 0
    local.get 0
    i32.store offset=12
    global.get 0
    i32.load offset=12
    ;; summary: ret = p0
    )
  (func (;test-if;) (type 1) (param i32) (result i32) ;; 3
    local.get 0
    i32.const 0
    i32.eq
    if (result i32)
      i32.const 1
    else
      i32.const 0
    end
    ;; summary: ret = [0,1]
    )
  (func (;test-if-return;) (type 1) (param i32) (result i32) ;; 4
    i32.const 5
    i32.const 0
    i32.eq
    if
      i32.const 6
      return
    end
    i32.const 0
    ;; summary: ret = [0, 6]
    )
  (func (;test-join;) (type 1) (param i32) (result i32) ;; 5
    local.get 0
    i32.eqz
    if  ;; label = @1
      i32.const 0
      return
    end
    i32.const 1200
    local.get 0
    i32.store
    i32.const -1
    ;; summary: ret = [-1, 0], memory? can be unchanged (if), or can be memory[1200: l0] -> we need an overapproximation of the taint flow, hence memory[1200] may be tainted -> maybe we need an extra domain to track that, where join([], [1200: l0]) = [1200: l0]. And in that case, we can probably completely ignore the memory from the relational domain?! (We definitely can, the big question is how does it influence precision)
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
  (func (;test-taint-arg0;) (type 1) (param i32) (result i32) ;; 20
    local.get 0
    ;; taint: ret is tainted with l0
    )
  (func (;test-taint-arg0-add;) (type 1) (param i32) (result i32) ;; 21
    local.get 0
    i32.const 1
    i32.add
    ;; taint: ret is tainted with l0
    )
  (func (;tset-taint-arg0-arg1;) (type 4) (param i32 i32 i32) (result i32) ;; 22
    local.get 0
    local.get 1
    i32.add
    ;; taint: ret is tainted with l0 and l1
    )
  (func (;test-taint-local-set;) (type 1) (param i32) (result i32) ;; 23
    (local i32)
    local.get 0
    local.set 1
    local.get 1
    ;; taint: ret is tainted with l0
    )
  (func (;test-taint-if;) (type 4) (param i32 i32 i32) (result i32) ;; 24
    local.get 0
    if (result i32)
      local.get 1
    else
      local.get 2
    end
    ;; taint: ret is tainted with l1 and l2
    )
  (func (;test-taint-if-localset;) (type 4) (param i32 i32 i32) (result i32) ;; 25
    (local i32)
    local.get 0
    if
      local.get 1
      local.set 3
    else
      local.get 2
      local.set 3
    end
    local.get 3)
  (func (;test-taint-getglobal;) (type 1) (param i32) (result i32) ;; 26
    global.get 0
    ;; taint: ret is tainted with g0
    )
  (func (;test-taint-setglobal;) (type 1) (param i32) (result i32) ;; 27
    local.get 0
    global.set 0
    global.get 0
    ;; ret: tainted with l0, global g0: tainted with l0
    )
  (func (;test-load;) (type 1) (param i32) (result i32) ;; 28
    local.get 0
    i32.load
    ;; ret = m[l0]
    )
  (func (;test-store;) (type 1) (param i32) (result i32) ;; 29
    global.get 0
    local.get 0
    ;; mem is [k: v]
    ;; no constraints
    i32.store
    ;; mem is [k: v']
    ;; constraints: k = g0, v' = l0
    local.get 0
    ;; ret = l0, mem[g0] = l0
    )
  (func (;test-store-join-addr-top;) (type 1) (param i32) (result i32) ;; 30
    local.get 0
    if (result i32)
      global.get 0
    else
      local.get 0
    end
    i32.const 0
    i32.store
    local.get 0
    ;; result: ret = l0, mem[X] = 0, where X is join(g0, l0) = Top
    )
  (func (;test-store-join-addr-itv;) (type 1) (param i32) (result i32) ;; 31
    local.get 0
    if (result i32)
      i32.const 1024
    else
      i32.const 1025
    end
    i32.const 0
    i32.store
    local.get 0
    ;; result: ret = l0, mem[X] = 0 where X is [1024, 1024]
    )
  (func (;test-load-twice;) (type 1) (param i32) (result i32) ;; 32
    global.get 0
    i32.load
    drop
    global.get 0
    i32.load
    ;; expected: mem is [a: b, c: d] where a = c = g0, b = d
    )
  (func (;test-store-twice;) (type 1) (param i32) (result i32) ;; 33
    global.get 0
    local.get 0
    i32.store
    global.get 0
    local.get 0
    i32.store
    i32.const 0
    ;; expected: mem is [a: b, c: d] where a = b = g0, c = d = l0
    )
  (func (;bug-loop-surrounded-vstack;) (type 3) (param i32 i32) (result i32) ;; 34
    (local i32 i32 i32)
    local.get 2
    loop  ;; label = @3
      local.get 2
      br_if 0 (;@3;)
    end
    local.set 2
    i32.const 0)
  (func (;bug-different-vstacks;) (type 3) (param i32 i32) (result i32) ;; 35
    (local i32 i32 i32)
    block  ;; label = @1
      block  ;; label = @2
        local.get 2
        loop  ;; label = @3
          local.get 0
          br_if 1 (;@2;)
          local.get 0
          i32.const 4
          i32.add
          local.set 0
          local.get 2
          i32.const -4
          i32.add
          local.tee 2
          i32.const 3
          i32.gt_u
          br_if 0 (;@3;)
        end
        local.set 2
      end
    end
    i32.const 0)
  (func (;entry-point;) (type 1) (param i32) (result i32) ;; 36
    local.get 0
    call 27)
  (table (;0;) 1 1 funcref)
  (memory (;0;) 2)
  (global (;0;) (mut i32) (i32.const 66560))
  (export "_start" (func 36))
  (export "memory" (memory 0)))
