(module
  ;; i32 -> i32 type
  (type (;0;) (func (param i32) (result i32)))
  (func (;0;) (type 0) (param i32) (result i32)
    (local i32 i32 i32)
      loop  ;; label = @2
        local.get 2
        local.get 1
        i32.load8_u
        i32.store8
        local.get 1
        i32.const 1
        i32.add
        local.set 1
        local.get 2
        i32.const 1
        i32.add
        local.tee 2
        local.get 3
        i32.ne
        br 0 (;@2;)
      end
    local.get 0)
  ;; every wasm program has to have one table
  (table (;0;) 1 1 funcref)
  ;; linear memory, of size 2 (initial and minimal size, in pages of 64kB)
  (memory (;0;) 2)
  ;; one global pointing to the last 4 bytes in memory
  (global (;0;) (mut i32) (i32.const 66560))
  ;; the memory is exported
  (export "memory" (memory 0))
  ;; the identity fuunction is exported
  (export "id" (func 0)))


;; CFG should be the following:

;; Loop entry <----
;;  v             |
;; local.get 2    |
;; ...            |
;; i32.ne         |
;;  v             |
;; br 0------------
