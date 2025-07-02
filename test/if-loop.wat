(module
  ;; i32 -> i32 type
  (type (;0;) (func (param i32) (result i32)))
  (func (;0;) (type 0) (param i32) (result i32)
    (local i32)
    local.get 0
    if
      block (result i32)
        local.get 0
      end
      block (result i32)
        local.get 0
      end
      drop
      drop
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

;; local.get 0
;;  v
;; if---------------------------
;;  v                          |
;; local.get 0                 |
;;  |                          v
;;  -----------------> local.get 0 (return)
