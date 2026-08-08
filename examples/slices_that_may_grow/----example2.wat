

;; Goal: reproduce the expr_parser-style issue more closely.
;;
;; We want a call that:
;;   - is kept without pointer analysis, because it may affect a later load;
;;   - is removed with pointer analysis, because it writes far away;
;;   - has several non-trivial argument computations;
;;   - has only one argument computation that is genuinely live later.
;;
;; Expected pointer-analysis slice:
;;   - call 0 removed;
;;   - call-only argument computations replaced by dummy zeros, not kept intact;
;;   - shared local.tee computation kept intact;
;;   - drops may remain because of the known stack-height limitation.

(module

  (type (;0;) (func (param i32 i32 i32 i32 i32)))
  (type (;1;) (func (result i32)))

  ;; Dead with pointer analysis: writes at 1000, while the later load reads at 0.
  ;; Conservatively live without pointer analysis: it may affect memory.
  (func (;0;) (type 0) (param i32 i32 i32 i32 i32)
    i32.const 1000
    i32.const 1234
    i32.store)

  (func (;1;) (type 1) (result i32)
    (local i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32)

    ;; Base locals used by argument computations.
    i32.const 10
    local.set 0

    i32.const 20
    local.set 1

    i32.const 30
    local.set 2

    i32.const 40
    local.set 3

    ;; Memory value that will later define local 16.
    i32.const 0
    i32.const 42
    i32.store

    ;; Argument 1: non-trivial but call-only.
    ;; If call 0 is removed, this whole computation should be replaceable by i32.const 0.
    local.get 0
    i32.const 48
    i32.add

    ;; Argument 2: non-trivial but call-only.
    ;; This is deliberately similar to the expr_parser local.get/const/add pattern.
    local.get 1
    i32.const 9
    i32.add

    ;; Argument 3: shared.
    ;; This value is both passed to call 0 and saved into local 4 for later use.
    ;; Therefore this computation should remain intact even with pointer analysis.
    local.get 2
    i32.const 18
    i32.add
    local.tee 4

    ;; Argument 4: non-trivial but call-only.
    local.get 3
    local.get 0
    i32.sub

    ;; Argument 5: non-trivial but call-only.
    local.get 1
    local.get 2
    i32.xor

    call 0

    ;; Load after call 0. Without pointer analysis, call 0 may affect this.
    ;; With pointer analysis, call 0 writes at 1000 and should not affect it.
    i32.const 0
    i32.load

    ;; Make the shared argument computation genuinely live for the criterion.
    local.get 4
    i32.add
    local.set 16

    ;; Slicing criterion.
    local.get 16)

  (memory (;0;) 1)
  (export "main" (func 1))
)