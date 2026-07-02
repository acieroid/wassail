

(module
  ;; Attempt to reproduce the expr_parser behaviour without local.tee.
  ;;
  ;; The idea is that the call arguments are built from locals that are also
  ;; used later, but the argument expressions themselves are not.
  ;;
  ;; Without pointer analysis:
  ;;   - call 0 is kept because it may affect the load.
  ;;
  ;; With pointer analysis:
  ;;   - call 0 should disappear.
  ;;   - Call-only argument expressions should ideally become dummy i32.const 0
  ;;     values rather than remaining intact.

  (type (func (param i32 i32 i32 i32 i32)))
  (type (func (result i32)))

  (func $dead (param i32 i32 i32 i32 i32)
    ;; Writes far away from the later load.
    i32.const 1000
    i32.const 1234
    i32.store)

  (func (export "main") (result i32)
    (local i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32)

    ;; Locals that will also be used later.
    i32.const 10
    local.set 0

    i32.const 20
    local.set 1

    ;; Value later loaded into local 16.
    i32.const 0
    i32.const 42
    i32.store

    ;; Arguments for the dead call.
    ;; These resemble the expr_parser pattern.
    local.get 0
    i32.const 48

    local.get 1
    i32.const 18
    i32.add

    i32.const 18
    i32.const 0
    call $dead

    ;; Load that makes the call conservatively relevant without pointer analysis.
    i32.const 0
    i32.load
    local.set 16

    ;; Reuse the same locals, but not the same expressions.
    local.get 0
    drop
    local.get 1
    drop

    ;; Criterion.
    local.get 16)

  (memory 1)
)