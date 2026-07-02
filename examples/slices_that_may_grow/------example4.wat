

(module
  ;; Attempt to reproduce the expr_parser behaviour by adding the surrounding
  ;; nested control-flow shape:
  ;;
  ;;   block
  ;;     block
  ;;       dead-call arguments
  ;;       call $dead
  ;;
  ;;       live load/and/br_if pattern
  ;;     end
  ;;   end
  ;;
  ;; There is no local.tee here. The dead call arguments are built only from
  ;; constants, locals, and an add, like the suspicious expr_parser site.
  ;;
  ;; Without pointer analysis:
  ;;   - call $dead is kept because it may affect the later i32.load that
  ;;     defines local 16.
  ;;
  ;; With pointer analysis:
  ;;   - call $dead should disappear because it writes at 1000;
  ;;   - its call-only arguments should become dummy i32.const 0 values if
  ;;     needed for stack shape, not remain as full computations.

  (type (func (param i32 i32 i32 i32 i32)))
  (type (func (result i32)))

  (func $dead (param i32 i32 i32 i32 i32)
    ;; Writes far away from the later load from address 0.
    i32.const 1000
    i32.const 1234
    i32.store)

  (func (export "main") (result i32)
    (local i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32)

    ;; Locals used by the dead call arguments and by the live br_if guard.
    i32.const 0
    local.set 0

    i32.const 20
    local.set 1

    ;; Memory byte used by the br_if guard: (*(local 0) & 32).
    i32.const 0
    i32.const 0
    i32.store8

    ;; Memory value later loaded into local 16.
    i32.const 4
    i32.const 42
    i32.store

    block  ;; outer block
      block  ;; inner block, target of br_if 0
        ;; Dead call arguments, shaped like expr_parser:
        ;;   local.get 0
        ;;   i32.const 48
        ;;   local.get 1
        ;;   i32.const 18
        ;;   i32.add
        ;;   i32.const 18
        ;;   i32.const 0
        local.get 0
        i32.const 48
        local.get 1
        i32.const 18
        i32.add
        i32.const 18
        i32.const 0
        call $dead

        ;; Live control-flow pattern immediately after the removed call.
        ;; This is meant to mimic the expr_parser code after call 47.
        local.get 0
        i32.load8_u
        i32.const 32
        i32.and
        br_if 0
      end
    end

    ;; This load makes call $dead conservatively relevant without pointer analysis.
    ;; With pointer analysis, call $dead writes at 1000 and should not affect it.
    i32.const 4
    i32.load
    local.set 16

    ;; Slicing criterion.
    local.get 16)

  (memory 1)
)