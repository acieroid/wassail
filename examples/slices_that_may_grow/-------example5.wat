

(module
  ;; Attempt to reproduce the expr_parser behaviour with two dead calls in the
  ;; same control-flow region.
  ;;
  ;; Path A:
  ;;   dead call
  ;;   br 1
  ;;
  ;; Path B:
  ;;   dead call
  ;;   live load8_u/and/br_if pattern
  ;;
  ;; Criterion: local.get 16, whose value comes from a load after the blocks.

  (type (func (param i32 i32 i32 i32 i32)))
  (type (func (result i32)))

  (func $dead (param i32 i32 i32 i32 i32)
    i32.const 1000
    i32.const 1234
    i32.store)

  (func (export "main") (result i32)
    (local i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32)

    i32.const 0
    local.set 0

    i32.const 20
    local.set 1

    ;; Data for the live branch guard.
    i32.const 0
    i32.const 0
    i32.store8

    ;; Data for the slicing criterion.
    i32.const 4
    i32.const 42
    i32.store

    block
      block
        ;; Choose the second path at runtime while keeping both paths present.
        i32.const 0
        br_if 0

        ;; -------- Path A --------
        local.get 0
        i32.const 48
        local.get 1
        i32.const 9
        i32.add
        i32.const 9
        i32.const 0
        call $dead
        br 1
      end

      ;; -------- Path B --------
      local.get 0
      i32.const 48
      local.get 1
      i32.const 18
      i32.add
      i32.const 18
      i32.const 0
      call $dead

      ;; Live pattern immediately after the second dead call.
      local.get 0
      i32.load8_u
      i32.const 32
      i32.and
      br_if 0
    end

    ;; Keeps the dead call conservatively without pointer analysis.
    i32.const 4
    i32.load
    local.set 16

    ;; Criterion.
    local.get 16)

  (memory 1)
)