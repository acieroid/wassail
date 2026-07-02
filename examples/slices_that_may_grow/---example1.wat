(module
  ;; Minimal example where one argument computation for a dead call is also
  ;; genuinely live for the slicing criterion.
  ;;
  ;; Goal:
  ;;   - Without pointer analysis, call 0 is kept because it may affect the load
  ;;     that defines local 16.
  ;;   - With pointer analysis, call 0 is removed because it writes at 1000,
  ;;     while the load reads at 0.
  ;;   - The shared computation local.get 1 + 18 should remain because it is
  ;;     used to compute the final local 16 value.
  ;;   - The other call-only arguments should disappear, not become drops.

  (type (;0;) (func (param i32 i32 i32 i32 i32)))
  (type (;1;) (func (result i32)))

  ;; Potentially relevant without pointer analysis, but irrelevant with pointer analysis.
  (func (;0;) (type 0) (param i32 i32 i32 i32 i32)
    i32.const 1000
    i32.const 1234
    i32.store)

  (func (;1;) (type 1) (result i32)
    (local i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32)

    ;; Locals used by the call arguments.
    i32.const 0
    local.set 0

    i32.const 7
    local.set 1

    ;; Memory value loaded after the call.
    i32.const 0
    i32.const 42
    i32.store

    ;; Dead call arguments, shaped like the suspicious call 47 site.
    local.get 0        ;; call-only argument: should disappear with pointer analysis
    i32.const 48       ;; call-only argument: should disappear with pointer analysis

    ;; Shared argument computation.
    ;; This value is passed to call 0, but also saved into local 2 and used later
    ;; to define local 16. So this computation should remain in both slices.
    local.get 1
    i32.const 18
    i32.add
    local.tee 2

    i32.const 18       ;; call-only argument: should disappear with pointer analysis
    i32.const 0        ;; call-only argument: should disappear with pointer analysis
    call 0             ;; kept without pointer analysis, removed with pointer analysis

    ;; This load makes call 0 conservatively relevant without pointer analysis.
    i32.const 0
    i32.load

    ;; Combine the memory value with the shared call-argument computation.
    ;; This makes the shared computation genuinely live for local.get 16.
    local.get 2
    i32.add
    local.set 16

    ;; Slicing criterion.
    local.get 16)

  (memory (;0;) 1)
  (export "main" (func 1))
)