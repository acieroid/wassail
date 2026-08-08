

(module
  ;; Attempt to reproduce the expr_parser behaviour with the newly identified
  ;; ingredient:
  ;;
  ;;   earlier dead call
  ;;   conditional br out of the surrounding block
  ;;   ... later dead call whose arguments are wrongly kept ...
  ;;
  ;; In expr_parser, the earlier call 47 followed by br appears to cause a huge
  ;; global-dependence region that includes the later call 47 arguments.
  ;;
  ;; Criterion: local.get 16, whose value comes from a load after the blocks.
  ;; Without pointer analysis, calls are conservatively kept because they may
  ;; affect that load. With pointer analysis, $dead writes at 1000 and should be
  ;; removed.

  (type (func (param i32 i32 i32 i32 i32)))
  (type (func (result i32)))

  (func $dead (param i32 i32 i32 i32 i32)
    i32.const 1000
    i32.const 1234
    i32.store)

  (func (export "main") (result i32)
    (local i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32)

    ;; Locals used by the call arguments.
    i32.const 0
    local.set 0

    i32.const 20
    local.set 20

    i32.const 8192
    local.set 26

    ;; Memory value eventually loaded into local 16.
    i32.const 4
    i32.const 42
    i32.store

    block  ;; label = @0, target of the first call's br
      block  ;; label = @1
        ;; Earlier dead call, shaped like the first suspicious call 47 site.
        i32.const 0
        local.set 16
        local.get 0
        i32.const 32
        local.get 20
        i32.const 0
        local.get 26
        call $dead

        ;; Make the branch conditional so the later dead call remains reachable.
        ;; This keeps the expr_parser-like "call followed by branch out" shape
        ;; without making the rest of the block statically unreachable.
        local.get 26
        i32.eqz
        br_if 1
      end

      ;; Later dead call. These arguments should not remain intact when
      ;; pointer analysis removes this call.
      local.get 0
      i32.const 48
      local.get 20
      i32.const 18
      i32.add
      i32.const 18
      i32.const 0
      call $dead
    end

    ;; The criterion depends on a load after the calls.
    ;; No-pointer slicing keeps the calls conservatively; pointer slicing should
    ;; remove them because they write at 1000 while this reads at 4.
    i32.const 4
    i32.load
    local.set 16
    local.get 16)

  (memory 1)
)