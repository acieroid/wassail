(module
  (func $main (local $i i32)
    ;; initialize i = 0
    i32.const 0
    local.set $i

    ;; while loop
    block $exit          ;; outer block (break target)
      loop $loop         ;; loop label
        ;; condition: if (i < 10)
        local.get $i
        i32.const 10
        i32.lt_s
        i32.eqz          ;; if !(i < 10) => break
        br_if $exit

        ;; body: i = i + 1
        local.get $i
        i32.const 1
        i32.add
        local.set $i

        ;; repeat loop
        br $loop
      end
    end
  )

  (export "main" (func $main))
)