(module
  (memory (export "memory") 1)

  (func $main (local $i i32) (local $l2 i32)
    ;; initialize i = 0
    i32.const 1
    local.set $i

    ;; while loop
    block $exit          ;; outer block (break target)
      loop $loop         ;; loop label
        ;; condition: if (i <= 10)
        local.get $i
        i32.const 2
        i32.gt_u
        ;; i32.eqz       
        br_if $exit      ;; if i > 10 => break

        local.get $i
        i32.const 1
        i32.gt_u
        if
          i32.const 14
          i32.load
          local.set $l2
        end

        ;; body: i = i + 1
        i32.const 14
        local.get $i
        i32.store


        i32.const 1
        local.get $i
        i32.add
        local.set $i

        ;; repeat loop
        br $loop
      end
    end
  )

  (export "main" (func $main))
)