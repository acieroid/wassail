(module
(memory (export "mem") 1)
  (func $main (param $x i32) (local $i i32)
    ;; initialize i = 0
    
    local.get $x
    i32.const 34
    i32.lt_u
    if
      i32.const 15
      i32.const 55
      i32.store
    else
    end

    i32.const 34
    i32.const 42
    i32.store

    ;; while loop
    block $exit          ;; outer block (break target)
      loop $loop         ;; loop label
        ;; condition: if (i < 10)
        i32.const 11
        i32.const 10
        i32.lt_s
        i32.eqz          ;; if !(i < 10) => break
        br_if $exit

        ;; repeat loop
        br $loop
      end
    end

    i32.const 26
    local.set 0
  )

  (export "main" (func $main))
)