(module
  (memory (export "mem") 1)
  (global $__stack_pointer (mut i32) (i32.const 1048576)) ;; esp
  (global $part1Value i32 (i32.const 0))
  (global $part2Value i32 (i32.const 1))

  (func $main (export "main") (result i32)
    ;; déclarations de variables
    (local $part1 i32)          ;; eax
    (local $part2 i32)          ;; ebx
    ;; (local $p_array0 i32)       ;; Pointeur vers la première case du tableau
    (local $i i32)              ;; ecx

    ;; Adjust esp for locals
    (global.set $__stack_pointer
      (i32.sub (global.get $__stack_pointer) (i32.const 44)))

    ;; part1 = &a[0]
    (local.set $part1
      (i32.add (global.get $__stack_pointer) (i32.const 4)))

    ;; part1 = &a[0]
    (local.set $part2
      (i32.add (global.get $__stack_pointer) (i32.const 24)))

    ;; p_array0 = part1
    (i32.store
      (global.get $__stack_pointer) (local.get $part1))

    ;; i = 0
    (local.set $i
      (i32.const 0))
    
    (block $exit_loop
      (loop $L1
        ;; *part1 = part1Value
        (i32.store
          (local.get $part1) (global.get $part1Value))

        ;; *part2 = part2Value
        (i32.store
          (local.get $part2) (global.get $part2Value))

        ;; part1++
        (local.set $part1
          (i32.add (i32.const 1) (local.get $part1)))
        ;; part2++
        (local.set $part2
          (i32.add (i32.const 1) (local.get $part2)))

        ;; i++
        (local.tee $i
          (i32.add (i32.const 1) (local.get $i)))

        ;; while i < 5
        i32.const 5
        i32.lt_u
        br_if $L1
      )
    )

    ;; Read value at address p_array0 and use it as return value
    (i32.load
      (i32.load 
        (global.get $__stack_pointer)))

    ;; reset stack pointer
    (global.set $__stack_pointer
      (i32.add (global.get $__stack_pointer) (i32.const 44)))

    return





    ;; (block $exit_loop
    ;;   (loop $L1
    ;;     local.get $part1
    ;;     global.get $part1Value
    ;;     i32.store
    ;;     ;; local.get $part2                 ;; \
    ;;     ;; local.get $i                     ;;  |
    ;;     ;; i32.const 4                      ;;   > Dans le cas de part2, j'incrémente ici : part2 + 4*i
    ;;     ;; i32.mul                          ;;  |
    ;;     ;; i32.add                          ;; /
    ;;     ;; global.get $part2Value
    ;;     ;; i32.store
    ;;     local.get $part2
    ;;     global.get $part2Value
    ;;     i32.store

    ;;     ;; incrémentation:
    ;;     local.get $part1                    ;; Dans le cas de part1, j'incémente ici
    ;;     i32.const 4
    ;;     i32.add
    ;;     local.set $part1

    ;;     local.get $part2
    ;;     i32.const 4
    ;;     i32.add
    ;;     local.set $part2

    ;;     ;;local.get $part2
    ;;     ;;i32.const 4
    ;;     ;;i32.add
    ;;     ;;local.set $part2 
        
    ;;     local.get $i 
    ;;     i32.const 1
    ;;     i32.add
    ;;     local.set $i

    ;;     ;; condition
    ;;     local.get $i
    ;;     i32.const 5
    ;;     i32.lt_u
    ;;     br_if $L1
    ;;   )
    ;; )

    ;; ;; Return the value stored at $p_array0 (the final result).
    ;; local.get $p_array0
    ;; i32.load

    ;; ;; reset stack pointer
    ;; global.get $__stack_pointer
    ;; i32.const 40
    ;; i32.add 
    ;; global.set $__stack_pointer

    ;; return
  )
)

