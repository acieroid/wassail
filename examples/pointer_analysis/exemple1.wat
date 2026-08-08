(module
  (memory (export "mem") 1)
  (global $__stack_pointer (mut i32) (i32.const 1024))
  (global $part1Value i32 (i32.const 0))
  (global $part2Value i32 (i32.const 1))

  (func $main (export "main") (result i32)
    ;; déclarations de variables
    ;; (local $local_pointer i32)  ;; esp
    (local $part1 i32)          ;; eax
    (local $part2 i32)          ;; ebx
    (local $p_array0 i32)       ;; Pointeur vers la première case du tableau
    (local $i i32)              ;; ecx

    ;; Initialisation de la position sur mémoire linéaire:
    i32.const 1024
    global.set $__stack_pointer
    i32.const 0 
    global.set $part1Value
    i32.const 1
    global.set $part2Value

    global.get $__stack_pointer
    i32.const 40
    i32.sub
    global.set $__stack_pointer

    ;; Adresse du tableau et p_array
    global.get $__stack_pointer
    local.set $p_array0

    ;; Adresses de part1 et part2 :
    local.get $p_array0
    local.tee $part1     ;; local.set; local.get

    
    i32.const 20
    i32.add
    local.set $part2

    ;; Initialisation du compteur de la boucle:
    i32.const 0 
    local.set $i

    (block $exit_loop
      (loop $L1
        local.get $part1
        global.get $part1Value
        i32.store
        ;; local.get $part2                 ;; \
        ;; local.get $i                     ;;  |
        ;; i32.const 4                      ;;   > Dans le cas de part2, j'incrémente ici : part2 + 4*i
        ;; i32.mul                          ;;  |
        ;; i32.add                          ;; /
        ;; global.get $part2Value
        ;; i32.store
        local.get $part2
        global.get $part2Value
        i32.store

        ;; incrémentation:
        local.get $part1                    ;; Dans le cas de part1, j'incémente ici
        i32.const 4
        i32.add
        local.set $part1

        local.get $part2
        i32.const 4
        i32.add
        local.set $part2

        ;;local.get $part2
        ;;i32.const 4
        ;;i32.add
        ;;local.set $part2 
        
        local.get $i 
        i32.const 1
        i32.add
        local.set $i

        ;; condition
        local.get $i
        i32.const 5
        i32.lt_u
        br_if $L1
      )
    )

    ;; Return the value stored at $p_array0 (the final result).
    local.get $p_array0
    i32.load

    ;; reset stack pointer
    global.get $__stack_pointer
    i32.const 40
    i32.add 
    global.set $__stack_pointer

    return
  )
)

