(module
  (memory (export "mem") 1)
  (global $__stack_pointer (mut i32) (i32.const 1024))

  (func $main (export "main") (result i32)
    ;; déclarations de variables
    ;; (local $local_pointer i32)  ;; esp
    (local $part1 i32)          ;; eax
    (local $part2 i32)          ;; ebx
    (local $p_array0 i32)       ;; Pointeur vers la première case du tableau
    (local $i i32)              ;; ecx

    ;; Initialisation de la position sur mémoire linéaire:
    global.get $__stack_pointer
    i32.const 40
    i32.sub
    global.set $__stack_pointer
    i32.const 100

    (if (result i32)
      (then
        i32.const 26
        i32.const 14
      )
      (else
        i32.const 42
        i32.const 88
      )
    )
    i32.add
  )
)





