(module
  ;; Define a mutable global variable initialized to 0
  (global $sp (mut i32) (i32.const 0)) 
  (global $heap (mut i32) (i32.const 0)) 

  ;; Function that adds 42 to its argument
  (func $add_stuff_to_heap (param $x i32) (result i32) (local $local_sp i32)
    ;; update stack pointer
    global.get $sp
    i32.const 8
    i32.sub
    local.tee $local_sp
    global.set $sp

    ;; put x + 14 on the stack
    local.get $local_sp
    i32.const 14
    local.get $x
    i32.add
    i32.store offset=0

    ;; put x - 14 on the stack
    local.get $local_sp
    local.get $x
    i32.const 14
    i32.sub
    i32.store offset=4


    ;; store x + 14 on the heap
    global.get $heap
    local.get $local_sp
    i32.load offset=0
    i32.store offset=0

    ;; store x - 14 on the heap
    global.get $heap
    local.get $local_sp
    i32.load offset=4
    i32.store offset=4
    
    ;; reset stack_pointer
    global.get $sp
    i32.const 8
    i32.add
    global.set $sp

    i32.const 26
    return
    )

  ;; Main function that increments global g and calls add42(10)
  (func $main (result i32)

    i32.const 0
    call $add_stuff_to_heap
    
    return)

  ;; Export the main function
  (export "main" (func $main))
)