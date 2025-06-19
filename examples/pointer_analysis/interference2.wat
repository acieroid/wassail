(module
  (func $main (local $i i32)
    ;; initialize i = 0
    
    i32.const 42
    i32.const 43
    i32.lt_u
    if (result i32)
      i32.const 10
      i32.const 0
      i32.store
    else
      i32.const 42
      i32.const 43
      i32.lt_u
      if (result i32)
        i32.const 10
        i32.const 15
        i32.store
      else
        i32.const 42
        i32.const 43
        i32.lt_u
        if (result i32)
          i32.const 10
          i32.const 30
          i32.store
        else
          i32.const 10
          i32.const 40
          i32.store
        end
      end
    end
    i32.const 10
    i32.load
    f32.const 300
    f32.store offset=1
    
  )
  (export "main" (func $main))
)