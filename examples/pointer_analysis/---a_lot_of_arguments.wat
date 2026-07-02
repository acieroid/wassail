(module

        (func (;0;) (param i32) (param i32) (param i32) (param i32) (param i32) (param i32) (param i32) (param i32) (param i32) (param i32) (param i32) (param i32)(param i32) (param i32) (result i32)
          local.get 11    ;; $x
        )

        (func (;1;) (result i32)
          i32.const 14
          i32.const 14
          i32.const 14
          i32.const 14
          i32.const 14
          i32.const 14
          i32.const 14
          i32.const 14
          i32.const 14
          i32.const 14
          i32.const 14
          i32.const 0
          i32.const 14
          i32.const 14
          call 0        ;; should return 0
          i32.eqz       ;; true
          if 
            i32.const 42
            i32.const 55
            i32.store
          else
            i32.const 14
            i32.const 66
            i32.store
          end
          i32.const 14
          i32.load
        )
        (memory (;0;) 1))