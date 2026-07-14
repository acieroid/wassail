(module
      (type (;0;) (func (result i32)))
      (type (;1;) (func))

      (func $f0 (type 1)
        i32.const 11
        global.set 1)

      (func $f1_not_in_table (type 1)
        i32.const 99
        global.set 1)

      (func $f2 (type 1)
        i32.const 42
        global.set 0)

      (func $test (type 0) (result i32)
        i32.const 1
        call_indirect (type 1)
        global.get 0)

      (table 2 funcref)
      (elem (i32.const 0) $f0 $f2)

      (global (mut i32) (i32.const 0))
      (global (mut i32) (i32.const 0)))