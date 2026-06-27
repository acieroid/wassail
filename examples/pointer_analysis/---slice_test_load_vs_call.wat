(module
        (type (;0;) (func (param i32)))
        (type (;1;) (func (param i32)))

        (func (;0;) (type 0) (param $p i32)
          local.get $p
          i32.const 999
          i32.store)

        (func (;1;) (type 1) (param $p i32)
          local.get $p
          call 0

          i32.const 4
          i32.load

          local.get $p
          call 0
          drop)

        (memory (;0;) 1))