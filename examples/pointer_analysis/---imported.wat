(module
        (import "env" "random_function"
          (func $random_function (param i32) (result i32)))

        (import "env" "fd_close"
          (func $fd_close (param i32) (result i32)))

        (memory (export "mem") 1)
        (global $g (mut i32) (i32.const 0))

        (func (export "main") (result i32)
          i32.const 42
          global.set $g

          i32.const 14
          global.get $g
          i32.store
          i32.const 14
          i32.load       ;; this value should be "g0"

          call $fd_close
          drop
          i32.const 14
          i32.load       ;; this value should be "g0"
          drop
          i32.const 14
          global.get $g
          i32.store
          i32.const 14
          i32.load       ;; this value should be "g0"

          call $random_function
          drop
          i32.const 14
          i32.load       ;; this value should be Top
          drop
          i32.const 14
          global.get $g
          i32.store
          i32.const 14
          i32.load       ;; this value should be Top
        )
      )