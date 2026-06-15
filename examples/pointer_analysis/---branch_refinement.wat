(module
      (memory (export "mem") 1)

      (func $main (export "main") (param $x i32) (result i32)
        (local $l1 i32)
        (local $true i32)
        (local $false i32)

        ;; l1 = {1,2}
        local.get $x
        if
          i32.const 1
          local.set $l1
        else
          i32.const 2
          local.set $l1
        end

        ;; true branch should refine l1 to 1
        ;; false branch should refine l1 to 2
        local.get $l1
        i32.const 1
        i32.eq
        if
          local.get $l1
          local.set $true
        else
          local.get $l1
          local.set $false
        end

        i32.const 0
      )
    )