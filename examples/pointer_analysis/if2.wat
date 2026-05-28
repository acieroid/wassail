(module
  (memory (export "mem") 1)
  ;; (global $g0 (mut i32) (i32.const 1024))

  (func $main (export "main") (result i32) (local $l1 i32) (local $l2 i32)
    i32.const 0
    if (result i32)
      i32.const 99
      local.set $l1
      i32.const 1
    else
      i32.const 2
    end

    local.tee $l1
    i32.const 2
    i32.lt_u
    i32.const 5
    local.tee $l2
    i32.const 3
    i32.gt_u
    ;; i32.const 1
    i32.and
    if (result i32)
      i32.const 6
    else
      i32.const 7
    end
    return
  )
)
