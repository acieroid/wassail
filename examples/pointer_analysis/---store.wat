(module
  (memory (export "mem") 1)
  (global $g0 (mut i32) (i32.const 1024))

  (func $main (export "main") (param $l0 i32) (local $l1 i32)
    i32.const 36                                         ;; [36]
    global.get $g0                                       ;; [g0; 36]
    i32.store offset=8     ;; [44:g0]                    ;; []
    i32.const 36                                         ;; [36]
    i32.load offset=8      ;; i0_4=g0                    ;; [g0]
    i32.const 4                                          ;; [4; g0]
    i32.add                ;; i0_6=(g0+4)                ;; [g0+4]
    global.get $g0         ;; i0_7=g0                    ;; [g0; g0+4]
    i32.store offset=112   ;; [44:Top;   (g0+116):g0]    ;; []
  )
)