(module
  (type (;0;) (func (result i32)))
  (type (;1;) (func (param i32) (result i32)))
  (import "foo" "bar1" (func $bar1 (type 0)))
  (import "foo" "bar2" (func $bar2 (type 0)))
  (import "foo" "bar3" (func $bar3 (type 1)))
  (table 3 funcref)
  (func $f1 (type 0) (result i32)
    i32.const 42)
  (elem (i32.const 0) func $bar1 $f1 $callBar1)
  (func $callBar1 (export "callBar1")
    i32.const 0
    call_indirect )
)
