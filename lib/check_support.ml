open Core_kernel

let instr_is_supported (i : Wasm.Ast.instr) : bool = match i.it with
  | Unreachable | Nop | Select | Drop
  | Block _ | Loop _ | If _
  | Br _ | BrIf _
  | Return | Call _
  | LocalGet _ | LocalSet _ | LocalTee _
  | GlobalGet _ | GlobalSet _
  | MemorySize
  | Const _
  | Test _ | Compare _ | Unary _ | Binary _ | Convert _
    -> true

  | MemoryGrow
  | Load _ | Store _
  | CallIndirect _
  | BrTable _
    -> false

let func_is_supported (f : Wasm.Ast.func) : bool =
  List.for_all f.it.body ~f:instr_is_supported
