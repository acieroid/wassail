open Core_kernel
open Helpers

let is_data_instr_supported (d : Instr.data Instr.labelled) : bool = match d.instr with
  | Nop | Drop | Select | MemorySize | MemoryGrow
  | Const _ | Unary _ | Binary _ | Compare _ | Test _
  | LocalGet _ | LocalSet _ | LocalTee _
  | GlobalGet _ | GlobalSet _ -> true
  | Load _ | Store _ -> false
  | Convert _ -> false

let is_control_instr_supported (c : Instr.control Instr.labelled) : bool = match c.instr with
  | Block _ | Loop _ | If _ | Call _ | Br _ | BrIf _ | Return | Unreachable -> true
  | CallIndirect _ | BrTable _ -> false

let is_block_supported (b : Basic_block.t) : bool = match b.content with
  | Control c -> is_control_instr_supported c
  | ControlMerge -> true
  | Data instrs -> List.for_all instrs ~f:is_data_instr_supported

let cfg_is_supported (cfg : Cfg.t) : bool =
  IntMap.for_all cfg.basic_blocks ~f:is_block_supported

