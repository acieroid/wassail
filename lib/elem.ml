open Core_kernel
open Wasm

type t = {
  index : Var.t; (** The table index (should always be 0 currently, wasm does
                    not support more than one table) *)
  offset : Instr.t list; (** The offset is given by a "constant expression",
                            i.e., a list of instrs that evaluates to a constant
                            *)
  init : Var.t list; (** Vector of elements (function addresses) *)
}
[@@deriving sexp, compare]

let of_wasm (e : Ast.var list Ast.segment) : t = {
  index = Var.of_wasm e.it.index;
  offset = List.map e.it.offset.it ~f:Instr.of_wasm;
  init = List.map e.it.init ~f:Var.of_wasm
}
             
      
