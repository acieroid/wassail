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

let of_wasm (m : Ast.module_) (e : Ast.var list Ast.segment) : t = {
  index = Var.of_wasm e.it.index;
  offset = fst (Instr.seq_of_wasm m (-1) e.it.offset.it [] 0 0 0 0);
  init = List.map e.it.init ~f:Var.of_wasm
}

let to_string (e : t) : string =
  Printf.sprintf "elem idx:%d offset:%s init:%s"
    e.index
    (Instr.list_to_string e.offset)
    (String.concat ~sep:"," (List.map e.init ~f:string_of_int))
