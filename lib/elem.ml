open Core_kernel
open Wasm

type t = {
  index : int; (** The table index (should always be 0 currently, wasm does
                    not support more than one table) *)
  offset : unit Instr.t list; (** The offset is given by a "constant expression",
                            i.e., a list of instrs that evaluates to a constant
                            *)
  init : int list; (** Vector of elements (function addresses) *)
}
[@@deriving sexp, compare]

let of_wasm (m : Ast.module_) (e : Ast.var list Ast.segment) : t = {
  index = Index.of_wasm e.it.index;
  offset = Instr.seq_of_wasm m e.it.offset.it;
  init = List.map e.it.init ~f:Index.of_wasm
}

let to_string (e : t) : string =
  Printf.sprintf "elem idx:%d offset:%s init:%s"
    e.index
    (Instr.list_to_string e.offset (fun _ -> ""))
    (String.concat ~sep:"," (List.map e.init ~f:string_of_int))
