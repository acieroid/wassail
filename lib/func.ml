open Core_kernel
open Wasm
type t = {
  locals : Type.t list;
  body : Instr.t list;
}
[@@deriving sexp, compare]

let of_wasm (m : Ast.module_) (_fid : int) (f : Ast.func) (_nargs : int) (_nglobals : int) (_nreturns : int) : t = {
  locals = List.map f.it.locals ~f:Type.of_wasm;
  body = Instr.seq_of_wasm m f.it.body;
}
let to_string (f : t) : string =
  Printf.sprintf "locals: %s\ncode:\n%s" (Type.list_to_string f.locals) (Instr.list_to_string f.body ~sep:"\n")
