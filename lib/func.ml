open Core_kernel
open Wasm
type t = {
  locals : Type.t list;
  body : Instr.t list;
}
[@@deriving sexp, compare]

let of_wasm (m : Ast.module_) (fid : int) (f : Ast.func) (nglobals : int): t = {
  body = fst (Instr.seq_of_wasm m fid f.it.body [] (List.length f.it.locals) nglobals);
  locals = List.map f.it.locals ~f:Type.of_wasm;
}
let to_string (f : t) : string =
  Printf.sprintf "locals: %s\ncode:\n%s" (Type.list_to_string f.locals) (Instr.list_to_string f.body ~sep:"\n")
