open Core_kernel
open Wasm
type t = {
  locals : Type.t list;
  body : Instr.t list;
}
[@@deriving sexp, compare, yojson]

let of_wasm (f : Ast.func) : t = {
  body = List.map f.it.body ~f:Instr.of_wasm;
  locals = List.map f.it.locals ~f:Type.of_wasm;
}
let to_string (f : t) : string =
  Printf.sprintf "locals: %s\ncode:\n%s" (Type.list_to_string f.locals) (Instr.list_to_string f.body ~sep:"\n")
