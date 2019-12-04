open Core_kernel
open Wasm

(** A variable in wasm is just an index *)
module T = struct
  type t = int
  [@@deriving sexp, compare]
end
include T
let of_wasm (v : Ast.var) : t = Int32.to_int_exn v.it
