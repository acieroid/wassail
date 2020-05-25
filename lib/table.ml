open Core_kernel
open Wasm

type t = {
  limits : Int32.t * (Int32.t option); (** Limits to the size of the table (min, max) *)
  (* elemtype is not included, as it can only be funcref *)
}
[@@deriving sexp, compare]

let of_wasm (t : Ast.table) : t = {
  limits = match t.it.ttype with
    | TableType ({min = min; max = max}, _) -> (min, max)
}
