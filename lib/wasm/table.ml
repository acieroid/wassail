open Core_kernel
open Wasm

(** Limits to the size of the table (min, max) *)
type table_type = Limits.t (* elemtype is not included, as it can only be funcref *) 
[@@deriving sexp, compare, equal]

let table_type_of_wasm (ttype : Wasm.Types.table_type) : table_type = match ttype with
  | TableType (limits, _) -> Limits.of_wasm limits

type t = {
  ttype : table_type
}
[@@deriving sexp, compare, equal]

let of_wasm (t : Ast.table) : t = {
  ttype = table_type_of_wasm t.it.ttype
}
