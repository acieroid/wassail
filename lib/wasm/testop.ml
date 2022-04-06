open Core
open Wasm

(** Test operations *)
module T = struct
  type t =
    | I32Eqz
    | I64Eqz
  [@@deriving sexp, compare, equal]
end
include T

let of_wasm (t : Ast.testop) : t =
  match t with
  | I32 Eqz -> I32Eqz
  | I64 Eqz -> I64Eqz
  | _ -> failwith "unsupported type: float in testop"

let to_mnemonic (t : t) : string =
  match t with
  | I32Eqz -> "i32.eqz"
  | I64Eqz -> "i64.eqz"

let to_string (t : t) : string = to_mnemonic t
