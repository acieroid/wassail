open Core
open Wasm

(** Test operations *)
module T = struct
  type t =
    | I32Eqz
  [@@deriving sexp, compare]
end
include T
let of_wasm (t : Ast.testop) : t =
  match t with
  | I32 Eqz -> I32Eqz
  | _ -> failwith "unsupported type"
let to_string (t : t) : string =
  match t with
  | I32Eqz -> "i32.eqz"
let eval (t : t) (v1 : Value.t) : Value.t =
  match (t, v1.value) with
  | (_, Bottom) -> v1
  | (I32Eqz, Const 0l) -> { v1 with value = Const 1l }
  | (I32Eqz, Const _) -> { v1 with value = Const 0l }
  | (I32Eqz, Int) -> Value.join { v1 with value = Const 0l } { v1 with value = Const 1l }

