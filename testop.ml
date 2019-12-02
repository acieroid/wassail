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
  match (t, v1) with
  | (_, Bottom) -> Bottom
  | (I32Eqz, Const 0l) -> Const 1l
  | (I32Eqz, Const _) -> Const 0l
  | (I32Eqz, Int) -> Value.join (Const 0l) (Const 1l)

