open Core_kernel
open Wasm
open Value

(** Test operations *)
module T = struct
  type t =
    | I32Eqz
  [@@deriving sexp, compare, yojson]
end
include T
let of_wasm (t : Ast.testop) : t =
  match t with
  | I32 Eqz -> I32Eqz
  | _ -> failwith "unsupported type"
let to_string (t : t) : string =
  match t with
  | I32Eqz -> "i32.eqz"

let eqz (v : Value.t) : Value.t = match (is_zero v, is_not_zero v) with
  | (true, false) -> const 1l
  | (false, true) -> const 0l
  | (false, false) -> bottom
  | (true, true) -> begin match v with
      | Symbolic _ -> simplify (Symbolic (Op (Eq, v, const 0l)))
      | _ -> bool
    end

let eval (t : t) (v1 : Value.t) : Value.t =
  match (t, v1) with
  | (_, Bottom) -> Bottom
  | (I32Eqz, v) -> eqz v

