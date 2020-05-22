open Core_kernel
open Wasm
open Value

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

let eqz (v : Value.t) : Value.t = match (is_zero v, is_not_zero v) with
  | (true, false) -> i32_const 1l
  | (false, true) -> i32_const 0l
  | (false, false) -> bottom I32
  | (true, true) -> begin match v.value with
      | Symbolic _ -> {
          value = simplify_value (Symbolic (Op (Eq, v.value,
                                                Symbolic (Const (PrimValue.zero_of_t v.typ))))); (* TODO: may be i64 *)
          typ = I32
        }
      | _ -> bool
    end

let eval (t : t) (v1 : Value.t) : Value.t =
  match (t, v1.value) with
  | (_, Bottom) -> v1
  | (I32Eqz, _) -> eqz v1

