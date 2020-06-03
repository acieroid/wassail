open Core_kernel
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

let eqz (v : Value.t) : Value.t = match (Value.is_zero v, Value.is_not_zero v) with
  | (true, false) ->
    (* defiitely zero *)
    Value.i32_const 1l
  | (false, true) ->
    (* definitely not zero *)
    Value.i32_const 0l
  | (false, false) ->
    (* not zero nor non-zero *)
    Logging.warn "bottom" (Printf.sprintf "bottom created when checking: eqz %s" (Value.to_string v));
    Value.bottom I32
  | (true, true) ->
    (* Both zero and non-zero. In this case, we could return [0,1]. But we want
       to record the information that v has been checked, mostly because this
       test is heavily used in conditionals. Hence, we return the abstract value
       "v=0", when possible. *)
    begin match v.value with
      | Symbolic _ -> {
          value = Value.simplify_value (Symbolic (Op (Eq, v.value,
                                                Symbolic (Const (Prim_value.zero_of_t v.typ)))));
          typ = I32
        }
      | _ -> Value.bool
    end

let eval (t : t) (v1 : Value.t) : Value.t =
  match (t, v1.value) with
  | (_, Bottom) -> v1
  | (I32Eqz, _) -> eqz v1

