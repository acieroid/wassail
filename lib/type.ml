open Core_kernel
open Wasm

(** These are types of values during the execution of wasm *)
module T = struct
  type t =
    | I32Type
    | I64Type
    | F32Type
    | F64Type
  [@@deriving sexp, compare]
end
include T

let of_wasm (vt : Types.value_type) : t =
  match vt with
  | Types.I32Type -> I32Type
  | Types.I64Type -> I64Type
  | Types.F32Type -> F32Type
  | Types.F64Type -> F64Type

let to_string (t : t) : string =
  match t with
  | I32Type -> "i32"
  | I64Type -> "i64"
  | F32Type -> "f32"
  | F64Type -> "f64"

let list_to_string (l : t list) : string =
  String.concat ~sep:", " (List.map l ~f:to_string)
