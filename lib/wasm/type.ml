open Core
open Wasm

(** These are types of values during the execution of wasm *)
module T = struct
  type t =
    | I32
    | I64
    | F32
    | F64
  [@@deriving sexp, compare, equal]
end
include T

let of_wasm (vt : Types.value_type) : t =
  match vt with
  | Types.I32Type -> I32
  | Types.I64Type -> I64
  | Types.F32Type -> F32
  | Types.F64Type -> F64

let to_string (t : t) : string =
  match t with
  | I32 -> "i32"
  | I64 -> "i64"
  | F32 -> "f32"
  | F64 -> "f64"

let size (t : t) : int32 =
  match t with
  | I32 | F32 -> 32l
  | I64 | F64 -> 64l

let list_to_string (l : t list) : string =
  String.concat ~sep:", " (List.map l ~f:to_string)

let funtype_to_string (l : t list * t list) : string =
  Printf.sprintf "%s -> %s" (list_to_string (fst l)) (list_to_string (snd l))
