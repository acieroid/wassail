open Core
open Wasm

(** These are the values (and their abstractions) *)
module T = struct
  type t =
    | Bottom
    | Const of int32
    | Int
    (* XXX: values are actually i32/i64/f32/f64 *)
  [@@deriving sexp, compare]
end
include T
include Comparator.Make(T)

let of_wasm (v : Values.value) : t =
  match v with
  | I32 x -> Const x
  | I64 _ -> failwith "unsupported type: I64"
  | F32 _ -> failwith "unsupported type: F32"
  | F64 _ -> failwith "unsupported type: F64"

let to_string (v : t) : string =
  match v with
  | Bottom -> "bottom"
  | Const n -> Int32.to_string n
  | Int -> "int"

(** Joins two values together *)
let join (v1 : t) (v2 : t) : t =
  match (v1, v2) with
  | (Bottom, x) -> x
  | (x, Bottom) -> x
  | (Const n1, Const n2) when n1 = n2 -> Const n1
  | (Const _, Const _) -> Int
  | _ -> Int

let is_zero (v : t) =
  match v with
  | Bottom -> false
  | Const 0l -> true
  | Const _ -> false
  | Int -> true

let is_not_zero (v : t) =
  match v with
  | Bottom -> false
  | Const 0l -> false
  | _ -> true

let bottom : t = Bottom

let zero (t : Type.t) : t =
  match t with
  | I32Type -> Const 0l
  | _ -> failwith "unsupported type"

let top (t : Type.t) : t =
  match t with
  | I32Type -> Const 0l
  | _ -> failwith "unsupported type"

let list_to_string (l : t list) : string =
  String.concat ~sep:", " (List.map l ~f:to_string)

