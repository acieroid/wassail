open Core_kernel
open Wasm
open Value

(** Relational operation *)
module T = struct
  type t =
    | I32Eq
    | I32Ne
    | I32LtS
    | I32GtS
    | I32LeS
    | I32GeS (* XXX: others *)
  [@@deriving sexp, compare]
end
include T
let of_wasm (r : Ast.relop) : t =
  match r with
  | I32 Eq -> I32Eq
  | I32 Ne -> I32Ne
  | I32 LtS -> I32LtS
  | I32 GtS -> I32GtS
  | I32 LeS -> I32LeS
  | I32 GeS -> I32GeS
  | I32 _ -> failwith "unsupported relational operation"
  | _ -> failwith "unsupported type"
let to_string (r : t) : string =
  match r with
  | I32Eq -> "i32.eq"
  | I32Ne -> "i32.ne"
  | I32LtS -> "i32.lt_s"
  | I32GtS -> "i32.gt_s"
  | I32LeS -> "i32.le_s"
  | I32GeS -> "i32.ge_s"

let eq (v1 : Value.t) (v2 : Value.t) : Value.t = match (v1, v2) with
  | (Const n1, Const n2) when n1 = n2 -> const 1l
  | _ -> bool

let ne (v1 : Value.t) (v2 : Value.t) : Value.t = match (v1, v2) with
  | (Const n1, Const n2) when n1 <> n2 -> const 1l
  | _ -> bool (* TODO *)

let lt_s (v1 : Value.t) (v2 : Value.t) : Value.t = match (v1, v2) with
  | (Const n1, Const n2) when n1 < n2 -> const 1l
  | _ -> bool (* TODO *)

let gt_s (v1 : Value.t) (v2 : Value.t) : Value.t = match (v1, v2) with
  | (Const n1, Const n2) when n1 > n2 -> const 1l
  | _ -> bool (* TODO *)

let le_s (v1 : Value.t) (v2 : Value.t) : Value.t = match (v1, v2) with
  | (Const n1, Const n2) when n1 <= n2 -> const 1l
  | _ -> bool (* TODO *)

let ge_s (v1 : Value.t) (v2 : Value.t) : Value.t = match (v1, v2) with
  | (Const n1, Const n2) when n1 >= n2 -> const 1l
  | _ -> bool (* TODO *)

let eval (r : t) (v1 : Value.t) (v2 : Value.t) : Value.t =
  match r with
  | I32Eq -> eq v1 v2
  | I32Ne -> ne v1 v2
  | I32LtS -> lt_s v1 v2
  | I32GtS -> gt_s v1 v2
  | I32LeS -> le_s v1 v2
  | I32GeS -> ge_s v1 v2
