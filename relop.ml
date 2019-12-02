open Core
open Wasm

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
let eval (r : t) (v1 : Value.t) (v2 : Value.t) : Value.t =
  match (r, v1, v2) with
  | (I32Eq, Const n1, Const n2) -> Const (if n1 = n2 then 1l else 0l)
  | (I32Eq, _, _) -> Int
  | (I32Ne, Const n1, Const n2) -> Const (if n1 <> n2 then 1l else 0l)
  | (I32Ne, _, _) -> Int
  | (I32LtS, Const n1, Const n2) -> Const (if n1 < n2 then 1l else 0l)
  | (I32LtS, _, _) -> Int
  | (I32GtS, Const n1, Const n2) -> Const (if n1 > n2 then 1l else 0l)
  | (I32GtS, _, _) -> Int
  | (I32LeS, Const n1, Const n2) -> Const (if n1 <= n2 then 1l else 0l)
  | (I32LeS, _, _) -> Int
  | (I32GeS, Const n1, Const n2) -> Const (if n1 >= n2 then 1l else 0l)
  | (I32GeS, _, _) -> Int

