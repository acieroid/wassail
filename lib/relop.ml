open Core_kernel
open Wasm

module T = struct
  (** Relational operation *)
  type op = Eq | Ne | LtS | LtU | GtS | GtU | LeS | LeU | GeS | GeU
  [@@deriving sexp, compare]
  type t = { op: op; typ: Type.t }
  [@@deriving sexp, compare]
end
include T

exception UnsupportedRelOp of t
[@@deriving sexp]

let of_wasm (r : Ast.relop) : t =
  let of_op (op : Wasm.Ast.IntOp.relop) : op = match op with
    | Eq -> Eq
    | Ne -> Ne
    | LtS -> LtS
    | LtU -> LtU
    | GtS -> GtS
    | GtU -> GtU
    | LeS -> LeS
    | LeU -> LeU
    | GeS -> GeS
    | GeU -> GeU
  in
  match r with
  | I32 op -> { typ = I32; op = of_op op }
  | I64 op -> { typ = I64; op = of_op op }
  | _ -> failwith "unsupported type: float in relop"

let to_string (r : t) : string =
  Printf.sprintf "%s.%s"
    (Type.to_string r.typ)
    (match r.op with
     | Eq -> "eq"
     | Ne -> "ne"
     | LtS -> "lt_s"
     | LtU -> "lt_u"
     | GtS -> "gt_s"
     | GtU -> "gt_u"
     | LeS -> "le_s"
     | LeU -> "le_u"
     | GeS -> "ge_s"
     | GeU -> "ge_u")
(*
let eq (v1 : Value.t) (v2 : Value.t) : Value.t = match (v1.value, v2.value) with
  | (Symbolic (Const n1), Symbolic (Const n2)) when Prim_value.eq n1 n2 -> Value.true_
  | _ -> Value.bool

let ne (v1 : Value.t) (v2 : Value.t) : Value.t = match (v1.value, v2.value) with
  | (Symbolic (Const n1), Symbolic (Const n2)) when Prim_value.ne n1 n2 -> Value.true_
  | _ -> Value.bool

let lt_s (v1 : Value.t) (v2 : Value.t) : Value.t = match (v1.value, v2.value) with
  | (Symbolic (Const n1), Symbolic (Const n2)) when Prim_value.lt_s n1 n2 -> Value.true_
  | (Symbolic a, Symbolic b) ->
    assert Stdlib.(v1.typ = v2.typ);
    { value = Symbolic (Op (Lt, (Symbolic a), (Symbolic b))); typ = I32 };
  | _ ->  Value.bool

let lt_u (v1 : Value.t) (v2 : Value.t) : Value.t = match (v1.value, v2.value) with
  | (Symbolic (Const n1), Symbolic (Const n2)) when Prim_value.lt_u n1 n2 -> Value.true_
  | _ -> Value.bool

let gt_s (v1 : Value.t) (v2 : Value.t) : Value.t = match (v1.value, v2.value) with
  | (Symbolic (Const n1), Symbolic (Const n2)) when Prim_value.gt_s n1 n2 -> Value.true_
  | _ -> Value.bool

let gt_u (v1 : Value.t) (v2 : Value.t) : Value.t = match (v1.value, v2.value) with
  | (Symbolic (Const n1), Symbolic (Const n2)) when Prim_value.gt_u n1 n2 -> Value.true_
  | _ -> Value.bool

let le_s (v1 : Value.t) (v2 : Value.t) : Value.t = match (v1.value, v2.value) with
  | (Symbolic (Const n1), Symbolic (Const n2)) when Prim_value.le_s n1 n2 -> Value.true_
  | _ -> Value.bool

let le_u (v1 : Value.t) (v2 : Value.t) : Value.t = match (v1.value, v2.value) with
  | (Symbolic (Const n1), Symbolic (Const n2)) when Prim_value.le_u n1 n2 -> Value.true_
  | _ -> Value.bool

let ge_s (v1 : Value.t) (v2 : Value.t) : Value.t = match (v1.value, v2.value) with
  | (Symbolic (Const n1), Symbolic (Const n2)) when Prim_value.ge_s n1 n2 -> Value.true_
  | _ -> Value.bool

let ge_u (v1 : Value.t) (v2 : Value.t) : Value.t = match (v1.value, v2.value) with
  | (Symbolic (Const n1), Symbolic (Const n2)) when Prim_value.ge_u n1 n2 -> Value.true_
  | _ -> Value.bool

let eval (r : t) (v1 : Value.t) (v2 : Value.t) : Value.t =
  match r.op with
  | Eq -> eq v1 v2
  | Ne -> ne v1 v2
  | LtS -> lt_s v1 v2
  | LtU -> lt_u v1 v2
  | GtS -> gt_s v1 v2
  | GtU -> gt_u v1 v2
  | LeS -> le_s v1 v2
  | LeU -> le_u v1 v2
  | GeS -> ge_s v1 v2
  | GeU -> ge_u v1 v2
*)
