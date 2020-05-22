open Core_kernel
open Wasm
open Value

module T = struct
  (** Relational operation *)
  type op = Eq | Ne | LtS | LtU | GtS | GtU | LeS | LeU | GeS | GeU
  [@@deriving sexp, compare]
  type t = { op: op; typ: Type.t }
  [@@deriving sexp, compare]
end
include T

exception UnsupportedRelOp of t

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
  | _ -> failwith "unsupported type"
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

let eq (v1 : Value.t) (v2 : Value.t) : Value.t = match (v1.value, v2.value) with
  | (Symbolic (Const n1), Symbolic (Const n2)) when PrimValue.eq n1 n2 -> const (PrimValue.of_int_t n1 1)
  | _ -> bool (* TODO *)

let ne (v1 : Value.t) (v2 : Value.t) : Value.t = match (v1.value, v2.value) with
  | (Symbolic (Const n1), Symbolic (Const n2)) when PrimValue.ne n1 n2 -> const (PrimValue.of_int_t n1 1)
  | _ -> bool (* TODO *)

let lt_s (v1 : Value.t) (v2 : Value.t) : Value.t = match (v1.value, v2.value) with
  | (Symbolic (Const n1), Symbolic (Const n2)) when PrimValue.lt_s n1 n2 -> const (PrimValue.of_int_t n1 1)
  | (Symbolic a, Symbolic b) ->
    assert Stdlib.(v1.typ = v2.typ);
    { value = Symbolic (Op (Lt, (Symbolic a), (Symbolic b))); typ = I32 };
  | _ ->  bool (* TODO *)

let gt_s (v1 : Value.t) (v2 : Value.t) : Value.t = match (v1.value, v2.value) with
  | (Symbolic (Const n1), Symbolic (Const n2)) when PrimValue.gt_s n1 n2 -> const (PrimValue.of_int_t n1 1)
  | _ -> bool (* TODO *)

let le_s (v1 : Value.t) (v2 : Value.t) : Value.t = match (v1.value, v2.value) with
  | (Symbolic (Const n1), Symbolic (Const n2)) when PrimValue.le_s n1 n2 -> const (PrimValue.of_int_t n1 1)
  | _ -> bool (* TODO *)

let ge_s (v1 : Value.t) (v2 : Value.t) : Value.t = match (v1.value, v2.value) with
  | (Symbolic (Const n1), Symbolic (Const n2)) when PrimValue.ge_s n1 n2 -> const (PrimValue.of_int_t n1 1)
  | _ -> bool (* TODO *)

let eval (r : t) (v1 : Value.t) (v2 : Value.t) : Value.t =
  match r with
  | { typ = I32; op = Eq } -> eq v1 v2
  | { typ = I32; op = Ne } -> ne v1 v2
  | { typ = I32; op = LtS } -> lt_s v1 v2
  | { typ = I32; op = GtS } -> gt_s v1 v2
  | { typ = I32; op = LeS } -> le_s v1 v2
  | { typ = I32; op = GeS } -> ge_s v1 v2
  | _ -> raise (UnsupportedRelOp r)
