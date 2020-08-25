open Core_kernel
open Wasm

module T = struct
  (** Relational operation *)
  type op = Eq | Ne | LtS | LtU | GtS | GtU | LeS | LeU | GeS | GeU
  [@@deriving sexp, compare, equal]
  type t = { op: op; typ: Type.t }
  [@@deriving sexp, compare, equal]
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
  let of_op_f (op : Wasm.Ast.FloatOp.relop) : op = match op with
    | Eq -> Eq
    | Ne -> Ne
    | Lt -> LtS
    | Gt -> GtS
    | Le -> LeS
    | Ge -> GeS
  in
  match r with
  | I32 op -> { typ = I32; op = of_op op }
  | I64 op -> { typ = I64; op = of_op op }
  | F32 op -> { typ = F32; op = of_op_f op }
  | F64 op -> { typ = F64; op = of_op_f op }

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
