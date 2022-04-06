open Core
open Wasm

module T = struct
  (** Relational operation *)
  type op = Eq | Ne | LtS | LtU | GtS | GtU | LeS | LeU | GeS | GeU | Lt | Gt | Le | Ge
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
    | Lt -> Lt
    | Gt -> Gt
    | Le -> Le
    | Ge -> Ge
  in
  match r with
  | I32 op -> { typ = I32; op = of_op op }
  | I64 op -> { typ = I64; op = of_op op }
  | F32 op -> { typ = F32; op = of_op_f op }
  | F64 op -> { typ = F64; op = of_op_f op }

let to_mnemonic (r : t) : string =
  Printf.sprintf "%s.%s"
    (Type.to_string r.typ)
    (match r.op with
     | Eq -> "eq"
     | Ne -> "ne"
     | Lt -> "lt"
     | LtS -> "lt_s"
     | LtU -> "lt_u"
     | Gt -> "gt"
     | GtS -> "gt_s"
     | GtU -> "gt_u"
     | Le -> "le"
     | LeS -> "le_s"
     | LeU -> "le_u"
     | Ge -> "ge"
     | GeS -> "ge_s"
     | GeU -> "ge_u")

let  to_string (r : t) : string = to_mnemonic r
