open Core_kernel

(** Binary operations *)
module T = struct
  type op = Add | Sub | Mul | DivS | DivU | RemS | RemU | And | Or | Xor | Shl | ShrS | ShrU | Rotl | Rotr | Min | Max | CopySign
  [@@deriving sexp, compare, equal]
  type t = { op: op; typ: Type.t }
  [@@deriving sexp, compare, equal]
end
include T

exception UnsupportedBinOp of t

(** Convert a wasm binop to an internal binop *)
let of_wasm (b : Wasm.Ast.binop) : t =
  let of_op (op : Wasm.Ast.IntOp.binop) : op = match op with
    | Add -> Add
    | Sub -> Sub
    | Mul -> Mul
    | DivS -> DivS
    | DivU -> DivU
    | RemS -> RemS
    | RemU -> RemU
    | And -> And
    | Or -> Or
    | Xor -> Xor
    | Shl -> Shl
    | ShrS -> ShrS
    | ShrU -> ShrU
    | Rotl -> Rotl
    | Rotr -> Rotr
  in
  let of_op_f (op : Wasm.Ast.FloatOp.binop) : op = match op with
    | Add -> Add
    | Sub -> Sub
    | Mul -> Mul
    | Div -> DivS
    | Min -> Min
    | Max -> Max
    | CopySign -> CopySign
  in
  match b with
  | I32 op -> { typ = I32; op = of_op op }
  | I64 op -> { typ = I64; op = of_op op }
  | F32 op -> { typ = F32; op = of_op_f op }
  | F64 op -> { typ = F64; op = of_op_f op }

let to_string (b : t) : string =
  Printf.sprintf "%s.%s"
    (Type.to_string b.typ)
    (match b.op with
     | Add -> "add"
     | Sub -> "sub"
     | Mul -> "mul"
     | DivS -> "div_s"
     | DivU -> "div_u"
     | RemS -> "rem_s"
     | RemU -> "rem_u"
     | And -> "and"
     | Or -> "or"
     | Xor -> "xor"
     | Shl -> "shl"
     | ShrS -> "shr_s"
     | ShrU -> "shr_u"
     | Rotl -> "rotl"
     | Rotr -> "rotr"
     | Min -> "min"
     | Max -> "max"
     | CopySign -> "copy_sign")
