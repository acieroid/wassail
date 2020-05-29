open Core_kernel

(** Binary operations *)
module T = struct
  type op = Add | Sub | Mul | DivS | DivU | RemS | RemU | And | Or | Xor | Shl | ShrS | ShrU | Rotl | Rotr
  [@@deriving sexp, compare]
  type t = { op: op; typ: Type.t }
  [@@deriving sexp, compare]
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
  match b with
  | I32 op -> { typ = I32; op = of_op op }
  | I64 op -> { typ = I64; op = of_op op }
  | _ -> failwith "Unsupported type"

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
     | Rotr -> "rotr")

(** Evaluates a binary operation on two values *)
let eval (m : Memory.t) (b : t) (v1 : Value.t) (v2 : Value.t) : Value.t =
  match b.op with
  | Add -> Value.add (Memory.resolve m v1) (Memory.resolve m v2)
  | Sub -> Value.sub (Memory.resolve m v1 ) (Memory.resolve m v2)
  | Mul -> Value.mul (Memory.resolve m v1) (Memory.resolve m v2)
  | RemS -> Value.rem_s (Memory.resolve m v1) (Memory.resolve m v2)
  (* Don't resolve for operations that are mostly used for conditions *)
  | Shl -> Value.shl v1 v2
  | And -> Value.and_ v1 v2
  | Or -> Value.or_ v1 v2
  | Xor -> Value.xor v1 v2
  | _ -> raise (UnsupportedBinOp b)
