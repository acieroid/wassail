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

(*
(** Evaluates a binary operation on two values *)
let eval (m : Memory.t) (b : t) (v1 : Value.t) (v2 : Value.t) : Value.t =
  let v1' =
    (* TODO: the problem is the following.
       we have *x << 2, where *x is 0.
       if we resolve, that means we have 0, which is later used in a condition
       if we don't resolve, we have *x << 2 resulting in Top.
       solution: keep *x and use Op? instead of returning top*)
    (* Try to resolve the value, only if it results in a fully precise constant *)
    let resolved = Memory.resolve m v1 in
    Printf.printf "resolved v1: %s\n" (Value.to_string resolved);
    if true (* Value.is_imprecise resolved *) then v1 else resolved in
  let v2' =
    let resolved = Memory.resolve m v2 in
    Printf.printf "resolved v2: %s\n" (Value.to_string resolved);
    if true (* Value.is_imprecise resolved *) then v2 else resolved in
  let res = match b.op with
  | Add -> Value.add v1' v2'
  | Sub -> Value.sub v1' v2'
  | Mul -> Value.mul v1' v2'
  | RemS -> Value.rem_s v1' v2'
  | Shl -> Value.shl v1' v2'
  | And -> Value.and_ v1' v2'
  | Or -> Value.or_ v1' v2'
  | Xor -> Value.xor v1' v2'
  | _ -> raise (UnsupportedBinOp b) in
  Printf.printf "op %s is called with %s and %s, resulting in %s\n" (to_string b) (Value.to_string v1') (Value.to_string v2') (Value.to_string res);
  res

  *)
