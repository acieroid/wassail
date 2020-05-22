open Core_kernel
open Value

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

let add (v1 : Value.t) (v2 : Value.t) : Value.t =
  assert Stdlib.(v1.typ = v2.typ);
  let v = match (v1.value, v2.value) with
  | (_, Symbolic (Const z)) when PrimValue.is_zero z -> v1.value
  | (Symbolic (Const n1), Symbolic (Const n2)) -> (const (PrimValue.add n1 n2)).value
  | (Symbolic (Op _), _) -> simplify_value (Symbolic (Op (Plus, v1.value, v2.value)))
  | (Symbolic (Parameter i), _) -> simplify_value (Symbolic (Op (Plus, Symbolic (Parameter i), v2.value)))
  | (Symbolic (Global i), _) -> simplify_value (Symbolic (Op (Plus, Symbolic (Global i), v2.value)))
  | (Interval (Const a, Const b), Symbolic (Const n)) -> Interval (Const (PrimValue.add a n), Const (PrimValue.add b n))
  | (RightOpenInterval (Const x), Symbolic (Const y)) -> RightOpenInterval (Const (PrimValue.add x y))
  | (LeftOpenInterval (Const x), Symbolic (Const y)) -> RightOpenInterval (Const (PrimValue.add x y))
  | (RightOpenInterval (Parameter i), Symbolic (Const n)) -> RightOpenInterval (Op (Plus, (parameter I32 i).value, (const n).value))
  | _ -> (top (Printf.sprintf "add %s %s" (Value.to_string v1) (Value.to_string v2))).value
  in { value = v; typ = v1.typ }

let sub (v1 : Value.t) (v2 : Value.t) : Value.t =
  assert Stdlib.(v1.typ = v2.typ);
  match (v1.value, v2.value) with
  | (Symbolic (Const n1), Symbolic (Const n2)) -> const (PrimValue.sub n1 n2)
  | (Symbolic (Global _), Symbolic (Const _)) -> { value = simplify_value (Symbolic (Op (Minus, v1.value, v2.value))); typ = v1.typ }
  | _ -> top (Printf.sprintf "sub %s %s" (Value.to_string v1) (Value.to_string v2))

let mul (v1 : Value.t) (v2 : Value.t) : Value.t =
  match (v1.value, v2.value) with
  | (Symbolic (Const n1), Symbolic (Const n2)) -> const (PrimValue.mul n1 n2)
  | _ -> top (Printf.sprintf "mul %s %s" (Value.to_string v1) (Value.to_string v2))

let rem_s (v1 : Value.t) (v2 : Value.t) : Value.t =
  match (v1.value, v2.value) with
  | (Symbolic (Const n1), Symbolic (Const n2)) -> const (PrimValue.rem_s n1 n2)
  | _ -> top (Printf.sprintf "rem_s %s %s" (Value.to_string v1) (Value.to_string v2))

let shl (v1 : Value.t) (v2 : Value.t) : Value.t =
  match (v1.value, v2.value) with
  (* | (Symbolic (Const n1), Symbolic (Const n2)) -> const (Int32.shift_left n1 (Int32.to_int_exn n2)) *) (* TODO *)
  (* | (Interval (Const a, Const b), Symbolic (Const 2l)) -> Interval (Const (Int32.( * ) a 4l), Const (Int32.( * ) b 4l)) *) (* TODO *)
  (* | (Symbolic _, Symbolic (Const 2l)) -> symbolic (Op (Times, v1, Symbolic (Const 4l))) *) (* TODO *)
  (* | (RightOpenInterval (Const 0l), Symbolic (Const 2l)) -> v1 *) (* TODO *)
  | _ -> top (Printf.sprintf "shl %s %s" (Value.to_string v1) (Value.to_string v2))

let and_ (v1 : Value.t) (v2 : Value.t) : Value.t =
  match (v1.value, v2.value) with
  | (Symbolic (Const n1), Symbolic (Const n2)) -> const PrimValue.(and_ n1 n2)
  | (_, Symbolic (Const one)) when PrimValue.is one 1 -> v1
  | (Symbolic (Const one), _) when PrimValue.is one 1 -> v1
  | _ -> top (Printf.sprintf "land %s %s" (Value.to_string v1) (Value.to_string v2))

let or_ (v1 : Value.t) (v2 : Value.t) : Value.t =
  match (v1.value, v2.value) with
  | (Symbolic (Const n1), Symbolic (Const n2)) -> const PrimValue.(or_ n1 n2)
  | (_, Symbolic (Const one)) when PrimValue.is one 1 -> v1
  | (Symbolic (Const one), _) when PrimValue.is one 1 -> v1
  | _ -> top (Printf.sprintf "land %s %s" (Value.to_string v1) (Value.to_string v2))

(** Evaluates a binary operation on two values *)
let eval (m : Memory.t) (b : t) (v1 : Value.t) (v2 : Value.t) : Value.t =
  match b.op with
  | Add -> add (Memory.resolve m v1) (Memory.resolve m v2)
  | Sub -> sub (Memory.resolve m v1 ) (Memory.resolve m v2)
  | Mul -> mul (Memory.resolve m v1) (Memory.resolve m v2)
  | RemS -> rem_s (Memory.resolve m v1) (Memory.resolve m v2)
  (* Don't resolve for operations that are mostly used for conditions *)
  | Shl -> shl (v1) (v2)
  | And -> and_ v1 v2
  | Or -> or_ v1 v2
  | _ -> raise (UnsupportedBinOp b)
