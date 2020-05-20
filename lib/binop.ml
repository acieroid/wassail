open Core_kernel
open Value

exception UnsupportedBinOp of string

module T = struct
  (** Binary operations *)
  type t =
    | I32Add
    | I32Sub
    | I32Mul
    | I32RemS
    | I32Shl
    | I32And
    | I32Or
    (* XXX: there are many other operations *)
  [@@deriving sexp, compare, yojson]
end
include T

(** Convert a wasm binop to an internal binop *)
let of_wasm (b : Wasm.Ast.binop) : t =
  match b with
  | I32 Add -> I32Add
  | I32 Sub -> I32Sub
  | I32 Mul -> I32Mul
  | I32 DivS -> raise (UnsupportedBinOp "div_s")
  | I32 DivU -> raise (UnsupportedBinOp "div_u")
  | I32 RemS -> I32RemS
  | I32 RemU -> raise (UnsupportedBinOp "rem_u")
  | I32 And -> I32And
  | I32 Or -> I32Or
  | I32 Xor -> raise (UnsupportedBinOp "xor")
  | I32 Shl -> I32Shl
  | I32 ShrS -> raise (UnsupportedBinOp "shr_s")
  | I32 ShrU -> raise (UnsupportedBinOp "shr_u")
  | I32 Rotl -> I32Shl
  | I32 Rotr -> raise (UnsupportedBinOp "rot_r")
  | _ -> raise (UnsupportedBinOp "???")

let to_string (b : t) : string =
  match b with
  | I32Add -> "i32.add"
  | I32Sub -> "i32.sub"
  | I32Mul -> "i32.sub"
  | I32RemS -> "i32.rem_s"
  | I32Shl -> "i32.shl"
  | I32And -> "i32.and"
  | I32Or -> "i32.or"

let add (v1 : Value.t) (v2 : Value.t) : Value.t =
  match (v1, v2) with
  | (_, Symbolic (Const 0l)) -> v1
  | (Symbolic (Const n1), Symbolic (Const n2)) -> const (Int32.(+) n1 n2)
  | (Symbolic (Op _), _) -> simplify (Symbolic (Op (Plus, v1, v2)))
  | (Symbolic (Parameter i), _) -> simplify (Symbolic (Op (Plus, Symbolic (Parameter i), v2)))
  | (Symbolic (Global i), _) -> simplify (Symbolic (Op (Plus, Symbolic (Global i), v2)))
  | (Interval (Const a, Const b), Symbolic (Const n)) -> Interval (Const (Int32.(+) a n), Const (Int32.(+) b n))
  | (RightOpenInterval (Const x), Symbolic (Const y)) -> RightOpenInterval (Const (Int32.(+) x y))
  | (LeftOpenInterval (Const x), Symbolic (Const y)) -> RightOpenInterval (Const (Int32.(+) x y))
  | (RightOpenInterval (Parameter i), Symbolic (Const n)) -> RightOpenInterval (Op (Plus, parameter i, const n))
  | _ -> top (Printf.sprintf "add %s %s" (Value.to_string v1) (Value.to_string v2))

let sub (v1 : Value.t) (v2 : Value.t) : Value.t =
  match (v1, v2) with
  | (Symbolic (Const n1), Symbolic (Const n2)) -> const (Int32.(-) n1 n2)
  | (Symbolic (Global _), Symbolic (Const _)) -> simplify (Symbolic (Op (Minus, v1, v2)))
  | _ -> top (Printf.sprintf "sub %s %s" (Value.to_string v1) (Value.to_string v2))

let mul (v1 : Value.t) (v2 : Value.t) : Value.t =
  match (v1, v2) with
  | (Symbolic (Const n1), Symbolic (Const n2)) -> const (Int32.( * ) n1 n2)
  | _ -> top (Printf.sprintf "mul %s %s" (Value.to_string v1) (Value.to_string v2))

let rem_s (v1 : Value.t) (v2 : Value.t) : Value.t =
  match (v1, v2) with
  | (Symbolic (Const n1), Symbolic (Const n2)) -> const (Int32.rem n1 n2)
  | _ -> top (Printf.sprintf "rem_s %s %s" (Value.to_string v1) (Value.to_string v2))

let shl (v1 : Value.t) (v2 : Value.t) : Value.t =
  match (v1, v2) with
  | (Symbolic (Const n1), Symbolic (Const n2)) -> const (Int32.shift_left n1 (Int32.to_int_exn n2))
  | (Interval (Const a, Const b), Symbolic (Const 2l)) -> Interval (Const (Int32.( * ) a 4l), Const (Int32.( * ) b 4l))
  | (Symbolic _, Symbolic (Const 2l)) -> symbolic (Op (Times, v1, Symbolic (Const 4l)))
  | (RightOpenInterval (Const 0l), Symbolic (Const 2l)) -> v1
  | _ -> top (Printf.sprintf "shl %s %s" (Value.to_string v1) (Value.to_string v2))

let (land) (v1 : Value.t) (v2 : Value.t) : Value.t =
  match (v1, v2) with
  | (Symbolic (Const n1), Symbolic (Const n2)) -> const (Int32.(land) n1 n2)
  | (_, Symbolic (Const 1l)) -> v1
  | (Symbolic (Const 1l), _) -> v1
  | _ -> top (Printf.sprintf "land %s %s" (Value.to_string v1) (Value.to_string v2))

let (lor) (v1 : Value.t) (v2 : Value.t) : Value.t =
  match (v1, v2) with
  | (Symbolic (Const n1), Symbolic (Const n2)) -> const (Int32.(lor) n1 n2)
  | (_, Symbolic (Const 1l)) -> v1
  | (Symbolic (Const 1l), _) -> v1
  | _ -> top (Printf.sprintf "land %s %s" (Value.to_string v1) (Value.to_string v2))

(** Evaluates a binary operation on two values *)
let eval (m : Memory.t) (b : t) (v1 : Value.t) (v2 : Value.t) : Value.t =
  match b with
  | I32Add -> add (Memory.resolve m v1) (Memory.resolve m v2)
  | I32Sub -> sub (Memory.resolve m v1) (Memory.resolve m v2)
  | I32Mul -> mul (Memory.resolve m v1) (Memory.resolve m v2)
  | I32RemS -> rem_s (Memory.resolve m v1) (Memory.resolve m v2)
  (* Don't resolve for operations that are mostly used for conditions *)
  | I32Shl -> shl (v1) (v2)
  | I32And -> (land) v1 v2
  | I32Or -> (lor) v1 v2
