open Core_kernel
open Wasm
open Value

module T = struct
  (** Binary operations *)
  type t =
    | I32Add
    | I32Sub
    | I32Mul
    | I32RemS
    | I32Shl
    | I32And
    (* XXX: there are many other operations *)
  [@@deriving sexp, compare, yojson]
end
include T
let of_wasm (b : Ast.binop) : t =
  match b with
  | I32 Add -> I32Add
  | I32 Sub -> I32Sub
  | I32 Mul -> I32Mul
  | I32 DivS -> failwith "unsupported operation: DivS"
  | I32 DivU -> failwith "unsupported operation: DivU"
  | I32 RemS -> I32RemS
  | I32 RemU -> failwith "unsupported operation: RemU"
  | I32 And -> I32And
  | I32 Or -> failwith "unsupported operation: Or"
  | I32 Xor -> failwith "unsupported operation: Xor"
  | I32 Shl -> I32Shl
  | I32 ShrS -> failwith "unsupported operation: ShrS"
  | I32 ShrU -> failwith "unsupported operation: ShrU"
  | I32 Rotl -> I32Shl
  | I32 Rotr -> failwith "unsupported operation: Rotr"
  | _ -> failwith "unsupported type"

let to_string (b : t) : string =
  match b with
  | I32Add -> "i32.add"
  | I32Sub -> "i32.sub"
  | I32Mul -> "i32.sub"
  | I32RemS -> "i32.rem_s"
  | I32Shl -> "i32.shl"
  | I32And -> "i32.and"

let add (v1 : Value.t) (v2 : Value.t) : Value.t =
  match (v1, v2) with
  | (_, Const 0l) -> v1
  | (Const n1, Const n2) -> Const (Int32.(+) n1 n2)
  | (Op _, _) -> simplify (Op (Plus, v1, v2))
  | _ -> top (Printf.sprintf "add %s %s" (Value.to_string v1) (Value.to_string v2))

let sub (v1 : Value.t) (v2 : Value.t) : Value.t =
  match (v1, v2) with
  | (Const n1, Const n2) -> Const (Int32.(-) n1 n2)
  | (Global _, Const _) -> Op (Minus, v1, v2)
  | _ -> top (Printf.sprintf "sub %s %s" (Value.to_string v1) (Value.to_string v2))

let mul (v1 : Value.t) (v2 : Value.t) : Value.t =
  match (v1, v2) with
  | (Const n1, Const n2) -> Const (Int32.( * ) n1 n2)
  | _ -> top (Printf.sprintf "mul %s %s" (Value.to_string v1) (Value.to_string v2))

let rem_s (v1 : Value.t) (v2 : Value.t) : Value.t =
  match (v1, v2) with
  | (Const n1, Const n2) -> Const (Int32.rem n1 n2)
  | _ -> top (Printf.sprintf "rem_s %s %s" (Value.to_string v1) (Value.to_string v2))

let shl (v1 : Value.t) (v2 : Value.t) : Value.t =
  match (v1, v2) with
  | (Const n1, Const n2) -> Const (Int32.shift_left n1 (Int32.to_int_exn n2))
  | _ -> top (Printf.sprintf "shl %s %s" (Value.to_string v1) (Value.to_string v2))

let (land) (v1 : Value.t) (v2 : Value.t) : Value.t =
  match (v1, v2) with
  | (Const n1, Const n2) -> Const (Int32.(land) n1 n2)
  | (Interval (0l, 1l), Const 1l) -> v1
  | _ -> top (Printf.sprintf "land %s %s" (Value.to_string v1) (Value.to_string v2))

(** Evaluates a binary operation on two values *)
let eval (b : t) (v1 : Value.t) (v2 : Value.t) : Value.t =
  match b with
  | I32Add -> add v1 v2
  | I32Sub -> sub v1 v2
  | I32Mul -> mul v1 v2
  | I32RemS -> rem_s v1 v2
  | I32Shl -> shl v1 v2
  | I32And -> (land) v1 v2
