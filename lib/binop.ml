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
  [@@deriving sexp, compare]
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

(** Evaluates a binary operation on two values *)
let eval (b : t) (v1 : Value.t) (v2 : Value.t) : Value.t =
  { value = begin match (b, v1.value, v2.value) with
        | (I32Add, Const n1, Const n2) -> Const (Int32.(+) n1 n2)
        | (I32Add, _, _) -> Int
        | (I32Sub, Const n1, Const n2) -> Const (Int32.(-) n1 n2)
        | (I32Sub, _, _) -> Int
        | (I32Mul, Const n1, Const n2) -> Const (Int32.( * ) n1 n2)
        | (I32Mul, _, _) -> Int
        | (I32RemS, Const n1, Const n2) -> Const (Int32.rem n1 n2)
        | (I32RemS, _, _) -> Int
        | (I32Shl, Const n1, Const n2) -> Const (Int32.shift_left n1 (Int32.to_int_exn n2))
        | (I32Shl, _, _) -> Int
        | (I32And, Const n1, Const n2) -> Const (Int32.(land) n1 n2)
        | (I32And, _, _) -> Int
      end;
    sources = SourceSet.union v1.sources v2.sources
  }

          
