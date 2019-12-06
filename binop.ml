open Core_kernel
open Wasm
open Helpers

module T = struct
  (** Binary operations *)
  type t =
    | I32Add
    | I32Sub
    | I32Mul
    | I32RemS
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
  | I32 And -> failwith "unsupported operation: And"
  | I32 Or -> failwith "unsupported operation: Or"
  | I32 Xor -> failwith "unsupported operation: Xor"
  | I32 Shl -> failwith "unsupported operation: Shl"
  | I32 ShrS -> failwith "unsupported operation: ShrS"
  | I32 ShrU -> failwith "unsupported operation: ShrU"
  | I32 Rotl -> failwith "unsupported operation: Rotl"
  | I32 Rotr -> failwith "unsupported operation: Rotr"
  | _ -> failwith "unsupported type"

let to_string (b : t) : string =
  match b with
  | I32Add -> "i32.add"
  | I32Sub -> "i32.sub"
  | I32Mul -> "i32.sub"
  | I32RemS -> "i32.rem_s"

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
      end;
    sources = IntPairSet.union v1.sources v2.sources
  }

          
