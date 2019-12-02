open Core
open Wasm
open Helpers

module T = struct
  (** Binary operations *)
  type t =
    | I32Add
    | I32Sub
    | I32Mul
    (* XXX: there are many other operations *)
  [@@deriving sexp, compare]
end
include T
let of_wasm (b : Ast.binop) : t =
  match b with
  | I32 Add -> I32Add
  | I32 Sub -> I32Sub
  | I32 Mul -> I32Mul
  | I32 _ -> failwith "unsupported operation"
  | _ -> failwith "unsupported type"

let to_string (b : t) : string =
  match b with
  | I32Add -> "i32.add"
  | I32Sub -> "i32.sub"
  | I32Mul -> "i32.sub"

(** Evaluates a binary operation on two values *)
let eval (b : t) (v1 : Value.t) (v2 : Value.t) : Value.t =
  { value = begin match (b, v1.value, v2.value) with
        | (I32Add, Const n1, Const n2) -> Const (Int32.(+) n1 n2)
        | (I32Add, _, _) -> Int
        | (I32Sub, Const n1, Const n2) -> Const (Int32.(-) n1 n2)
        | (I32Sub, _, _) -> Int
        | (I32Mul, Const n1, Const n2) -> Const (Int32.( * ) n1 n2)
        | (I32Mul, _, _) -> Int
      end;
    sources = IntPairSet.union v1.sources v2.sources
  }

          
