open Core_kernel
open Wasm

module ByteAbstr = struct
  module T = struct
    type t =
      | Const of char
      | Byte
    [@@deriving sexp, compare]
  end
  include T
  let zero = Const (Char.of_int_exn 0)
  let join (b1 : t) (b2 : t) : t =
    match (b1, b2) with
    | Const x, Const y when Stdlib.(x = y) -> Const x
    | _, _ -> Byte
end

module T = struct
  type t = {
    data: ByteAbstr.t; (* Abstraction: everything merged into the same value *)
    max_size: int option;
  }
  [@@deriving sexp, compare]
end
include T
let page_size = 65536
let of_wasm (m : Ast.memory) : t =
  match m.it.mtype with
  | MemoryType t ->
    {
      data = ByteAbstr.zero;
      max_size = Option.map t.max ~f:Int32.to_int_exn
    }
