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
    min_size: int;
    max_size: int option;
  }
  [@@deriving sexp, compare]
end
include T
let page_size = 65536

let of_wasm_type (t : Types.memory_type) : t = match t with
  | MemoryType t -> {
      min_size = Int32.to_int_exn t.min;
      max_size = Option.map t.max ~f:Int32.to_int_exn
    }

let of_wasm (m : Ast.memory) : t = of_wasm_type m.it.mtype
