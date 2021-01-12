open Core_kernel
open Wasm

module T = struct
  type t = {
    min_size: int;
    max_size: int option;
  }
  [@@deriving sexp, compare, equal]
end
include T

let page_size = 65536

let of_wasm_type (t : Types.memory_type) : t = match t with
  | MemoryType t -> {
      min_size = Int32.to_int_exn t.min;
      max_size = Option.map t.max ~f:Int32.to_int_exn
    }

let of_wasm (m : Ast.memory) : t = of_wasm_type m.it.mtype
