open Core_kernel
open Wasm

(** Description of memory operations (load and store) *)
module T = struct
  type extension = SX | ZX
  [@@deriving sexp, compare]
  type pack_size = Pack8 | Pack16 | Pack32
  [@@deriving sexp, compare]
  type t = {
    typ: Type.t;
    align: int;
    offset: int;
    (* The extension part is only use for load operation and should be ignored for store operations *)
    sz: (pack_size * extension) option;
  }
  [@@deriving sexp, compare]
end
include T
let to_string (op : t) : string =
  Printf.sprintf "align=%d, offset=%d, sz=%s"
    op.align op.offset
    (match op.sz with
     | Some (pack, ext) ->
       Printf.sprintf "%s,%s"
         (match pack with
          | Pack8 -> "8"
          | Pack16 -> "16"
          | Pack32 -> "32")
         (match ext with
          | SX -> "sx"
          | ZX -> "zx")
     | None -> "none")
let of_wasm_load (op : Ast.loadop) : t = {
  typ = Type.of_wasm op.ty;
  align = op.align;
  offset = Int32.to_int_exn op.offset;
  sz = Option.map op.sz ~f:(fun (pack, ext) ->
      (match pack with
       | Memory.Pack8 -> Pack8
       | Memory.Pack16 -> Pack16
       | Memory.Pack32 -> Pack32),
      match ext with
      | Memory.SX -> SX
      | Memory.ZX -> ZX);
}
let of_wasm_store (op : Ast.storeop) : t = {
  typ = Type.of_wasm op.ty;
  align = op.align;
  offset = Int32.to_int_exn op.offset;
  sz = Option.map op.sz ~f:(function
      | Memory.Pack8 -> (Pack8, SX)
      | Memory.Pack16 -> (Pack16, SX)
      | Memory.Pack32 -> (Pack32, SX));
}
