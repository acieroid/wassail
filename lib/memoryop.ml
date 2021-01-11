open Core_kernel
open Wasm

(** Description of memory operations (load and store) *)
module T = struct
  type extension = SX | ZX
  [@@deriving sexp, compare, equal]
  type pack_size = Pack8 | Pack16 | Pack32
  [@@deriving sexp, compare, equal]
  type t = {
    typ: Type.t;
    offset: int;
    (* The extension part is only use for load operation and should be ignored for store operations *)
    sz: (pack_size * extension) option;
    align: int;
  }
  [@@deriving sexp, compare, equal]
end
include T
let to_string (op : t) : string =
  Printf.sprintf "typ=%s offset=%d, sz=%s"
    (Type.to_string op.typ)
    op.offset
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
       | Wasm.Types.Pack8 -> Pack8
       | Wasm.Types.Pack16 -> Pack16
       | Wasm.Types.Pack32 -> Pack32),
      match ext with
      | Wasm.Types.SX -> SX
      | Wasm.Types.ZX -> ZX);
}
let of_wasm_store (op : Ast.storeop) : t = {
  typ = Type.of_wasm op.ty;
  align = op.align;
  offset = Int32.to_int_exn op.offset;
  sz = Option.map op.sz ~f:(function
      | Wasm.Types.Pack8 -> (Pack8, SX)
      | Wasm.Types.Pack16 -> (Pack16, SX)
      | Wasm.Types.Pack32 -> (Pack32, SX));
}
