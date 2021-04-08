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

let suffix_to_string (op : t) : string =
  match op.sz with
  | None -> ""
  | Some (Pack8, SX) -> "8_s"
  | Some (Pack16, SX) -> "16_s"
  | Some (Pack32, SX) -> "32_s"
  | Some (Pack8, ZX) -> "8_u"
  | Some (Pack16, ZX) -> "16_u"
  | Some (Pack32, ZX) -> "32_u"

let size (op : t) : int =
  match op.sz with
  | None -> Type.size op.typ
  | Some (Pack8, _) -> 1
  | Some (Pack16, _) -> 2
  | Some (Pack32, _) -> 4

let to_string (op : t) : string =
  Printf.sprintf "%s%s"
    (if op.offset = 0 then "" else Printf.sprintf "offset=%d " op.offset)
    (if op.align = 0 || op.align = size op then "" else (Printf.sprintf "align=%d" op.align))

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
