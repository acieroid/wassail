open Core
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
    align: int32;
  }
  [@@deriving sexp, compare, equal]
end
include T

let suffix_to_string (op : t) (is_load : bool) : string =
  match op.sz with
  | None -> ""
  | Some (Pack8, SX) -> if is_load then "8_s" else "8"
  | Some (Pack16, SX) -> if is_load then "16_s" else "16"
  | Some (Pack32, SX) -> if is_load then "32_s" else "32"
  | Some (Pack8, ZX) -> "8_u"
  | Some (Pack16, ZX) -> "16_u"
  | Some (Pack32, ZX) -> "32_u"

let size (op : t) : int32 =
  match op.sz with
  | None -> begin match Type.size op.typ with
      | 32l -> 4l
      | 64l -> 8l
      | _ -> assert(false)
    end
  | Some (Pack8, _) -> 1l
  | Some (Pack16, _) -> 2l
  | Some (Pack32, _) -> 4l

let to_string (op : t) : string =
  let offset = if op.offset = 0 then "" else Printf.sprintf "offset=%d" op.offset in
  let align_exp = Int32.pow 2l op.align in
  let align = if Int32.(align_exp = size op) then "" else (Printf.sprintf "align=%ld" align_exp) in
  let sep = if String.is_empty offset || String.is_empty align then "" else " " in
  Printf.sprintf "%s%s%s" offset sep align

let of_wasm_load (op : Ast.loadop) : t = {
  typ = Type.of_wasm op.ty;
  align = Int32.of_int_exn op.align;
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
  align = Int32.of_int_exn op.align;
  offset = Int32.to_int_exn op.offset;
  sz = Option.map op.sz ~f:(function
      | Wasm.Types.Pack8 -> (Pack8, SX)
      | Wasm.Types.Pack16 -> (Pack16, SX)
      | Wasm.Types.Pack32 -> (Pack32, SX));
}
