open Core

module T = struct
  type pack_size = Pack8 | Pack16 | Pack32 (* TODO: duplicate with memoryop *)
  [@@deriving sexp, compare, equal]
  type op = Clz | Ctz | Popcnt | ExtendS of pack_size | Neg | Abs | Ceil | Floor | Trunc | Nearest | Sqrt
  [@@deriving sexp, compare, equal]
  type t = { op: op; typ: Type.t }
  [@@deriving sexp, compare, equal]
end
include T

let of_wasm (u : Wasm.Ast.unop) : t =
  let of_op (op : Wasm.Ast.IntOp.unop) : op = match op with
    | Clz -> Clz
    | Ctz -> Ctz
    | Popcnt -> Popcnt
    | ExtendS Pack8 -> ExtendS Pack8
    | ExtendS Pack16 -> ExtendS Pack16
    | ExtendS Pack32 -> ExtendS Pack32
  in
  let of_op_f (op : Wasm.Ast.FloatOp.unop) : op = match op with
    | Neg -> Neg
    | Abs -> Abs
    | Ceil -> Ceil
    | Floor -> Floor
    | Trunc -> Trunc
    | Nearest -> Nearest
    | Sqrt -> Sqrt
  in
  match u with
  | I32 op -> { typ = I32; op = of_op op }
  | I64 op -> { typ = I64; op = of_op op }
  | F32 op -> { typ = F32; op = of_op_f op }
  | F64 op -> { typ = F64; op = of_op_f op }

let pack_size_to_int (p : pack_size) : int = match p with
  | Pack8 -> 8
  | Pack16 -> 16
  | Pack32 -> 32

let to_mnemonic (u : t) : string =
  Printf.sprintf "%s.%s"
    (Type.to_string u.typ)
    (match u.op with
     | Clz -> "clz"
     | Ctz -> "ctz"
     | Popcnt -> "popcnt"
     | ExtendS p -> Printf.sprintf "extend%d_s" (pack_size_to_int p)
     | Abs -> "abs"
     | Neg -> "neg"
     | Sqrt -> "sqrt"
     | Ceil -> "ceil"
     | Floor -> "floor"
     | Trunc -> "trunc"
     | Nearest -> "nearest")

let to_string (u : t) : string = to_mnemonic u
