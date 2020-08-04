open Wasm

(** Test operations *)
module T = struct
  type t =
    | ExtendSI32
    | ExtendUI32
    | WrapI64
    | TruncSF32
    | TruncUF32
    | TruncSF64
    | TruncUF64
    | TruncSatSF32
    | TruncSatUF32
    | TruncSatSF64
    | TruncSatUF64
    | ReinterpretFloat
    | FloatOp (* TODO: refine *)
  [@@deriving sexp, compare, equal]
end
include T
let of_wasm (t : Ast.cvtop) : t =
  match t with
  | I32 ExtendSI32 -> ExtendSI32
  | I32 ExtendUI32 -> ExtendUI32
  | I32 WrapI64 -> WrapI64
  | I32 TruncSF32 -> TruncSF32
  | I32 TruncUF32 -> TruncUF32
  | I32 TruncSF64 -> TruncSF64
  | I32 TruncUF64 -> TruncUF64
  | I32 TruncSatSF32 -> TruncSatSF32
  | I32 TruncSatUF32 -> TruncSatUF32
  | I32 TruncSatSF64 -> TruncSatSF64
  | I32 TruncSatUF64 -> TruncSatUF64
  | I32 ReinterpretFloat -> ReinterpretFloat
  | I64 ExtendSI32 -> ExtendSI32
  | I64 ExtendUI32 -> ExtendUI32
  | I64 WrapI64 -> WrapI64
  | I64 TruncSF32 -> TruncSF32
  | I64 TruncUF32 -> TruncUF32
  | I64 TruncSF64 -> TruncSF64
  | I64 TruncUF64 -> TruncUF64
  | I64 TruncSatSF32 -> TruncSatSF32
  | I64 TruncSatUF32 -> TruncSatUF32
  | I64 TruncSatSF64 -> TruncSatSF64
  | I64 TruncSatUF64 -> TruncSatUF64
  | I64 ReinterpretFloat -> ReinterpretFloat
  | F32 _ | F64 _ -> FloatOp
let to_string (t : t) : string =
  match t with
  | _ -> "cvt"
