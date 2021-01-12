open Wasm

(** Test operations *)
module T = struct
  type op =
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
    | ConvertSI32
    | ConvertUI32
    | ConvertSI64
    | ConvertUI64
    | PromoteF32
    | DemoteF64
    | ReinterpretInt
  [@@deriving sexp, compare, equal]
  type t = { op: op; typ: Type.t }
  [@@deriving sexp, compare, equal]
end
include T
let of_wasm (t : Ast.cvtop) : t =
  let of_op (op : Wasm.Ast.IntOp.cvtop) : op = match op with
  | ExtendSI32 -> ExtendSI32
  | ExtendUI32 -> ExtendUI32
  | WrapI64 -> WrapI64
  | TruncSF32 -> TruncSF32
  | TruncUF32 -> TruncUF32
  | TruncSF64 -> TruncSF64
  | TruncUF64 -> TruncUF64
  | TruncSatSF32 -> TruncSatSF32
  | TruncSatUF32 -> TruncSatUF32
  | TruncSatSF64 -> TruncSatSF64
  | TruncSatUF64 -> TruncSatUF64
  | ReinterpretFloat -> ReinterpretFloat in
  let of_op_f (op : Wasm.Ast.FloatOp.cvtop) : op = match op with
    | ConvertSI32 -> ConvertSI32
    | ConvertUI32 -> ConvertUI32
    | ConvertSI64 -> ConvertSI64
    | ConvertUI64 -> ConvertUI64
    | PromoteF32 -> PromoteF32
    | DemoteF64 -> DemoteF64
    | ReinterpretInt -> ReinterpretInt in
  match t with
  | I32 op -> { typ = I32; op = of_op op }
  | I64 op -> { typ = I64; op = of_op op }
  | F32 op -> { typ = F32; op = of_op_f op }
  | F64 op -> { typ = F64; op = of_op_f op }

let to_mnemonic (t : t) : string =
  Printf.sprintf "%s.%s"
    (Type.to_string t.typ)
    (match t.op with
     | ExtendSI32 -> "extend32_s"
     | ExtendUI32 -> "extend32_u"
     | WrapI64 -> "wrap_i64"
     | TruncSF32 -> "trunc_f32_s"
     | TruncUF32 -> "trunc_f32_u"
     | TruncSF64 -> "trunc_f64_s"
     | TruncUF64 -> "trunc_f64_u"
     | TruncSatSF32 -> "trunc_sat_f32_s"
     | TruncSatUF32 -> "trunc_sat_f32_u"
     | TruncSatSF64 -> "trunc_sat_f64_s"
     | TruncSatUF64 -> "trunc_sat_f64_u"
     | ReinterpretFloat -> Printf.sprintf "reinterpret_f%d" (Type.size t.typ)
     | ConvertSI32 -> "convert_i32_s"
     | ConvertUI32 -> "convert_i32_u"
     | ConvertSI64 -> "convert_i64_s"
     | ConvertUI64 -> "convert_i64_u"
     | PromoteF32 -> "promote_f32"
     | DemoteF64 -> "demote_f64"
     | ReinterpretInt -> Printf.sprintf "reinterpret_i%d" (Type.size t.typ))

let to_string (t : t) : string = to_mnemonic t
