open Core_kernel

module T = struct
  type op = Clz | Ctz | Popcnt | ExtendS | Other
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
    | ExtendS _todo -> ExtendS
  in
  let of_op_f (op : Wasm.Ast.FloatOp.unop) : op = match op with
    | _ -> Other (* TODO *)
  in
  match u with
  | I32 op -> { typ = I32; op = of_op op }
  | I64 op -> { typ = I64; op = of_op op }
  | F32 op -> { typ = F32; op = of_op_f op }
  | F64 op -> { typ = F64; op = of_op_f op }

let to_string (u : t) : string =
  Printf.sprintf "%s.%s"
    (Type.to_string u.typ)
    (match u.op with
     | Clz -> "clz"
     | Ctz -> "ctz"
     | Popcnt -> "popcnt"
     | ExtendS -> "extend_s"
     | Other -> "todo")
