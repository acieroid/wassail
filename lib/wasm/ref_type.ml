type t = | FuncRefType | ExternRefType
[@@deriving sexp, compare, equal]

let of_wasm (t : Wasm.Types.ref_type) : t =
  match t with
  | FuncRefType -> FuncRefType
  | ExternRefType -> ExternRefType

let to_string (t : t) : string =
  match t with
  | FuncRefType -> "funcref"
  | ExternRefType -> "externref"
