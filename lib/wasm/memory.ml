type memory_type = Limits.t
[@@deriving sexp, compare, equal]

let memory_type_of_wasm (mtype : Wasm.Types.memory_type) : memory_type = match mtype with
  | MemoryType limits -> Limits.of_wasm limits

type t = {
  mtype : memory_type
}
[@@deriving sexp, compare, equal]

let of_wasm (m : Wasm.Ast.memory) : t = {
  mtype = memory_type_of_wasm m.it.mtype
}
