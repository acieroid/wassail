open Core_kernel

type desc =
  | FuncExport of Int32.t
  | TableExport of Int32.t
  | MemoryExport of Int32.t
  | GlobalExport of Int32.t
[@@deriving sexp, compare, equal]

type t = {
  name : string;
  edesc : desc;
}
[@@deriving sexp, compare, equal]

let of_wasm (e : Wasm.Ast.export) : t = {
  name = Wasm.Utf8.encode e.it.name;
  edesc = match e.it.edesc.it with
    | FuncExport x -> FuncExport x.it
    | TableExport x -> TableExport x.it
    | MemoryExport x -> MemoryExport x.it
    | GlobalExport x -> GlobalExport x.it
}
