open Core_kernel

type desc =
  | FuncImport of Int32.t
  | TableImport of Limits.t
  | MemoryImport of Limits.t
  | GlobalImport of Global.global_type
[@@deriving sexp, compare, equal]

type t = {
  module_name: string;
  item_name: string;
  idesc: desc;
}
[@@deriving sexp, compare, equal]

let of_wasm (im : Wasm.Ast.import) : t = {
  module_name = Wasm.Utf8.encode im.it.module_name;
  item_name = Wasm.Utf8.encode im.it.item_name;
  idesc = match im.it.idesc.it with
    | FuncImport x -> FuncImport x.it
    | TableImport tt -> TableImport (Table.table_type_of_wasm tt)
    | MemoryImport mt -> MemoryImport (Memory.memory_type_of_wasm mt)
    | GlobalImport gt -> GlobalImport (Global.global_type_of_wasm gt)
}
