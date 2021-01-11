open Core_kernel
open Helpers

module T = struct
  (** A WebAssembly module *)
  type t = {
    start : Int32.t option;
    types : (Type.t list * Type.t list) list; (** The types declared in the module *)
    globals : Global.t list; (** The globals *)
    global_types : Type.t list; (** The types for the globals *)
    nglobals : int; (** The number of globals *)
    nimports : int; (** The number of function imported *)
    imports : Import.t list;
    imported_funcs : (int * string * (Type.t list * Type.t list)) list; (** The description of the imported function: their id, name, and type *)
    exports : Export.t list;
    exported_funcs : (int * string * (Type.t list * Type.t list)) list; (** The description of the exported functions: the id, name, and type *)
    funcs : Func_inst.t list; (** The functions defined in the module *)
    memories : Memory.t list; (** The memory types *)
    memory_insts : Memory_inst.t list; (** The memory instances *)
    tables : Table.t list; (** The table types *)
    table_insts : Table_inst.t list; (** The table instances *)
    data : Segment.DataSegment.t list; (** The data segment containing initial data *)
    elems : Segment.ElemSegment.t list; (** The elem segment *)
  }
  [@@deriving sexp, compare, equal]
end
include T

(** Get the function instance for the function with index fidx *)
let get_funcinst (m : t) (fidx : int) : Func_inst.t =
  List.nth_exn m.funcs (fidx-(List.length m.imported_funcs))

(** Get the memory instance for the memory with index idx *)
let get_meminst (m : t) (idx : int) : Memory_inst.t =
  List.nth_exn m.memory_insts idx

(** Get the type with index tid *)
let get_type (m : t) (tid : Int32.t) : Type.t list * Type.t list =
  List.nth_exn m.types (Int32.to_int_exn tid)

(** Get the type of the function with index fidx *)
let get_func_type (m : t) (fidx : int) : Type.t list * Type.t list =
  (List.nth_exn m.funcs (fidx-m.nimports)).typ

(** Constructs a Wasm_module *)
let of_wasm (m : Wasm.Ast.module_) : t =
  let minst = Module_inst.of_wasm m in
  let imported_funcs = List.filter_mapi m.it.imports ~f:(fun idx import -> match import.it.idesc.it with
      | FuncImport v ->
        Some (idx, Wasm.Ast.string_of_name import.it.item_name,
              let type_idx = Index.of_wasm v in
              match (List.nth m.it.types type_idx) with
              | Some ({it = Wasm.Types.FuncType (a, b); _}) ->
                      (List.map a ~f:Type.of_wasm, List.map b ~f:Type.of_wasm)
              | None -> failwith (Printf.sprintf "Wasm_module.of_wasm: type %d not found" type_idx))
      | _ -> None) in
  let nimports = List.length imported_funcs in
  let globals = List.map m.it.globals ~f:(Global.of_wasm m) in
  let nglobals = List.length m.it.globals in
  let global_types = List.map m.it.globals ~f:(fun g -> match g.it.gtype with
      | Wasm.Types.GlobalType (t, _) -> Type.of_wasm t) in
  let funcs = List.mapi m.it.funcs ~f:(fun i f -> Func_inst.of_wasm m minst (i+nimports) f nglobals) in
  let ftype (fidx : int) : Type.t list * Type.t list = if fidx < nimports then
      match List.nth imported_funcs fidx with
      | Some (_, _, typ) -> typ
      | None -> failwith (Printf.sprintf "Wasm_module.of_wasm: import %d not found" fidx)
    else
      match List.nth funcs (fidx-nimports) with
      | Some f ->
        assert (f.idx = fidx);
        f.typ
      | None -> failwith (Printf.sprintf "Wasm_module.of_wasm: function %d not found" fidx) in
  let exports = List.map m.it.exports ~f:Export.of_wasm in
  let exported_funcs = List.filter_map m.it.exports ~f:(fun export -> match export.it.edesc.it with
      | FuncExport v ->
        let idx = Int32.to_int_exn v.it in
        Some (idx, (Wasm.Ast.string_of_name export.it.name), ftype idx)
      | _ -> None) in
  let memories = List.map m.it.memories ~f:Memory.of_wasm in
  let memory_insts = List.filter_map m.it.imports ~f:(fun import -> match import.it.idesc.it with
      | MemoryImport m -> Some (Memory_inst.of_wasm_type m)
      | _ -> None) @ (List.map m.it.memories ~f:Memory_inst.of_wasm) in
  let data = List.map m.it.data ~f:(Segment.DataSegment.of_wasm m) in
  let imports = List.map m.it.imports ~f:Import.of_wasm in
  let tables = List.map m.it.tables ~f:Table.of_wasm in
  let table_insts = List.map m.it.tables ~f:(fun t ->
        Table_inst.init
          (Table.of_wasm t)
          (List.map m.it.elems ~f:(Elem.of_wasm m))) in
  let start = Option.map m.it.start ~f:(fun v -> v.it) in
  let elems = List.map m.it.elems ~f:(Segment.ElemSegment.of_wasm m) in
  ({
    start;
    imports; nimports; imported_funcs;
    exports; exported_funcs;
    funcs;
    data; elems;
    global_types; nglobals; globals;
    memories; memory_insts;
    tables; table_insts;
    types = List.map m.it.types ~f:(fun t -> match t.it with
        | Wasm.Types.FuncType (a, b) -> (List.map a ~f:Type.of_wasm,
                                         List.map b ~f:Type.of_wasm))
  })

let of_file (file : string) : t =
  apply_to_file file of_wasm

let of_string (string : string) : t =
  apply_to_string string of_wasm

