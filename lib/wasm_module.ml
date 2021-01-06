open Core_kernel
open Helpers

module T = struct

  (** A WebAssembly module *)
  type t = {
    types : (Type.t list * Type.t list) list; (** The types declared in the module *)
    global_types : Type.t list; (** The types for the globals *)
    nglobals : int; (** The number of globals *)
    nimports : int; (** The number of function imported *)
    imported_funcs : (int * string * (Type.t list * Type.t list)) list; (** The description of the imported function: their id, name, and type *)
    exported_funcs : (int * string * (Type.t list * Type.t list)) list; (** The description of the exported functions: the id, name, and type *)
    funcs : Func_inst.t list; (** The functions defined in the module *)
    mems : Memory_inst.t list; (** The memories specification *)
    tables : Table_inst.t list; (** The tables specification *)
  }
  [@@deriving sexp, compare, equal]
end
include T

(** Get the function instance for the function with index fidx *)
let get_funcinst (m : t) (fidx : int) : Func_inst.t =
  List.nth_exn m.funcs (fidx-(List.length m.imported_funcs))

(** Get the memory instance for the memory with index idx *)
let get_meminst (m : t) (idx : int) : Memory_inst.t =
  List.nth_exn m.mems idx

(** Get the type with index tid *)
let get_type (m : t) (tid : int) : Type.t list * Type.t list =
  List.nth_exn m.types tid

(** Get the type of the function with index fidx *)
let get_func_type (m : t) (fidx : int) : Type.t list * Type.t list =
  (List.nth_exn m.funcs (fidx-m.nimports)).typ


(** Constructs a Wasm_module *)
let of_wasm (m : Wasm.Ast.module_) : t =
  let minst = Module_inst.of_wasm m in
  let imported_funcs = List.filter_mapi m.it.imports ~f:(fun idx import -> match import.it.idesc.it with
      | FuncImport v ->
        Some (idx, Wasm.Ast.string_of_name import.it.item_name,
              match (List.nth_exn m.it.types (Index.of_wasm v)).it with
              | Wasm.Types.FuncType (a, b) -> (List.map a ~f:Type.of_wasm,
                                               List.map b ~f:Type.of_wasm))
      | _ -> None) in
  let nimports = List.length imported_funcs in
  let nglobals = List.length m.it.globals in
  let funcs = List.mapi m.it.funcs ~f:(fun i f -> Func_inst.of_wasm m minst (i+nimports) f nglobals) in
  let exported_funcs = List.filter_map m.it.exports ~f:(fun export -> match export.it.edesc.it with
      | FuncExport v ->
        let idx = Int32.to_int_exn v.it in
        Some (idx, (Wasm.Ast.string_of_name export.it.name), (List.nth_exn funcs (idx-nimports)).typ)
      | _ -> None) in
  let memories = List.filter_map m.it.imports ~f:(fun import -> match import.it.idesc.it with
      | MemoryImport m -> Some (Memory_inst.of_wasm_type m)
      | _ -> None) @ (List.map m.it.memories ~f:Memory_inst.of_wasm) in
  ({
    nimports;
    imported_funcs;
    exported_funcs;
    funcs;
    global_types = List.map m.it.globals ~f:(fun g -> match g.it.gtype with
      | Wasm.Types.GlobalType (t, _) -> Type.of_wasm t);
    nglobals = nglobals;
    (*globals = List.map m.it.globals ~f:Global_inst.of_wasm; *)
    mems = memories;
    tables = List.map m.it.tables ~f:(fun t ->
        Table_inst.init
          (Table.of_wasm t)
          (List.map m.it.elems ~f:(Elem.of_wasm m)));
    types = List.map m.it.types ~f:(fun t -> match t.it with
        | Wasm.Types.FuncType (a, b) -> (List.map a ~f:Type.of_wasm,
                                         List.map b ~f:Type.of_wasm))
  })

let of_file (file : string) : t =
  apply_to_file file of_wasm

let of_string (string : string) : t =
  apply_to_string string of_wasm

