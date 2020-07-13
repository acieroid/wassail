open Core_kernel

module T = struct
  type t = {
    nimports : int;
    imported_funcs : (int * string * (Type.t list * Type.t list)) list;
    funcs : Func_inst.t list;
    global_types : Type.t list;
    nglobals : int;
    mems : Memory_inst.t list;
    tables : Table_inst.t list;
    types : (Type.t list * Type.t list) list;
    (* XXX: other fields *)
  }
  [@@deriving sexp, compare]
end
include T
let get_funcinst (m : t) (a : Address.t) : Func_inst.t =
  List.nth_exn m.funcs (a-(List.length m.imported_funcs))
(*let get_global (m : t) (a : Address.t) : Global_inst.t =
  List.nth_exn m.globals a *)
(*let set_global (m : t) (a : Address.t) (v : Value.t) : t =
  { m with globals = List.mapi m.globals ~f:(fun i g ->
        if i = a then { g with value = Value.join g.value v } else g) } *)
let get_meminst (m : t) (a : Address.t) : Memory_inst.t =
  List.nth_exn m.mems a
let join (s1 : t) (s2 : t) : t =
  assert Stdlib.(s1.funcs = s2.funcs);
  s1 (*with
         globals = List.map2_exn s1.globals s2.globals ~f:Global_inst.join *)
let of_wasm (m : Wasm.Ast.module_) : t =
  let minst = Module_inst.of_wasm m in
  let imported_funcs = List.filter_mapi m.it.imports ~f:(fun idx import -> match import.it.idesc.it with
        | FuncImport v ->
          Some (idx, Wasm.Ast.string_of_name import.it.item_name,
                match (List.nth_exn m.it.types (Var.of_wasm v)).it with
                | Wasm.Types.FuncType (a, b) -> (List.map a ~f:Type.of_wasm,
                                                 List.map b ~f:Type.of_wasm))
        | _ -> None) in
  let nimports = List.length imported_funcs in
  let nglobals = List.length m.it.globals in
  let memories = List.filter_map m.it.imports ~f:(fun import -> match import.it.idesc.it with
      | MemoryImport m -> Some (Memory_inst.of_wasm_type m)
      | _ -> None) @ (List.map m.it.memories ~f:Memory_inst.of_wasm) in
  ({
    nimports = nimports;
    imported_funcs = imported_funcs;
    funcs = List.mapi m.it.funcs ~f:(fun i f -> Func_inst.of_wasm m minst (i+nimports) f nglobals);
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

let get_type (m : t) (tid : int) : Type.t list * Type.t list =
  List.nth_exn m.types tid

let get_func_type (m : t) (fid : int) : Type.t list * Type.t list =
  (List.nth_exn m.funcs (fid-m.nimports)).typ
