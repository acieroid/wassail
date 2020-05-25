open Core_kernel

module T = struct
  type t = {
    funcs : Func_inst.t list;
    globals : Global_inst.t list;
    mems : Memory_inst.t list;
    tables : Table_inst.t list;
    types : (Type.t list * Type.t list) list;
    (* XXX: other fields *)
  }
  [@@deriving sexp, compare]
end
include T
let get_funcinst (s : t) (a : Address.t) : Func_inst.t =
  List.nth_exn s.funcs a
let get_global (s : t) (a : Address.t) : Global_inst.t =
  List.nth_exn s.globals a
let set_global (s : t) (a : Address.t) (v : Value.t) : t =
  { s with globals = List.mapi s.globals ~f:(fun i g ->
        if i = a then { g with value = Value.join g.value v } else g) }
let get_meminst (s : t) (a : Address.t) : Memory_inst.t =
  List.nth_exn s.mems a
let join (s1 : t) (s2 : t) : t =
  assert Stdlib.(s1.funcs = s2.funcs);
  { s1 with
    globals = List.map2_exn s1.globals s2.globals ~f:Global_inst.join
  }
let of_wasm (m : Wasm.Ast.module_) : t =
  let minst = Module_inst.of_wasm m in
  ({
    funcs = List.mapi m.it.funcs ~f:(Func_inst.of_wasm m minst);
    globals = List.map m.it.globals ~f:Global_inst.of_wasm;
    mems = List.map m.it.memories ~f:Memory_inst.of_wasm;
    tables = List.map m.it.tables ~f:(fun t ->
        Table_inst.init
          (Table.of_wasm t)
          (List.map m.it.elems ~f:Elem.of_wasm));
    types = List.map m.it.types ~f:(fun t -> match t.it with
        | Wasm.Types.FuncType (a, b) -> (List.map a ~f:Type.of_wasm,
                              List.map b ~f:Type.of_wasm))
  })
