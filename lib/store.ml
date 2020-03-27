open Core_kernel
open Wasm

module T = struct
  type t = {
    funcs : Func_inst.t list;
    globals : Global_inst.t list;
    mems : Memory_inst.t list;
    (* XXX: other fields *)
  }
  [@@deriving sexp, compare, yojson]
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
  assert (s1.funcs = s2.funcs);
  { s1 with
    globals = List.map2_exn s1.globals s2.globals ~f:Global_inst.join
  }
let init (m : Ast.module_) : t =
  let minst = Module_inst.init m in
  ({
    funcs = List.mapi m.it.funcs ~f:(Func_inst.of_wasm m minst);
    globals = List.map m.it.globals ~f:Global_inst.of_wasm;
    mems = List.map m.it.memories ~f:Memory_inst.of_wasm;
  })
