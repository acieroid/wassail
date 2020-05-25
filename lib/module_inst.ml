open Core_kernel
open Wasm

module T = struct
  type t = {
    funcaddrs: Address.t list;
    globaladdrs: Address.t list;
    memaddrs: Address.t list;
    (* Other fields are not represented because we don't need them. These are:
       - types: function types (this has already been validated so we can ignore it)
       - tableddrs: we don't support tables (yet)
       - exports: we don't take exports into account (yet) *)
  }
  [@@deriving sexp, compare]
end
include T

let of_wasm (m : Ast.module_) : t =
  let funcaddrs = List.mapi m.it.funcs ~f:(fun i _ -> i) in
  let globaladdrs = List.mapi m.it.globals ~f:(fun i _ -> i) in
  let memaddrs = List.mapi m.it.memories ~f:(fun i _ -> i) in
  { funcaddrs; globaladdrs; memaddrs }
