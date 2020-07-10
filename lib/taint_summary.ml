open Core_kernel
open Helpers

type t = {
  globals : Spec_inference.var list;
  args : Spec_inference.var list;
  ret : Spec_inference.var option;
  state : Taint_domain.t;
}

let to_string (s : t) : string = Taint_domain.to_string s.state

let bottom (nglobals : int) (nargs : int) (nret : int) : t =
  { globals = List.init nglobals ~f:(fun i -> Spec_inference.Global i);
    args = List.init nargs ~f:(fun i -> Spec_inference.Local i);
    ret = (match nret with
        | 0 -> None
        | 1 -> Some (Spec_inference.Var 0)
        | _ -> failwith "more than one return value");
    state = Taint_domain.bottom }

let top (nglobals : int) (nargs : int) (nret : int) : t =
  let globals = List.init nglobals ~f:(fun i -> Spec_inference.Global i) in
  let args = List.init nargs ~f:(fun i -> Spec_inference.Local i) in
  let ret = (match nret with
        | 0 -> None
        | 1 -> Some (Spec_inference.Var 0)
        | _ -> failwith "more than one return value") in
  { globals = globals;
    args = args;
    ret = ret;
    state = Taint_domain.top globals ret }

let of_import (_idx : int) (name : string) (nglobals : int) (args : Type.t list) (ret : Type.t list) : t =
  match name with
  | "fd_write" | "proc_exit" ->
    (* no taint transfer *)
    bottom nglobals (List.length args) (List.length ret)
  | _ ->
    Logging.info (Printf.sprintf "Imported function is not modelled: %s" name);
    top nglobals (List.length args) (List.length ret)

let initial_summaries (cfgs : Cfg.t IntMap.t) (module_ : Wasm_module.t) (typ : [`Bottom | `Top]) : t IntMap.t =
  List.fold_left module_.imported_funcs
    ~init:(IntMap.map cfgs ~f:(fun cfg ->
        (match typ with
         | `Bottom -> bottom
         | `Top -> top) module_.nglobals (List.length cfg.arg_types) (List.length cfg.return_types)))
    ~f:(fun summaries (idx, name, (args, ret)) ->
        IntMap.set summaries ~key:idx ~data:(of_import idx name module_.nglobals args ret))
