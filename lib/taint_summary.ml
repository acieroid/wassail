open Core_kernel
open Helpers

type t = {
  args : Var.t list;
  globals : Var.t list; (** globals after the function execution *)
  ret : Var.t option;
  mem : Var.t list;
  state : Taint_domain.t;
  (* TODO: memory *)
}

type state = Taint_domain.t

let to_string (s : t) : string = Taint_domain.to_string s.state

let bottom (cfg : Cfg.t) (_vars : Var.t list) : t =
  let globals = List.mapi cfg.global_types ~f:(fun i _ -> Var.Var i) in
  let args = List.mapi cfg.arg_types ~f:(fun i _ -> Var.Local i) in
  let ret = (match cfg.return_types with
      | [] -> None
      | _ :: [] -> Some (Var.Var (List.length globals))
      | _ -> failwith "more than one return value") in
  { globals; args; ret;
    mem = [];
    state = Taint_domain.bottom }

let top (cfg : Cfg.t) (_vars : Var.t list) : t =
  let globals = List.mapi cfg.global_types ~f:(fun i _ -> Var.Var i) in
  let args = List.mapi cfg.arg_types ~f:(fun i _ -> Var.Local i) in
  let ret = (match cfg.return_types with
      | [] -> None
      | _ :: [] -> Some (Var.Var (List.length globals))
      | _ -> failwith "more than one return value") in
  { globals; args; ret;
    mem = []; (* TODO: mem *)
    state = Taint_domain.top globals ret }

let of_import (_idx : int) (name : string) (nglobals : int) (args : Type.t list) (ret : Type.t list) : t =
  let globals = List.init nglobals ~f:(fun i -> Var.Var i) in
  let args = List.mapi args ~f:(fun i _ -> Var.Local i) in
  let ret = match ret with
    | [] -> None
    | _ :: [] -> Some (Var.Var (nglobals+1))
    | _ -> failwith "more than one return value" in
  { globals; args; ret;
    mem = [];
    state = match name with
      | "fd_write" | "proc_exit" ->
        Taint_domain.bottom_with_keys (globals @ args @ (Option.to_list ret))
      | _ ->
        Logging.info (Printf.sprintf "Imported function is not modelled: %s" name);
        Taint_domain.top globals ret }

let initial_summaries (cfgs : Cfg.t IntMap.t) (module_ : Wasm_module.t) (typ : [`Bottom | `Top]) : t IntMap.t =
  List.fold_left module_.imported_funcs
    ~init:(IntMap.map cfgs ~f:(fun cfg ->
        (match typ with
         | `Bottom -> bottom
         | `Top -> top) cfg []))
    ~f:(fun summaries (idx, name, (args, ret)) ->
        IntMap.set summaries ~key:idx ~data:(of_import idx name module_.nglobals args ret))

let make (cfg : Cfg.t) (state : Taint_domain.t)
    (ret : Var.t option) (globals_post : Var.t list)
    (mem_post : Var.t list)
  : t =
  { globals = globals_post;
    args = List.init (List.length cfg.arg_types) ~f:(fun i -> Var.Local i);
    ret = ret;
    mem = mem_post;
    state = Taint_domain.restrict state (globals_post @ mem_post @ (Option.to_list ret)) }

let apply (summary : t) (state : Taint_domain.t) (args : Var.t list) (globals : Var.t list) (ret : Var.t option) : Taint_domain.t =
  (* To apply a summary, we first rename the return value and the globals:
     if summary.state is for example [v0 : l0][v1: l1] where v0 is the return value and v1 is a global,
     and the variable for the return value (ret) and global (globals) are x0 and x1, then we obtain:
     [x0: l0][x1: l1]
    *)
  let with_ret = match summary.ret, ret with
    | Some r, Some r' -> Taint_domain.rename_key summary.state r r'
    | None, None -> summary.state
    | _ -> failwith "incompatible return value for summary"
  in
  let with_globals = List.fold_left (List.map2_exn summary.globals globals ~f:(fun x y -> (x, y)))
      ~init:with_ret
      ~f:(fun acc (g, g') -> Taint_domain.rename_key acc g g') in
  (* Then we update the argument values.
     If the arguments are a0 and a1, then we replace l0 by state[a0] and l1 by state[a1] *)
  let with_args = List.fold_left (List.map2_exn summary.args args ~f:(fun x y -> (x, y)))
      ~init:with_globals
      ~f:(fun acc (a, a') -> Taint_domain.replace_taint acc a (Taint_domain.get_taint state a')) in
  (* TODO: apply mem! *)
  with_args
  
