open Core_kernel
open Helpers

module Domain = Relational_domain

type state = Domain.t

(** A summary is the final state of the function, with some extra informations *)
type t = {
  in_arity : int; (** The input arity for the function of this summary *)
  params : string list; (** The name of the parameters  *)
  return : string option; (** The name of the return value *)
  mem_pre : string list;
  mem_post : string list;
  globals_pre : string list;
  globals_post : string list;
  state : state; (** The final state *)
}

let to_string (s : t) : string =
  Printf.sprintf "pre: mem: [%s]\npost: ret: %s, globals: [%s], mem: [%s]\nstate: %s"
    (String.concat ~sep:"," s.mem_pre) (* TODO: should maybe be a map? *)
    (Option.value ~default:"none" s.return)
    (String.concat ~sep:"," s.globals_post)
    (String.concat ~sep:"," s.mem_post) (* TODO: should maybe be a map? *)
    (Relational_domain.to_string s.state)

(** Constructs a summary.
    @param cfg the CFG for which the summary approximates the behavior
    @param ret the return value of the function (if there is any)
    @param state the final state of the summarized function *)
let make (cfg : Cfg.t) (state : Domain.t) (ret : string option)
    (mem_pre : string list) (mem_post : string list)
    (globals_post : string list)
  : t =
  (* Filter the state to only keep relevant variables:
      - the parameters
      - the return value if there is one (i.e., the top of the stack)
      - any variable bound in the store
      - any variable used by a global *)
  let params = List.mapi cfg.arg_types ~f:(fun argi _ -> Var.to_string (Var.Local argi)) in
  let globals_pre = List.mapi cfg.global_types ~f:(fun i _ -> Var.to_string (Var.Global i)) in
  let to_keep = params @ globals_pre @ globals_post @ mem_pre @ mem_post @ (Option.to_list ret) in
  { params = params;
    return = ret;
    mem_pre = mem_pre;
    mem_post = mem_post;
    globals_pre = globals_pre;
    globals_post = globals_post;
    in_arity = List.length cfg.arg_types;
    state = Domain.keep_only state to_keep;
    }

(** Constructs an empty bottom summary given a CFG *)
let bottom (cfg : Cfg.t) (vars : Var.t list) : t =
  let ret = if List.length cfg.return_types = 1 then Some "ret" else None in
  let params = List.mapi cfg.arg_types ~f:(fun argi _ -> Var.to_string (Var.Local argi)) in
  make cfg (Domain.bottom cfg ((Option.to_list ret) @ params @ (List.map ~f:Var.to_string vars))) ret [] [] []

(** Constructs the top summary given a CFG *)
let top (cfg : Cfg.t) (vars : Var.t list) : t =
  let ret = if List.length cfg.return_types = 1 then Some "ret" else None in
  let params = List.mapi cfg.arg_types ~f:(fun argi _ -> Var.to_string (Var.Local argi)) in
  (* TODO: top memory and globals? *)
  make cfg (Domain.top cfg ((Option.to_list ret) @ params @ (List.map ~f:Var.to_string vars))) ret [] [] []

(** Constructs a summary from an imported function *)
let of_import (_idx : int) (name : string) (args : Type.t list) (ret : Type.t list) : t =
  (* These should be fairly easy to encode: we just list constraints between input and output, no constraint if we don't know anything about that name *)
  let params = List.mapi args ~f:(fun argi _ -> Var.to_string (Var.Local argi)) in
  assert (List.length ret <= 1); (* wasm spec does not allow for more than one return type (currently) *)
  Printf.printf "[summary %d]: ret is %d\n" _idx (List.length ret);
  let return = match ret with
    | [] -> None
    | _ :: [] -> Some "ret"
    | _ -> failwith (Printf.sprintf "more than one return value for %s" name) in
  let params_and_return = params @ (Option.to_list return) in
  let apron_vars = Array.of_list (List.map params_and_return ~f:Apron.Var.of_string) in
  let apron_env = Apron.Environment.make apron_vars [| |] in (* only int variables *)
  (* Everything is set to top *)
  let constraints = Apron.Abstract1.of_box Domain.manager apron_env apron_vars
      (Array.of_list (List.map params_and_return ~f:(fun _ -> Apron.Interval.top))) in
  let topstate = Domain.{ env = apron_env; constraints = constraints } in
  let state = match name with
    | "fd_write" ->
      (* returns 0 *)
      Domain.add_constraint topstate "ret" "0"
    | "proc_exit" ->
      (* does not return anything, hence no constraint *)
      topstate
    | _ ->
      (* We have not modelled this imported function, so we don't have any constraints *)
      Logging.info (Printf.sprintf "Imported function is not modelled: %s" name);
      topstate
  in
  { params = params;
    return = return;
    mem_pre = [];
    mem_post = [];
    globals_pre = [];
    globals_post = [];
    in_arity = List.length args;
    state = state }

(** Apply the summary to a state, updating the vstack as if the function was
   called, AND updating the set of called functions *)
let apply (summary : t) (state : Domain.t) (args: string list) (ret : string option) : Domain.t =
  try
    Printf.printf "Applying summary %s\n" (to_string summary);
    (* A summary encodes the relation between the arguments and return value.
       To apply it, we do the following: *)
    let args_and_ret = args @ (Option.to_list ret) in
    (* 1. Rename the parameters and return value in the summary to match the actual arguments and return value *)
    let rename_from = List.map (summary.params @ (Option.to_list summary.return)) ~f:Apron.Var.of_string in
    let rename_to = List.map args_and_ret ~f:Apron.Var.of_string in
    Printf.printf "vars: %s\n" (String.concat ~sep:","
                                  (List.map (Array.to_list (fst (Apron.Environment.vars summary.state.env)))
                                  ~f:Apron.Var.to_string));
    Printf.printf "renaming in state %s\nFrom %s to %s\n" (Domain.to_string summary.state) (String.concat ~sep:"," (List.map rename_from ~f:Apron.Var.to_string )) (String.concat ~sep:"," (List.map rename_to ~f:Apron.Var.to_string));
    let renamed = Apron.Abstract1.rename_array Domain.manager summary.state.constraints
        (Array.of_list rename_from)
        (Array.of_list rename_to) in
    (* 2. Change the environment of the constraints to match the current environment in which we apply the summary.
        Apron's doc says: "variables that are introduced are unconstrained"*)
    let changed = Apron.Abstract1.change_environment Domain.manager renamed state.env false in
    (* 3. Finally, meet the summary with the current state *)
    let final = Apron.Abstract1.meet Domain.manager state.constraints changed in
    (* TODO: memory and globals *)
    { state with constraints = final }
  with
  | Apron.Manager.Error { exn; funid; msg } ->
    failwith (Printf.sprintf "Apron error in Summary.apply: exc: %s, funid: %s, msg: %s" (Apron.Manager.string_of_exc exn) (Apron.Manager.string_of_funid funid) msg)

(** Constructs all summaries for a given module, including imported functions *)
let initial_summaries (cfgs : Cfg.t IntMap.t) (module_ : Wasm_module.t) (typ : [`Bottom | `Top]) : t IntMap.t =
  List.fold_left module_.imported_funcs
    (* Summaries for defined functions are all initialized to bottom (or top) *)
    ~init:(IntMap.map cfgs ~f:(fun cfg ->
        (match typ with
         | `Bottom -> bottom
         | `Top -> top) cfg [] (* TODO: vars *)))
    ~f:(fun summaries (idx, name, (args, ret)) ->
        IntMap.set summaries ~key:idx ~data:(of_import idx name args ret))

