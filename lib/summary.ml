(* A function summary *)
open Core_kernel
open Helpers

(** A summary is the final state of the function, with some extra informations *)
type t = {
  in_arity : int; (** The input arity for the function of this summary *)
  params : string list; (** The name of the parameters  *)
  return : string option; (** The name of the return value *)
  state : Domain.state; (** The final state *)
}

let to_string (s : t) : string =
  Printf.sprintf "ret: %s, %s"
    (Option.value ~default:"none" s.return)
    (Domain.to_string s.state)

(** Constructs a summary.
    @param cfg the CFG for which the summary approximates the behavior
    @param ret the return value of the function (if there is any)
    @param state the final state of the summarized function *)
let make (cfg : Cfg.t) (state : Domain.state) (ret : string option) : t =
  (* Filter the state to only keep relevant variables:
      - the parameters
      - the return value if there is one (i.e., the top of the stack)
      - any variable bound in the store (TODO)
     - any variable used by a global (TODO) *)
  let params = List.mapi cfg.arg_types ~f:(fun argi _ -> Spec_inference.var_to_string (Spec_inference.Local argi)) in
  (* TODO: globals and memories *)
  let to_keep = params @ (Option.to_list ret) in
  { params = params;
    return = ret;
    in_arity = List.length cfg.arg_types;
    state = Domain.keep_only state to_keep;
    }

(** Constructs an empty bottom summary given a CFG *)
let bottom (cfg : Cfg.t) (_module_ : Wasm_module.t) (vars : Spec_inference.var list) : t =
  make cfg (Domain.bottom cfg vars) (if List.length cfg.return_types = 1 then Some "ret" else None)

(** Constructs a summary from an imported function *)
let of_import (_idx : int) (name : string) (args : Type.t list) (ret : Type.t list) (_module_ : Wasm_module.t) : t =
  (* These should be fairly easy to encode: we just list constraints between input and output, no constraint if we don't know anything about that name *)
  let params = List.mapi args ~f:(fun argi _ -> Spec_inference.var_to_string (Spec_inference.Local argi)) in
  assert (List.length ret <= 1); (* wasm spec does not allow for more than one return type (currently) *)
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
    in_arity = List.length args;
    state = state }

(** Constructs all summaries for a given module, including imported functions *)
let initial_summaries (cfgs : Cfg.t IntMap.t) (module_ : Wasm_module.t) : t IntMap.t =
  List.fold_left module_.imported_funcs
    (* Summaries for defined functions are all initialized to bottom *)
    ~init:(IntMap.map cfgs ~f:(fun cfg -> bottom cfg module_ [] (* TODO: vars *)))
    ~f:(fun sum import -> match import with
        | (idx, name, (args, ret)) -> IntMap.set sum ~key:idx ~data:(of_import idx name args ret module_))

(* Apply the summary to a state, updating the vstack as if the function was
   called, AND updating the set of called functions *)
let apply (summary : t) (state : Domain.state) (args: string list) (ret : string option) : Domain.state =
  try
    (* A summary encodes the relation between the arguments and return value.
       To apply it, we do the following: *)
    let args_and_ret = args @ (Option.to_list ret) in
    (* 1. Rename the parameters and return value in the summary to match the actual arguments and return value *)
    let rename_from = List.map (summary.params @ (Option.to_list summary.return)) ~f:Apron.Var.of_string in
    let rename_to = List.map args_and_ret ~f:Apron.Var.of_string in
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
