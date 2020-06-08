(* A function summary *)
open Core_kernel
open Helpers

(** A summary is the final state of the function, with some extra informations *)
type t = {
  in_arity : int;
  params_and_return : string list; (** The name of the parameters and (optional) return value *)
  state : Domain.state; (** The final state *)
}

let to_string (s : t) : string =
  Printf.sprintf "state: %s"
    (Domain.to_string s.state)

(** Constructs a summary given a CFG and a domain state resulting from that CFG *)
let make (cfg : Cfg.t) (state : Domain.state) : t =
  (* Filter the state to only keep relevant variables, i.e., the parameters and return value (if there is one) *)
  let params = List.mapi cfg.arg_types ~f:(fun argi _ -> Domain.arg_name cfg.idx argi) in
  let params_and_ret = (if List.length cfg.return_types = 1 then [Domain.return_name cfg.idx] else []) @ params in
  { params_and_return = params_and_ret;
    in_arity = List.length cfg.arg_types;
    state = Domain.keep_only state params_and_ret;
  }

(** Constructs an empty bottom summary given a CFG *)
let bottom (cfg : Cfg.t) (module_ : Wasm_module.t) : t =
  make cfg (Domain.bottom cfg module_.nglobals cfg.vars)

(** Constructs a summary from an imported function *)
let of_import (_idx : int) (_name : string) (_args : Type.t list) (_ret : Type.t list) (_module_ : Wasm_module.t) : t =
  (* These should be fairly easy to encode: we just list constraints between input and output, no constraint if we don't know anything about that name *)
  failwith "TODO: import summaries"
    (* {
  nargs = List.length args;
  typ = args, ret;
  result = begin match name with
    | "fd_write" ->
      (* fd_write always returns 0 *)
      [Value.zero Type.I32]
    | "proc_exit" ->
      (* proc_exit does not return anything *)
      []
    | _ ->
      (* We have not model this imported function, so we return top *)
      List.map ret ~f:(fun t -> Value.top t (Printf.sprintf "import %s" name))
  end;
  (* We assume (probably unsoundly) that imports don't change memory nor globals *)
  globals = List.mapi module_.globals ~f:(fun i g -> Value.global g.typ i);
  memory = Memory.initial
}
    *)

(** Constructs all summaries for a given module, including imported functions *)
let initial_summaries (cfgs : Cfg.t IntMap.t) (module_ : Wasm_module.t) : t IntMap.t =
  List.fold_left module_.imported_funcs
    (* Summaries for defined functions are all initialized to bottom *)
    ~init:(IntMap.map cfgs ~f:(fun cfg -> bottom cfg module_))
    ~f:(fun sum import -> match import with
        | (idx, name, (args, ret)) -> IntMap.set sum ~key:idx ~data:(of_import idx name args ret module_))

(* Apply the summary to a state, updating the vstack as if the function was
   called, AND updating the set of called functions *)
let apply (summary : t) (_fidx : Var.t) (state : Domain.state) (ret : string option) (_module_ : Wasm_module.t) : Domain.state =
  let retl = (match ret with
      | Some v -> [v]
      | None -> []) in
  (* A summary encodes the relation between the arguments and return value.
     To apply it, we do the following: *)
  let args_and_ret = List.take state.vstack summary.in_arity @ retl in
  (* 1. Rename the parameters and return value in the summary to match the actual arguments and return value *)
  (* TODO: move that code to domain.ml in  a nice function *)
  let renamed = Apron.Abstract1.rename_array Domain.manager summary.state.constraints
      (Array.of_list (List.map summary.params_and_return ~f:Apron.Var.of_string))
      (Array.of_list (List.map args_and_ret ~f:Apron.Var.of_string)) in
  (* 2. Change the environment of the constraints to match the current environment in which we apply the summary.
        Apron's doc says: "variables that are introduced are unconstrained"*)
  let changed = Apron.Abstract1.change_environment Domain.manager renamed state.env false in
  (* 3. Finally, meet the summary with the current state *)
  let final = Apron.Abstract1.meet Domain.manager state.constraints changed in
  { state with vstack = retl @ (List.drop state.vstack summary.in_arity);
               (* TODO: memory *)
               (* TODO: globals CAN change (but not often). At least make sure to fail when they do *)
               constraints = final }

