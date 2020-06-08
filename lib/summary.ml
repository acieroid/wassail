(* A function summary *)
open Core_kernel
open Helpers

(** A summary is the final state of the function *)
type t = Domain.state

let to_string (s : t) : string =
  Printf.sprintf "state: %s\nenv: %s\n more: %s\n"
    (Domain.to_string s)
    (String.concat ~sep:"," (List.map (Array.to_list (fst (Apron.Environment.vars s.env))) ~f:Apron.Var.to_string))
    (try
       (Domain.constraints_to_string
          (Apron.Abstract1.rename_array Domain.manager
             ((* Only keeps the return values and parameters *)
               (Apron.Abstract1.forget_array Domain.manager s.constraints
                  (Array.filter (fst (Apron.Environment.vars s.env))
                     (* TODO: don't use fixed names *)
                     ~f:(fun v -> Stdlib.(Apron.Var.to_string v <> "f0_ret" && Apron.Var.to_string v <> "f0_p0" ))) false))
             [| Apron.Var.of_string "f0_ret" ; Apron.Var.of_string "f0_p0" |] [| Apron.Var.of_string "ret" ; Apron.Var.of_string "arg" |]))
     with
     | Apron.Manager.Error { exn; funid; msg } ->
       failwith (Printf.sprintf "Apron error in add_constraint: exc: %s, funid: %s, msg: %s" (Apron.Manager.string_of_exc exn) (Apron.Manager.string_of_funid funid) msg))


(** Constructs a summary given a CFG and a domain state resulting from that CFG *)
let make (_cfg : Cfg.t) (state : Domain.state) : t = state

(** Constructs an empty bottom summary given a CFG *)
let bottom (cfg : Cfg.t) (module_ : Wasm_module.t) : t = Domain.bottom cfg (module_.nglobals) cfg.vars

(** Constructs a summary from an imported function *)
let of_import (_name : string) (_args : Type.t list) (_ret : Type.t list) (_module_ : Wasm_module.t) : t = failwith "TODO"
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
        | (idx, name, (args, ret)) -> IntMap.set sum ~key:idx ~data:(of_import name args ret module_))

(* Apply the summary to a state, updating the vstack as if the function was
   called, AND updating the set of called functions *)
let apply (_sum : t) (_fidx : Var.t) (_st : Domain.state) (_module_ : Wasm_module.t) : Domain.state = failwith "TODO: use Abstract1.substitute?"
    (*
  Printf.printf "[fidx: %d] Applying summary %s to state %s!" fidx (to_string sum) (Domain.to_string st);
  let map = List.foldi st.globals ~init:(List.foldi (List.rev st.vstack) ~init:Value.ValueValueMap.ValueMap.empty
                                           ~f:(fun i acc v -> Value.ValueValueMap.ValueMap.add_exn acc
                                                  ~key:(Value.parameter I32 i).value
                                                  ~data:v.value))
      ~f:(fun i acc v -> Value.ValueValueMap.ValueMap.add_exn acc
             ~key:(Value.global (List.nth_exn module_.globals i).typ i).value
             ~data:v.value) in
  { st with
    vstack = sum.result @ (List.map (List.drop st.vstack sum.nargs) ~f:(fun v -> Value.adapt v map));
    memory = Memory.join st.memory (Memory.adapt sum.memory map);
    globals = Globals.adapt sum.globals map;
  }
*)
