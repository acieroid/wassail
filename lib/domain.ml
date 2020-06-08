open Core_kernel

let manager = Oct.manager_alloc ()

(** A state of the wasm VM *)
type state = {
  constraints : Oct.t Apron.Abstract1.t; (** The relational constraints *)
  env : Apron.Environment.t; (** The relational environment *)
  vstack : Vstack.t; (** The value stack *)
  locals : Locals.t; (** The locals *)
  globals : Globals.t; (** The globals *)
  memory : Memory.t; (** The linear memory *)
}

let bind_compare (res : int) (cmp : 'a -> 'a -> int) (x : 'a) (y : 'a) : int =
  if res = 0 then 0 else cmp x y
let compare_state (s1 : state) (s2 : state) : int =
  bind_compare
    (bind_compare
       (bind_compare
          (bind_compare
             (bind_compare (Vstack.compare s1.vstack s2.vstack)
                Locals.compare s1.locals s2.locals)
             Globals.compare s1.globals s2.globals)
          Memory.compare s1.memory s2.memory)
       Apron.Environment.compare s1.env s2.env)
    (fun c1 c2 ->
       if Apron.Abstract1.is_eq manager c1 c2 then 0
       else if Apron.Abstract1.is_leq manager c1 c2 then -1
       else 1) s1.constraints s2.constraints

let to_string (s : state) : string =
  Printf.sprintf "{vstack: [%s],\n locals: [%s],\n globals: [%s],\n heap: %s\n}"
    (Vstack.to_string s.vstack)
    (Locals.to_string s.locals)
    (Globals.to_string s.globals)
    (Memory.to_string s.memory)

(** Initializes the state for the analysis of CFG cfg.
    Introduces a few extra variables, e.g., f-1-p0 for the first argument of function
    @param cfg is the CFG under analysis
    @param nglobals is the number of globals in the wasm program analyzed
 *)
let init (cfg : Cfg.t) (nglobals : int) : state =
  let locals = List.mapi cfg.arg_types ~f:(fun argi _typ -> (Printf.sprintf "f%d-p%d" cfg.idx argi, Apron.Interval.top)) @
               List.mapi cfg.local_types ~f:(fun locali _typ -> (Printf.sprintf "f%d-l%d" cfg.idx locali, Apron.Interval.of_int 0 0)) in
  let globals = List.init nglobals ~f:(fun globali -> (Printf.sprintf "f%d-g%d" cfg.idx globali, Apron.Interval.top)) in
  let vars_and_vals = locals @ globals @ (List.map cfg.vars ~f:(fun v -> (v, Apron.Interval.top))) in
  let apron_vars = Array.of_list (List.map vars_and_vals  ~f:(fun (var, _) -> Apron.Var.of_string var)) in
  let apron_env = Apron.Environment.make apron_vars [| |] in
  let apron_abs = Apron.Abstract1.of_box manager apron_env apron_vars (Array.of_list (List.map vars_and_vals ~f:(fun (_, v) -> v))) in
  {
    vstack = [];
    locals = List.map locals ~f:fst;
    globals = List.map globals ~f:fst;
    memory = Memory.empty;
    constraints = apron_abs;
    env = apron_env;
  }

(** Creates the bottom value *)
let bottom (cfg : Cfg.t) (nglobals : int) (vars : string list) : state =
  let locals = List.mapi cfg.arg_types ~f:(fun argi _typ -> (Printf.sprintf "f%d-p%d" cfg.idx argi, Apron.Interval.bottom)) @
               List.mapi cfg.local_types ~f:(fun locali _typ -> (Printf.sprintf "f%d-l%d" cfg.idx locali, Apron.Interval.bottom)) in
  let globals = List.init nglobals ~f:(fun globali -> (Printf.sprintf "f%d-g%d" cfg.idx globali, Apron.Interval.bottom)) in
  let vars_and_vals = locals @ globals @ (List.map vars ~f:(fun v -> (v, Apron.Interval.bottom))) in
  let apron_vars = Array.of_list (List.map vars_and_vals  ~f:(fun (var, _) -> Apron.Var.of_string var)) in
  let apron_env = Apron.Environment.make apron_vars [| |] in
  let apron_abs = Apron.Abstract1.of_box manager apron_env apron_vars (Array.of_list (List.map vars_and_vals ~f:(fun (_, v) -> v))) in
  {
    vstack = [];
    locals = List.map locals ~f:fst;
    globals = List.map globals ~f:fst;
    memory = Memory.empty;
    constraints = apron_abs;
    env = apron_env;
  }

let join (s1 : state) (s2 : state) : state = {
  constraints = Apron.Abstract1.join manager s1.constraints s2.constraints;
  vstack = Vstack.join s1.vstack s2.vstack;
  locals = List.map2_exn s1.locals s2.locals ~f:Value.join;
  globals = Globals.join s1.globals s2.globals;
  memory = Memory.join s1.memory s2.memory;
  env = (assert Stdlib.(s1.env = s2.env); s1.env);
}

let add_constraint (s : state) (v : string) (linexpr : string) : state =
  { s with constraints = Apron.Abstract1.assign_linexpr manager s.constraints (Apron.Var.of_string v) (Apron.Parser.linexpr1_of_string s.env linexpr) None }
