open Core_kernel

type apron_domain = Polka.loose Polka.t
let manager = Polka.manager_alloc_loose ()

(** A state of the wasm VM *)
type state = {
  constraints : apron_domain Apron.Abstract1.t; (** The relational constraints *)
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

let constraints_to_string (c : apron_domain Apron.Abstract1.t) : string =
  (Apron.Abstract1.print Stdlib.Format.str_formatter c; Stdlib.Format.flush_str_formatter ())

let to_string (s : state) : string =
  Printf.sprintf "{\n vstack: [%s],\n memory: %s\n constraints: %s\n}"
    (Vstack.to_string s.vstack)
    (* (Locals.to_string s.locals)
       (Globals.to_string s.globals) *)
    (Memory.to_string s.memory)
    (constraints_to_string s.constraints)

(** Returns the name of the variable for a function's argument *)
let arg_name (fid : int) (argi : int) : string =
  Printf.sprintf "f%d_p%d" fid argi

(** Returns the name of the return variable for a function *)
let return_name (fid : int) : string =
  Printf.sprintf "f%d_ret" fid

(** Initializes the state for the analysis of CFG cfg.
    Introduces a few extra variables, e.g., f-1-p0 for the first argument of function
    @param cfg is the CFG under analysis
    @param nglobals is the number of globals in the wasm program analyzed
 *)
let init (cfg : Cfg.t) (nglobals : int) : state =
  let locals = List.mapi cfg.arg_types ~f:(fun argi _typ -> (arg_name cfg.idx argi, Apron.Interval.top)) @
               List.mapi cfg.local_types ~f:(fun locali _typ -> (Printf.sprintf "f%d_l%d" cfg.idx locali, Apron.Interval.of_int 0 0)) in
  let globals = List.init nglobals ~f:(fun globali -> (Printf.sprintf "f%d_g%d" cfg.idx globali, Apron.Interval.top)) in
  assert (List.length cfg.return_types <= 1); (* wasm spec does not allow for more than one return type (currently) *)
  let vars_and_vals = locals @ globals @ (List.map cfg.vars ~f:(fun v -> (v, Apron.Interval.top))) in
  let apron_vars = Array.of_list (List.map vars_and_vals  ~f:(fun (var, _) -> Apron.Var.of_string var)) in
  Printf.printf "make env with %s\n" (String.concat ~sep:"," (List.map vars_and_vals ~f:fst)) ;
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
  let locals = List.mapi cfg.arg_types ~f:(fun argi _typ -> (arg_name cfg.idx argi, Apron.Interval.bottom)) @
               List.mapi cfg.local_types ~f:(fun locali _typ -> (Printf.sprintf "f%d_l%d" cfg.idx locali, Apron.Interval.bottom)) in
  let globals = List.init nglobals ~f:(fun globali -> (Printf.sprintf "f%d_g%d" cfg.idx globali, Apron.Interval.bottom)) in
  let vars_and_vals = locals @ globals @ (List.map vars ~f:(fun v -> (v, Apron.Interval.bottom))) in
  let apron_vars = Array.of_list (List.map vars_and_vals  ~f:(fun (var, _) -> Apron.Var.of_string var)) in
  Printf.printf "vars: %s\n" (String.concat ~sep:"," (List.map ~f:fst vars_and_vals));
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

let join (s1 : state) (s2 : state) : state =
  Printf.printf "join %s\n and %s\n" (to_string s1) (to_string s2);
  {
  constraints = Apron.Abstract1.join manager s1.constraints s2.constraints;
  vstack = Vstack.join s1.vstack s2.vstack;
  locals = List.map2_exn s1.locals s2.locals ~f:Value.join;
  globals = Globals.join s1.globals s2.globals;
  memory = Memory.join s1.memory s2.memory;
  env = (assert Stdlib.(s1.env = s2.env); s1.env);
}

let join_opt (s1 : state) (s2 : state option) : state =
  match s2 with
  | Some s -> join s1 s
  | None -> s1

(** Add multiple constraints *)
let add_constraints (s : state) (constraints : (string * string) list) : state =
  try
    { s
      with constraints =
             Apron.Abstract1.assign_linexpr_array manager s.constraints
               (Array.of_list (List.map constraints ~f:(fun (v, _) -> Apron.Var.of_string v)))
               (Array.of_list (List.map constraints ~f:(fun (_, c) -> Apron.Parser.linexpr1_of_string s.env c)))
               None }
  with
  | Apron.Manager.Error { exn; funid; msg } ->
    failwith (Printf.sprintf "Apron error in add_constraint: exc: %s, funid: %s, msg: %s" (Apron.Manager.string_of_exc exn) (Apron.Manager.string_of_funid funid) msg)

(** Add one constrait of the form v = linexpr to the state constraints, returns the updated state *)
let add_constraint (s : state) (v : string) (linexpr : string) : state =
  add_constraints s [(v,  linexpr)]


(** Only keep the given variables in the constraints, returns the updated state *)
let keep_only (s : state) (vars : string list) : state =
  { s with constraints = Apron.Abstract1.forget_array manager s.constraints
               (Array.filter (fst (Apron.Environment.vars s.env)) (* fst because we only have int variables for now *)
                  ~f:(fun v -> not (List.mem vars (Apron.Var.to_string v) ~equal:Stdlib.(=))))
               false (* not sure what this means *)
  }

(** Checks if the value of variable v may be equal to a given number.
    Returns two booleans: the first one indicates if v can be equal to n, and the second if it can be different than n *)
let is_equal (s : state) (v : string) (n : int) : bool * bool =
  let box = Apron.Abstract1.to_box manager s.constraints in
  let dim = Apron.Environment.dim_of_var box.box1_env (Apron.Var.of_string v) in
  let interval = Array.get box.interval_array dim in
  (* Apron doc: cmp is: "0: equality -1: i1 included in i2 +1: i2 included in i1 -2: i1.inf less than or equal to i2.inf +2: i1.inf greater than i2.inf" *)
  match Apron.Interval.cmp interval (Apron.Interval.of_int n n) with
  | 0 -> (true, false) (* definitely n, and only n *)
  | -1 -> (true, false) (* should not happen, because that's caught by the previous case *)
  | +1 -> (true, true) (* [n,n] is contained in v: can be n or not *)
  | -2 | +2 -> (false, true) (* definitely not n *)
  | _ -> failwith "should not happen"

let is_zero (s : state) (v : string) : bool * bool = is_equal s v 0

(** Checks if two variables may be equal.
    Returns two booleans: the first one indicates if they may be equal, the second one if they may be different *)
let are_equal (s : state) (v1 : string) (v2 : string) : bool * bool =
  let interval = Apron.Abstract1.bound_linexpr manager s.constraints (Apron.Parser.linexpr1_of_string s.env (Printf.sprintf "%s-%s" v1 v2)) in
  match Apron.Interval.cmp interval (Apron.Interval.of_int 0 0) with
  | 0 -> (true, false)
  | -1 -> (true, false)
  | +1 -> (true, true)
  | -2 | +2 -> (false, true)
  | _ -> failwith "should not happen"

let are_precisely_equal (s : state) (v1 : string) (v2 : string) =
  match are_equal s v1 v2 with
  | true, false -> true
  | _ -> false
