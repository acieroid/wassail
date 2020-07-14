open Core_kernel

type apron_domain = Polka.strict Polka.t
let manager = Polka.manager_alloc_strict ()

(** A state that contains the constraints *)
type t = {
  constraints : apron_domain Apron.Abstract1.t; (** The relational constraints *)
  env : Apron.Environment.t; (** The relational environment *)
}

let bind_compare (res : int) (cmp : 'a -> 'a -> int) (x : 'a) (y : 'a) : int =
  if res = 0 then 0 else cmp x y
let compare (s1 : t) (s2 : t) : int =
  bind_compare (Apron.Environment.compare s1.env s2.env)
    (fun c1 c2 ->
       if Apron.Abstract1.is_eq manager c1 c2 then 0
       else if Apron.Abstract1.is_leq manager c1 c2 then -1
       else 1) s1.constraints s2.constraints

let constraints_to_string (c : apron_domain Apron.Abstract1.t) : string =
  (Apron.Abstract1.print Stdlib.Format.str_formatter c; Stdlib.Format.flush_str_formatter ())

let to_string (s : t) : string =
  Printf.sprintf "constraints: %s" (constraints_to_string s.constraints)

(** Initializes the state for the analysis of CFG cfg. All variables are unconstrained, except for locals with have 0 as initial value
    @param cfg is the CFG under analysis
    @param vars the set of Apron variables to create. It is expected that variables for locals are named l0 to ln, where l0 to li are parameters (there are i params), and li+1 to ln are locals that are initialized to 0.
 *)
let init (cfg : Cfg.t) (vars : Spec_inference.var list) : t =
  assert (List.length cfg.return_types <= 1); (* wasm spec does not allow for more than one return type (currently) *)
  let vars_and_vals = List.map vars ~f:(fun v -> (Apron.Var.of_string (Spec_inference.var_to_string v), match v with
    | Spec_inference.Local n when n >= List.length cfg.arg_types -> Apron.Interval.of_int 0 0
    | Spec_inference.Local _ | Spec_inference.Global _ | Spec_inference.MemoryKey _ | Spec_inference.MemoryVal _ -> Apron.Interval.top
    | _ -> Apron.Interval.bottom))
  in
  let apron_vars = Array.of_list (List.map vars_and_vals  ~f:fst) in
  let apron_env = Apron.Environment.make apron_vars [| |] in
  let apron_abs = Apron.Abstract1.of_box manager apron_env apron_vars (Array.of_list (List.map vars_and_vals ~f:(fun (_, v) -> v))) in
  { env = apron_env; constraints = apron_abs }

(** Creates the bottom value *)
let bottom (_cfg : Cfg.t) (vars : string list) : t =
  let apron_vars = Array.of_list (List.map vars ~f:Apron.Var.of_string) in
  let apron_env = Apron.Environment.make apron_vars [| |] in
  let apron_abs = Apron.Abstract1.bottom manager apron_env in
  { constraints = apron_abs; env = apron_env }

(** Creates the top value *)
let top (_cfg : Cfg.t) (vars : string list) : t =
  let apron_vars = Array.of_list (List.map vars ~f:Apron.Var.of_string) in
  let apron_env = Apron.Environment.make apron_vars [| |] in
  let apron_abs = Apron.Abstract1.top manager apron_env in
  { constraints = apron_abs; env = apron_env }

let join (s1 : t) (s2 : t) : t =
  let res = {
  constraints = Apron.Abstract1.join manager s1.constraints s2.constraints;
  env = (assert Stdlib.(s1.env = s2.env); s1.env);
} in
  Printf.printf "join %s\n and %s\ngives %s\n" (to_string s1) (to_string s2) (to_string res);
  res

let widen (s1 : t) (s2 : t) : t =
  let res = {
    constraints = Apron.Abstract1.widening manager s1.constraints s2.constraints;
    env = (assert Stdlib.(s1.env = s2.env); s1.env);
  } in
  Printf.printf "widening %s\n and %s\ngives %s\n" (to_string s1) (to_string s2) (to_string res);
  res

let meet (s1 : t) (s2 : t) : t =
  let res = {
    constraints = Apron.Abstract1.meet manager s1.constraints s2.constraints;
    env = (assert Stdlib.(s1.env = s2.env); s1.env);
  } in
  Printf.printf "meet %s\n and %s\ngives %s\n" (to_string s1) (to_string s2) (to_string res);
  res

let join_opt (s1 : t) (s2 : t option) : t =
  match s2 with
  | Some s -> join s1 s
  | None -> s1

(** Add multiple constraints *)
let add_constraints (s : t) (constraints : (string * string) list) : t =
  try
    List.iter constraints ~f:(fun (x, y) -> Printf.printf "add constraint: %s = %s\n" x y);
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
let add_constraint (s : t) (v : string) (linexpr : string) : t =
  add_constraints s [(v,  linexpr)]

let add_equality_constraints (s : t) (vs : (Spec_inference.var * Spec_inference.var) list) : t =
  add_constraints s (List.map vs ~f:(fun (v1, v2) -> (Spec_inference.var_to_string v1, Spec_inference.var_to_string v2)))

let add_equality_constraint (s : t) (v1 : Spec_inference.var) (v2 : Spec_inference.var) : t =
  add_constraint s (Spec_inference.var_to_string v1) (Spec_inference.var_to_string v2)

let add_interval_constraint (s : t) (v : Spec_inference.var) (bounds: int * int) : t =
  add_constraint s (Spec_inference.var_to_string v) (Printf.sprintf "[%d;%d]" (fst bounds) (snd bounds))

let meet_interval (s : t) (v : string) (bounds : int * int) : t =
  let earray = Apron.Lincons1.array_make s.env 1 in
  Apron.Lincons1.array_set earray 0 (Apron.Parser.lincons1_of_string s.env (Printf.sprintf "%s=[%d;%d]" v (fst bounds) (snd bounds)));
  { s with constraints = Apron.Abstract1.meet_lincons_array manager s.constraints earray }

(** Only keep the given variables in the constraints, returns the updated t *)
let keep_only (s : t) (vars : string list) : t =
  { s with constraints = Apron.Abstract1.forget_array manager s.constraints
               (Array.filter (fst (Apron.Environment.vars s.env)) (* fst because we only have int variables for now *)
                  ~f:(fun v -> not (List.mem vars (Apron.Var.to_string v) ~equal:Stdlib.(=))))
               false (* not sure what this means *)
  }

(** Checks if the value of variable v may be equal to a given number.
    Returns two booleans: the first one indicates if v can be equal to n, and the second if it can be different than n *)
let is_equal (s : t) (v : Spec_inference.var) (n : int) : bool * bool =
  let box = Apron.Abstract1.to_box manager s.constraints in
  let dim = Apron.Environment.dim_of_var box.box1_env (Apron.Var.of_string (Spec_inference.var_to_string v)) in
  let interval = Array.get box.interval_array dim in
  (* Apron doc: cmp is: "0: equality -1: i1 included in i2 +1: i2 included in i1 -2: i1.inf less than or equal to i2.inf +2: i1.inf greater than i2.inf" *)
  match Apron.Interval.cmp interval (Apron.Interval.of_int n n) with
  | 0 -> (true, false) (* definitely n, and only n *)
  | -1 -> (true, false) (* should not happen, because that's caught by the previous case *)
  | +1 -> (true, true) (* [n,n] is contained in v: can be n or not *)
  | -2 | +2 -> (false, true) (* definitely not n *)
  | _ -> failwith "should not happen"

let is_zero (s : t) (v : Spec_inference.var) : bool * bool = is_equal s v 0

(** Checks if two variables may be equal.
    Returns two booleans: the first one indicates if they may be equal, the second one if they may be different *)
let are_equal (s : t) (v1 : Spec_inference.var) (v2 : Spec_inference.var) : bool * bool =
  if Stdlib.(v1 = v2) then (true, false) else
  let interval = Apron.Abstract1.bound_linexpr manager s.constraints (Apron.Parser.linexpr1_of_string s.env (Printf.sprintf "%s-%s" (Spec_inference.var_to_string v1) (Spec_inference.var_to_string v2))) in
  match Apron.Interval.cmp interval (Apron.Interval.of_int 0 0) with
  | 0 -> (true, false)
  | -1 -> (true, false)
  | +1 -> (true, true)
  | -2 | +2 -> (false, true)
  | _ -> failwith "should not happen"

(** Check if v1 = v2+offset, i.e., v1-v2 = offset *)
let are_equal_offset (s : t) (v1 : Spec_inference.var) (v2 : Spec_inference.var) (offset : int) : bool * bool =
  if Stdlib.(v1 = v2) then
    if offset = 0 then (true, false) else (false, true)
  else
  let interval = Apron.Abstract1.bound_linexpr manager s.constraints (Apron.Parser.linexpr1_of_string s.env (Printf.sprintf "%s-%s" (Spec_inference.var_to_string v1) (Spec_inference.var_to_string v2))) in
  match Apron.Interval.cmp interval (Apron.Interval.of_int offset offset) with
  | 0 -> (true, false)
  | -1 -> (true, false)
  | +1 -> (true, true)
  | -2 | +2 -> (false, true)
  | _ -> failwith "should not happen"



let are_precisely_equal (s : t) (v1 : Spec_inference.var) (v2 : Spec_inference.var) =
  match are_equal s v1 v2 with
  | true, false -> true
  | _ -> false
