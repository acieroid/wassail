open Core_kernel

type apron_domain = Polka.equalities Polka.t
let manager = Polka.manager_alloc_equalities ()

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

let equal (s1 : t) (s2 : t) : bool = compare s1 s2 = 0

let constraints_to_string (c : apron_domain Apron.Abstract1.t) : string =
  (Apron.Abstract1.print Stdlib.Format.str_formatter c; Stdlib.Format.flush_str_formatter ())

let to_string (s : t) : string =
  Printf.sprintf "constraints: %s" (constraints_to_string s.constraints)

(** Initializes the state for the analysis of CFG cfg. All variables are unconstrained, except for locals with have 0 as initial value
    @param cfg is the CFG under analysis
    @param vars the set of Apron variables to create. It is expected that variables for locals are named l0 to ln, where l0 to li are parameters (there are i params), and li+1 to ln are locals that are initialized to 0.
 *)
let init (cfg : 'a Cfg.t) (vars : Var.Set.t) : t =
  assert (List.length cfg.return_types <= 1); (* wasm spec does not allow for more than one return type (currently) *)
  let vars_and_vals = List.filter_map (Var.Set.to_list vars) ~f:(fun v -> match v with
      | Var.Local n when n >= List.length cfg.arg_types ->
        (* Locals that are not arguments are mapped to 0 *)
        Some (Apron.Var.of_string (Var.to_string v), Apron.Interval.of_int 0 0)
      | Var.Local _ | Var.Global _ ->
        (* Other locals (arguments), and globals are top *)
        Some (Apron.Var.of_string (Var.to_string v), Apron.Interval.top)
      | Var.Const _ ->
        (* Const are not used in the relational domain *)
        None
      | _ ->
        (* Other variables are mapped to bottom *)
        Some (Apron.Var.of_string (Var.to_string v), Apron.Interval.bottom))
  in
  let apron_vars = Array.of_list (List.map vars_and_vals  ~f:fst) in
  let apron_env = Apron.Environment.make apron_vars [| |] in
  let apron_abs = Apron.Abstract1.of_box manager apron_env apron_vars (Array.of_list (List.map vars_and_vals ~f:(fun (_, v) -> v))) in
  { env = apron_env; constraints = apron_abs }

(** Creates the bottom value *)
let bottom (_cfg : 'a Cfg.t) (vars : Var.Set.t) : t =
  let apron_vars = Array.of_list (List.filter_map (Var.Set.to_list vars) ~f:(fun v -> match v with
      | Var.Const _ -> None
        (* Const are not used in the relational domain *)
      | _ -> Some (Apron.Var.of_string (Var.to_string v)))) in
  let apron_env = Apron.Environment.make apron_vars [| |] in
  let apron_abs = Apron.Abstract1.bottom manager apron_env in
  { constraints = apron_abs; env = apron_env }

(** Creates the top value *)
let top (_cfg : 'a Cfg.t) (vars : Var.Set.t) : t =
  let apron_vars = Array.of_list (List.filter_map (Var.Set.to_list vars) ~f:(fun v -> match v with
      | Const _ ->
        (* Const are not used in the relational domain *)
        None
      | _ -> Some (Apron.Var.of_string (Var.to_string v)))) in
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

(** Constructs the rhs of a constraint as v1+v2 *)
let add (v1 : Var.t) (v2 : Var.t) : string =
  match v1, v2 with
  | Const x, Const y -> Prim_value.to_string (Prim_value.add x y)
  | _ -> (Printf.sprintf "%s+%s" (Var.to_string v1) (Var.to_string v2))

(** Constructs the rhs of a constraint as v1-v2 *)
let sub (v1 : Var.t) (v2 : Var.t) : string =
  match v1, v2 with
  | Const x, Const y -> Prim_value.to_string (Prim_value.sub x y)
  | _ -> (Printf.sprintf "%s-%s" (Var.to_string v1) (Var.to_string v2))

(** Add multiple constraints *)
let add_constraints (s : t) (constraints : (Var.t * string) list) : t =
  let filtered_constraints = List.filter constraints ~f:(fun (l, _r) -> match l with
      | Const _ ->
        (* Const are not Apron variables, hence these are filtered out when adding constraints *)
        false
      | _ -> true) in
  try
    List.iter filtered_constraints ~f:(fun (x, y) -> Printf.printf "add constraint: %s = %s\n" (Var.to_string x) y);
    { s
      with constraints =
             Apron.Abstract1.assign_linexpr_array manager s.constraints
               (Array.of_list (List.map filtered_constraints ~f:(fun (v, _) -> Apron.Var.of_string (Var.to_string v))))
               (Array.of_list (List.map filtered_constraints ~f:(fun (_, c) -> Apron.Parser.linexpr1_of_string s.env c)))
               None }
  with
  | Apron.Manager.Error { exn; funid; msg } ->
    failwith (Printf.sprintf "Apron error in add_constraint: exc: %s, funid: %s, msg: %s" (Apron.Manager.string_of_exc exn) (Apron.Manager.string_of_funid funid) msg)

(** Add one constrait of the form v = linexpr to the state constraints, returns the updated state *)
let add_constraint (s : t) (v : Var.t) (linexpr : string) : t =
  add_constraints s [(v,  linexpr)]

let add_equality_constraints (s : t) (vs : (Var.t * Var.t) list) : t =
  add_constraints s (List.map vs ~f:(fun (v1, v2) -> (v1, Var.to_string v2)))

let add_equality_constraint (s : t) (v1 : Var.t) (v2 : Var.t) : t =
  add_constraint s v1 (Var.to_string v2)

let add_interval_constraint (s : t) (v : Var.t) (bounds: int * int) : t =
  add_constraint s v (Printf.sprintf "[%d;%d]" (fst bounds) (snd bounds))

let meet_interval (s : t) (v : string) (bounds : int * int) : t =
  let earray = Apron.Lincons1.array_make s.env 1 in
  Apron.Lincons1.array_set earray 0 (Apron.Parser.lincons1_of_string s.env (Printf.sprintf "%s=[%d;%d]" v (fst bounds) (snd bounds)));
  { s with constraints = Apron.Abstract1.meet_lincons_array manager s.constraints earray }

(** Only keep the given variables in the constraints, returns the updated t *)
let keep_only (s : t) (vars : Var.t list) : t =
  let str_vars = List.map vars ~f:Var.to_string in
  { s with constraints = Apron.Abstract1.forget_array manager s.constraints
               (Array.filter (fst (Apron.Environment.vars s.env)) (* fst because we only have int variables for now *)
                  ~f:(fun v -> not (List.mem str_vars (Apron.Var.to_string v) ~equal:Stdlib.(=))))
               false (* not sure what this means *)
  }

(** Checks if the value of variable v may be equal to a given number.
    Returns two booleans: the first one indicates if v can be equal to n, and the second if it can be different than n *)
let is_equal (s : t) (v : Var.t) (n : int) : bool * bool =
  let box = Apron.Abstract1.to_box manager s.constraints in
  let dim = Apron.Environment.dim_of_var box.box1_env (Apron.Var.of_string (Var.to_string v)) in
  let interval = Array.get box.interval_array dim in
  (* Apron doc: cmp is: "0: equality -1: i1 included in i2 +1: i2 included in i1 -2: i1.inf less than or equal to i2.inf +2: i1.inf greater than i2.inf" *)
  match Apron.Interval.cmp interval (Apron.Interval.of_int n n) with
  | 0 -> (true, false) (* definitely n, and only n *)
  | -1 -> (true, false) (* should not happen, because that's caught by the previous case *)
  | +1 -> (true, true) (* [n,n] is contained in v: can be n or not *)
  | -2 | +2 -> (false, true) (* definitely not n *)
  | _ -> failwith "should not happen"

let is_zero (s : t) (v : Var.t) : bool * bool = is_equal s v 0

(** Checks if two variables may be equal.
    Returns two booleans: the first one indicates if they may be equal, the second one if they may be different *)
let are_equal (s : t) (v1 : Var.t) (v2 : Var.t) : bool * bool =
  if Stdlib.(v1 = v2) then (true, false) else
    match v1, v2 with
    | Const _, Const _ -> (false, true) (* if this could be true, it would have ben caught by the previous if *)
    | _ ->
      let interval = Apron.Abstract1.bound_linexpr manager s.constraints (Apron.Parser.linexpr1_of_string s.env (Printf.sprintf "%s-%s" (Var.to_string v1) (Var.to_string v2))) in
      match Apron.Interval.cmp interval (Apron.Interval.of_int 0 0) with
      | 0 -> (true, false)
      | -1 -> (true, false)
      | +1 -> (true, true)
      | -2 | +2 -> (false, true)
      | _ -> failwith "should not happen"

(** Check if v1 = v2, where v1 and v2 have offsets.
    Return value is similar to are_equal *)
let are_equal_offset (s : t) ((v1, offset1) : Var.with_offset) ((v2, offset2) : Var.with_offset) : bool * bool =
  let to_eq itv =
    (* Convert an interval that results from a equality check (performed by checking equality of x and y by computing the interval of x-y, hence [0,0] indicates equality) into the two necessary booleans *)
    match Apron.Interval.cmp itv (Apron.Interval.of_int 0 0) with
    | 0 -> Printf.printf "tf1\n"; (true, false)
    | -1 -> Printf.printf "tf2\n"; (true, false)
    | +1 -> Printf.printf "tt\n"; (true, true)
    | -2 | +2 -> Printf.printf "ft\n"; (false, true)
    | _ -> failwith "should not happen"
  in
             Printf.printf "Checking equality of %s+%d and %s+%d\n" (Var.to_string v1) offset1 (Var.to_string v2) offset2;
  if Stdlib.(v1 = v2) then
    if offset1 = offset2 then (true, false) else (false, true)
  else
    (* Ideally, we'd want to check if (v1+offset1)-(v2+offset2) = [0,0].
       But we can't do that directly with apron, as only one constant coefficient can appear, and
       both offset1 and offset2 are constants, and so can be either v1, v2, or both.
       We just do a case by case instead *)
    match v1, v2 with
    | Const v1, Const v2 ->
      (* all constants, no need for Apron for that *)
      if Prim_value.equal (Prim_value.add_int v1 offset1) (Prim_value.add_int v2 offset2)
      then (true, false)
      else (false, true)
    | Const v1, _ ->
      (* (v1+offset1-offset2)-v2 =? [0,0] *)
      to_eq (Apron.Abstract1.bound_linexpr manager s.constraints
               (Apron.Parser.linexpr1_of_string s.env
                  (Printf.sprintf "%s-%s"
                     (Prim_value.to_string (Prim_value.add_int v1 (offset1-offset2)))
                     (Var.to_string v2))))
    | _, Const v2 ->
      (* (v2+offset2-offset1)-v1 =? [0,0] *)
      to_eq (Apron.Abstract1.bound_linexpr manager s.constraints
               (Apron.Parser.linexpr1_of_string s.env
                  (Printf.sprintf "%s-%s"
                     (Prim_value.to_string (Prim_value.add_int v2 (offset2-offset1)))
                     (Var.to_string v1))))
    | _, _ ->
      (* (offset2-offset1)+(v2-v1) =? [0,0] *)
      to_eq (Apron.Abstract1.bound_linexpr manager s.constraints
               (Apron.Parser.linexpr1_of_string s.env
                  (Printf.sprintf "%d+%s-%s"
                     (offset1 + offset2)
                     (Var.to_string v2) (Var.to_string v1))))


let are_precisely_equal (s : t) (v1 : Var.t) (v2 : Var.t) =
  match are_equal s v1 v2 with
  | true, false -> true
  | _ -> false
