open Core_kernel

(** The apron domain used.
    We use octagon here, but simpler domains could be good enough.
    However, with a simpler domain (e.g., Polka.equalities Polka.t), many inline tests will likely fail
  *)
type apron_domain = Oct.t

(** Apron requires a "manager" *)
let manager = Oct.manager_alloc ()

(** A state that contains the constraints *)
type t = {
  constraints : apron_domain Apron.Abstract1.t; (** The relational constraints (also contains the environment) *)
}

(** Catch apron errors when executing function `f`, print it (uses `name` as the source from the error), and returns `default` in case of error *)
let catch_apron_error (name : string) (f : unit -> 'a) (default : 'a) : 'a =
  try
    f ()
  with
  | Apron.Manager.Error { exn; funid; msg } ->
    Printf.printf "Apron error in function %s: %s, funid: %s, msg: %s" name (Apron.Manager.string_of_exc exn) (Apron.Manager.string_of_funid funid) msg;
    default

(** Catch apron errors and rethrow them using failwith. This is required to have helpful error messages *)
let rethrow_apron_error (name : string) (f : unit -> 'a) : 'a =
  try
    f ()
  with
  | Apron.Manager.Error { exn; funid; msg } ->
    failwith (Printf.sprintf "Apron error in function %s: %s, funid: %s, msg: %s" name (Apron.Manager.string_of_exc exn) (Apron.Manager.string_of_funid funid) msg)

(** Converts an Apron environment to its string representation *)
let itv_to_string (itv : Apron.Interval.t) : string =
  rethrow_apron_error "Relational_domain.env_to_string" (fun () ->
      Apron.Interval.print Stdlib.Format.str_formatter itv; Stdlib.Format.flush_str_formatter ())

(** Converts an Apron environment to its string representation *)
let env_to_string (env : Apron.Environment.t) : string =
  rethrow_apron_error "Relational_domain.env_to_string" (fun () ->
      Apron.Environment.print Stdlib.Format.str_formatter env; Stdlib.Format.flush_str_formatter ())

(** Converts an Apron linexpr to its string representation *)
let linexpr_to_string (linexpr : Apron.Linexpr1.t) :string =
  rethrow_apron_error "Relational_domain.linexpr_to_string" (fun () ->
      Apron.Linexpr1.print Stdlib.Format.str_formatter linexpr; Stdlib.Format.flush_str_formatter ())

(** Converts an Apron set of constraints to its string representation *)
let constraints_to_string (c : apron_domain Apron.Abstract1.t) : string =
  rethrow_apron_error "Relational_domain.constraints_to_string" (fun () ->
      (Apron.Abstract1.print Stdlib.Format.str_formatter c; Stdlib.Format.flush_str_formatter ()))

(** Converts a state to its string representation *)
let to_string (s : t) : string =
  Printf.sprintf "constraints: %s" (constraints_to_string s.constraints)

(** Converts a state to its string representation, also showing the environment *)
let to_full_string (s : t) : string =
  Printf.sprintf "env: %s, constraints: %s" (env_to_string s.constraints.env) (constraints_to_string s.constraints)

(** Compares two states *)
let compare (s1 : t) (s2 : t) : int =
  let bind_compare (res : int) (cmp : 'a -> 'a -> int) (x : 'a) (y : 'a) : int =
    if res = 0 then cmp x y else res in
  rethrow_apron_error "Relational_domain.compare" (fun () ->
      bind_compare (Apron.Environment.compare s1.constraints.env s2.constraints.env)
        (fun c1 c2 ->
           if Apron.Abstract1.is_eq manager c1 c2 then 0
           else if Apron.Abstract1.is_leq manager c1 c2 then -1
           else 1) s1.constraints s2.constraints)

(** Checks equality between two states *)
let equal (s1 : t) (s2 : t) : bool =
  compare s1 s2 = 0

(** Checks subsumption between two states: does s1 subsume s2? *)
let subsumes (s1 : t) (s2 : t) : bool =
  rethrow_apron_error "Relational_domain.subsumes" (fun () ->
      Apron.Abstract1.is_leq manager s2.constraints s1.constraints)

(** Used in tests to check equality or subsumption. In many cases, while we
   would want two things to be equal, the imprecision in the abstract domain
   makes it so that they are not equal. But they should still preserve the
   subsumption *)
let should_subsume_or_equal (v1 : t) (v2 : t) : bool =
  if subsumes v1 v2 then begin
    if not (equal v1 v2) then
      Printf.printf "[IMPRECISION] %s not equal to %s\n" (to_string v1) (to_string v2);
    true
  end else
    false

(** Checks equality and print an error message in case both values are not equal *)
let should_equal (v1 : t) (v2 : t) : bool =
  if not (equal v1 v2) then begin
    Printf.printf "[FAILURE] %s not equal to %s\n" (to_string v1) (to_string v2);
    false
  end else
    true

(** Creates the bottom value *)
let bottom (vars : Var.Set.t) : t =
  rethrow_apron_error "Relational_domain.bottom" (fun () ->
      let apron_vars = Array.of_list (List.filter_map (Var.Set.to_list vars) ~f:(fun v -> match v with
          | Var.Const _ -> None
          (* Const are not used in the relational domain *)
          | _ -> Some (Apron.Var.of_string (Var.to_string v)))) in
      let apron_env = Apron.Environment.make apron_vars [| |] in
      let apron_abs = Apron.Abstract1.bottom manager apron_env in
      { constraints = apron_abs })


let%test_unit "bottom should not result in an error" =
  let _: t = bottom Var.Set.empty in
  let _: t = bottom (Var.Set.of_list [Var.Local 0; Var.Return; Var.Global 0]) in
  ()

(** Creates the top value *)
let top (vars : Var.Set.t) : t =
  rethrow_apron_error "Relational_domain.top" (fun () ->
      let apron_vars = Array.of_list (List.filter_map (Var.Set.to_list vars) ~f:(fun v -> match v with
          | Const _ ->
            (* Const are not used in the relational domain *)
            None
          | _ -> Some (Apron.Var.of_string (Var.to_string v)))) in
      let apron_env = Apron.Environment.make apron_vars [| |] in
      let apron_abs = Apron.Abstract1.top manager apron_env in
      { constraints = apron_abs })

let%test_unit "top should not result in an error" =
  let _: t = top Var.Set.empty in
  let _: t = top (Var.Set.of_list [Var.Local 0; Var.Return; Var.Global 0]) in
  ()

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
  (* (Printf.printf "add_constraints to %s: [%s]\n"
       (to_full_string s)
       (String.concat ~sep:", "
          (List.map constraints ~f:(fun (left, right) -> Printf.sprintf "%s = %s" (Var.to_string left) right)))); *)
  let filtered_constraints = List.filter constraints ~f:(fun (l, r) -> match l with
      | Const _ ->
        (* Const are not Apron variables, hence these are filtered out when adding constraints *)
        false
      | _ when String.equal (Var.to_string l) r -> false
      |_  -> true) in
  rethrow_apron_error "Relational_domain.add_constraints" (fun () ->
      List.iter filtered_constraints ~f:(fun (x, y) ->
          (Log.info
             (Printf.sprintf "add constraint: %s = %s" (Var.to_string x) y)));
      let lhs = List.map filtered_constraints ~f:(fun (v, _) -> Apron.Var.of_string (Var.to_string v)) in
      let rhs = List.map filtered_constraints ~f:(fun (_, c) -> Apron.Parser.linexpr1_of_string s.constraints.env c) in
      { constraints = Apron.Abstract1.assign_linexpr_array manager s.constraints (Array.of_list lhs)  (Array.of_list rhs) None })

let%test "add_constraints top (l0 = l0) results in top" =
  let top: t = top (Var.Set.of_list [Var.Local 0; Var.Var (Instr.Label.Test.lab 0); Var.Return; Var.Global 0]) in
  let top2 = add_constraints top [(Var.Local 0, "l0")] in
  should_equal top top2

let%test "add_constraints twice does not change anything" =
  let vars = Var.Set.of_list [Var.Local 0; Var.Local 1; Var.Local 2] in
  let v = add_constraints (top vars) [(Var.Local 0, "l1")] in
  let v' = add_constraints v [(Var.Local 0, "l1")] in
  should_equal v v'

let%test "two add_constraints can be done in one call" =
  let vars = Var.Set.of_list [Var.Local 0; Var.Local 1; Var.Local 2] in
  (* l0 = l1 *)
  let v0 = add_constraints (top vars) [(Var.Local 0, "l1")] in
  (* l0 = l1 and l1 = l2 *)
  let v1 = add_constraints v0 [(Var.Local 1, "l2")] in
  (* l0 = l1 and l1 = l2 *)
  let v1' = add_constraints (top vars) [(Var.Local 0, "l1"); (Var.Local 1, "l2")] in
  should_equal v1 v1'

(** Add one constraint of the form v = linexpr to the state constraints, returns the updated state *)
let add_constraint (s : t) (v : Var.t) (linexpr : string) : t =
  add_constraints s [(v,  linexpr)]

(** Add an equality constraint between pairs of variable, i.e., v1 = v2 *)
let add_equality_constraints (s : t) (vs : (Var.t * Var.t) list) : t =
  add_constraints s (List.map vs ~f:(fun (v1, v2) -> (v1, Var.to_string v2)))

(** Creates a new state from equality constraints, starting from the top abstract value *)
let of_equality_constraints (vars : Var.Set.t) (constraints : (Var.t * Var.t) list) : t =
  add_equality_constraints (top vars) constraints

let%test "of_equality_constraints is the same as top with adding constraints" =
  let vars = Var.Set.of_list [Var.Local 0; Var.Local 1; Var.Local 2] in
  (* l0 = l1 and l1 = l2 *)
  let v1 = add_equality_constraints (top vars) [(Var.Local 0, Var.Local 1); (Var.Local 1, Var.Local 2)] in
  let v2 = of_equality_constraints vars [(Var.Local 0, Var.Local 1); (Var.Local 1, Var.Local 2)] in
  equal v1 v2

(** Like add_equality_constraints, but adds a single constraint *)
let add_equality_constraint (s : t) (v1 : Var.t) (v2 : Var.t) : t =
  add_constraint s v1 (Var.to_string v2)

(** Constraints one variable to a given interval *)
let add_interval_constraint (s : t) (v : Var.t) (bounds: int * int) : t =
  add_constraint s v (Printf.sprintf "[%d;%d]" (fst bounds) (snd bounds))

let%test "adding an interval constraint should apply to all equal variables" =
  (* That may not be actually true. *)
  let vars = Var.Set.of_list [Var.Local 0; Var.Local 1; Var.Local 2] in
  (* l0 = l1 and l1 = l2 *)
  let v0 = of_equality_constraints vars [(Var.Local 0, Var.Local 1); (Var.Local 1, Var.Local 2)] in
  (* l0 = l1 and l1 = l2 and l0 in [0, 10] *)
  let v1 = add_interval_constraint v0 (Var.Local 0) (0, 10) in
  let _v2 = add_interval_constraint v0 (Var.Local 1) (0, 10) in
  let _v3 = add_interval_constraint v0 (Var.Local 2) (20, 30) in
  not (equal v0 v1) (* && equal v1 v2 && equal v2 v1 && not (equal v1 v3) && not (equal v0 v3) *)

(** Meets an expression with the given interval *)
let meet_interval (s : t) (v : string) (bounds : int * int) : t =
  rethrow_apron_error "Relational_domain.meet_interval" (fun () ->
      let earray = Apron.Lincons1.array_make s.constraints.env 1 in
      Apron.Lincons1.array_set earray 0 (Apron.Parser.lincons1_of_string s.constraints.env (Printf.sprintf "%s=[%d;%d]" v (fst bounds) (snd bounds)));
      { constraints = Apron.Abstract1.meet_lincons_array manager s.constraints earray })

let%test "meet_interval restricts the variable domain" =
  let vars = Var.Set.of_list [Var.Local 0; Var.Local 1; Var.Local 2] in
  (* v0 is l0 = l1 and l1 = l2, with l0 in [0,100] *)
  let v0 = add_interval_constraint (of_equality_constraints vars [(Var.Local 0, Var.Local 1); (Var.Local 1, Var.Local 2)]) (Var.Local 0) (0, 100) in
  (* v1 is v0 with l0 = [0,10] *)
  let v1 = meet_interval v0 "l0" (0, 10) in
  (* v1' should be exactly equal as v1 *)
  let v1' = add_interval_constraint (of_equality_constraints vars [(Var.Local 0, Var.Local 1); (Var.Local 1, Var.Local 2)]) (Var.Local 0) (0, 10) in
  equal v1 v1'

(** Change the variables used in a state: new variables are unconstrained, variables that exist in s but not in vars are removed *)
let change_vars (s : t) (vars : Var.Set.t) : t =
  rethrow_apron_error "Relational.change_vars" (fun () ->
      let apron_vars = List.map (Var.Set.to_list vars) ~f:(fun v -> Apron.Var.of_string (Var.to_string v)) in
      let new_env = Apron.Environment.make (Array.of_list apron_vars) [||] in
      { constraints = Apron.Abstract1.change_environment manager s.constraints new_env
            false (* "projects new variables to the 0-plane", i.e., they have a value of 0. That's not what we want here.*)
      })

let%test "change_vars works as expected" =
  let vars = Var.Set.of_list [Var.Local 0; Var.Local 1; Var.Local 2; Var.Local 3] in
  let vars_restricted = Var.Set.of_list [Var.Local 0; Var.Local 1] in
  (* l0 = l1 and l2 = l3 *)
  let v0 = of_equality_constraints vars [(Var.Local 0, Var.Local 1); (Var.Local 2, Var.Local 3)] in
  (* v1 is v0 where only l0 and l1 are kept, l2 and l3 are lost *)
  let v1 = change_vars v0 vars_restricted in
  (* v1' is directly l0 = l1 *)
  let v1' = of_equality_constraints vars_restricted [(Var.Local 0, Var.Local 1)] in
  should_equal v1 v1'

(** Only keep the given variables in the constraints, returns the updated t *)
let keep_only (s : t) (vars : Var.Set.t) : t =
  rethrow_apron_error "Relational.keep_only" (fun () ->
      let str_vars = List.map (Var.Set.to_list vars) ~f:Var.to_string in
      let arr_vars = (Array.filter (fst (Apron.Environment.vars s.constraints.env)) (* fst because we only have int variables for now *)
                        ~f:(fun v -> not (List.mem str_vars (Apron.Var.to_string v) ~equal:Stdlib.(=)))) in
      change_vars { constraints = Apron.Abstract1.forget_array manager s.constraints arr_vars
                                     false (* not sure what this means *) }
        vars)

let%test "keep_only works as expected" =
  let vars = Var.Set.of_list [Var.Local 0; Var.Local 1; Var.Local 2; Var.Local 3] in
  let vars_restricted = Var.Set.of_list [Var.Local 0; Var.Local 1] in
  (* l0 = l1 and l2 = l3 *)
  let v0 = of_equality_constraints vars [(Var.Local 0, Var.Local 1); (Var.Local 2, Var.Local 3)] in
  let v1 = keep_only v0 vars_restricted in
  let v1' = of_equality_constraints vars_restricted [(Var.Local 0, Var.Local 1)] in
  should_equal v1 v1'

(** Checks if the value of variable v may be equal to a given number.
    Returns two booleans: the first one indicates if v can be equal to n, and the second if it can be different than n *)
let is_equal (s : t) (v : Var.t) (n : Int32.t) : bool * bool =
  rethrow_apron_error "Relational_domain.is_equal" (fun () ->
      let box = Apron.Abstract1.to_box manager s.constraints in
      let dim = Apron.Environment.dim_of_var box.box1_env (Apron.Var.of_string (Var.to_string v)) in
      let interval = Array.get box.interval_array dim in
      (* Apron doc: cmp is: "0: equality -1: i1 included in i2 +1: i2 included in i1 -2: i1.inf less than or equal to i2.inf +2: i1.inf greater than i2.inf" *)
      match Apron.Interval.cmp interval (Apron.Interval.of_int (Int32.to_int_exn n) (Int32.to_int_exn n)) with
      | 0 -> (true, false) (* definitely n, and only n *)
      | -1 -> (true, false) (* should not happen, because that's caught by the previous case *)
      | +1 -> (true, true) (* [n,n] is contained in v: can be n or not *)
      | -2 | +2 -> (false, true) (* definitely not n *)
      | _ -> failwith "should not happen")

let%test "is_equal works as expected" =
  let vars = Var.Set.of_list [Var.Local 0; Var.Local 1] in
  (* l0 = l1, l0 = [0,100] *)
  let v0 = add_interval_constraint (of_equality_constraints vars [(Var.Local 0, Var.Local 1)]) (Var.Local 0) (0, 100) in
  (* l0 = l1, l0 = [0,0] *)
  let v1 = add_interval_constraint (of_equality_constraints vars [(Var.Local 0, Var.Local 1)]) (Var.Local 0) (0, 0) in
  (* in v0, l0 =? 0, could be true, could be false *)
  Stdlib.(=) (is_equal v0 (Var.Local 0) 0l) (true, true) &&
  (* in v0, l0 =? 0, false *)
  Stdlib.(=) (is_equal v0 (Var.Local 0) 200l) (false, true) &&
  (* in v0, l0 =? 0, true *)
  Stdlib.(=) (is_equal v1 (Var.Local 0) 0l) (true, false)

(** Shortcut for is_equal s v 0 *)
let is_zero (s : t) (v : Var.t) : bool * bool = is_equal s v 0l

(** Checks if two variables may be equal.
    Returns two booleans: the first one indicates if they may be equal, the second one if they may be different *)
let are_equal (s : t) (v1 : Var.t) (v2 : Var.t) : bool * bool =
  rethrow_apron_error "Relational_domain.are_equal" (fun () ->
      if Stdlib.(v1 = v2) then (true, false) else
        match v1, v2 with
        | Const _, Const _ -> (false, true) (* if this could be true, it would have ben caught by the previous if *)
        | _ ->
          let interval = Apron.Abstract1.bound_linexpr manager s.constraints (Apron.Parser.linexpr1_of_string s.constraints.env (Printf.sprintf "%s-%s" (Var.to_string v1) (Var.to_string v2))) in
          match Apron.Interval.cmp interval (Apron.Interval.of_int 0 0) with
          | 0 -> (true, false)
          | -1 -> (true, false)
          | +1 -> (true, true)
          | -2 | +2 -> (false, true)
          | _ -> failwith "should not happen")

let%test "are_equal works as expected" =
  let vars = Var.Set.of_list [Var.Local 0; Var.Local 1] in
  (* l0 = l1, l0 = [0,100] *)
  let v0 = add_interval_constraint (of_equality_constraints vars [(Var.Local 0, Var.Local 1)]) (Var.Local 0) (0, 100) in
  (* top *)
  let v1 = top vars in
  (* l0 = [0,100], l1 = [200, 300] *)
  let v2 = add_interval_constraint (add_interval_constraint (top vars) (Var.Local 0) (0, 100)) (Var.Local 1) (200, 300) in
  (* in v0, l0 =? l1, could be true *)
  fst (are_equal v0 (Var.Local 0) (Var.Local 1)) &&
  (* in v1, l0 =? l1, could be true *)
  fst (are_equal v1 (Var.Local 0) (Var.Local 1)) &&
  (* in v2, l0 =? l1, could be false *)
  snd (are_equal v2 (Var.Local 0) (Var.Local 1))

(** Check if v1 = v2, where v1 and v2 have offsets.
    Return value is similar to are_equal *)
let are_equal_with_offset (s : t) ((v1, offset1) : Var.with_offset) ((v2, offset2) : Var.with_offset) : bool * bool =
  rethrow_apron_error "Relational_domain.are_equal_offset" (fun () ->
      let to_eq itv =
        (* Convert an interval that results from a equality check (performed by checking equality of x and y by computing the interval of x-y, hence [0,0] indicates equality) into the two necessary booleans *)
        match Apron.Interval.cmp itv (Apron.Interval.of_int 0 0) with
        | 0 -> (true, false)
        | -1 -> (true, false)
        | +1 -> (true, true)
        | -2 | +2 -> (false, true)
        | _ -> failwith "should not happen"
      in
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
                   (Apron.Parser.linexpr1_of_string s.constraints.env
                      (Printf.sprintf "%s-%s"
                         (Prim_value.to_string (Prim_value.add_int v1 (offset1-offset2)))
                         (Var.to_string v2))))
        | _, Const v2 ->
          (* (v2+offset2-offset1)-v1 =? [0,0] *)
          to_eq (Apron.Abstract1.bound_linexpr manager s.constraints
                   (Apron.Parser.linexpr1_of_string s.constraints.env
                      (Printf.sprintf "%s-%s"
                         (Prim_value.to_string (Prim_value.add_int v2 (offset2-offset1)))
                         (Var.to_string v1))))
        | _, _ ->
          (* (offset2-offset1)+(v2-v1) =? [0,0] *)
          to_eq (Apron.Abstract1.bound_linexpr manager s.constraints
                   (Apron.Parser.linexpr1_of_string s.constraints.env
                      (Printf.sprintf "%d+%s-%s"
                         (offset1 + offset2)
                         (Var.to_string v2) (Var.to_string v1)))))

let%test "are_equal_with_offset works as expected" =
  let vars = Var.Set.of_list [Var.Local 0; Var.Local 1] in
  (* l0 = l1, l0 = [0,100] *)
  let v0 = add_interval_constraint (of_equality_constraints vars [(Var.Local 0, Var.Local 1)]) (Var.Local 0) (0, 100) in
  (* top *)
  let v1 = top vars in
  (* l0 = [0,100], l1 = [200, 300] *)
  let v2 = add_interval_constraint (add_interval_constraint (top vars) (Var.Local 0) (0, 100)) (Var.Local 1) (200, 300) in
  (* l1 = l0+5 *)
  let v3 = add_constraint (top vars) (Var.Local 1) ("l0+5") in
  (* in v0, l0+5 =? l1+5, could true *)
  fst (are_equal_with_offset v0 (Var.Local 0, 5) (Var.Local 1, 5)) &&
  (* in v1, l0+5 =? l1+5, could be true, could be false *)
  fst (are_equal_with_offset v1 (Var.Local 0, 5) (Var.Local 1, 5)) &&
  snd (are_equal_with_offset v1 (Var.Local 0, 5) (Var.Local 1, 5)) &&
  (* in v2, l0+5 =? l1+5, could be false *)
  snd (are_equal_with_offset v2 (Var.Local 0, 5) (Var.Local 1, 5)) &&
  (* in v3, l1+0 =? l0+5, could be true *)
  fst (are_equal_with_offset v3 (Var.Local 1, 0) (Var.Local 0, 5))

(** Joins two states *)
let join (s1 : t) (s2 : t) : t =
  rethrow_apron_error "Relational_domain.join" (fun () ->
      let res = {
        constraints = Apron.Abstract1.join manager s1.constraints s2.constraints;
      } in
      Log.info
        (Printf.sprintf "join %s\n and %s\ngives %s\n" (to_string s1) (to_string s2) (to_string res));
      res)

let%test "join should correctly join" =
  let vars = Var.Set.of_list [Var.Local 0; Var.Local 1; Var.Local 2] in
  (* l0 = l1, l0 = [0,100] *)
  let v0 = add_interval_constraint (of_equality_constraints vars [(Var.Local 0, Var.Local 1)]) (Var.Local 0) (0, 100) in
  (* l0 = [0,100], l1 = [200, 300] *)
  let v1 = add_interval_constraint (add_interval_constraint (top vars) (Var.Local 0) (0, 100)) (Var.Local 1) (200, 300) in
  (* l0 = l1, l2 = [50, 200] *)
  let v2 = add_interval_constraint (of_equality_constraints vars [(Var.Local 0, Var.Local 1)]) (Var.Local 0) (50, 200) in
  (* l0 = l1, l2 = [0, 200] *)
  let v3 = add_interval_constraint (of_equality_constraints vars [(Var.Local 0, Var.Local 1)]) (Var.Local 0) (0, 200) in
  let top = top vars in
  let bottom = bottom vars  in
  should_equal (join top bottom) top &&
  should_equal (join top top) top &&
  should_equal (join bottom bottom) bottom &&
  should_subsume_or_equal (join v0 v1) v1 &&
  should_equal (join v0 v2) v3 &&
  should_equal (join v0 top) top &&
  should_equal (join top v0) top &&
  should_equal (join v0 bottom) v0 && should_equal (join bottom v0) v0 &&
  should_equal (join v1 bottom) v1 && should_equal (join bottom v1) v1

(** Like join, but the second argument is an option: if it is None, it is treated as bottom *)
let join_opt (s1 : t) (s2 : t option) : t =
  match s2 with
  | Some s -> join s1 s
  | None -> s1

(** Widen two states *)
let widen (s1 : t) (s2 : t) : t =
  rethrow_apron_error "Relational_domain.widen" (fun () ->
      let res = {
        constraints = Apron.Abstract1.widening manager s1.constraints s2.constraints;
      } in
      Log.info
        (Printf.sprintf "widening %s\n and %s\ngives %s\n" (to_string s1) (to_string s2) (to_string res));
      res)

let%test "widen should correctly widen" =
  let top = top Var.Set.empty in
  let bottom = bottom Var.Set.empty in
  equal (widen top bottom) top &&
  equal (widen top top) top &&
  equal (widen bottom bottom) bottom

(** Meet two states *)
let meet (s1 : t) (s2 : t) : t =
  rethrow_apron_error "Relational_domain.meet" (fun () ->
      let res = {
        constraints = Apron.Abstract1.meet manager s1.constraints s2.constraints;
      } in
      Log.info
        (Printf.sprintf "meet %s\n and %s\ngives %s\n" (to_string s1) (to_string s2) (to_string res));
      res)

let%test "meet should correctly meet" =
  let vars = Var.Set.of_list [Var.Local 0; Var.Local 1; Var.Local 2] in
  (* l0 = l1, l2 = [0,100] *)
  let v0 = add_interval_constraint (of_equality_constraints vars [(Var.Local 0, Var.Local 1)]) (Var.Local 2) (0, 100) in
  (* l0 = [0,100], l1 = [200, 300] *)
  let v1 = add_interval_constraint (add_interval_constraint (top vars) (Var.Local 0) (0, 100)) (Var.Local 1) (200, 300) in
  (* l0 = l1, l2 = [50, 200] *)
  let v2 = add_interval_constraint (of_equality_constraints vars [(Var.Local 0, Var.Local 1)]) (Var.Local 2) (50, 200) in
  (* l0 = l1, l2 = [50, 100] *)
  let v3 = add_interval_constraint (of_equality_constraints vars [(Var.Local 0, Var.Local 1)]) (Var.Local 2) (50, 100) in
  let top = top vars in
  let bottom = bottom vars in
  should_equal (meet v0 v0) v0 &&
  should_equal (meet top bottom) bottom &&
  should_equal (meet bottom top) bottom &&
  should_equal (meet v0 v1) bottom &&
  should_equal (meet v1 v0) bottom &&
  should_equal (meet v0 v2) v3 &&
  should_equal (meet v2 v0) v3


(** Initializes the state for the analysis of CFG cfg. All variables are unconstrained, except for locals with have 0 as initial value
    @param cfg is the CFG under analysis
    @param vars the set of Apron variables to create. It is expected that variables for locals are named l0 to ln, where l0 to li are parameters (there are i params), and li+1 to ln are locals that are initialized to 0.
 *)
let init (cfg : 'a Cfg.t) (vars : Var.Set.t) : t =
  rethrow_apron_error "Relational_domain.init" (fun () ->
      assert (List.length cfg.return_types <= 1); (* wasm spec does not allow for more than one return type (currently) *)
      let zeros = List.filter (Var.Set.to_list vars) ~f:(function
          | Var.Local n when n >= List.length cfg.arg_types ->
            (* Locals that are not argments are mapped to 0 *)
            true
          | _ -> false) in
      let bottoms = List.filter (Var.Set.to_list vars) ~f:(function
          | Var.Local _ | Var.Global _ ->
            (* Locals are either top (arguments) or 0 (non-arguments.
               Globals are always top *)
            false
          | _ -> true) in
      let abs = top vars in (* start with top *)
      (* Restrict some vars to bottom (i.e., forgets them) *)
      let abs' = { constraints = Apron.Abstract1.forget_array manager abs.constraints (Array.of_list (List.map bottoms ~f:(fun v -> Apron.Var.of_string (Var.to_string v)))) false }in
      (* Restricts other vars to 0 *)
      let abs'' = add_constraints abs' (List.map zeros ~f:(fun v -> (v, "0"))) in
      abs'')

let%test_unit "init should not result in an error" =
  let module_ = Wasm_module.of_string "(module
  (type (;0;) (func (param i32) (result i32)))
  (func (;test;) (type 0) (param i32) (result i32)
    local.get 0)
  (func (;test-call;) (type 0) (param i32) (result i32)
    local.get 0
    call 0)
  (table (;0;) 1 1 funcref)
  (memory (;0;) 2)
  (global (;0;) (mut i32) (i32.const 66560)))" in
  let cfg0 = Cfg_builder.build 0l module_ in
  let cfg1 = Cfg_builder.build 1l module_ in
  let _: t = init cfg0 (Var.Set.of_list [Var.Local 0; Var.Return; Var.Global 0]) in
  let _: t = init cfg1 (Var.Set.of_list [Var.Local 0; Var.Return; Var.Global 0]) in
  ()
