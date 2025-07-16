(** Abstract Store Domain

    This module defines an abstract domain that models the mapping of program variables
    (including memory locations) to abstract values, represented as Reduced Interval Congruences (RICs).
    It supports operations typical of abstract interpretation frameworks, such as join, meet,
    widening, weak updates, and memory truncation.
*)
open Core
open Reduced_interval_congruence 

module Value = struct
  type t =
    | ValueSet of RIC.t
    | Boolean of Boolean.t
  [@@deriving sexp, compare, equal]

  let to_string (value : t) : string =
    match value with
    | ValueSet vs -> RIC.to_string vs
    | Boolean b -> Boolean.to_string b

  let join (value1 : t) (value2 : t) : t =
    match value1, value2 with
    | ValueSet vs1, ValueSet vs2 -> ValueSet (RIC.join vs1 vs2)
    | Boolean b1, Boolean b2 -> Boolean (Boolean.join b1 b2)
    | _ -> assert false

  let meet (value1 : t) (value2 : t) : t =
    match value1, value2 with
    | ValueSet vs1, ValueSet vs2 -> ValueSet (RIC.meet vs1 vs2)
    | Boolean b1, Boolean b2 -> Boolean (Boolean.meet b1 b2)
    | _ -> assert false

  let update_relative_offset (v : t) (actual_values : RIC.t String.Map.t) : t =
    match v with
    | Boolean _ -> v
    | ValueSet vs -> ValueSet (RIC.update_relative_offset ~ric_:vs ~actual_values)
end

(** Type [t] is the abstract store: a map from variables to their abstract values. *)
(* type t = RIC.t Variable.Map.t *)
type t = Value.t Variable.Map.t
[@@deriving sexp, compare, equal]

(** [top] is a placeholder for the top store. Currently modeled as an empty map. *)
let top : t = Variable.Map.empty (* TODO: better definition of TOP *)

(** [extract_memory_variables store] returns all memory-related variables in [store]. *)
let extract_memory_variables (store : t) : Variable.t list =
  Variable.Map.extract_memory_variables store

let extract_locals_and_globals (store : t) : Variable.t list =
  Variable.Map.extract_locals_and_globals store

(** [get store ~var] returns the abstract value (RIC) associated with [var] in [store].

    - If [var] is found, returns its value.
    - If [var] is a constant, returns the appropriate singleton RIC.
    - If [var] is symbolic, returns a relative RIC.
    - If [var] is memory not in the store, returns the join of overlapping memory variables if covered.
    - If uncovered, returns [RIC.Top].
*)
let get (store : t) ~(var : Variable.t) : Value.t =
  match Variable.Map.find store var with
    | Some value -> value
    | None ->
      ValueSet 
        (match var with
        | Affected -> RIC.Bottom
        | Var Const I32 n -> Reduced_interval_congruence.RIC.ric (0, Int 0, Int 0, ("", Option.value_exn (Int32.to_int n)))
        | Var Const _ -> Reduced_interval_congruence.RIC.Top
        (* | Var Merge _ -> RIC.Bottom *)
        | Var _ -> RIC.Bottom
        (* | Var _ -> RIC.relative_ric (Variable.to_string var) *)
        | Mem _ ->
          let mems = extract_memory_variables store in
          if Variable.is_covered ~by:mems var then
            List.fold 
              ~init:RIC.Bottom
              ~f:(fun acc m -> 
                if Variable.share_addresses m var then
                  match (Option.value_exn (Variable.Map.find store m)) with
                  | ValueSet vs -> RIC.join acc vs
                  | Boolean _ -> RIC.Top (* TODO: Assert false? *)
                else if Variable.comparable_offsets m var then
                  acc
                else
                  RIC.Top)
              mems
          else
            RIC.Top)

let extract_global_values (store : t) : RIC.t String.Map.t =
  let globals = 
    List.filter 
      (Variable.Map.extract_locals_and_globals store)
      ~f:(fun var -> 
        match var with
        | Variable.Var Var.Global _ -> true
        | _ -> false) in
  let global_values =
    List.map globals ~f:(fun var -> 
      Variable.to_string var,
      match get store ~var with
      | Value.ValueSet vs -> vs
      | Value.Boolean _ -> RIC.Top) in (* TODO: if not i32, shouldn't it be Top? *)
  List.fold
    global_values
    ~init:String.Map.empty
    ~f:(fun acc (key, data) -> Map.set acc ~key ~data)

let extract_argument_values (store : t) ~(args : Var.t list) : RIC.t String.Map.t =
  let values = 
    List.map 
      args 
      ~f:(fun var -> 
        match get store ~var:(Variable.Var var) with
        | Value.ValueSet vs -> vs
        | Value.Boolean _ -> RIC.Top) in
  List.fold2_exn
    (List.init (List.length args) ~f:(fun i -> Var.to_string (Var.Local i)))
    values
    ~init:String.Map.empty
    ~f:(fun acc key data -> Map.set acc ~key ~data)

(** [subsumes t1 t2] returns true if [t1] over-approximates [t2] for every variable. TODO: rethink this function! *)
let subsumes (t1 : t) (t2 : t) : bool =
  (* For each variable in t2, its RIC must be subsumed by its RIC in t1 *)
  Variable.Map.fold t2 ~init:true ~f:(fun ~key:k ~data:v2 sub ->
    if sub then
      if Value.equal v2 (Value.ValueSet RIC.Bottom) then
        true (* v2 is bottom, so any value in t1[k] would subsume it, no need to check *)
      else 
        match Variable.Map.find t1 k, v2 with
        | Some ValueSet v1, ValueSet v2 -> RIC.subsumes v1 v2 
        | _ -> false (* true?? *)
    else 
      false)

(** [to_string store] converts the abstract store to a string, showing all variable bindings. *)
let to_string (vs : t) : string =
  Printf.sprintf "[%s]" (String.concat ~sep:"; "
                          (List.map (Variable.Map.to_alist vs)
                            ~f:(fun (k, t) ->
                                Printf.sprintf "%s ↦ %s"
                                  (Variable.to_string k)
                                  (Value.to_string t)))) (* TODO: maybe no need to print booleans in the end to save space? *)

(** [to_string_without_bottoms store] returns a string that omits any variables mapped to [RIC.Bottom]. *)
let to_string_without_bottoms (vs : t) : string =
  let restricted = Variable.Map.filter vs ~f:(fun d ->
    match d with
    | Boolean _ -> true
    | ValueSet d -> not (RIC.equal RIC.Bottom d)) in
  to_string restricted

(** [update_all store vars ric] sets [ric] for each variable in [vars] within [store]. *)
let update_all (store : t) (vars : Variable.Set.t) (new_value : Value.t) : t =
  Variable.Map.update_all store vars new_value

(** [make_compatible ~this_store ~relative_to] splits memory variables in [this_store] 
    so they align with memory structure in [relative_to]. *)
let make_compatible ~(this_store : t) ~(relative_to : t) : t = 
  Variable.Map.make_compatible ~this:this_store ~relative_to ~get

(** [meet store1 store2] computes the greatest lower bound of two stores. *)
let meet (store1 : t) (store2 : t) : t =
  Variable.Map.merge store1 store2 ~f:(fun ~key:var value ->
    match value with 
    | `Both (ValueSet x, ValueSet y) -> Some (Value.ValueSet (RIC.meet x y))
    | `Both (Boolean x, Boolean y) -> Some (Value.Boolean (Boolean.meet x y))
    | `Both _ -> None (* TODO: make sure this is sound *)
    | `Left ValueSet _ | `Right ValueSet _ ->
      Some (
        match (get store1 ~var), (get store2 ~var) with
        | ValueSet a, ValueSet b -> ValueSet (RIC.meet a b)
        | _ -> assert false)
    | `Left Boolean _ | `Right Boolean _ -> None
  )

(** [widen store1 store2] widens [store2] with respect to [store1] to accelerate fixpoint computation. *)
let widen (store1 : t) (store2 : t) : t =
  let store1 = make_compatible ~this_store:store1 ~relative_to:store2 in
  let store2 = make_compatible ~this_store:store2 ~relative_to:store1 in
  Variable.Map.merge store2 store1 ~f:(fun ~key:k v ->
    match k, v with
    | _, `Both (ValueSet x, ValueSet y) -> Some (Value.ValueSet (RIC.widen y ~relative_to:x))
    | _, `Both _ -> Some (Value.ValueSet RIC.Top)
    | Mem _, `Right ValueSet y -> Some (Value.ValueSet (RIC.widen y ~relative_to:RIC.Top))
    | Var _, `Right ValueSet y -> Some (Value.ValueSet (RIC.widen y ~relative_to:RIC.Bottom))
    | Mem _, `Left ValueSet x -> Some (Value.ValueSet (RIC.widen RIC.Top ~relative_to:x))
    | Var _, `Left ValueSet x -> Some (Value.ValueSet (RIC.widen RIC.Bottom ~relative_to:x))
    | _ -> Some (Value.ValueSet RIC.Top))

(** [to_top_RIC store var] sets [var] to [RIC.Top] in [store]. *)
let to_top_RIC (store : t) (var : Variable.t) : t =
  Variable.Map.set store ~key:var ~data:(Value.ValueSet RIC.Top)

(** [to_top_RICs store vars] sets each variable in [vars] to [RIC.Top] in [store]. *)
let to_top_RICs (store : t) (vars : Variable.Set.t) : t =
  Variable.Set.fold vars ~init:store ~f:to_top_RIC

(** [to_bottom_RIC store var] sets [var] to [RIC.Bottom] in [store]. *)
let to_bottom_RIC (store : t) (var : Variable.t) : t =
  Variable.Map.set store ~key:var ~data:(Value.ValueSet RIC.Bottom)

(** [to_bottom_RICs store vars] sets each variable in [vars] to [RIC.Bottom] in [store]. *)
let to_bottom_RICs (store : t) (vars : Variable.Set.t) : t =
  Variable.Set.fold vars ~init:store ~f:to_bottom_RIC

let filter_relative_offsets (store : t) (rel_offset : string) : t =
  Variable.Map.filteri store
    ~f:(fun ~key:var ~data:vs ->
      match var, vs with
      | Mem RIC {offset = (v, _); _}, _ -> String.equal v rel_offset
      | _ -> true)

(** [truncate_memory_var store ~var ~accessed_addresses] removes accessed addresses from [var]'s region. 
    The untouched portion is preserved in the store. *)
let truncate_memory_var (store : t) ~(var : Variable.t) ~(accessed_addresses : RIC.accessed) : t =
  let vs = get store ~var:var in
  let store = Variable.Map.remove store var in
  (* if not (Variable.equal var Variable.entire_memory) then *)
  let accessed = accessed_addresses.fully :: accessed_addresses.partially in
  let untouched_addresses =
    match var with
    | Var _ | Affected -> assert false
    | Mem addr ->
      (List.fold ~init:[addr]
                ~f:(fun acc x -> List.concat (List.map ~f:(fun y -> RIC.remove ~this:x ~from:y) acc))
                accessed)
  in
  let untouched_variables = Variable.Set.of_list (List.map ~f:(fun x -> Variable.Mem x) untouched_addresses) in
  update_all store untouched_variables vs
  (* else *)
    (* store *)

(** [set store ~var ~vs] updates [var] in [store] to [vs]. Fails if [var] overlaps with existing memory vars. *)
let set (store : t) ~(var : Variable.t) ~(vs : Value.t) : t =
  let store = 
    if Variable.is_linear_memory var then 
        let store = make_compatible ~this_store:store ~relative_to:(Variable.Map.empty |> Variable.Map.set ~key:var ~data:vs) in
        Variable.Map.filter_keys ~f:(fun v -> not (Variable.share_addresses v var)) store
        (* Variable.Map.filter ~f:(fun value_set -> not (Value.equal (ValueSet RIC.Top) value_set)) store *)
    else 
      store
  in
  let is_valid =
    Variable.Map.fold store ~init:true ~f:(fun ~key:k ~data:_ acc ->
      acc && 
      match k, var with
      | Var _, _ | _, Var _  | Affected, _ | _, Affected -> true
      | Mem _, Mem _ ->
        Variable.equal k var ||
        not (Variable.share_addresses k var)) in
  if not is_valid then
    failwith "error: trying to update a memory variable that overlaps with other memory variables"
  else
    let store = if Variable.is_linear_memory var then
      filter_relative_offsets store (Variable.get_relative_offset var)
    else
      store
    in
    Variable.Map.set store ~key:var ~data:vs

(** [bottom] is the least informative abstract store, mapping no variables. *)
let bottom : t = Variable.Map.empty  |> set ~var:Variable.entire_memory ~vs:(ValueSet RIC.Bottom)

let remove_pointers_to_top (store : t) : t =
  let store =
    Variable.Map.filteri store ~f:(fun ~key ~data -> 
      (not (Variable.is_linear_memory key)) || (not (Value.equal (ValueSet RIC.Top) data))) in
  if List.is_empty (extract_memory_variables store) then
    set store ~var:Variable.entire_memory ~vs:(ValueSet RIC.Top)
  else
    store

(** [join store1 store2] computes the least upper bound of two stores. 
    Missing memory variables are treated as [RIC.Top]. *)
let join (store1 : t) (store2 : t) : t =
  if equal store1 bottom then store2
  else if equal store2 bottom then store1
  else
  let store1 = make_compatible ~this_store:store1 ~relative_to:store2 in
  let store2 = make_compatible ~this_store:store2 ~relative_to:store1 in
  (* print_endline ("joining these two states: " ^ to_string store1 ^ "  and  " ^ to_string store2); *)
  let store =
    Variable.Map.merge store1 store2 ~f:(fun ~key:var v -> 
        match v with
        | `Both (ValueSet x, ValueSet y) -> Some (Value.ValueSet (RIC.join x y))
        | `Both (Boolean x, Boolean y) -> Some (Value.Boolean (Boolean.join x y))
        | `Both _ | `Left Boolean _ | `Right Boolean _ -> Some (Value.ValueSet RIC.Top)
        | `Left ValueSet _ | `Right ValueSet _ ->
          Some (
            match (get store1 ~var), (get store2 ~var) with
            | ValueSet a, ValueSet b -> ValueSet (RIC.join a b)
            | _ -> assert false))
  in
  (* print_endline ("result: " ^ to_string store); *)
  let store = remove_pointers_to_top store in
  store

(** [update_accessed_vars store accessed_addresses] applies [truncate_memory_var] to all memory variables. *)
let update_accessed_vars (store : t) (accessed_addresses : RIC.accessed) : t =
  let memory_vars = extract_memory_variables store in
  let store = 
    List.fold 
      ~init:store 
      ~f:(fun store var -> truncate_memory_var store ~var:var ~accessed_addresses:accessed_addresses) memory_vars 
  in
  if List.is_empty (extract_memory_variables store) then 
    set store ~var:Variable.entire_memory ~vs:(ValueSet RIC.Top)
  else
    store

(** [weak_update store ~previous_state ~var ~vs] weakly updates [store] at [var] by joining [vs] with 
    the value in [previous_state], preserving values for unaffected regions. *)
let weak_update (store : t) ~(previous_state : t) ~(var : Variable.t) ~(vs : RIC.t) : t =
  let address = 
    match var with
    | Variable.Var _ | Affected -> assert false
    | Variable.Mem address -> address
  in
  let memory_variables = extract_memory_variables previous_state in
  let affected_variables = 
    List.filter ~f:(fun (v, _) -> not (Variable.equal v (Mem RIC.Bottom)))
      (List.map ~f:(fun v -> 
          match v with 
          | Variable.Var _ | Affected -> assert false
          | Variable.Mem addr -> (Variable.Mem (RIC.meet addr address)), (get previous_state ~var:v))
        memory_variables)
  in
  let store = List.fold ~init:store
            ~f:(fun store (v, prev_vs) -> 
              let vs = 
                match prev_vs with
                | Boolean _ -> Value.ValueSet RIC.Top
                | ValueSet prev_vs -> ValueSet (RIC.join prev_vs vs)
              in
              set store ~var:v ~vs)
            affected_variables
  in
  if List.is_empty (extract_memory_variables store) then
    set store ~var:Variable.entire_memory ~vs:(ValueSet RIC.Top)
  else
    store

(** [assign_constant_value store ~const ~to_] sets [to_] to the singleton RIC containing [const]. *)
let assign_constant_value (store : t) ~(const : int32) ~(to_ : Variable.t): t =
  let vs = RIC.ric (0, Int 0, Int 0, ("", Option.value_exn (Int32.to_int const))) in
  (*Variable.Map.*)set store ~var:to_ ~vs:(ValueSet vs)

(** [copy_value_set store ~from ~to_] copies the value from [from] to [to_]. 
    Does nothing if [to_] is a constant. *)
let copy_value_set (store : t) ~(from : Variable.t) ~(to_ : Variable.t) : t =
  match to_ with
  | Var Var.Const _ -> store
  | _ ->
    let vs = get store ~var:from in
    set store ~var:to_ ~vs:vs

(** [i32_add store ~x ~y ~result] performs addition of [x] and [y], stores result in [result]. *)
let i32_add (store : t) ~(x : Variable.t) ~(y : Variable.t) ~(result : Variable.t) : t =
  let vs =
    Value.ValueSet (
      match x, y with
      | Var Var.Const Prim_value.I32 x, Var Var.Const Prim_value.I32 y -> 
        let x = Option.value_exn (Int32.to_int x) in
        let y = Option.value_exn (Int32.to_int y) in
        RIC.ric (0, Int 0, Int 0, ("", x + y))
      | Var Var.Const Prim_value.I32 x, Var y -> 
        (let x = Option.value_exn (Int32.to_int x) in
        let vs_y = get store ~var:(Variable.Var y) in
        match vs_y with
        | ValueSet vs_y -> RIC.add_offset vs_y x
        | Boolean _ -> RIC.Top)
      | Var x, Var Var.Const Prim_value.I32 y -> 
        (let y = Option.value_exn (Int32.to_int y) in
        let vs_x = get store ~var:(Variable.Var x) in
        match vs_x with
        | ValueSet vs_x -> RIC.add_offset vs_x y
        | Boolean _ -> RIC.Top)
      | Var x, Var y ->
        (let vs_x = get store ~var:(Variable.Var x) in
        let vs_y = get store ~var:(Variable.Var y) in
        match vs_x, vs_y with
        | ValueSet vs_x, ValueSet vs_y -> RIC.plus vs_x vs_y
        | _ -> RIC.Top)
      | _ -> failwith "error")
  in
  set store ~var:result ~vs:vs

(** [i32_sub store ~x ~y ~result] performs subtraction [y - x], stores result in [result]. *)
let i32_sub (store : t) ~(x : Variable.t) ~(y : Variable.t) ~(result : Variable.t) : t =
  let vs =
    Value.ValueSet (
      match x, y with
      | Var Var.Const Prim_value.I32 x, Var Var.Const Prim_value.I32 y -> 
        let x = Option.value_exn (Int32.to_int x) in
        let y = Option.value_exn (Int32.to_int y) in
        RIC.ric (0, Int 0, Int 0, ("", y - x))
      | Var Var.Const Prim_value.I32 x, Var y -> 
        (let x = - (Option.value_exn (Int32.to_int x)) in
        let vs_y = get store ~var:(Variable.Var y) in
        match vs_y with
        | ValueSet vs_y -> RIC.add_offset vs_y x
        | _ -> RIC.Top)
      | Var x, Var Var.Const Prim_value.I32 y -> 
        (let y = Option.value_exn (Int32.to_int y) in
        let vs_x = get store ~var:(Variable.Var x) in
        match vs_x with
        | ValueSet vs_x -> RIC.add_offset (RIC.negative vs_x) y
        | _ -> RIC.Top)
      | Var x, Var y ->
        (let vs_x = get store ~var:(Variable.Var x) in
        let vs_y = get store ~var:(Variable.Var y) in
        match vs_x, vs_y with
        | ValueSet vs_x, ValueSet vs_y ->RIC.plus (RIC.negative vs_x) vs_y
        | _ -> RIC.Top)
      | _ -> failwith "error")
  in
  set store ~var:result ~vs:vs

(** [v1_equals_v2_plus_c store ~v1 ~v2 ~c] sets [v1] to [v2 + c] in the abstract store. *)
let v1_equals_v2_plus_c (store : t) ~(v1 : Variable.t) ~(v2 : Variable.t) ~(c : int) : t =
  let vs2 = get store ~var:v2 in
  let vs1 = 
    match vs2 with
    | ValueSet vs2 -> Value.ValueSet (RIC.add_offset vs2 c)
    | _ -> if c = 0 then vs2 else ValueSet RIC.Top in
  set store ~var:v1 ~vs:vs1


let affect_memory (store : t) ~(addresses : Value.t) : t =
  (match addresses with
  | Value.Boolean _ -> assert false
  | _ -> ());
  let previously_affected = get store ~var:(Variable.Affected) in
  (match previously_affected with
  | Value.Boolean _ -> assert false
  | _ -> ());
  let new_affected_memory = Value.join previously_affected (addresses) in
  Variable.Map.set store ~key:(Variable.Affected) ~data:new_affected_memory











(*
TTTTTTTTTTTTTTTTTTTTTTTEEEEEEEEEEEEEEEEEEEEEE   SSSSSSSSSSSSSSS TTTTTTTTTTTTTTTTTTTTTTT   SSSSSSSSSSSSSSS 
T:::::::::::::::::::::TE::::::::::::::::::::E SS:::::::::::::::ST:::::::::::::::::::::T SS:::::::::::::::S
T:::::::::::::::::::::TE::::::::::::::::::::ES:::::SSSSSS::::::ST:::::::::::::::::::::TS:::::SSSSSS::::::S
T:::::TT:::::::TT:::::TEE::::::EEEEEEEEE::::ES:::::S     SSSSSSST:::::TT:::::::TT:::::TS:::::S     SSSSSSS
TTTTTT  T:::::T  TTTTTT  E:::::E       EEEEEES:::::S            TTTTTT  T:::::T  TTTTTTS:::::S            
        T:::::T          E:::::E             S:::::S                    T:::::T        S:::::S            
        T:::::T          E::::::EEEEEEEEEE    S::::SSSS                 T:::::T         S::::SSSS         
        T:::::T          E:::::::::::::::E     SS::::::SSSSS            T:::::T          SS::::::SSSSS    
        T:::::T          E:::::::::::::::E       SSS::::::::SS          T:::::T            SSS::::::::SS  
        T:::::T          E::::::EEEEEEEEEE          SSSSSS::::S         T:::::T               SSSSSS::::S 
        T:::::T          E:::::E                         S:::::S        T:::::T                    S:::::S
        T:::::T          E:::::E       EEEEEE            S:::::S        T:::::T                    S:::::S
      TT:::::::TT      EE::::::EEEEEEEE:::::ESSSSSSS     S:::::S      TT:::::::TT      SSSSSSS     S:::::S
      T:::::::::T      E::::::::::::::::::::ES::::::SSSSSS:::::S      T:::::::::T      S::::::SSSSSS:::::S
      T:::::::::T      E::::::::::::::::::::ES:::::::::::::::SS       T:::::::::T      S:::::::::::::::SS 
      TTTTTTTTTTT      EEEEEEEEEEEEEEEEEEEEEE SSSSSSSSSSSSSSS         TTTTTTTTTTT       SSSSSSSSSSSSSSS   
*)


let%test_module "abstract store tests" = (module struct
  let%test "Abstract_store_tests" =
    print_endline "_______ _____________________ _______\n        Abstract Store Domain        \n------- --------------------- -------\n";
    true
  
  let var1 = Variable.Var (Var.Global 0)
  let var2 = Variable.Var (Var.Local 0)
  let var3 = Variable.mem (0, Int 0, Int 0, ("", 4))

  let vs =
    Variable.Map.empty
    |> set ~var:var1 ~vs:(Value.ValueSet (RIC.ric (2, Int 0, Int 3, ("", 4))))
    |> set ~var:var2 ~vs:(Value.ValueSet (RIC.Bottom))
    |> set ~var:var3 ~vs:(Value.ValueSet (RIC.Top))

  let%test "to_string includes all bindings" =
    let s = to_string vs in
    print_endline ("[to_string]\n\t" ^ s); true

  let%test "to_string_without_bottoms omits Bottoms" =
    let s = to_string_without_bottoms vs in
    print_endline ("[to_string_without_bottoms]\n\t" ^ s); true

  let%test "join abstract stores" =
    let var1 = Variable.Var (Var.Global 0) in
    let var2 = Variable.Var (Var.Local 0) in
    let store1 =
      Variable.Map.empty
      |> Variable.Map.set ~key:var1 ~data:(Value.ValueSet (RIC.ric (2, Int 0, Int 2, ("", 0))))
      |> Variable.Map.set ~key:var2 ~data:(Value.ValueSet (RIC.ric (3, Int 1, Int 3, ("", 0))))
    in
    let store2 =
      Variable.Map.empty
      |> Variable.Map.set ~key:var1 ~data:(Value.ValueSet (RIC.ric (2, Int 1, Int 4, ("", 0))))
      |> Variable.Map.set ~key:var2 ~data:(Value.ValueSet (RIC.ric (3, Int 2, Int 5, ("", 0))))
    in
    let joined = join store1 store2 in
    let expected =
      Variable.Map.empty
      |> Variable.Map.set ~key:var1 ~data:(Value.ValueSet (RIC.ric (2, Int 0, Int 4, ("", 0))))
      |> Variable.Map.set ~key:var2 ~data:(Value.ValueSet (RIC.ric (3, Int 1, Int 5, ("", 0))))
      |> set ~var:Variable.entire_memory ~vs:(Value.ValueSet (RIC.Top))
    in
    print_endline ("[JOIN]\n\t"  ^ to_string store1 ^ "\n\t" ^ to_string store2 ^ "\n\t\t" ^ to_string joined);
    equal joined expected

  let%test "join abstract stores 2" =
    let var1 = Variable.Var (Var.Local 0) in
    let var2 = Variable.entire_memory in
    let store1 =
      Variable.Map.empty
      |> Variable.Map.set ~key:var1 ~data:(Value.ValueSet (RIC.ric (0, Int 0, Int 0, ("", 3))))
      |> Variable.Map.set ~key:var2 ~data:(Value.ValueSet RIC.Top)
    in
    let store2 =
      Variable.Map.empty
      |> Variable.Map.set ~key:var2 ~data:(Value.ValueSet RIC.Bottom)
    in
    let joined = join store1 store2 in
    let expected =
      Variable.Map.empty
      |> Variable.Map.set ~key:var1 ~data:(Value.ValueSet (RIC.ric (0, Int 0, Int 0, ("", 3))))
      |> Variable.Map.set ~key:var2 ~data:(Value.ValueSet RIC.Top)
    in
    print_endline ("[JOIN]\n\t"  ^ to_string store1 ^ "\n\t" ^ to_string store2 ^ "\n\t\t" ^ to_string joined);
    equal joined expected

  let%test "join abstract stores 3" =
    let var1 = Variable.Var (Var.Local 0) in
    let var2 = Variable.Var (Var.Local 3) in
    let store1 =
      Variable.Map.empty
      |> Variable.Map.set ~key:var1 ~data:(Value.ValueSet (RIC.ric (0, Int 0, Int 0, ("", 3))))
      |> Variable.Map.set ~key:var2 ~data:(Value.ValueSet (RIC.ric (0, Int 0, Int 0, ("", 6))))
    in
    let store2 =
      Variable.Map.empty
      |> Variable.Map.set ~key:var2 ~data:(Value.ValueSet RIC.Bottom)
    in
    let joined = join store1 store2 in
    let expected =
      Variable.Map.empty
      |> Variable.Map.set ~key:var1 ~data:(Value.ValueSet (RIC.ric (0, Int 0, Int 0, ("", 3))))
      |> Variable.Map.set ~key:var2 ~data:(Value.ValueSet (RIC.ric (0, Int 0, Int 0, ("", 6))))
      |> Variable.Map.set ~key:Variable.entire_memory ~data:(Value.ValueSet RIC.Top)
    in
    print_endline ("[JOIN]\n\t"  ^ to_string store1 ^ "\n\t" ^ to_string store2 ^ "\n\t\t" ^ to_string joined);
    equal joined expected

  let%test "meet abstract stores" =
    let var1 = Variable.Var (Var.Global 0) in
    let var2 = Variable.Var (Var.Local 0) in
    let store1 =
      Variable.Map.empty
      |> Variable.Map.set ~key:var1 ~data:(Value.ValueSet (RIC.ric (2, Int 0, Int 4, ("", 0))))
      |> Variable.Map.set ~key:var2 ~data:(Value.ValueSet (RIC.ric (3, Int 1, Int 5, ("", 0))))
    in
    let store2 =
      Variable.Map.empty
      |> Variable.Map.set ~key:var1 ~data:(Value.ValueSet (RIC.ric (2, Int 2, Int 6, ("", 0))))
      |> Variable.Map.set ~key:var2 ~data:(Value.ValueSet (RIC.ric (3, Int 0, Int 2, ("", 0))))
    in
    let met = meet store1 store2 in
    let expected =
      Variable.Map.empty
      |> Variable.Map.set ~key:var1 ~data:(Value.ValueSet (RIC.ric (2, Int 2, Int 4, ("", 0))))
      |> Variable.Map.set ~key:var2 ~data:(Value.ValueSet (RIC.ric (3, Int 1, Int 2, ("", 0))))
    in
    print_endline ("[MEET]\n\t"  ^ to_string store1 ^ "\n\t" ^ to_string store2 ^ "\n\t\t" ^ to_string met);
    equal met expected

  let%test "truncate memory var retains untouched regions" =
    let addr = RIC.ric (1, Int 0, Int 10, ("", 0)) in
    let mem_var = Variable.Mem addr in
    let vs = Value.ValueSet (RIC.ric (1, Int 0, Int 1, ("", 0))) in
    print_endline "[truncate_memory_var]";
    print_endline ("\tvariable to truncate: " ^ Variable.to_string mem_var);
    let store = Variable.Map.singleton mem_var vs in
    print_endline ("\tinitial store: " ^ to_string store);
    let accessed = { RIC.fully = RIC.ric (1, Int 1, Int 2, ("", 0)); partially = [] } in
    print_endline ("\taccessed addresses: " ^ RIC.to_string accessed.fully);
    let truncated = truncate_memory_var store ~var:mem_var ~accessed_addresses:accessed in
    print_endline ("\ttruncated new store: " ^ to_string truncated);
    let expected = set (Variable.Map.singleton (Variable.mem (0, Int 0, Int 0, ("", 0))) vs) ~var:(Variable.mem (1, Int 3, Int 10, ("", 0))) ~vs in
    equal expected truncated

  let%test "truncate memory 2" =
    let addr = RIC.ric (1, Int 0, Int 10, ("", 0)) in
    let mem_var = Variable.Mem addr in
    let vs = Value.ValueSet (RIC.ric (1, Int 0, Int 1, ("", 0))) in
    print_endline "[truncate_memory_var]";
    print_endline ("\tvariable to truncate: " ^ Variable.to_string mem_var);
    let store = Variable.Map.singleton mem_var vs in
    print_endline ("\tinitial store: " ^ to_string store);
    let accessed = { RIC.fully = RIC.ric (1, Int 1, Int 2, ("", 100)); partially = [] } in
    print_endline ("\taccessed addresses: " ^ RIC.to_string accessed.fully);
    let truncated = truncate_memory_var store ~var:mem_var ~accessed_addresses:accessed in
    print_endline ("\ttruncated new store: " ^ to_string truncated);
    equal store truncated


  let%test "weak_update" =
    let addr1 = RIC.ric (1, Int 0, Int 1, ("", 0)) in
    let addr2 = RIC.ric (1, Int 3, Int 4, ("", 0)) in
    let mem1 = Variable.Mem addr1 in
    let mem2 = Variable.Mem addr2 in
    let prev_state =
      Variable.Map.empty
      |> Variable.Map.set ~key:mem1 ~data:(Value.ValueSet (RIC.ric (0, Int 0, Int 4, ("", 42))))
      |> Variable.Map.set ~key:mem2 ~data:(Value.ValueSet (RIC.ric (0, Int 2, Int 6, ("", 36))))
    in
    print_endline "[weak update]";
    print_endline ("\tprevious state: " ^ to_string prev_state);
    let store = Variable.Map.empty in
    let new_val = RIC.ric (0, Int 2, Int 6, ("", 0)) in
    print_endline ("\tnew value-set: " ^ RIC.to_string new_val);
    let new_mem = Variable.mem (1, Int 1, Int 3, ("", 0)) in
    print_endline ("\tvariable being updated: " ^ Variable.to_string new_mem);
    let updated = weak_update store ~previous_state:prev_state ~var:new_mem ~vs:new_val in
    print_endline ("\tresult of weak_update:\n\t\t" ^ to_string updated);
    let expected = 
      Variable.Map.empty
      |> Variable.Map.set ~key:(Variable.mem (0, Int 0, Int 0, ("", 1))) ~data:(Value.ValueSet (RIC.ric (42, Int 0, Int 1, ("", 0))))
      |> Variable.Map.set ~key:(Variable.mem (0, Int 0, Int 0, ("", 3))) ~data:(Value.ValueSet (RIC.ric (36, Int 0, Int 1, ("", 0)))) 
    in
    equal expected updated

  let%test "get: get memory var in bottom state" =
    let var = Variable.mem (1, Int 0, Int 2, ("", 0)) in
    let store = bottom
    in
    let result = get store ~var:var in
    print_endline ("[get]\n\tinitial store: " ^ to_string store ^ "\n\tvariable to search for: " ^ Variable.to_string var
      ^ "\n\textracted value-set: " ^ Value.to_string result);
    Value.equal result (Value.ValueSet RIC.Bottom)

  let%test "get: variable not in store but covered by two others" =
    let var1 = Variable.mem (1, Int 0, Int 2, ("", 0)) in
    let var2 = Variable.mem (1, Int 3, Int 5, ("", 0)) in
    let target = Variable.mem (1, Int 2, Int 3, ("", 0)) in
    let vs1 = RIC.ric (1, Int 0, Int 2, ("", 0)) in
    let vs1' = Value.ValueSet vs1 in
    let vs2 = RIC.ric (2, Int 3, Int 5, ("", 0)) in
    let vs2' = Value.ValueSet vs2 in
    let store =
      Variable.Map.empty
      |> set ~var:var1 ~vs:vs1'
      |> set ~var:var2 ~vs:vs2'
    in
    let result = get store ~var:target in
    print_endline ("[get]\n\tinitial store: " ^ to_string store ^ "\n\tvariable to search for: " ^ Variable.to_string target
      ^ "\n\textracted value-set: " ^ Value.to_string result);
    Value.equal result (Value.ValueSet (RIC.join vs1 vs2))

  let%test "get: variable not in store and not covered by others" =
    let var1 = Variable.mem (1, Int 0, Int 2, ("", 0)) in
    let var2 = Variable.mem (1, Int 4, Int 5, ("", 0)) in
    let target = Variable.mem (1, Int 2, Int 3, ("", 0)) in
    let vs1 = RIC.ric (1, Int 0, Int 2, ("", 0)) in
    let vs1' = Value.ValueSet (vs1) in
    let vs2 = RIC.ric (2, Int 3, Int 5, ("", 0)) in
    let vs2' = Value.ValueSet (vs2) in
    let store =
      Variable.Map.empty
      |> set ~var:var1 ~vs:vs1'
      |> set ~var:var2 ~vs:vs2'
    in
    let result = get store ~var:target in
    print_endline ("[get]\n\tinitial store: " ^ to_string store ^ "\n\tvariable to search for: " ^ Variable.to_string target
      ^ "\n\textracted value-set: " ^ Value.to_string result);
    Value.equal result (Value.ValueSet RIC.Top)

  let%test "make two stores compatible to join" =
    let m1 = Variable.Mem (RIC.ric (1, Int 1, Int 4, ("", 0))) in
    let vs1 = RIC.ric (0, Int 0, Int 0, ("", 42)) in
    let m2 = Variable.Mem (RIC.ric (2, Int 0, Int 4, ("", 0))) in
    let vs2 = RIC.ric (0, Int 0, Int 0, ("", 36)) in
    let store1 = set top ~var:m1 ~vs:(Value.ValueSet vs1) in
    let store2 = set top ~var:m2 ~vs:(Value.ValueSet vs2) in
    print_endline "[make_compatible]";
    print_endline ("\tstore1: " ^ to_string store1);
    print_endline ("\tstore2: " ^ to_string store2);
    let store1 = make_compatible ~this_store:store1 ~relative_to:store2 in
    let store2 = make_compatible ~this_store:store2 ~relative_to:store1 in
    print_endline ("\tcompatible store1: " ^ to_string store1);
    print_endline ("\tcompatible store2: " ^ to_string store2);
    print_endline ("\tjoin: " ^ to_string (join store1 store2));
    let expected = Variable.Map.empty
      |> set ~var:(Variable.Mem (RIC.ric (2, Int 0, Int 1, ("", 2)))) ~vs:(Value.ValueSet (RIC.ric (6, Int 0, Int 1, ("", 36)))) in
    equal (join store1 store2) expected

  let%test "join: Mem variables with incompatible offsets" =
    let mem1 = Variable.Mem (RIC.ric (1, Int 0, Int 4, ("stack", 0))) in
    let mem2 = Variable.Mem (RIC.ric (1, Int 0, Int 4, ("heap", 0))) in
    let vs1 = RIC.ric (1, Int 0, Int 4, ("stack", 0)) in
    let vs2 = RIC.ric (1, Int 0, Int 4, ("heap", 0)) in
    let store1 = Variable.Map.singleton mem1 (Value.ValueSet vs1) in
    let store2 = Variable.Map.singleton mem2 (Value.ValueSet vs2)  in
    let joined = join store1 store2 in
    print_endline "[join: incompatible Mem offsets]";
    print_endline ("\tstore1: " ^ to_string store1);
    print_endline ("\tstore2: " ^ to_string store2);
    print_endline ("\tjoined: " ^ to_string joined);
    Value.equal (Value.ValueSet RIC.Top) (get joined ~var:mem2)

  let%test "set: update Mem[(\"b\" + 0)] in presence of Mem[(\"a\" + 0)]" =
    let var_a = Variable.mem (0, Int 0, Int 0, ("a", 0)) in
    let var_b = Variable.mem (0, Int 0, Int 0, ("b", 0)) in
    let vs_a = RIC.ric (1, Int 0, Int 2, ("a", 0)) in
    let vs_b = RIC.ric (1, Int 0, Int 2, ("b", 0)) in
    let store = Variable.Map.singleton var_a (Value.ValueSet vs_a) in
    let updated_store = set store ~var:var_b ~vs:(Value.ValueSet vs_b) in
    print_endline "[set: different symbolic offsets]";
    print_endline ("\tinitial store: " ^ to_string store);
    print_endline ("\tupdating value-set of variable " ^ Variable.to_string var_b);
    print_endline ("\tupdated store: " ^ to_string updated_store);
    Variable.Map.length updated_store = 1 &&
    Value.equal (get updated_store ~var:var_a) (Value.ValueSet RIC.Top) &&
    Value.equal (get updated_store ~var:var_b) (Value.ValueSet vs_b)

  let%test "set: set memory variable in bottom state" =
    let var = Variable.mem (1, Int 0, Int 2, ("", 0)) in
    let vs = RIC.ric (1, Int 0, Int 2, ("", 0)) in
    let store = bottom |> set ~var:var ~vs:(Value.ValueSet vs) in
    print_endline ("[set: bottom state]\n\tInitial state: " ^ to_string bottom ^ "\n\tstore after setting a value in bottom store: " ^ to_string store ^ "\n\tvariable to set: " ^ Variable.to_string var
      ^ "\n\tvalue-set to set it to: " ^ RIC.to_string vs);
    true

  let%test "set: set memory variable in bottom state (relative address)" =
    let var = Variable.mem (1, Int 0, Int 2, ("a", 0)) in
    let vs = RIC.ric (1, Int 0, Int 2, ("", 0)) in
    let store = bottom |> set ~var:var ~vs:(Value.ValueSet vs) in
    print_endline ("[set: bottom state (relative address)]\n\tInitial state: " ^ to_string bottom ^ "\n\tstore after setting a value in bottom store: " ^ to_string store ^ "\n\tvariable to set: " ^ Variable.to_string var
      ^ "\n\tvalue-set to set it to: " ^ RIC.to_string vs);
    true
end)