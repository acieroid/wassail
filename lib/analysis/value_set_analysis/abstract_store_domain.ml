(** Abstract Store Domain

    This module defines an abstract domain that models the mapping of program variables
    (including memory locations) to abstract values, represented as Reduced Interval Congruences (RICs).
    It supports operations typical of abstract interpretation frameworks, such as join, meet,
    widening, weak updates, and memory truncation.
*)
open Core
open Reduced_interval_congruence 

(** Type [t] is the abstract store: a map from variables to their abstract values. *)
type t = RIC.t Variable.Map.t
[@@deriving sexp, compare, equal]

(** [bottom] is the least informative abstract store, mapping no variables. *)
let bottom : t = Variable.Map.empty

(** [top] is a placeholder for the top store. Currently modeled as an empty map. *)
let top : t = Variable.Map.empty (* TODO: better definition of TOP *)

(** [extract_memory_variables store] returns all memory-related variables in [store]. *)
let extract_memory_variables (store : t) : Variable.t list =
  let all_vars = Variable.Map.keys store in
  List.filter ~f:Variable.is_linear_memory all_vars

(** [get store ~var] returns the abstract value (RIC) associated with [var] in [store].

    - If [var] is found, returns its value.
    - If [var] is a constant, returns the appropriate singleton RIC.
    - If [var] is symbolic, returns a relative RIC.
    - If [var] is memory not in the store, returns the join of overlapping memory variables if covered.
    - If uncovered, returns [RIC.Top].
*)
let get (store : t) ~(var : Variable.t) : Reduced_interval_congruence.RIC.t =
  match Variable.Map.find store var with
    | Some vs -> vs
    | None ->
      match var with
      | Var Const I32 n -> Reduced_interval_congruence.RIC.ric (0, Int 0, Int 0, ("", Option.value_exn (Int32.to_int n)))
      | Var Const _ -> Reduced_interval_congruence.RIC.Bottom
      | Var _ -> Reduced_interval_congruence.RIC.relative_ric (Variable.to_string var)
      | Mem _ ->
        let mems = extract_memory_variables store in
        if Variable.is_covered ~by:mems var then
          List.fold 
            ~init:RIC.Bottom
            ~f:(fun acc m -> 
              if Variable.share_addresses m var then
                RIC.join acc (Option.value_exn (Variable.Map.find store m))
              else
                acc)
            mems
        else
          RIC.Top

(** [subsumes t1 t2] returns true if [t1] over-approximates [t2] for every variable. *)
let subsumes (t1 : t) (t2 : t) : bool =
  (* For each variable in t2, its RIC must be subsumed by its RIC in t1 *)
  Variable.Map.fold t2 ~init:true ~f:(fun ~key:k ~data:v2 sub ->
    if sub then
      if RIC.equal v2 RIC.Bottom then
        true (* v2 is bottom, so any value in t1[k] would subsume it, no need to check *)
      else 
        match Variable.Map.find t1 k with
        | Some v1 -> RIC.subsumes v1 v2 
        | None -> false 
    else 
      false)

(** [to_string store] converts the abstract store to a string, showing all variable bindings. *)
let to_string (vs : t) : string =
  Printf.sprintf "[%s]" (String.concat ~sep:"; "
                          (List.map (Variable.Map.to_alist vs)
                            ~f:(fun (k, t) ->
                                Printf.sprintf "%s ↦ %s"
                                  (Variable.to_string k)
                                  (RIC.to_string t))))

(** [to_string_without_bottoms store] returns a string that omits any variables mapped to [RIC.Bottom]. *)
let to_string_without_bottoms (vs : t) : string =
  let restricted = Variable.Map.filter vs ~f:(fun d ->
    not (RIC.equal RIC.Bottom d)) in
  to_string restricted

(** [update_all store vars ric] sets [ric] for each variable in [vars] within [store]. *)
let update_all (store : t) (vars : Variable.Set.t) (new_RIC : RIC.t) : t =
  Variable.Set.fold vars ~init:store ~f:(fun acc v -> Variable.Map.set acc ~key:v ~data:new_RIC)

(** [make_compatible ~this_store ~relative_to] splits memory variables in [this_store] 
    so they align with memory structure in [relative_to]. *)
let make_compatible ~(this_store : t) ~(relative_to : t) : t =
  let store1 = this_store in
  let store2 = relative_to in
  let mems1 = extract_memory_variables store1 in
  let mems2 = extract_memory_variables store2 in
  List.fold
    ~init:store1
    ~f:(fun store m2 ->
      match m2 with
      | Variable.Mem addr_m2 -> 
        List.fold
          ~init:store
          ~f:(fun store m1 ->
            match m1 with
            | Variable.Mem addr_m1 ->
              let met_addrs = RIC.meet addr_m2 addr_m1 in
              if RIC.equal RIC.Bottom met_addrs then
                store
              else
                let new_addresses = met_addrs :: (RIC.remove ~this:met_addrs ~from:addr_m1) in
                let new_mem_vars = List.map ~f:(fun addr -> Variable.Mem addr) new_addresses in
                let vs = get store ~var:m1 in
                let store = Variable.Map.remove store m1 in
                update_all store (Variable.Set.of_list new_mem_vars) vs
            | _ -> store )
          mems1
      | _ -> store)
    mems2

(** [join store1 store2] computes the least upper bound of two stores. 
    Missing memory variables are treated as [RIC.Top]. *)
let join (store1 : t) (store2 : t) : t =
  let store1 = make_compatible ~this_store:store1 ~relative_to:store2 in
  let store2 = make_compatible ~this_store:store2 ~relative_to:store1 in
  Variable.Map.merge store1 store2 ~f:(fun ~key:var v -> 
      match v with
      | `Both (x, y) -> Some (RIC.join x y)
      | `Left x | `Right x -> 
        match var with
        | Variable.Mem _ -> Some RIC.Top (* absent values are considered to be TOP *)
        | _ -> Some x)

(** [join_loop_head store1 store2] joins stores at loop head, treating [bottom] specially to preserve initialization. *)
let join_loop_head (store1 : t) (store2 : t) : t =
  match store1, store2 with
  | b, s2 when equal b bottom -> s2
  | s1, b when equal b bottom -> s1
  | _ -> join store1 store2

(** [meet store1 store2] computes the greatest lower bound of two stores. *)
let meet (store1 : t) (store2 : t) : t =
  Variable.Map.merge store1 store2 ~f:(fun ~key:var vs ->
      match vs with 
      | `Both (x, y) -> Some (RIC.meet x y)
      | `Left x | `Right x -> 
        match var with
        | Variable.Mem _ -> Some x (* vs meet TOP returns vs *)
        | _ -> Some (RIC.Bottom))

(** [widen store1 store2] widens [store2] with respect to [store1] to accelerate fixpoint computation. *)
let widen (store1 : t) (store2 : t) : t =
  Variable.Map.merge store2 store1 ~f:(fun ~key:k v ->
      match k, v with
      | _, `Both (x, y) -> Some (RIC.widen y ~relative_to:x)
      | Mem _, `Right y -> Some (RIC.widen y ~relative_to:RIC.Top)
      | Var _, `Right y -> Some (RIC.widen y ~relative_to:RIC.Bottom)
      | Mem _, `Left x -> Some (RIC.widen RIC.Top ~relative_to:x)
      | Var _, `Left x -> Some (RIC.widen RIC.Bottom ~relative_to:x))

(** [to_top_RIC store var] sets [var] to [RIC.Top] in [store]. *)
let to_top_RIC (store : t) (var : Variable.t) : t =
  Variable.Map.set store ~key:var ~data:RIC.Top

(** [to_top_RICs store vars] sets each variable in [vars] to [RIC.Top] in [store]. *)
let to_top_RICs (store : t) (vars : Variable.Set.t) : t =
  Variable.Set.fold vars ~init:store ~f:to_top_RIC

(** [to_bottom_RIC store var] sets [var] to [RIC.Bottom] in [store]. *)
let to_bottom_RIC (store : t) (var : Variable.t) : t =
  Variable.Map.set store ~key:var ~data:RIC.Bottom

(** [to_bottom_RICs store vars] sets each variable in [vars] to [RIC.Bottom] in [store]. *)
let to_bottom_RICs (store : t) (vars : Variable.Set.t) : t =
  Variable.Set.fold vars ~init:store ~f:to_bottom_RIC

(** [set store ~var ~vs] updates [var] in [store] to [vs]. Fails if [var] overlaps with existing memory vars. *)
let set (store : t) ~(var : Variable.t) ~(vs : Reduced_interval_congruence.RIC.t) : t =
  let is_valid =
    Variable.Map.fold store ~init:true ~f:(fun ~key:k ~data:_ acc ->
      acc && 
      match k, var with
      | Var _, _ | _, Var _ -> true
      | Mem _, Mem _ ->
        Variable.equal k var ||
        not (Variable.share_addresses k var)) in
  if not is_valid then
    failwith "error: trying to update a memory variable that overlaps with other memory variables"
  else
    Variable.Map.set store ~key:var ~data:vs

(** [truncate_memory_var store ~var ~accessed_addresses] removes accessed addresses from [var]'s region. 
    The untouched portion is preserved in the store. *)
let truncate_memory_var (store : t) ~(var : Variable.t) ~(accessed_addresses : RIC.accessed) : t =
  let vs = get store ~var:var in
  let store = Variable.Map.remove store var in
  (* let store = set store ~var:var ~vs:RIC.Top in *)
  let accessed = accessed_addresses.fully :: accessed_addresses.partially in
  let untouched_addresses =
    match var with
    | Var _ -> assert false
    | Mem addr ->
      (List.fold ~init:[addr]
                 ~f:(fun acc x -> List.concat (List.map ~f:(fun y -> RIC.remove ~this:x ~from:y) acc))
                 accessed)
  in
  let untouched_variables = Variable.Set.of_list (List.map ~f:(fun x -> Variable.Mem x) untouched_addresses) in
  update_all store untouched_variables vs

(** [update_accessed_vars store accessed_addresses] applies [truncate_memory_var] to all memory variables. *)
let update_accessed_vars (store : t) (accessed_addresses : RIC.accessed) : t =
  let memory_vars = extract_memory_variables store in
  List.fold ~init:store ~f:(fun store var -> truncate_memory_var store ~var:var ~accessed_addresses:accessed_addresses) memory_vars

(** [weak_update store ~previous_state ~var ~vs] weakly updates [store] at [var] by joining [vs] with 
    the value in [previous_state], preserving values for unaffected regions. *)
let weak_update (store : t) ~(previous_state : t) ~(var : Variable.t) ~(vs : RIC.t) : t =
  let address = 
    match var with
    | Variable.Var _ -> assert false
    | Variable.Mem address -> address
  in
  (* print_endline ("address: " ^ RIC.to_string address); *)
  let memory_variables = extract_memory_variables previous_state in
  (* print_endline ("all mem vars: " ^ List.to_string ~f:Variable.to_string memory_variables); *)
  let affected_variables = 
    List.filter ~f:(fun (v, _) -> not (Variable.equal v (Mem RIC.Bottom)))
      (List.map ~f:(fun v -> 
          match v with 
          | Variable.Var _ -> assert false
          | Variable.Mem addr -> (Variable.Mem (RIC.meet addr address)), (get previous_state ~var:v))
        memory_variables)
  in
  let leftover_addresses = 
    List.fold 
      ~init:[address]
      ~f:(fun acc var ->
          match var with
          | Var _-> assert false
          | Mem addr ->
            List.concat (List.map ~f:(fun x -> RIC.remove ~this:addr ~from:x) acc)
        )
      memory_variables
  in
  let leftover_variables =
    Variable.Set.of_list (List.map ~f:(fun vs -> Variable.Mem vs) leftover_addresses) in
  (* print_endline ("all leftover vars: " ^ List.to_string ~f:Variable.to_string (Variable.Set.to_list leftover_variables)); *)
  let store = 
    if Variable.Set.is_empty leftover_variables then
      store
    else
      update_all store leftover_variables RIC.Top in
  List.fold ~init:store
            ~f:(fun store (v, prev_vs) -> 
              set store ~var:v ~vs:(RIC.join prev_vs vs))
            affected_variables

(** [assign_constant_value store ~const ~to_] sets [to_] to the singleton RIC containing [const]. *)
let assign_constant_value (store : t) ~(const : int32) ~(to_ : Variable.t): t =
  let vs = RIC.ric (0, Int 0, Int 0, ("", Option.value_exn (Int32.to_int const))) in
  Variable.Map.set store ~key:to_ ~data:vs

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
    match x, y with
    | Var Var.Const Prim_value.I32 x, Var Var.Const Prim_value.I32 y -> 
      let x = Option.value_exn (Int32.to_int x) in
      let y = Option.value_exn (Int32.to_int y) in
      RIC.ric (0, Int 0, Int 0, ("", x + y))
    | Var Var.Const Prim_value.I32 x, Var y -> 
      let x = Option.value_exn (Int32.to_int x) in
      let vs_y = get store ~var:(Variable.Var y) in
      RIC.add_offset vs_y x
    | Var x, Var Var.Const Prim_value.I32 y -> 
      let y = Option.value_exn (Int32.to_int y) in
      let vs_x = get store ~var:(Variable.Var x) in
      RIC.add_offset vs_x y
    | Var x, Var y ->
      let vs_x = get store ~var:(Variable.Var x) in
      let vs_y = get store ~var:(Variable.Var y) in
      RIC.plus vs_x vs_y
    | _ -> failwith "error" 
  in
  set store ~var:result ~vs:vs

(** [i32_sub store ~x ~y ~result] performs subtraction [y - x], stores result in [result]. *)
let i32_sub (store : t) ~(x : Variable.t) ~(y : Variable.t) ~(result : Variable.t) : t =
  let vs =
    match x, y with
    | Var Var.Const Prim_value.I32 x, Var Var.Const Prim_value.I32 y -> 
      let x = Option.value_exn (Int32.to_int x) in
      let y = Option.value_exn (Int32.to_int y) in
      RIC.ric (0, Int 0, Int 0, ("", y - x))
    | Var Var.Const Prim_value.I32 x, Var y -> 
      let x = - (Option.value_exn (Int32.to_int x)) in
      let vs_y = get store ~var:(Variable.Var y) in
      RIC.add_offset vs_y x
    | Var x, Var Var.Const Prim_value.I32 y -> 
      let y = Option.value_exn (Int32.to_int y) in
      let vs_x = RIC.negative (get store ~var:(Variable.Var x)) in
      RIC.add_offset vs_x y
    | Var x, Var y ->
      let vs_x = RIC.negative (get store ~var:(Variable.Var x)) in
      let vs_y = get store ~var:(Variable.Var y) in
      RIC.plus vs_x vs_y
    | _ -> failwith "error" 
  in
  set store ~var:result ~vs:vs

(** [v1_equals_v2_plus_c store ~v1 ~v2 ~c] sets [v1] to [v2 + c] in the abstract store. *)
let v1_equals_v2_plus_c (store : t) ~(v1 : Variable.t) ~(v2 : Variable.t) ~(c : int) : t =
  let vs2 = get store ~var:v2 in
  let vs1 = RIC.add_offset vs2 c in
  set store ~var:v1 ~vs:vs1

















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
    |> set ~var:var1 ~vs:(RIC.ric (2, Int 0, Int 3, ("", 4)))
    |> set ~var:var2 ~vs:RIC.Bottom
    |> set ~var:var3 ~vs:RIC.Top

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
      |> Variable.Map.set ~key:var1 ~data:(RIC.ric (2, Int 0, Int 2, ("", 0)))
      |> Variable.Map.set ~key:var2 ~data:(RIC.ric (3, Int 1, Int 3, ("", 0)))
    in
    let store2 =
      Variable.Map.empty
      |> Variable.Map.set ~key:var1 ~data:(RIC.ric (2, Int 1, Int 4, ("", 0)))
      |> Variable.Map.set ~key:var2 ~data:(RIC.ric (3, Int 2, Int 5, ("", 0)))
    in
    let joined = join store1 store2 in
    let expected =
      Variable.Map.empty
      |> Variable.Map.set ~key:var1 ~data:(RIC.ric (2, Int 0, Int 4, ("", 0)))
      |> Variable.Map.set ~key:var2 ~data:(RIC.ric (3, Int 1, Int 5, ("", 0)))
    in
    print_endline ("[JOIN]\n\t"  ^ to_string store1 ^ "\n\t" ^ to_string store2 ^ "\n\t\t" ^ to_string joined);
    equal joined expected

  let%test "meet abstract stores" =
    let var1 = Variable.Var (Var.Global 0) in
    let var2 = Variable.Var (Var.Local 0) in
    let store1 =
      Variable.Map.empty
      |> Variable.Map.set ~key:var1 ~data:(RIC.ric (2, Int 0, Int 4, ("", 0)))
      |> Variable.Map.set ~key:var2 ~data:(RIC.ric (3, Int 1, Int 5, ("", 0)))
    in
    let store2 =
      Variable.Map.empty
      |> Variable.Map.set ~key:var1 ~data:(RIC.ric (2, Int 2, Int 6, ("", 0)))
      |> Variable.Map.set ~key:var2 ~data:(RIC.ric (3, Int 0, Int 2, ("", 0)))
    in
    let met = meet store1 store2 in
    let expected =
      Variable.Map.empty
      |> Variable.Map.set ~key:var1 ~data:(RIC.ric (2, Int 2, Int 4, ("", 0)))
      |> Variable.Map.set ~key:var2 ~data:(RIC.ric (3, Int 1, Int 2, ("", 0)))
    in
    print_endline ("[MEET]\n\t"  ^ to_string store1 ^ "\n\t" ^ to_string store2 ^ "\n\t\t" ^ to_string met);
    equal met expected

  let%test "truncate memory var retains untouched regions" =
    let addr = RIC.ric (1, Int 0, Int 10, ("", 0)) in
    let mem_var = Variable.Mem addr in
    let vs = RIC.ric (1, Int 0, Int 1, ("", 0)) in
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
    let vs = RIC.ric (1, Int 0, Int 1, ("", 0)) in
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
      |> Variable.Map.set ~key:mem1 ~data:(RIC.ric (0, Int 0, Int 4, ("", 42)))
      |> Variable.Map.set ~key:mem2 ~data:(RIC.ric (0, Int 2, Int 6, ("", 36)))
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
      |> Variable.Map.set ~key:(Variable.mem (0, Int 0, Int 0, ("", 1))) ~data:(RIC.ric (42, Int 0, Int 1, ("", 0)))
      |> Variable.Map.set ~key:(Variable.mem (0, Int 0, Int 0, ("", 2))) ~data:(RIC.Top)
      |> Variable.Map.set ~key:(Variable.mem (0, Int 0, Int 0, ("", 3))) ~data:(RIC.ric (36, Int 0, Int 1, ("", 0))) 
    in
    equal expected updated

  let%test "get: variable not in store but covered by two others" =
    let var1 = Variable.mem (1, Int 0, Int 2, ("", 0)) in
    let var2 = Variable.mem (1, Int 3, Int 5, ("", 0)) in
    let target = Variable.mem (1, Int 2, Int 3, ("", 0)) in
    let vs1 = RIC.ric (1, Int 0, Int 2, ("", 0)) in
    let vs2 = RIC.ric (2, Int 3, Int 5, ("", 0)) in
    let store =
      Variable.Map.empty
      |> set ~var:var1 ~vs:vs1
      |> set ~var:var2 ~vs:vs2
    in
    let result = get store ~var:target in
    print_endline ("[get]\n\tinitial store: " ^ to_string store ^ "\n\tvariable to search for: " ^ Variable.to_string target
      ^ "\n\textracted value-set: " ^ RIC.to_string result);
    RIC.equal result (RIC.join vs1 vs2)

  let%test "get: variable not in store and not covered by others" =
    let var1 = Variable.mem (1, Int 0, Int 2, ("", 0)) in
    let var2 = Variable.mem (1, Int 4, Int 5, ("", 0)) in
    let target = Variable.mem (1, Int 2, Int 3, ("", 0)) in
    let vs1 = RIC.ric (1, Int 0, Int 2, ("", 0)) in
    let vs2 = RIC.ric (2, Int 3, Int 5, ("", 0)) in
    let store =
      Variable.Map.empty
      |> set ~var:var1 ~vs:vs1
      |> set ~var:var2 ~vs:vs2
    in
    let result = get store ~var:target in
    print_endline ("[get]\n\tinitial store: " ^ to_string store ^ "\n\tvariable to search for: " ^ Variable.to_string target
      ^ "\n\textracted value-set: " ^ RIC.to_string result);
    RIC.equal result RIC.Top

  let%test "make two stores compatible to join" =
    let m1 = Variable.Mem (RIC.ric (1, Int 1, Int 4, ("", 0))) in
    let vs1 = RIC.ric (0, Int 0, Int 0, ("", 42)) in
    let m2 = Variable.Mem (RIC.ric (2, Int 0, Int 4, ("", 0))) in
    let vs2 = RIC.ric (0, Int 0, Int 0, ("", 36)) in
    let store1 = set bottom ~var:m1 ~vs:vs1 in
    let store2 = set bottom ~var:m2 ~vs:vs2 in
    print_endline "[make_compatible]";
    print_endline ("\tstore1: " ^ to_string store1);
    print_endline ("\tstore2: " ^ to_string store2);
    let store1 = make_compatible ~this_store:store1 ~relative_to:store2 in
    let store2 = make_compatible ~this_store:store2 ~relative_to:store1 in
    print_endline ("\tcompatible store1: " ^ to_string store1);
    print_endline ("\tcompatible store2: " ^ to_string store2);
    print_endline ("\tjoin: " ^ to_string (join store1 store2));
    let expected = Variable.Map.empty
      |> set ~var:(Variable.mem(0, Int 0, Int 0, ("", 0))) ~vs:RIC.Top
      |> set ~var:(Variable.mem(0, Int 0, Int 0, ("", 1))) ~vs:RIC.Top
      |> set ~var:(Variable.mem(0, Int 0, Int 0, ("", 3))) ~vs:RIC.Top
      |> set ~var:(Variable.mem(2, Int 0, Int 1, ("", 6))) ~vs:RIC.Top
      |> set ~var:(Variable.Mem (RIC.ric (2, Int 0, Int 1, ("", 2)))) ~vs:(RIC.ric (6, Int 0, Int 1, ("", 36))) in
    equal (join store1 store2) expected

  let%test "join: Mem variables with incompatible offsets" =
    let mem1 = Variable.Mem (RIC.ric (1, Int 0, Int 4, ("stack", 0))) in
    let mem2 = Variable.Mem (RIC.ric (1, Int 0, Int 4, ("heap", 0))) in
    let vs1 = RIC.ric (1, Int 0, Int 4, ("stack", 0)) in
    let vs2 = RIC.ric (1, Int 0, Int 4, ("heap", 0)) in
    let store1 = Variable.Map.singleton mem1 vs1 in
    let store2 = Variable.Map.singleton mem2 vs2 in
    let joined = join store1 store2 in
    print_endline "[join: incompatible Mem offsets]";
    print_endline ("\tstore1: " ^ to_string store1);
    print_endline ("\tstore2: " ^ to_string store2);
    print_endline ("\tjoined: " ^ to_string joined);
    RIC.equal RIC.Top (get joined ~var:mem2)
end)