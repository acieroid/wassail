(** better documentation to come later *)
open Core

open Reduced_interval_congruence 

(** Type [t] represents a mapping from variables to their abstract values (RICs). *)
type t = RIC.t Variable.Map.t
[@@deriving sexp, compare, equal]

(** The bottom element of the domain, mapping no variables to any RICs. *)
let bottom : t = Variable.Map.empty
let top : t = Variable.Map.empty (* TODO: better definition of TOP *)

(** Constructs the top element of the domain for a given list of variables.
    Each variable is mapped to [RIC.Top], indicating maximum uncertainty. *)
(* let top (variables : Variable.t list) : t =
  let set_top_value_set (s : t) (v : Variable.t) : t =
    Variable.Map.set s ~key:v ~data:RIC.Top in
  List.fold_left variables ~init:bottom ~f:set_top_value_set *)

(** [subsumes t1 t2] returns [true] if [t1] over-approximates [t2], meaning
    each RIC in [t2] is subsumed by the corresponding RIC in [t1]. *)
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

(** Converts an abstract store to a human-readable string representation, including all bindings. *)
let to_string (vs : t) : string =
  Printf.sprintf "[%s]" (String.concat ~sep:", "
                          (List.map (Variable.Map.to_alist vs)
                            ~f:(fun (k, t) ->
                                Printf.sprintf "%s ↦ %s"
                                  (Variable.to_string k)
                                  (RIC.to_string t))))

(** Converts an abstract store to a string while omitting any variable bindings that map to [RIC.Bottom]. *)

let to_string_without_bottoms (vs : t) : string =
  let restricted = Variable.Map.filter vs ~f:(fun d ->
    not (RIC.equal RIC.Bottom d)) in
  to_string restricted

(** Computes the least upper bound (join) of two value sets, combining their information. *)
let join (store1 : t) (store2 : t) : t =
  Variable.Map.merge store1 store2 ~f:(fun ~key:_ v -> 
      match v with
      | `Both (x, y) -> Some (RIC.join x y)
      | `Left x | `Right x -> Some x)

(** Computes the greatest lower bound (meet) of two value sets, retaining only shared information. *)
let meet (store1 : t) (store2 : t) : t =
  Variable.Map.merge store1 store2 ~f:(fun ~key:_ v ->
      match v with 
      | `Both (x, y) -> Some (RIC.meet x y)
      | `Left _ | `Right _ -> Some (RIC.Bottom))

(* widen store2 relative to store1 ***** TODO: check that it's not the opposite *)
let widen (store1 : t) (store2 : t) : t =
  Variable.Map.merge store1 store2 ~f:(fun ~key:k v ->
      match k, v with
      | _, `Both (x, y) -> Some (RIC.widen y ~relative_to:x)
      | Mem _, `Right y -> Some (RIC.widen y ~relative_to:RIC.Top)
      | Var _, `Right y -> Some (RIC.widen y ~relative_to:RIC.Bottom)
      | Mem _, `Left x -> Some (RIC.widen RIC.Top ~relative_to:x)
      | Var _, `Left x -> Some (RIC.widen RIC.Bottom ~relative_to:x))

(** [get_RIC store var] retrieves the RIC associated with [var] in the abstract store [store],
    or returns [RIC.Bottom] if [var] is unbound. TOP IF VAR IS A POINTER ON THE MEMORY *)
let get_RIC (store : t) (var : Variable.t) : RIC.t =
  match Variable.Map.find store var with 
  | Some r -> r
  | None -> 
    match var with
    | Mem _ -> RIC.Top 
    | _ -> RIC.Bottom 

(** Resets the RIC associated with a single variable to [RIC.Bottom]. *)
let reset_RIC (store : t) (var : Variable.t) : t =
  match Variable.Map.find store var with 
  | Some _ -> Variable.Map.set store ~key:var ~data:RIC.Bottom
  | None -> store
  

(** Resets the RICs of a list of variables to [RIC.Bottom] within the abstract store. *)
let reset_RICs (store : t) (vars : Variable.Set.t) : t =
  Variable.Set.fold vars ~init:store ~f:reset_RIC

(** [update_all store vars new_RIC] sets [new_RIC] as the RIC of all [vars] in [store]. *)
let update_all (store : t) (vars : Variable.Set.t) (new_RIC : RIC.t) : t =
  Variable.Set.fold vars ~init:store ~f:(fun acc v -> Variable.Map.set acc ~key:v ~data:new_RIC)

(** [to_TOP_RIC store var] sets the RIC of [var] to [RIC.Top]. *)
let to_top_RIC (store : t) (var : Variable.t) : t =
  Variable.Map.set store ~key:var ~data:RIC.Top

(** [to_top_RICs store vars] sets the RIC of each variable in [vars] to [RIC.Top]. *)
let to_top_RICs (store : t) (vars : Variable.Set.t) : t =
  Variable.Set.fold vars ~init:store ~f:to_top_RIC

(** [to_bottom_RIC store var] sets the RIC of [var] to [RIC.Bottom]. *)
let to_bottom_RIC (store : t) (var : Variable.t) : t =
  Variable.Map.set store ~key:var ~data:RIC.Bottom

(** [to_bottom_RICs store vars] sets the RIC of each variable in [vars] to [RIC.Bottom]. *)
let to_bottom_RICs (store : t) (vars : Variable.Set.t) : t =
  Variable.Set.fold vars ~init:store ~f:to_bottom_RIC

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
    failwith "error: trying to update a memory variable that shares addresses with other memory variables"
  else
    Variable.Map.set store ~key:var ~data:vs

let substitute (state : t) (substitutions : (Variable.t * Reduced_interval_congruence.RIC.t) list) : t =
  List.fold substitutions ~init:state ~f:(fun state (v, vs) -> set state ~var:v ~vs:vs)


(** The following functions will be useful when defining the transfer function: *)

(* Utility functions *)
(* Join of a set of RICs *)
let join_of_set (store : t) (vars : Variable.Set.t) : RIC.t =
  Variable.Set.fold ~init:RIC.Bottom ~f:(fun acc v -> RIC.join acc (get_RIC store v)) vars

(* Get all memory variables *)

let assign_constant_value (store : t) ~(const : int32) ~(to_ : Variable.t): t =
  let vs = RIC.ric (0, Int 0, Int 0, ("", Option.value_exn (Int32.to_int const))) in
  Variable.Map.set store ~key:to_ ~data:vs

let copy_value_set (store : t) ~(from : Variable.t) ~(to_ : Variable.t) : t =
  let vs =
    match Variable.Map.find store from with
    | Some vs -> vs
    | None ->
      match from with
      | Var _ -> RIC.Bottom
      | Mem _ -> RIC.Top
  in
  Variable.Map.set store ~key:to_ ~data:vs



(* v1 = v2 + c *)
let v1_equals_v2_plus_c (store : t) ~(v1 : Variable.t) ~(v2 : Variable.t) ~(c : int) : t =
  let vs2 = 
    match Variable.Map.find store v2 with
    | Some vs -> vs
    | None -> RIC.Bottom
  in
  let vs1 = RIC.add_offset vs2 c in
  Variable.Map.set store ~key:v1 ~data:vs1

(* tmp = *(v + c) *)
(* On devra définir une fonction pour le join des vs d'un set *)















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
  

  let var1 = Variable.Var (Var.Global 0)
  let var2 = Variable.Var (Var.Local 0)
  let var3 = Variable.Mem (RIC.ric (0, Int 0, Int 0, ("", 4)))

  let vs =
    Variable.Map.empty
    |> Variable.Map.set ~key:var1 ~data:(RIC.ric (2, Int 0, Int 3, ("", 4)))
    |> Variable.Map.set ~key:var2 ~data:RIC.Bottom
    |> Variable.Map.set ~key:var3 ~data:RIC.Top

  let%test "to_string includes all bindings" =
    let s = to_string vs in
    print_endline s; true

  let%test "to_string_without_bottoms omits Bottoms" =
    let s = to_string_without_bottoms vs in
    print_endline s; true

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
    print_endline (to_string store1 ^ "\n\tJoin " ^ to_string store2 ^ "\n\t\t= " ^ to_string joined);
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
    print_endline (to_string store1 ^ "\n\tMeet " ^ to_string store2 ^ "\n\t\t= " ^ to_string met);
    equal met expected
end)