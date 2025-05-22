(** This module defines an abstract domain for value sets using Reduced Interval Congruence (RIC).
    A value set is a mapping from variables to RIC elements, which represent sets of possible values.
    This domain is designed for static analysis, particularly in pointer analysis, and supports
    operations such as join, meet, comparison, resetting, and transfer functions for program semantics. *)
open Core

open Reduced_interval_congruence 

(** Type [t] represents a mapping from variables to their abstract values (RICs). *)
type t = RIC.t Variable.Map.t
[@@deriving sexp, compare, equal]

(** The bottom element of the domain, mapping no variables to any RICs. *)
let bottom : t = Variable.Map.empty

(** Constructs the top element of the domain for a given list of variables.
    Each variable is mapped to [RIC.Top], indicating maximum uncertainty. *)
let top (variables : Variable.t list) : t =
  let set_top_value_set (s : t) (v : Variable.t) : t =
    Variable.Map.set s ~key:v ~data:RIC.Top in
  List.fold_left variables ~init:bottom ~f:set_top_value_set

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

(** Converts a value set to a human-readable string representation, including all bindings. *)
let to_string (vs : t) : string =
  Printf.sprintf "[%s]" (String.concat ~sep:", "
                          (List.map (Variable.Map.to_alist vs)
                            ~f:(fun (k, t) ->
                                Printf.sprintf "%s ↦ %s"
                                  (Variable.to_string k)
                                  (RIC.to_string t))))

(** Converts a value set to a string while omitting any variable bindings that map to [RIC.Bottom]. *)
let to_string_without_bottoms (vs : t) : string =
  let restricted = Variable.Map.filter vs ~f:(fun d ->
    not (RIC.equal RIC.Bottom d)) in
  to_string restricted

(** Computes the least upper bound (join) of two value sets, combining their information. *)
let join (vs1 : t) (vs2 : t) : t =
  Variable.Map.merge vs1 vs2 ~f:(fun ~key:_ v -> 
      match v with
      | `Both (x, y) -> Some (RIC.join x y)
      | `Left x | `Right x -> Some x)

(** Computes the greatest lower bound (meet) of two value sets, retaining only shared information. *)
let meet (vs1 : t) (vs2 : t) : t =
  Variable.Map.merge vs1 vs2 ~f:(fun ~key:_ v ->
      match v with 
      | `Both (x, y) -> Some (RIC.meet x y)
      | `Left _ | `Right _ -> Some (RIC.Bottom))

(** [get_RIC vs var] retrieves the RIC associated with [var] in the value set [vs],
    or returns [RIC.Bottom] if [var] is unbound. *)
let get_RIC (vs : t) (var : Variable.t) : RIC.t =
  match Variable.Map.find vs var with 
  | Some r -> r
  | None -> RIC.Bottom 

(** Resets the RIC associated with a single variable to [RIC.Bottom]. *)
let reset_RIC (vs : t) (var : Variable.t) : t =
  match Variable.Map.find vs var with 
  | Some _ -> Variable.Map.set vs ~key:var ~data:RIC.Bottom
  | None -> vs
  

(** Resets the RICs of a list of variables to [RIC.Bottom] within the value set. *)
let reset_RICs (vs : t) (vars : Variable.Set.t) : t =
  Variable.Set.fold vars ~init:vs ~f:reset_RIC

(** [update_all vs vars new_RIC] sets [new_RIC] as the RIC of all [vars] in [vs]. *)
let update_all (vs : t) (vars : Variable.Set.t) (new_RIC : RIC.t) : t =
  Variable.Set.fold vars ~init:vs ~f:(fun acc v -> Variable.Map.set acc ~key:v ~data:new_RIC)

(** [to_TOP_RIC vs var] sets the RIC of [var] to [RIC.Top]. *)
let to_top_RIC (vs : t) (var : Variable.t) : t =
  Variable.Map.set vs ~key:var ~data:RIC.Top

(** [to_top_RICs vs vars] sets the RIC of each variable in [vars] to [RIC.Top]. *)
let to_top_RICs (vs : t) (vars : Variable.Set.t) : t =
  Variable.Set.fold vars ~init:vs ~f:to_top_RIC





(** The following functions will be useful when defining the transfer function: *)

(* R1 = R2 + c *)
(** [copy_with_offset vs v1 v2 c] sets [v1] to be [v2 + c], abstractly. *)
let copy_with_offset (vs : t) (v1 : Variable.t) (v2 : Variable.t) (c : int) : t =
  let v2_RIC = get_RIC vs v2 in 
  let offset_RIC = RIC.add_offset v2_RIC c in
  Variable.Map.set vs ~key:v1 ~data:offset_RIC

(* R1 <= c *)
(** [less_than_constant vs v c] refines [v]'s RIC to values ≤ [c]. *)
let less_than_constant (vs : t) (v : Variable.t) (c : int) : t =
  let v_RIC = get_RIC vs v in 
  let c_RIC = RIC.ric (1, NegInfinity, Int c, ("", 0)) in 
  let new_RIC = RIC.meet v_RIC c_RIC in
  Variable.Map.set vs ~key:v ~data:new_RIC
  
(* R1 >= R2 *)
(** [v1_greater_than_v2 vs v1 v2] refines [v1] to be ≥ [v2]'s values. *)
let v1_greater_than_v2 (vs : t) (v1 : Variable.t) (v2 : Variable.t) : t =
  let v1_RIC = get_RIC vs v1 in
  let v2_RIC = get_RIC vs v2 in 
  let ric_lb = RIC.remove_upper_bound v2_RIC in 
  let new_RIC = RIC.meet v1_RIC ric_lb in
  Variable.Map.set vs ~key:v1 ~data:new_RIC

(* *(R1 + c1) = R2 + c2 *)
(** [indirect_assign vs s vars v1 c1 v2 c2 other_conditions]
    models *(v1 + c1) = v2 + c2 for size [s]. If the update is precise (single target), a strong update
    is done. Otherwise, a weak update is performed. *)
let indirect_assign (vs : t) (s : int) (vars : Variable.Set.t) (v1 : Variable.t) (c1 : int) (v2 : Variable.t) (c2 : int) (other_conditions : bool) : t =
  let v1_RIC = get_RIC vs v1 in
  let v2_RIC = get_RIC vs v2 in 
  let (f, p) = RIC.accessed vars s (RIC.add_offset v1_RIC c1) in
  let tmp = reset_RICs (to_top_RICs vs p) f in
  if Variable.Set.cardinal f = 1 && Variable.Set.cardinal p = 0 && other_conditions then (* strong update *)
    let new_RIC = RIC.add_offset v2_RIC c2 in
    update_all tmp f new_RIC
  else (* weak update *)
    Variable.Set.fold f ~init:tmp ~f:(fun acc var ->
        let v_old_RIC = get_RIC vs var in 
        let v_new_RIC = RIC.join (RIC.add_offset v2_RIC c2) v_old_RIC in
        Variable.Map.set acc ~key:var ~data:v_new_RIC
      )

(** [dereferencing vs s vars v1 v2 c1 c2] models v1 = *(v2 + c1) + c2. 
    If the dereference is precise, it joins the accessed RICs. *)
let dereferencing (vs : t) (s : int) (vars : Variable.Set.t) (v1 : Variable.t) (v2 : Variable.t) (c1 : int) (c2 : int) : t =
  let v2_RIC = get_RIC vs v2 in
  let (f, p) = RIC.accessed vars s (RIC.add_offset v2_RIC c1) in
  if Variable.Set.cardinal p = 0 then 
    let ric_rhs = Variable.Set.fold f ~init:RIC.Bottom ~f:(fun acc v -> RIC.join acc (get_RIC vs v)) in
    Variable.Map.set vs ~key:v1 ~data:(RIC.add_offset ric_rhs c2)
  else
    to_top_RIC vs v1














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

let%test_module "value_set tests" = (module struct
 (* Tests will come later *)
end)