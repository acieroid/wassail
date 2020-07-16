open Core_kernel

(** A taint value is a set of variables *)
type taint =
  | Taints of Var.Set.t
  | TopTaint
[@@deriving sexp, compare, equal]

let taint_bottom : taint = Taints Var.Set.empty
let taint_top : taint = TopTaint
let taint (v : Var.t) : taint = Taints (Var.Set.singleton v)

(** Joining taints is simply taking their union *)
let join_taint (t1 : taint) (t2 : taint) : taint = match t1, t2 with
  | TopTaint, _
  | _, TopTaint -> TopTaint
  | Taints t1, Taints t2 -> Taints (Var.Set.union t1 t2)

let taint_replace_join (t : taint) (var : Var.t) (t2 : taint) = match t with
  | Taints ts -> join_taint (Taints (Var.Set.remove ts var)) t2
  | TopTaint -> TopTaint

let taint_to_string (t : taint) : string = match t with
  | Taints t ->
    String.concat ~sep:"," (List.map (Var.Set.to_list t)
                              ~f:Var.to_string)
  | TopTaint -> "TopTaint"

(** Performs multiple substitutions of taint variables*)
let taint_substitute (t : taint) (subst : (Var.t * taint) list) : taint = match t with
  | Taints ts ->
    (* It is important to perform all substitutions at the same time, otherwise we risk substituting incorrectly, e.g. with the substitution [(l0, l1); (l1; l2)]: clearly, l0 should become l1, and not "become l1 then become l2" *)
    let (vars_to_remove, taint_to_add) = Var.Set.fold ts
        ~init:(Var.Set.empty, taint_bottom)
        ~f:(fun (rm, add) x ->
            match List.find subst ~f:(fun (y, _) -> Var.equal x y) with
            | None -> (rm, add)
            | Some (_, z) ->
              Printf.printf "subst %s by %s\n" (Var.to_string x) (taint_to_string z);
              (Var.Set.add rm x, join_taint z add)) in
    (join_taint (Taints (Var.Set.diff ts vars_to_remove)) taint_to_add)
  | TopTaint -> TopTaint

(** The state of the taint analysis is a map from variables to their taint values.
    If a variable is not bound in the state, it is assumed that its taint is bottom *)
type t = taint Var.Map.t
[@@deriving sexp, compare, equal]

let to_string (s : t) : string =
  Printf.sprintf "[%s]" (String.concat ~sep:", "
                           (List.map (Var.Map.to_alist s)
                              ~f:(fun (k, t) ->
                                  Printf.sprintf "%s: %s"
                                    (Var.to_string k)
                                    (taint_to_string t))))




let join (s1 : t) (s2 : t) : t =
  Var.Map.merge s1 s2 ~f:(fun ~key:_ v -> match v with
      | `Both (x, y) -> Some (join_taint x y)
      | `Left x | `Right x -> Some x)

let get_taint (s : t) (var : Var.t) : taint =
  match Var.Map.find s var with
  | Some t -> t
  | None -> Taints Var.Set.empty

(** Rename a key in the taint domain: rename_key [a: b][c: d] a e results in [e: b][c: d] *)
let rename_key (s : t) (from : Var.t) (to_ : Var.t) : t =
  Var.Map.set (Var.Map.remove s from) ~key:to_ ~data:(Var.Map.find_exn s from)

(** Replace all taints: replace_taint [a: {b,c}][d: {b,e}] b f results in [a: {c} join f][d: {e} join f] *)
let replace_taint (s : t) (from : Var.t) (to_ : taint) : t =
  Var.Map.map s ~f:(fun v -> taint_replace_join v from to_)

(** Add taint to a variable *)
let add_taint (s : t) (v : Var.t) (taint : taint) : t =
  (* Printf.printf "update taint: %s -> %s\n" (Var.to_string v) (taint_to_string taint); *)
  Var.Map.update s v ~f:(function
      | None -> taint
      | Some t -> join_taint t taint)

(** Make the taint flow from one variable to another *)
let add_taint_v (s : t) (v : Var.t) (taint : Var.t) : t =
  add_taint s v (get_taint s taint)

(** Sets the taint of a variable to top *)
let set_top_taint (s : t) (v : Var.t) : t =
  (* Printf.printf "add taint: %s -> top\n" (Var.to_string v); *)
  Var.Map.set s ~key:v ~data:TopTaint

(** The bottom state does not contain any taint *)
let bottom : t = Var.Map.empty

let bottom_with_keys (keys : Var.t list) : t =
  Var.Map.of_alist_exn (List.map keys ~f:(fun k -> (k, taint_bottom)))

(** The top state taints all globals and the return value with the top taint *)
let top (globals : Var.t list) (ret : Var.t option) : t =
  Option.fold ret
    ~init:(List.fold_left globals
             ~init:bottom
             ~f:set_top_taint)
    ~f:set_top_taint

(** Restrict the domain of a taint state to only variables contained in the vars argument *)
let restrict (s : t) (vars : Var.t list) : t =
  Var.Map.filter_keys s ~f:(fun k -> List.mem vars k ~equal:Var.equal)
