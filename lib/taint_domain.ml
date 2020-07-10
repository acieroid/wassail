open Core_kernel

(** A taint value is a set of variables *)
type taint =
  | Taints of Spec_inference.VarSet.t
  | TopTaint
[@@deriving sexp, compare, equal]

let taint (v : Spec_inference.var) : taint = Taints (Spec_inference.VarSet.singleton v)

(** Joining taints is simply taking their union *)
let join_taint (t1 : taint) (t2 : taint) : taint = match t1, t2 with
  | TopTaint, _
  | _, TopTaint -> TopTaint
  | Taints t1, Taints t2 -> Taints (Spec_inference.VarSet.union t1 t2)

let taint_to_string (t : taint) : string = match t with
  | Taints t ->
    String.concat ~sep:"," (List.map (Spec_inference.VarSet.to_list t)
                              ~f:Spec_inference.var_to_string)
  | TopTaint -> "TopTaint"

(** The state of the taint analysis is a map from variables to their taint values.
    If a variable is not bound in the state, it is assumed that its taint is bottom *)
type t = taint Spec_inference.VarMap.t
[@@deriving sexp, compare, equal]

let to_string (s : t) : string =
  Printf.sprintf "[%s]" (String.concat ~sep:", "
                           (List.map (Spec_inference.VarMap.to_alist s)
                              ~f:(fun (k, t) ->
                                  Printf.sprintf "%s: %s"
                                    (Spec_inference.var_to_string k)
                                    (taint_to_string t))))




let join (s1 : t) (s2 : t) : t =
  Spec_inference.VarMap.merge s1 s2 ~f:(fun ~key:_ v -> match v with
      | `Both (x, y) -> Some (join_taint x y)
      | `Left x | `Right x -> Some x)

let get_taint (s : t) (var : Spec_inference.var) : taint =
  match Spec_inference.VarMap.find s var with
  | Some t -> t
  | None -> Taints Spec_inference.VarSet.empty

(** Add taint to avariable *)
let add_taint (s : t) (v : Spec_inference.var) (taint : Spec_inference.var) : t =
  Printf.printf "add taint: %s -> %s\n" (Spec_inference.var_to_string v) (Spec_inference.var_to_string taint);
  Spec_inference.VarMap.update s v ~f:(function
      | None -> get_taint s taint
      | Some t -> join_taint t (get_taint s taint))

(** Sets the taint of a variable to top *)
let set_top_taint (s : t) (v : Spec_inference.var) : t =
  Printf.printf "add taint: %s -> top\n" (Spec_inference.var_to_string v);
  Spec_inference.VarMap.set s ~key:v ~data:TopTaint

(** The bottom state does not contain any taint *)
let bottom : t = Spec_inference.VarMap.empty

(** The top state taints all globals and the return value with the top taint *)
let top (globals : Spec_inference.var list) (ret : Spec_inference.var option) : t =
  Option.fold ret
    ~init:(List.fold_left globals
             ~init:bottom
             ~f:set_top_taint)
    ~f:set_top_taint

(** Restrict the domain of a taint state to only variables contained in the vars argument *)
let restrict (s : t) (vars : Spec_inference.var list) : t =
  Spec_inference.VarMap.filter_keys s ~f:(fun k -> List.mem vars k ~equal:Spec_inference.equal_var)
  
