open Core_kernel

(** A taint value is a set of variables *)
type taint = Spec_inference.VarSet.t
[@@deriving sexp, compare, equal]

(** Joining taints is simply taking their union *)
let join_taint (t1 : taint) (t2 : taint) : taint = Spec_inference.VarSet.union t1 t2

let taint_to_string (t : taint) : string = String.concat ~sep:"," (List.map (Spec_inference.VarSet.to_list t)
                                                                     ~f:Spec_inference.var_to_string)

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
      | `Both (x, y) -> Some (Spec_inference.VarSet.union x y)
      | `Left x | `Right x -> Some x)

let get_taint (s : t) (var : Spec_inference.var) : taint =
  match Spec_inference.VarMap.find s var with
  | Some t -> t
  | None -> Spec_inference.VarSet.empty

(** Add taint to avariable *)
let add_taint (s : t) (v : Spec_inference.var) (taint : Spec_inference.var) : t =
  Printf.printf "add taint: %s -> %s\n" (Spec_inference.var_to_string v) (Spec_inference.var_to_string taint);
  Spec_inference.VarMap.update s v ~f:(function
      | None -> get_taint s taint
      | Some t -> Spec_inference.VarSet.union t (get_taint s taint))
