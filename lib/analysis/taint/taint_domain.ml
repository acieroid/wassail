open Core

module Taint = struct
  (** A taint value is a set of variables *)
  type t =
    | Taints of Var.Set.t
    | TopTaint
  [@@deriving sexp, compare, equal]

  (** The bottom taint *)
  let bottom : t = Taints Var.Set.empty

  (** The top taint *)
  let top : t = TopTaint

  (** Creates a taint from a variable *)
  let taint (v : Var.t) : t = Taints (Var.Set.singleton v)

  (** Creates a taint from multiple variables *)
  let taints (vs : Var.Set.t) : t = Taints vs

  (** Checks that t1 subsumes t2 *)
  let subsumes (t1 : t) (t2 : t) : bool = match t1, t2 with
    | TopTaint, _ -> true
    | _, TopTaint -> false
    | Taints v1, Taints v2 -> Var.Set.is_subset v2 ~of_:v1

  (** Joining taints is simply taking their union *)
  let join (t1 : t) (t2 : t) : t = match t1, t2 with
    | TopTaint, _
    | _, TopTaint -> TopTaint
    | Taints t1, Taints t2 -> Taints (Var.Set.union t1 t2)

  (** Converts a taint to its string representation *)
  let to_string (t : t) : string = match t with
    | Taints t ->
      if Var.Set.is_empty t then "_" else
        String.concat ~sep:"," (List.map (Var.Set.to_list t)
                                  ~f:Var.to_string)
    | TopTaint -> "TopTaint"

  (** Parses a taint from its string representation *)
  let of_string (s : string) : t = match s with
    | "TopTaint" -> TopTaint
    | "_" -> Taints Var.Set.empty
    | _ -> Taints (Var.Set.of_list (List.map (String.split s ~on:',') ~f:(fun s ->
        match String.get s 0 with
        | 'l' -> Var.Local (int_of_string (String.drop_prefix s 1))
        | 'g' -> Var.Global (int_of_string (String.drop_prefix s 1))
        | _ -> failwith "invalid taint")))

  (** Performs multiple substitutions of taint variables.
      Substitutions are described by a list of pairs: the Var.t is the variable to substitute, and the t is the taint to substitite it with *)
  let substitute (t : t) (subst : (Var.t * t) list) : t = match t with
    | Taints ts ->
      (* It is important to perform all substitutions at the same time, otherwise we risk substituting incorrectly, e.g. with the substitution [(l0, l1); (l1; l2)]: clearly, l0 should become l1, and not "become l1 then become l2" *)
      let (vars_to_remove, taint_to_add) = Var.Set.fold ts
          ~init:(Var.Set.empty, bottom)
          ~f:(fun (rm, add) x ->
              match List.find subst ~f:(fun (y, _) -> Var.equal x y) with
              | None -> (rm, add)
              | Some (_, z) -> (Var.Set.add rm x, join z add)) in
      (join (Taints (Var.Set.diff ts vars_to_remove)) taint_to_add)
    | TopTaint -> TopTaint


  module Test = struct
    let%test "taint subsumption works as expected" =
      subsumes TopTaint bottom &&
      subsumes (taint (Var.Local 0)) bottom &&
      subsumes (taint (Var.Local 0)) (taint (Var.Local 0)) &&
      subsumes (taints (Var.Set.of_list [Var.Local 0; Var.Local 1])) (taint (Var.Local 0)) &&
      not (subsumes bottom (taint (Var.Local 0))) &&
      not (subsumes bottom TopTaint)

    let%test "taint joining works as expected" =
      equal (join bottom bottom) bottom &&
      equal (join top bottom) top &&
      equal (join bottom top) top &&
      equal (join (taint (Var.Local 0)) (taint (Var.Local 1)))
        (taints (Var.Set.of_list [Var.Local 0; Var.Local 1]))

    let%test "taint to_string works as expected" =
      String.equal "_" (to_string bottom) &&
      String.equal "l0" (to_string (taint (Var.Local 0))) &&
      String.equal "l0,l1" (to_string (taints (Var.Set.of_list [Var.Local 0; Var.Local 1])))

    let%test "taint substitute works as expected" =
      let l0 = taint (Var.Local 0) in
      let l1 = taint (Var.Local 1) in
      let l0l1 = taints (Var.Set.of_list [Var.Local 0; Var.Local 1]) in
      equal (substitute bottom []) bottom &&
      equal (substitute bottom [(Var.Local 0, top)]) bottom &&
      equal (substitute top [(Var.Local 0, bottom)]) top &&
      equal (substitute l0 [(Var.Local 0, taint (Var.Local 1))]) l1 &&
      equal (substitute l0 [(Var.Local 0, top)]) top &&
      equal (substitute l0l1 [(Var.Local 0, l1)]) l1 &&
      equal (substitute l0l1 [(Var.Local 0, l0)]) l0l1 &&
      equal (substitute l0l1 [(Var.Local 1, l0)]) l0
  end
end

module Make : Helpers.ABSTRACT_DOMAIN with type t = Taint.t Var.Map.t = struct

  (** The state of the taint analysis is a map from variables to their taint values.
      If a variable is not bound in the state, it is assumed that its taint is bottom *)
  type t = Taint.t Var.Map.t
  [@@deriving sexp, compare, equal]

  (** The bottom state does not contain any taint *)
  let bottom : t = Var.Map.empty

  (** Convert a taint map to its string representation with all details *)
  let to_string (s : t) : string =
    Printf.sprintf "[%s]" (String.concat ~sep:", "
                             (List.map (Var.Map.to_alist s)
                                ~f:(fun (k, t) ->
                                    Printf.sprintf "%s: %s"
                                      (Var.to_string k)
                                      (Taint.to_string t))))

  (** Join two taint maps together *)
  let join (s1 : t) (s2 : t) : t =
    Var.Map.merge s1 s2 ~f:(fun ~key:_ v -> match v with
        | `Both (x, y) -> Some (Taint.join x y)
        | `Left x | `Right x -> Some x)


  let widen (_s1 : t) (s2 : t) : t =
    s2 (* no widening *)

end

include Make

(** The top state taints all globals and the return value with the top taint *)
let top (globals : Var.t list) (ret : Var.t option) : t =
  (* Sets the taint of a variable to top *)
  let set_top_taint (s : t) (v : Var.t) : t =
    Var.Map.set s ~key:v ~data:TopTaint in
  Option.fold ret
    ~init:(List.fold_left globals
             ~init:bottom
             ~f:set_top_taint)
    ~f:set_top_taint

(** Check if t1 subsumes t2 *)
let subsumes (t1 : t) (t2 : t) : bool =
  (* For each variable in t2, its taint should be subsumed by its taint in t1 *)
  Var.Map.fold t2 ~init:true ~f:(fun ~key:k ~data:v2 sub ->
      if sub then
        if Taint.equal v2 Taint.bottom then
          true (* v2 is bottom, so any value in t1[k] would subsume it, no need to check *)
        else match Var.Map.find t1 k with
          | Some v1 -> Taint.subsumes v1 v2
          | None -> false
      else
        false)


(** Converts a taint map to its string representation, using only the non-identity taints (e.g., if l0 is tainted by exactly l0, it is not printed *)
let only_non_id_to_string (s : t) : string =
  let restricted = Var.Map.filteri s ~f:(fun ~key:k ~data:d ->
      match d with
      | Taints taints -> not (Var.Set.equal taints (Var.Set.singleton k))
      | _ -> true) in
  to_string restricted


(** Get the taint of a variable in the taint map *)
let get_taint (s : t) (var : Var.t) : Taint.t =
  match Var.Map.find s var with
  | Some t -> t
  | None -> Taints Var.Set.empty

(** Add taint to a variable *)
let add_taint (s : t) (v : Var.t) (taint : Taint.t) : t =
  Var.Map.update s v ~f:(function
      | None -> taint
      | Some t -> Taint.join t taint)

(** Make the taint flow from one variable to another *)
let add_taint_v (s : t) (v : Var.t) (taint : Var.t) : t =
  add_taint s v (get_taint s taint)


module Test = struct
  let%test "subsumption works as expected" =
    let open Instr.Label.Test in
    (* [i0 -> bottom *)
    let bot = Var.Map.of_alist_exn [(Var.Local 0, Taint.taint (Var.Local 0));
                                    (Var.Local 1, Taint.taint (Var.Local 1));
                                    (Var.Var (lab 0), Taint.bottom)] in
    (* [i0 -> l0] *)
    let l0 = Var.Map.of_alist_exn [(Var.Local 0, Taint.taint (Var.Local 0));
                                   (Var.Local 1, Taint.taint (Var.Local 1));
                                   (Var.Var (lab 0), Taint.taint (Var.Local 0))] in
    (* [i0 -> l1] *)
    let l1 = Var.Map.of_alist_exn [(Var.Local 0, Taint.taint (Var.Local 0));
                                   (Var.Local 1, Taint.taint (Var.Local 1));
                                   (Var.Var (lab 0), Taint.taint (Var.Local 1))] in
    (* [i0 -> top] *)
    let top = Var.Map.of_alist_exn [(Var.Local 0, Taint.taint (Var.Local 0));
                                    (Var.Local 1, Taint.taint (Var.Local 1));
                                    (Var.Var (lab 0), Taint.top)] in
    subsumes l0 bot && not (subsumes bot l0)  &&
    subsumes l1 bot && not (subsumes bot l1) &&
    subsumes top bot && not (subsumes bot top)  &&
    subsumes top l0 && not (subsumes l0 top) &&
    subsumes top l1 && not (subsumes l1 top) &&
    not (subsumes l0 l1) && not (subsumes l1 l0)

  let%test "to_string produces the expected string" =
    let open Instr.Label.Test in
    (* [i0 -> bottom *)
    let bot = Var.Map.of_alist_exn [(Var.Local 0, Taint.taint (Var.Local 0));
                                    (Var.Local 1, Taint.taint (Var.Local 1));
                                    (Var.Var (lab 0), Taint.bottom)] in
    (* [i0 -> l0] *)
    let l0 = Var.Map.of_alist_exn [(Var.Local 0, Taint.taint (Var.Local 0));
                                   (Var.Local 1, Taint.taint (Var.Local 1));
                                   (Var.Var (lab 0), Taint.taint (Var.Local 0))] in
    let l0l1 =  Var.Map.of_alist_exn [(Var.Local 0, Taint.taint (Var.Local 0));
                                      (Var.Local 1, Taint.taint (Var.Local 1));
                                      (Var.Var (lab 0), Taint.taints (Var.Set.of_list [Var.Local 0; Var.Local 1]))] in
    String.equal (to_string bot) "[i0: _, l0: l0, l1: l1]" &&
    String.equal (to_string l0) "[i0: l0, l0: l0, l1: l1]" &&
    String.equal (to_string l0l1) "[i0: l0,l1, l0: l0, l1: l1]"

  let%test "join produces the expected taint maps" =
    let open Instr.Label.Test in
    (* [i0 -> bottom *)
    let bot = Var.Map.of_alist_exn [(Var.Local 0, Taint.taint (Var.Local 0));
                                    (Var.Local 1, Taint.taint (Var.Local 1));
                                    (Var.Var (lab 0), Taint.bottom)] in
    (* [i0 -> l0] *)
    let l0 = Var.Map.of_alist_exn [(Var.Local 0, Taint.taint (Var.Local 0));
                                   (Var.Local 1, Taint.taint (Var.Local 1));
                                   (Var.Var (lab 0), Taint.taint (Var.Local 0))] in
    (* [i0 -> l1] *)
    let l1 = Var.Map.of_alist_exn [(Var.Local 0, Taint.taint (Var.Local 0));
                                   (Var.Local 1, Taint.taint (Var.Local 1));
                                   (Var.Var (lab 0), Taint.taint (Var.Local 1))] in
    (* [i0 -> {l0,l1}] *)
    let l0l1 =  Var.Map.of_alist_exn [(Var.Local 0, Taint.taint (Var.Local 0));
                                      (Var.Local 1, Taint.taint (Var.Local 1));
                                      (Var.Var (lab 0), Taint.taints (Var.Set.of_list [Var.Local 0; Var.Local 1]))] in
    (* [i0 -> top] *)
    let top = Var.Map.of_alist_exn [(Var.Local 0, Taint.taint (Var.Local 0));
                                    (Var.Local 1, Taint.taint (Var.Local 1));
                                    (Var.Var (lab 0), Taint.top)] in
    equal (join bot l0) l0 && equal (join l0 bot) l0 &&
    equal (join l0 l1) l0l1 && equal (join l1 l0) l0l1 &&
    equal (join l0 l0l1) l0l1 && equal (join l0l1 l0) l0l1 &&
    equal (join bot top) top && equal (join top bot) top &&
    equal (join top l0) top && equal (join l0 top) top

  let%test "get_taint produces the expected taint" =
    let open Instr.Label.Test in
    (* [i0 -> bottom *)
    let bot = Var.Map.of_alist_exn [(Var.Local 0, Taint.taint (Var.Local 0));
                                    (Var.Local 1, Taint.taint (Var.Local 1));
                                    (Var.Var (lab 0), Taint.bottom)] in
    (* [i0 -> l0] *)
    let l0 = Var.Map.of_alist_exn [(Var.Local 0, Taint.taint (Var.Local 0));
                                   (Var.Local 1, Taint.taint (Var.Local 1));
                                   (Var.Var (lab 0), Taint.taint (Var.Local 0))] in
    (* [i0 -> l1] *)
    let l1 = Var.Map.of_alist_exn [(Var.Local 0, Taint.taint (Var.Local 0));
                                   (Var.Local 1, Taint.taint (Var.Local 1));
                                   (Var.Var (lab 0), Taint.taint (Var.Local 1))] in
    (* [i0 -> {l0,l1}] *)
    let l0l1 =  Var.Map.of_alist_exn [(Var.Local 0, Taint.taint (Var.Local 0));
                                      (Var.Local 1, Taint.taint (Var.Local 1));
                                      (Var.Var (lab 0), Taint.taints (Var.Set.of_list [Var.Local 0; Var.Local 1]))] in
    (* [i0 -> top] *)
    let top = Var.Map.of_alist_exn [(Var.Local 0, Taint.taint (Var.Local 0));
                                    (Var.Local 1, Taint.taint (Var.Local 1));
                                    (Var.Var (lab 0), Taint.top)] in
    Taint.equal (get_taint bot  (Var.Var (lab 0))) Taint.bottom &&
    Taint.equal (get_taint l0   (Var.Var (lab 0))) (Taint.taint (Var.Local 0)) &&
    Taint.equal (get_taint l1   (Var.Var (lab 0))) (Taint.taint (Var.Local 1)) &&
    Taint.equal (get_taint l0l1 (Var.Var (lab 0))) (Taint.taints (Var.Set.of_list [Var.Local 0; Var.Local 1])) &&
    Taint.equal (get_taint top  (Var.Var (lab 0))) Taint.top

  let%test "add_taint adds the taint as expected" =
    let open Instr.Label.Test in
    (* [i0 -> bottom *)
    let bot = Var.Map.of_alist_exn [(Var.Local 0, Taint.taint (Var.Local 0));
                                    (Var.Local 1, Taint.taint (Var.Local 1));
                                    (Var.Var (lab 0), Taint.bottom)] in
    (* [i0 -> l0] *)
    let l0 = Var.Map.of_alist_exn [(Var.Local 0, Taint.taint (Var.Local 0));
                                   (Var.Local 1, Taint.taint (Var.Local 1));
                                   (Var.Var (lab 0), Taint.taint (Var.Local 0))] in
    (* [i0 -> {l0,l1}] *)
    let l0l1 =  Var.Map.of_alist_exn [(Var.Local 0, Taint.taint (Var.Local 0));
                                      (Var.Local 1, Taint.taint (Var.Local 1));
                                      (Var.Var (lab 0), Taint.taints (Var.Set.of_list [Var.Local 0; Var.Local 1]))] in
    (* [i0 -> top] *)
    let top = Var.Map.of_alist_exn [(Var.Local 0, Taint.taint (Var.Local 0));
                                    (Var.Local 1, Taint.taint (Var.Local 1));
                                    (Var.Var (lab 0), Taint.top)] in
    equal (add_taint bot (Var.Var (lab 0)) (Taint.taint (Var.Local 0))) l0 &&
    equal (add_taint l0 (Var.Var (lab 0)) (Taint.taint (Var.Local 1))) l0l1 &&
    equal (add_taint l0l1 (Var.Var (lab 0)) (Taint.taint (Var.Local 1))) l0l1 &&
    equal (add_taint top (Var.Var (lab 0)) (Taint.taint (Var.Local 1))) top

  let%test "add_taint_v adds the taint as expected" =
    let open Instr.Label.Test in
    (* [i0 -> bottom *)
    let bot = Var.Map.of_alist_exn [(Var.Local 0, Taint.taint (Var.Local 0));
                                    (Var.Local 1, Taint.taint (Var.Local 1));
                                    (Var.Var (lab 0), Taint.bottom)] in
    (* [i0 -> l0] *)
    let l0 = Var.Map.of_alist_exn [(Var.Local 0, Taint.taint (Var.Local 0));
                                   (Var.Local 1, Taint.taint (Var.Local 1));
                                   (Var.Var (lab 0), Taint.taint (Var.Local 0))] in
    (* [i0 -> {l0,l1}] *)
    let l0l1 =  Var.Map.of_alist_exn [(Var.Local 0, Taint.taint (Var.Local 0));
                                      (Var.Local 1, Taint.taint (Var.Local 1));
                                      (Var.Var (lab 0), Taint.taints (Var.Set.of_list [Var.Local 0; Var.Local 1]))] in
    (* [i0 -> top] *)
    let top = Var.Map.of_alist_exn [(Var.Local 0, Taint.taint (Var.Local 0));
                                    (Var.Local 1, Taint.taint (Var.Local 1));
                                    (Var.Var (lab 0), Taint.top)] in
    equal (add_taint_v bot (Var.Var (lab 0)) (Var.Local 0)) l0 &&
    equal (add_taint_v l0 (Var.Var (lab 0)) (Var.Local 1)) l0l1 &&
    equal (add_taint_v l0l1 (Var.Var (lab 0)) (Var.Local 1)) l0l1 &&
    equal (add_taint_v top (Var.Var (lab 0)) (Var.Local 1)) top

end
