open Core_kernel
open Helpers

module Spec = struct
  (** The state is a specification of the runtime components *)
  type t = {
    vstack : Var.t list;
    locals : Var.t list;
    globals : Var.t list;
    memory : Var.t Var.OffsetMap.t;
  }
  [@@deriving compare, equal]

  let to_string (s : t) : string =
    Printf.sprintf "{\nvstack: [%s]\nlocals: [%s]\nglobals: [%s]\nmemory: [%s]\n}"
      (String.concat ~sep:", " (List.map s.vstack ~f:Var.to_string))
      (String.concat ~sep:", " (List.map s.locals ~f:Var.to_string))
      (String.concat ~sep:", " (List.map s.globals ~f:Var.to_string))
      (String.concat ~sep:", " (List.map (Var.OffsetMap.to_alist s.memory) ~f:(fun ((k, offset), v) -> Printf.sprintf "%s+%d: %s" (Var.to_string k) offset (Var.to_string v))))

  let to_dot_string (s : t) : string =
    Printf.sprintf "[%s] [%s]"
      (String.concat ~sep:", " (List.map s.vstack ~f:Var.to_string))
  (* (String.concat ~sep:", " (List.map s.locals ~f:Var.to_string))*)
  (String.concat ~sep:", " (List.map (Var.OffsetMap.to_alist s.memory) ~f:(fun ((k, offset), v) -> Printf.sprintf "%s+%d: %s" (Var.to_string k) offset (Var.to_string v))))


  let map_vars (s : t) ~(f : Var.t -> Var.t) : t =
    { vstack = List.map s.vstack ~f:f;
      locals = List.map s.locals ~f:f;
      globals = List.map s.globals ~f:f;
      memory = Var.OffsetMap.map_vars s.memory ~f:f }

  (** Returns all variables contained in the memory of a state *)
  let memvars (s : t) : Var.t list =
    (List.concat (List.map (Var.OffsetMap.to_alist s.memory)
                    ~f:(fun ((k, _offset), v) -> [k; v])))

  (** Returns all the variables contained in the state *)
  let vars_of (s : t) : Var.Set.t =
    Var.Set.union (Var.Set.of_list s.vstack)
      (Var.Set.union (Var.Set.of_list s.locals)
         (Var.Set.union (Var.Set.of_list s.globals)
            (Var.Set.of_list (memvars s))))

  (** Returns all the variables contained in the spec map *)
  let vars (data : (t * t) IntMap.t) : Var.Set.t =
    List.fold_left (IntMap.to_alist data)
      ~init:Var.Set.empty
      ~f:(fun acc (_, (pre, post)) ->
          Var.Set.union acc (Var.Set.union (vars_of pre) (vars_of post)))

  (** Extract vars that have changed between two states.
      Represent these changes as a list of pairs, where the first element is the original variable,
      and the second element is the new variable *)
  let extract_different_vars (s1 : t) (s2 : t) : (Var.t * Var.t) list =
    let f (l1 : Var.t list) (l2 : Var.t list) : (Var.t * Var.t) list =
      assert (List.length l1 = List.length l2);
      List.filter_map (List.map2_exn l1 l2 ~f:(fun v1 v2 -> (v1, v2, Var.equal v1 v2)))
        ~f:(fun (v1, v2, eq) -> if not eq then Some (v1, v2) else None) in
    let fvstack (l1 : Var.t list) (l2 : Var.t list) : (Var.t * Var.t) list =
      (* Like f, but only checks a prefix.
           For example, it can sometimes happen that on one path we have [x, y] as the vstack, and another we have [y].
           We can safely assume that if the code has passed validation, then y will never be used.
           Hence, it is safe to treat the first vstack as if it was [x] *)
      let min_size = min (List.length l1) (List.length l2) in
      f (List.take l1 min_size) (List.take l2 min_size) in
    let fmap (m1 : Var.t Var.OffsetMap.t) (m2 : Var.t Var.OffsetMap.t) : (Var.t * Var.t) list =
      List.filter_map ((Var.OffsetMap.keys m1) @ (Var.OffsetMap.keys m2)) ~f:(fun k ->
          match (Var.OffsetMap.find m1 k, Var.OffsetMap.find m2 k) with
          | Some v1, Some v2 ->
            (* The key is present in both memories, check if they map to different values *)
            if Var.equal v1 v2 then None else Some (v1, v2)
          | _ ->
            (* The key is only present in one memory, hence the value is not considered different *)
            None) in
    (fvstack s1.vstack s2.vstack) @ (f s1.locals s2.locals) @ (f s1.globals s2.globals) @ (fmap s1.memory s2.memory)

  (** Extract vars that have been redefined in a merge block *)
  let new_merge_variables (cfg : t Cfg.t) (merge_block : t Basic_block.t) : (Var.t * Var.t) list =
    (* The predecessors of merge_block *)
    let preds = Cfg.predecessors cfg merge_block.idx in
    let state_after = Cfg.state_after_block cfg merge_block.idx in
    List.fold_left preds ~init:[] ~f:(fun acc pred_idx ->
        let state_before = Cfg.state_after_block cfg pred_idx in
        (extract_different_vars state_before state_after) @ acc)

  let ret (i : t Instr.t) : Var.t =
    List.hd_exn (Instr.annotation_after i).vstack
end

include Spec

