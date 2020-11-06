(* Implementation of dominance algorithms, based on  Cooper, Keith D.; Harvey, Timothy J; Kennedy, Ken (2001). "A Simple, Fast Dominance Algorithm". *)
open Core_kernel
open Helpers

(** A dominance tree *)
type 'a t =
  | Branch of 'a Basic_block.t * Var.t * 'a t list
  | Jump of 'a Basic_block.t * 'a t list

(** Visits a graph in reverse postorder *)
let reverse_postorder (entry : int) (succs : int -> int list) : int list =
  let rec dfs (visited : int list) (node : int) : int list =
    if List.mem visited node ~equal:Int.(=) then
      visited
    else
      node :: (List.fold_left (succs node) ~init:visited ~f:dfs) in
  dfs [] entry (* no need to reserve as we pushed in front of the list *)

let%test "reverse postorder is correct - wikipedia example" =
  let graph : int list IntMap.t = IntMap.of_alist_exn [(0, [1; 2]); (1, [3]); (2, [3])] in
  let actual = reverse_postorder 0 (fun n -> match IntMap.find graph n with
      | Some ns -> ns
      | None -> []) in
  (* Any of these two orderings are valid reverse postorders *)
  let expected1 = [0; 1; 2; 3] in
  let expected2 = [0; 2; 1; 3] in
  (List.equal Int.(=) actual expected1) || (List.equal Int.(=) actual expected2)

(** The naive algorithm to compute a dominator tree, from Wikipedia.
    Returns a map that for each node, maps to its set of dominators.
    TODO This can be refined based on the approach described in:
       Cooper, Keith D.; Harvey, Timothy J; Kennedy, Ken (2001). "A Simple, Fast Dominance Algorithm".
    NOTE: the naive algorithm from Wikipedia is slightly different than the naive on in Cooper et al.!*)
let dominator_tree (entry : int) (nodes : int list) (_succs : int -> int list) (preds : int -> int list) : IntSet.t IntMap.t =
  (* let nodes_reverse_postorder : int list = reverse_postorder entry succs in *)
  let nodes_without_entry = List.filter nodes ~f:(fun n -> not Int.(n = entry)) in
  let dom0 : IntSet.t IntMap.t = IntMap.of_alist_exn (List.map nodes ~f:(fun n ->
      if n = entry then
        (n, IntSet.singleton n)
      else
        (n, IntSet.of_list nodes))) in
  let rec loop dom =
    let dom', changed =
      List.fold_left nodes_without_entry ~init:(dom, false) ~f:(fun (dom, changed) n ->
          let new_set =
            IntSet.union (IntSet.singleton n)
              (match List.reduce
                       (List.map (preds n) ~f:(fun p -> IntMap.find_exn dom p))
                       ~f:IntSet.inter with
              | Some r -> r
              | None -> IntSet.empty) in
          if not (IntSet.equal new_set (IntMap.find_exn dom n)) then
            (IntMap.set dom ~key:n ~data:new_set, true)
          else
            (dom, changed)) in
    if changed then loop dom' else dom'
  in
  loop dom0

let%test "dominator tree is correctly computed - wikipedia example" =
  let graph : int list IntMap.t = IntMap.of_alist_exn [(1, [2]); (2, [3; 6]); (3, [5]); (4, [5]); (5, [2])] in
  let rev_graph : int list IntMap.t = IntMap.of_alist_exn [(6, [2]); (5, [4; 3]); (4, [2]); (3, [2]); (2, [1])] in
  let succs (n : int) : int list = match IntMap.find graph n with
    | Some ns -> ns
    | None -> [] in
  let preds (n : int) : int list = match IntMap.find rev_graph n with
    | Some ns -> ns
    | None -> [] in
  let actual = dominator_tree 1 [1; 2; 3; 4; 5; 6] succs preds in
  let expected = IntMap.of_alist_exn [(1, IntSet.singleton 1);
                                      (2, IntSet.of_list [1; 2]);
                                      (3, IntSet.of_list [1; 2; 3]);
                                      (4, IntSet.of_list [1; 2; 4]);
                                      (5, IntSet.of_list [1; 2; 5]);
                                      (6, IntSet.of_list [1; 2; 6])] in
  IntMap.equal IntSet.equal actual expected


(*
val branch_condition : Spec_inference.state t -> Var.t option


(** Computes the dominance tree of a CFG *)
let dominance (cfg : Spec_inference.state Cfg.t) : Spec_inference.state t =
  let entry : int = cfg.entry_block in
  let nodes : int list = IntMap.keys cfg.basic_blocks in
  let succs (node : int) : int list =
    let edges = IntMap.find_exn cfg.edges node in
    List.map edges ~f:fst
  in
  let preds (node : int) : int list =
    let back_edges = IntMap.find_exn cfg.back_edges node in
    List.map back_edges ~f:fst
  in
  let edges : IntSet.t IntMap.t = dominance_naive entry nodes succs preds in
  let rec build_tree (node : int) : Spec_inference.state t =
    let successors : Spec_inference.state t list = match IntMap.find edges node with
      | Some ns -> List.map (IntSet.to_list ns) ~f:build_tree
      | None -> [] in
    let block = Cfg.find_block_exn cfg node in
    match Basic_block.branch_condition block with
    | Some v -> Branch (block, v, successors)
    | None -> Jump (block, successors) in
  build_tree entry
  *)
