(* Implementation of dominance algorithms, based on  Cooper, Keith D.; Harvey, Timothy J; Kennedy, Ken (2001). "A Simple, Fast Dominance Algorithm". *)
open Core_kernel
open Helpers

(** A tree representation *)
module Tree = struct
  type t = {
    children: IntSet.t IntMap.t; (* Representation of the tree in a forward manner: edges from a node to children *)
    parent: int IntMap.t;  (* Representation of the tree in a backward manner: edge from a node to its parent *)
    entry : int; (* The entry node of the tree *)
    nodes : IntSet.t; (* All nodes of the tree *)
  }
  [@@deriving equal, compare]

  (** Converts the tree to its string representation *)
  let to_string (tree : t) : string =
    (IntMap.to_string tree.children IntSet.to_string)

  (** Return the children of a node in the tree *)
  let children (tree : t) (node : int) : IntSet.t =
    match IntMap.find tree.children node with
    | Some c -> c
    | None -> failwith (Printf.sprintf "No children found in tree %s, node %d" (to_string tree) node)

  (** Compute the reverse representation of a tree, from a map of nodes to their children to a map of nodes to their (only) parent *)
  let revert_tree_representation (entry : int) (tree : IntSet.t IntMap.t) : int IntMap.t =
    let rec visit (rtree : int IntMap.t) (node : int) =
      let children = match IntMap.find tree node with Some c -> c | None -> failwith "no children found when reverting tree computation" in
      let rtree_with_children = IntSet.fold children ~init:rtree ~f:(fun rtree child ->
          IntMap.add_exn rtree ~key:child ~data:node) in
      IntSet.fold children ~init:rtree_with_children ~f:visit
    in
    visit IntMap.empty entry

  (** Constructs a tree from its representation as a map (encoded as a list) from nodes to their children *)
  let of_children_map (entry : int) (children_map : (int * int list) list) : t =
    let check_validity_of_children_map = true in
    (* First check that the children map is correctly constructed *)
    if check_validity_of_children_map then begin
      (* - Every node that is listed as a children is also listed as having children (a node without children has an empty list of children *)
      let all_keys = IntSet.of_list (List.map children_map ~f:fst) in
      let all_children = List.fold_left children_map ~init:IntSet.empty ~f:(fun acc (_, cs) -> IntSet.union acc (IntSet.of_list cs)) in
      if not (IntSet.for_all all_children ~f:(fun c -> IntSet.mem all_keys c)) then
        failwith (Printf.sprintf "of_children_map: invalid tree spec  where not all nodes are specified: %s" (String.concat ~sep:" | " (List.map children_map ~f:(fun (p, cs) -> Printf.sprintf "%d -> %s" p (String.concat ~sep:"," (List.map cs ~f:string_of_int))))));
      (* - Every node is the children of only one node. We actually check that when concatenating all children, every element is unique. *)
      let all_children_l = List.fold_left children_map ~init:[] ~f:(fun acc (_, cs) -> cs @ acc) in
      if not ((IntSet.length all_children) = List.length all_children_l) then
        (* if both sizes are the same, then the list only contains unique elements *)
        failwith (Printf.sprintf "of_children_map: invalid tree spec where some children have multiple parents: %s" (String.concat ~sep:" | " (List.map children_map ~f:(fun (p, cs) -> Printf.sprintf "%d -> %s" p (String.concat ~sep:"," (List.map cs ~f:string_of_int))))));
    end;
    let children = IntMap.map (IntMap.of_alist_exn children_map) ~f:IntSet.of_list in
    { children;
      parent = revert_tree_representation entry children;
      entry;
      nodes = IntSet.of_list (IntMap.keys children) }

  (** Computes a spanning tree of a given graph *)
  let spanning_tree_from_graph (entry : int) (succs : int -> int list) : t =
    let rec dfs (tree : int list IntMap.t) (added : IntSet.t) (node : int) : (int list IntMap.t * IntSet.t) =
      if IntMap.mem tree node then
        (* node already visited, ignore it *)
        (tree, added)
      else
        let children = succs node in
        let new_children = List.filter children ~f:(fun child -> not (IntSet.mem added child)) in
        let tree_with_children_connected = IntMap.add_exn tree ~key:node ~data:new_children in
        let added' = IntSet.union added (IntSet.of_list new_children) in
        List.fold_left children ~init:(tree_with_children_connected, added') ~f:(fun (tree, added) node -> dfs tree added node)
    in
    of_children_map entry (IntMap.to_alist (fst (dfs IntMap.empty IntSet.empty entry)))

  (** Extract the parent of a node in constant time *)
  let parent (tree : t) (node : int) : int option =
    if node = tree.entry then
      None
    else IntMap.find tree.parent node

  (** Just like parent, but throws an exception if there is no parent *)
  let parent_exn (tree : t) (node : int) : int = match parent tree node with
    | Some p -> p
    | None -> failwith "Node has no parent"

  (** Changes the parent of a node *)
  let set_parent (tree : t) (node : int) (new_parent : int) : t =
    let disconnected_from_old_parent = match parent tree node with
      | Some old_parent -> IntMap.update tree.children old_parent ~f:(function
          | Some children -> IntSet.remove children node
          | None -> IntSet.empty)
      | None -> tree.children in
    { tree with parent = IntMap.update tree.parent node ~f:(fun _ -> new_parent);
                children = IntMap.update disconnected_from_old_parent new_parent ~f:(function
                    | Some children -> IntSet.add children node
                    | None -> IntSet.singleton node) }

  (** Helper function used internally to fold over ancestors of a given node *)
  let fold_ancestors (tree : t) (node : int) (init : 'a) (f : 'a -> int -> 'a) : 'a =
    let rec loop (node : int) (acc : 'a) : 'a =
      match parent tree node with
      | Some p -> loop p (f acc p)
      | None when node = tree.entry -> acc
      | None -> failwith (Printf.sprintf "fold_ancestors: missing parent link in tree? node %d has no parent" node) in
    loop node init

  (** Computes the nearest common ancestor of two nodes in a tree *)
  let nca (tree : t) (node1 : int) (node2 : int) : int option =
    if node1 = node2 then
      Some node2
    else
      (* Compute ancestors of node1 (including node1) *)
      let n1_ancestors = IntSet.add (fold_ancestors tree node1 IntSet.empty IntSet.add) node1 in
      if IntSet.mem n1_ancestors node2 then
        (* either n2 is in the ancestor of n1, then it is clearly the nca *)
        Some node2
      else
        (* othewsie, go over the ancestors of n1: we look for the first common
           ancestor, starting from node2 *)
        fold_ancestors tree node2 None (fun acc n ->
            match acc with
            | Some _ -> acc (* ancestor already found *)
            | None when IntSet.mem n1_ancestors n ->
              Some n (* nearest common ancestor found *)
            | None -> acc)

  module Test = struct
    let%test "revert tree computation" =
      let entry : int = 0 in
      let tree : IntSet.t IntMap.t = IntMap.map ~f:IntSet.of_list (IntMap.of_alist_exn [(0, [1; 2]); (1, [3; 4]); (2, [5]); (3, []); (4, []); (5, [])]) in
      let actual = revert_tree_representation entry tree in
      let expected : int IntMap.t = IntMap.of_alist_exn [(1, 0); (3, 1); (4, 1); (2, 0); (5, 2)] in
      IntMap.equal Int.(=) actual expected

    let%test "spanning tree computation" =
      let graph : int list IntMap.t = IntMap.of_alist_exn [(0, [4; 2]); (4, [5; 3]); (2, [5; 1]); (3, [1])] in
      let succs (n : int) : int list = match IntMap.find graph n with
        | Some ns -> ns
        | None -> [] in
      let entry = 0 in
      let actual = spanning_tree_from_graph entry succs in
      (* this is only one of the expected spanning trees *)
      let expected = of_children_map entry [(0, [4; 2]); (1, []); (2, []); (3, [1]); (4, [5; 3]); (5, [])] in
      equal actual expected

    let%test "spanning tree computation - other example" =
      let entry = 1 in
      let graph : int list IntMap.t = IntMap.of_alist_exn [(1, [2]); (2, [3; 4; 6]); (3, [5]); (4, [5]); (5, [2])] in
      let succs (n : int) : int list = match IntMap.find graph n with
        | Some ns -> ns
        | None -> [] in
      let actual = spanning_tree_from_graph entry succs in
      let expected = of_children_map entry [(1, [2]); (2, [3; 4; 6]); (3, [5]); (4, []); (5, []); (6, [])] in
      equal actual expected

    let%test "spanning tree computation - reverse example" =
      let entry = 6 in
      let rev_graph : int list IntMap.t = IntMap.of_alist_exn [(6, [2]); (5, [4; 3]); (4, [2]); (3, [2]); (2, [1; 5])] in
      let preds (n : int) : int list = match IntMap.find rev_graph n with
        | Some ns -> ns
        | None -> [] in
      let actual = spanning_tree_from_graph entry preds in
      let expected = of_children_map entry [(6, [2]); (2, [1; 5]); (5, [3; 4]); (3, []); (4, []); (1, [])] in
      equal actual expected

    let%test "spanning tree computation - other reverse example" =
      let entry = 7 in
      let rev_graph : int list IntMap.t = IntMap.of_alist_exn [(7, [6]); (6, [5]); (5, [4; 3]); (4, [3]); (3, [2]); (2, [])] in
      let preds (n : int) : int list = match IntMap.find rev_graph n with
        | Some ns -> ns
        | None -> [] in
      let actual = spanning_tree_from_graph entry preds in
      let expected = of_children_map entry [(2, []); (3, [2]); (4, []); (5, [3; 4]); (6, [5]); (7, [6])] in
      equal actual expected

    let%test "rtree nearest common ancestor"=
      let tree : t = of_children_map 0 [(0, [1; 2]); (1, [3; 4]); (2, [5]); (3, []); (4, []); (5, [])] in
      (match nca tree 3 1 with Some 1 -> true | _ -> false) &&
      (match nca tree 3 4 with Some 1 -> true | _ -> false) &&
      (match nca tree 0 4 with Some 0 -> true | _ -> false)
  end
end

(** Computation of the dominator tree using the algorithm from Cooper, Harvey
    and Kennedy (2001), based on the description made here:
    https://www.cs.au.dk/~gerth/advising/thesis/henrik-knakkegaard-christensen.pdf. *)
let dominator_tree (entry : int) (nodes : int list) (succs : int -> int list) (preds : int -> int list) : Tree.t =
  let rec loop (tree : Tree.t) : Tree.t =
    let (tree, changed) = List.fold_left nodes ~init:(tree, false) ~f:(fun (tree, changed) node1 ->
        List.fold_left (preds node1) ~init:(tree, changed) ~f:(fun (tree, changed) node2 ->
            let parent_n1 = match Tree.parent tree node1 with
              | Some p -> p
              | None -> failwith "dominator_tree accessing a node without parent" in
            let nca = match Tree.nca tree node2 parent_n1 with
              | Some n -> n
              | None -> failwith "dominator_tree accessing nodes without common ancestor" in
            if parent_n1 <> node2 && nca <> parent_n1 then
              (Tree.set_parent tree node1 nca, true)
            else
              (tree, changed))) in
    if changed then loop tree else tree
  in
  loop (Tree.spanning_tree_from_graph entry succs)

(** A dominator tree. TODO: use Tree.t instead *)
type domtree =
  | Branch of Spec.t Basic_block.t * Var.t * domtree list
  | Jump of Spec.t Basic_block.t * domtree list

(** Extract the final branch condition of a block, if there is any *)
let branch_condition (cfg : Spec.t Cfg.t) (block : Spec.t Basic_block.t) : Var.t option =
  match block.content with
  | Control c -> begin match c.instr with
      | BrIf _ | BrTable _ | If _ ->
        (* These are the only conditionals in the language, and they all depend
           on the top stack variable before their execution *)
        List.hd (Cfg.state_before_block cfg block.idx).vstack
      | _ -> None
    end
  | Data _ -> None

(** Computes the dominator tree of a CFG . *)
let cfg_dominator (cfg : Spec.t Cfg.t) : domtree =
  let entry : int = cfg.entry_block in
  let nodes : int list = IntMap.keys cfg.basic_blocks in
  let succs (node : int) : int list =
    let edges = Cfg.outgoing_edges cfg node in
    List.map edges ~f:fst
  in
  let preds (node : int) : int list =
    let back_edges = Cfg.incoming_edges cfg node in
    List.map back_edges ~f:fst
  in
  let tree : Tree.t = dominator_tree entry nodes succs preds in
  let rec build_tree (node : int) : domtree =
    let successors : domtree list = List.map (IntSet.to_list (Tree.children tree node)) ~f:build_tree in
    let block = Cfg.find_block_exn cfg node in
    match branch_condition cfg block with
    | Some v -> Branch (block, v, successors)
    | None -> Jump (block, successors) in
  build_tree entry

(** Computes the post-dominator tree of a CFG *)
let cfg_post_dominator (cfg : Spec.t Cfg.t) : Tree.t =
  let exit : int = cfg.exit_block in
  let nodes : int list = IntMap.keys cfg.basic_blocks in
  let succs (node : int) : int list =
    let edges = Cfg.outgoing_edges cfg node in
    List.map edges ~f:fst
  in
  let preds (node : int) : int list =
    let back_edges = Cfg.incoming_edges cfg node in
    List.map back_edges ~f:fst
  in
  (* Note that we invert succs and preds here, and start from exit, in order to have the post-dominator tree *)
  dominator_tree exit nodes preds succs

module Test = struct
  let%test "dominator tree is correctly computed - wikipedia example" =
    let entry = 1 in
    let graph : int list IntMap.t = IntMap.of_alist_exn [(1, [2]); (2, [3; 4; 6]); (3, [5]); (4, [5]); (5, [2])] in
    let succs (n : int) : int list = match IntMap.find graph n with
      | Some ns -> ns
      | None -> [] in
    let rev_graph : int list IntMap.t = IntMap.of_alist_exn [(6, [2]); (5, [4; 3]); (4, [2]); (3, [2]); (2, [1; 5])] in
    let preds (n : int) : int list = match IntMap.find rev_graph n with
      | Some ns -> ns
      | None -> [] in
    let actual : Tree.t = dominator_tree entry [1; 2; 3; 4; 5; 6] succs preds in
    let expected : Tree.t = Tree.of_children_map 1 [(1, [2]); (2, [3; 4; 5; 6]); (3, []); (4, []); (5, []); (6, [])] in
    Tree.equal actual expected

  let%test "post-dominator tree is correctly computed by reversing the order of the graph - wikipedia example" =
    let graph : int list IntMap.t = IntMap.of_alist_exn [(1, [2]); (2, [3; 4; 6]); (3, [5]); (4, [5]); (5, [2])] in
    let succs (n : int) : int list = match IntMap.find graph n with
      | Some ns -> ns
      | None -> [] in
    let rev_graph : int list IntMap.t = IntMap.of_alist_exn [(6, [2]); (5, [4; 3]); (4, [2]); (3, [2]); (2, [1; 5])] in
    let preds (n : int) : int list = match IntMap.find rev_graph n with
      | Some ns -> ns
      | None -> [] in
    (* Note the reverse succs/preds argument, as well as the entry point being 6 *)
    let actual = dominator_tree 6 [1; 2; 3; 4; 5; 6] preds succs in
    let expected = Tree.of_children_map 6 [(6, [2]); (2, [1; 5]); (5, [3; 4]); (1, []); (3, []); (4, [])] in
    Tree.equal actual expected

  let%test "post dominator computation" =
    let module_ = Wasm_module.of_string "(module
  (type (;0;) (func (param i32) (result i32)))
  (func (;test;) (type 0) (param i32) (result i32)
    block
      i32.const 1 ;; This is a branch condition
      br_if 0     ;; The condition depends on var 'Const 1', and this block has index 3
      i32.const 2
      local.get 0
      i32.add
      drop
    end
    local.get 0)
  (table (;0;) 1 1 funcref)
  (memory (;0;) 2)
  (global (;0;) (mut i32) (i32.const 66560)))" in
    let cfg = Spec_analysis.analyze_intra1 module_ 0l in
    let actual = cfg_post_dominator cfg in
    let expected = Tree.of_children_map 7 [(2, []); (3, [2]); (4, []); (5, [3; 4]); (6, [5]); (7, [6])] in
    Tree.equal actual expected
end
