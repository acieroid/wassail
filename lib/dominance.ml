(* Implementation of dominance algorithms, based on  Cooper, Keith D.; Harvey, Timothy J; Kennedy, Ken (2001). "A Simple, Fast Dominance Algorithm". *)
open Core_kernel
open Helpers

module Tree = struct
  type t = {
    children: IntSet.t IntMap.t; (* Representation of the tree in a forward manner: edges from a node to children *)
    parent: int IntMap.t;  (* Representation of the tree in a backward manner: edge from a node to its parent *)
    entry : int; (* The entry node of the tree *)
    nodes : IntSet.t; (* All nodes of the tree *)
  }
  [@@deriving equal, compare]

  let to_string (tree : t) : string =
    (IntMap.to_string tree.children IntSet.to_string)

  (** Compute the reverse representation of a tree, from a map of nodes to their children to a map of nodes to their (only) parent *)
  let revert_tree_representation (entry : int) (tree : IntSet.t IntMap.t) : int IntMap.t =
    let rec visit (rtree : int IntMap.t) (node : int) =
      let children = IntMap.find_exn tree node in
      let rtree_with_children = IntSet.fold children ~init:rtree ~f:(fun rtree child ->
          IntMap.add_exn rtree ~key:child ~data:node) in
      IntSet.fold children ~init:rtree_with_children ~f:visit
    in
    visit IntMap.empty entry

  let%test "revert tree computation" =
    let entry : int = 0 in
    let tree : IntSet.t IntMap.t = IntMap.map ~f:IntSet.of_list (IntMap.of_alist_exn [(0, [1; 2]); (1, [3; 4]); (2, [5]); (3, []); (4, []); (5, [])]) in
    let actual = revert_tree_representation entry tree in
    let expected : int IntMap.t = IntMap.of_alist_exn [(1, 0); (3, 1); (4, 1); (2, 0); (5, 2)] in
    IntMap.equal Int.(=) actual expected

  (** Constructs a tree from its representation as a map (encoded as a list) from nodes to their children *)
  let of_children_map (entry : int) (children_map : (int * int list) list) : t =
    let check_validity_of_children_map = true in
    (* First check that the children map is correctly constructed *)
    if check_validity_of_children_map then begin
      (* - Every node that is listed as a children is also listed as having children (a node without children has an empty list of children *)
      let all_keys = IntSet.of_list (List.map children_map ~f:fst) in
      let all_children = List.fold_left children_map ~init:IntSet.empty ~f:(fun acc (_, cs) -> IntSet.union acc (IntSet.of_list cs)) in
      if not (IntSet.for_all all_children ~f:(fun c -> IntSet.mem all_keys c)) then
        failwith (Printf.sprintf "of_children_map: invalid tree spec where not all nodes are specified: %s" (String.concat ~sep:" | " (List.map children_map ~f:(fun (p, cs) -> Printf.sprintf "%d -> %s" p (String.concat ~sep:"," (List.map cs ~f:string_of_int))))));
      (* - Every node is the children of only one node. We actually check that when concatenating all children, every element is unique. *)
      let all_children_l = List.fold_left children_map ~init:[] ~f:(fun acc (_, cs) -> cs @ acc) in
      if not ((IntSet.length all_children) = List.length all_children_l) then
        (* if both sizes are the same, then the list only contains unique elements *)
        failwith "of_children_map: invalid tree spec where some children have multiple parents"
    end;
    let children = IntMap.map (IntMap.of_alist_exn children_map) ~f:IntSet.of_list in
    { children;
      parent = revert_tree_representation entry children;
      entry;
      nodes = IntSet.of_list (IntMap.keys children) }

  (** Computes a spanning tree of a given graph *)
  let spanning_tree_from_graph (entry : int) (succs : int -> int list) : t =
    let rec dfs (tree : int list IntMap.t) (node : int) : int list IntMap.t =
      if IntMap.mem tree node then
        (* already visited *)
        tree
      else
        let children = succs node in
        let unvisited_children = List.filter children ~f:(fun child -> not (IntMap.mem tree child)) in
        let tree_with_children_connected = IntMap.add_exn tree ~key:node ~data:unvisited_children in
        List.fold_left children ~init:tree_with_children_connected ~f:dfs
    in
    Printf.printf "call of_children_map\n";
    of_children_map entry (IntMap.to_alist (dfs IntMap.empty entry))

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
      | None -> failwith "missing parent link in tree?" in
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

  let%test "rtree nearest common ancestor"=
    let tree : t = of_children_map 0 [(0, [1; 2]); (1, [3; 4]); (2, [5]); (3, []); (4, []); (5, [])] in
    (match nca tree 3 1 with Some 1 -> true | _ -> false) &&
    (match nca tree 3 4 with Some 1 -> true | _ -> false) &&
    (match nca tree 0 4 with Some 0 -> true | _ -> false)
end


(*
let revert_rtree_representation (entry : int) (rtree : int IntMap.t) : int list IntMap.t =
  let add_parent_children (tree : int list IntMap.t) (node : int) =
    if node = entry then
      tree
    else
      let parent = IntMap.find_exn rtree node in
      IntMap.update tree parent ~f:(function
          | Some children -> node :: children
          | None -> [node]) in
  List.fold_left (IntMap.keys rtree) ~init:IntMap.empty ~f:add_parent_children


let%test "revert rtree computation"=
  let entry : int = 0 in
  let rtree : int IntMap.t = IntMap.of_alist_exn [(1, 0); (3, 1); (4, 1); (2, 0); (5, 2)] in
  let actual = revert_rtree_representation entry rtree in
  let expected : int list IntMap.t = IntMap.of_alist_exn [(0, [1; 2]); (1, [3; 4]); (2, [5])] in
  IntMap.equal (fun a b -> IntSet.equal (IntSet.of_list a) (IntSet.of_list b)) actual expected

(** Extracts th parent of a node from a rtree *)
let rtree_parent (rtree : int IntMap.t) (node : int) : int =
  IntMap.find_exn rtree node

(** Changes the paraent of a node in an rtree *)
let rtree_set_parent (rtree : int IntMap.t) (node : int) (parent : int) : int IntMap.t =
  IntMap.update rtree node ~f:(fun _ -> parent)
*)

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

let%test "dominator tree is correctly computed - wikipedia example" =
  let entry = 1 in
  let graph : int list IntMap.t = IntMap.of_alist_exn [(1, [2]); (2, [3; 4; 6]); (3, [5]); (4, [5]); (5, [2])] in
  let succs (n : int) : int list = match IntMap.find graph n with
    | Some ns -> ns
    | None -> [] in
  let rev_graph : int list IntMap.t = IntMap.of_alist_exn [(6, [2]); (5, [4; 3]); (4, [2]); (3, [2]); (2, [1])] in
  let preds (n : int) : int list = match IntMap.find rev_graph n with
    | Some ns -> ns
    | None -> [] in
  Printf.printf "spanning\n";
  let spanning = Tree.spanning_tree_from_graph entry succs in
  Printf.printf "spanning: %s\n" (Tree.to_string spanning);
  let actual : Tree.t = dominator_tree entry [1; 2; 3; 4; 5; 6] succs preds in
  let expected : Tree.t = Tree.of_children_map 1 [(1, [2]); (2, [3; 4; 5; 6]); (3, []); (4, []); (5, []); (6, [])] in
  Tree.equal actual expected

(*
let%test "post-dominator tree is correctly computed by reversing the order of the graph - wikipedia example" =
  let graph : int list IntMap.t = IntMap.of_alist_exn [(1, [2]); (2, [3; 4; 6]); (3, [5]); (4, [5]); (5, [2])] in
  let succs (n : int) : int list = match IntMap.find graph n with
    | Some ns -> ns
    | None -> [] in
  let rev_graph : int list IntMap.t = IntMap.of_alist_exn [(6, [2]); (5, [4; 3]); (4, [2]); (3, [2]); (2, [1])] in
  let preds (n : int) : int list = match IntMap.find rev_graph n with
    | Some ns -> ns
    | None -> [] in
  (* Note the reverse succs/preds argument, as well as the entry point being 6 *)
  let actual = dominator_tree 6 [1; 2; 3; 4; 5; 6] preds succs in
  Printf.printf "%s\n" (Tree.to_string actual);
  false
*)
(*

(** A dominance tree *)
type t =
  | Branch of Spec_inference.state Basic_block.t * Var.t * t list
  | Jump of Spec_inference.state Basic_block.t * t list

let branch_condition (_block : Spec_inference.state Basic_block.t) : Var.t option =
  failwith "TODO"

(** Computes the dominance tree of a CFG. *)
let dominance (cfg : Spec_inference.state Cfg.t) : t =
  let exit : int = cfg.exit_block in
  let nodes : int list = IntMap.keys cfg.basic_blocks in
  let preds (node : int) : int list =
    let back_edges = IntMap.find_exn cfg.back_edges node in
    List.map back_edges ~f:fst
  in
  let edges : IntSet.t IntMap.t = dominator_tree entry nodes preds in
  let rec build_tree (node : int) : t =
    let successors : t list = match IntMap.find edges node with
      | Some ns -> List.map (IntSet.to_list ns) ~f:build_tree
      | None -> [] in
    let block = Cfg.find_block_exn cfg node in
    match branch_condition block with
    | Some v -> Branch (block, v, successors)
    | None -> Jump (block, successors) in
  build_tree entry

(** Computes the post-dominance tree of a CFG *)
let post_dominance (cfg : Spec_inference.state Cfg.t) : IntSet.t IntMap.t =
  let entry : int = cfg.entry_block in
  let nodes : int list = IntMap.keys cfg.basic_blocks in
  let succs (node : int) : int list =
    let edges = IntMap.find_exn cfg.edges node in
    List.map edges ~f:fst
  in
  let edges : IntSet.t IntMap.t = dominator_tree entry nodes succs in (* Note that we pass succs rather than pred here, in order to have the post-dominator tree *)
  edges


(* TODO: compute immediate post-dominators *)

(* Analogous to the definition of dominance above, a node z is said to post-dominate a node n if all paths to the exit node of the graph starting at n must go through z. Similarly, the immediate post-dominator of a node n is the postdominator of n that doesn't strictly postdominate any other strict postdominators of n. *)
(*
(* Algorithm adapted from https://homepages.dcc.ufmg.br/~fernando/classes/dcc888/ementa/slides/ProgramSlicing.pdf *)
let control_dep (root : t) (is_immediate_post_dom : t -> Var.t -> bool) =
  let push (tree : t) (preds : Var.t list) : Var.t list = match tree with
    | Branch (_, p,_) -> p :: preds
    | Jump (_, _) -> preds in
  (* Creates the edges from a given block, where each edge links a block index to a control variable it depends on *)
  let link (block : Spec_inference.state Basic_block.t) (pred : Var.t) : (Var.t * Var.t) list = match block.content with
    | ControlMerge -> failwith "TODO"
    | Control instr -> begin match instr.instr with
        | Block _ | Loop _ -> []
        | If _ -> failwith "top of the stack depends on pred"
        | Call _ -> failwith "n arguments from the stack dpend on pred"
        | CallIndirect _ -> failwith "n+1 arguments from the stack depend on pred"
        | Br _ -> []
        | BrIf _ -> failwith "top of the stack depends on pred"
        | BrTable _ -> failwith "top of the stack depends on pred"
        | Return -> failwith "arity of the block from the stack depend on pred"
        | Unreachable -> []
      end
    | Data instrs -> List.fold_left instrs ~init:[] ~f:(fun acc instr ->
        let to_add = match  instr.instr with
          | _ -> TODO
        in to_add @ acc) in
  (* vchildren simply recurses down the tree *)
  let rec vchildren (blocks : t list) (preds : Var.t list)  = match blocks with
    | [] -> []
    | (n :: ns) -> vnode n preds @ vchildren ns preds
  (* vnode visits a tree node *)
  and vnode (tree : t) (preds : Var.t list) =
    (* Extract the block and its children from the root of the tree *)
    let (block, children) = match tree with
    | Branch (block, _, children) -> (block, children)
    | Jump (block, children) -> (block, children)  in
    match preds with
    | h :: t when is_immediate_post_dom tree h ->
      vnode tree t
    | [] -> vchildren children (push tree preds)
    | h :: _ ->
      link block h @ vchildren children (push tree preds) in
  vnode root []
*)
*)
