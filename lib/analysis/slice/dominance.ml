(* Implementation of dominance algorithms, based on  Cooper, Keith D.; Harvey, Timothy J; Kennedy, Ken (2001). "A Simple, Fast Dominance Algorithm". *)
open Core_kernel
open Helpers

module Graph = struct
  type 'a t = {
    entry: 'a;
    exit: 'a;
    nodes: 'a list;
    succs : 'a -> 'a list;
    preds : 'a -> 'a list;
  }

  let reverse (graph : 'a t) : 'a t =
    { entry = graph.exit; exit = graph.entry; nodes = graph.nodes; succs = graph.preds; preds = graph.succs; }

  let of_alist_exn (entry : int) (exit : int) (desc : (int * int list) list) : 'a t =
    let nodes = List.concat_map desc ~f:(fun (from, to_) -> from :: to_) in
    let map : int list IntMap.t = IntMap.of_alist_exn desc in
    let succs n = match IntMap.find map n with
      | Some ns -> ns
      | None -> [] in
    let rev_map : int list IntMap.t = List.fold_left desc ~init:IntMap.empty ~f:(fun acc (k, vs) ->
        List.fold_left vs ~init:acc ~f:(fun acc v -> IntMap.add_multi acc ~key:v ~data:k)) in
    let preds n = match IntMap.find rev_map n with
      | Some ns -> ns
      | None -> [] in
    { entry; nodes; succs; preds; exit }

  (** Computes a spanning tree *)
  let spanning_tree (graph : int t) : Tree.t =
    let rec dfs (tree : int list IntMap.t) (added : IntSet.t) (node : int) : (int list IntMap.t * IntSet.t) =
      if IntMap.mem tree node then
        (* node already visited, ignore it *)
        (tree, added)
      else
        let children = graph.succs node in
        let new_children = List.filter children ~f:(fun child -> not (IntSet.mem added child)) in
        let new_children_no_duplicate = IntSet.to_list (IntSet.of_list new_children) in
        let tree_with_children_connected = IntMap.add_exn tree ~key:node ~data:new_children_no_duplicate in
        let added' = IntSet.union added (IntSet.of_list new_children_no_duplicate) in
        List.fold_left children ~init:(tree_with_children_connected, added') ~f:(fun (tree, added) node -> dfs tree added node)
    in
    Tree.of_children_map graph.entry (IntMap.to_alist (fst (dfs IntMap.empty IntSet.empty graph.entry)))

  let%test "spanning tree computation" =
    let entry = 0 in
    let exit = 1 in (* does not matter here *)
    let graph = of_alist_exn entry exit [(0, [4; 2]); (4, [5; 3]); (2, [5; 1]); (3, [1])] in
    let actual = spanning_tree graph in
    (* this is only one of the expected spanning trees *)
    let expected = Tree.of_children_map entry [(0, [4; 2]); (1, []); (2, []); (3, [1]); (4, [5; 3]); (5, [])] in
    Tree.check_equality ~actual ~expected

  let%test "spanning tree computation - other example" =
    let entry = 1 in
    let exit = 6 in (* does not matter here *)
    let graph = of_alist_exn entry exit [(1, [2]); (2, [3; 4; 6]); (3, [5]); (4, [5]); (5, [2])] in
    let actual = spanning_tree graph in
    let expected = Tree.of_children_map entry [(1, [2]); (2, [3; 4; 6]); (3, [5]); (4, []); (5, []); (6, [])] in
    Tree.check_equality ~actual ~expected

  let%test "spanning tree computation - reverse example" =
    let entry = 1 in
    let exit = 6 in
    let rev_graph = reverse (of_alist_exn entry exit [(1, [2]); (2, [3; 4; 6]); (3, [5]); (4, [5]); (5, [2])]) in
    let actual = spanning_tree rev_graph in
    let expected = Tree.of_children_map exit [(6, [2]); (2, [1; 5]); (5, [3; 4]); (3, []); (4, []); (1, [])] in
    Tree.check_equality ~actual ~expected

  let%test "spanning tree computation - other reverse example" =
    let entry = 2 in
    let exit = 7 in
    let rev_graph = reverse (of_alist_exn entry exit [(2, [3]); (3, [4; 5]); (4, [5]); (5, [6]); (6, [7])]) in
    let actual = spanning_tree rev_graph in
    let expected = Tree.of_children_map exit [(2, []); (3, [2]); (4, []); (5, [3; 4]); (6, [5]); (7, [6])] in
    Tree.check_equality ~actual ~expected

  let%test "spanning tree computation - loop" =
    let entry = 1 in
    let exit = 13 in (* does not matter here *)
    let graph = of_alist_exn entry exit [(1, [2]); (2, [3]); (3, [4; 11]); (11, [13]); (4, [5]); (5, [6]); (6, [7]); (7, [5])] in
    let actual = spanning_tree graph in
    let expected = Tree.of_children_map entry [(1, [2]); (2, [3]); (3, [4; 11]); (11, [13]); (4, [5]); (5, [6]); (6, [7]); (7, []); (13, [])] in
    Tree.check_equality ~actual ~expected

  (** Computation of the dominator tree using the algorithm from Cooper, Harvey
      and Kennedy (2001), based on the description made here:
      . *)
  let dominator_tree (graph : 'a t) : Tree.t =
    let rec loop (tree : Tree.t) : [ `Result of Tree.t | `Unreachable of int ] =
      let (tree, changed, unreachable) =
        List.fold_left graph.nodes ~init:(tree, false, []) ~f:(fun (tree, changed, unreachable) node1 ->
            List.fold_left (graph.preds node1) ~init:(tree, changed, unreachable) ~f:(fun (tree, changed, unreachable) node2 ->
                match Tree.parent tree node1 with
                | Some parent_n1 ->
                  begin try
                    let nca = match Tree.nca tree node2 parent_n1 with
                      | Some n -> n
                      | None -> failwith "dominator_tree accessing nodes without common ancestor" in
                    if parent_n1 <> node2 && nca <> parent_n1 then
                      (Tree.set_parent tree node1 nca, true, unreachable)
                    else
                      (tree, changed, unreachable)
                  with _ ->
                    (tree, changed, node2 :: unreachable)
                end
                | None ->
                  (* This is an unreachable node, part of an infinite loop.
                     Don't visit it and mark it as unreachable *)
                  (tree, changed, node1 :: unreachable))) in
      if List.is_empty unreachable then
        begin if changed then loop tree else `Result tree end
      else
        `Unreachable (List.hd_exn unreachable)
    in
    let rec outer_loop (graph : 'a t) : Tree.t =
      match loop (spanning_tree graph) with
      | `Result res -> res
      | `Unreachable node ->
        (* There has been unreachable nodes.
           We will therefore compute one of the dominator trees, according to Chris Dodd's answer here:
           https://stackoverflow.com/questions/35399281/how-can-i-build-the-post-dominator-tree-of-a-function-with-an-endless-loop
           To do so, we add an edge from the entry of the graph to the one of the unreachable node and try again *)
        outer_loop {
          graph with
          succs = (fun n -> if n = graph.entry then node :: (graph.succs n) else graph.succs n);
          preds = (fun n -> if n = node then graph.entry :: (graph.preds n) else graph.preds n);
        } in
    outer_loop graph
end

let graph_of_cfg (cfg : 'a Cfg.t) : int Graph.t =
  let entry = cfg.entry_block in
  let exit = cfg.exit_block in
  let nodes = IntMap.keys cfg.basic_blocks in
  let succs (node : int) : int list =
    let edges = Cfg.outgoing_edges cfg node in
    List.map edges ~f:fst
  in
  let preds (node : int) : int list =
    let back_edges = Cfg.incoming_edges cfg node in
    List.map back_edges ~f:fst in
  { entry; exit; nodes; succs; preds; }

(** Extract the final branch condition of a block, if there is any *)
let branch_condition (cfg : Spec.t Cfg.t) (block : Spec.t Basic_block.t) : Var.t option =
  match block.content with
  | Control c -> begin match c.instr with
      | BrIf _ | BrTable _ | If _ ->
        (* These are the only conditionals in the language, and they all depend
           on the top stack variable before their execution *)
        begin match (Cfg.state_before_block cfg block.idx (Spec_inference.init_state cfg)) with
        | Spec.Bottom -> None
        | Spec.NotBottom s -> List.hd s.vstack
        end
      | _ -> None
    end
  | Data _ -> None

(** Computes the dominator tree of a CFG . *)
let cfg_dominator (cfg : Spec.t Cfg.t) : Tree.t =
  Graph.dominator_tree (graph_of_cfg (Cfg.without_empty_nodes_with_no_predecessors cfg))

(** Computes the post-dominator tree of a CFG *)
let cfg_post_dominator (cfg : Spec.t Cfg.t) : Tree.t =
  (* Note that we invert succs and preds here, and start from exit, in order to have the post-dominator tree *)
  Graph.dominator_tree (Graph.reverse (graph_of_cfg (Cfg.without_empty_nodes_with_no_predecessors cfg)))

module Test = struct
  let%test "dominator tree is correctly computed - wikipedia example" =
    let entry = 1 in
    let exit = 6 in
    let graph = Graph.of_alist_exn entry exit [(1, [2]); (2, [3; 4; 6]); (3, [5]); (4, [5]); (5, [2])] in
    let actual : Tree.t = Graph.dominator_tree graph in
    let expected : Tree.t = Tree.of_children_map 1 [(1, [2]); (2, [3; 4; 5; 6]); (3, []); (4, []); (5, []); (6, [])] in
    Tree.check_equality ~actual ~expected

  let%test "post-dominator tree is correctly computed by reversing the order of the graph - wikipedia example" =
    let entry = 1 in
    let exit = 6 in
    let rev_graph = (Graph.reverse (Graph.of_alist_exn entry exit [(1, [2]); (2, [3; 4; 6]); (3, [5]); (4, [5]); (5, [2])])) in
    let actual = Graph.dominator_tree rev_graph in
    let expected = Tree.of_children_map 6 [(6, [2]); (2, [1; 5]); (5, [3; 4]); (1, []); (3, []); (4, [])] in
    Tree.check_equality ~actual ~expected

  let%test "post dominator computation should produce expected results on a simple function" =
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
    Tree.check_equality ~actual ~expected

  let%test_unit "post dominator should work with CFGs that have empty blocks with no predecessors" =
    let module_ = Wasm_module.of_string "(module
  (type (;0;) (func))
  (func (;6;) (type 0) ;; int main()
    (local i32 i32 i32)
    ;; Local 0: i
    ;; Local 1: x
    ;; Local 2: c
    block
      loop
        local.get 0
        i32.const 0
        i32.ne
        br_if 1
        local.get 2
        if  ;; label = @3
          i32.const 0
          ;; The following instruction is the slicing criterion
          local.set 1 ;; x = result of f()
          i32.const 0
          local.set 2 ;; c = result of g()
        end
        local.get 0
        local.set 0 ;; i = result of h(i)
        br 0
      end
    end)
  (table (;0;) 1 1 funcref)
  (memory (;0;) 2)
  (global (;0;) (mut i32) (i32.const 66560)))" in
    let cfg = Spec_analysis.analyze_intra1 module_ 0l in
    let _actual = cfg_post_dominator cfg in
    ()

  let%test_unit "post-dominator tree works on a CFG that has one node with two edges to the same successor" =
    let module_ = Wasm_module.of_string "(module
  (type (;0;) (func (result i32)))
  (func (;0;) (type 0) (result i32)
    block  ;; label = @1
      i32.const 33
      drop
      i32.const 1
      br_if 0 (;@1;)
    end
    block (result i32)  ;; label = @1
      i32.const 42
      i32.const 1
      br_if 1 (;@0;)
      drop
      i32.const 11
    end
    drop
    loop (result i32)  ;; label = @1
      i32.const 0
      br_if 0 (;@1;)
      i32.const 99
    end
    return))" in
    let cfg = Spec_analysis.analyze_intra1 module_ 0l in
    let _actual = cfg_post_dominator cfg in
    ()

  let%test_unit "post-dominator tree is computed on a CFG that contains an infinite loop" =
    let module_ = Wasm_module.of_string "(module
  (type (;0;) (func (result i32)))
  (func (;0;) (type 0) (result i32)
    loop
      br 0
    end
    unreachable))" in
    let cfg = Spec_analysis.analyze_intra1 module_ 0l in
    let _actual = cfg_post_dominator cfg in
    ()
end

