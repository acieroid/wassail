open Core
open Helpers

(** A lexical successor tree is a tree where a node a is the parent of a node
     b if it is one of its immediate successor *)
type t = {
  successors: Tree.t;
  index_to_instr: unit Instr.t IntMap.t;
  block_entries : int Instr.Label.Map.t; (* Maps block labels to their instruction's index *)
  no_final_node : bool;
}
[@@deriving compare, equal]

let block_exit (t : t) (block_label : Instr.Label.t) : 'a Instr.t =
  let entry = Instr.Label.Map.find_exn t.block_entries block_label in
  match Tree.parent t.successors entry with
  | Some node -> IntMap.find_exn t.index_to_instr node
  | None -> failwith "could not find block exit"

let make (instrs : unit Instr.t list) : t =
  let idx = ref 0 in
  let next_idx () : int =
    idx := !idx + 1;
    !idx in
  let add_parent (roots : Tree.t list) (idx : int) : Tree.t =
    if List.is_empty roots then
      Tree.of_node idx
    else
      (* This node is a successor of all the roots that we have *)
      Tree.merge roots idx in
  let rec helper
      (instrs : unit Instr.t list)
      (roots : Tree.t list)
      (indices : unit Instr.t IntMap.t)
      (entries : int Instr.Label.Map.t) :
    Tree.t list * unit Instr.t IntMap.t * int Instr.Label.Map.t =
    match instrs with
    | [] -> roots, indices, entries
    | Data instr :: rest ->
      let idx = next_idx () in
      let roots' = [add_parent roots idx] in
      let indices' = IntMap.add_exn indices ~key:idx ~data:(Data instr) in
      helper rest roots' indices' entries
    | Control instr :: rest ->
      let idx = next_idx () in
      let indices' = IntMap.add_exn indices ~key:idx ~data:(Control instr) in
      begin match instr.instr with
        | Merge -> failwith "lst: there should be no merge instructions before constructing the CFG"
        | If (_, _, instrs1, instrs2) ->
          let roots1, indices'', entries' = helper instrs1 [] indices' entries in
          let roots2, indices''', entries'' = helper instrs2 [] indices'' entries' in
          let roots' = (add_parent roots idx) :: roots1 @ roots2 in
          let entries''' = Instr.Label.Map.add_exn entries'' ~key:instr.label ~data:idx in
          helper rest roots' indices''' entries'''
        | Block (_, _, instrs)
        | Loop (_, _, instrs) ->
          let roots', indices'', entries' = helper instrs [] indices' entries in
          let roots'' = (add_parent roots idx) :: roots' in
          let entries'' = Instr.Label.Map.add_exn entries' ~key:instr.label ~data:idx in
          helper rest roots'' indices'' entries''
        | _ ->
          let roots' = [add_parent roots idx] in
          helper rest roots' indices' entries
      end in
  let roots, indices, entries = helper instrs [] IntMap.empty Instr.Label.Map.empty in
  match roots with
  | [] -> failwith "empty successor tree"
  | r :: [] -> { successors = r; index_to_instr = indices; block_entries = entries; no_final_node = false };
  | _ ->
    let idx = next_idx () in
    { successors = add_parent roots idx; index_to_instr = indices; block_entries = entries; no_final_node = true }

module Test = struct
  let lab id = Instr.Label.{ section = Instr.Label.Dummy; id }
  let data id i = Instr.Data { instr = i; label = lab id; line_number = id; annotation_before = (); annotation_after = () }
  let control id i = Instr.Control { instr = i; label = lab id; line_number = id; annotation_before = (); annotation_after = () }
  let%test "lexical successor tree for a single instruction" =
    let t = make [data 1 (Instr.Const (Prim_value.of_int 0))] in
    Tree.check_equality ~actual:t.successors ~expected:(Tree.of_node 1)

  let%test "lexical successor tree of a sequence of instructions" =
    let t = make [data 1 (Instr.Const (Prim_value.of_int 0));
                  data 2 (Instr.Const (Prim_value.of_int 0));
                  data 3 (Instr.Const (Prim_value.of_int 0));
                  data 4 (Instr.Const (Prim_value.of_int 0))] in
    let expected = Tree.of_children_map 4 [(4, [3]); (3, [2]); (2, [1]); (1, [])] in
    Tree.check_equality ~actual:t.successors ~expected

  let%test "lexical successor tree with blocks" =
    let t = make [data 1 (Instr.Const (Prim_value.of_int 0));
                  control 2 (Instr.Block (None, (0, 0),
                                          [data 3 (Instr.Const (Prim_value.of_int 0));
                                           control 4 (Instr.Block (None, (0, 0),
                                                                   [control 5 (Instr.Br 0l)]))]));
                  data 6 (Instr.Const (Prim_value.of_int 0))] in
    (* 6 -> 2 -> 1
         -> 4 -> 3
         -> 5 *)
    let expected = Tree.of_children_map 6 [(2, [1]); (1, []); (3, []); (4, [3]); (5, []); (6, [5; 4; 2])] in
    Tree.check_equality ~actual:t.successors ~expected

  let%test "lexical successor tree with if" =
    let t = make [data 1 (Instr.Const (Prim_value.of_int 0));
                  control 2 (Instr.If (None, (0, 1),
                                       [data 3 (Instr.Const (Prim_value.of_int 0))],
                                       [data 4 (Instr.Const (Prim_value.of_int 0))]));
                  data 5 (Instr.Const (Prim_value.of_int 0))] in
    let expected = Tree.of_children_map 5 [(5, [4; 3; 2]); (4, []); (3, []); (2, [1]); (1, [])] in
    Tree.check_equality ~actual:t.successors ~expected


  let%test "lexical successor tree with if without instructions after branches" =
    let t = make [data 1 (Instr.Const (Prim_value.of_int 0));
                  control 2 (Instr.If (None, (0, 1),
                                       [data 3 (Instr.Const (Prim_value.of_int 0))],
                                       [data 4 (Instr.Const (Prim_value.of_int 0))]))] in
    (* What we expect is that a dummy node is added to act as the root *)
    let expected = Tree.of_children_map 5 [(5, [4; 3; 2]); (4, []); (3, []); (2, [1]); (1, [])] in
    Tree.check_equality ~actual:t.successors ~expected

  let%test "lexical successor tree with if without else branch" =
    let t = make [data 1 (Instr.Const (Prim_value.of_int 0));
                  data 2 (Instr.Const (Prim_value.of_int 0));
                  data 3 (Instr.Compare Relop.{ typ = I32; op = Eq });
                  control 4 (Instr.If (None, (0, 0),
                                       [data 5 (Instr.Const (Prim_value.of_int 0));
                                        control 6 Instr.Return],
                                       []));
                  data 7 (Instr.Const (Prim_value.of_int 0))] in
    (* 7 -> 4 -> 3 -> 2 -> 1
         -> 6 -> 5 *)
    let expected = Tree.of_children_map 7 [(7, [4; 6]); (4, [3]); (3, [2]); (2, [1]); (1, []);
                                           (6, [5]); (5, [])] in
    Tree.check_equality ~actual:t.successors ~expected

end
