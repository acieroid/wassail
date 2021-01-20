open Core_kernel
open Helpers

(** A predicate is represented as the variable used in the predicate as well as the label of the control instruction using it *)
module Pred = struct
  module T = struct
    type t = Var.t * Instr.Label.t
    [@@deriving sexp, compare, equal]
    let to_string (t : t) : string =
      Printf.sprintf "%s@%s" (Var.to_string (fst t)) (Instr.Label.to_string (snd t))
  end
  include T
  module Set = struct
    include Set.Make(T)
    let to_string (t : t) : string =
      String.concat ~sep:"," (List.map ~f:T.to_string (to_list t))
  end
end

(** Algorithm for control dependencies, adapted from https://homepages.dcc.ufmg.br/~fernando/classes/dcc888/ementa/slides/ProgramSlicing.pdf *)
let control_dep (cfg : Spec.t Cfg.t) (root : Dominance.domtree) (is_immediate_post_dom : Dominance.domtree -> Var.t -> bool) : (Var.t * Pred.t) list =
  let push (tree : Dominance.domtree) (preds : Pred.t list) : Pred.t list = match tree with
    | Branch (b, p,_) -> begin match b.content with
        | Control i -> (p, i.label) :: preds
        | _ -> failwith "control_dep: pushing a non-control block predicate"
      end
    | Jump (_, _) -> preds in
  (* Creates the edges from a given block, where each edge links a defined variabl to a control variable it depends on *)
  let link (block : Spec.t Basic_block.t) (pred : Pred.t) : (Var.t * Pred.t) list =
    let defined = match block.content with
      | Control instr -> Spec_inference.instr_def cfg (Instr.Control instr)
      | Data instrs -> List.fold_left instrs ~init:[] ~f:(fun acc instr ->
          (Spec_inference.instr_def cfg (Instr.Data instr)) @ acc) in
    List.map defined ~f:(fun d -> (d, pred)) in
  (* vchildren simply recurses down the tree *)
  let rec vchildren (blocks : Dominance.domtree list) (preds : Pred.t list) : (Var.t * Pred.t) list = match blocks with
    | [] -> []
    | (n :: ns) -> vnode n preds @ vchildren ns preds
  (* vnode visits a tree node *)
  and vnode (tree : Dominance.domtree) (preds : Pred.t list) : (Var.t * Pred.t) list =
    (* Extract the block and its children from the root of the tree *)
    let (block, children) = match tree with
    | Branch (block, _, children) -> (block, children)
    | Jump (block, children) -> (block, children) in
    match preds with
    | h :: t when is_immediate_post_dom tree (fst h) ->
      vnode tree t
    | [] -> vchildren children (push tree preds)
    | h :: _ ->
      link block h @ vchildren children (push tree preds) in
  vnode root []

(** Construct a map from predicates at the end of a block (according to `branch_condition`), to the corrsponding block index *)
let extract_preds (cfg : Spec.t Cfg.t) : int Var.Map.t =
  IntMap.fold cfg.basic_blocks ~init:Var.Map.empty ~f:(fun ~key:idx ~data:block acc ->
      match Dominance.branch_condition cfg block with
      | Some pred -> Var.Map.add_exn acc ~key:pred ~data:idx
      | None -> acc)

let%test "extract_preds when there is no pred" =
  let module_ = Wasm_module.of_string "(module
  (type (;0;) (func (param i32) (result i32)))
  (func (;test;) (type 0) (param i32) (result i32)
    i32.const 256
    i32.const 512
    i32.const 0
    select)
  (table (;0;) 1 1 funcref)
  (memory (;0;) 2)
  (global (;0;) (mut i32) (i32.const 66560)))" in
  let cfg = Spec_analysis.analyze_intra1 module_ 0l in
  let preds = extract_preds cfg in
  Var.Map.is_empty preds

let%test "extract_preds when there are preds" =
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
  let actual = extract_preds cfg in
  let expected = Var.Map.of_alist_exn [(Var.Const (Prim_value.of_int 1), 3)] in
  Var.Map.equal (Int.(=)) actual expected

(** Computes the control dependencies of a CFG (as a map from variables to the control variables they depend upon) *)
let make (cfg : Spec.t Cfg.t) : Pred.Set.t Var.Map.t =
  let pdom = Dominance.cfg_post_dominator cfg in
  let preds : int Var.Map.t = extract_preds cfg in
  let deps = control_dep cfg (Dominance.cfg_dominator cfg) (fun tree pred ->
      (* Check if tree is the post-dominator of pred: look in pdom if (the node that contains) pred is a child of tree *)
      let tree_idx : int = match tree with Branch (b, _, _) | Jump (b, _) -> b.idx in
      let children : IntSet.t = Tree.children pdom tree_idx in
      let children_idx : int = match Var.Map.find preds pred with Some idx -> idx | None -> failwith "make failed when accessing children index" in
      IntSet.mem children children_idx) in
  Var.Map.map (Var.Map.of_alist_multi deps) ~f:Pred.Set.of_list

(** Return the control dependencies for a variable *)
let find (cdeps : Pred.Set.t Var.Map.t) (var : Var.t) : Pred.Set.t =
  match Var.Map.find cdeps var with
  | Some preds -> preds
  | None -> Pred.Set.empty

let%test "control dependencies computation" =
  let open Instr.Label.Test in
  let module_ = Wasm_module.of_string "(module
  (type (;0;) (func (param i32) (result i32)))
  (func (;test;) (type 0) (param i32) (result i32)
    block         ;; Instr 0
      memory.size ;; Instr 1
      br_if 0     ;; Instr 2
      memory.size ;; Instr 3. Depends on var 1
      block       ;; Instr 4
        memory.size ;; Instr 5. This one too depends on var 1
        br_if 0     ;; Instr 6
        memory.size ;; Instr 7. This one depends on var 5 (also on var 1 transitively, but here we compute direct control dependencies)
        drop        ;; Instr 8
      end
      drop          ;; Intr 9
      memory.size   ;; Instr 10. This one depends on var 1
      drop
    end
    memory.size)
  )" in
  let cfg = Spec_analysis.analyze_intra1 module_ 0l in
  let actual = Var.Map.map (make cfg) ~f:(fun p -> Var.Set.of_list (List.map (Pred.Set.to_list p) ~f:fst)) in
  let var n = Var.Var n in
  let vars n = Var.Set.of_list [var n] in
  let expected = Var.Map.of_alist_exn [(var (lab 3), vars (lab 1)); (var (lab 5), vars (lab 1)); (var (lab 7), vars (lab 5)); (var (lab 10), vars (lab 1))] in
  Var.Map.equal Var.Set.equal actual expected

let%test "control deps with br_if" =
  let open Instr.Label.Test in
  let module_ = Wasm_module.of_string "(module
  (type (;0;) (func (param i32) (result i32)))
  (func (;test;) (type 0) (param i32) (result i32)
    block         ;; Instr 0
      memory.size ;; Instr 1 -- introduces var i1
      br_if 0     ;; Instr 2
      memory.size ;; Instr 3 -- var i3 introduced here has a control dependencies on var i1 used at instr 2
      drop        ;; Instr 4
    end
    local.get 0)   ;; Instr 5
  )" in
  let cfg = Spec_analysis.analyze_intra1 module_ 0l in
  let actual = make cfg in
  let expected = Var.Map.of_alist_exn [(Var.Var (lab 3), Pred.Set.singleton (Var.Var (lab 1), lab 2))] in
  Var.Map.equal Pred.Set.equal actual expected
