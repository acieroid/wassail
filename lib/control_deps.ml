open Core_kernel
open Helpers

(** Algorithm for control dependencies, adapted from https://homepages.dcc.ufmg.br/~fernando/classes/dcc888/ementa/slides/ProgramSlicing.pdf *)
let control_dep (root : Dominance.domtree) (is_immediate_post_dom : Dominance.domtree -> Var.t -> bool) : (Var.t * Var.t) list =
  let push (tree : Dominance.domtree) (preds : Var.t list) : Var.t list = match tree with
    | Branch (_, p,_) -> p :: preds
    | Jump (_, _) -> preds in
  (* Creates the edges from a given block, where each edge links a defined variabl to a control variable it depends on *)
  let link (block : Spec_inference.state Basic_block.t) (pred : Var.t) : (Var.t * Var.t) list =
    (* TODO: extract defined in Spec_inference.defined, this is a useful construct that we may need somewhere else *)
    let defined = match block.content with
      | ControlMerge ->
        let defined_vstack = List.filter_opt (List.map2_exn
                                                block.annotation_before.vstack
                                                block.annotation_after.vstack
                                                ~f:(fun v1 v2 -> if Var.equal v1 v2 then None else Some v2)) in
        let defined_local =  List.filter_opt (List.map2_exn
                                                block.annotation_before.locals
                                                block.annotation_after.locals
                                                ~f:(fun v1 v2 -> if Var.equal v1 v2 then None else Some v2)) in
        let defined_globals = List.filter_opt (List.map2_exn
                                                 block.annotation_before.globals
                                                 block.annotation_after.globals
                                                 ~f:(fun v1 v2 -> if Var.equal v1 v2 then None else Some v2)) in
        (* TODO: memory
           let defined_mem = List.filter_opt (List.map2_exn
                                       block.annotation_before.memory
                                       block.annotation_after.memory
                                       ~f:(fun v1 v2 -> if Var.equal v1 v2 then None else Some v2)) in *)
        defined_vstack @ defined_local @ defined_globals
      | Control instr ->
        let n_top_of_stack (n : int) : Var.t list = List.take instr.annotation_after.vstack n in
        begin match instr.instr with
          | Call ((arity_out, _), _) -> n_top_of_stack arity_out
          | CallIndirect ((arity_out, _), _) -> n_top_of_stack arity_out
          | Block _ | Loop _ | If _ | Br _ | BrIf _ | BrTable _ | Return | Unreachable -> []
        end
      | Data instrs -> List.fold_left instrs ~init:[] ~f:(fun acc instr ->
          let to_add =
            let n_top_of_stack (n : int) : Var.t list = List.take instr.annotation_after.vstack n in
            match instr.instr with
          | Nop | Drop | MemoryGrow -> []
          | Select | MemorySize | Const _ | Unary _ | Binary _
          | Compare _ | Test _ | Convert _ -> n_top_of_stack 1
          | LocalGet _ | GlobalGet _ -> [] (* these actually don't define new variables *)
          | LocalSet l | LocalTee l -> [List.nth_exn instr.annotation_after.locals l]
          | GlobalSet g -> [List.nth_exn instr.annotation_after.globals g]
          | Load _ -> n_top_of_stack 1
          | Store {offset; _} ->
            let addr = List.hd_exn instr.annotation_before.vstack in
            [match Var.OffsetMap.find instr.annotation_after.memory (addr, offset) with
             | Some v -> v
             | None -> failwith (Printf.sprintf "Wrong memory annotation while looking for %s+%d in memory" (Var.to_string addr) offset)]
        in to_add @ acc) in
    List.map defined ~f:(fun d -> (d, pred)) in
  (* vchildren simply recurses down the tree *)
  let rec vchildren (blocks : Dominance.domtree list) (preds : Var.t list) : (Var.t * Var.t) list = match blocks with
    | [] -> []
    | (n :: ns) -> vnode n preds @ vchildren ns preds
  (* vnode visits a tree node *)
  and vnode (tree : Dominance.domtree) (preds : Var.t list) : (Var.t * Var.t) list =
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

(** Construct a map from predicates at the end of a block (according to `branch_condition`), to the corrsponding block index *)
let extract_preds (cfg : Spec_inference.state Cfg.t) : int Var.Map.t =
  IntMap.fold cfg.basic_blocks ~init:Var.Map.empty ~f:(fun ~key:idx ~data:block acc ->
      match Dominance.branch_condition block with
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
  let cfg = Spec_analysis.analyze_intra1 module_ 0 in
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
  let cfg = Spec_analysis.analyze_intra1 module_ 0 in
  let actual = extract_preds cfg in
  let expected = Var.Map.of_alist_exn [(Var.Const (Prim_value.of_int 1), 3)] in
  Var.Map.equal (Int.(=)) actual expected

(** Computes the control dependencies of a CFG (as a map from variables to the control variables they depend upon) *)
let cdeps (cfg : Spec_inference.state Cfg.t) : Var.Set.t Var.Map.t =
  let pdom = Dominance.cfg_post_dominator cfg in
  let preds : int Var.Map.t = extract_preds cfg in
  let deps = control_dep (Dominance.cfg_dominator cfg) (fun tree pred ->
      (* Check if tree is the post-dominator of pred: look in pdom if (the node that contains) pred is a child of tree *)
      let tree_idx : int = match tree with Branch (b, _, _) | Jump (b, _) -> b.idx in
      let children : IntSet.t = Dominance.Tree.children pdom tree_idx in
      let children_idx : int = match Var.Map.find preds pred with Some idx -> idx | None -> failwith "cdeps failed when accessing children index" in
      IntSet.mem children children_idx) in
  Var.Map.map (Var.Map.of_alist_multi deps) ~f:Var.Set.of_list

let%test "control dependencies computation" =
  let module_ = Wasm_module.of_string "(module
  (type (;0;) (func (param i32) (result i32)))
  (func (;test;) (type 0) (param i32) (result i32)
    block
      i32.const 1
      br_if 0
      i32.const 2 ;; This variable clearly depends on 1
      block
        i32.const 3 ;; This one too
        br_if 0
        i32.const 4 ;; This one depends on 3 (also on 1 transitively, but here we compute direct control dependencies)
        drop
      end
      drop
      i32.const 5 ;; This one depends on 1
      drop
    end
    i32.const 6)
  (table (;0;) 1 1 funcref)
  (memory (;0;) 2)
  (global (;0;) (mut i32) (i32.const 66560)))" in
  let cfg = Spec_analysis.analyze_intra1 module_ 0 in
  let actual = cdeps cfg in
  let var n = Var.Const (Prim_value.of_int n) in
  let vars n = Var.Set.of_list [var n] in
  let expected = Var.Map.of_alist_exn [(var 2, vars 1); (var 3, vars 1); (var 4, vars 3); (var 5, vars 1)] in
  Var.Map.equal Var.Set.equal actual expected
