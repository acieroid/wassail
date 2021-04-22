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

(** Exact algorithm for control dependencies, adapted from the description by
   Ferrante et al.  This returns control dependencies at the level of blocks: a
   mapping a -> {b} means that block with index a is control-dependent on the
   block with index b.*)
let control_deps_exact_block (cfg : Spec.t Cfg.t) : IntSet.t IntMap.t =
  let pdom = Dominance.cfg_post_dominator cfg in
  let post_dominates (x : int) (y : int) : bool = Tree.is_ancestor pdom x y in
  let traverse (x : int) (y : int) : int list =
    (* Traverse the post-dominator tree backwards until we reach y's parent.
       Return the list of nodes visited *)
    match Tree.nca pdom x y with
    | None -> Log.warn "No nca found"; []
    | Some nca -> begin match Tree.nodes_between pdom x nca with
        | Some l -> x :: l
        | None -> Log.warn "No nodes found"; []
      end in
  let edges = List.filter (Cfg.all_edges cfg) ~f:(fun (a, b) -> not (post_dominates b a)) in
  List.fold_left edges ~init:IntMap.empty
    ~f:(fun acc (a, b) ->
        List.fold_left (traverse b a) ~init:acc ~f:(fun acc dep ->
            (* Mark the node "dep" as control-dependent on a *)
            IntMap.update acc dep ~f:(function
                | None -> IntSet.singleton a
                | Some a' -> IntSet.add a' a)))

(** Returns exact control dependencies on the level of instructions: a mapping a -> {b} means that instruction a is control-dependent on instruction b *)
let control_deps_exact_instrs (cfg : Spec.t Cfg.t) : Instr.Label.Set.t Instr.Label.Map.t =
  List.fold_left (IntMap.to_alist (control_deps_exact_block cfg))
    ~init:Instr.Label.Map.empty
    ~f:(fun acc (a, bs) ->
        let a_instrs = (Basic_block.all_direct_instruction_labels
                          (Cfg.find_block_exn cfg a)) in
        let b_instrs = IntSet.fold bs
            ~init:Instr.Label.Set.empty
            ~f:(fun acc b ->
                Instr.Label.Set.union acc
                  (Basic_block.all_direct_instruction_labels
                     (Cfg.find_block_exn cfg b))) in
        Instr.Label.Set.fold a_instrs
          ~init:acc
          ~f:(fun acc a ->
              Instr.Label.Set.fold b_instrs
                ~init:acc
                ~f:(fun acc b ->
                    Instr.Label.Map.update acc a ~f:(function
                        | None -> Instr.Label.Set.singleton b
                        | Some b' -> Instr.Label.Set.add b' b))))

(** Algorithm for control dependencies, adapted from https://homepages.dcc.ufmg.br/~fernando/classes/dcc888/ementa/slides/ProgramSlicing.pdf *)
(* TODO: the results of this algorithm do not seem correct. The implementation may contain a mistake *)
let control_dep (cfg : Spec.t Cfg.t) (is_immediate_post_dom : int -> Var.t -> bool) : (Var.t * Pred.t) list =
  let tree : Tree.t = Dominance.cfg_dominator cfg in
  let push (block : Spec.t Basic_block.t) (preds : Pred.t list) : Pred.t list =
    Printf.printf "push %d %s\n" block.idx (String.concat ~sep:"," (List.map preds ~f:Pred.to_string));
    match Dominance.branch_condition cfg block with
    | Some pred -> (* It is a branch *)
      begin match block.content with
        | Control i -> (pred, i.label) :: preds
        | _ -> failwith "control_dep: pushing a non-control block predicate"
      end
    | None -> preds
  in
  (* Creates the edges from a given block, where each edge links a defined variable to a control variable it depends on *)
  let link (block : Spec.t Basic_block.t) (pred : Pred.t) : (Var.t * Pred.t) list =
    Printf.printf "link %d %s\n" block.idx (Pred.to_string pred);
    let defined = match block.content with
      | Control instr -> Spec_inference.instr_def cfg (Instr.Control instr)
      | Data instrs -> List.fold_left instrs ~init:[] ~f:(fun acc instr ->
          (Spec_inference.instr_def cfg (Instr.Data instr)) @ acc) in
    List.map defined ~f:(fun d -> (d, pred)) in
  (* vchildren simply recurses down the tree *)
  let rec vchildren (block_indices : int list) (preds : Pred.t list) : (Var.t * Pred.t) list =
    Printf.printf "Visiting childrens (%s) with preds: %s\n"
      (String.concat ~sep:"," (List.map block_indices ~f:string_of_int))
      (String.concat ~sep:"," (List.map preds ~f:Pred.to_string));
    match block_indices with
    | [] -> []
    | (n :: ns) -> vnode n preds @ vchildren ns preds
  (* vnode visits a tree node *)
  and vnode (block_idx : int) (preds : Pred.t list) : (Var.t * Pred.t) list =
    Printf.printf "visiting block %d with preds: %s\n" block_idx (String.concat ~sep:"," (List.map preds ~f:Pred.to_string));
    (* Extract the block and its children from the root of the tree *)
    let block = Cfg.find_block_exn cfg block_idx in
    let children = IntSet.to_list (Tree.children tree block_idx) in
    match preds with
    | h :: t when is_immediate_post_dom block_idx (fst h) ->
      vnode block_idx t
    | [] -> vchildren children (push block preds)
    | h :: _ ->
      link block h @ vchildren children (push block preds) in
  vnode tree.entry []

(** Construct a map from predicates at the end of a block (according to `branch_condition`), to the corresponding block index *)
let extract_preds (cfg : Spec.t Cfg.t) : int Var.Map.t =
  IntMap.fold cfg.basic_blocks ~init:Var.Map.empty ~f:(fun ~key:idx ~data:block acc ->
      match Dominance.branch_condition cfg block with
      | Some pred ->
        Var.Map.add_exn acc ~key:pred ~data:idx
      | None -> acc)

let%test "extract_preds when there is no predicate should return the empty set" =
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

let%test "extract_preds when there are predicates should return the corresponding predicates" =
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
  let deps = control_dep cfg (fun block_idx pred ->
      (* Checkch if tree is the post-dominator of pred: look in pdom if (the node that contains) pred is a child of tree *)
      let children : IntSet.t = Tree.children pdom block_idx in
      let children_idx : int = match Var.Map.find preds pred with Some idx -> idx | None -> failwith "make failed when accessing children index" in
      let res = IntSet.mem children children_idx in
      if res then Printf.printf "%d is post-dom of %s\n" block_idx (Var.to_string pred);
      res
    ) in
  Var.Map.map (Var.Map.of_alist_multi deps) ~f:Pred.Set.of_list

(** Return the control dependencies for a variable *)
let find (cdeps : Pred.Set.t Var.Map.t) (var : Var.t) : Pred.Set.t =
  match Var.Map.find cdeps var with
  | Some preds -> preds
  | None -> Pred.Set.empty

let annotate (cfg : Spec.t Cfg.t) : string =
  let deps = make cfg in
  (* the "to" of the arrow is easy: it is the instruction of which the label is in Pred.
     the "from" is more difficult: we identify variables only here... so we need to go over the CFG and see each var used by each instruction *)
  let instrs = Cfg.all_instructions_list cfg in
  String.concat ~sep:"\n"
    (List.concat_map instrs
       ~f:(fun instr ->
           let label = Instr.label instr in
           let uses = Spec_inference.instr_use cfg instr in
           List.concat_map uses ~f:(fun var ->
               List.map (Pred.Set.to_list (find deps var))
                 ~f:(fun (_var, label') ->
                     Printf.sprintf "block%d:instr%s -> block%d:instr%s [color=green]"
                       (Cfg.find_enclosing_block_exn cfg label).idx
                       (Instr.Label.to_string label)
                       (Cfg.find_enclosing_block_exn cfg label').idx
                       (Instr.Label.to_string label')))))

let annotate_exact (cfg : Spec.t Cfg.t) : string =
  let deps = control_deps_exact_block cfg in
  String.concat ~sep:"\n"
    (IntMap.fold deps
       ~init:[]
       ~f:(fun ~key:a ~data:bs acc ->
           IntSet.fold bs
             ~init:acc
             ~f:(fun acc b ->
                 (Printf.sprintf "block%d -> block%d [color=green]" a b) :: acc)))

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

let%test "exact control deps in presence of a loop with br_if should have a dependency" =
  let open Instr.Label.Test in
  let module_ = Wasm_module.of_string "(module
  (type (;0;) (func))
  (func (;test;) (type 0)
    (local i32)
    loop          ;; Instr 0
      local.get 0 ;; Instr 1
      drop        ;; Instr 2
      i32.const 0 ;; Instr 3
      local.set 0 ;; Instr 4
      i32.const 1 ;; Instr 5
      br_if 0     ;; Instr 6 -- jumps back at beginning of loop
    end)
  )" in
  let cfg = Spec_analysis.analyze_intra1 module_ 0l in
  let actual = control_deps_exact_instrs cfg in
  let expected = Instr.Label.Map.of_alist_exn [(merge 1, Instr.Label.Set.singleton (lab 6))] in
  Instr.Label.Map.equal Instr.Label.Set.equal actual expected

let%test "exact control deps should have control blocks control-dependent" =
  let open Instr.Label.Test in
  let module_ = Wasm_module.of_string "(module
  (type (;0;) (func))
  (func (;foo;) (type 0)
    block ;; Instr 0
      i32.const 0 ;; Instr 1
      if ;; Instr 2
        br 1 ;; Instr 3. This block is control-dependent on the condition
      else
        nop
      end
   end
  ))" in
  let cfg = Spec_analysis.analyze_intra1 module_ 0l in
  let actual = control_deps_exact_instrs cfg in
  let expected = Instr.Label.Map.of_alist_exn [(lab 3, Instr.Label.Set.singleton (lab 2));
                                               (lab 4, Instr.Label.Set.singleton (lab 2))] in
  Instr.Label.Map.equal Instr.Label.Set.equal actual expected
