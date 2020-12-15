open Core_kernel
open Helpers

module SlicePart = struct
  module T = struct
    type t =
      | Instruction of Instr.label
      | Merge of int
    [@@deriving sexp, compare, equal]
    let to_string (t : t) : string = match t with
      | Instruction l -> Printf.sprintf "instr(%d)" l
      | Merge idx -> Printf.sprintf "merge(%d)" idx
  end
  include T
  module Set = struct
    include Set.Make(T)
    let to_string (s : t) : string = String.concat ~sep:"," (List.map (to_list s) ~f:T.to_string)
  end
end

(** Performs backwards slicing on `cfg`, using the slicing criterion
   `criterion`, encoded as an instruction index. Returns the set
   of instructions that are part of the slice, as a list of instruction
    indices. *)
let slicing (cfg : Spec.t Cfg.t) (criterion : Instr.label) : SlicePart.Set.t =
  let control_dependencies = Control_deps.make cfg in
  let (_, _, data_dependencies) = Use_def.make cfg in
  let rec loop (worklist : SlicePart.Set.t) (slice : SlicePart.Set.t) : SlicePart.Set.t =
    (* Perform backward slicing as follows:
       Given an instruction as the slicing criterion (we can derive variable uses from instructions),
       perform the following fixpoint algorithm, starting with W = instr
         let instr = pop(W)
         add instr to the current slice
         for use in instr_uses(instr):
           for def in usedef(use):
             if def contains an istruction, add def.instr to W
           for _, instr' in cdeps(use.var):
             add instr to W *)
    match SlicePart.Set.choose worklist with
    | None -> (* worklist is empty *)
      slice
    | Some slicepart when SlicePart.Set.mem slice slicepart ->
      (* Already seen this slice part, no need to process it again *)
      loop (SlicePart.Set.remove worklist slicepart) slice
    | Some slicepart ->
      let uses = match slicepart with
        | Instruction instr -> Use_def.instr_use (Cfg.find_instr_exn cfg instr)
        | Merge block_idx ->
          (* to find uses of a merge block, we look at variables that are
             redefined: all such initial variables are then considered to be
             used *)
          let vars = Spec_inference.new_merge_variables cfg (Cfg.find_block_exn cfg block_idx) in
          List.map vars ~f:fst in
      let worklist' = List.fold_left uses ~init:worklist
          ~f:(fun w use ->
              let def = Use_def.UseDefChains.get data_dependencies (match slicepart with
                  | Instruction instr -> Use_def.Use.Instruction (instr, use)
                  | Merge blockidx -> Use_def.Use.Merge (blockidx, use)) in
              let data_dependencies = match def with
                | Use_def.Def.Instruction (instr', _) -> SlicePart.Set.singleton (Instruction instr')
                | Use_def.Def.Merge (blockidx, _) -> SlicePart.Set.singleton (Merge blockidx)
                | Use_def.Def.Entry _ -> SlicePart.Set.empty
                | Use_def.Def.Constant _ -> SlicePart.Set.empty in
              let preds = Control_deps.find control_dependencies use in (* the control dependencies for the current use *)
              let control_dependencies = Control_deps.Pred.Set.fold preds ~init:SlicePart.Set.empty ~f:(fun w (_, instr') ->
                  (* TODO: can't merge block also have control dependencies? Maybe not relevant, as they will have data dependencies on what they redefine *)
                  SlicePart.Set.add w (Instruction instr')) in
              SlicePart.Set.union w (SlicePart.Set.union data_dependencies control_dependencies)) in
      loop (SlicePart.Set.remove worklist' slicepart) (SlicePart.Set.add slice slicepart) in
  loop (SlicePart.Set.singleton (Instruction criterion)) SlicePart.Set.empty

let%test "simple slicing - first slicing criterion, only const" =
  Instr.reset_counter ();
  let module_ = Wasm_module.of_string "(module
  (type (;0;) (func (param i32) (result i32)))
  (func (;test;) (type 0) (param i32) (result i32)
    memory.size ;; Instr 0
    memory.size ;; Instr 1
    i32.add     ;; Instr 2
    drop        ;; Instr 3
    memory.size ;; Instr 4
    memory.size ;; Instr 5
    i32.add)    ;; Instr 6
  (table (;0;) 1 1 funcref)
  (memory (;0;) 2)
  (global (;0;) (mut i32) (i32.const 66560)))" in
  let cfg = Spec_analysis.analyze_intra1 module_ 0 in
  let actual = slicing cfg 2 in
  let expected = SlicePart.Set.of_list [Instruction 0; Instruction 1; Instruction 2] in
  SlicePart.Set.equal actual expected

let%test "simple slicing - second slicing criterion, with locals" =
  Instr.reset_counter ();
  Spec_inference.propagate_globals := false;
  Spec_inference.propagate_locals := false;
  Spec_inference.use_const := false;
  let module_ = Wasm_module.of_string "(module
  (type (;0;) (func (param i32) (result i32)))
  (func (;test;) (type 0) (param i32) (result i32)
    memory.size ;; Instr 0
    memory.size ;; Instr 1
    i32.add     ;; Instr 2
    drop        ;; Instr 3
    local.get 0 ;; Instr 4
    memory.size ;; Instr 5
    i32.add)    ;; Instr 6
  (table (;0;) 1 1 funcref)
  (memory (;0;) 2)
  (global (;0;) (mut i32) (i32.const 66560)))" in
  let cfg = Spec_analysis.analyze_intra1 module_ 0 in
  let actual = slicing cfg 6 in
  let expected = SlicePart.Set.of_list [Instruction 4; Instruction 5; Instruction 6] in
  SlicePart.Set.equal actual expected

let%test "slicing with block and br_if" =
  Instr.reset_counter ();
  let module_ = Wasm_module.of_string "(module
  (type (;0;) (func (param i32) (result i32)))
  (func (;test;) (type 0) (param i32) (result i32)
    block         ;; Instr 0
      memory.size ;; Instr 1
      br_if 0     ;; Instr 2
      memory.size ;; Instr 3
      drop        ;; Instr 4
    end
    local.get 0)   ;; Instr 5
  (table (;0;) 1 1 funcref)

  (memory (;0;) 2)
  (global (;0;) (mut i32) (i32.const 66560)))" in
  let cfg = Spec_analysis.analyze_intra1 module_ 0 in
  let actual = slicing cfg 3 in
  let expected = SlicePart.Set.of_list [Instruction 0; Instruction 1; Instruction 2; Instruction 3] in
  SlicePart.Set.equal actual expected

let%test "slicing with merge blocks" =
  Instr.reset_counter ();
  let module_ = Wasm_module.of_string "(module
  (type (;0;) (func (param i32) (result i32)))
  (func (;test;) (type 0) (param i32) (result i32)
    memory.size     ;; Instr 0
    if (result i32) ;; Instr 1
      memory.size   ;; Instr 2
    else
      memory.size   ;; Instr 3
    end
    ;; Merge block 4 here
    ;; ----
    memory.size     ;; Instr 4
    memory.size     ;; Instr 5
    i32.add         ;; Instr 6
    drop            ;; Instr 7
    ;; ---- this previous part should not be part of the slice
    memory.size     ;; Instr 8
    i32.add)        ;; Instr 9
  (table (;0;) 1 1 funcref)
  (memory (;0;) 2)
  (global (;0;) (mut i32) (i32.const 66560)))" in
  let cfg = Spec_analysis.analyze_intra1 module_ 0 in
  let actual = slicing cfg 9 in
  let expected = SlicePart.Set.of_list [Instruction 0; Instruction 1; Instruction 2; Instruction 3; Merge 4; Instruction 8; Instruction 9] in
  SlicePart.Set.equal actual expected

(** Performs backwards slicing on `cfg`, relying on `slicing` defined above.
    Returns the slice as a modified CFG *)
let slice (cfg : Spec.t Cfg.t) (criterion : Instr.label) : unit Cfg.t =
  let sliceparts = slicing cfg criterion in
  let slice_instructions = IntSet.of_list (List.filter_map (SlicePart.Set.to_list sliceparts) ~f:(function
      | Instruction i -> Some i
      | _ -> None)) in
  let slice_merge_blocks = IntSet.of_list (List.filter_map (SlicePart.Set.to_list sliceparts) ~f:(function
      | Merge block_idx -> Some block_idx
      | _ -> None)) in
  let instructions = IntMap.map (IntMap.filteri cfg.instructions ~f:(fun ~key:idx ~data:_ -> IntSet.mem slice_instructions idx)) ~f:Instr.clear_annotation in
  let (basic_blocks, edges, back_edges) =
    IntMap.fold cfg.basic_blocks ~init:(IntMap.empty, cfg.edges, cfg.back_edges) ~f:(fun ~key:block_idx ~data:block (basic_blocks, edges, back_edges) ->
        (* Remove any annotation of the block *)
        let block = Basic_block.clear_annotation block in
        (* The block could be empty after slicing and not to keep,
           EXCEPT if it is the entry block or if it is the exit block.
        NOTE: this could be refined to remove entry/exit block only if
           it has one successor (which becomes the new entry  block)
           or one predecessor (which becomes the new exit block) *)
        let keep_anyway = if block_idx = cfg.entry_block then
            true (* List.length (Cfg.successors cfg block_idx) > 1 *)
          else if block_idx = cfg.exit_block then
            true (* List.length (Cfg.predecessors cfg block_idx) > 1 *)
          else
            false in
        (* The new block, if it is kept *)
        let new_block : unit Basic_block.t option = match block.content with
          | ControlMerge ->
            (* Keep merge block if it is part of the slice *)
            if keep_anyway || IntSet.mem slice_merge_blocks block.idx then
              Some block
            else
              None
          | Control i ->
            (* Keep control block if its instruction is part of the slice *)
            if IntSet.mem slice_instructions i.label then
              Some block
            else if keep_anyway then
              (* Block needs to be kept but not its content, change it to a data block *)
              Some { block with content = Basic_block.Data [] }
            else
              None
          | Data instrs ->
            (* Only keep instructions that are part of the slice.
               All annotations are removed. *)
            let instrs = List.filter_map instrs ~f:(fun instr ->
                if IntSet.mem slice_instructions instr.label then
                  (* Instruction is part of the slice, annotation is emptied *)
                  Some {instr with annotation_before = (); annotation_after = () }
                else
                  (* INstruction is not part of the slice, drop it *)
                  None) in
            if keep_anyway || not (List.is_empty instrs) then
              (* Otherwise, keep it and clear its annotation *)
              Some { block with content = (Basic_block.Data instrs);
                                annotation_before = (); annotation_after = () }
            else
              (* If there are no such instructions, don't keep the block *)
              None
        in
        match new_block with
        | Some block ->
          (* Block is kept *)
          (IntMap.add_exn basic_blocks ~key:block.idx ~data:block,
           edges, back_edges (* Edges are kept *)
          )
        | None ->
          (* Block is not kept because it is empty *)
          (basic_blocks, (* Don't add it *)
           (* Edges for that block need to be rewritten:
              - for each incoming edge, merge it to each outgoing edge
              - in case there is a branching edge, keep the branching information
           *)
           begin
             let without_edges =
               let edges' = Cfg.Edges.remove_from edges block_idx in (* Remove all edges starting from the current block *)
               let srcs = Cfg.Edges.from back_edges block_idx in (* Find all edges that go to this node *)
               List.fold_left srcs ~init:edges' ~f:(fun edges (src, _) ->
                   (* and remove them *)
                   Cfg.Edges.remove edges src block_idx) in
             let outgoing_edges = Cfg.Edges.from edges block_idx in
             let incoming_edges = Cfg.Edges.from back_edges block_idx in
             (* Connect each incoming edge to each outgoing edge *)
             List.fold_left incoming_edges ~init:without_edges ~f:(fun edges (src, cond) ->
                 List.fold_left outgoing_edges ~init:edges ~f:(fun edges (dst, _) ->
                     if src <> dst then begin
                       Cfg.Edges.add edges src (dst, cond (* Keep the first condition as the second block can't be executed anymore *))
                     end else
                       (* No self-cycle. TODO: ensure correctness, but because the block is removed, that should not be a problem *)
                       edges))
         end,
           (* Mimic what's done for edges, this time for back edges *)
           begin
             let without_back_edges =
               let back_edges' = Cfg.Edges.remove_from back_edges block_idx in
               let dsts = Cfg.Edges.from edges block_idx in
               List.fold_left dsts ~init:back_edges' ~f:(fun back_edges (dst, _) ->
                   let after = Cfg.Edges.remove back_edges dst block_idx in
                   after
                 ) in
             let outgoing_edges = Cfg.Edges.from edges block_idx in
             let incoming_edges = Cfg.Edges.from back_edges block_idx in
             List.fold_left incoming_edges ~init:without_back_edges ~f:(fun back_edges (src, cond) ->
                 List.fold_left outgoing_edges ~init:back_edges ~f:(fun back_edges (dst, _) ->
                     if src <> dst then
                       Cfg.Edges.add back_edges dst (src, cond)
                     else
                       back_edges))
           end)) in
  (* Finally, we remove all edges going to the exit block *)
  let edges_going_to_exit = Cfg.Edges.from back_edges cfg.exit_block in (* the edges that go to the exit block *)
  let back_edges = Cfg.Edges.remove_from back_edges cfg.exit_block in
  let edges = List.fold_left edges_going_to_exit
      ~init:edges
      ~f:(fun edges (src, _) -> Cfg.Edges.remove edges src cfg.exit_block) in
  { cfg with basic_blocks; instructions; edges; back_edges }

let%test_unit "slicing with merge blocks using slice" =
  Instr.reset_counter ();
  let module_ = Wasm_module.of_string "(module
  (type (;0;) (func (param i32) (result i32)))
  (func (;test;) (type 0) (param i32) (result i32)
    memory.size     ;; Instr 0
    if (result i32) ;; Instr 1
      memory.size   ;; Instr 2
    else
      memory.size   ;; Instr 3
    end
    ;; Merge block 4 here
    ;; ----
    memory.size     ;; Instr 4
    memory.size     ;; Instr 5
    i32.add         ;; Instr 6
    drop            ;; Instr 7
    ;; ---- this previous part should not be part of the slice
    memory.size     ;; Instr 8
    i32.add)        ;; Instr 9
  (table (;0;) 1 1 funcref)
  (memory (;0;) 2)
  (global (;0;) (mut i32) (i32.const 66560)))" in
  let cfg = Spec_analysis.analyze_intra1 module_ 0 in
  let sliced_cfg = slice cfg 9 in
  let _annotated_sliced_cfg = Spec_inference.Intra.analyze module_ sliced_cfg in
  (* Nothing is really tested here, besides the fact that we don't want any exceptions to be thrown *)
  ()

(** Return the indices of each call_indirect instructions *)
let find_call_indirect_instructions (cfg : Spec.t Cfg.t) : int list =
  List.filter_map (Cfg.all_instructions cfg) ~f:(fun instr -> match instr with
      | Control {label; instr = CallIndirect _; _} -> Some label
      | _ -> None)

let%test_unit "slicing with memory" =
  Instr.reset_counter ();
  let module_ = Wasm_module.of_string "(module
  (type (;0;) (func (param i32) (result i32)))
  (func (;test;) (type 0) (param i32) (result i32)
    memory.size     ;; Instr 0
    memory.size     ;; Instr 1
    i32.store       ;; Instr 2
    memory.size     ;; Instr 3
    memory.size     ;; Instr 4
    i32.store       ;; Instr 5
  )
  (table (;0;) 1 1 funcref)
  (memory (;0;) 2)
  (global (;0;) (mut i32) (i32.const 66560)))" in
  let cfg = Spec_analysis.analyze_intra1 module_ 0 in
  let sliced_cfg = slice cfg 5 in
  let _annotated_sliced_cfg = Spec_inference.Intra.analyze module_ sliced_cfg in
  ()

let%test_unit "slicing bigger program" =
  Instr.reset_counter ();
  let module_ = Wasm_module.of_file "../../../benchmarks/polybench-clang/trmm.wat" in
  Spec_inference.propagate_globals := false;
  Spec_inference.propagate_locals := false;
  Spec_inference.use_const := false;
  let cfg = Spec_analysis.analyze_intra1 module_ 14 in
  Printf.printf "number of vars: %d\n" (Var_prop.count_vars cfg);
  List.iter (find_call_indirect_instructions cfg) ~f:(fun instr_idx ->
      (* instr_idx is the label of a call_indirect instruction, slice it *)
      Spec_inference.propagate_locals := false;
      Spec_inference.propagate_globals := false;
      Spec_inference.use_const := false;
      let sliced_cfg = slice cfg instr_idx in
      (* We should be able to re-annotate the graph *)
      (* TODO: test fails if we enable the following: *)
      Spec_inference.propagate_locals := true;
      Spec_inference.propagate_globals := true;
      Spec_inference.use_const := true;
      let _annotated_slice_cfg = Spec_inference.Intra.analyze module_ sliced_cfg in
      Printf.printf "big program vars after slicing: %d\n" (Var_prop.count_vars (Var_prop.var_prop _annotated_slice_cfg));
      ())
