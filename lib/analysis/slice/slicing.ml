open Core_kernel
open Helpers

(** Identify instructions to keep in a backwards slice on `cfg`, using the
   slicing criterion `criterion`, encoded as an instruction index. Returns the
   set of instructions that are part of the slice, as a set of instruction
   labels. *)
let instructions_to_keep (cfg : Spec.t Cfg.t) (criterion : Instr.Label.t) : Instr.Label.Set.t =
  let control_dependencies = Control_deps.make cfg in
  let (_, _, data_dependencies) = Use_def.make cfg in
  let rec loop (worklist : Instr.Label.Set.t) (slice : Instr.Label.Set.t) : Instr.Label.Set.t =
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
    match Instr.Label.Set.choose worklist with
    | None -> (* worklist is empty *)
      slice
    | Some slicepart when Instr.Label.Set.mem slice slicepart ->
      Log.debug (Printf.sprintf "Instruction %s already processed\n" (Instr.Label.to_string slicepart));
      (* Already seen this slice part, no need to process it again *)
      loop (Instr.Label.Set.remove worklist slicepart) slice
    | Some slicepart ->
      Log.debug (Printf.sprintf "Processing instruction %s\n" (Instr.Label.to_string slicepart));
      let uses =  Spec_inference.instr_use cfg (Cfg.find_instr_exn cfg slicepart) in
      let worklist' = List.fold_left uses ~init:worklist
          ~f:(fun w use ->
              Log.debug (Printf.sprintf "Use: %s\n" (Var.to_string use));
              let def = Use_def.UseDefChains.get data_dependencies (Use_def.Use.make slicepart use) in
              let data_dependencies = match def with
                | Use_def.Def.Instruction (instr', _) ->
                  Log.debug (Printf.sprintf "data dependency on: %s\n" (Instr.Label.to_string instr'));
                  Instr.Label.Set.singleton instr'
                | Use_def.Def.Entry _ -> Instr.Label.Set.empty
                | Use_def.Def.Constant _ -> Instr.Label.Set.empty in
              let preds = Control_deps.find control_dependencies use in (* the control dependencies for the current use *)
              let control_dependencies = Control_deps.Pred.Set.fold preds ~init:Instr.Label.Set.empty ~f:(fun w (_, instr') ->
                  Log.debug (Printf.sprintf "control dependency on: %s" (Instr.Label.to_string instr'));
                  Instr.Label.Set.add w instr') in
              Instr.Label.Set.union w (Instr.Label.Set.union data_dependencies control_dependencies)) in
      loop (Instr.Label.Set.remove worklist' slicepart) (Instr.Label.Set.add slice slicepart) in
  loop (Instr.Label.Set.singleton criterion) Instr.Label.Set.empty

(** Return the net effect of a block on the stack size: positive if the block adds values on the stack, negative if it removes them *)
let block_effect_on_stack_size (cfg : Spec.t Cfg.t) (block_idx : int) : int =
  (List.length ((Cfg.state_after_block cfg block_idx).vstack)) - (List.length ((Cfg.state_before_block cfg block_idx).vstack))

(** Like block_effect_on_stack_size, but for lists of instructions *)
let instrs_effect_on_stack_size (instrs : (Instr.data, Spec.t) Instr.labelled list) : int =
  if List.is_empty instrs then
    0
  else
    let first = List.hd_exn instrs in
    let last = List.last_exn instrs in
    (List.length last.annotation_after.vstack) - (List.length first.annotation_before.vstack)


(** Construct a dummy list of instruction that has the given net effect on the stack size *)
let dummy_instrs (net_effect : int) : (Instr.data, unit) Instr.labelled list =
  let dummy_label (i : int) : Instr.Label.t = { section = Instr.Label.Function (-1l); id = i } in
  if net_effect = 0 then []
  else if net_effect < 0 then List.init (- net_effect) ~f:(fun i -> { Instr.instr = Instr.Drop; label = dummy_label i; annotation_before = (); annotation_after = (); })
  else List.init net_effect ~f:(fun i -> { Instr.instr = Instr.Const (Prim_value.I32 0l); label = dummy_label i; annotation_before = (); annotation_after = () })

(** Construct a dummy block that has the given net effect on the stack
   size. Uses the given block for every field that is not the list of
   instructions, in order to construct the new block. This enables keeping the
   same index. *)
let dummy_data_block (net_effect : int) (block : 'a Basic_block.t) : unit Basic_block.t =
  let instrs = dummy_instrs net_effect in
  Basic_block.clear_annotation { block with content = Basic_block.Data instrs }

(** Performs backwards slicing on `cfg`, relying on `slicing` defined above.
    Returns the slice as a modified CFG *)
let slice (cfg : Spec.t Cfg.t) (criterion : Instr.Label.t) : unit Cfg.t =
  let sliceparts = instructions_to_keep cfg criterion in
  let instructions = Instr.Label.Map.map (Instr.Label.Map.filteri cfg.instructions ~f:(fun ~key:idx ~data:_ -> Instr.Label.Set.mem sliceparts idx)) ~f:Instr.clear_annotation in
  let (basic_blocks, edges, back_edges) =
    IntMap.fold cfg.basic_blocks ~init:(IntMap.empty, cfg.edges, cfg.back_edges) ~f:(fun ~key:block_idx ~data:block (basic_blocks, edges, back_edges) ->
        (* Remove any annotation of the block *)
        (* let block = Basic_block.clear_annotation block in *)
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
          | Control i ->
            (* Keep control block if its instruction is part of the slice *)
            if Instr.Label.Set.mem sliceparts i.label then
              Some (Basic_block.clear_annotation block)
            else
              let net_effect = block_effect_on_stack_size cfg block.idx in
              if keep_anyway || net_effect <> 0 then
                (* Block needs to be kept but not its content, change it to a data block with the same effect on the stack *)
                Some (dummy_data_block net_effect block)
              else
                None
          | Data instrs ->
            (* Only keep instructions that are part of the slice, making sure to preserve the stack structure.
               All annotations are removed. *)
            let (slice_instrs, to_remove_final) = List.fold_left instrs ~init:([], []) ~f:(fun (slice_instrs, to_remove) instr ->
                let instr_is_part_of_slice = Instr.Label.Set.mem sliceparts instr.label in
                Log.debug (Printf.sprintf "Instr %s is part of the slice: %b\n" (Instr.Label.to_string instr.label) instr_is_part_of_slice);
                if instr_is_part_of_slice then begin
                  Log.debug (Printf.sprintf "effect on stack size: %d\n" (instrs_effect_on_stack_size to_remove));
                  let dummy_instrs_that_preserve_stack_shape = dummy_instrs (instrs_effect_on_stack_size to_remove) in
                  (slice_instrs @ dummy_instrs_that_preserve_stack_shape @ [Instr.clear_annotation_data instr],
                   [])
                end else
                  (slice_instrs, to_remove @ [instr])) in
            let slice_instrs = slice_instrs @ dummy_instrs (instrs_effect_on_stack_size to_remove_final) in
            if keep_anyway || not (List.is_empty slice_instrs) then
              (* Otherwise, keep it *)
              Some { block with content = Basic_block.Data slice_instrs; }
            else
              (* If there are no instructions to keep, don't keep the block *)
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

(** Return the indices of each call_indirect instructions *)
let find_call_indirect_instructions (cfg : Spec.t Cfg.t) : Instr.Label.t list =
  List.filter_map (Cfg.all_instructions cfg) ~f:(fun instr -> match instr with
      | Control {label; instr = CallIndirect _; _} -> Some label
      | _ -> None)

module Test = struct
  open Instr.Label.Test
  let build_cfg (program : string) : Wasm_module.t * Spec.t Cfg.t =
    let module_ = Wasm_module.of_string program in
    let cfg = Spec_analysis.analyze_intra1 module_ 0l in
    (module_, cfg)

  let%test "simple slicing - first slicing criterion, only const" =
    let _, cfg = build_cfg "(module
  (type (;0;) (func (param i32) (result i32)))
  (func (;test;) (type 0) (param i32) (result i32)
    memory.size ;; Instr 0
    memory.size ;; Instr 1
    i32.add     ;; Instr 2 -- slicing criterion
    drop        ;; Instr 3
    memory.size ;; Instr 4
    memory.size ;; Instr 5
    i32.add)    ;; Instr 6
  )" in
    let actual = instructions_to_keep cfg (lab 2) in
    let expected = Instr.Label.Set.of_list [lab 0; lab 1; lab 2] in
    Instr.Label.Set.check_equality ~actual:actual ~expected:expected

  let%test "simple slicing - second slicing criterion, with locals" =
    Spec_inference.propagate_globals := false;
    Spec_inference.propagate_locals := false;
    Spec_inference.use_const := false;
    let _, cfg = build_cfg "(module
  (type (;0;) (func (param i32) (result i32)))
  (func (;test;) (type 0) (param i32) (result i32)
    memory.size ;; Instr 0
    memory.size ;; Instr 1
    i32.add     ;; Instr 2
    drop        ;; Instr 3
    local.get 0 ;; Instr 4
    memory.size ;; Instr 5
    i32.add)    ;; Instr 6 -- slicing criterion
  )" in
    let actual = instructions_to_keep cfg (lab 6) in
    let expected = Instr.Label.Set.of_list [lab 4; lab 5; lab 6] in
    Instr.Label.Set.check_equality ~actual:actual ~expected:expected

  let%test "slicing with block and br_if" =
    let _, cfg = build_cfg "(module
  (type (;0;) (func (param i32) (result i32)))
  (func (;test;) (type 0) (param i32) (result i32)
    block         ;; Instr 0
      memory.size ;; Instr 1
      br_if 0     ;; Instr 2
      memory.size ;; Instr 3 -- slicing criterion
      drop        ;; Instr 4
    end
    local.get 0)  ;; Instr 5
  )" in
    let actual = instructions_to_keep cfg (lab 3) in
    (* TODO: is it {0,1,2,3} or {3}? {3} seems correct, but this test used to expect {0,1,2,3} *)
    let expected = Instr.Label.Set.of_list [lab 3] in
    Instr.Label.Set.check_equality ~actual:actual ~expected:expected

  let%test "slicing with block and br_if -- second slicing criterion" =
    let _, cfg = build_cfg "(module
  (type (;0;) (func (param i32) (result i32)))
  (func (;test;) (type 0) (param i32) (result i32)
    block         ;; Instr 0
      memory.size ;; Instr 1
      br_if 0     ;; Instr 2 -- has a data dep on 1
      memory.size ;; Instr 3 -- has a control dep on 2
      drop        ;; Instr 4 -- slicing criterion, has a data dep on instr 3
    end
    local.get 0)  ;; Instr 5
  )" in
    let actual = instructions_to_keep cfg (lab 4) in
    let expected = Instr.Label.Set.of_list [lab 1; lab 2; lab 3; lab 4] in
    Instr.Label.Set.check_equality ~actual:actual ~expected:expected

  let%test "slicing with merge blocks" =
    let _, cfg = build_cfg "(module
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
    i32.add)        ;; Instr 9 -- slicing criterion
  )" in
    let actual = instructions_to_keep cfg (lab 9) in
    let expected = Instr.Label.Set.of_list [lab 0; lab 1; lab 2; lab 3; merge 4; lab 8; lab 9] in
    Instr.Label.Set.check_equality ~actual:actual ~expected:expected

  let%test_unit "slicing with merge blocks using slice" =
    let module_, cfg = build_cfg "(module
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
    let sliced_cfg = slice cfg (lab 9) in
    let _annotated_sliced_cfg = Spec_inference.Intra.analyze module_ sliced_cfg in
    (* Nothing is really tested here, besides the fact that we don't want any exceptions to be thrown *)
    ()

   let%test_unit "slicing with a block containing a single drop" =
     let module_, cfg = build_cfg "(module
   (type (;0;) (func (param i32) (result i32)))
   (func (;test;) (type 0) (param i32) (result i32)
    block           ;; Instr 0
      local.get 0   ;; Instr 1 [i0]
      local.get 0   ;; Instr 2 [i1, i0]
      if            ;; Instr 3 [i0]
        drop        ;; Instr 4 []
        i32.const 0 ;; Instr 5 [i4]
      else
        nop         ;; Instr 6 [i0]
      end
                    ;; [i0] and [i4] merged into [m1]
      i32.const 32  ;; Instr 7 ;; [i6, m1]
      i32.add       ;; Instr 8 ;; [i7]
    end)
   )" in
     let sliced_cfg = slice cfg (lab 8) in
     let _annotated_sliced_cfg = Spec_inference.Intra.analyze module_ sliced_cfg in
     ()

   let%test_unit "slicing with a block containing a single drop - variant" =
     let module_, cfg = build_cfg "(module
   (type (;0;) (func (param i32) (result i32)))
   (func (;test;) (type 0) (param i32) (result i32)
    block           ;; Instr 0
      local.get 0   ;; Instr 1
      local.get 0   ;; Instr 2
      if            ;; Instr 3
        drop        ;; Instr 4
        i32.const 0 ;; Instr 5
      else
        i32.const 1 ;; Instr 6
        drop        ;; Instr 7
      end
      i32.const 32  ;; Instr 8
      i32.add       ;; Instr 9
    end)
   )" in
     let sliced_cfg = slice cfg (lab 9) in
     let _annotated_sliced_cfg = Spec_inference.Intra.analyze module_ sliced_cfg in
     ()

   let%test_unit "slicing with memory" =
     let module_, cfg = build_cfg "(module
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
     let sliced_cfg = slice cfg (lab 5) in
     let _annotated_sliced_cfg = Spec_inference.Intra.analyze module_ sliced_cfg in
     ()

   let%test_unit "slicing function 14 of trmm" =
     let module_ = Wasm_module.of_file "../../../benchmarks/polybench-clang/trmm.wat" in
     Spec_inference.propagate_globals := false;
     Spec_inference.propagate_locals := false;
     Spec_inference.use_const := false;
     let cfg = Spec_analysis.analyze_intra1 module_ 14l in
     Printf.printf "--------------\n";
     Printf.printf "DOT:\n%s\n" (Cfg.to_dot cfg ~annot_str:Spec.to_dot_string);
     Printf.printf "number of vars of trmm(14): %d\n" (Var_prop.count_vars cfg);
     List.iter (find_call_indirect_instructions cfg) ~f:(fun instr_idx ->
        (* instr_idx is the label of a call_indirect instruction, slice it *)
        Spec_inference.propagate_locals := false;
        Spec_inference.propagate_globals := false;
        Spec_inference.use_const := false;
        let sliced_cfg = slice cfg instr_idx in
        Printf.printf "-------------------\n";
        Printf.printf "DOT after slicing:\n%s\n" (Cfg.to_dot sliced_cfg);
        (* We should be able to re-annotate the graph *)
        Spec_inference.propagate_locals := true;
        Spec_inference.propagate_globals := true;
        Spec_inference.use_const := true;
        let _annotated_slice_cfg = Spec_inference.Intra.analyze module_ sliced_cfg in
        Printf.printf "trmm(14) vars after slicing: %d\n" (Var_prop.count_vars (Var_prop.var_prop _annotated_slice_cfg));
        ())

   let%test_unit "slicing function 22 of trmm" =
     let module_ = Wasm_module.of_file "../../../benchmarks/polybench-clang/trmm.wat" in
     Spec_inference.propagate_globals := false;
     Spec_inference.propagate_locals := false;
     Spec_inference.use_const := false;
     let cfg = Spec_analysis.analyze_intra1 module_ 22l in
     Printf.printf "number of vars of trmm(22): %d\n" (Var_prop.count_vars cfg);
     List.iter (find_call_indirect_instructions cfg) ~f:(fun instr_idx ->
        (* instr_idx is the label of a call_indirect instruction, slice it *)
        Spec_inference.propagate_locals := false;
        Spec_inference.propagate_globals := false;
        Spec_inference.use_const := false;
        let sliced_cfg = slice cfg instr_idx in
        (* We should be able to re-annotate the graph *)
          Spec_inference.propagate_locals := true;
        Spec_inference.propagate_globals := true;
        Spec_inference.use_const := true;
        let _annotated_slice_cfg = Spec_inference.Intra.analyze module_ sliced_cfg in
        Printf.printf "trmm(22) vars after slicing: %d\n" (Var_prop.count_vars (Var_prop.var_prop _annotated_slice_cfg));
        ())

end

