open Core_kernel
open Helpers

(** Identify instructions to keep in a backwards slice on `cfg`, using the
   slicing criterion `criterion`, encoded as an instruction index. Returns the
   set of instructions that are part of the slice, as a set of instruction
   labels. *)
let instructions_to_keep (cfg : Spec.t Cfg.t) (criterion : Instr.Label.t) : Instr.Label.Set.t =
  let control_dependencies = Control_deps.make cfg in
  let (_, _, data_dependencies) = Use_def.make cfg in
  let cfg_instructions = Cfg.all_instructions cfg in
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
      (* Already seen this slice part, no need to process it again *)
      loop (Instr.Label.Set.remove worklist slicepart) slice
    | Some slicepart ->
      let uses =  Spec_inference.instr_use cfg (Cfg.find_instr_exn cfg_instructions slicepart) in
      let worklist' = List.fold_left uses ~init:worklist
          ~f:(fun w use ->
              let def = Use_def.UseDefChains.get data_dependencies (Use_def.Use.make slicepart use) in
              let data_dependencies = match def with
                | Use_def.Def.Instruction (instr', _) ->
                  Instr.Label.Set.singleton instr'
                | Use_def.Def.Entry _ -> Instr.Label.Set.empty
                | Use_def.Def.Constant _ -> Instr.Label.Set.empty in
              let preds = Control_deps.find control_dependencies use in (* the control dependencies for the current use *)
              let control_dependencies = Control_deps.Pred.Set.fold preds ~init:Instr.Label.Set.empty ~f:(fun w (_, instr') ->
                  Instr.Label.Set.add w instr') in
              Instr.Label.Set.union w (Instr.Label.Set.union data_dependencies control_dependencies)) in
      loop (Instr.Label.Set.remove worklist' slicepart) (Instr.Label.Set.add slice slicepart) in
  loop (Instr.Label.Set.singleton criterion) Instr.Label.Set.empty

(** Return the net effect of a block on the stack size: positive if the block adds values on the stack, negative if it removes them *)
let block_effect_on_stack_size (cfg : Spec.t Cfg.t) (block_idx : int) : int =
  (List.length ((Cfg.state_after_block cfg block_idx).vstack)) - (List.length ((Cfg.state_before_block cfg block_idx).vstack))

(** Like block_effect_on_stack_size, but for lists of instructions *)
let instrs_effect_on_stack_size (instrs : (Instr.data, Spec.t) Instr.labelled list) : int = match instrs with
  | [] -> 0
  | first :: _ ->
    let last = List.last_exn instrs in
    (List.length last.annotation_after.vstack) - (List.length first.annotation_before.vstack)

(** Construct a dummy list of instruction that has the given net effect on the stack size *)
let dummy_instrs (net_effect : int) (next_label : unit -> int) : (Instr.data, unit) Instr.labelled list =
  let dummy_label () : Instr.Label.t = { section = Instr.Label.Dummy; id = next_label () } in
  if net_effect = 0 then []
  else if net_effect < 0 then List.init (- net_effect) ~f:(fun _ -> { Instr.instr = Instr.Drop; label = dummy_label (); annotation_before = (); annotation_after = (); })
  else List.init net_effect ~f:(fun _ -> { Instr.instr = Instr.Const (Prim_value.I32 0l); label = dummy_label (); annotation_before = (); annotation_after = () })

(** Construct a dummy block that has the given net effect on the stack
   size. Uses the given block for every field that is not the list of
   instructions, in order to construct the new block. This enables keeping the
   same index. *)
let dummy_data_block (net_effect : int) (next_label : unit -> int) (block : 'a Basic_block.t) : unit Basic_block.t =
  let instrs = dummy_instrs net_effect next_label in
  Basic_block.clear_annotation { block with content = Basic_block.Data instrs }

let block_net_effect (block : 'a Basic_block.t) : int =
  match block.content with
  | Control c ->
    (* TODO: check that return is correctly handled. It has a net effect of 0 because it does not change the stack, but in theory there might be multiple returns leading to the final block, all with a different stack *)
    Instr.net_effect_control c
  | Data instrs -> List.fold_left instrs ~init:0 ~f:(fun acc instr ->
      acc + (Instr.net_effect_data instr))

let slice (cfg : Spec.t Cfg.t) (criterion : Instr.Label.t) : unit Cfg.t =
  let next_label : unit -> int =
    let counter : int ref = ref 0 in
    fun () ->
      let v = !counter in
      counter := v+1;
      v
  in
  let instructions_in_slice : Instr.Label.Set.t = instructions_to_keep cfg criterion in
  let blocks_in_slice: IntSet.t = IntSet.filter (IntSet.of_list (IntMap.keys cfg.basic_blocks)) ~f:(fun block_idx ->
      not (Instr.Label.Set.is_empty
             (Instr.Label.Set.inter
                instructions_in_slice
                (Basic_block.all_direct_instruction_labels (Cfg.find_block_exn cfg block_idx))))) in
  let data_block_propagate_effect_at_beginning (block : unit Basic_block.t) (effect : int) : unit Basic_block.t =
    let instrs = dummy_instrs effect next_label in
    match block.content with
    | Data instrs' -> { block with content = Data (instrs @ instrs') }
    | _ -> failwith "Unexpected: not a data block" in
  let block_is_part_of_slice (block_idx : int) : bool =
    IntSet.mem blocks_in_slice block_idx ||
    (* Treat the exit block as part of the slice *)
    block_idx = cfg.exit_block in
  let block_idx_counter : int ref = ref (fst (IntMap.max_elt_exn cfg.basic_blocks)) in
  let next_available_block_idx () : int =
    block_idx_counter := !block_idx_counter + 1;
    !block_idx_counter in
  let insert_dummy_blocks_between (cfg : unit Cfg.t) (src : int) (dst : int) (effect : int) : unit Cfg.t =
    let instrs = dummy_instrs effect next_label in
    let block = Basic_block.{ idx = next_available_block_idx ();
                              content = Data instrs } in
    Cfg.insert_block_between cfg src dst block in
  (* Add merge block before each block that has multiple predecessor and is itself not a merge block *)
  let add_missing_merge_blocks (cfg : unit Cfg.t) : unit Cfg.t =
    IntMap.fold cfg.basic_blocks
      ~init:cfg
      ~f:(fun ~key:_ ~data:block cfg ->
          if Basic_block.is_merge block then
            (* Keep it *)
            cfg
          else
            let preds = Cfg.predecessors cfg block.idx in
            if List.length preds <= 1 then
              (* At most one predecessor, we can keep the block as is *)
              cfg
            else
              (* More than one predecessor, we need to insert a merge block *)
              let merge_block = Basic_block.{ idx = next_available_block_idx ();
                                                    content = Control { instr = Merge;
                                                                        label = { section = MergeInFunction cfg.idx; id = next_label () };
                                                                        annotation_before = ();
                                                                        annotation_after = (); } } in
              List.fold_left preds
                ~init:cfg
                ~f:(fun cfg pred ->
                    Cfg.insert_block_between cfg pred block.idx merge_block)) in
  let rec slicing_loop
      (worklist : int list) (* list of blocks *)
      (visited : IntSet.t)
      (cfg : unit Cfg.t)
      (removed : IntSet.t IntMap.t) (* Removed blocks and the blocks that "replace" them for the edges that were starting at the removed block *)
    : (unit Cfg.t * IntSet.t IntMap.t) =
    match worklist with
    | [] ->
      (* Slicing finished *)
      cfg, removed
    | block_idx :: rest when IntSet.mem visited block_idx ->
      slicing_loop rest visited cfg removed
    | block_idx :: rest when block_is_part_of_slice block_idx ->
      (* The block is part of the slice, only keep the relevant portions *)
      (* TODO: in a first approximation, we keep the block as is *)
      let successors = Cfg.successors cfg block_idx in
      slicing_loop
        (rest @ successors)
        (IntSet.add visited block_idx)
        cfg
        removed
    | block_idx :: rest when block_idx = cfg.entry_block ->
      (* The entry block is not part of the slice, replace it with a dummy block *)
      (* TODO: it could be removed if it has a single successor that can itself be an entry block (having no back edges).
         We could instead remove it directly, and add it back if the entry block of the CFG has either multiple succesors (or is it really a problem? Maybe not) or if it has back edges *)
      let block = Cfg.find_block_exn cfg block_idx in
      let cfg' = Cfg.replace_block cfg (dummy_data_block 0 next_label block) in
      let successors = Cfg.successors cfg block_idx in
      slicing_loop
        (rest @ successors)
        (IntSet.add visited block_idx)
        cfg'
        removed
    | block_idx :: rest ->
      (* The block is not part of the slice: we remove it *)
      let cfg' = Cfg.remove_block_rewrite_edges cfg block_idx in
      let successors = Cfg.successors cfg block_idx in
      slicing_loop
        (rest @ successors)
        (IntSet.add visited block_idx)
        cfg'
        (IntMap.add_exn removed ~key:block_idx ~data:(IntSet.of_list (Cfg.predecessors cfg block_idx)))
  in
  let adapt_blocks_for_effect (init_cfg : Spec.t Cfg.t) (sliced_cfg : unit Cfg.t) : unit Cfg.t =
    let stack_size_before (block_idx : int) : int =
      let pre = Cfg.state_before_block init_cfg block_idx in
      List.length pre.vstack in
    let stack_size_after (block_idx : int) : int =
      let post = Cfg.state_after_block init_cfg block_idx in
      List.length post.vstack in
    let successors (block_idx : int) : (int * int) list =
      List.map (Cfg.successors sliced_cfg block_idx) ~f:(fun next -> (block_idx, next)) in
    let rec loop (worklist : (int * int) list) (visited : IntPairSet.t) (cfg : unit Cfg.t) : unit Cfg.t =
      (* NOTE: we recurse on sliced_cfg, as cfg may contain extra blocks for which we can't know their size before/after *)
      match worklist with
      | [] -> cfg
      | (previous_block_idx, block_idx) :: rest when IntPairSet.mem visited (previous_block_idx, block_idx) ->
        (* Edge already visited *)
        assert (stack_size_after previous_block_idx = stack_size_before block_idx);
        loop rest visited cfg
      | (previous_block_idx, block_idx) :: rest when stack_size_after previous_block_idx = stack_size_before block_idx ->
        (* Nothing needs to be done, the stack size match *)
        loop (rest @ (successors block_idx)) (IntPairSet.add visited (previous_block_idx, block_idx)) cfg
      | (previous_block_idx, block_idx) :: rest ->
        (* Needs to be adapted *)
        let block = Cfg.find_block_exn cfg block_idx in
        let effect_to_add = (stack_size_before block_idx) - (stack_size_after previous_block_idx) in
        let cfg' = match block.content with
          | Control _ ->
            (* We can't add extra instructions to a control block, we have to insert a new block *)
            insert_dummy_blocks_between cfg previous_block_idx block_idx effect_to_add
          | Data _ ->
            (* We can modify the data block *)
            Cfg.replace_block cfg (data_block_propagate_effect_at_beginning block effect_to_add) in
        loop (rest @ (successors block_idx)) (IntPairSet.add visited (previous_block_idx, block_idx)) cfg' in
    loop (successors sliced_cfg.entry_block) IntPairSet.empty sliced_cfg in
  let remove_annotations (cfg : Spec.t Cfg.t) : unit Cfg.t = Cfg.map_annotations cfg ~f:(fun _ -> (), ()) in
  let (cfg_sliced, _removed) = slicing_loop [cfg.entry_block] IntSet.empty (remove_annotations cfg) IntMap.empty in
  adapt_blocks_for_effect cfg (add_missing_merge_blocks cfg_sliced)

(** Return the indices of each call_indirect instructions *)
let find_call_indirect_instructions (cfg : Spec.t Cfg.t) : Instr.Label.t list =
  List.filter_map (Cfg.all_instructions_list cfg) ~f:(fun instr -> match instr with
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
     let vars_before_slicing = Var_prop.count_vars cfg in
     List.iter (find_call_indirect_instructions cfg) ~f:(fun instr_idx ->
         Printf.printf "INSTR: %s\n" (Instr.Label.to_string instr_idx);
        (* instr_idx is the label of a call_indirect instruction, slice it *)
        Spec_inference.propagate_locals := false;
        Spec_inference.propagate_globals := false;
        Spec_inference.use_const := false;
        let sliced_cfg = slice cfg instr_idx in
        (* We should be able to re-annotate the graph *)
        Spec_inference.propagate_locals := true;
        Spec_inference.propagate_globals := true;
        Spec_inference.use_const := true;
        let annotated_slice_cfg = Spec_inference.Intra.analyze module_ sliced_cfg in
        let vars_after_slicing = Var_prop.count_vars (Var_prop.var_prop annotated_slice_cfg) in
        assert (vars_after_slicing < vars_before_slicing);
        ())

   let%test_unit "slicing function 22 of trmm" =
     let module_ = Wasm_module.of_file "../../../benchmarks/polybench-clang/trmm.wat" in
     Spec_inference.propagate_globals := false;
     Spec_inference.propagate_locals := false;
     Spec_inference.use_const := false;
     let cfg = Spec_analysis.analyze_intra1 module_ 22l in
     let vars_before_slicing = Var_prop.count_vars cfg in
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
         let annotated_slice_cfg = Spec_inference.Intra.analyze module_ sliced_cfg in
         let vars_after_slicing = Var_prop.count_vars (Var_prop.var_prop annotated_slice_cfg) in
         assert (vars_after_slicing < vars_before_slicing);
        ())

end

