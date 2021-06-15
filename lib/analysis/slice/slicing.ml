open Core_kernel

module InSlice = struct
  module T = struct
    (** Intermediary data structure used as part of the slicing to track which
        instruction should be part of the slice, and for what reason *)
    type t = {
      label: Instr.Label.t; (** The label of the instruction that should be added to the slice *)
      reason: Var.t option; (** The corresponding var that make this instruction part of the slice, if there is one *)
    }
    [@@deriving sexp, compare, equal]

    let to_string (t : t) : string = match t.reason with
      | None -> Instr.Label.to_string t.label
      | Some var -> Printf.sprintf "%s(%s)" (Instr.Label.to_string t.label) (Var.to_string var)

    let make (label : Instr.Label.t) (var : Var.t option) (instructions : 'a Instr.t Instr.Label.Map.t) =
      Log.info (Printf.sprintf "instr %s is part of the slice due to %s\n"
        (Instr.Label.to_string label)
        (match var with
         | Some v -> Var.to_string v
         | None -> "no var"));
      { label ;
        reason = match Cfg.find_instr instructions label with
          | Some (Instr.Control { instr = Merge; _ }) -> var
          | Some _ -> None
          | None -> failwith "Did not find the instruction! Should not happen"
      }
  end
  module Set = Set.Make(T)
  include T
end

(** Identify instructions to keep in a backwards slice on `cfg`, using the
    slicing criterion `criterion`, encoded as an instruction index. Returns the
    set of instructions that are part of the slice, as a set of instruction
    labels. *)
let instructions_to_keep (cfg : Spec.t Cfg.t) (criterion : Instr.Label.t) : Instr.Label.Set.t =
  let instructions = Cfg.all_instructions cfg in
  let control_dependencies = Control_deps.control_deps_exact_instrs cfg in
  let (_, _, data_dependencies) = Use_def.make cfg in
  let mem_dependencies = Memory_deps.make cfg in
  let cfg_instructions = Cfg.all_instructions cfg in
  let rec loop (worklist : InSlice.Set.t) (slice : Instr.Label.Set.t) (visited : InSlice.Set.t) : Instr.Label.Set.t =
    (* Perform backward slicing as follows:
       Given an instruction as the slicing criterion (we can derive variable uses from instructions),
       perform the following fixpoint algorithm, starting with W = instr
         let instr = pop(W)
         add instr to the current slice
         for use in instr_uses(instr):
           for def in usedef(use):
             if def contains an istruction, add def.instr to W
           for _, instr' in cdeps(use.var):
             add instr to W
         for instr' in mem_deps(instr):
           add instr to W *)
    match InSlice.Set.choose worklist with
    | None -> (* worklist is empty *)
      slice
    | Some slicepart when InSlice.Set.mem visited slicepart ->
      (* Already seen this slice part, no need to process it again *)
      loop (InSlice.Set.remove worklist slicepart) slice visited
    | Some slicepart ->
      (* Add instr to the current slice *)
      let slice' = Instr.Label.Set.add slice slicepart.label in
      let visited' = InSlice.Set.add visited slicepart in
      let uses =
        match Cfg.find_instr cfg_instructions slicepart.label with
        | None -> failwith (Printf.sprintf "slicing: cannot find instruction with label %s among %s \n" (Instr.Label.to_string slicepart.label) (String.concat ~sep:"," (List.map (Instr.Label.Map.keys cfg_instructions) ~f:Instr.Label.to_string)))
        | Some instr -> Spec_inference.instr_use cfg ?var:slicepart.reason instr in
      (* For use in instr_uses(instr) *)
      let worklist' = List.fold_left uses ~init:worklist
          ~f:(fun w use ->
              (* Get the definition corresponding to the current use *)
              let def = Use_def.UseDefChains.get data_dependencies (Use_def.Use.make slicepart.label use) in
              (* For def in usedef(use): if def contains an instruction, add def.instr to W *)
              let data_deps : InSlice.Set.t = match def with
                | Use_def.Def.Instruction (instr', var) ->
                  InSlice.Set.singleton (InSlice.make instr' (Some var) instructions)
                | Use_def.Def.Entry _ -> InSlice.Set.empty
                | Use_def.Def.Constant _ -> InSlice.Set.empty in
              InSlice.Set.union w data_deps) in
      (* Add all control dependencies of instr to W *)
      let control_deps : InSlice.Set.t = match Instr.Label.Map.find control_dependencies slicepart.label with
        | None -> InSlice.Set.empty
        | Some deps -> InSlice.Set.of_list (List.map (Instr.Label.Set.to_list deps)
                                              ~f:(fun label -> InSlice.make label None instructions)) in
      let worklist'' = InSlice.Set.union worklist' control_deps in
      (* For instr' in mem_deps(instr): add instr to W *)
      let worklist''' = InSlice.Set.union worklist''
          (InSlice.Set.of_list
             (List.map ~f:(fun label -> InSlice.make label None instructions)
                (Instr.Label.Set.to_list (Memory_deps.deps_for mem_dependencies slicepart.label)))) in
      loop (InSlice.Set.remove worklist''' slicepart) slice' visited' in
  let agrawal (slice : Instr.Label.Set.t) : Instr.Label.Set.t =
    (* For each instruction in the slice, we add all br instructions that are control-dependent on it *)
    Instr.Label.Set.fold (Instr.Label.Set.of_list (Instr.Label.Map.keys cfg_instructions))
      ~init:slice
      ~f:(fun slice label ->
          let instr = Instr.Label.Map.find_exn cfg_instructions label in
          match instr with
          | Control { instr = Br _; _ } -> begin match Instr.Label.Map.find control_dependencies label with
              | Some instrs -> begin match Instr.Label.Set.find_map instrs
                                             ~f:(fun i -> if Instr.Label.Set.mem slice i then Some i else None) with
                  | Some _ ->
                    Log.info (Printf.sprintf "Agrawal tells us to add %s to the slice\n" (Instr.Label.to_string label));
                    Instr.Label.Set.add slice label
                  | None -> slice
                end
              | None -> slice
            end
          | _ -> slice) in
  let initial_worklist = InSlice.Set.singleton { label = criterion; reason = None } in
  let initial_slice = Instr.Label.Set.empty in
  agrawal (loop initial_worklist initial_slice InSlice.Set.empty)

(** Construct a dummy list of instruction that has the given net effect on the stack size *)
let dummy_instrs (net_effect : int) (next_label : unit -> int) : (Instr.data, unit) Instr.labelled list =
  let dummy_label () : Instr.Label.t = { section = Instr.Label.Dummy; id = next_label () } in
  if net_effect = 0 then []
  else if net_effect < 0 then List.init (- net_effect) ~f:(fun _ -> { Instr.instr = Instr.Drop; label = dummy_label (); annotation_before = (); annotation_after = (); })
  else List.init net_effect ~f:(fun _ -> { Instr.instr = Instr.Const (Prim_value.I32 0l); label = dummy_label (); annotation_before = (); annotation_after = () })

let instrs_net_effect (instrs : unit Instr.t list) : int =
  (* TODO: if we want to support return, we need a preanalysis to know its actual net effect in the current CFG *)
  List.fold_left instrs ~init:0 ~f:(fun acc instr -> acc + (Instr.net_effect instr))

let counter : int ref = ref 0
let reset_counter () : unit = counter := 0
let next_label : unit -> int =
    fun () ->
      let v = !counter in
      counter := v+1;
      v

let replace_with_equivalent_instructions (instrs : unit Instr.t list) : unit Instr.t list =
  let net_effect = instrs_net_effect instrs in
  List.map (dummy_instrs net_effect next_label) ~f:(fun i -> Instr.Data i)

let rec slice_alternative (original_instructions : unit Instr.t list) (instructions_to_keep : Instr.Label.Set.t)  : unit Instr.t list =
  let rec loop (instrs : unit Instr.t list) (to_remove_rev : unit Instr.t list) : unit Instr.t list =
    match instrs with
    | [] -> replace_with_equivalent_instructions (List.rev to_remove_rev)
    | (Control ({ instr = Block (bt, arity, body); _ } as instr)) as entire_instr :: rest ->
      let sliced_body = slice_alternative body instructions_to_keep in
      if List.is_empty sliced_body then
        (* Block body is empty, drop the block entirely *)
        loop rest (entire_instr :: to_remove_rev)
      else
        (replace_with_equivalent_instructions to_remove_rev) @ [Instr.Control { instr with instr = Block (bt, arity, sliced_body) }] @ loop rest []
    | (Control ({ instr = Loop (bt, arity, body); _ } as instr)) as entire_instr :: rest ->
      let sliced_body = slice_alternative body instructions_to_keep in
      if List.is_empty sliced_body then
        loop rest (entire_instr :: to_remove_rev)
      else
        (replace_with_equivalent_instructions to_remove_rev) @ [Instr.Control { instr with instr = Loop (bt, arity, sliced_body) }] @ loop rest []
    | (Control ({ instr = If (bt, arity, then_, else_); _ } as instr)) as entire_instr :: rest ->
      let sliced_then = slice_alternative then_ instructions_to_keep in
      let sliced_else = slice_alternative else_ instructions_to_keep in
      if List.is_empty sliced_then && List.is_empty sliced_else then
        loop rest (entire_instr :: to_remove_rev)
      else
        (replace_with_equivalent_instructions to_remove_rev) @ [Instr.Control { instr with instr = If (bt, arity, slice_alternative then_ instructions_to_keep, slice_alternative else_ instructions_to_keep) }] @ loop rest []
    | instr :: rest when Instr.Label.Set.mem instructions_to_keep (Instr.label instr) ->
    (replace_with_equivalent_instructions to_remove_rev) @ [instr] @ loop rest []
    | instr :: rest ->
      loop rest (instr :: to_remove_rev) in
  loop original_instructions []

let slice_alternative_to_funcinst (cfg : Spec.t Cfg.t) (slicing_criterion : Instr.Label.t) : Func_inst.t =
  let instructions = slice_alternative (Cfg.body (Cfg.clear_annotations cfg)) (instructions_to_keep cfg slicing_criterion) in
  { idx = cfg.idx;
    name = Some cfg.name;
    type_idx = cfg.type_idx;
    typ = (cfg.arg_types, cfg.return_types);
    code = { locals = cfg.local_types; body = instructions } }

(* 
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

let keep_entire_blocks = ref false

let slice (cfg : Spec.t Cfg.t) ?instrs:(instructions_in_slice : Instr.Label.Set.t = Instr.Label.Set.empty) (criterion : Instr.Label.t) : unit Cfg.t =
  let init_spec = Spec_inference.init_state cfg in
  let next_label : unit -> int =
    let counter : int ref = ref 0 in
    fun () ->
      let v = !counter in
      counter := v+1;
      v
  in
  let instructions_in_slice : Instr.Label.Set.t =
    if Instr.Label.Set.is_empty instructions_in_slice then begin
      Log.debug "Computing instructions to keep";
      instructions_to_keep cfg criterion
    end else
      instructions_in_slice in
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
  let rec block_is_part_of_slice (block_idx : int) : bool =
    (* The block is part of the slice if it contains an instruction that is part of the slice *)
    IntSet.mem blocks_in_slice block_idx ||
    (* Treat the exit block as part of the slice *)
    block_idx = cfg.exit_block ||
    (* The block is also part of the slice if it is a control block with a merge block as successor, which itself is part of the slice.
       TODO: this is a coarse overapproximation, it could be refined to only those blocks that can reach the slicing criterion *)
    has_multiple_successors_and_merge_successor_in_slice block_idx
  and has_multiple_successors_and_merge_successor_in_slice (block_idx : int) : bool =
    let successors = Cfg.successors cfg block_idx in
    if List.length successors <= 1 then
      false
    else
      match List.find successors ~f:(fun idx ->
          let block = Cfg.find_block_exn cfg idx in
          Basic_block.is_merge block && block_is_part_of_slice idx) with
      | Some _ ->
        true
      | _ -> false
  in
  let block_idx_counter : int ref = ref (fst (IntMap.max_elt_exn cfg.basic_blocks)) in
  let next_available_block_idx () : int =
    block_idx_counter := !block_idx_counter + 1;
    !block_idx_counter in
  let insert_dummy_blocks_between (cfg : unit Cfg.t) (src : int) (dst : int) (effect : int) : unit Cfg.t =
    let instrs = dummy_instrs effect next_label in
    let block = Basic_block.{ idx = next_available_block_idx ();
                              content = Data instrs;
                              block_kind = None;
                              label = None;
                            } in
    Cfg.insert_block_between cfg src dst block in
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
      (* TODO: in a first approximation, we keep the block as is. If we want to remove this approximation, we can remove instructions but we have to watch out for the stack size *)
      let cfg' =
        if !keep_entire_blocks then
          (* The entire block is kept, CFG remains unchanged *)
          cfg
        else
          (* Only keep the relevant instructions *)
          let block = Cfg.find_block_exn cfg block_idx in
          let block' = match block.content with
            | Data instrs ->
              (* Remove all unecessary instructions, adapting instructions that need to in order to preserve stack shape *)
              let instrs' =
                let (pre_instrs, eff) = List.fold_left instrs ~init:([], 0) ~f:(fun (acc, effect) instr ->
                  if Instr.Label.Set.mem instructions_in_slice instr.label then
                    (* Instruction is part of the slice, we need to add it, but first we adapt to account for the effect *)
                    (instr :: (dummy_instrs effect next_label @ acc), 0)
                  else
                    (* Instruction not part of the slice *)
                    let cur_effect = Instr.net_effect_data instr in
                    (acc, cur_effect + effect)) in
                List.rev (dummy_instrs eff next_label @ pre_instrs) in
              { block with content = Data instrs' }
            | _ -> block in
          Cfg.replace_block cfg block' in
      let successors = Cfg.successors cfg block_idx in
      slicing_loop
        (rest @ successors)
        (IntSet.add visited block_idx)
        cfg'
        removed
    | block_idx :: rest when block_idx = cfg.entry_block ->
      (* The entry block is not part of the slice, replace it with a dummy block with the same effect on stack size *)
      let block = Cfg.find_block_exn cfg block_idx in
      let cfg' = Cfg.replace_block cfg (dummy_data_block (block_net_effect block) next_label block) in
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
  (* Add or remove blocks to account for size differences due to removal of blocks. This assumes blocks kept their overall effect *)
  let adapt_blocks_for_effect (init_cfg : Spec.t Cfg.t) (sliced_cfg : unit Cfg.t) : unit Cfg.t =
    let stack_size_before (block_idx : int) : int =
      let pre = Cfg.state_before_block init_cfg block_idx (Spec_inference.init_state cfg) in
      List.length pre.vstack in
    let stack_size_after (block_idx : int) : int =
      let post = Cfg.state_after_block init_cfg block_idx init_spec in
      List.length post.vstack in
    let successors (block_idx : int) : (int * int) list =
      List.map (Cfg.successors sliced_cfg block_idx) ~f:(fun next -> (block_idx, next)) in
    let rec loop (worklist : (int * int) list) (visited : IntPairSet.t) (cfg : unit Cfg.t) : unit Cfg.t =
      (* NOTE: we recurse on sliced_cfg, as cfg may contain extra blocks for which we can't know their size before/after *)
      match worklist with
      | [] -> cfg
      | (previous_block_idx, block_idx) :: rest when IntPairSet.mem visited (previous_block_idx, block_idx) ->
        (* Edge already visited *)
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
            let predecessors = Cfg.predecessors sliced_cfg block_idx in
            let predecessors_stack_sizes = List.map predecessors ~f:stack_size_after in
            let predecessors_have_same_stack_size = IntSet.length (IntSet.of_list predecessors_stack_sizes) <= 1 in
            if predecessors_have_same_stack_size then
              (* We can only safely modify a data block if it has a single predecessors
                 or if all predecessors have the same stack size *)
              Cfg.replace_block cfg (data_block_propagate_effect_at_beginning block effect_to_add)
            else
              (* otherwise we need to add an extra edge *)
              insert_dummy_blocks_between cfg previous_block_idx block_idx effect_to_add
        in
        loop (rest @ (successors block_idx)) (IntPairSet.add visited (previous_block_idx, block_idx)) cfg' in
    loop (successors sliced_cfg.entry_block) IntPairSet.empty sliced_cfg in
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
                                                                        label = { section = Instr.Label.Dummy; id = next_label () };
                                                                        annotation_before = ();
                                                                        annotation_after = (); };
                                              block_kind = None;
                                              label = None;
                                            } in
              List.fold_left preds
                ~init:cfg
                ~f:(fun cfg pred ->
                    Cfg.insert_block_between cfg pred block.idx merge_block)) in
  let remove_annotations (cfg : Spec.t Cfg.t) : unit Cfg.t = Cfg.map_annotations cfg ~f:(fun _ -> (), ()) in
  Log.debug "Performing slicing loop";
  let (cfg_sliced, _removed) = slicing_loop [cfg.entry_block] IntSet.empty (remove_annotations cfg) IntMap.empty in
  Log.debug "Performing final adaptations";
  add_missing_merge_blocks (adapt_blocks_for_effect cfg cfg_sliced)
*)
  
(** Return the indices of each call_indirect instructions *)
let find_call_indirect_instructions (cfg : Spec.t Cfg.t) : Instr.Label.t list =
  List.filter_map (Cfg.all_instructions_list cfg) ~f:(fun instr -> match instr with
      | Control {label; instr = CallIndirect _; _} -> Some label
      | _ -> None)

module Test = struct
  open Instr.Label.Test
  let build_cfg ?fidx:(fidx : int32 = 0l) (program : string) : Wasm_module.t * Spec.t Cfg.t =
    let module_ = Wasm_module.of_string program in
    let cfg = Spec_analysis.analyze_intra1 module_ fidx in
    (module_, Cfg.without_empty_nodes_with_no_predecessors cfg)

  let%test "simple slicing - first slicing criterion, only const" =
    Spec_inference.propagate_globals := false;
    Spec_inference.propagate_locals := false;
    Spec_inference.use_const := false;
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
    Spec_inference.propagate_globals := false;
    Spec_inference.propagate_locals := false;
    Spec_inference.use_const := false;
    let _, cfg = build_cfg "(module
  (type (;0;) (func (param i32) (result i32)))
  (func (;test;) (type 0) (param i32) (result i32)
    block         ;; Instr 0
      memory.size ;; Instr 1 -- data dependency of instruction 2
      br_if 0     ;; Instr 2 -- control dependency of the slicing criterion
      memory.size ;; Instr 3 -- slicing criterion
      drop        ;; Instr 4
    end
    local.get 0)  ;; Instr 5
  )" in
    let actual = instructions_to_keep cfg (lab 3) in
    let expected = Instr.Label.Set.of_list [lab 1; lab 2; lab 3] in
    Instr.Label.Set.check_equality ~actual:actual ~expected:expected

  let%test "slicing with block and br_if -- second slicing criterion" =
     Spec_inference.propagate_globals := false;
     Spec_inference.propagate_locals := false;
     Spec_inference.use_const := false;
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
     Spec_inference.propagate_globals := false;
     Spec_inference.propagate_locals := false;
     Spec_inference.use_const := false;
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
     Spec_inference.propagate_globals := false;
     Spec_inference.propagate_locals := false;
     Spec_inference.use_const := false;
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
    i32.add)        ;; Instr 9
   (table (;0;) 1 1 funcref)
   (memory (;0;) 2)
   (global (;0;) (mut i32) (i32.const 66560)))" in
    let _funcinst = slice_alternative_to_funcinst cfg (lab 9) in
    (* Nothing is really tested here, besides the fact that we don't want any exceptions to be thrown *)
    ()

   let%test_unit "slicing with a block containing a single drop should produce a valid slice" =
     Spec_inference.propagate_globals := false;
     Spec_inference.propagate_locals := false;
     Spec_inference.use_const := false;
     let _, cfg = build_cfg "(module
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
     let _funcinst = slice_alternative_to_funcinst cfg (lab 8) in
     ()

   let%test_unit "slicing intra-block block containing a single drop - variant" =
     Spec_inference.propagate_globals := false;
     Spec_inference.propagate_locals := false;
     Spec_inference.use_const := false;
     let _, cfg = build_cfg "(module
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
     let _funcinst = slice_alternative_to_funcinst cfg (lab 9) in
     ()

   let%test_unit "slicing with a block containing a single drop - variant" =
     Spec_inference.propagate_globals := false;
     Spec_inference.propagate_locals := false;
     Spec_inference.use_const := false;
     let _, cfg = build_cfg "(module
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
     let _funcinst = slice_alternative_to_funcinst cfg (lab 9) in
     ()

   let check_slice original sliced fidx criterion =
     Spec_inference.propagate_globals := false;
     Spec_inference.propagate_locals := false;
     Spec_inference.use_const := false;
     let _, cfg = build_cfg ~fidx original in
     let expected = (slice_alternative_to_funcinst cfg (lab ~fidx criterion)).code.body in
     let _, expected_cfg = build_cfg ~fidx sliced in
     let actual = Cfg.body expected_cfg in
     Printf.printf "expected\n";
     List.iter expected ~f:(fun i -> Printf.printf "%s\n" (Instr.to_string i));
     Printf.printf "actual\n";
     List.iter actual ~f:(fun i -> Printf.printf "%s\n" (Instr.to_string i));
     Printf.printf "length: %d vs. %d\n" (List.length expected) (List.length actual);
     List.equal (fun x y ->
         if Instr.equal (fun () () -> true) (Instr.drop_labels x) (Instr.drop_labels y) then
           true
         else begin
           Printf.printf "instruction not equal: %s != %s\n" (Instr.to_string x) (Instr.to_string y);
           false
         end) expected actual




   let%test "slicing intra-block should only include the relevant instructions" =
     let original = "(module
   (type (;0;) (func (param i32) (result i32)))
   (func (;test;) (type 0) (param i32) (result i32)
    i32.const 0     ;; Instr 0
    i32.const 1     ;; Instr 1
    i32.add         ;; Instr 2
    drop            ;; Instr 3
    i32.const 2     ;; Instr 4
    i32.const 3     ;; Instr 5
    i32.add         ;; Instr 6
   )
   (table (;0;) 1 1 funcref)
   (memory (;0;) 2)
   (global (;0;) (mut i32) (i32.const 66560)))" in
     let sliced = "(module
   (type (;0;) (func (param i32) (result i32)))
   (func (;test;) (type 0) (param i32) (result i32)
    i32.const 0     ;; Instr 0
    i32.const 1     ;; Instr 1
    i32.add         ;; Instr 2
   )
   (table (;0;) 1 1 funcref)
   (memory (;0;) 2)
   (global (;0;) (mut i32) (i32.const 66560)))" in
     check_slice original sliced 0l 2

   let%test "slicing with memory does not fail" =
     let original =  "(module
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
     let sliced = "(module
   (type (;0;) (func (param i32) (result i32)))
   (func (;test;) (type 0) (param i32) (result i32)
    memory.size     ;; Instr 3
    memory.size     ;; Instr 4
    i32.store       ;; Instr 5
   )
   (table (;0;) 1 1 funcref)
   (memory (;0;) 2)
   (global (;0;) (mut i32) (i32.const 66560)))" in
     check_slice original sliced 0l 5

   let%test "slicing with memory contains the relevant store instruction" =
     let original = "(module
  (type (;0;) (func (param i32) (result i32)))
  (func (;test;) (type 0) (param i32) (result i32)
    memory.size     ;; Instr 0
    memory.size     ;; Instr 1
    i32.store       ;; Instr 2
    memory.size     ;; Instr 3
    i32.load)       ;; Instr 4
  )" in
     check_slice original original (* all instructions are kept *) 0l 4

   let%test "slice with merge block should not contain irrelevant instructions" =
     let original = "(module
  (type (;0;) (func (param i32 i32) (result i32)))
  (func (;test;) (type 0) (param i32 i32) (result i32)
    local.get 0 ;; Instr 0
    if ;; Instr 1
      i32.const 42 ;; Instr 2
      local.set 0 ;; Instr 3
    end
    local.get 1) ;; Instr 4
  )" in
     (* The slice should only contain instruction 4 among the original instructions.
     It can contain an empty block/if though. *)
     let sliced = "(module
  (type (;0;) (func (param i32 i32) (result i32)))
  (func (;test;) (type 0) (param i32 i32) (result i32)
    local.get 1) ;; Instr 4
  )" in
     check_slice original sliced 0l 4

   let%test "slice with merge block should not contain non-relevant instructions, variation" =
     let original = "(module
  (type (;0;) (func (param i32 i32) (result i32)))
  (func (;test;) (type 0) (param i32 i32) (result i32)
    local.get 0 ;; Instr 0
    if ;; Instr 1
      i32.const 42 ;; Instr 2
      local.set 0 ;; Instr 3
    else
      i32.const 42 ;; Instr 4
      local.set 1 ;; Instr 5
    end
    local.get 1) ;; Instr 6
  )" in
     (* The slice should not contain instructions 2 and 3 *)
     let sliced =
       "(module
  (type (;0;) (func (param i32 i32) (result i32)))
  (func (;test;) (type 0) (param i32 i32) (result i32)
    local.get 0 ;; Instr 0
    if ;; Instr 1
    else
      i32.const 42 ;; Instr 4
      local.set 1 ;; Instr 5
    end
    local.get 1) ;; Instr 6
  )" in
     check_slice original sliced 0l 6

   let%test "slice with an empty if" =
     let original = "(module
(type (;0;) (func (param i32) (result i32)))
(func (;0;) (type 0)
  local.get 0
  i32.const -1
  i32.eq
  if
  end
  local.get 0
)
)" in
     let sliced = "(module
(type (;0;) (func (param i32) (result i32)))
(func (;0;) (type 0)
  local.get 0
))" in
     check_slice original sliced 0l 4

   let%test "slice with a if that only contains br" =
     let original = "(module
(type (;0;) (func (param i32) (result i32)))
(func (;0;) (type 0)
  block
    local.get 0
    block
      if
        br 0
      else
        br 1
      end
    end
    local.get 0
  end
))" in
     (* This is not the ideal slice, but this is good enough *)
     let sliced = "(module
(type (;0;) (func (param i32) (result i32)))
(func (;0;) (type 0)
  block
    i32.const 0
    block
      drop
    end
    local.get 0
  end
)
)" in
     check_slice original sliced 0l 6


   let%test "slice on the example from Agrawal 1994 (Fig. 3) should be correct" =
     let original = "(module
  (type (;0;) (func))
  (type (;1;) (func (result i32)))
  (type (;1;) (func (param i32) (result i32)))
  (func (;eof;) (type 1) (result i32)
    i32.const 0)
  (func (;read;) (type 1) (result i32)
    i32.const 0)
  (func (;f;) (type 2) (param i32) (result i32)
    local.get 0)
  (func (;test;) (type 0)
    (local i32 i32 i32)
    ;; Local 0: sum
    ;; Local 1: positive
    ;; Local 2: x
    block ;; block 0
      loop ;; loop 0 (L3)
        call 0 ;; eof() --> Instr 2 should be part of the slice
        br_if 1 ;; goto end of block 0 if eof() --> Instr 3 should be part of the slice
        block ;; block 1
          block ;; block 2
            call 1 ;; read() --> Inst r6 should be part of the slice
            local.tee 2 ;; x = read()
            br_if 0 ;; jump to end of block 2 (L8) if x != 0 (was: x > 0) --> Instr 8 should be part of the slice
            local.get 2
            call 2 ;; f(x)
            local.get 0
            i32.add ;; sum + f(x)
            local.set 0 ;; sum = sum + f(x)
            br 1 ;; jump to end of block 1 (L13)
          end ;; end of block 2 (L8)
          block ;; block 3
            local.get 1 ;; --> Instr 16 should be part of the slice
            i32.const 1 ;; --> Instr 17 should be part of the slice
            i32.add     ;; --> Instr 18 should be part of the slice
            local.set 1 ;; positives = positives + 1 --> should be part of the slice
            local.get 2
            br_if 0 ;; jump to end of block 3 (L12) if x != 0 (was: x%2 != 0)
            local.get 2
            call 2 ;; f(x) (was: f2(x))
            local.get 0
            i32.add
            local.set 0 ;; sum = sum + f2(x) (was + f2(x))
            br 1 ;; jump to end of block 1 (L13)
          end ;; end of block 3 (L12)
          local.get 2
          call 2
          local.get 0
          i32.add
          local.set 0 ;; sum = sum + f(x) (was + f3(x))
        end ;; end of block 1 (L13)
        br 0 ;; jump to beginning of loop 0 (L3) ;; --> Instr 33 should be part of the slice (with Agrawal's algorithm, not the conventional one!)
      end ;; end of loop 0
    end ;; end of block 0 (L14)
    local.get 0
    call 2 ;; f(sum) (was: write(sum))
    drop
    local.get 1 ;; --> Instr 37 should be part of the slice
    ;; The following instruction is the slicing criterion, i.e., instruction number 38
    call 2 ;; f(positives) (was: write(positives)) --> Instr 38 should be part of the slice
    drop
    ))" in
     let sliced = "(module
  (type (;0;) (func))
  (type (;1;) (func (result i32)))
  (type (;1;) (func (param i32) (result i32)))
  (func (;eof;) (type 1) (result i32)
    i32.const 0)
  (func (;read;) (type 1) (result i32)
    i32.const 0)
  (func (;f;) (type 2) (param i32) (result i32)
    local.get 0)
  (func (;test;) (type 0)
    (local i32 i32 i32)
    block
      loop
        call 0
        br_if 1
        block
          block
            call 1
            br_if 0
            br 1
          end ;; end of block 2 (L8)
          block
            local.get 1
            i32.const 1
            i32.add
            local.set 1
          end
        end
        br 0
      end
    end
    local.get 1
    call 2
    drop
    ))" in
     check_slice original sliced 3l 38

   let%test "slice on the example from Agrawal 1994 (Fig. 5) should be correct" =
     let original = "(module
  (type (;0;) (func))
  (type (;1;) (func (result i32)))
  (type (;1;) (func (param i32) (result i32)))
  (func (;eof;) (type 1) (result i32)
    i32.const 0)
  (func (;read;) (type 1) (result i32)
    i32.const 0)
  (func (;f;) (type 2) (param i32) (result i32)
    local.get 0)
  (func (;test;) (type 0)
    (local i32 i32 i32)
    ;; Local 0: sum
    ;; Local 1: positive
    ;; Local 2: x
    block ;; block 0
      loop ;; loop 0 (L3)
        call 0 ;; eof() ;; --> Instr 2, part of the slice
        i32.const 0 ;; --> Instr 3, part of the slice
        i32.ne ;; --> Instr 4, part of the slice
        br_if 1 ;; goto end of block 0 if !eof() --> Instr 5, part of the slice
        call 1 ;; read() ;; --> Instr 6, part of the slice
        local.tee 2 ;; x = read()
        if ;; if (x != 0) (was if (x <= 0)) --> part of the slice
          local.get 2
          call 2 ;; f(x)
          local.get 0
          i32.add ;; sum + f(x)
          local.set 0 ;; sum = sum + f(x)
          br 1 ;; jump to the beggining of loop 0 (L3) ;; --> Instr 14, part of the slice (with Agrawal's additions)
        end
        local.get 1 ;; --> part of the slice
        i32.const 1 ;; --> part of the slice
        i32.add     ;; --> part of the slice
        local.set 1 ;; positives = positives + 1 --> Instr 18,  part of the slice
        local.get 2
        if ;; if (x != 0) (was: x%2 != 0)
          local.get 2
          call 2 ;; f(x) (was: f2(x))
          local.get 0
          i32.add
          local.set 0 ;; sum = sum + f2(x) (was + f2(x))
          br 1 ;; jump to beginning of loop (L3)
        end
        local.get 2
        call 2
        local.get 0
        i32.add
        local.set 0 ;; sum = sum + f(x) (was + f3(x))
        br 0 ;; jump to beginning of loop 0 (L3)
      end ;; end of loop 0
    end ;; end of block 0 (L14)
    local.get 0
    call 2 ;; f(sum) (was: write(sum))
    drop
    local.get 1 ;; --> Instr 36, part of the slice
    ;; The following instruction is the slicing criterion, i.e., instruction number 37
    call 2 ;; f(positives) (was: write(positives)) --> Instr 37, part of the slice
    drop
    ))" in
     let sliced = "(module
  (type (;0;) (func))
  (type (;1;) (func (result i32)))
  (type (;1;) (func (param i32) (result i32)))
  (func (;eof;) (type 1) (result i32)
    i32.const 0)
  (func (;read;) (type 1) (result i32)
    i32.const 0)
  (func (;f;) (type 2) (param i32) (result i32)
    local.get 0)
  (func (;test;) (type 0)
    (local i32 i32 i32)
  block
    loop
      call 0
      i32.const 0
      i32.ne
      br_if 1
      call 1
      if
        br 1
      end
      local.get 1
      i32.const 1
      i32.add
      local.set 1
    end
  end
  local.get 1
  call 2
  drop))" in
     check_slice original sliced 3l 37

   let%test_unit "slicing function 14 of trmm" =
     let module_ = Wasm_module.of_file "../../../benchmarks/polybench-clang/trmm.wat" in
     Spec_inference.propagate_globals := false;
     Spec_inference.propagate_locals := false;
     Spec_inference.use_const := false;
     let cfg = Spec_analysis.analyze_intra1 module_ 14l in
     List.iter (find_call_indirect_instructions cfg) ~f:(fun instr_idx ->
        (* instr_idx is the label of a call_indirect instruction, slice it *)
        Spec_inference.propagate_locals := false;
        Spec_inference.propagate_globals := false;
        Spec_inference.use_const := false;
        let funcinst = slice_alternative_to_funcinst cfg instr_idx in
        let module_ = Wasm_module.replace_func module_ 14l funcinst in
        (* We should be able to re-annotate the graph *)
        Spec_inference.propagate_locals := true;
        Spec_inference.propagate_globals := true;
        Spec_inference.use_const := true;
        let _new_cfg = Spec_analysis.analyze_intra1 module_ 14l in
        ())

   let%test_unit "slicing function 22 of trmm" =
     let module_ = Wasm_module.of_file "../../../benchmarks/polybench-clang/trmm.wat" in
     Spec_inference.propagate_globals := false;
     Spec_inference.propagate_locals := false;
     Spec_inference.use_const := false;
     let cfg = Spec_analysis.analyze_intra1 module_ 22l in
     List.iter (find_call_indirect_instructions cfg) ~f:(fun instr_idx ->
         (* instr_idx is the label of a call_indirect instruction, slice it *)
         Spec_inference.propagate_locals := false;
         Spec_inference.propagate_globals := false;
         Spec_inference.use_const := false;
         let funcinst = slice_alternative_to_funcinst cfg instr_idx in
         let module_ = Wasm_module.replace_func module_ 22l funcinst in
         (* We should be able to re-annotate the graph *)
         Spec_inference.propagate_locals := true;
         Spec_inference.propagate_globals := true;
         Spec_inference.use_const := true;
         let _new_cfg = Spec_analysis.analyze_intra1 module_ 22l in
         ())

   let%test "slicing of SCAM mug example (variant 1) should produce the full program as the slice" =
     let original = "(module
  (type (;0;) (func))
  (type (;1;) (func (result i32)))
  (type (;2;) (func (param i32) (result i32)))
  (func (;0;) (type 2) ;; int p(int i)
    i32.const 0)
  (func (;1;) (type 2) ;; int q(int c)
    i32.const 0)
  (func (;2;) (type 1) ;; int f()
    i32.const 0)
  (func (;3;) (type 1) ;; int g()
    i32.const 0)
  (func (;4;) (type 2) ;; int h(int i)
    i32.const 0)
  (func (;5;) (type 0) ;; int main()
    (local i32 i32 i32)
    ;; Local 0: i
    ;; Local 1: x
    ;; Local 2: c
    local.get 0
    call 0 ;; p(i)
    if ;; label = @1
      loop ;; label = @2
        local.get 2
        call 1 ;; q(c)
        if  ;; label = @3
          call 2 ;; f()
          ;; The following instruction is the slicing criterion
          local.set 1 ;; x = result of f()
          call 3 ;; g()
          local.set 2 ;; c = result of g()
        end
        local.get 0
        call 4 ;; ;; h(i)
        local.set 0 ;; i = result of h(i)
        local.get 0
        call 0 ;; p(i)
        br_if 0 (;@2;)
      end
    end)
   )" in
     check_slice original original 5l 8

   let%test "slicing of SCAM mug example (variant 2) should produce the full program as the slice" =
     let original = "(module
  (type (;0;) (func))
  (type (;1;) (func (result i32)))
  (type (;2;) (func (param i32) (result i32)))
  (func (;0;) (type 2) ;; int p(int i)
    i32.const 0)
  (func (;1;) (type 2) ;; int q(int c)
    i32.const 0)
  (func (;2;) (type 1) ;; int f()
    i32.const 0)
  (func (;3;) (type 1) ;; int g()
    i32.const 0)
  (func (;4;) (type 2) ;; int h(int i)
    i32.const 0)
  (func (;5;) (type 0) ;; int main()
    (local i32 i32 i32)
    ;; Local 0: i
    ;; Local 1: x
    ;; Local 2: c
    block
      loop
        local.get 0
        call 0 ;; p(i)
        i32.const 0
        i32.ne
        br_if 1
        local.get 2
        call 1 ;; q(c)
        if  ;; label = @3
          call 2 ;; f()
          ;; The following instruction is the slicing criterion
          local.set 1 ;; x = result of f()
          call 3 ;; g()
          local.set 2 ;; c = result of g()
        end
        local.get 0
        call 4 ;; ;; h(i)
        local.set 0 ;; i = result of h(i)
        br 0
      end
    end)
  )" in
     check_slice original original 5l 11

   let%test "slicing of Montréal boat example should produce the full program as the slice" =
     let original = "(module
  (type (;0;) (func))
  (type (;1;) (func (result i32)))
  (type (;2;) (func (param i32) (result i32)))
  (func (;0;) (type 2) ;; p(j)
    i32.const 0)
  (func (;1;) (type 2) ;; int q(k)
    i32.const 0)
  (func (;2;) (type 2) ;; int f1(k)
    i32.const 0)
  (func (;3;) (type 2) ;; int f2(k)
    i32.const 0)
  (func (;4;) (type 2) ;; int f3(k)
    i32.const 0)
  (func (;5;) (type 0) ;; void main()
    (local i32 i32 i32)
    ;; Local 0: j
    ;; Local 1: k
    local.get 0
    call 0 ;; p(j)
    if ;; label = @1
      loop ;; label = @2
        local.get 1
        call 1 ;; q(k)
        if  ;; label = @3
          local.get 1
          call 2 ;; f1(k)
          local.set 1 ;; k = result of f1(k)
        else
          local.get 1
          call 3 ;; f2(k)
          local.set 1 ;; k = result of f2(k)
          local.get 0
          call 4 ;; f3(j)
          local.set 0 ;; j = result of f3(j)
        end
        local.get 0
        call 0 ;; p(j)
        br_if 0 (;@2;)
      end
      local.get 0 ;; Slicing criterion
      drop
    end))" in
     check_slice original original 5l 19

   let word_count ="(module
  (type (;0;) (func))
  (type (;1;) (func (result i32)))
  (func (;0;) (type 1) ;; char getchar()
    i32.const 0)
  (func (;1;) (type 0) ;; void main()
    (local i32 i32 i32 i32 i32)
    ;; Local 0: c
    ;; Local 1: nl
    ;; Local 2: nw
    ;; Local 3: nc
    ;; Local 4: inword
    ;; EOF = -1
    ;; '\\n' = 10
    ;; ' ' = 32
    ;; '\\t' = 9
    call 0 ;; getchar();
    local.tee 0 ;; c = result of getchar();
    i32.const 0 ;; EOF
    i32.ne ;; c != EOF
    if ;; label = @1
      loop ;; label = @2
        local.get 3
        i32.const 1
        i32.add
        local.set 3 ;; nc = nc + 1
        local.get 0
        i32.const 10
        i32.eq ;; c = '\\n'
        if
          local.get 1
          i32.const 1
          i32.add
          local.set 1 ;; nl = nl + 1
        end
        local.get 0
        i32.const 32
        i32.eq ;; c == ' '
        ;; In the original program, the condition is c == ' ' || c == '\\n' || c = '\\t'
        if
          i32.const 0
          local.set 4 ;; inword = NO
        else
          local.get 4
          if ;; inword == NO
            i32.const 1
            local.set 4 ;; inword = YES
            local.get 2
            i32.const 1
            i32.add
            local.set 2 ;; nw = nw + 1
          end
        end
        call 0
        local.tee 0
        i32.const 0 ;; EOF
        i32.ne ;; c != EOF
        br_if 0
      end
    end
    local.get 0 ;; c
    drop
    local.get 1 ;; nl
    drop
    local.get 2 ;; nw
    drop
    local.get 3 ;; nc
    drop
    local.get 4 ;; inword
    drop))"

   let%test "slicing of word count example (slicing criterion 1) should produce the expected slice" =
     let slice = "(module
(type (;0;) (func))
(type (;1;) (func (result i32)))
(func (;0;) (type 1)
  i32.const 0
)
(func (;1;) (type 0)
(local i32 i32 i32 i32 i32)
  call 0 ;; getchar();
  local.tee 0 ;; c = getchar();
  i32.const 0 ;; EOF
  i32.ne ;; c != EOF
  if
    loop
      local.get 0 ;; c
      i32.const 32 ;; ' '
      i32.eq ;; c == ' '
      if
        i32.const 0 ;; NO
        local.set 4 ;; inword = NO
      else
        local.get 4 ;; inword
        if ;; inword == NO
          i32.const 1 ;; YES
          local.set 4 ;; inword = YES
          local.get 2 ;; nw
          i32.const 1
          i32.add
          local.set 2 ;; nw = nw + 1
        end
      end
      call 0
      local.tee 0
      i32.const 0
      i32.ne ;; c != EOF
      br_if 0
    end
  end
  local.get 2 ;; c
  drop
))" in
     check_slice word_count slice 1l 41
   let%test "slicing of word count example (slicing criterion 2) should produce the expected slice" =
     let slice = "(module
(type (;0;) (func))
(type (;1;) (func (result i32)))
(func (;0;) (type 1)
  i32.const 0
)
(func (;1;) (type 0)
(local i32 i32 i32 i32 i32)
  call 0
  i32.const 0
  i32.ne
  if
    loop
      local.get 3
      i32.const 1
      i32.add
      local.set 3  ;; nc = nc + 1
      call 0 ;; getchar()
      i32.const 0
      i32.ne ;; c != EOF
      br_if 0
    end
  end
  local.get 3
  drop
))" in
     check_slice word_count slice 1l 43

  let%test "slicing of word count example (slicing criterion 3) should produce the expected slice" =
     let slice = "(module
(type (;0;) (func))
(type (;1;) (func (result i32)))
(func (;0;) (type 1)
  i32.const 0
)
(func (;1;) (type 0)
(local i32 i32 i32 i32 i32)
  call 0
  local.tee 0
  i32.const 0
  i32.ne ;; c != EOF
  if
    loop
      local.get 0
      i32.const 10
      i32.eq ;; c = '\\n'
      if
        local.get 1
        i32.const 1
        i32.add
        local.set 1 ;; nl = nl + 1
      end
      call 0
      local.tee 0 ;; c = getchar()
      i32.const 0
      i32.ne
      br_if 0 ;; c != EOF
    end
  end
  local.get 1
  drop
))" in
     check_slice word_count slice 1l 39

    let%test "slicing of word count example (slicing criterion 4) should produce the expected slice" =
     let slice = "(module
(type (;0;) (func))
(type (;1;) (func (result i32)))
(func (;0;) (type 1)
  i32.const 0
)
(func (;1;) (type 0)
(local i32 i32 i32 i32 i32)
  call 0 ;; c = getchar()
  local.tee 0
  i32.const 0
  i32.ne ;; c != EOF
  if
    loop
      local.get 0
      i32.const 32
      i32.eq ;; c == ' '
      if
        i32.const 0
        local.set 4 ;; inword = NO
      else
        local.get 4 ;; inword == YES
        if
          i32.const 1
          local.set 4 ;; inword = NO
        end
      end
      call 0
      local.tee 0 ;; c = getchar()
      i32.const 0
      i32.ne
      br_if 0 ;; c == EOF
    end
  end
  local.get 4
  drop
))" in
     check_slice word_count slice 1l 45

    let%test "slicing of word count example (slicing criterion 5) should produce the expected slice" =
     let slice = "(module
(type (;0;) (func))
(type (;1;) (func (result i32)))
(func (;0;) (type 1)
  i32.const 0
)
(func (;1;) (type 0)
(local i32 i32 i32 i32 i32)
  call 0  ;; getchar()
  local.tee 0  ;; c = getchar();
  i32.const 0
  i32.ne ;; c != EOF
  if
    loop
      call 0 ;; getchar()
      local.tee 0 ;; c = getchar()
      i32.const 0
      i32.ne ;; c != EOF
      br_if 0
    end
  end
  local.get 0 ;; c
  drop
))" in
     check_slice word_count slice 1l 37


 end
