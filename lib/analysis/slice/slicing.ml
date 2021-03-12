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

(** Performs backwards slicing on `cfg`, relying on `slicing` defined above.
    Returns the slice as a modified CFG *)
let slice_old (cfg : Spec.t Cfg.t) (criterion : Instr.Label.t) : unit Cfg.t =
  let next_label : unit -> int =
    let counter : int ref = ref 0 in
    fun () ->
      let v = !counter in
      counter := v+1;
      v
  in
  (* Phase 0: identify instructions that should be part of the slice *)
  let sliceparts = instructions_to_keep cfg criterion in
  (* Phase 1: construct a broad slice, replacing blocks that are not necessary with their stack-preserving equivalent block *)
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
            List.length (Cfg.predecessors cfg block_idx) > 1 (* was: false *) in
        (* The new block, if it is kept *)
        let new_block : unit Basic_block.t option = match block.content with
          | Control { instr = Merge; _ } ->
            (* Keep merge blocks *)
            Some (Basic_block.clear_annotation block)
          | Control i ->
            (* Keep control block if its instruction is part of the slice *)
            if Instr.Label.Set.mem sliceparts i.label then
              Some (Basic_block.clear_annotation block)
            else
              let net_effect = block_effect_on_stack_size cfg block.idx in
              if keep_anyway || net_effect <> 0 then
                (* Block needs to be kept but not its content, change it to a data block with the same effect on the stack *)
                Some (dummy_data_block net_effect next_label block)
              else
                None
          | Data instrs ->
            (* Only keep instructions that are part of the slice, making sure to preserve the stack structure.
               All annotations are removed. *)
            let (slice_instrs, to_remove_final) = List.fold_left instrs ~init:([], []) ~f:(fun (slice_instrs, to_remove) instr ->
                let instr_is_part_of_slice = Instr.Label.Set.mem sliceparts instr.label in
                if instr_is_part_of_slice then begin
                  let dummy_instrs_that_preserve_stack_shape = dummy_instrs (instrs_effect_on_stack_size to_remove) next_label in
                  (slice_instrs @ dummy_instrs_that_preserve_stack_shape @ [Instr.clear_annotation_data instr],
                   [])
                end else
                  (slice_instrs, to_remove @ [instr])) in
            let slice_instrs = slice_instrs @ dummy_instrs (instrs_effect_on_stack_size to_remove_final) next_label in
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
  (* Phase 2: remove all edges going to the exit block *)
  let edges_going_to_exit = Cfg.Edges.from back_edges cfg.exit_block in (* the edges that go to the exit block *)
  let back_edges = Cfg.Edges.remove_from back_edges cfg.exit_block in
  let edges = List.fold_left edges_going_to_exit
      ~init:edges
      ~f:(fun edges (src, _) -> Cfg.Edges.remove edges src cfg.exit_block) in
  (* Phase 3: Perform a walk on the constructed CFG to remove sequences of
     blocks that cancel each other -> not done, see other implementation below
     *)
  { cfg with basic_blocks; edges; back_edges }

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
(*   let block_is_merge_block (block_idx : int) : bool =
    (* We look up in the initial CFG, which is OK because merge blocks should not be removed nor changed.
       This assumption may be relaxed later.*)
    match Cfg.find_block cfg block_idx with
    | None -> false (* This is a new block *)
    | Some block -> begin match block.content with
      | Basic_block.Control { instr = Merge; _ } -> true
      | _ -> false
     end in *)
  let block_idx_counter : int ref = ref (fst (IntMap.max_elt_exn cfg.basic_blocks)) in
  let next_available_block_idx () : int =
    block_idx_counter := !block_idx_counter + 1;
    !block_idx_counter in
  let insert_dummy_blocks_between (cfg : unit Cfg.t) (src : int) (dst : int) (effect : int) : unit Cfg.t =
    let instrs = dummy_instrs effect next_label in
    Printf.printf "inserting dummy block (effect: %d) between %d and %d\n" effect src dst;
    let block = Basic_block.{ idx = next_available_block_idx ();
                              content = Data instrs } in
    Cfg.insert_block_between cfg src dst block in
  let find_actual_previous (removed : IntSet.t IntMap.t) (idx : int) : IntSet.t =
    (* Go throug the removed info to find the replacing previous block. This is a transitive process *)
    let rec loop (worklist : IntSet.t) (stable : IntSet.t) : IntSet.t = match IntSet.choose worklist with
      | None -> stable
      | Some idx ->
        let worklist = IntSet.remove worklist idx in
        begin match IntMap.find removed idx with
          | None -> loop worklist (IntSet.add stable idx) (* Block has not been removed *)
          | Some prev -> loop (IntSet.union worklist prev) stable (* Block has been removed *)
        end
    in
    loop (IntSet.singleton idx) IntSet.empty in
  let is_dummy_block (block_idx : int) : bool =
    (* A dummy block is not part of the original CFG, so we can simply check that using its index *)
    block_idx > (fst (IntMap.max_elt_exn cfg.basic_blocks)) in
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
  let step = ref 0 in
  let rec loop
      (worklist : (int * int * int) list) (* Worklist is: previous block, current block, effect to add *)
      (visited : IntPairSet.t)
      (cfg : unit Cfg.t)
      (edges_to_add : (int * int * int) list)
      (removed : IntSet.t IntMap.t) (* Removed blocks and the blocks that "replace" them for the edges that were starting at the removed block *)
    : (unit Cfg.t * (int * int * int) list * IntSet.t IntMap.t) =
    step := !step + 1;
    match worklist with
    | [] ->
      (* Slicing finished *)
      Printf.printf "Slicing finished\n";
      cfg, edges_to_add, removed
    | (previous_block_idx, block_idx, _) :: rest when IntPairSet.mem visited (previous_block_idx, block_idx) ->
      Printf.printf "Already visited block: %d\n" block_idx;
      (* The block has already been visited. TODO: what should we do? This is not yet clear.
         Basically this can happen in two cases:
         - if the block is part of a cycle: this falls under the second possible case, as we will visit the node only once
         - if the block has multiple predecessors.
           In a first approximation, we can assume all predecessor produce the same resulting stack,
           hence after the first adaptation of the current block, the second will require the same adaptation.
           Hence, nothing needs to be done.
           If that is not the case (TODO: confirm that it can happen or not), we need to introduce a predecessor to perform the adaptation (on one side, we have adapted as needed, on the other side we introduce an extra block between the current predecessor and the current block, that will perform the extra adaptation) 
        -> Conclusion: if the assumption holds that all nodes that have multiple predecessors only require one adaptation, we need to do nothing in this case (and we don't need to *)
      loop rest visited cfg edges_to_add removed
    | (_, block_idx, _) :: rest when IntMap.mem removed block_idx ->
      Printf.printf "block has already been removed: %d\n" block_idx;
      (* TODO: if effect to add is not 0, this can be tricky? *)
      loop rest visited cfg edges_to_add removed
    | (previous_block_idx, block_idx, effect_to_add) :: rest when is_dummy_block block_idx ->
      let cfg = if effect_to_add = block_net_effect (Cfg.find_block_exn cfg block_idx) then
          (* The block already handles the effect needed *)
          cfg
        else begin
          Printf.printf "TODO: dummy block %d does not correctly handle effect (%d)\n" block_idx effect_to_add;
          cfg
        end in
      let successors = Cfg.successors cfg block_idx in
      loop
        (rest @ List.map successors ~f:(fun idx -> (block_idx, idx, 0)))
        (IntPairSet.add visited (previous_block_idx, block_idx))
        cfg
        edges_to_add
        removed
    | (previous_block_idx, block_idx, effect_to_add) :: rest when block_is_part_of_slice block_idx ->
      Printf.printf "Block is part of slice: %d, effect to add: %d \n" block_idx effect_to_add;
      (* The block is part of the slice, only keep the relevant portions *)
      (* TODO: in a first approximation, we keep the block as is *)
      let cfg', edges_to_add' = if effect_to_add = 0 then
          (* We keep the block as is *)
          cfg, edges_to_add
        else
          let block = Cfg.find_block_exn cfg block_idx in
          (* We have to propagate the effect in the block by adding dummy instructions *)
          match block.content with
          | Control _ ->
            (* This is a control block, we can't add dummy instructions at the beginning so we have to add a dummy data block before it *)
            (* insert_dummy_blocks_between cfg (find_actual_previous removed previous_block_idx) block_idx effect_to_add *)
            Printf.printf "block %d is a control block so we need to add an edge from %d with effect %d\n" block_idx previous_block_idx effect_to_add;
            cfg, (previous_block_idx, block_idx, effect_to_add) :: edges_to_add (* TODO: effect need to be added. This can be done after the facts: we only have to track where we need to add the effects (before which block) and what effect it is *)
          | Data _ ->
            Cfg.replace_block cfg (data_block_propagate_effect_at_beginning block effect_to_add), edges_to_add in
      let successors = Cfg.successors cfg block_idx in
      loop
        (rest @ List.map successors ~f:(fun idx -> (block_idx, idx, 0)))
        (IntPairSet.add visited (previous_block_idx, block_idx))
        cfg'
        edges_to_add'
        removed
    | (previous_block_idx, block_idx, effect_to_add) :: rest when block_idx = cfg.entry_block ->
      Printf.printf "block is entry: %d\n" block_idx;
      (* The entry block is not part of the slice, replace it with a dummy block *)
      (* TODO: it could be removed if it has a single successor that can itself be an entry block (having no back edges).
         We could instead remove it directly, and add it back if the entry block of the CFG has either multiple succesors (or is it really a problem? Maybe not) or if it has back edges *)
      assert(effect_to_add = 0); (* This is an invariant of our CFG, we shouldn't have back edges to the entry *)
      let block = Cfg.find_block_exn cfg block_idx in
      let net_effect = block_net_effect block in
      let cfg' = Cfg.replace_block cfg (dummy_data_block net_effect next_label block) in
      let successors = Cfg.successors cfg block_idx in
      loop
        (rest @ List.map successors ~f:(fun idx -> (block_idx, idx, 0)))
        (IntPairSet.add visited (previous_block_idx, block_idx))
        cfg'
        edges_to_add
        removed
(*    | (previous_block_idx, block_idx, effect_to_add) :: rest when block_is_merge_block block_idx ->
      Printf.printf "block is merge: %d\n" block_idx;
      (* The block is not part of the slice but needs to be kept to preserve the structure of the CFG
         (it is a merge block, which have multiple predecessors). *)
      let cfg' =
        if effect_to_add = 0 then
          (* Simply keep CFG unmodified *)
          cfg
        else begin
          (* We need to insert a dummy blocks before the current block to account for the effect *)
          insert_dummy_blocks_between cfg (find_actual_previous removed previous_block_idx) block_idx effect_to_add
        end
      in
      let successors = Cfg.successors cfg block_idx in
      loop (List.map successors ~f:(fun idx -> (block_idx, idx, 0)) @ rest)
        (IntPairSet.add visited (previous_block_idx, block_idx))
        cfg'
        removed*)
(*    | (previous_block_idx, block_idx, effect_to_add) :: rest when block_idx = cfg.exit_block ->
      Printf.printf "block is exit: %d from %d (%s in the latest cfg)\n" block_idx previous_block_idx (IntSet.to_string (find_actual_previous removed previous_block_idx));
      (* The exit block is not part of the slice and it is not a merge block, replace it with a dummy block *)
      let block = Cfg.find_block_exn cfg block_idx in
      let net_effect = block_net_effect block in
      Printf.printf "net effect: %d + %d\n" net_effect effect_to_add;
      let cfg' = Cfg.replace_block cfg (dummy_data_block (net_effect + effect_to_add) next_label block) in
      let successors = Cfg.successors cfg block_idx in
      assert (List.length successors = 0);
      loop rest (IntPairSet.add visited (previous_block_idx, block_idx)) cfg' removed *)
    | (previous_block_idx, block_idx, effect_to_add) :: rest ->
      Printf.printf "block can be removed: %d\n" block_idx;
      (* The block is not part of the slice: we remove it *)
      let net_effect = block_net_effect (Cfg.find_block_exn cfg block_idx) in
      let effect_to_propagate = effect_to_add + net_effect in
      Printf.printf "effect to propagate: %d\n" effect_to_propagate;
      let cfg' = Cfg.remove_block_rewrite_edges cfg block_idx in
      let successors = Cfg.successors cfg block_idx in
      loop
        (rest @ (List.map successors ~f:(fun idx -> (block_idx, idx, effect_to_propagate))))
        (IntPairSet.add visited (previous_block_idx, block_idx))
        cfg'
        edges_to_add
        (IntMap.add_exn removed ~key:block_idx ~data:(IntSet.of_list (Cfg.predecessors cfg block_idx)))
  in
  let add_missing_edges (cfg : unit Cfg.t) (removed : IntSet.t IntMap.t) (to_add : (int * int * int) list) : unit Cfg.t =
    (* We insert a block between src and dst to account for effect *)
    fst (List.fold_left to_add
      ~init:(cfg, IntPairSet.empty)
      ~f:(fun (cfg, added) (src, dst, effect) ->
          let actual_src = find_actual_previous removed src in
          Printf.printf "adding dummy block between %d (actual: %s) and %d\n" src (IntSet.to_string actual_src) dst;
          IntSet.fold actual_src ~init:(cfg, added)
            ~f:(fun (cfg, added) src ->
                if IntPairSet.mem added (src, dst) then
                  (* We already added an edge, do nothing  *)
                  (cfg, added)
                else
                  (* Add it *)
                  (insert_dummy_blocks_between cfg src dst effect, IntPairSet.add added (src, dst))))) in
  let remove_annotations (cfg : Spec.t Cfg.t) : unit Cfg.t = Cfg.map_annotations cfg ~f:(fun _ -> (), ()) in
  let (cfg_sliced, edges_to_add, removed) = loop [(-1, cfg.entry_block, 0)] IntPairSet.empty (remove_annotations cfg) [] IntMap.empty in
  let cfg_with_missing_edges = add_missing_edges cfg_sliced removed edges_to_add in
  add_missing_merge_blocks cfg_with_missing_edges

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

