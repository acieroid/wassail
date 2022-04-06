open Core
open Helpers

let compute_block_arities (instrs : unit Instr.t list) : (int * int) Instr.Label.Map.t =
  let rec loop (instrs : unit Instr.t list) (acc : (int * int) Instr.Label.Map.t) =
    match instrs with
    | [] -> acc
    | (Data _) :: rest -> loop rest acc
    | (Control i) :: rest -> loop rest (match i.instr with
        | Block (_, arity, body) ->
          loop body (Instr.Label.Map.set acc ~key:i.label ~data:arity)
        | Loop (_, arity, body) ->
          loop body (Instr.Label.Map.set acc ~key:i.label ~data:arity)
        | If (_, arity, then_, else_) ->
          loop else_ (loop then_ (Instr.Label.Map.set acc ~key:i.label ~data:arity))
        | _ -> acc)
  in
  loop instrs Instr.Label.Map.empty

let compute_enclosing_blocks (instrs : unit Instr.t list) : Instr.Label.t Instr.Label.Map.t =
  let add (instr : Instr.Label.t) (current_block : Instr.Label.t option) (mapping : Instr.Label.t Instr.Label.Map.t) =
    match current_block with
    | Some l -> Instr.Label.Map.set mapping ~key:instr ~data:l
    | None -> mapping in
  let rec loop (instrs : unit Instr.t list) (current_block : Instr.Label.t option) (acc : Instr.Label.t Instr.Label.Map.t) =
    match instrs with
    | [] -> acc
    | (Data i) :: rest -> loop rest current_block (add i.label current_block acc)
    | (Control i) :: rest ->
      let acc = add i.label current_block acc in
      loop rest current_block (match i.instr with
        | Block (_, _, body) ->
          loop body (Some i.label) acc
        | Loop (_, _, body) ->
          loop body (Some i.label) acc
        | If (_, _, then_, else_) ->
          loop else_ (Some i.label) (loop then_ (Some i.label) acc)
        | _ -> acc)
  in
  loop instrs None Instr.Label.Map.empty

(** Constructs a CFG for function `fid` in a module. *)
let build (module_ : Wasm_module.t) (fid : Int32.t) : unit Cfg.t =
  (* TODO: this implementation is really not ideal and should be cleaned *)
  let rec check_no_rest (rest : 'a Instr.t list) : unit = match rest with
    | [] -> ()
    | Control { instr = Unreachable; _ } :: rest -> check_no_rest rest
    | _ -> Log.info (Printf.sprintf "Ignoring unreachable instructions after jump: %s" (Instr.list_to_string rest (fun _ -> "")))
  in
  let simplify = true in
  let funcinst = Wasm_module.get_funcinst module_ fid in
  let cur_idx : int ref = ref 0 in
  let new_idx () : int = let v = !cur_idx in cur_idx := v + 1; v in
  let mk_data_block (reverse_instrs : (Instr.data, unit) Instr.labelled list) : unit Basic_block.t =
    let instrs = List.rev reverse_instrs in
    Basic_block.{ idx = new_idx (); content = Data instrs; } in
  let mk_control_block (instr : (unit Instr.control, unit) Instr.labelled) : unit Basic_block.t =
    Basic_block.{ idx = new_idx () ; content = Control instr } in
  let mk_merge_block () =
    let idx = new_idx () in
    Basic_block.{ idx ; content = Control {
        instr = Merge;
        label = { section = MergeInFunction fid; id = idx };
        annotation_before = ();
        annotation_after = ();
      };
    } in
  let mk_empty_block () : unit Basic_block.t =
    Basic_block.{ idx = new_idx () ; content = Data []; } in
  let loop_heads = ref IntSet.empty in
  let entry_exit = ref [] in (* TODO: can be removed *)
  let rec helper (instrs : (Instr.data, unit) Instr.labelled list) (remaining : 'a Instr.t list) : (
    (* The blocks created *)
    'a Basic_block.t list *
    (* The edges within the blocks created *)
    (int * int * (bool option)) list *
    (* The break points as (block_idx, break_level, edge_data) *)
    (int * Int32.t * (bool option)) list *
    (* The blocks that have to be connected to the return *)
    int list *
    (* The entry and exit of the created blocks *)
    int * int) =
    match remaining with
    | [] ->
      (* If there's no instruction anymore, build the block and connect it to exit_id *)
      let block = mk_data_block instrs in
      ([block] (* only this block *), [] (* no edge *),
       [] (* no break point *), [] (* not connected to return *),
       block.idx, block.idx)
    | Data instr :: rest ->
      (* Instruction instr is part of the block, but not the end of it so we continue *)
      helper (instr :: instrs) rest
    | Control instr :: rest -> begin match instr.instr with
        | Merge -> failwith "cfg_builder: There should be no merge instructions before constructing the CFG"
        | BrIf level ->
          (* This is a break up to level `level` *)
          (* First, construct the current block *)
          let block = mk_data_block instrs in
          (* Then construct the brif block *)
          let brif_block = mk_control_block instr in
          (* Finally, construct the rest of the CFG *)
          let (blocks, edges, breaks, returns, entry', exit') = helper [] rest in
          (
            (* add the new blocks *)
            block :: brif_block :: blocks,
            (* add an edge between this block, brif (false branch), and finally the rest *)
            (block.idx, brif_block.idx, None) :: (brif_block.idx, entry', Some false) :: edges,
            (* add a break for the t branch *)
            (brif_block.idx, level, Some true) :: breaks,
            (* no return *)
            returns,
            block.idx, exit')
        |  If (_bt, _arity, instrs1, instrs2) ->
          (* Construct the current block *)
          let block = mk_data_block instrs in
          (* Construct the if block *)
          let if_block = mk_control_block instr in
          (* Visit the then and else branches *)
          let (blocks, edges, breaks, returns, then_entry, then_exit) = helper [] instrs1 in
          let (blocks', edges', breaks', returns', else_entry, else_exit) = helper [] instrs2 in
          (* Construct the merge block *)
          let merge_block = mk_merge_block () in
          entry_exit := (if_block.idx, merge_block.idx) :: !entry_exit;
          (* Construct the rest of the CFG *)
          let (blocks'', edges'', breaks'', returns'', entry, exit') = helper [] rest in
          (* Compute the new break levels (just like for Block and Loop)
             All breaks with level 0 break the current block
             All other breaks see their level decreased by one *)
          let all_breaks = breaks @ breaks' in
          let new_breaks = List.map (List.filter all_breaks
                                       ~f:(fun (_, level, _) -> Int32.(level > 0l)))
              ~f:(fun (idx, level, branch) -> (idx, Int32.(level - 1l), branch)) in
          let break_edges = List.map (List.filter all_breaks
                                        ~f:(fun (_, level, _) -> Int32.(level = 0l)))
              ~f:(fun (idx, _, branch) -> (idx, merge_block.idx, branch)) in
          (block :: if_block :: merge_block :: (blocks @ blocks' @ blocks''),
           (* Edges *)
           (block.idx, if_block.idx, None) :: (if_block.idx, then_entry, Some true) :: (if_block.idx, else_entry, Some false) ::
           (then_exit, merge_block.idx, None) :: (else_exit, merge_block.idx, None) :: (merge_block.idx, entry, None) :: (break_edges @ edges @ edges' @ edges''),
           (* no breaks and returns *)
           new_breaks @ breaks'',
           returns @ returns' @ returns'',
           block.idx, exit')
        | Br level ->
          (* Similar to break, but because it is inconditional, there is no edge
             from this block to the next. In practice, rest should always be
             empty here *)
          check_no_rest rest;
          let block = mk_data_block instrs in
          let br_block = mk_control_block instr in
          let (blocks, edges, breaks, returns, _entry', exit') = helper [] rest in
          (block :: br_block :: blocks (* add the current block *),
           (block.idx, br_block.idx, None) :: edges (* only sequential edge *),
           (br_block.idx, level, None) :: breaks (* add the break *),
           returns (* no return *),
           block.idx, exit')
        | BrTable (table, level) ->
          (* Similar to break, but there are multiple outgoing edges here *)
          check_no_rest rest;
          let block = mk_data_block instrs in
          let br_block = mk_control_block instr in
          let (blocks, edges, breaks, returns, _entry', exit') = helper [] rest in
          (block :: br_block :: blocks (* add the current block *),
           (block.idx, br_block.idx, None) :: edges (* only sequential edge *),
           (br_block.idx, level, None) :: (List.map table ~f:(fun lvl -> (br_block.idx, lvl, None))) @ breaks (* add all the breaks *),
           returns,
           block.idx, exit')
        | Call _ | CallIndirect _ ->
          (* Also similar to br, but connects the edges differently. Moreover,
             we don't include the call in this block because it has to be
             treated differently. *)
          let block = mk_data_block instrs in
          let call_block = mk_control_block instr in
          let (blocks, edges, breaks, returns, entry', exit') = helper [] rest in
          ((* add the current block and the function block *)
            block :: call_block :: blocks,
            (* connect current block to function block, and function block to the rest *)
            (block.idx, call_block.idx, None) :: (call_block.idx, entry', None) :: edges,
            (* no break and no return*)
            breaks,
            returns,
            block.idx, exit')
        | ((Block (_bt, _arity, instrs')) as b)
        | ((Loop (_bt, _arity, instrs')) as b) ->
          (* Create a new block with all instructions collected, without the current instruction *)
          let block = mk_data_block instrs in
          (* Is the current instruction a loop? *)
          let is_loop = match b with
            | Loop _ -> true
            | _ -> false in
          (* The block entry *)
          let block_entry = if is_loop then mk_merge_block () else mk_empty_block () in
          begin if is_loop then
              loop_heads := IntSet.add !loop_heads block_entry.idx
          end;
          (* Recurse inside the block *)
          let (blocks, edges, breaks, returns, entry', exit') = helper [] instrs' in
          (* Create a node for the exit of the block *)
          let block_exit = if is_loop then mk_empty_block () else mk_merge_block () in
          entry_exit := (block_entry.idx, block_exit.idx) :: !entry_exit;
          (* Recurse after the block *)
          let (blocks', edges', breaks', returns', entry'', exit'') = helper [] rest in
          (* Compute the new break levels:
             All breaks with level 0 break the current block
             All other breaks see their level decreased by one.
             Important: break are handled differently within loop: a break goes back to the loop entry*)
          let all_breaks = breaks in
          let new_breaks = breaks' @ (List.map (List.filter all_breaks
                                                  ~f:(fun (_, level, _) -> Int32.(level > 0l))))
              ~f:(fun (idx, level, branch) -> (idx, Int32.(level - 1l), branch)) in
          let break_edges = List.map (List.filter all_breaks
                                        ~f:(fun (_, level, _) -> Int32.(level = 0l)))
              ~f:(fun (idx, _, branch) -> (idx, (if is_loop then block_entry.idx else block_exit.idx), branch)) in
          (* Compute the new edges. This is different between a loop and a
             block, for the exit of the inside of the block *)
          let new_edges = if is_loop then
              [(block.idx, block_entry.idx, None); (block_entry.idx, entry', None); (exit', block_exit.idx, None); (block_exit.idx, entry'', None)]
            else
              [(block.idx, block_entry.idx, None); (block_entry.idx, entry', None); (exit', block_exit.idx, None); (block_exit.idx, entry'', None)]
          in
          (block :: block_entry :: block_exit :: (blocks @ blocks') (* add all blocks *),
           new_edges @ break_edges @ edges @ edges' (* add edges *),
           new_breaks (* filtered breaks *),
           returns @ returns' (* returns are propagated as is *),
           block.idx, exit'')
        | Return ->
          (* Return block. The rest of the instructions does not matter (it
             should be empty) *)
          check_no_rest rest;
          (* We create a new block with all instructions collected *)
          let block = mk_data_block instrs in
          (* We create a control block for this return *)
          let return_block = mk_control_block instr in
          ([return_block; block],
           (* The previous block is connected to the return *)
           [(block.idx, return_block.idx, None)],
           (* No breaks *)
           [],
           (* The return block is marked as returning *)
           [return_block.idx],
           (* The entry block *)
           block.idx,
           (* The exit block (it should not matter here) *)
           return_block.idx)
        | Unreachable ->
          (* Simply construct a block containig the unreachable instruction *)
          let block = mk_data_block instrs in
          let unreachable_block = mk_control_block instr in
          let (blocks, edges, breaks, returns, _entry', exit') = helper [] rest in
          (block :: unreachable_block :: blocks,
           (block.idx, unreachable_block.idx, None) :: edges (* The unreachable block is not connected to anything except to the "return" block of the CFG *),
           breaks, unreachable_block.idx :: returns, block.idx, exit')

      end
  in
  let (blocks, edges, breaks, returns, _entry_idx, exit_idx) = helper [] funcinst.code.body in
  (* Create the return block *)
  let return_block = mk_merge_block () in
  let blocks' = return_block :: blocks in
  (* There can still be breaks to the exit of the function *)
  let breaks_to_exit, remaining_breaks = List.partition_tf breaks ~f:(fun (_, lvl, _) -> Int32.(lvl = 0l)) in
  begin if not (List.is_empty remaining_breaks) then
      (* there shouldn't be any breaks outside the function *)
      Log.warn (Printf.sprintf "There are %d breaks outside of function %ld" (List.length breaks) fid)
  end;
  (* Connect the return block and the remaining breaks to it, and remove all edges that start from a return block (as they are unreachable) *)
  let edges' = (exit_idx, return_block.idx, None) ::
               List.map breaks_to_exit ~f:(fun (block_idx, _, data) -> (block_idx, return_block.idx, data)) @
               List.map returns ~f:(fun from -> (from, return_block.idx, None)) @ (List.filter edges ~f:(fun (src, _, _) -> not (List.mem returns src ~equal:Stdlib.(=)))) in
  (* We now filter empty normal blocks *)
  let (actual_blocks, filtered_blocks) = List.partition_tf blocks' ~f:(fun block -> match block.content with
      | Data [] ->
        (* only filter if we need to simplify, and it is an empty data block which is not a block/loop entry or exit *)
        if simplify then
          false
        else
          true
      | _ -> true) in
  let filtered_blocks_idx = List.map filtered_blocks ~f:(fun b -> b.idx) in
  (* And we have to redirect the edges: if there is an edge to a removed block, we make it point to its successors *)
  let actual_edges = List.fold_left filtered_blocks_idx ~init:edges' ~f:(fun edges idx ->
      (* idx is removed, so we find all edges pointing to idx (and we keep track of the other ones, as only these should be kept) *)
      let (pointing_to, edges') = List.partition_tf edges ~f:(fun (_, dst, _) -> dst = idx) in
      (* and we find all edges pointing from idx (again, keeping track of the other ones) *)
      let (pointing_from, edges'') = List.partition_tf edges' ~f:(fun (src, _, _) -> src = idx) in
      (* now we connect everything from both sets *)
      List.concat (List.map pointing_to ~f:(fun (src, _, data) -> List.map pointing_from ~f:(fun (_, dst, data') ->
          match (data, data') with
          | Some b, None -> (src, dst, Some b)
          | None, Some b -> (src, dst, Some b)
          | None, None -> (src, dst, None)
          | Some _, Some _ -> failwith "trying to merge two conditional edges, should not happen"))) @ edges'') in

  (* Create the entry block if needed *)
  let first_block = Option.value_exn (List.min_elt (List.map actual_blocks ~f:(fun b -> b.idx)) ~compare:Stdlib.compare) in
  (* In general, the first block is the entry block. But in some cases, it could be a block with back edges, and we want to avoid that. So we check if there's an edge to the entry block: if there is one, we need an extra entry block *)
  let entry_block, actual_blocks', actual_edges' = match List.find actual_edges ~f:(fun (_, idx, _) -> idx = first_block) with
    | None -> first_block, actual_blocks, actual_edges
    | Some _ ->
      let block = mk_empty_block () in
      block.idx, block :: actual_blocks, (block.idx, first_block, None) :: actual_edges in

  let edges = IntMap.map (IntMap.of_alist_multi (List.map actual_edges' ~f:(fun (src, dst, data) -> (src, (dst, data)))))
      ~f:(fun es -> Cfg.Edge.Set.of_list es) in
  let back_edges = IntMap.map (IntMap.of_alist_multi (List.map actual_edges' ~f:(fun (left, right, data) -> (right, (left, data)))))
      ~f:(fun es -> Cfg.Edge.Set.of_list es) in
  let basic_blocks = IntMap.of_alist_exn (List.map actual_blocks' ~f:(fun b -> (b.idx, b))) in

  let label_to_instr =
    let rec loop (instrs : unit Instr.t list) (acc : unit Instr.t Instr.Label.Map.t) : unit Instr.t Instr.Label.Map.t =
      match instrs with
      | [] -> acc
      | (Control { instr = Block (_, _, instrs); label; _ } as i) :: rest
      | (Control { instr = Loop (_, _, instrs); label; _ } as i) :: rest ->
        loop (rest @ instrs) (Instr.Label.Map.add_exn acc ~key:label ~data:i)
      | (Control { instr = If (_, _, instrs1, instrs2); label; _ } as i) :: rest ->
        loop (rest @ instrs1 @ instrs2) (Instr.Label.Map.add_exn acc ~key:label ~data:i)
      | i :: rest ->
        loop rest (Instr.Label.Map.add_exn acc ~key:(Instr.label i) ~data:i)
    in
    loop funcinst.code.body Instr.Label.Map.empty in
  Cfg.{
    (* Exported functions have names, non-exported don't *)
    exported = Option.is_some funcinst.name;
    (* The name itself *)
    name = Option.value funcinst.name ~default:"<unexported>";
    (* The index of this block is the integer that represent the address of this function *)
    idx = fid;
    (* The type index of this function *)
    type_idx = funcinst.type_idx;
    (* Global types *)
    global_types = module_.imported_global_types @ module_.global_types;
    (* Argument types *)
    arg_types = fst funcinst.typ;
    (* Return types *)
    return_types = snd funcinst.typ;
    (* Types of the locals *)
    local_types = funcinst.code.locals;
    (* The basic blocks *)
    basic_blocks = basic_blocks;
    (* The forward edges *)
    edges;
    (* The backward edges *)
    back_edges;
    (* The entry block *)
    entry_block = entry_block;
    (* The exit block is the return block *)
    exit_block = return_block.idx;
    (* The loop heads *)
    loop_heads = !loop_heads;
    (* The instruction labels in the right order *)
    instructions = (List.map funcinst.code.body ~f:Instr.label);
    (* A mapping from instruction labels to instructions *)
    label_to_instr;
    (* Arity of each block *)
    block_arities = compute_block_arities funcinst.code.body;
    (* Mapping from label to enclosing block *)
    label_to_enclosing_block = compute_enclosing_blocks funcinst.code.body;
  }

let build_all (mod_ : Wasm_module.t) : unit Cfg.t Int32Map.t =
  Int32Map.of_alist_exn (List.mapi mod_.funcs ~f:(fun i _ ->
      let faddr = Int32.(mod_.nfuncimports + (Int32.of_int_exn i)) in
      (faddr, build mod_ faddr)))

module Test = struct
  (** Check that building the CFG for each function of a .wat file succeeds.
      Does not actually check that the CFG is correct. *)
  let test_cfgs file =
    let wasm_mod = Wasm_module.of_file file in
    List.iteri wasm_mod.funcs
      ~f:(fun i _ ->
          let faddr = Int32.(wasm_mod.nfuncimports + (Int32.of_int_exn i)) in
          let _ : unit Cfg.t = build wasm_mod faddr in
          ())

  let%test_unit "CFG for simple.wat can be built" = test_cfgs "../../../test/simple.wat"
  let%test_unit "CFG for if-loop.wat can be built" = test_cfgs "../../../test/if-loop.wat"
  let%test_unit "CFG for rel.wat can be built"  = test_cfgs "../../../test/rel.wat"
  let%test_unit "CFG for memcpy.wat can be built" = test_cfgs "../../../test/memcpy.wat"
  let%test_unit "CFG for loop.wat can be built" = test_cfgs "../../../test/loop.wat"
  let%test_unit "CFG for loop-brif.wat can be built" = test_cfgs "../../../test/loop-brif.wat"
end

