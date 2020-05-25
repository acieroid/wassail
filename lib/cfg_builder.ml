open Core_kernel
open Helpers


let build (faddr : Address.t) (m : Wasm_module.t) : Cfg.t =
  (* true to simplify the CFG, false to disable simplification *)
  let simplify = true in
  let funcinst = Wasm_module.get_funcinst m faddr in
  let cur_idx : int ref = ref 0 in
  let new_idx () : int = let v = !cur_idx in cur_idx := v + 1; v in
  let mk_data_block (reverse_instrs : Instr.data list) : Basic_block.t =
    let instrs = List.rev reverse_instrs in
    Basic_block.{ idx = new_idx (); content = Data instrs } in
  let mk_control_block (instr : Instr.control) : Basic_block.t =
    Basic_block.{ idx = new_idx () ; content = Control instr } in
  let mk_empty_block () : Basic_block.t =
    Basic_block.{ idx = new_idx () ; content = Nothing } in
  let rec helper (instrs : Instr.data list) (remaining : Instr.t list) : (
    (* The blocks created *)
    Basic_block.t list *
    (* The edges within the blocks created *)
    (int * int * (bool option)) list *
    (* The break points as (block_idx, break_level, edge_data) *)
    (int * int * (bool option)) list *
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
    | Control instr :: rest -> begin match instr with
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
        | If (instrs1, instrs2) ->
          (* Construct the current block *)
          let block = mk_data_block instrs in
          let if_block = mk_control_block instr in
          (* Visit the then and else branches *)
          let (blocks, edges, breaks, returns, then_entry, then_exit) = helper [] instrs1 in
          let (blocks', edges', breaks', returns', else_entry, else_exit) = helper [] instrs2 in
          (* Construct the rest of the CFG *)
          let (blocks'', edges'', breaks'', returns'', entry, exit') = helper [] rest in
          (* Compute the new break levels (just like for Block and Loop)
             All breaks with level 0 break the current block
             All other breaks see their level decreased by one *)
          let all_breaks = breaks @ breaks' in
          let new_breaks = List.map (List.filter all_breaks
                                       ~f:(fun (_, level, _) -> level > 0))
              ~f:(fun (idx, level, branch) -> (idx, level - 1, branch)) in
          let break_edges = List.map (List.filter all_breaks
                                        ~f:(fun (_, level, _) -> level = 0))
              ~f:(fun (idx, _, branch) -> (idx, entry, branch)) in
          (block :: if_block :: (blocks @ blocks' @ blocks''),
           (* Edges *)
           (block.idx, if_block.idx, None) :: (if_block.idx, then_entry, Some true) :: (if_block.idx, else_entry, Some false) ::
           (then_exit, entry, None) :: (else_exit, entry, None) :: (break_edges @ edges @ edges' @ edges''),
           (* no breaks and returns *)
           new_breaks @ breaks'',
           returns @ returns' @ returns'',
           block.idx, exit')
        | Br level ->
          (* Similar to break, but because it is inconditional, there is no edge
             from this block to the next. In practice, rest should always be
             empty here *)
          assert (List.is_empty rest);
          let block = mk_data_block instrs in
          let br_block = mk_control_block instr in
          let (blocks, edges, breaks, returns, _entry', exit') = helper [] rest in
          (block :: br_block :: blocks (* add the current block *),
           (block.idx, br_block.idx, None) :: edges (* only sequential edge *),
           (br_block.idx, level, None) :: breaks (* add the break *),
           returns (* no return *),
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
        | ((Block instrs') as b)
        | ((Loop instrs') as b) ->
          (* Create a new block with all instructions collected, without the
             last one *)
          let block = mk_data_block instrs in
          let is_loop = match b with
            | Loop _ -> true
            | _ -> false in
          let block_entry = mk_empty_block () in
          (* Recurse inside the block *)
          let (blocks, edges, breaks, returns, entry', exit') = helper [] instrs' in
          (* Create a node for the exit of the block *)
          let block_exit = mk_empty_block () in
          (* Recurse after the block *)
          let (blocks', edges', breaks', returns', entry'', exit'') = helper [] rest in
          (* Compute the new break levels:
             All breaks with level 0 break the current block
             All other breaks see their level decreased by one *)
          let all_breaks = breaks @ breaks' in (* TODO: shouldn't it be without breaks'? *)
          let new_breaks = List.map (List.filter all_breaks
                                       ~f:(fun (_, level, _) -> level > 0))
              ~f:(fun (idx, level, branch) -> (idx, level - 1, branch)) in
          let break_edges = List.map (List.filter all_breaks
                                        ~f:(fun (_, level, _) -> level = 0))
              ~f:(fun (idx, _, branch) -> (idx, block_exit.idx, branch)) in
          (* Compute the new edges. This is different between a loop and a
             block, for the exit of the inside of the block *)
          let new_edges = if is_loop then
              [(block.idx, block_entry.idx, None); (block_entry.idx, entry', None); (exit', block_entry.idx, None); (block_exit.idx, block_entry.idx, None)]
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
          assert (List.is_empty rest);
          (* We create a new block with all instructions collected, and return
             it *)
          let block = mk_data_block instrs in
          (* It is not connected to anything, but is marked as to be connected to a return block *)
          ([block], [], [], [block.idx], block.idx, block.idx)
        | Unreachable ->
          (* Simply construct a block containig the unreachable instruction *)
          let block = mk_data_block instrs in
          let unreachable_block = mk_control_block instr in
          let (blocks, edges, breaks, returns, entry', exit') = helper [] rest in
          (block :: unreachable_block :: blocks,
           (block.idx, unreachable_block.idx, None) :: (unreachable_block.idx, entry', None) :: edges,
           breaks, returns, block.idx, exit')

      end
  in
  let (blocks, edges, breaks, returns, _entry_idx, exit_idx) = helper [] funcinst.code.body in
  (* Create the return block *)
  let return_block = mk_empty_block () in
  let blocks' = return_block :: blocks in
  (* Connect the return block, and remove all edges that start from a return block (as they are unreachable) *)
  let edges' = (exit_idx, return_block.idx, None) :: List.map returns ~f:(fun from -> (from, return_block.idx, None)) @ (List.filter edges ~f:(fun (src, _, _) -> not (List.mem returns src ~equal:Stdlib.(=)))) in
  assert (List.is_empty breaks); (* there shouldn't be any breaks outside the function *)
  (* We now filter empty normal blocks *)
  let (actual_blocks, filtered_blocks) = List.partition_tf blocks' ~f:(fun block -> match block.content with
      | Data [] -> not simplify (* only filter if we need to simplify *)
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
  Cfg.{
    (* Exported functions have names, non-exported don't *)
    exported = Option.is_some funcinst.name;
    (* The name itself *)
    name = Option.value funcinst.name ~default:"<unexported>";
    (* The index of this block is the integer that represent the address of this function *)
    idx = faddr;
    (* Argument types *)
    arg_types = fst funcinst.typ;
    (* Return types *)
    return_types = snd funcinst.typ;
    (* Types of the locals *)
    local_types = funcinst.code.locals;
    (*The basic blocks *)
    basic_blocks = BasicBlocks.IntMap.of_alist_exn (List.map actual_blocks ~f:(fun b -> (b.idx, b)));
    (* The forward edges *)
    edges = IntMap.of_alist_multi (List.map actual_edges ~f:(fun (src, dst, data) -> (src, (dst, data))));
    (* The backward edges *)
    back_edges = IntMap.of_alist_multi (List.map actual_edges ~f:(fun (left, right, data) -> (right, (left, data))));
    (* The entry block *)
    (* TODO: probably not fully correct so we have to pay close attention to that: there should be a single entry block *)
    entry_block = Option.value_exn (List.min_elt (List.map actual_blocks ~f:(fun b -> b.idx)) ~compare:Stdlib.compare);
    (* The exit block is the return block *)
    exit_block = return_block.idx }
