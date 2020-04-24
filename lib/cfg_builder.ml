open Core_kernel
open Helpers

let build (faddr : Address.t) (m : Wasm_module.t) : Cfg.t =
  let funcinst = Wasm_module.get_funcinst m faddr in
  let cur_idx : int ref = ref 0 in
  let new_idx () : int = let v = !cur_idx in cur_idx := v + 1; v in
  let mk_block (reverse_instrs : Instr.t list) : Basic_block.t =
    let instrs = List.rev reverse_instrs in
    Basic_block.{ idx = new_idx (); instrs = instrs; sort = Basic_block.Normal; } in
  let mk_funblock (f : Address.t) : Basic_block.t =
    Basic_block.{ idx = new_idx () ; instrs = [Call f]; sort = Function } in
  let mk_block_entry (is_loop : bool) : Basic_block.t =
    Basic_block.{ idx = new_idx () ; instrs = [];
                  sort = if is_loop then LoopEntry else BlockEntry } in
  let mk_block_exit (is_loop : bool) : Basic_block.t =
    Basic_block.{ idx = new_idx () ; instrs = [];
                  sort = if is_loop then LoopExit else BlockExit } in
  let mk_block_return () : Basic_block.t =
    Basic_block.{ idx = new_idx () ; instrs = []; sort = Return } in
  let rec helper (instrs : Instr.t list) (remaining : Instr.t list) : (
    (* The blocks created *)
    Basic_block.t list *
    (* The edges within the blocks created *)
    (int * int) list *
    (* The break points as (block_idx, break_level *)
    (int * int) list *
    (* The blocks that have to be connected to the return *)
    int list *
    (* The entry and exit of the created blocks *)
    int * int) =
    match remaining with
    | [] ->
      (* If there's no instruction anymore, build the block and connect it to exit_id *)
      let block = mk_block instrs in
      ([block] (* only this block *), [] (* no edge *),
       [] (* no break point *), [] (* not connected to return *),
       block.idx, block.idx)
    | BrIf level :: rest ->
      (* This is a break up to level `level` *)
      (* First, construct the current block *)
      let block = mk_block (BrIf level :: instrs) in
      (* Then, construct the rest of the CFG *)
      let (blocks, edges, breaks, returns, entry', exit') = helper [] rest in
      (block :: blocks (* add the current block *),
       (block.idx, entry') :: edges (* add an edge between this block and the rest *),
       (block.idx, level) :: breaks (* add a break *),
       returns (* no return *),
       block.idx, exit')
    | Br level :: rest ->
      (* Similar to break, but because it is inconditional, there is no edge from this block to the next. In practice, rest should always be empty here *)
      assert (rest = []);
      let block = mk_block (Br level :: instrs) in
      let (blocks, edges, breaks, returns, _entry', exit') = helper [] rest in
      (block :: blocks (* add the current block *),
       edges (* no edge *),
       (block.idx, level) :: breaks (* add the break *),
       returns (* no return *),
       block.idx, exit')
    | Call f :: rest ->
      (* Also similar to br, but connects the edges differently. Moreover, we don't include the call in this block because it has to be treated differently. *)
      let block = mk_block (instrs) in
      let fblock = mk_funblock f in
      let (blocks, edges, breaks, returns, entry', exit') = helper [] rest in
      (block :: fblock :: blocks (* add the current block and the function block *),
       (block.idx, fblock.idx) :: (fblock.idx, entry') :: edges (* connect current block to function block, and function block to the rest *),
       breaks (* no break *),
       returns (* no return *),
       block.idx, exit')
    | ((Block instrs') as b) :: rest
    | ((Loop instrs') as b) :: rest ->
      (* Create a new block with all instructions collected, without the last one *)
      let block = mk_block instrs in
      let is_loop = match b with
        | Loop _ -> true
        | _ -> false in
      let block_entry = mk_block_entry is_loop in
      (* Recurse inside the block *)
      let (blocks, edges, breaks, returns, entry', exit') = helper [] instrs' in
      (* Create a node for the exit of the block *)
      let block_exit = mk_block_exit is_loop in
      (* Recurse after the block *)
      let (blocks', edges', breaks', returns', entry'', exit'') = helper [] rest in
      (* Compute the new break levels:
         All breaks with level 0 break the current block
         All other breaks see their level decreased by one *)
      let new_breaks = List.map (List.filter (breaks @ breaks') ~f:(fun (_, level) -> level > 0)) ~f:(fun (idx, level) -> (idx, level -1)) in
      let break_edges = List.map (List.filter (breaks @ breaks') ~f:(fun (_, level) -> level = 0)) ~f:(fun (idx, _) -> (idx, block_exit.idx)) in
      (* Compute the new edges. This is different between a loop and a block, for the exit of the inside of the block *)
      let new_edges = if is_loop then
          [(block.idx, block_entry.idx); (block_entry.idx, entry'); (exit', block_entry.idx); (block_exit.idx, block_entry.idx)]
        else
          [(block.idx, block_entry.idx); (block_entry.idx, entry'); (exit', block_exit.idx); (block_exit.idx, entry'')]
      in
      (block :: block_entry :: block_exit :: (blocks @ blocks') (* add all blocks *),
       new_edges @ break_edges @ edges @ edges' (* add edges *),
       new_breaks (* filtered breaks *),
       returns @ returns' (* returns are propagated as is *),
       block.idx, exit'')
    | Return :: rest ->
      (* Return block. The rest of the instructions does not matter (it should be empty) *)
      assert (rest = []);
      (* We create a new block with all instructions collected, and return it *)
      let block = mk_block instrs in
      (* It is not connected to anything, but is marked as to be connected to a return block *)
      ([block], [], [], [block.idx], block.idx, block.idx)
    | i :: rest ->
      (* Instruction i is part of the block, but not the end of it so we continue *)
      helper (i :: instrs) rest
  in
  let (blocks, edges, breaks, returns, _entry_idx, exit_idx) = helper [] funcinst.code.body in
  let return_block = mk_block_return () in
  let blocks' = return_block :: blocks in
  let edges' = (exit_idx, return_block.idx) :: List.map returns ~f:(fun from -> (from, return_block.idx)) @ edges in
  assert (breaks = []); (* there shouldn't be any breaks outside the function *)
  (* We now filter empty normal blocks *)
  let (actual_blocks, filtered_blocks) = List.partition_tf blocks' ~f:(fun block -> match (block.sort, block.instrs) with
      | Normal, [] -> false
      | _ -> true) in
  let filtered_blocks_idx = List.map filtered_blocks ~f:(fun b -> b.idx) in
  (* And we have to redirect the edges: if there is an edge to a removed block, we make it point to its successors *)
  let actual_edges = List.fold_left filtered_blocks_idx ~init:edges' ~f:(fun edges idx ->
      (* idx is removed, so we find all edges pointing to idx (and we keep track of the other ones, as only these should be kept) *)
      let (pointing_to, edges') = List.partition_tf edges ~f:(fun (_, dst) -> dst = idx) in
      (* and we find all edges pointing from idx (again, keeping track of the other ones) *)
      let (pointing_from, edges'') = List.partition_tf edges' ~f:(fun (src, _) -> src = idx) in
      (* now we connect everything from both sets *)
      List.concat (List.map pointing_to ~f:(fun (src, _) -> List.map pointing_from ~f:(fun (_, dst) -> (src, dst)))) @ edges'') in
  Cfg.{
    (* Exported functions have names, non-exported don't *)
    exported = Option.is_some funcinst.name;
    (* The name itself *)
    name = Option.value funcinst.name ~default:"<unexported>";
    (* The index of this block is the integer that represent the address of this function *)
    idx = faddr;
    (* Arity of the function *)
    arity = funcinst.arity;
    (* Number of locals in the function *)
    nlocals = List.length funcinst.code.locals;
    (*The basic blocks *)
    basic_blocks = BasicBlocks.IntMap.of_alist_exn (List.map actual_blocks ~f:(fun b -> (b.idx, b)));
    (* The forward edges *)
    edges = IntMap.of_alist_multi actual_edges;
    (* The backward edges *)
    back_edges = IntMap.of_alist_multi (List.map actual_edges ~f:(fun (left, right) -> (right, left)));
    (* The entry block *)
    (* TODO: probably not fully correct so we have to pay close attention to that: there should be a single entry block *)
    entry_block = Option.value_exn (List.min_elt (List.map actual_blocks ~f:(fun b -> b.idx)) ~compare:Pervasives.compare);
    (* The exit block is the return block *)
    exit_block = return_block.idx }
