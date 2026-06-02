open Core
open Helpers

type func_metadata = {
  instructions: Instr.Label.t list;
  label_to_instr: unit Instr.t Instr.Label.Map.t;
  block_arities: (int * int) Instr.Label.Map.t;
  label_to_enclosing_block: Instr.Label.t Instr.Label.Map.t;
  label_to_enclosing_block_id: int Instr.Label.Map.t;
}

type build_stats = {
  mutable raw_cfg: float;
  mutable return_edges: float;
  mutable filter_empty_blocks: float;
  mutable redirect_empty_edges: float;
  mutable entry_block: float;
  mutable edge_maps: float;
  mutable block_map: float;
  mutable metadata: float;
  mutable enclosing_block_id: float;
}

let empty_build_stats () = {
  raw_cfg = 0.0;
  return_edges = 0.0;
  filter_empty_blocks = 0.0;
  redirect_empty_edges = 0.0;
  entry_block = 0.0;
  edge_maps = 0.0;
  block_map = 0.0;
  metadata = 0.0;
  enclosing_block_id = 0.0;
}

let add_build_stats dst src =
  dst.raw_cfg <- dst.raw_cfg +. src.raw_cfg;
  dst.return_edges <- dst.return_edges +. src.return_edges;
  dst.filter_empty_blocks <- dst.filter_empty_blocks +. src.filter_empty_blocks;
  dst.redirect_empty_edges <- dst.redirect_empty_edges +. src.redirect_empty_edges;
  dst.entry_block <- dst.entry_block +. src.entry_block;
  dst.edge_maps <- dst.edge_maps +. src.edge_maps;
  dst.block_map <- dst.block_map +. src.block_map;
  dst.metadata <- dst.metadata +. src.metadata;
  dst.enclosing_block_id <- dst.enclosing_block_id +. src.enclosing_block_id

let time_build_phase stats add_elapsed f =
  match stats with
  | None -> f ()
  | Some stats ->
    let start = Time_float.now () in
    let result = f () in
    let elapsed = Time_float.Span.to_sec (Time_float.diff (Time_float.now ()) start) in
    add_elapsed stats elapsed;
    result

let compute_func_metadata (instrs : unit Instr.t list) (basic_blocks : unit Basic_block.t list) : func_metadata =
  let add_enclosing label current_block label_to_enclosing_block =
    match current_block with
    | Some block_label -> Instr.Label.Map.set label_to_enclosing_block ~key:label ~data:block_label
    | None -> label_to_enclosing_block
  in
  let rec loop instrs current_block metadata =
    match instrs with
    | [] -> metadata
    | instr :: rest ->
      let label = Instr.label instr in
      let metadata = {
        metadata with
        label_to_instr = Instr.Label.Map.add_exn metadata.label_to_instr ~key:label ~data:instr;
        label_to_enclosing_block = add_enclosing label current_block metadata.label_to_enclosing_block;
      } in
      let metadata =
        match instr with
        | Instr.Control { instr = Instr.Block (_, arity, body); label; _ }
        | Instr.Control { instr = Instr.Loop (_, arity, body); label; _ } ->
          loop body (Some label)
            { metadata with
              block_arities = Instr.Label.Map.set metadata.block_arities ~key:label ~data:arity;
            }
        | Instr.Control { instr = Instr.If (_, arity, then_, else_); label; _ } ->
          loop else_ (Some label)
            (loop then_ (Some label)
               { metadata with
                 block_arities = Instr.Label.Map.set metadata.block_arities ~key:label ~data:arity;
               })
        | _ -> metadata
      in
      loop rest current_block metadata
  in
  let metadata =
    loop instrs None {
      instructions = List.map instrs ~f:Instr.label;
      label_to_instr = Instr.Label.Map.empty;
      block_arities = Instr.Label.Map.empty;
      label_to_enclosing_block = Instr.Label.Map.empty;
      label_to_enclosing_block_id = Instr.Label.Map.empty;
    }
  in
  let label_to_enclosing_block_id =
    let add label idx acc = Instr.Label.Map.add_exn acc ~key:label ~data:idx in
    List.fold basic_blocks ~init:Instr.Label.Map.empty ~f:(fun acc bb ->
      match bb.content with
      | Control { label; _ } | Call { label; _ } -> add label bb.idx acc
      | Data instrs ->
        List.fold instrs ~init:acc ~f:(fun acc instr -> add instr.label bb.idx acc)
      | Entry | Return _ | Imported _ -> acc)
  in
  { metadata with label_to_enclosing_block_id }

let merge_edge_data (left : bool option) (right : bool option) : bool option =
  match left, right with
  | Some b, None | None, Some b -> Some b
  | None, None -> None
  | Some _, Some _ -> failwith "trying to merge two conditional edges, should not happen"

let redirect_edges_around_empty_blocks
    (filtered_blocks_idx : int list)
    (edges : (int * int * bool option) list)
  : (int * int * bool option) list =
  let filtered_blocks = IntSet.of_list filtered_blocks_idx in
  let outgoing =
    IntMap.of_alist_multi
      (List.map edges ~f:(fun (src, dst, data) -> src, (dst, data)))
  in
  let cache = Hashtbl.Poly.create () in
  let rec reachable_non_empty_blocks seen dst data =
    if not (IntSet.mem filtered_blocks dst) then
      [dst, data]
    else if IntSet.mem seen dst then
      []
    else
      let compute () =
        match IntMap.find outgoing dst with
        | None -> []
        | Some successors ->
          let seen = IntSet.add seen dst in
          List.concat_map successors ~f:(fun (dst', data') ->
              reachable_non_empty_blocks seen dst' (merge_edge_data data data'))
      in
      if IntSet.is_empty seen then
        Hashtbl.find_or_add cache (dst, data) ~default:compute
      else
        compute ()
  in
  List.concat_map edges ~f:(fun (src, dst, data) ->
      if IntSet.mem filtered_blocks src then
        []
      else
        List.map (reachable_non_empty_blocks IntSet.empty dst data)
          ~f:(fun (dst, data) -> src, dst, data))

let concat3 left middle right =
  List.rev_append left (List.rev_append middle right)

type break_targets = (int * bool option) list IntMap.t

let empty_break_targets = IntMap.empty

let add_break_target breaks ~target_depth ~block_idx ~data =
  IntMap.update breaks target_depth ~f:(function
      | None -> [block_idx, data]
      | Some existing -> (block_idx, data) :: existing)

let merge_break_targets left right =
  IntMap.merge left right ~f:(fun ~key:_ -> function
      | `Left v | `Right v -> Some v
      | `Both (left, right) -> Some (List.rev_append left right))

let merge3_break_targets left middle right =
  merge_break_targets left (merge_break_targets middle right)

let pop_break_targets breaks target_depth =
  IntMap.find breaks target_depth |> Option.value ~default:[],
  IntMap.remove breaks target_depth

let break_target_depth current_depth level =
  current_depth - Int32.to_int_exn level

let count_break_targets breaks =
  IntMap.fold breaks ~init:0 ~f:(fun ~key:_ ~data acc -> acc + List.length data)

(** Constructs a CFG for function `fid` in a module. *)
let build ?stats:(stats : build_stats option) (module_ : Wasm_module.t) (fidx : Int32.t) : unit Cfg.t =
  (* XXX: this implementation is really not ideal and should be cleaned *)
  let rec check_no_rest (rest : 'a Instr.t list) : unit = match rest with
    | [] -> ()
    | Control { instr = Unreachable; _ } :: rest -> check_no_rest rest
    | _ -> Log.info (fun () -> Printf.sprintf "Ignoring unreachable instructions after jump: %s" (Instr.list_to_string rest (fun _ -> "")))
  in
  let simplify = true in
  let funcinst = Wasm_module.get_funcinst module_ fidx in
  let cur_idx : int ref = ref 0 in
  let new_idx () : int = let v = !cur_idx in cur_idx := v + 1; v in
  let mk_data_block (reverse_instrs : (Instr.data, unit) Instr.labelled list) : unit Basic_block.t =
    let instrs = List.rev reverse_instrs in
    Basic_block.{ idx = new_idx (); content = Data instrs; fidx; annotation_before = (); annotation_after = () } in
  let mk_data_node (reverse_instrs : (Instr.data, unit) Instr.labelled list) : int * unit Basic_block.t list * int list =
    match reverse_instrs with
    | [] ->
      let idx = new_idx () in
      idx, [], [idx]
    | _ ->
      let block = mk_data_block reverse_instrs in
      block.idx, [block], []
  in
  let mk_control_block (instr : (unit Instr.control, unit) Instr.labelled) : unit Basic_block.t =
    Basic_block.{ idx = new_idx (); content = Control instr; fidx; annotation_before = (); annotation_after = () } in
  let mk_call_block (instr : (Instr.call, unit) Instr.labelled) : unit Basic_block.t =
    Basic_block.{ idx = new_idx (); content = Call instr; fidx; annotation_before = (); annotation_after = () } in
  let mk_merge_block () =
    let idx = new_idx () in
    Basic_block.{ idx ; fidx; content = Control {
        instr = Merge;
        label = { section = MergeInFunction fidx; id = idx };
        line_number = -1;
        annotation_before = ();
        annotation_after = ();
      };
        annotation_before = ();
        annotation_after = ();
    } in
  let mk_empty_block () : unit Basic_block.t =
    Basic_block.{ idx = new_idx () ; content = Data []; fidx; annotation_before = (); annotation_after = () } in
  let new_virtual_empty_block_idx () : int = new_idx () in
  let loop_heads = ref IntSet.empty in
  let rec helper
      (depth : int)
      (instrs : (Instr.data, unit) Instr.labelled list)
      (remaining : 'a Instr.t list) : (
    (* The blocks created *)
    'a Basic_block.t list *
    (* The edges within the blocks created *)
    (int * int * (bool option)) list *
    (* The break points, keyed by the absolute target nesting depth. *)
    break_targets *
    (* The blocks that have to be connected to the return *)
    int list *
    (* Structural empty block ids that only exist for edge redirection. *)
    int list *
    (* The entry and exit of the created blocks *)
    int * int) =
    match remaining with
    | [] ->
      (* If there's no instruction anymore, build the block and connect it to exit_id *)
      let block_idx, block_blocks, virtual_empty_blocks = mk_data_node instrs in
      (block_blocks (* only this block *), [] (* no edge *),
       empty_break_targets (* no break point *), [] (* not connected to return *),
       virtual_empty_blocks,
       block_idx, block_idx)
    | Data instr :: rest ->
      (* Instruction instr is part of the block, but not the end of it so we continue *)
      helper depth (instr :: instrs) rest
    | Call instr :: rest ->
      (* Similar to br, but connects the edges differently. Moreover,
             we don't include the call in this block because it has to be
             treated differently. *)
      let block_idx, block_blocks, block_virtual_empty_blocks = mk_data_node instrs in
      let call_block = mk_call_block instr in
      let (blocks, edges, breaks, returns, virtual_empty_blocks, entry', exit') = helper depth [] rest in
      ((* add the current block and the function block *)
        List.rev_append block_blocks (call_block :: blocks),
        (* connect current block to function block, and function block to the rest *)
        (block_idx, call_block.idx, None) :: (call_block.idx, entry', None) :: edges,
        (* no break and no return*)
        breaks,
        returns,
        List.rev_append block_virtual_empty_blocks virtual_empty_blocks,
        block_idx, exit')
    | Control instr :: rest -> begin match instr.instr with
        | Merge -> failwith "cfg_builder: There should be no merge instructions before constructing the CFG"
        | BrIf level ->
          (* This is a break up to level `level` *)
          (* First, construct the current block *)
          let block_idx, block_blocks, block_virtual_empty_blocks = mk_data_node instrs in
          (* Then construct the brif block *)
          let brif_block = mk_control_block instr in
          (* Finally, construct the rest of the CFG *)
          let (blocks, edges, breaks, returns, virtual_empty_blocks, entry', exit') = helper depth [] rest in
          (
            (* add the new blocks *)
            List.rev_append block_blocks (brif_block :: blocks),
            (* add an edge between this block, brif (false branch), and finally the rest *)
            (block_idx, brif_block.idx, None) :: (brif_block.idx, entry', Some false) :: edges,
            (* add a break for the t branch *)
            add_break_target breaks
              ~target_depth:(break_target_depth depth level)
              ~block_idx:brif_block.idx
              ~data:(Some true),
            (* no return *)
            returns,
            List.rev_append block_virtual_empty_blocks virtual_empty_blocks,
            block_idx, exit')
        |  If (_bt, _arity, instrs1, instrs2) ->
          (* Construct the current block *)
          let block_idx, block_blocks, block_virtual_empty_blocks = mk_data_node instrs in
          (* Construct the if block *)
          let if_block = mk_control_block instr in
          (* Visit the then and else branches *)
          let construct_depth = depth + 1 in
          let (blocks, edges, breaks, returns, virtual_empty_blocks, then_entry, then_exit) = helper construct_depth [] instrs1 in
          let (blocks', edges', breaks', returns', virtual_empty_blocks', else_entry, else_exit) = helper construct_depth [] instrs2 in
          (* Construct the merge block *)
          let merge_block = mk_merge_block () in
          (* Construct the rest of the CFG *)
          let (blocks'', edges'', breaks'', returns'', virtual_empty_blocks'', entry, exit') = helper depth [] rest in
          (* Compute the new break levels (just like for Block and Loop)
             All breaks with level 0 break the current block
             All other breaks see their level decreased by one *)
          let all_breaks = merge_break_targets breaks breaks' in
          let current_breaks, outer_breaks = pop_break_targets all_breaks construct_depth in
          let new_breaks = merge_break_targets outer_breaks breaks'' in
          let break_edges =
            List.map current_breaks ~f:(fun (idx, branch) -> (idx, merge_block.idx, branch))
          in
          (List.rev_append block_blocks (if_block :: merge_block :: concat3 blocks blocks' blocks''),
           (* Edges *)
           (block_idx, if_block.idx, None) :: (if_block.idx, then_entry, Some true) :: (if_block.idx, else_entry, Some false) ::
           (then_exit, merge_block.idx, None) :: (else_exit, merge_block.idx, None) :: (merge_block.idx, entry, None) :: concat3 break_edges edges (List.rev_append edges' edges''),
           (* no breaks and returns *)
           new_breaks,
           concat3 returns returns' returns'',
           concat3 block_virtual_empty_blocks virtual_empty_blocks (List.rev_append virtual_empty_blocks' virtual_empty_blocks''),
           block_idx, exit')
        | Br level ->
          (* Similar to break, but because it is inconditional, there is no edge
             from this block to the next. In practice, rest should always be
             empty here *)
          check_no_rest rest;
          let block_idx, block_blocks, block_virtual_empty_blocks = mk_data_node instrs in
          let br_block = mk_control_block instr in
          let (blocks, edges, breaks, returns, virtual_empty_blocks, _entry', exit') = helper depth [] rest in
          (List.rev_append block_blocks (br_block :: blocks) (* add the current block *),
           (block_idx, br_block.idx, None) :: edges (* only sequential edge *),
           add_break_target breaks
             ~target_depth:(break_target_depth depth level)
             ~block_idx:br_block.idx
             ~data:None (* add the break *),
           returns (* no return *),
           List.rev_append block_virtual_empty_blocks virtual_empty_blocks,
           block_idx, exit')
        | BrTable (table, level) ->
          (* Similar to break, but there are multiple outgoing edges here *)
          check_no_rest rest;
          let block_idx, block_blocks, block_virtual_empty_blocks = mk_data_node instrs in
          let br_block = mk_control_block instr in
          let (blocks, edges, breaks, returns, virtual_empty_blocks, _entry', exit') = helper depth [] rest in
          let breaks =
            List.fold table
              ~init:(add_break_target breaks
                       ~target_depth:(break_target_depth depth level)
                       ~block_idx:br_block.idx
                       ~data:None)
              ~f:(fun breaks lvl ->
                  add_break_target breaks
                    ~target_depth:(break_target_depth depth lvl)
                    ~block_idx:br_block.idx
                    ~data:None)
          in
          (List.rev_append block_blocks (br_block :: blocks) (* add the current block *),
           (block_idx, br_block.idx, None) :: edges (* only sequential edge *),
           breaks,
           returns,
           List.rev_append block_virtual_empty_blocks virtual_empty_blocks,
           block_idx, exit')
        | ((Block (_bt, _arity, instrs')) as b)
        | ((Loop (_bt, _arity, instrs')) as b) ->
          (* Create a new block with all instructions collected, without the current instruction *)
          let block_idx, block_blocks, block_virtual_empty_blocks = mk_data_node instrs in
          (* Is the current instruction a loop? *)
          let is_loop = match b with
            | Loop _ -> true
            | _ -> false in
          (* The block entry *)
          let block_entry_idx, block_entry_blocks, block_entry_virtual =
            if is_loop then
              let block_entry = mk_merge_block () in
              block_entry.idx, [block_entry], []
            else
              let block_entry_idx = new_virtual_empty_block_idx () in
              block_entry_idx, [], [block_entry_idx]
          in
          begin if is_loop then
              loop_heads := IntSet.add !loop_heads block_entry_idx
          end;
          (* Recurse inside the block *)
          let construct_depth = depth + 1 in
          let (blocks, edges, breaks, returns, virtual_empty_blocks, entry', exit') = helper construct_depth [] instrs' in
          (* Create a node for the exit of the block *)
          let block_exit_idx, block_exit_blocks, block_exit_virtual =
            if is_loop then
              let block_exit_idx = new_virtual_empty_block_idx () in
              block_exit_idx, [], [block_exit_idx]
            else
              let block_exit = mk_merge_block () in
              block_exit.idx, [block_exit], []
          in
          (* Recurse after the block *)
          let (blocks', edges', breaks', returns', virtual_empty_blocks', entry'', exit'') = helper depth [] rest in
          (* Compute the new break levels:
             All breaks with level 0 break the current block
             All other breaks see their level decreased by one.
             Important: break are handled differently within loop: a break goes back to the loop entry*)
          let current_breaks, inner_breaks = pop_break_targets breaks construct_depth in
          let new_breaks = merge_break_targets breaks' inner_breaks in
          let break_edges =
            List.map current_breaks
              ~f:(fun (idx, branch) -> (idx, (if is_loop then block_entry_idx else block_exit_idx), branch))
          in
          (* Compute the new edges. This is different between a loop and a
             block, for the exit of the inside of the block *)
          let new_edges = if is_loop then
              [(block_idx, block_entry_idx, None); (block_entry_idx, entry', None); (exit', block_exit_idx, None); (block_exit_idx, entry'', None)]
            else
              [(block_idx, block_entry_idx, None); (block_entry_idx, entry', None); (exit', block_exit_idx, None); (block_exit_idx, entry'', None)]
          in
          let structural_blocks = List.rev_append block_entry_blocks block_exit_blocks in
          let structural_virtual_empty_blocks =
            List.rev_append block_entry_virtual block_exit_virtual
          in
          (List.rev_append block_blocks (List.rev_append structural_blocks (List.rev_append blocks blocks')) (* add all blocks *),
           concat3 new_edges break_edges (List.rev_append edges edges') (* add edges *),
           new_breaks (* filtered breaks *),
           List.rev_append returns returns' (* returns are propagated as is *),
           concat3 block_virtual_empty_blocks structural_virtual_empty_blocks (List.rev_append virtual_empty_blocks virtual_empty_blocks'),
           block_idx, exit'')
        | Return ->
          (* Return block. The rest of the instructions does not matter (it
             should be empty) *)
          check_no_rest rest;
          (* We create a new block with all instructions collected *)
          let block_idx, block_blocks, block_virtual_empty_blocks = mk_data_node instrs in
          (* We create a control block for this return *)
          let return_block = mk_control_block instr in
          (return_block :: block_blocks,
           (* The previous block is connected to the return *)
           [(block_idx, return_block.idx, None)],
           (* No breaks *)
           empty_break_targets,
           (* The return block is marked as returning *)
           [return_block.idx],
           block_virtual_empty_blocks,
           (* The entry block *)
           block_idx,
           (* The exit block (it should not matter here) *)
           return_block.idx)
        | Unreachable ->
          (* Simply construct a block containig the unreachable instruction *)
          let block_idx, block_blocks, block_virtual_empty_blocks = mk_data_node instrs in
          let unreachable_block = mk_control_block instr in
          let (blocks, edges, breaks, returns, virtual_empty_blocks, _entry', exit') = helper depth [] rest in
          (List.rev_append block_blocks (unreachable_block :: blocks),
           (block_idx, unreachable_block.idx, None) :: edges (* The unreachable block is not connected to anything except to the "return" block of the CFG *),
           breaks, unreachable_block.idx :: returns, List.rev_append block_virtual_empty_blocks virtual_empty_blocks, block_idx, exit')

      end
  in
  let (blocks, edges, breaks, returns, virtual_empty_blocks, _entry_idx, exit_idx) =
    time_build_phase stats
      (fun stats elapsed -> stats.raw_cfg <- stats.raw_cfg +. elapsed)
      (fun () -> helper 0 [] funcinst.code.body)
  in
  (* Create the return block *)
  let return_block = mk_merge_block () in
  let blocks' = return_block :: blocks in
  (* There can still be breaks to the exit of the function *)
  let edges' =
    time_build_phase stats
      (fun stats elapsed -> stats.return_edges <- stats.return_edges +. elapsed)
      (fun () ->
         let breaks_to_exit, remaining_breaks = pop_break_targets breaks 0 in
         begin if not (IntMap.is_empty remaining_breaks) then
             (* there shouldn't be any breaks outside the function *)
             Log.warn (fun () -> Printf.sprintf "There are %d breaks outside of function %ld" (count_break_targets remaining_breaks) fidx)
         end;
         let return_blocks = IntSet.of_list returns in
         (* Connect the return block and the remaining breaks to it, and remove all edges that start from a return block (as they are unreachable) *)
         (exit_idx, return_block.idx, None) ::
         List.map breaks_to_exit ~f:(fun (block_idx, data) -> (block_idx, return_block.idx, data)) @
         List.map returns ~f:(fun from -> (from, return_block.idx, None)) @ (List.filter edges ~f:(fun (src, _, _) -> not (IntSet.mem return_blocks src))))
  in
  (* We now filter empty normal blocks *)
  let (actual_blocks, filtered_blocks) =
    time_build_phase stats
      (fun stats elapsed -> stats.filter_empty_blocks <- stats.filter_empty_blocks +. elapsed)
      (fun () ->
         List.partition_tf blocks' ~f:(fun block -> match block.content with
             | Data [] ->
               (* only filter if we need to simplify, and it is an empty data block which is not a block/loop entry or exit *)
               if simplify then
                 false
               else
                 true
             | _ -> true))
  in
  let filtered_blocks_idx = List.rev_append virtual_empty_blocks (List.map filtered_blocks ~f:(fun b -> b.idx)) in
  (* And we have to redirect the edges: if there is an edge to a removed block, we make it point to its successors *)
  let actual_edges =
    time_build_phase stats
      (fun stats elapsed -> stats.redirect_empty_edges <- stats.redirect_empty_edges +. elapsed)
      (fun () -> redirect_edges_around_empty_blocks filtered_blocks_idx edges')
  in

  (* Create the entry block if needed *)
  let entry, actual_blocks', actual_edges' =
    time_build_phase stats
      (fun stats elapsed -> stats.entry_block <- stats.entry_block +. elapsed)
      (fun () ->
         let first_block = Option.value_exn (List.min_elt (List.map actual_blocks ~f:(fun b -> b.idx)) ~compare:Stdlib.compare) in
         (* In general, the first block is the entry block. But in some cases, it could be a block with back edges, and we want to avoid that. So we check if there's an edge to the entry block: if there is one, we need an extra entry block *)
         match List.find actual_edges ~f:(fun (_, idx, _) -> idx = first_block) with
         | None -> first_block, actual_blocks, actual_edges
         | Some _ ->
           let block = mk_empty_block () in
           block.idx, block :: actual_blocks, (block.idx, first_block, None) :: actual_edges)
  in

  let edges, back_edges =
    time_build_phase stats
      (fun stats elapsed -> stats.edge_maps <- stats.edge_maps +. elapsed)
      (fun () ->
         let edges, back_edges =
           List.fold actual_edges' ~init:(IntMap.empty, IntMap.empty)
             ~f:(fun (edges, back_edges) (src, dst, data) ->
                 (Cfg.Edges.add edges src (dst, data),
                  Cfg.Edges.add back_edges dst (src, data)))
         in
         edges, back_edges)
  in
  let basic_blocks =
    time_build_phase stats
      (fun stats elapsed -> stats.block_map <- stats.block_map +. elapsed)
      (fun () -> IntMap.of_alist_exn (List.map actual_blocks' ~f:(fun b -> (b.idx, b))))
  in

  let metadata =
    time_build_phase stats
      (fun stats elapsed -> stats.metadata <- stats.metadata +. elapsed)
      (fun () -> compute_func_metadata funcinst.code.body (IntMap.data basic_blocks))
  in
  Cfg.{
    (* Exported functions have names, non-exported don't *)
    exported = Option.is_some funcinst.name;
    (* The name itself *)
    name = Option.value funcinst.name ~default:"<unexported>";
    (* The index of this block is the integer that represent the address of this function *)
    idx = fidx;
    (* The type index of this function *)
    type_idx = funcinst.type_idx;
    (* Global types *)
    global_types = Wasm_module.get_global_types module_;
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
    entry_block = entry;
    (* The exit block is the return block *)
    exit_block = return_block.idx;
    (* The loop heads *)
    loop_heads = !loop_heads;
    (* The instruction labels in the right order *)
    instructions = metadata.instructions;
    (* A mapping from instruction labels to instructions *)
    label_to_instr = metadata.label_to_instr;
    (* Arity of each block *)
    block_arities = metadata.block_arities;
    (* Mapping from label to enclosing block *)
    label_to_enclosing_block = metadata.label_to_enclosing_block;
    label_to_enclosing_block_id = metadata.label_to_enclosing_block_id;
  }

let build_with_stats (module_ : Wasm_module.t) (fidx : Int32.t) : unit Cfg.t * build_stats =
  let stats = empty_build_stats () in
  let cfg = build ~stats module_ fidx in
  cfg, stats

let build_imported (module_ : Wasm_module.t) (desc : Wasm_module.func_desc) : unit Cfg.t =
  (* An imported function only contains one dummy basic block and a final merge block *)
  let imported_block = Basic_block.{
      idx = 0;
      fidx = desc.idx;
      content = Imported desc;
      annotation_before = ();
      annotation_after = ();
    } in
  let last_block = Basic_block.{
      idx = 1;
      fidx = desc.idx;
      content = Control {
        instr = Merge;
        label = { section = MergeInFunction desc.idx; id = 1 };
        line_number = -1;
        annotation_before = ();
        annotation_after = ();
      };
      annotation_before = ();
      annotation_after = ();
    } in
  Cfg.{
    exported = false;
    name = desc.name;
    idx = desc.idx;
    local_types = [];
    type_idx = desc.type_idx;
    global_types = Wasm_module.get_global_types module_;
    arg_types = desc.arguments;
    return_types = desc.returns;
    basic_blocks = IntMap.of_alist_exn [
        (imported_block.idx, imported_block);
        (last_block.idx, last_block);
      ];
    edges = IntMap.of_alist_exn [(imported_block.idx, Edge.Set.singleton (last_block.idx, None))];
    back_edges = IntMap.of_alist_exn [(last_block.idx, Edge.Set.singleton (imported_block.idx, None))];
    entry_block = imported_block.idx;
    exit_block = last_block.idx;
    loop_heads = IntSet.empty;
    instructions = [];
    label_to_instr = Instr.Label.Map.empty;
    block_arities = Instr.Label.Map.empty;
    label_to_enclosing_block = Instr.Label.Map.empty;
    label_to_enclosing_block_id = Instr.Label.Map.empty;
  }

let build_all (mod_ : Wasm_module.t) : unit Cfg.t Int32Map.t =
  Wasm_module.fold_defined_funcs mod_
    ~init:(Wasm_module.fold_imported_functions mod_ ~init:Int32Map.empty
             ~f:(fun cfgs desc ->
                 Int32Map.set cfgs ~key:desc.idx ~data:(build_imported mod_ desc)))
    ~f:(fun cfgs f ->
        Int32Map.set cfgs ~key:f.idx ~data:(build mod_ f.idx))

module Test = struct
  (** Check that building the CFG for each function of a .wat file succeeds.
      Does not actually check that the CFG is correct. *)
  let test_cfgs file =
    let wasm_mod = Wasm_module.of_file file in
    Wasm_module.iter_defined_funcs wasm_mod
      ~f:(fun f ->
          let _ : unit Cfg.t = build wasm_mod f.idx in
          ())

  let%test_unit "CFG for simple.wat can be built" = test_cfgs "../../../test/simple.wat"
  let%test_unit "CFG for if-loop.wat can be built" = test_cfgs "../../../test/if-loop.wat"
  let%test_unit "CFG for rel.wat can be built"  = test_cfgs "../../../test/rel.wat"
  let%test_unit "CFG for memcpy.wat can be built" = test_cfgs "../../../test/memcpy.wat"
  let%test_unit "CFG for loop.wat can be built" = test_cfgs "../../../test/loop.wat"
  let%test_unit "CFG for loop-brif.wat can be built" = test_cfgs "../../../test/loop-brif.wat"
end
