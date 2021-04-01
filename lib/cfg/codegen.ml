open Core_kernel

let true_edge (edges : Cfg.Edge.t list) : Cfg.Edge.t option =
  List.find edges
    ~f:(function
        | (_, Some true) -> true
        | _ -> false)

let false_edge (edges : Cfg.Edge.t list) : Cfg.Edge.t option =
  List.find edges
    ~f:(function
        | (_, Some false) -> true
        | _ -> false)

let relevant_successor (cfg : unit Cfg.t) (block : unit Basic_block.t) : int option =
  match block.content with
  | Control { instr = BrIf _; _ } ->
    (* If we are in a loop, we want to stay in the loop without going back to the entry, hence we take the false edge.
       If we are in a block, we want to stay in the block without going out of the block, hence we also take the false edge. *)
    Option.map ~f:fst (false_edge (Cfg.outgoing_edges cfg block.idx))
  | Control { instr = BrTable _; _ } -> failwith "br_table not supported"
  | _ ->
    let successors = Cfg.successors cfg block.idx in
    assert (List.length successors <= 1);
    List.hd successors

(** Returns the list of instructions generated for this block (and possibly
   other blocks that are enclosed in this block (basically, in case of
   if/loop/block) as well as the next block after the execution of this one.
   There can be multiple successor for blocks such as br_if etc., but only one
   is returned: the one that is important for generating the code in the right
   order (e.g., the one that does not perform the jump).  *)
let rec codegen_block (cfg : unit Cfg.t) (block : unit Basic_block.t) : unit Instr.t list * int option =
  let control i = Instr.Control ({ instr = i; annotation_before = (); annotation_after = (); label = { section = Dummy; id = 0 }}) in
  let data i = Instr.Data ({ instr = i; annotation_before = (); annotation_after = (); label = { section = Dummy; id = 0 }}) in
  match block.block_kind with
  | Some (LoopEntry (bt, arity)) ->
    let successors = Cfg.successors cfg block.idx in
    assert (List.length successors = 1);
    let succ = List.hd_exn successors in
    let (body, _) = codegen_until cfg (Some Basic_block.LoopExit) succ in
    let exit_block = Cfg.corresponding_exit_exn cfg block.idx in
    [control (Instr.Loop (bt, arity, body))], Some exit_block
  | Some BlockEntry (bt, arity) ->
    let successors = Cfg.successors cfg block.idx in
    assert (List.length successors = 1);
    let succ = List.hd_exn successors in
    let (body, _) = codegen_until cfg (Some Basic_block.BlockExit) succ in
    let exit_block = Cfg.corresponding_exit_exn cfg block.idx in
    [control (Block (bt, arity, body))], Some exit_block
  | Some LoopExit
  | Some BlockExit
  | Some IfExit ->
    [], relevant_successor cfg block
  | None ->
    match block.content with
    | Data instrs -> List.map instrs ~f:(fun i -> data i.instr), relevant_successor cfg block
    | Control { instr = Merge; _ } -> [], relevant_successor cfg block
    | Control { instr = Block _; _ }
    | Control { instr = Loop _; _ } -> failwith "Not expecting block or loop instructions in CFG block"
    | Control { instr = If (bt, arity, _, _); _ } ->
      let edges = Cfg.outgoing_edges cfg block.idx in
      assert (List.length edges = 2);
      begin match (true_edge edges, false_edge edges) with
      | (Some (t, _), Some (f, _)) ->
        let (t_code, exit_block_idx) = codegen_until cfg (Some IfExit) t in
        let (f_code, exit_block_idx') = codegen_until cfg (Some IfExit) f in
        assert (Option.equal (=) exit_block_idx exit_block_idx');
        [control (If (bt, arity, t_code, f_code))], exit_block_idx
      | _ -> failwith "Should not happen"
      end
    | Control { instr; _ } ->
      [control instr], relevant_successor cfg block
and codegen_until (cfg : unit Cfg.t) (until_kind : Basic_block.block_kind option) (block_idx : int) : unit Instr.t list * int option =
  let return l = List.concat (List.rev l) in
  let rec loop (instrs : unit Instr.t list list) (block_idx : int) : unit Instr.t list * int option =
    let block = Cfg.find_block_exn cfg block_idx in
    match until_kind with
    | Some _ when Option.equal Basic_block.equal_block_kind block.block_kind until_kind ->
      (* We stop generating here *)
      return instrs, relevant_successor cfg block
    | _ ->
      match codegen_block cfg block with
      | code, Some next -> loop (code :: instrs) next
      | code, None -> return (code :: instrs), None in
  loop [] block_idx

let codegen (cfg : unit Cfg.t) : unit Instr.t list =
  fst (codegen_until cfg None cfg.entry_block)

