open Core_kernel
open Helpers

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
  | Control { instr = Br _; _ } ->
    (* No relevant successor, as we want to stay in the block *)
    None
  | Control { instr = BrTable _; _ } -> failwith "br_table not supported"
  | _ ->
    let successors = Cfg.successors cfg block.idx in
    (* if (List.length successors <= 1) then Log.warn (Printf.sprintf "No successor for block %d" block.idx); *)
    List.hd successors

let label_idx : int ref = ref 0
let init_labels () = label_idx := 0
let new_label () : Instr.Label.t =
  label_idx := !label_idx + 1;
  { section = Dummy; id = !label_idx }

(** Returns the list of instructions generated for this block (and possibly
   other blocks that are enclosed in this block (basically, in case of
   if/loop/block) as well as the next block after the execution of this one.
   There can be multiple successor for blocks such as br_if etc., but only one
   is returned: the one that is important for generating the code in the right
   order (e.g., the one that does not perform the jump).  *)
let rec codegen_block (cfg : unit Cfg.t) (block : unit Basic_block.t) (visited : IntSet.t) : unit Instr.t list * int option =
  if IntSet.mem visited block.idx then
    [], None
  else
    let control i = Instr.Control ({ instr = i; annotation_before = (); annotation_after = (); label = new_label ()}) in
    let data i = Instr.Data ({ instr = i; annotation_before = (); annotation_after = (); label = new_label ()}) in
    match block.block_kind with
    | Some (LoopEntry (bt, arity)) ->
      let successors = Cfg.successors cfg block.idx in
      assert (List.length successors = 1);
      let succ = List.hd_exn successors in
      let exit_block = Cfg.corresponding_exit_exn cfg block.idx in
      let (body, _) = codegen_until cfg succ exit_block (IntSet.add visited block.idx) in
      [control (Instr.Loop (bt, arity, body))], Some exit_block
    | Some BlockEntry (bt, arity) ->
      let exit_block = Cfg.corresponding_exit_exn cfg block.idx in
      let successors = Cfg.successors cfg block.idx in
      assert (List.length successors = 1);
      let succ = List.hd_exn successors in
      let (body, _) = codegen_until cfg succ exit_block (IntSet.add visited block.idx) in
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
            let exit_block = Cfg.corresponding_exit_exn cfg block.idx in
            let (t_code, exit_block_idx) = codegen_until cfg t exit_block (IntSet.add visited block.idx) in
            let (f_code, exit_block_idx') = codegen_until cfg f exit_block (IntSet.add visited block.idx) in
            assert (Option.equal (=) exit_block_idx exit_block_idx');
            [control (If (bt, arity, t_code, f_code))], exit_block_idx
          | _ -> failwith "Should not happen"
        end
      | Control { instr; _ } ->
        [control instr], relevant_successor cfg block
and codegen_until (cfg : unit Cfg.t) (start_block_idx : int) (stop_block_idx : int) (visited : IntSet.t) : unit Instr.t list * int option =
  let return l = List.concat (List.rev l) in
  let rec loop (instrs : unit Instr.t list list) (block_idx : int) (visited : IntSet.t) : unit Instr.t list * int option =
    let block = Cfg.find_block_exn cfg block_idx in
    if block_idx = stop_block_idx then begin
      (* We stop generating here *)
      return instrs, relevant_successor cfg block
    end else
      match codegen_block cfg block visited with
      | code, Some next -> loop (code :: instrs) next visited
      | code, None ->
        return (code :: instrs), None in
  loop [] start_block_idx visited

let codegen (cfg : unit Cfg.t) : unit Instr.t list =
  fst (codegen_until cfg cfg.entry_block (-1) IntSet.empty)

let cfg_to_func_inst (cfg : unit Cfg.t) : Func_inst.t =
  let body: unit Instr.t list = codegen cfg in
    { idx = cfg.idx;
      name = Some cfg.name;
      type_idx = cfg.type_idx;
      typ = (cfg.arg_types, cfg.return_types);
      code = { locals = cfg.local_types; body }
    }

module Test = struct
  let data i : unit Instr.t = Data Instr.{ instr = i; label = new_label (); annotation_before = (); annotation_after = (); }
  let control i : unit Instr.t = Control Instr.{ instr = i; label = new_label (); annotation_before = (); annotation_after = (); }

  let%test "codegen for trivial module should generate all the code" =
    let module_ = Wasm_module.of_string "(module
   (type (;0;) (func (param i32) (result i32)))
   (func (;test;) (type 0) (param i32) (result i32)
    memory.size
    memory.size
    i32.store
    memory.size
    memory.size
    i32.store
   )
   (table (;0;) 1 1 funcref)
   (memory (;0;) 2)
   (global (;0;) (mut i32) (i32.const 66560)))" in
    let cfg = Cfg_builder.build module_ 0l in
    let instrs = codegen cfg in
    List.length instrs = 6

(* BROKEN DUE TO LABELS: we need to attach the right labels to expected
  let%test "codegen for nested blocks should generate all the code" =
    let module_ = Wasm_module.of_string "(module
   (type (;0;) (func (param i32) (result i32)))
   (func (;test;) (type 0) (param i32) (result i32)
    block
      i32.const 0
      drop
      block
        i32.const 1
        br_if 0
        i32.const 2
        br 1
      end
      i32.const 3
    end
   )
   (table (;0;) 1 1 funcref)
   (memory (;0;) 2)
   (global (;0;) (mut i32) (i32.const 66560)))" in
    let cfg = Cfg_builder.build module_ 0l in
    let actual = codegen cfg in

    init_labels ();
    let expected = [
      control (Instr.Block (None, (0, 0), [
          data (Instr.Const (Prim_value.I32 0l));
          data Instr.Drop;
          control (Instr.Block (None, (0, 0), [
              data (Instr.Const (Prim_value.I32 1l));
              control (Instr.BrIf 0l);
              data (Instr.Const (Prim_value.I32 2l));
              control (Instr.Br 1l)]));
          data (Instr.Const (Prim_value.I32 3l))]))] in
    init_labels ();
     List.equal (Instr.equal (fun () () -> true)) actual expected *)

  let%test "codegen for memory operations should produce the correct offset and alignment when printed" =
    let module_str = "(module
(type (;0;) (func (param i32) (result i32)))
(func (;0;) (type 0)
  i32.const 1812
  i32.const -1
  i32.store offset=12
  i32.const 1812
  i32.const -1
  i64.store offset=8 align=4
  i32.const 1812
  i32.const -1
  i32.store16 align=1
  i32.const 1812
  i32.load16_u align=1
)
(table (;0;) 1 1 funcref)
(memory (;0;) 2)
(global (;0;) (mut i32) (i32.const 66560))
)" in
    let module_ = Wasm_module.of_string module_str in
    let cfg = Cfg_builder.build module_ 0l in
    let module_ = Wasm_module.replace_func module_ 0l (cfg_to_func_inst cfg) in
    let printed = Wasm_module.to_string module_ in
    String.equal printed module_str

  let%test "codegen for consecutive blocks should produce the same result" =
    let module_str = "(module
(type (;0;) (func (param i32) (result i32)))
(func (;0;) (type 0)
  block
    loop
      i32.const 0
      br_if 1
      i32.const 0
      br_if 0
      block

      end
      block
        i32.const 0
        i32.const 1
        i32.add
        drop
        i32.const 0
        br_if 0
      end
    end
  end
  i32.const 0
  drop
  i32.const 0
  drop
)
(table (;0;) 1 1 funcref)
(memory (;0;) 2)
(global (;0;) (mut i32) (i32.const 66560))
)" in
    let module_ = Wasm_module.of_string module_str in
    let cfg = Cfg_builder.build module_ 0l in
    let module_ = Wasm_module.replace_func module_ 0l (cfg_to_func_inst cfg) in
    let printed = Wasm_module.to_string module_ in
    Printf.printf "GENERATED:\n%s\n" printed;
    String.equal printed module_str
end
