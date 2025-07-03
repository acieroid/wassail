open Core
open Helpers


module ICFG (* : Cfg_base.CFG_LIKE *)= struct

  module Edge = Call_graph.Edge
  (** A reverse call-graph edge. *)
  module ReverseEdge = struct
    module T = struct
      type t = {
        source_block : Cfg.BlockIdx.t;
        source_fidx : Int32.t;
        direct: bool;
      }
      [@@deriving sexp, compare, equal]
    end
    include T

    module Set = struct
      include Set
      include Set.Make(T)
    end
  end

  type block_kind =
    | Regular
    | Entry
    | Return
  [@@deriving sexp, compare, equal]

  module BlockIdx = struct
    module T = struct
      type t = {
        fidx: Int32.t;
        block_idx: Cfg.BlockIdx.t;
        kind: block_kind;
      }
      [@@deriving sexp, compare, equal]
      let to_string (block_idx : t) : string =
        Printf.sprintf "%ld_%s%s" block_idx.fidx (Cfg.BlockIdx.to_string block_idx.block_idx)
          (match block_idx.kind with
           | Regular -> ""
           | Entry -> "entry"
           | Return -> "return")
    end
    include T
    module Set = Set.Make(T)
    module Map = Map.Make(T)
  end

  (** An interprocedural CFG is simply represented as a set of CFGs. The only
      difference is in the way the successor of a call is extracted.*)
  type 'a t = {
    (** The entry function *)
    entry : Int32.t;
    (** The CFGs, indexed by the function index. *)
    cfgs: 'a Cfg.t Int32Map.t;
    (** The call graph, represented as a map from instruction label to function index (from callers to callees) *)
    calls : Edge.Set.t Instr.Label.Map.t;
    (** And the reverse call graph, from callees to callers *)
    reverse_calls : ReverseEdge.Set.t Int32Map.t;
  }
  [@@deriving equal]

  (* Inspired by Call_graph's constructor. Some differences:
     - The nodes are either call instructions, or function entries
     - TODO: what about calls to external function? Have a stub node? *)
  let make_calls (wasm_mod : Wasm_module.t) : Edge.Set.t Instr.Label.Map.t =
    let find_targets = Call_graph.indirect_call_targets in
    let calls : Edge.Set.t Instr.Label.Map.t ref = ref Instr.Label.Map.empty in
    let add_edge edge = function
      | None -> Edge.Set.singleton edge
      | Some fs -> Edge.Set.add fs edge in
    let rec collect_calls (instr : 'a Instr.t) : unit = match instr with
      | Call { instr = CallDirect (_, _, callee); _ } ->
        let edge : Edge.t = { target = callee; direct = true } in
        calls := Int32Map.update !calls (Instr.label instr) ~f:(add_edge edge)
      | Call { instr = CallIndirect (_, _, _, typ); _ } ->
        calls := List.fold_left (find_targets wasm_mod typ)
            ~init:!calls
            ~f:(fun calls f' ->
                let edge : Edge.t = { target = f'; direct = false } in
                Int32Map.update calls (Instr.label instr) ~f:(add_edge edge))
      | Control { instr = Block (_, _, instrs); _ }
      | Control { instr = Loop (_, _, instrs); _ } ->
        collect_calls_instrs instrs
      | Control { instr = If (_,_, instrs1, instrs2); _ } ->
        collect_calls_instrs instrs1;
        collect_calls_instrs instrs2
      | _ -> ()
    and collect_calls_instrs (instrs : 'a Instr.t list) : unit =
      List.iter instrs ~f:collect_calls in
    List.iter wasm_mod.funcs
      ~f:(fun f ->
          List.iter f.code.body
            ~f:collect_calls);
    !calls

  let make (wasm_mod : Wasm_module.t) : 'a t =
    let cfgs = Cfg_builder.build_all wasm_mod in
    let entry = Option.value_exn ~message:"ICFG.make expects a program with an entry point" wasm_mod.start in
    let calls = make_calls wasm_mod in
    let reverse_calls = Map.to_alist calls
                        |> List.concat_map ~f:(fun (source, edges) ->
                            let source_fidx = match source.section with
                              | Function n | MergeInFunction n -> n
                              | _ -> failwith "invalid instruction source" in
                            let cfg = Map.find_exn cfgs source_fidx in
                            let source_block = Map.find_exn cfg.label_to_enclosing_block_id source in
                            Edge.Set.to_list edges
                            |> List.map ~f:(fun edge -> (edge.target, ReverseEdge.{
                                source_fidx;
                                source_block;
                                direct = edge.direct
                              })))
                        |> Int32Map.of_alist_multi
                        |> Int32Map.map ~f:ReverseEdge.Set.of_list in
    { entry; cfgs; calls; reverse_calls }

  let find_block_exn (icfg : 'a t) (block_idx : BlockIdx.t) : 'a Basic_block.t =
    let cfg = Int32Map.find_exn icfg.cfgs block_idx.fidx in
    let block = Cfg.find_block_exn cfg block_idx.block_idx in
    match block_idx.kind with
    | Regular -> block
    | Entry -> begin match block.content with
        | Call instr -> { block with content = Entry instr }
        | _ -> failwith "Unexpected: got a entry block for a non-call"
      end
    | Return -> begin match block.content with
        | Call instr -> { block with content = Return instr }
        | _ -> failwith "Unexpected got a return block for a non call"
      end

  let is_loop_head (icfg : 'a t) (block_idx : BlockIdx.t) : bool =
    let cfg = Int32Map.find_exn icfg.cfgs block_idx.fidx in
    Cfg.is_loop_head cfg block_idx.block_idx

  let predecessors (icfg : 'a t) (block_idx : BlockIdx.t) : (BlockIdx.t * bool option) list =
    let make_block_idx (fidx : Int32.t) (internal_block_idx : int) (kind : block_kind) : BlockIdx.t =
      { fidx;
        block_idx = internal_block_idx;
        kind;
      } in
    let cfg = Int32Map.find_exn icfg.cfgs block_idx.fidx in
    match block_idx.kind with
    | Return ->
      (* If it is a return node, its predecessors are the exit nodes of the callee *)
      begin match Cfg.find_block_exn cfg block_idx.block_idx with
      | { content = Return call_instr; _ } ->
        let callees = Map.find_exn icfg.calls call_instr.label in
        callees |> Set.to_list
        |> List.map ~f:(fun { target = callee; _ } ->
            let callee_cfg = Map.find_exn icfg.cfgs callee in
            (* The predecessor must be a regular node (a Return cannot be preceded by another Return or an Entry, there must be an instruction *)
            (* TODO: what if there's an empty function? Not sure this is possible, as a function need to have a regular entry block *)
            make_block_idx callee_cfg.idx callee_cfg.exit_block Regular, None)
      | _ -> failwith "not a return"
      end
    | Entry ->
      (* If it is an entry node, its predecessors are the calls that map to it *)
      let callers = Map.find_exn icfg.reverse_calls block_idx.fidx in
      callers
      |> Set.to_list
      |> List.map ~f:(fun { source_fidx = caller; source_block = caller_block; _ } ->
          (* The predecessor must be a regular node. An Entry node must be preceded ba Call node *)
          make_block_idx caller caller_block Regular, None)
    | Regular ->
      (* If it is the first node of a CFG, then the predecessor is the artificial entry node *)
      if cfg.entry_block = block_idx.block_idx then
        [({ block_idx with kind = Entry }, None)]
      else
        (* Otherwise, it is the same as for a regular CFG *)
        Cfg.predecessors cfg block_idx.block_idx
        |> List.map ~f:(fun (idx, opt) -> make_block_idx block_idx.fidx idx Regular, opt)

  let successors (icfg : 'a t) (block_idx : BlockIdx.t) : BlockIdx.t list =
    let make_block_idx (fidx : Int32.t) (internal_block_idx : int) (kind : block_kind) : BlockIdx.t =
      { fidx;
        block_idx = internal_block_idx;
        kind;
        (* return = Basic_block.is_call (Cfg.find_block_exn (Int32Map.find_exn icfg.cfgs fidx) internal_block_idx) *)
      } in
    let cfg = Map.find_exn icfg.cfgs block_idx.fidx in
    let bb = Map.find_exn cfg.basic_blocks block_idx.block_idx in
    match block_idx.kind with
    | Entry ->
      (* If it is an entry node, its successor is the first node of the callee *)
      begin match bb.content with
      | Call { label; _ } ->
        let callees = Map.find_exn icfg.calls label in
        callees
        |> Set.to_list
        |> List.map ~f:(fun { target = callee; _ } ->
            let callee_cfg = Map.find_exn icfg.cfgs callee in
            make_block_idx callee_cfg.idx callee_cfg.entry_block Regular)
      | _ -> failwith "Unexpected, expected call"
      end
    | Return ->
      (* If it is a return node, its successor is the successor of the call instruction *)
      Cfg.successors cfg block_idx.block_idx
      |> List.map ~f:(fun idx -> make_block_idx block_idx.fidx idx Regular)
    | Regular ->
      if block_idx.block_idx = cfg.exit_block then
        (* If it is an exit node, its successor is the return node of the caller's call instr *)
        let callers = Map.find_exn icfg.reverse_calls cfg.idx in
        callers
        |> Set.to_list
        |> List.map ~f:(fun { source_block; source_fidx; _ } ->
            make_block_idx source_fidx source_block Regular)
      else
        (* Otherwise, it is the same as for a regular CFG *)
        Cfg.successors cfg block_idx.block_idx
        |> List.map ~f:(fun idx -> make_block_idx block_idx.fidx idx Regular)

  let entry_block (icfg : 'a t) : BlockIdx.t =
    let cfg = Int32Map.find_exn icfg.cfgs icfg.entry in
    { fidx = icfg.entry;
      block_idx = cfg.entry_block;
      kind = Entry; }

  let map_annotations (icfg : 'a t) ~(f : 'a Instr.t -> 'b * 'b) : 'b t =
    { icfg with
      cfgs = Int32Map.map icfg.cfgs ~f:(Cfg.map_annotations ~f) }

end

include ICFG

let to_dot
    ?annot_str:(annot_str : ('a -> string) = fun _ -> "")
    ?extra_data:(extra_data : string = "")
    (icfg : 'a t) : string =

  let find_entry_exit (block : 'a Basic_block.t) : (Int32.t * int * int) list =
    match block with
    | { content = Call i; _ } -> begin match Instr.Label.Map.find icfg.calls i.label with
        | None -> failwith "No call?!"
        | Some edges ->
          List.map (Edge.Set.to_list edges) ~f:(fun edge ->
              let cfg = Int32Map.find_exn icfg.cfgs edge.target in
              (edge.target, cfg.entry_block, cfg.exit_block))
      end
    | _ -> failwith "not a call?" in

  (* Each CFG is put into a cluster *)
  let clusters = List.map (Int32Map.to_alist icfg.cfgs) ~f:(fun (fidx, cfg) ->
      let prefix = Printf.sprintf "%ld_" fidx in
      (* The nodes are the same than the CFG ones, but we introduce extra nodes for returns *)
      let nodes = String.concat ~sep:"\n" (List.concat_map (IntMap.to_alist cfg.basic_blocks)
                                             ~f:(fun (_, b) ->
                                                 let color =
                                                   if b.idx = cfg.entry_block then
                                                     "green"
                                                   else if b.idx = cfg.exit_block then
                                                     "red"
                                                   else
                                                     "black" in
                                                 (Basic_block.to_dot ~prefix ~color ~annot_str b) ::
                                                 (* If this is a call node, we add an extra return edge *)
                                                 (if Basic_block.is_call b then
                                                   [Printf.sprintf "block%ld_%dreturn [shape=Mrecord, label=\"{Return %ld_%d}\"];" fidx b.idx fidx b.idx]
                                                 else
                                                   []))) in
      (* The edges of the CFGs, with the edges from call dashed *)
      let edges = String.concat ~sep:"\n" (List.concat_map (IntMap.to_alist cfg.edges) ~f:(fun (src, dsts) ->
          if Basic_block.is_call (IntMap.find_exn cfg.basic_blocks src) then
            (* The call node will be connected to the entry node in the called
               function, and the exit node of the called function will be
               connected to the return node. We just have to connect the return
               node to the next block *)
            List.concat_map (Edge.Set.to_list dsts) ~f:(fun (dst, _) ->
                [Printf.sprintf "block%ld_%dreturn -> block%ld_%d;\n" fidx src fidx dst;
                 Printf.sprintf "block%ld_%d -> block%ld_%dreturn [style=dashed];\n" fidx src fidx src])
          else
            (* This is a regular edge *)
            List.map (Edge.Set.to_list dsts) ~f:(fun (dst, br) -> Printf.sprintf "block%ld_%d -> block%ld_%d [label=\"%s\"];\n" fidx src fidx dst (match br with
                | Some true -> "t"
                | Some false -> "f"
                | None -> "")))) in
      Printf.sprintf "subgraph cluster_%ld {label=\"function %ld\";\ncolor=blue;\n%s\n%s\n}\n" fidx fidx nodes edges) in
  (* The edges between calls and returns *)
  let inter_edges = String.concat ~sep:"\n" (List.concat_map (Int32Map.to_alist icfg.cfgs) ~f:(fun (fidx, cfg) ->
      List.concat_map (IntMap.to_alist cfg.basic_blocks) ~f:(fun (_, b) ->
          if Basic_block.is_call b then
            List.concat_map (find_entry_exit b) ~f:(fun (target_fidx, entry_block, exit_block) ->
            [
              Printf.sprintf "block%ld_%d -> block%ld_%d" fidx b.idx target_fidx entry_block;
              Printf.sprintf "block%ld_%d -> block%ld_%dreturn" target_fidx exit_block fidx b.idx;
            ])
          else
            []))) in
  Printf.sprintf "digraph ICFG {\n%s\n%s\n%s\n}\n" (String.concat ~sep:"\n" clusters) inter_edges extra_data

module Test = struct
  let expect (module_str : string) (calls : Edge.Set.t Instr.Label.Map.t) : bool =
    let module_ = Wasm_module.of_string module_str in
    let icfg = make module_ in
    (* Printf.printf "------\n%s------\n" (to_dot icfg); *)
    (* Printf.printf "%s\n" (String.concat ~sep:"," (List.map ~f:Instr.Label.to_string (Instr.Label.Map.keys calls)));
       Printf.printf "%s\n" (String.concat ~sep:"," (List.map ~f:Instr.Label.to_string (Instr.Label.Map.keys icfg.calls))); *)
    Instr.Label.Map.equal Edge.Set.equal icfg.calls calls

  let%test "ICFG for module with two functions and one direct call" =
    expect "(module
  (type (;0;) (func))
  (type (;1;) (func (param i32) (result i32)))
  (func (;0;) (type 0)
    i32.const 0
    call 1
    drop)
  (func (;1;) (type 1) (param i32) (result i32)
     local.get 0)
  (table (;0;) 1 1 funcref)
  (memory (;0;) 2)
  (global (;0;) (mut i32) (i32.const 66560))
  (start 0))"
      (Instr.Label.Map.of_alist_exn [
          (Instr.Label.{ section = Function 0l; id = 1; },
           (Edge.Set.of_list [{ target = 1l; direct = true }]))])

  let%test "ICFG for word count" =
    expect "(module
  (type (;0;) (func))
  (type (;1;) (func (result i32)))
  (start 1)
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
      (Instr.Label.Map.of_alist_exn [
          (Instr.Label.{ section = Function 1l; id = 0; }, Edge.Set.of_list [{ target = 0l; direct = true }]);
          (Instr.Label.{ section = Function 1l; id = 32; }, Edge.Set.of_list [{ target = 0l; direct = true }])])

end
