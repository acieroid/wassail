open Core
open Helpers

module ICFG (* : Cfg_base.CFG_LIKE *)= struct

  module Edge = Call_graph.Edge

  module BlockIdx = struct
    module T = struct
      type t = Int32.t * Cfg.BlockIdx.t
      [@@deriving sexp, compare, equal]
      let to_string ((f, idx) : t) : string =
        Printf.sprintf "%ld_%s" f (Cfg.BlockIdx.to_string idx)
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
    (** The call graph, represented as a map from instruction label to function index *)
    calls : Edge.Set.t Instr.Label.Map.t; (* TODO: from basic block *)
  }
  [@@deriving equal]

  (* TODO: a successor is the usual successor, except for:
     - call nodes: it is the entry of the called function
     - exit block of a function: it is a ficticious "return" node, with the same id as the call node? *)

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
    { entry; cfgs; calls = make_calls wasm_mod }

  let find_block_exn (_icfg : 'a t) (_block_idx : BlockIdx.t) : 'a Basic_block.t =
    failwith "TODO: find_block_exn"

  let incoming_edges (_icfg : 'a t) (_block_idx : BlockIdx.t) : (BlockIdx.t * bool option) list =
    failwith "TODO: incoming_edges"

  let is_loop_head (_icfg : 'a t) (_block_idx : BlockIdx.t) : bool =
    failwith "TODO: is_loop_head"

  let successors (_icfg : 'a t) (_block_idx : BlockIdx.t) : BlockIdx.t list =
    failwith "TODO: successors"

  let predecessors (_icfg : 'a t) (_block_idx : BlockIdx.t) : BlockIdx.t list =
    failwith "TODO: predecessors"

  let entry_block (_icfg : 'a t) : BlockIdx.t =
    failwith "TODO: entry_block"

  let map_annotations (_icfg : 'a t) ~(f : 'a Instr.t -> 'b * 'b) : 'b t =
    ignore f;
    failwith "TODO: map_annotations"

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

  (* All nodes = all nodes from the CFGs *)
  (* TODO: use clusters: https://graphviz.org/Gallery/directed/cluster.html *)
  (* Each CFG is put into a cluster *)
  let clusters = List.map (Int32Map.to_alist icfg.cfgs) ~f:(fun (fidx, cfg) ->
      let prefix = Int32.to_string fidx in
      (* The nodes are the same than the CFG ones, but we introduce extra nodes for returns *)
      let nodes = String.concat ~sep:"\n" (List.concat_map (IntMap.to_alist cfg.basic_blocks)
                                             ~f:(fun (_, b) ->
                                                 (Basic_block.to_dot ~prefix ~annot_str b) ::
                                                 (if Basic_block.is_call b then
                                                   [Printf.sprintf "block%ld%dreturn [shape=Mrecord, label=\"{Return %ld%d}\"];" fidx b.idx fidx b.idx]
                                                 else
                                                   []))) in
      (* The edges of the CFGs, with the edges from call dashed (TODO) *)
      let edges = String.concat ~sep:"\n" (List.concat_map (IntMap.to_alist cfg.edges) ~f:(fun (src, dsts) ->
          if Basic_block.is_call (IntMap.find_exn cfg.basic_blocks src) then
            (* The call node will be connected to the entry node in the called
               function, and the exit node of the called function will be
               connected to the return node. We just have to connect the return
               node to the next block *)
            List.map (Edge.Set.to_list dsts) ~f:(fun (dst, _) -> Printf.sprintf "block%ld%dreturn -> block%ld%d;\n" fidx src fidx dst)
          else
            List.map (Edge.Set.to_list dsts) ~f:(fun (dst, br) -> Printf.sprintf "block%ld%d -> block%ld%d [label=\"%s\"];\n" fidx src fidx dst (match br with
                | Some true -> "t"
                | Some false -> "f"
                | None -> "")))) in
      Printf.sprintf "%s\n%s\n" nodes edges) in
  (* The edges between calls and returns *)
  let inter_edges = String.concat ~sep:"\n" (List.concat_map (Int32Map.to_alist icfg.cfgs) ~f:(fun (fidx, cfg) ->
      List.concat_map (IntMap.to_alist cfg.basic_blocks) ~f:(fun (_, b) ->
          if Basic_block.is_call b then
            List.concat_map (find_entry_exit b) ~f:(fun (target_fidx, entry_block, exit_block) ->
            [
              Printf.sprintf "block%ld%d -> block%ld%d" fidx b.idx target_fidx entry_block;
              Printf.sprintf "block%ld%d -> block%ld%dreturn" target_fidx exit_block fidx b.idx;
            ])
          else
            []))) in
  Printf.sprintf "digraph ICFG {\n%s\n%s\n%s\n}\n" (String.concat ~sep:"\n" clusters) inter_edges extra_data

module Test = struct
  let expect (module_str : string) (calls : Edge.Set.t Instr.Label.Map.t) : bool =
    let module_ = Wasm_module.of_string module_str in
    let icfg = make module_ in
    Printf.printf "------\n%s------\n" (to_dot icfg);
    Instr.Label.Map.equal Edge.Set.equal icfg.calls calls

  let%test "ICFG for module with two functions and one direct call" =
    expect "(module
  (type (;0;) (func (param i32) (result i32)))
  (func (;0;) (type 0) (param i32) (result i32)
    i32.const 0
    call 1)
  (func (;1;) (type 0) (param i32) (result i32)
     local.get 0)
  (table (;0;) 1 1 funcref)
  (memory (;0;) 2)
  (global (;0;) (mut i32) (i32.const 66560))
  (start 0))"
      (Instr.Label.Map.of_alist_exn [
          (Instr.Label.{ section = Function 0l; id = 1; },
           (Edge.Set.of_list [{ target = 1l; direct = true }]))])


end
