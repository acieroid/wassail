open Core
open Helpers

module Edge = Call_graph.Edge
module EdgeSet = Call_graph.EdgeSet

(** An interprocedural CFG is simply represented as a set of CFGs. The only
   difference is in the way the successor of a call is extracted. *)
type 'a t = {
  (** The CFGs, indexed by the function index. *)
  cfgs: 'a Cfg.t Int32Map.t;
  (** The call graph, represented as a map from instruction label to function index *)
  calls : EdgeSet.t Instr.Label.Map.t;
}

(* Inspired by Call_graph's constructor. Some differences:
  - The nodes are either call instructions, or function entries
  - TODO: what about calls to external function? Have a stub? *)
let make_calls (wasm_mod : Wasm_module.t) : EdgeSet.t Instr.Label.Map.t =
  let find_targets = Call_graph.indirect_call_targets in
  let calls : EdgeSet.t Instr.Label.Map.t ref = ref Instr.Label.Map.empty in
  let add_edge edge = function
    | None -> EdgeSet.singleton edge
    | Some fs -> EdgeSet.add fs edge in
  let rec collect_calls (instr : 'a Instr.t) : unit = match instr with
    | Control { instr = Call (_, _, callee); _ } ->
      let edge : Edge.t = { target = callee; direct = true } in
      calls := Int32Map.update !calls (Instr.label instr) ~f:(add_edge edge)
    | Control { instr = CallIndirect (_, _, _, typ); _ } ->
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
  { cfgs; calls = make_calls wasm_mod }

module Test = struct
  let expect (module_str : string) (calls : EdgeSet.t Instr.Label.Map.t) : bool =
    let module_ = Wasm_module.of_string module_str in
    let icfg = make module_ in
    Instr.Label.Map.equal EdgeSet.equal icfg.calls calls

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
  (global (;0;) (mut i32) (i32.const 66560)))"
      (Instr.Label.Map.of_alist_exn [
          (Instr.Label.{ section = Function 0l; id = 1; },
           (EdgeSet.of_list [{ target = 1l; direct = true }]))])


end
