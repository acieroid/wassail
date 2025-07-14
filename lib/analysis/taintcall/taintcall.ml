open Core
open Helpers

module Domain = Taintcall_domain
module Transfer = Taintcall_transfer
module Summary = Transfer.Summary
module ClassicalInter = Intra.MakeClassicalInter(Transfer)
module Intra = Intra.MakeSummaryBased(Transfer)
module Inter = Inter.MakeSummaryBased(Transfer)(Intra)

let initial_summaries (cfgs : 'a Cfg.t Int32Map.t)  (module_ : Wasm_module.t) : Summary.t Int32Map.t =
  let taint_summaries = Taint_summary.initial_summaries cfgs module_ `Bottom in
  Int32Map.map taint_summaries ~f:(fun sum -> (Taintcall_domain.Call.bottom, sum))

let analyze_intra : Wasm_module.t -> Int32.t list -> (Summary.t * Domain.t Cfg.t option) Int32Map.t =
  Analysis_helpers.mk_intra
    (fun cfgs module_ ->
       (Int32Map.map ~f:(fun x -> (x, None)) (initial_summaries cfgs module_)))
    (fun data module_ cfg ->
       Log.info
         (Printf.sprintf "---------- Taint analysis of function %s ----------" (Int32.to_string cfg.idx));
       (* Run the taint analysis *)
       let annotated_cfg = (* Relational.Transfer.dummy_annotate *) cfg in
       let summaries = Int32Map.map data ~f:fst in
       let result_cfg = Intra.analyze module_ annotated_cfg summaries in
       let summary = Transfer.extract_summary module_ annotated_cfg result_cfg in
       (summary, Some result_cfg))

let annotate (wasm_mod : Wasm_module.t) (summaries : Summary.t Int32Map.t) (spec_cfg : Spec_domain.t Cfg.t) : Domain.t Cfg.t =
  let rel_cfg = (* Relational.Transfer.dummy_annotate *) spec_cfg in
  Intra.analyze wasm_mod rel_cfg summaries

let analyze_inter : Wasm_module.t -> Int32.t list list -> (Spec_domain.t Cfg.t * Domain.t Cfg.t * Summary.t) Int32Map.t =
  Analysis_helpers.mk_inter
    (fun _cfgs _wasm_mod -> Int32Map.empty)
    (fun module_ ~cfgs:scc ~summaries:cfgs_and_summaries ->
       Log.info
         (Printf.sprintf "---------- CallTaint analysis of SCC {%s} ----------"
            (String.concat ~sep:"," (List.map (Int32Map.keys scc) ~f:Int32.to_string)));
       (* Run the taint analysis *)
       let annotated_scc = scc (* Int32Map.map scc ~f:Relational.Transfer.dummy_annotate *) in
       let summaries = Int32Map.mapi cfgs_and_summaries ~f:(fun ~key:_idx ~data:(_spec_cfg, _taint_cfg, summary) -> summary) in
       let results = Inter.analyze module_ ~cfgs:annotated_scc ~summaries:summaries in
       Int32Map.mapi results ~f:(fun ~key:idx ~data:(taint_cfg, summary) ->
           let spec_cfg = Int32Map.find_exn scc idx in
           Printf.printf "%ld call summary: %s\n" idx (Domain.Call.to_string (fst summary));
           (spec_cfg, taint_cfg, summary)))

(** Detects calls to sinks with data coming from the exported functions' arguments.
    Sinks are declared as a set of function indices. *)
let detect_unsafe_calls_from_exported_to_sinks (module_ : Wasm_module.t) (sinks : Int32Set.t) : (int32 * int32 * Taint_domain.Taint.t) list =
  let cg = Call_graph.make module_ in
  let schedule = Call_graph.analysis_schedule cg module_.nfuncimports in
  let results = analyze_inter module_ schedule in
  Log.warn "Analyzing unsafe flows in indirect calls is not yet implemented";
  let found = ref [] in
  Int32Map.iteri results ~f:(fun ~key:fidx ~data:(_spec_cfg, _taint_cfg, summary) ->
      if Wasm_module.is_exported module_ fidx then
        let taintcall_summary = fst summary in
        Int32Map.iteri taintcall_summary ~f:(fun ~key:target ~data:taint ->
            if Int32Set.mem sinks target then begin
              List.iter taint ~f:(fun t ->
                  let unsafe = match t with
                    | TopTaint -> true
                    | Taints taints -> Option.is_some (Var.Set.find taints ~f:(function
                        | Local _ -> true
                        | _ -> false)) in
                  if unsafe then begin
                    Log.info (Printf.sprintf "Function %ld is eventually calling sink %ld with the following taint: %s" fidx target (Taint_domain.Taint.to_string t));
                      found := (fidx, target, t) :: !found
                  end)
            end));
  !found

module TaintFlow = struct
  type t = int32 * int32 * Taint_domain.Taint.t
  [@@deriving sexp, compare, equal]
end

module TaintFlowSet = struct
  include Set
  include Set.Make(TaintFlow)
end

let detect_flows_from_sources_to_sinks (module_ : Wasm_module.t) (sources : StringSet.t) (sinks : StringSet.t) : TaintFlowSet.t =
  let cg = Call_graph.make module_ in
  let schedule = Call_graph.analysis_schedule cg module_.nfuncimports in
  let funcnames = Wasm_module.get_funcnames module_ in
  let _sources_specifications = Int32Map.of_alist_exn (List.filter_map (StringSet.to_list sources) ~f:(fun source_name ->
      match StringMap.find funcnames source_name with
      | Some source_idx -> Some (source_idx, Var.Other source_name)
      | None ->
        Log.warn (Printf.sprintf "source does not exist in binary (this could be perfectly fine): %s" source_name);
        None (* This source does not exist in the binary, so we ignore it *)
    )) in
  let sink_indices_to_names = Int32Map.of_alist_exn (List.filter_map (StringSet.to_list sinks) ~f:(fun sink_name ->
      match StringMap.find funcnames sink_name with
      | Some sink_idx -> Some (sink_idx, sink_name)
      | None ->
        Log.warn (Printf.sprintf "sink does not exist in binary (this could be perfectly fine): %s" sink_name);
        None (* This sink does not exist in the binary, so we ignore it *)
    )) in
  Log.warn "TODO: source specification";
  (* Taintcall_transfer.TaintTransfer.return_taint_specifications := sources_specifications; *)
  let results = analyze_inter module_ schedule in
  let found = ref TaintFlowSet.empty in
  Int32Map.iteri results ~f:(fun ~key:fidx ~data:(_spec_cfg, _taint_cfg, summary) ->
      let taintcall_summary = fst summary in
      Int32Map.iteri taintcall_summary ~f:(fun ~key:target ~data:taint ->
          if Int32Map.mem sink_indices_to_names target then begin
            List.iter taint ~f:(fun t ->
                let unsafe = match t with
                  | TopTaint -> true
                  | Taints taints -> Option.is_some (Var.Set.find taints ~f:(function
                      | Other _ -> true
                      | _ -> false)) in
                if unsafe then found := TaintFlowSet.add !found (fidx, target, t))
          end));
  List.iter (TaintFlowSet.to_list !found) ~f:(fun (fidx, target, taint) ->
      Log.info (Printf.sprintf "Function %ld is eventually calling sink %s with the following taint: %s" fidx (Int32Map.find_exn sink_indices_to_names target) (Taint_domain.Taint.to_string taint)));
  !found

let detect_flows_from_exported_to_imported (module_ : Wasm_module.t) : TaintFlowSet.t =
  let imports = StringSet.of_list (List.filter_map module_.imports ~f:(function
      | { item_name; idesc = FuncImport _; _ } -> Some item_name
      | _ -> None)) in
  let exports = StringSet.of_list (List.filter_map module_.exports ~f:(function
      | { name; edesc = Export.FuncExport _ } -> Some name
      | _ -> None)) in
  detect_flows_from_sources_to_sinks module_ exports imports

module Test = struct

  let check_equal =
    List.equal (fun (caller, callee, taint) (expected_caller, expected_callee, expected_taint) ->
        Int32.equal caller expected_caller &&
        Int32.equal callee expected_callee &&
        Taint_domain.Taint.equal taint expected_taint)

  let%test "call from exported to sink" =
    let module_ = Wasm_module.of_string "(module
  (type (;0;) (func (param i32 i32) (result i32)))
  (type (;1;) (func (param i32) (result i32)))
  (func (;0;) (type 1) (param i32) (result i32)
    local.get 0
    ;; Calls 1 with l0
    ;; 2 calls 4 (strcpy) with its own l0
    call 1)
  (func (;1;) (type 1) (param i32) (result i32)
    (local i32)
    global.get 0 ;; [g0]
    i32.const 48 ;; [48, g0]
    i32.sub ;; [g0-48]
    local.tee 1 ;; l1 = g0-48
    global.set 0 ;; g0' = g0-48 --> allocated 48 bytes of stack memory
    local.get 1 ;; [g0-48]
    local.get 0 ;; [l0, g0-48]
    call 2 ;; call strcpy with argument (tainted) and g0-48 (stack memory)
    drop
    local.get 1
    i32.const 48
    i32.add
    global.set 0
    i32.const 0)
  (func (;2;) (type 0) (param i32 i32) (result i32)
    ;; This is strcpy (but we remove its implementation for simplicity)
    local.get 0)
  (table (;0;) 1 1 funcref)
  (memory (;0;) 2)
  (global (;0;) (mut i32) (i32.const 66560))
  (export \"memory\" (memory 0))
  (export \"submit\" (func 1))
  (export \"strcpy\" (func 2)))" in
    (* Function 0 is not exported so it should not be detected. Function 1 is,
       and calls strcpy, so it should be detected. *)
    let actual = detect_unsafe_calls_from_exported_to_sinks module_ (Int32Set.singleton 2l) in
    let expected = [(1l, 2l, Taint_domain.Taint.Taints (Var.Set.singleton (Var.Local 0)))] in
    check_equal actual expected

  let%test "call from exported to sink through another function" =
    let module_ = Wasm_module.of_string "(module
  (type (;0;) (func (param i32 i32) (result i32)))
  (type (;1;) (func (param i32) (result i32)))
  (func (;0;) (type 0) (param i32 i32) (result i32)
    local.get 1
    ;; Calls 1 with l1
    ;; 2 calls 4 (strcpy) with its own l0
    call 1)
  (func (;1;) (type 1) (param i32) (result i32)
    (local i32)
    global.get 0 ;; [g0]
    i32.const 48 ;; [48, g0]
    i32.sub ;; [g0-48]
    local.tee 1 ;; l1 = g0-48
    global.set 0 ;; g0' = g0-48 --> allocated 48 bytes of stack memory
    local.get 1 ;; [g0-48]
    local.get 0 ;; [l0, g0-48]
    call 2 ;; call strcpy with argument (tainted) and g0-48 (stack memory)
    drop
    local.get 1
    i32.const 48
    i32.add
    global.set 0
    i32.const 0)
  (func (;2;) (type 0) (param i32 i32) (result i32)
    ;; This is strcpy (but we remove its implementation for simplicity)
    local.get 0)
  (table (;0;) 1 1 funcref)
  (memory (;0;) 2)
  (global (;0;) (mut i32) (i32.const 66560))
  (export \"memory\" (memory 0))
  (export \"strcpy\" (func 2))
  (export \"foo\" (func 0)))" in
    (* Function 0 is exported, calls 1, which calls a sink. It should be detected then. And it should be detected with l1. *)
    let actual = detect_unsafe_calls_from_exported_to_sinks module_ (Int32Set.singleton 2l) in
    let expected = [(0l, 2l, Taint_domain.Taint.Taints (Var.Set.singleton (Var.Local 1)))] in
    check_equal actual expected

end
