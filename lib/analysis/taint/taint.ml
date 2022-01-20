open Core_kernel
open Helpers

module Options = Taint_options
module Domain = Taint_domain
module Transfer = Taint_transfer.Make
module Summary = Taint_summary
module Intra = Intra.Make(Transfer)
module Inter = Inter.Make(Intra)

let analyze_intra : Wasm_module.t -> Int32.t list -> (Summary.t * Domain.t Cfg.t option) Int32Map.t =
  Analysis_helpers.mk_intra
    (fun cfgs wasm_mod ->
       (Int32Map.map ~f:(fun x -> (x, None)) (Summary.initial_summaries cfgs wasm_mod `Bottom)))
    (fun data wasm_mod cfg ->
       Log.info
         (Printf.sprintf "---------- Taint analysis of function %s ----------" (Int32.to_string cfg.idx));
       (* Run the taint analysis *)
       Options.use_relational := false;
       let annotated_cfg = Relational.Transfer.dummy_annotate cfg in
       let summaries = Int32Map.map data ~f:fst in
       let (result_cfg, taint_summary) = Intra.analyze wasm_mod annotated_cfg summaries in
       (taint_summary, Some result_cfg))

let annotate (wasm_mod : Wasm_module.t) (summaries : Summary.t Int32Map.t) (spec_cfg : Spec.t Cfg.t) : Domain.t Cfg.t =
  let rel_cfg = Relational.Transfer.dummy_annotate spec_cfg in
  fst (Intra.analyze wasm_mod rel_cfg summaries)

let check (expected : Summary.t) (actual : Summary.t) : bool =
  if Summary.subsumes actual expected then
    if Summary.equal actual expected then
      true
    else begin
      Printf.printf "\n[IMPRECISION] summaries not equal:\nexpected: %s\nactual: %s\n" (Summary.to_string expected) (Summary.to_string actual);
      true (* not equal, but it does subsume so the test does not fail *)
    end
  else begin
    Printf.printf "\nsummaries does not subsume:\nexpected: %s\nactual: %s\n" (Summary.to_string expected) (Summary.to_string actual);
    false
  end

let analyze_inter : Wasm_module.t -> Int32.t list list -> (Spec.t Cfg.t * Taint_domain.t Cfg.t * Summary.t) Int32Map.t =
  Analysis_helpers.mk_inter
    (fun _cfgs _wasm_mod -> Int32Map.empty)
    (fun wasm_mod scc cfgs_and_summaries ->
       Log.info
         (Printf.sprintf "---------- Taint analysis of SCC {%s} ----------"
            (String.concat ~sep:"," (List.map (Int32Map.keys scc) ~f:Int32.to_string)));
       (* Run the taint analysis *)
       Options.use_relational := false;
       let annotated_scc = Int32Map.map scc ~f:Relational.Transfer.dummy_annotate in
       let summaries = Int32Map.mapi cfgs_and_summaries ~f:(fun ~key:_idx ~data:(_spec_cfg, _taint_cfg, summary) -> summary) in
       let results = Inter.analyze wasm_mod annotated_scc summaries in
       Int32Map.mapi results ~f:(fun ~key:idx ~data:(taint_cfg, summary) ->
           let spec_cfg = Int32Map.find_exn scc idx in
           (spec_cfg, taint_cfg, summary)))

(** Detects calls to sinks with data coming from the exported functions' arguments.
    Sinks are declared as a set of function indices. *)
let detect_unsafe_calls_to_sinks (module_ : Wasm_module.t) (sinks : Int32Set.t) : unit =
  let cg = Call_graph.make module_ in
  let schedule = Call_graph.analysis_schedule cg module_.nfuncimports in
  let results = analyze_inter module_ schedule in
  Log.warn "Analyzing unsafe flows in indirect calls is not yet implemented";
  (* TODO: have a way of extracting sinks set from their names *)
  (* TODO: restrict this to exported function arguments only! *)
  (* TODO: but also treat as sink anything that calls a sink with one of its argument? *)
  Int32Map.iteri results ~f:(fun ~key:fidx ~data:(spec_cfg, taint_cfg, _summary) ->
      let call_blocks = Cfg.all_direct_calls_blocks spec_cfg in
      List.iter call_blocks ~f:(fun block ->
          let spec_state_before_call = Cfg.state_before_block spec_cfg block.idx (Spec_inference.init_state spec_cfg) in
          let (arity, target) = match block.content with
            | Control { instr = Instr.Call (arity, _, target); _ } -> (arity, target)
            | _ -> failwith "unexpected" in
          if Int32Set.mem sinks target then
            let args = List.take (Spec.get_or_fail spec_state_before_call).vstack (fst arity) in
            let taint_before_call = Cfg.state_before_block taint_cfg block.idx Taint_domain.bottom in
            List.iter args ~f:(fun arg ->
                let taint = Taint_domain.get_taint taint_before_call arg in
                let unsafe = match taint with
                  | TopTaint -> true
                  | Taints taints -> Option.is_some (Var.Set.find taints ~f:(function
                      | Local _ -> true
                      | _ -> false)) in
                if unsafe then
                  Log.info (Printf.sprintf "Function %ld is calling sink %ld with arg %s and the following taint: %s" fidx target (Var.to_string arg) (Taint_domain.Taint.to_string taint)))))

module Test = struct
  let%test "simple function has no taint" =
    let module_ = Wasm_module.of_string "(module
  (type (;0;) (func (param i32) (result i32)))
  (func (;test;) (type 0) (param i32) (result i32)
    i32.const 256
    i32.const 512
    i32.const 0
    select)
  (table (;0;) 1 1 funcref)
  (memory (;0;) 2)
  (global (;0;) (mut i32) (i32.const 66560)))" in
    let actual = fst (Int32Map.find_exn (analyze_intra module_ [0l]) 0l) in
    let expected = Summary.{ ret = Some Domain.Taint.bottom; mem = Domain.Taint.bottom; globals = [Domain.Taint.taint (Var.Global 0)] } in
    check expected actual

  let%test "test store" =
    let module_ = Wasm_module.of_string "(module
  (type (;0;) (func (param i32) (result i32)))
  (func (;test;) (type 0) (param i32) (result i32)
    global.get 0
    local.get 0
    i32.store
    local.get 0)
  (table (;0;) 1 1 funcref)
  (memory (;0;) 2)
  (global (;0;) (mut i32) (i32.const 66560)))" in
    let actual = fst (Int32Map.find_exn (analyze_intra module_ [0l]) 0l) in
    let expected = Summary.{ ret = Some (Domain.Taint.taint (Var.Local 0)); mem = Domain.Taint.taint (Var.Local 0); globals = [Domain.Taint.taint (Var.Global 0)] } in
    check expected actual
end
