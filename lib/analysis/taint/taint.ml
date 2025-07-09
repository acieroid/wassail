open Core
open Helpers

module Options = Taint_options
module Domain = Taint_domain
module Transfer = Taint_transfer.Make
module Summary = Taint_summary
module ClassicalInter = Intra.MakeClassicalInter(Transfer)
module Intra = Intra.MakeSummaryBased(Transfer)
module Inter = Inter.MakeSummaryBased(Transfer)(Intra)

let analyze_intra : Wasm_module.t -> Int32.t list -> (Summary.t * Domain.t Cfg.t option) Int32Map.t =
  Analysis_helpers.mk_intra
    (fun cfgs module_ ->
       (Int32Map.map ~f:(fun x -> (x, None)) (Summary.initial_summaries cfgs module_ `Bottom)))
    (fun data module_ cfg ->
       Log.info
         (Printf.sprintf "---------- Taint analysis of function %s ----------" (Int32.to_string cfg.idx));
       (* Run the taint analysis *)
       (* Options.use_relational := false; *)
       let annotated_cfg = (* Relational.Transfer.dummy_annotate  *) cfg in
       let summaries = Int32Map.map data ~f:fst in
       let result_cfg = Intra.analyze module_ annotated_cfg summaries in
       let taint_summary = Transfer.extract_summary module_ annotated_cfg result_cfg in
       (taint_summary, Some result_cfg))

let annotate (wasm_mod : Wasm_module.t) (summaries : Summary.t Int32Map.t) (spec_cfg : Spec.t Cfg.t) : Domain.t Cfg.t =
  let rel_cfg = (* Relational.Transfer.dummy_annotate *) spec_cfg in
  Intra.analyze wasm_mod rel_cfg summaries

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
    (fun wasm_mod ~cfgs:scc ~summaries:cfgs_and_summaries ->
       Log.info
         (Printf.sprintf "---------- Taint analysis of SCC {%s} ----------"
            (String.concat ~sep:"," (List.map (Int32Map.keys scc) ~f:Int32.to_string)));
       (* Run the taint analysis *)
       (* Options.use_relational := false; *)
       let annotated_scc = scc (* Int32Map.map scc ~f:Relational.Transfer.dummy_annotate *) in
       let summaries = Int32Map.mapi cfgs_and_summaries ~f:(fun ~key:_idx ~data:(_spec_cfg, _taint_cfg, summary) -> summary) in
       let summaries' = List.fold_left wasm_mod.imported_funcs
           ~init:summaries
           ~f:(fun summaries (idx, name, (args, ret)) ->
               Int32Map.set summaries ~key:idx ~data:(Summary.of_import name wasm_mod.nglobals args ret)) in
       let results = Inter.analyze wasm_mod ~cfgs:annotated_scc ~summaries:summaries' in
       Int32Map.mapi results ~f:(fun ~key:idx ~data:(taint_cfg, summary) ->
           let spec_cfg = Int32Map.find_exn scc idx in
           (spec_cfg, taint_cfg, summary)))

(** Extracts the index of functions that are considered sinks, based on their names *)
let find_sinks_from_names (module_ : Wasm_module.t) (names : StringSet.t) : Int32Set.t =
  let funs = List.filter_map module_.funcs ~f:(fun f ->
      match Wasm_module.get_funcname module_ f.idx with
      | Some name when StringSet.mem names name -> Some f.idx
      | _ -> None) in
  Int32Set.of_list funs

module TestIntra = struct
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


let analyze_inter_classical (module_ : Wasm_module.t) (entry : Int32.t) : Domain.t Icfg.t =
  ClassicalInter.analyze module_ (Analysis_helpers.mk_inter_classical module_ entry)

module TestInter = struct
  let does_not_fail (module_str : string) (fidx : int32) : unit =
    let module_ = Wasm_module.of_string module_str in
    let icfg = analyze_inter_classical module_ fidx in
    Printf.printf "---\n%s\n---\n" (Icfg.to_dot ~annot_str:Domain.to_string icfg);
    ()

  let%test_unit "interprocedural taint does not fail with function call" =
    (* TODO: don't only check that it doesn't fail *)
    does_not_fail "(module
  (type (;0;) (func (param i32) (result i32)))
  (func (;0;) (type 0) (param i32) (result i32)
    local.get 0
    i32.const 3
    i32.mul
    call 1)
  (func (;1;) (type 0) (param i32) (result i32)
    ;; []
    local.get 0
    i32.const 0
    i32.add)
  )" 0l
end
