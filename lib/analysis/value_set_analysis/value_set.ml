open Core
open Helpers

module Options = Value_set_options
module Domain = Abstract_store_domain
(* module RIC = Reduced_interval_congruence.RIC *)
module TransferFunction = Value_set_transfer.Make
module Summary = Value_set_summary
module ClassicalInter = Intra.MakeClassicalInter(TransferFunction)
module ValueSetCallAdapter: Intra.CALL_ADAPTER
  with module Transfer = TransferFunction
  and type extra = TransferFunction.summary Int32Map.t =
struct
  module Transfer = TransferFunction
  type extra = TransferFunction.summary Int32Map.t

  let analyze_call
      (module_ : Wasm_module.t)
      (_cfg : Transfer.annot_expected Cfg.t)
      (instr : Transfer.annot_expected Instr.labelled_call)
      (state : Transfer.State.t)
      (summaries : extra)
    : Transfer.State.t =
    if !Value_set_options.print_trace then print_endline (string_of_int instr.line_number ^ ":\t" ^ Instr.call_to_string instr.instr);
    let apply_summary f arity state =
      match Map.find summaries f with
      | None ->
        if Int32.(f < module_.nfuncimports) then
          let desc = List32.nth_exn module_.imported_funcs f in
          Transfer.imported module_ desc instr.annotation_before instr.annotation_after state
        else
          (* This function depends on another function that has not been analyzed yet, so it is part of some recursive loop. It will eventually stabilize. *)
          state
      | Some summary ->
        Transfer.apply_summary module_ f arity instr state summary
    in
    match instr.instr with
    | CallDirect (arity, _, f) -> apply_summary f arity state
    | CallIndirect (_, arity, _, typ) ->
      let call_index =
        state
        |> Domain.get
            ~var:(Variable.Var (pop (Spec_domain.get_or_fail instr.annotation_before).vstack))
      in
      let targets = Call_graph.indirect_call_targets module_ typ
        |> List.filter ~f:(fun idx ->
            Value_set_abstractions.meet
              call_index
              (ValueSet (Reduced_interval_congruence.RIC.of_int32 idx))
            |> Value_set_abstractions.equal Value_set_abstractions.bottom
            |> not)
      in
      if List.is_empty targets then (Log.error "indirect call index doesn't match any function"; failwith "invalid program: indirect call index doesn't match any function");
      List.fold_left targets
        ~init:Transfer.bottom
        ~f:(fun acc idx -> Transfer.State.join (apply_summary idx arity state) acc)
end
module Intra = Intra.MakeSumm(TransferFunction)(ValueSetCallAdapter)
module Inter = Inter.MakeSummaryBased(TransferFunction)(Intra)

let analyze_intra : Wasm_module.t -> Int32.t list -> (Summary.t * Domain.t Cfg.t option) Int32Map.t =
  Analysis_helpers.mk_intra
    (fun cfgs wasm_mod ->
      (Int32Map.map ~f:(fun x -> (x, None)) (Summary.initial_summaries cfgs wasm_mod `Bottom)))
    (fun data wasm_mod cfg ->
      Log.info
        (Printf.sprintf "-------------------- Value-set analysis of function %s --------------------" (Int32.to_string cfg.idx));
      (* Run the value-set analysis *)
      let annotated_cfg = (* Relational.Transfer.dummy_annotate  *) cfg in
      let summaries = Int32Map.map data ~f:fst in
      (* let (result_cfg, value_set_summary) = Intra.analyze wasm_mod annotated_cfg summaries in *)
      let result_cfg = Intra.analyze wasm_mod annotated_cfg summaries in
      let value_set_summary = TransferFunction.extract_summary wasm_mod annotated_cfg result_cfg in
      (value_set_summary, Some result_cfg))

(* let annotate (wasm_mod : Wasm_module.t) (summaries : Summary.t Int32Map.t) (spec_cfg : Spec_domain.t Cfg.t) : Domain.t Cfg.t =
  let rel_cfg = (* Relational.Transfer.dummy_annotate *) spec_cfg in
  Intra.analyze wasm_mod rel_cfg summaries *)

let analyze_inter : Wasm_module.t -> Int32.t list list -> (Spec_domain.t Cfg.t * Abstract_store_domain.t Cfg.t * Summary.t) Int32Map.t =
  Analysis_helpers.mk_inter
    (fun _cfgs _wasm_mod -> Int32Map.empty)
    (fun wasm_mod ~cfgs:scc ~summaries:cfgs_and_summaries ->
      Log.info
        (Printf.sprintf "---------- Value-set analysis of SCC {%s} ----------"
          (String.concat ~sep:", " (List.map (Int32Map.keys scc) ~f:Int32.to_string)));
      (* Run the value-set analysis *)
      let annotated_scc = scc in
      let summaries = Int32Map.mapi cfgs_and_summaries ~f:(fun ~key:_idx ~data:(_spec_cfg, _value_set_cfg, summary) -> summary) in
      let summaries' = List.fold_left wasm_mod.imported_funcs
          ~init:summaries
          ~f:(fun summaries desc ->
              Int32Map.set summaries ~key:desc.idx ~data:(Summary.of_import desc.name wasm_mod.nglobals desc.arguments desc.returns)) in
      let _ =
        let oc = Out_channel.create ~append:true "store_types.txt" in
        Out_channel.close oc
      in
      let results = Inter.analyze wasm_mod ~cfgs:annotated_scc ~summaries:summaries' in
      Int32Map.mapi results ~f:(fun ~key:idx ~data:(value_set_cfg, summary) ->
          let spec_cfg = Int32Map.find_exn scc idx in
          (spec_cfg, value_set_cfg, summary)))
      
let analyze_inter_classical (module_ : Wasm_module.t) (entry : Int32.t) : Domain.t Icfg.t =
  ClassicalInter.analyze module_ (Analysis_helpers.mk_inter_classical module_ entry)


type pointer_analysis = 
  Domain.t Cfg.t * Spec_domain.t Instr.t Instr.Label.Map.t * Domain.t Int32Map.t

let run_pointer_analysis 
    (module_ : Wasm_module.t) 
    (cfg : unit Cfg.t) 
    (funidx : int32)
  : pointer_analysis =
  let original_use_const = !Spec_inference.use_const
  and original_prop_globals = !Spec_inference.propagate_globals
  and original_prop_locals = !Spec_inference.propagate_locals in
  Spec_inference.use_const := true;
  Spec_inference.propagate_globals := true;
  Spec_inference.propagate_locals := true;
  let cfg_spec_with_propagation = Spec_inference.Intra.analyze module_ cfg () in
  let instructions_from_pointer_cfg = Cfg.all_instructions cfg_spec_with_propagation in
  let cg = Call_graph.make module_ in
  let schedule = Call_graph.analysis_schedule cg module_.nfuncimports |> List.concat in
  let cfg_pointers_map = analyze_intra module_ schedule in
  let cfg_pointers =
    match Int32Map.find cfg_pointers_map funidx with
    | None -> failwith ("No entry for function " ^ Int32.to_string funidx)
    | Some (_summary, None) -> failwith ("Function" ^ Int32.to_string funidx ^ "has no CFG")
    | Some (_summary, Some cfg) -> cfg in
  let summaries = Int32Map.map cfg_pointers_map ~f:(fun (summary, _) -> summary) in
  Spec_inference.use_const := original_use_const;
  Spec_inference.propagate_globals := original_prop_globals;
  Spec_inference.propagate_locals := original_prop_locals;
  (cfg_pointers, instructions_from_pointer_cfg, summaries)

(* module Test = struct
  let%test "add.wat" =
    let module_ = Wasm_module.of_string
      "(module
        (memory (export \"mem\") 1)
        (global $g0 (mut i32) (i32.const 1024))

        (func $main (export \"main\") (param $l0 i32) (result i32) (local $l1 i32)
          i32.const 42
          local.set $l0
          local.get $l0
          global.get $g0
          i32.add
          return
        )
      )" in
      let actual = (Int32Map.find_exn (analyze_intra module_ [0l]) 0l) in
      match actual with
      | _, Some cfg ->
        let instruction_labels = Cfg.all_instruction_labels cfg in
        let instructions = Cfg.all_instructions cfg in
        List.fold 
        ~init:() 
        ~f:(fun _ label -> 
          print_endline ("instruction " ^ Instr.Label.to_string label
          ^ ": " ^ Instr.to_string (Option.value_exn (Instr.Label.Map.find instructions label))))
        (Set.to_list instruction_labels);
        let value_sets = Cfg.all_annots cfg in
        List.fold
        ~init:()
        ~f:(fun _ store -> 
          print_endline (Abstract_store_domain.to_string store))
        value_sets;
        true
      | _ -> false
end *)


(* module Test = struct
  let%test "simple function g0 = g1" =
    let module_ = Wasm_module.of_string 
      "(module
        (memory (export \"mem\") 1)
        (global $__stack_pointer (mut i32) (i32.const 1024))
        (global $part1Value i32 (i32.const 0))
        (global $part2Value i32 (i32.const 1))

        (func $main (export \"main\") (result i32)
          i32.const 42
          global.set $__stack_pointer
          global.get $__stack_pointer
          global.set $part1Value
          global.get $part2Value
          return
        )
      )" 
    in
    let actual = fst (Int32Map.find_exn (analyze_intra module_ [0l]) 0l) in
    let expected = Summary.{ ret = Some Reduced_interval_congruence.RIC.Bottom; mem = Domain.bottom; globals = [Reduced_interval_congruence.RIC.relative_ric (Var.to_string (Var.Global 0)); 
                                                                                    Reduced_interval_congruence.RIC.relative_ric (Var.to_string (Var.Global 0));
                                                                                    Reduced_interval_congruence.RIC.relative_ric (Var.to_string (Var.Global 2))] } in
    check expected actual

end *)