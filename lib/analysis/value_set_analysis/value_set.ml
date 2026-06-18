(** Entry points for the value-set analysis.

    This module wires the value-set transfer function to the generic intra- and
    interprocedural analysis engines, initializes summaries, applies summaries at
    call sites, and exposes the main analysis functions used by Wassail. *)

open Core
open Helpers

module Options = Value_set_options
module Domain = Abstract_store_domain
module RIC = Reduced_interval_congruence.RIC
module TransferFunction = Value_set_transfer.Make
module Summary = Value_set_summary
module ClassicalInter = Intra.MakeClassicalInter(TransferFunction)
module ValueSetCallAdapter: Intra.CALL_ADAPTER
  with module Transfer = TransferFunction
  and type extra = TransferFunction.summary Int32Map.t =
struct
  module Transfer = TransferFunction
  type extra = TransferFunction.summary Int32Map.t

  (** [analyze_call module_ cfg instr state summaries] applies the effect of
    [instr] to [state] using the available function summaries. *)
  let analyze_call
      (module_ : Wasm_module.t)
      (_cfg : Transfer.annot_expected Cfg.t)
      (instr : Transfer.annot_expected Instr.labelled_call)
      (state : Transfer.State.t)
      (summaries : extra)
    : Transfer.State.t =
    Print_trace.instruction instr.line_number (Call instr) state.unreachable;
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
        state |> Domain.get
          ~var:(Variable.Var (pop (Spec_domain.get_or_fail instr.annotation_before).vstack))
      in
      let targets = Call_graph.indirect_call_targets module_ typ
        |> List.filter ~f:(fun idx ->
            match call_index with
            | Bitfield _
            | Boolean _ -> true
            | ValueSet RIC { offset = (o,_); _ } when not (String.is_empty o) -> true
            | ValueSet _ ->
                Value_set_abstraction.meet
                  call_index
                  (ValueSet (Reduced_interval_congruence.RIC.of_int32 idx))
                |> Value_set_abstraction.equal Value_set_abstraction.bottom
                |> not)
      in
      if List.is_empty targets then (Log.error "indirect call index doesn't match any function"; failwith "invalid program: indirect call index doesn't match any function");
      List.fold_left targets
        ~init:Transfer.bottom
        ~f:(fun acc idx -> Transfer.State.join (apply_summary idx arity state) acc)
end
module Intra = Intra.MakeSumm(TransferFunction)(ValueSetCallAdapter)
module Inter = Inter.MakeSummaryBased(TransferFunction)(Intra)

(** [analyze_intra module_ schedule] runs the summary-based intraprocedural
    value-set analysis following [schedule].

    Each analyzed function returns its summary and, when available, its annotated
    value-set CFG. *)
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
      let result_cfg = Intra.analyze wasm_mod annotated_cfg summaries in
      let value_set_summary = TransferFunction.extract_summary wasm_mod annotated_cfg result_cfg in
      (value_set_summary, Some result_cfg))

(** [analyze_inter module_ schedule] runs the summary-based interprocedural
    value-set analysis over the SCCs in [schedule].

    Each result contains the original spec CFG, the value-set CFG, and the final
    summary. *)
let analyze_inter : Wasm_module.t -> Int32.t list list -> (Spec_domain.t Cfg.t * Domain.t Cfg.t * Summary.t) Int32Map.t =
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
              Int32Map.set summaries ~key:desc.idx ~data:(Summary.of_import desc.idx desc.name wasm_mod.nglobals desc.arguments desc.returns)) in
      Inter.analyze wasm_mod ~cfgs:annotated_scc ~summaries:summaries'
      |> Int32Map.mapi ~f:(fun ~key:idx ~data:(value_set_cfg, summary) ->
          let spec_cfg = Int32Map.find_exn scc idx in
          (spec_cfg, value_set_cfg, summary)))
      
(** [analyze_inter_classical module_ entry] runs the classical interprocedural
    value-set analysis from [entry]. *)
let analyze_inter_classical (module_ : Wasm_module.t) (entry : Int32.t) : Domain.t Icfg.t =
  ClassicalInter.analyze module_ (Analysis_helpers.mk_inter_classical module_ entry)

(** Result of [run_pointer_analysis]: the value-set CFG, the propagated spec
    instructions, and the computed summaries. 
    
    This type is used by the slicer. *)
type pointer_analysis = 
  Domain.t Cfg.t * Spec_domain.t Instr.t Instr.Label.Map.t * Domain.t Int32Map.t

(** [run_pointer_analysis module_ cfg funidx] runs the analysis on function [funidx], 
    needed by other WASSAIL tools, namely the slicer. *)
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



(*
TTTTTTTTTTTTTTTTTTTTTTTEEEEEEEEEEEEEEEEEEEEEE   SSSSSSSSSSSSSSS TTTTTTTTTTTTTTTTTTTTTTT   SSSSSSSSSSSSSSS 
T:::::::::::::::::::::TE::::::::::::::::::::E SS:::::::::::::::ST:::::::::::::::::::::T SS:::::::::::::::S
T:::::::::::::::::::::TE::::::::::::::::::::ES:::::SSSSSS::::::ST:::::::::::::::::::::TS:::::SSSSSS::::::S
T:::::TT:::::::TT:::::TEE::::::EEEEEEEEE::::ES:::::S     SSSSSSST:::::TT:::::::TT:::::TS:::::S     SSSSSSS
TTTTTT  T:::::T  TTTTTT  E:::::E       EEEEEES:::::S            TTTTTT  T:::::T  TTTTTTS:::::S            
        T:::::T          E:::::E             S:::::S                    T:::::T        S:::::S            
        T:::::T          E::::::EEEEEEEEEE    S::::SSSS                 T:::::T         S::::SSSS         
        T:::::T          E:::::::::::::::E     SS::::::SSSSS            T:::::T          SS::::::SSSSS    
        T:::::T          E:::::::::::::::E       SSS::::::::SS          T:::::T            SSS::::::::SS  
        T:::::T          E::::::EEEEEEEEEE          SSSSSS::::S         T:::::T               SSSSSS::::S 
        T:::::T          E:::::E                         S:::::S        T:::::T                    S:::::S
        T:::::T          E:::::E       EEEEEE            S:::::S        T:::::T                    S:::::S
      TT:::::::TT      EE::::::EEEEEEEE:::::ESSSSSSS     S:::::S      TT:::::::TT      SSSSSSS     S:::::S
      T:::::::::T      E::::::::::::::::::::ES::::::SSSSSS:::::S      T:::::::::T      S::::::SSSSSS:::::S
      T:::::::::T      E::::::::::::::::::::ES:::::::::::::::SS       T:::::::::T      S:::::::::::::::SS 
      TTTTTTTTTTT      EEEEEEEEEEEEEEEEEEEEEE SSSSSSSSSSSSSSS         TTTTTTTTTTT       SSSSSSSSSSSSSSS   
*)

let%test_module "value-set tests" = (module struct


  let%test "value-set summary tests" =
    print_endline "\n_______ _________________ _______\n        Integration tests        \n------- ----------------- -------\n"; true

  let test_label name = Printf.printf "%s\n" name

  let analyze (schedule : int32 list) (fct : int32) (program : string) : Domain.t =
    let cfg =
      ((program
      |> Wasm_module.of_string
      |> analyze_intra) schedule
      |> Int32Map.find_exn) fct
      |> snd
      |> Option.value_exn 
    in
    match (Cfg.find_block_exn cfg cfg.exit_block).content with
    | Control i -> i.annotation_after
    | _ -> failwith "Cfg.exit_annotation_exn: exit block is not a control block"

  let analyze_inter' (program : string) ~(fct_idx : int32) : Summary.t =
    let module_ = program |> Wasm_module.of_string in
    let cg = module_ |> Call_graph.make in
    let schedule = Call_graph.analysis_schedule cg module_.nfuncimports in
    let final_annotation (cfg : 'a Cfg.t) : 'a =
      let exit_block = Cfg.find_block_exn cfg (Cfg.exit_block cfg) in
      exit_block.annotation_after in
    analyze_inter module_ schedule 
    |> (fun a -> Int32Map.find_exn a fct_idx)
    |> (fun (_,cfg,_) -> cfg)
    |> final_annotation

  let i_var (f : int32) (id : int) : Variable.t = Variable.Var (Var {section = Function f; id})

  let check_value (state : Domain.t) (var : Variable.t) (value : Domain.Value.t) : bool =
    let actual_value = state |> Domain.get ~var in
    actual_value |> Domain.Value.equal value && (Printf.printf "\tSUCCESS: %s = %s\n" (Variable.to_string var) (Domain.Value.to_string actual_value); true)
    || (Printf.printf "\t\tFAILURE: %s = [expected: %s; actual: %s]\n" (Variable.to_string var) (Domain.Value.to_string value) (Domain.Value.to_string actual_value); false)


  let () = Value_set_options.show_intermediates := true

  let%test "add.wat" =
    let exit_state = 
      "(module
        (memory (export \"mem\") 1)
        (global $g0 (mut i32) (i32.const 1024))

        (func $main (export \"main\") (param $l0 i32) (result i32) (local $l1 i32)
    ;; add two constants:
          i32.const 42
          i32.const 14
          i32.add 
          drop

    ;; add a constant and a global:
          i32.const 99
          global.get $g0
          i32.add
          drop

    ;; add a constant and a parameter:
          i32.const 77
          local.get $l0
          i32.add
          drop

    ;; add a constant and a local
          i32.const 5
          local.get $l1
          i32.add
          drop

    ;; add a global and a parameter:
          global.get $g0
          local.get $l0
          i32.add
        )
      )"
    |> analyze [0l] 0l 
    in
    test_label "[add.wat]";
    check_value exit_state (i_var 0l 2) (ValueSet (RIC.constant 56l))
    && check_value exit_state (i_var 0l 6) (ValueSet (RIC.(constant 99l + relative_ric "g0")))
    && check_value exit_state (i_var 0l 10) (ValueSet (RIC.(constant 77l + relative_ric "l0")))
    && check_value exit_state (i_var 0l 14) (ValueSet (RIC.constant 5l))
    && check_value exit_state (i_var 0l 18) (ValueSet (RIC.(relative_ric "g0" + relative_ric "l0")))

  let%test "sub.wat" =
    let exit_state = 
      "(module
        (memory (export \"mem\") 1)
        (global $g0 (mut i32) (i32.const 1024))

        (func $main (export \"main\") (param $l0 i32) (result i32) (local $l1 i32)
          ;; constant - constant
          i32.const 14
          i32.const 3
          i32.sub ;; 11
          drop

          ;; constant - (negative constant)
          i32.const 42
          i32.const -13
          i32.sub ;; 55
          drop

          ;; constant - global
          i32.const 26
          global.get $g0
          i32.sub ;; 26+negg0
          drop

          ;; constant - param
          i32.const 126
          local.get $l0
          i32.sub ;; 126+negl0
          drop

          ;; constant - local
          i32.const 1024
          local.get $l1
          i32.sub ;; 1024
          drop

          ;; global - cst
          global.get $g0
          i32.const 26
          i32.sub ;; g0 - 26
          drop

          ;; param - cst
          local.get $l0
          i32.const 126
          i32.sub ;; l0 - 126
          drop

          ;; local - cst
          local.get $l1
          i32.const 1024
          i32.sub ;; - 1024
          drop

          ;; global - param
          global.get $g0
          local.get $l0
          i32.sub ;; g0+negl0
        )
      )
      "
      |> analyze [0l] 0l 
    in
    test_label "[sub.wat]";
       check_value exit_state (i_var 0l 2)  (ValueSet (RIC.constant 11l))
    && check_value exit_state (i_var 0l 6)  (ValueSet (RIC.(constant 55l)))
    && check_value exit_state (i_var 0l 10) (ValueSet (RIC.(constant 26l + relative_ric "negg0")))
    && check_value exit_state (i_var 0l 14) (ValueSet (RIC.(constant 126l + relative_ric "negl0")))
    && check_value exit_state (i_var 0l 18) (ValueSet (RIC.(constant 1024l)))
    && check_value exit_state (i_var 0l 22) (ValueSet (RIC.(relative_ric "g0" - constant 26l)))
    && check_value exit_state (i_var 0l 26) (ValueSet (RIC.(relative_ric "l0" - constant 126l)))
    && check_value exit_state (i_var 0l 30) (ValueSet (RIC.(constant (-1024l))))
    && check_value exit_state (i_var 0l 34) (ValueSet (RIC.(relative_ric "g0" + relative_ric "negl0")))

  let%test "global.set.get.wat" =
    let exit_state = 
      "(module
        (memory (export \"mem\") 1)
        (global $g0 (mut i32) (i32.const 1024))
        (global $g1 (mut i32) (i32.const 1024))

        (func $main (export \"main\") (param $l0 i32) (result i32) (local $l1 i32)
          ;; global = param
          local.get $l0
          global.set $g0
          
          ;; global = const
          i32.const 14
          global.set $g1

          global.get $g1
        )
      )"
      |> analyze [0l] 0l 
    in
    test_label "[global.set.get.wat]";
       check_value exit_state (Variable.Var (Var.Global 0))  (ValueSet (RIC.relative_ric "l0"))
    && check_value exit_state (Variable.Var (Var.Global 1))  (ValueSet (RIC.(constant 14l)))
    && check_value exit_state (Variable.Var (Var.Return (0l,0l))) (ValueSet (RIC.constant 14l))


  let%test "local.set.get.wat" =
    let exit_state =
      "(module
        (memory (export \"mem\") 1)

        (func $main (export \"main\") (param $l0 i32) (result i32) (local $l1 i32)
          ;; local = const
          i32.const 14
          local.set $l1

          ;; get local
          local.get $l1
        )
      )"
      |> analyze [0l] 0l
    in
    test_label "[local.set.get.wat]";
       check_value exit_state (Variable.Var (Var.Local 1)) (ValueSet (RIC.constant 14l))
    && check_value exit_state (Variable.Var (Var.Return (0l, 0l))) (ValueSet (RIC.constant 14l))

  let%test "local.tee.wat" =
    let exit_state =
      "(module
      (memory (export \"mem\") 1)

      (func $main (export \"main\") (param $l0 i32) (result i32) (local $l1 i32)
        ;; local = const, but keep value on stack
        i32.const 42
        local.tee $l1
        )
      )"
      |> analyze [0l] 0l
    in
    test_label "[local.tee.wat]";
       check_value exit_state (Variable.Var (Var.Local 1)) (ValueSet (RIC.constant 42l))
    && check_value exit_state (Variable.Var (Var.Return (0l, 0l))) (ValueSet (RIC.constant 42l))

  let%test "global.get.wat" =
    let exit_state =
      "(module
        (memory (export \"mem\") 1)
        (global $g0 (mut i32) (i32.const 1024))

        (func $main (export \"main\") (result i32)
          ;; get initial global value
          global.get $g0
        )
      )"
      |> analyze [0l] 0l
    in
    test_label "[global.get.wat]";
    check_value exit_state (Variable.Var (Var.Return (0l, 0l))) (ValueSet (RIC.relative_ric "g0"))


  let%test "eqz.wat" =
    let exit_state =
      "(module
        (memory (export \"mem\") 1)

        (func $main (export \"main\") (result i32)
          ;; zero is true
          i32.const 0
          i32.eqz
          drop

          ;; non-zero is false
          i32.const 42
          i32.eqz
        )
      )"
      |> analyze [0l] 0l
    in
    test_label "[eqz.wat]";
       check_value exit_state (i_var 0l 1) (ValueSet RIC.one)
    && check_value exit_state (Variable.Var (Var.Return (0l, 0l))) (ValueSet RIC.zero)

  let%test "select.wat" =
    let exit_state =
      "(module
        (memory (export \"mem\") 1)

        (func $main (export \"main\") (result i32)
          ;; true condition keeps first value
          i32.const 10
          i32.const 20
          i32.const 1
          select
          drop

          ;; false condition keeps second value
          i32.const 10
          i32.const 20
          i32.const 0
          select
        )
      )"
      |> analyze [0l] 0l
    in
    test_label "[select.wat]";
       check_value exit_state (i_var 0l 3) (ValueSet (RIC.constant 10l))
    && check_value exit_state (Variable.Var (Var.Return (0l, 0l))) (ValueSet (RIC.constant 20l))

  let%test "select.unknown-condition.wat" =
    let exit_state =
      "(module
        (memory (export \"mem\") 1)

        (func $main (export \"main\") (param $l0 i32) (result i32)
          i32.const 10
          i32.const 20
          local.get $l0
          select
        )
      )"
      |> analyze [0l] 0l
    in
    test_label "[select.unknown-condition.wat]";
    check_value exit_state
      (Variable.Var (Var.Return (0l, 0l)))
      (ValueSet (RIC.join (RIC.constant 10l) (RIC.constant 20l)))

  let%test "select.nonzero-condition.wat" =
    let exit_state =
      "(module
        (memory (export \"mem\") 1)

        (func $main (export \"main\") (result i32)
          ;; condition is definitely nonzero
          i32.const 10
          i32.const 20
          i32.const -7
          select
        )
      )"
      |> analyze [0l] 0l
    in
    test_label "[select.nonzero-condition.wat]";
    check_value exit_state
      (Variable.Var (Var.Return (0l, 0l)))
      (ValueSet (RIC.constant 10l))

  let%test "select.zero-condition.wat" =
    let exit_state =
      "(module
        (memory (export \"mem\") 1)

        (func $main (export \"main\") (result i32)
          ;; condition is definitely zero
          i32.const 10
          i32.const 20
          i32.const 0
          select
        )
      )"
      |> analyze [0l] 0l
    in
    test_label "[select.zero-condition.wat]";
    check_value exit_state
      (Variable.Var (Var.Return (0l, 0l)))
      (ValueSet (RIC.constant 20l))

  let%test "select.relative-value.wat" =
    let exit_state =
      "(module
        (memory (export \"mem\") 1)

        (func $main (export \"main\") (param $l0 i32) (result i32)
          local.get $l0
          i32.const 42
          i32.const 1
          select
        )
      )"
      |> analyze [0l] 0l
    in
    test_label "[select.relative-value.wat]";
    check_value exit_state
      (Variable.Var (Var.Return (0l, 0l)))
      (ValueSet (RIC.relative_ric "l0"))
  
  let%test "if.constant-condition.wat" =
    let exit_state =
      "(module
        (memory (export \"mem\") 1)

        (func $main (export \"main\") (result i32)
          i32.const 1
          if (result i32)
            i32.const 10
          else
            i32.const 20
          end
        )
      )"
      |> analyze [0l] 0l
    in
    test_label "[if.constant-condition.wat]";
    check_value exit_state
      (Variable.Var (Var.Return (0l, 0l)))
      (ValueSet (RIC.constant 10l))

  let%test "if.in-loop.accessible-branches.wat" =
    let exit_state =
      "(module
        (memory (export \"mem\") 1)

        (func $main (export \"main\") (result i32) (local $i i32) (local $res i32)
          ;; i = 0
          i32.const 0
          local.set $i

          block
            loop
              ;; if i == 0:
              ;;   res = 10
              ;; else:
              ;;   res = 20
              local.get $i
              i32.eqz
              if
                i32.const 10
                local.set $res
              else
                i32.const 20
                local.set $res
              end

              ;; i++
              local.get $i
              i32.const 1
              i32.add
              local.tee $i

              ;; continue while i < 2
              i32.const 2
              i32.lt_s
              br_if 0
            end
          end

          local.get $res
        )
      )"
      |> analyze [0l] 0l
    in
    test_label "[if.in-loop.accessible-branches.wat]";
    check_value exit_state
      (Variable.Var (Var.Return (0l, 0l)))
      (ValueSet (RIC.join (RIC.constant 10l) (RIC.constant 20l)))

  let compare_test ~(name : string) ~(op : string) ~(lhs : int32) ~(rhs : int32) ~(expected : int32) : bool =
    let exit_state =
      Printf.sprintf
        "(module
          (memory (export \"mem\") 1)

          (func $main (export \"main\") (result i32)
            i32.const %ld
            i32.const %ld
            i32.%s
          )
        )"
        lhs
        rhs
        op
      |> analyze [0l] 0l
    in
    test_label ("[" ^ name ^ "]");
    check_value exit_state
      (Variable.Var (Var.Return (0l, 0l)))
      (ValueSet (RIC.constant expected))

  let%test "eq: equal constants" =
    compare_test
      ~name:"eq: 42 == 42"
      ~op:"eq"
      ~lhs:42l
      ~rhs:42l
      ~expected:1l

  let%test "eq: distinct constants" =
    compare_test
      ~name:"eq: 42 == 43"
      ~op:"eq"
      ~lhs:42l
      ~rhs:43l
      ~expected:0l

  let%test "ne: equal constants" =
    compare_test
      ~name:"ne: 42 != 42"
      ~op:"ne"
      ~lhs:42l
      ~rhs:42l
      ~expected:0l

  let%test "ne: distinct constants" =
    compare_test
      ~name:"ne: 42 != 43"
      ~op:"ne"
      ~lhs:42l
      ~rhs:43l
      ~expected:1l

  let%test "lt_s: signed negative is less than positive" =
    compare_test
      ~name:"lt_s: (-1) <s 1"
      ~op:"lt_s"
      ~lhs:(-1l)
      ~rhs:1l
      ~expected:1l

  let%test "lt_s: equality is false" =
    compare_test
      ~name:"lt_s: 7 <s 7"
      ~op:"lt_s"
      ~lhs:7l
      ~rhs:7l
      ~expected:0l

  let%test "lt_u: unsigned negative is greater than positive" =
    compare_test
      ~name:"lt_u: (-1) <u 1"
      ~op:"lt_u"
      ~lhs:(-1l)
      ~rhs:1l
      ~expected:0l

  let%test "lt_u: small unsigned is less than large unsigned" =
    compare_test
      ~name:"lt_u: 1 <u (-1)"
      ~op:"lt_u"
      ~lhs:1l
      ~rhs:(-1l)
      ~expected:1l

  let%test "le_s: signed negative is less or equal to positive" =
    compare_test
      ~name:"le_s: (-1) <=s 1"
      ~op:"le_s"
      ~lhs:(-1l)
      ~rhs:1l
      ~expected:1l

  let%test "le_s: equality is true" =
    compare_test
      ~name:"le_s: 7 <=s 7"
      ~op:"le_s"
      ~lhs:7l
      ~rhs:7l
      ~expected:1l

  let%test "le_u: unsigned negative is greater than positive" =
    compare_test
      ~name:"le_u: (-1) <=u 1"
      ~op:"le_u"
      ~lhs:(-1l)
      ~rhs:1l
      ~expected:0l

  let%test "le_u: equality is true" =
    compare_test
      ~name:"le_u: (-1) <=u (-1)"
      ~op:"le_u"
      ~lhs:(-1l)
      ~rhs:(-1l)
      ~expected:1l

  let%test "gt_s: signed negative is not greater than positive" =
    compare_test
      ~name:"gt_s: (-1) >s 1"
      ~op:"gt_s"
      ~lhs:(-1l)
      ~rhs:1l
      ~expected:0l

  let%test "gt_s: signed positive is greater than negative" =
    compare_test
      ~name:"gt_s: 1 >s (-1)"
      ~op:"gt_s"
      ~lhs:1l
      ~rhs:(-1l)
      ~expected:1l

  let%test "gt_u: unsigned negative is greater than positive" =
    compare_test
      ~name:"gt_u: (-1) >u 1"
      ~op:"gt_u"
      ~lhs:(-1l)
      ~rhs:1l
      ~expected:1l

  let%test "gt_u: equality is false" =
    compare_test
      ~name:"gt_u: 7 >u 7"
      ~op:"gt_u"
      ~lhs:7l
      ~rhs:7l
      ~expected:0l

  let%test "ge_s: signed positive is greater or equal to negative" =
    compare_test
      ~name:"ge_s: 1 >=s (-1)"
      ~op:"ge_s"
      ~lhs:1l
      ~rhs:(-1l)
      ~expected:1l

  let%test "ge_s: signed negative is not greater or equal to positive" =
    compare_test
      ~name:"ge_s: (-1) >=s 1"
      ~op:"ge_s"
      ~lhs:(-1l)
      ~rhs:1l
      ~expected:0l

  let%test "ge_s: equality is true" =
    compare_test
      ~name:"ge_s: (-1) >=s (-1)"
      ~op:"ge_s"
      ~lhs:(-1l)
      ~rhs:(-1l)
      ~expected:1l

  let%test "ge_u: unsigned negative is greater or equal to positive" =
    compare_test
      ~name:"ge_u: (-1) >=u 1"
      ~op:"ge_u"
      ~lhs:(-1l)
      ~rhs:1l
      ~expected:1l

  let%test "ge_u: equality is true" =
    compare_test
      ~name:"ge_u: 7 >=u 7"
      ~op:"ge_u"
      ~lhs:7l
      ~rhs:7l
      ~expected:1l

  let () = Value_set_options.show_intermediates := false

  let compare_test_non_singleton ~(name : string) ~(op : string) ~(lhs : int32 * int32) ~(rhs : int32 * int32) ~(expected : RIC.t) : bool =
    let exit_state =
      Printf.sprintf
        "(module
          (memory (export \"mem\") 1)

          (func $main (export \"main\") (param $x i32) (result i32) (local $l1 i32) (local $l2 i32)
            local.get $x
            if
              i32.const %ld
              local.set $l1
              i32.const %ld
              local.set $l2
            else
              i32.const %ld
              local.set $l1
              i32.const %ld
              local.set $l2
            end
            local.get $l1
            local.get $l2
            i32.%s
          )
        )"
        (fst lhs)
        (fst rhs)
        (snd lhs)
        (snd rhs)
        op
      |> analyze [0l] 0l
    in
    test_label ("[" ^ name ^ "]");
    check_value exit_state
      (Variable.Var (Var.Return (0l, 0l)))
      (ValueSet expected)


  let%test "non-singleton eq: disjoint values are always false" =
    compare_test_non_singleton
      ~name:"non-singleton eq: {1,2} == {3,4}"
      ~op:"eq"
      ~lhs:(1l, 2l)
      ~rhs:(3l, 4l)
      ~expected:RIC.zero

  let%test "non-singleton eq: identical values may be true or false" =
    compare_test_non_singleton
      ~name:"non-singleton eq: {1,2} == {1,2}"
      ~op:"eq"
      ~lhs:(1l, 2l)
      ~rhs:(1l, 2l)
      ~expected:RIC.(join zero one)

  let%test "non-singleton eq: same singleton after both branches is always true" =
    compare_test_non_singleton
      ~name:"non-singleton eq: {7} == {7}"
      ~op:"eq"
      ~lhs:(7l, 7l)
      ~rhs:(7l, 7l)
      ~expected:RIC.one

  let%test "non-singleton eq: non-disjoint sets" =
    compare_test_non_singleton
      ~name:"non-singleton eq: {7,8} == {8,12}"
      ~op:"eq"
      ~lhs:(7l, 8l)
      ~rhs:(8l, 12l)
      ~expected:RIC.(join zero one)

  let%test "non-singleton ne: disjoint values are always true" =
    compare_test_non_singleton
      ~name:"non-singleton ne: {1,2} != {3,4}"
      ~op:"ne"
      ~lhs:(1l, 2l)
      ~rhs:(3l, 4l)
      ~expected:RIC.one

  let%test "non-singleton ne: identical values may be true or false" =
    compare_test_non_singleton
      ~name:"non-singleton ne: {1,2} != {1,2}"
      ~op:"ne"
      ~lhs:(1l, 2l)
      ~rhs:(1l, 2l)
      ~expected:RIC.(join zero one)

  let%test "non-singleton and singleton ne:" =
    compare_test_non_singleton
      ~name:"non-singleton ne: {1,2} != {2}"
      ~op:"ne"
      ~lhs:(1l, 2l)
      ~rhs:(2l, 2l)
      ~expected:RIC.(join zero one)

  let%test "non-singleton lt_s: all lhs values are below rhs values" =
    compare_test_non_singleton
      ~name:"non-singleton lt_s: {-4,-2} <s {1,3}"
      ~op:"lt_s"
      ~lhs:(-4l, -2l)
      ~rhs:(1l, 3l)
      ~expected:RIC.one

  let%test "non-singleton lt_s: all lhs values are above rhs values" =
    compare_test_non_singleton
      ~name:"non-singleton lt_s: {4,6} <s {-1,0}"
      ~op:"lt_s"
      ~lhs:(4l, 6l)
      ~rhs:(-1l, 0l)
      ~expected:RIC.zero

  let%test "non-singleton lt_s: overlapping values may be true or false" =
    compare_test_non_singleton
      ~name:"non-singleton lt_s: {1,3} <s {2,4}"
      ~op:"lt_s"
      ~lhs:(1l, 3l)
      ~rhs:(2l, 4l)
      ~expected:RIC.(join zero one)

  let%test "non-singleton le_s: equality boundary may be true or false" =
    compare_test_non_singleton
      ~name:"non-singleton le_s: {2,4} <=s {2,3}"
      ~op:"le_s"
      ~lhs:(2l, 4l)
      ~rhs:(2l, 3l)
      ~expected:RIC.(join zero one)

  let%test "non-singleton gt_s: all lhs values are above rhs values" =
    compare_test_non_singleton
      ~name:"non-singleton gt_s: {5,8} >s {1,4}"
      ~op:"gt_s"
      ~lhs:(5l, 8l)
      ~rhs:(1l, 4l)
      ~expected:RIC.one

  let%test "non-singleton ge_s: all lhs values are below rhs values" =
    compare_test_non_singleton
      ~name:"non-singleton ge_s: {-3,-1} >=s {0,2}"
      ~op:"ge_s"
      ~lhs:(-3l, -1l)
      ~rhs:(0l, 2l)
      ~expected:RIC.zero

  let%test "non-singleton lt_u: negative signed values are large unsigned values" =
    compare_test_non_singleton
      ~name:"non-singleton lt_u: {-2,-1} <u {1,2}"
      ~op:"lt_u"
      ~lhs:(-2l, -1l)
      ~rhs:(1l, 2l)
      ~expected:RIC.zero

  let%test "non-singleton lt_u: small positives are below negative signed values" =
    compare_test_non_singleton
      ~name:"non-singleton lt_u: {1,2} <u {-2,-1}"
      ~op:"lt_u"
      ~lhs:(1l, 2l)
      ~rhs:(-2l, -1l)
      ~expected:RIC.one

  let%test "non-singleton le_u: equality among large unsigned values may be true or false" =
    compare_test_non_singleton
      ~name:"non-singleton le_u: {-2,-1} <=u {-2,-1}"
      ~op:"le_u"
      ~lhs:(-2l, -1l)
      ~rhs:(-2l, -1l)
      ~expected:RIC.(join zero one)

  let%test "non-singleton gt_u: negative signed values are above positives" =
    compare_test_non_singleton
      ~name:"non-singleton gt_u: {-2,-1} >u {1,2}"
      ~op:"gt_u"
      ~lhs:(-2l, -1l)
      ~rhs:(1l, 2l)
      ~expected:RIC.one

  let%test "non-singleton ge_u: positives are not above negative signed values" =
    compare_test_non_singleton
      ~name:"non-singleton ge_u: {1,2} >=u {-2,-1}"
      ~op:"ge_u"
      ~lhs:(1l, 2l)
      ~rhs:(-2l, -1l)
      ~expected:RIC.zero

  let%test "non-singleton ge_u: negatives" =
    compare_test_non_singleton
      ~name:"non-singleton ge_u: {-6,-4} >=u {-7,-5}"
      ~op:"ge_u"
      ~lhs:(-6l, -4l)
      ~rhs:(-7l, -5l)
      ~expected:RIC.(join zero one)

  let%test "non-singleton ge_u: negatives disjoint" =
    compare_test_non_singleton
      ~name:"non-singleton ge_u: {-6,-4} >=u {-7,-9}"
      ~op:"ge_u"
      ~lhs:(-6l, -4l)
      ~rhs:(-7l, -9l)
      ~expected:RIC.one

  let%test "non-singleton lt_s: same set may be true or false" =
    compare_test_non_singleton
      ~name:"non-singleton lt_s: {1,2} <s {1,2}"
      ~op:"lt_s"
      ~lhs:(1l, 2l)
      ~rhs:(1l, 2l)
      ~expected:RIC.(join zero one)

  let%test "non-singleton le_s: same set is not always true" =
    compare_test_non_singleton
      ~name:"non-singleton le_s: {1,2} <=s {1,2}"
      ~op:"le_s"
      ~lhs:(1l, 2l)
      ~rhs:(1l, 2l)
      ~expected:RIC.(join zero one)

  let%test "non-singleton gt_s: same set may be true or false" =
    compare_test_non_singleton
      ~name:"non-singleton gt_s: {1,2} >s {1,2}"
      ~op:"gt_s"
      ~lhs:(1l, 2l)
      ~rhs:(1l, 2l)
      ~expected:RIC.(join zero one)

  let%test "non-singleton ge_s: same set is not always true" =
    compare_test_non_singleton
      ~name:"non-singleton ge_s: {1,2} >=s {1,2}"
      ~op:"ge_s"
      ~lhs:(1l, 2l)
      ~rhs:(1l, 2l)
      ~expected:RIC.(join zero one)

  let%test "non-singleton lt_s: equality-only boundary is false" =
    compare_test_non_singleton
      ~name:"non-singleton lt_s: {2} <s {2}"
      ~op:"lt_s"
      ~lhs:(2l, 2l)
      ~rhs:(2l, 2l)
      ~expected:RIC.zero

  let%test "non-singleton le_s: equality-only boundary is true" =
    compare_test_non_singleton
      ~name:"non-singleton le_s: {2} <=s {2}"
      ~op:"le_s"
      ~lhs:(2l, 2l)
      ~rhs:(2l, 2l)
      ~expected:RIC.one

  let%test "non-singleton lt_u: mixed signs falls back to maybe" =
    compare_test_non_singleton
      ~name:"non-singleton lt_u: {-1,1} <u {0,2}"
      ~op:"lt_u"
      ~lhs:(-1l, 1l)
      ~rhs:(0l, 2l)
      ~expected:RIC.(join zero one)

  let%test "non-singleton gt_u: mixed signs falls back to maybe" =
    compare_test_non_singleton
      ~name:"non-singleton gt_u: {-1,1} >u {0,2}"
      ~op:"gt_u"
      ~lhs:(-1l, 1l)
      ~rhs:(0l, 2l)
      ~expected:RIC.(join zero one)

  let%test "non-singleton le_u: positives below positives" =
    compare_test_non_singleton
      ~name:"non-singleton le_u: {1,2} <=u {3,4}"
      ~op:"le_u"
      ~lhs:(1l, 2l)
      ~rhs:(3l, 4l)
      ~expected:RIC.one

  let%test "non-singleton ge_u: positives below positives is false" =
    compare_test_non_singleton
      ~name:"non-singleton ge_u: {1,2} >=u {3,4}"
      ~op:"ge_u"
      ~lhs:(1l, 2l)
      ~rhs:(3l, 4l)
      ~expected:RIC.zero

  let%test "compare condition refines local in if branches" =
    let exit_state =
      "(module
        (memory (export \"mem\") 1)

        (func $main (export \"main\") (param $x i32) (result i32)
          (local $l1 i32)
          (local $true i32)
          (local $false i32)

          ;; l1 = {1,2}
          local.get $x
          if
            i32.const 1
            local.set $l1
          else
            i32.const 2
            local.set $l1
          end

          ;; true branch should refine l1 to 1
          ;; false branch should refine l1 to 2
          local.get $l1
          i32.const 1
          i32.eq
          if
            local.get $l1
            local.set $true
          else
            local.get $l1
            local.set $false
          end

          i32.const 0
        )
      )"
      |> analyze [0l] 0l
    in
    test_label "[compare condition refines local in if branches]";
      check_value exit_state
        (Variable.Var (Var.Local 2))
        (ValueSet RIC.(join zero one))
    && check_value exit_state
        (Variable.Var (Var.Local 3))
        (ValueSet (RIC.(join (constant 2l) zero)))

  let%test "drop.wat" =
    let exit_state =
      "(module
        (memory (export \"mem\") 1)

        (func $main (export \"main\") (result i32) (local $l0 i32)
          i32.const 42
          local.set $l0

          local.get $l0

          i32.const 10
          drop
        )
      )"
      |> analyze [0l] 0l
    in
    test_label "[drop.wat]";
      check_value exit_state
        (Variable.Var (Var.Local 0))
        (ValueSet (RIC.constant 42l))
    && check_value exit_state
        (Variable.Var (Var.Return (0l, 0l)))
        (ValueSet (RIC.constant 42l))

  let%test "nop.wat" =
    let exit_state =
      "(module
        (memory (export \"mem\") 1)

        (func $main (export \"main\") (result i32) (local $l0 i32)
          i32.const 42
          local.set $l0

          nop

          local.get $l0
        )
      )"
      |> analyze [0l] 0l
    in
    test_label "[nop.wat]";
      check_value exit_state
        (Variable.Var (Var.Local 0))
        (ValueSet (RIC.constant 42l))
    && check_value exit_state
        (Variable.Var (Var.Return (0l, 0l)))
        (ValueSet (RIC.constant 42l))

  let%test "unreachable.wat" =
    let exit_state =
      "(module
        (memory (export \"mem\") 1)

        (func $main (export \"main\") (result i32)
          unreachable

          i32.const 42
        )
      )"
      |> analyze [0l] 0l
    in
    test_label "[unreachable.wat]";
    Domain.equal exit_state Domain.bottom

  let%test "br.wat" =
    let exit_state =
      "(module
        (memory (export \"mem\") 1)

        (func $main (export \"main\") (result i32)
          block (result i32)
            i32.const 42
            br 0

            ;; unreachable
            i32.const 99
          end
        )
      )"
      |> analyze [0l] 0l
    in
    test_label "[br.wat]";
    check_value exit_state
      (Variable.Var (Var.Return (0l, 0l)))
      (ValueSet (RIC.constant 42l))

  let%test "eqz.non-singleton.wat" =
    let exit_state =
      "(module
        (memory (export \"mem\") 1)

        (func $main (export \"main\") (param $x i32) (result i32) (local $l0 i32)
          local.get $x
          if
            i32.const 0
            local.set $l0
          else
            i32.const 42
            local.set $l0
          end

          local.get $l0
          i32.eqz
        )
      )"
      |> analyze [0l] 0l
    in
    test_label "[eqz.non-singleton.wat]";
    check_value exit_state
      (Variable.Var (Var.Return (0l, 0l)))
      (ValueSet RIC.(join zero one))

  let%test "eqz.refines-local-in-if.wat" =
    let exit_state =
      "(module
        (memory (export \"mem\") 1)

        (func $main (export \"main\") (param $x i32) (result i32)
          (local $l0 i32)
          (local $true i32)
          (local $false i32)

          ;; l0 = {0,42}
          local.get $x
          if
            i32.const 0
            local.set $l0
          else
            i32.const 42
            local.set $l0
          end

          local.get $l0
          i32.eqz
          if
            ;; true branch: l0 == 0
            local.get $l0
            local.set $true
          else
            ;; false branch: l0 != 0
            local.get $l0
            local.set $false
          end

          i32.const 0
        )
      )"
      |> analyze [0l] 0l
    in
    test_label "[eqz.refines-local-in-if.wat]";
      check_value exit_state
        (Variable.Var (Var.Local 2))
        (ValueSet RIC.zero)
    && check_value exit_state
        (Variable.Var (Var.Local 3))
        (ValueSet RIC.(join zero (constant 42l)))

  let%test "local.tee.non-singleton.wat" =
    let exit_state =
      "(module
        (memory (export \"mem\") 1)

        (func $main (export \"main\") (param $x i32) (result i32) (local $l1 i32)
          local.get $x
          if (result i32)
            i32.const 10
          else
            i32.const 20
          end

          local.tee $l1
        )
      )"
      |> analyze [0l] 0l
    in
    test_label "[local.tee.non-singleton.wat]";
      check_value exit_state
        (Variable.Var (Var.Local 1))
        (ValueSet (RIC.join (RIC.constant 10l) (RIC.constant 20l)))
    && check_value exit_state
        (Variable.Var (Var.Return (0l, 0l)))
        (ValueSet (RIC.join (RIC.constant 10l) (RIC.constant 20l)))

  let%test "global.set.get.non-singleton.wat" =
    let exit_state =
      "(module
        (memory (export \"mem\") 1)

        (global $g0 (mut i32) (i32.const 0))

        (func $main (export \"main\") (param $x i32) (result i32)
          local.get $x
          if (result i32)
            i32.const 10
          else
            i32.const 20
          end

          global.set $g0

          global.get $g0
        )
      )"
      |> analyze [0l] 0l
    in
    test_label "[global.set.get.non-singleton.wat]";
      check_value exit_state
        (Variable.Var (Var.Global 0))
        (ValueSet (RIC.join (RIC.constant 10l) (RIC.constant 20l)))
    && check_value exit_state
        (Variable.Var (Var.Return (0l, 0l)))
        (ValueSet (RIC.join (RIC.constant 10l) (RIC.constant 20l)))


  let%test "select.non-singleton-nonzero-condition.wat" =
    let exit_state =
      "(module
        (memory (export \"mem\") 1)

        (func $main (export \"main\") (param $x i32) (result i32) (local $cond i32)
          ;; cond = {1,2}
          local.get $x
          if (result i32)
            i32.const 1
          else
            i32.const 2
          end
          local.set $cond

          i32.const 10
          i32.const 20
          local.get $cond
          select
        )
      )"
      |> analyze [0l] 0l
    in
    test_label "[select.non-singleton-nonzero-condition.wat]";
    check_value exit_state
      (Variable.Var (Var.Return (0l, 0l)))
      (ValueSet (RIC.constant 10l))

  let%test "select.non-singleton-maybe-zero-condition.wat" =
    let exit_state =
      "(module
        (memory (export \"mem\") 1)

        (func $main (export \"main\") (param $x i32) (result i32) (local $cond i32)
          ;; cond = {0,2}
          local.get $x
          if (result i32)
            i32.const 0
          else
            i32.const 2
          end
          local.set $cond

          i32.const 10
          i32.const 20
          local.get $cond
          select
        )
      )"
      |> analyze [0l] 0l
    in
    test_label "[select.non-singleton-maybe-zero-condition.wat]";
    check_value exit_state
      (Variable.Var (Var.Return (0l, 0l)))
      (ValueSet RIC.(join (constant 10l) (constant 20l)))

  let%test "memory.store-load.singleton-address.wat" =
    let exit_state =
      "(module
        (memory (export \"mem\") 1)

        (func $main (export \"main\") (result i32)
          i32.const 4
          i32.const 42
          i32.store

          i32.const 4
          i32.load
        )
      )"
      |> analyze [0l] 0l
    in
    test_label "[memory.store-load.singleton-address.wat]";
    check_value exit_state
      (Variable.Var (Var.Return (0l, 0l)))
      (ValueSet (RIC.constant 42l))

  let%test "memory.load.unwritten-address.wat" =
    let exit_state =
      "(module
        (memory (export \"mem\") 1)

        (func $main (export \"main\") (result i32)
          i32.const 4
          i32.load
        )
      )"
      |> analyze [0l] 0l
    in
    test_label "[memory.load.unwritten-address.wat]";
    check_value exit_state
      (Variable.Var (Var.Return (0l, 0l)))
      (ValueSet Top)

  let%test "memory.store-overwrites-same-address.wat" =
    let exit_state =
      "(module
        (memory (export \"mem\") 1)

        (func $main (export \"main\") (result i32)
          i32.const 4
          i32.const 10
          i32.store

          i32.const 4
          i32.const 20
          i32.store

          i32.const 4
          i32.load
        )
      )"
      |> analyze [0l] 0l
    in
    test_label "[memory.store-overwrites-same-address.wat]";
    check_value exit_state
      (Variable.Var (Var.Return (0l, 0l)))
      (ValueSet (RIC.constant 20l))

  let%test "memory.store-keeps-distinct-address.wat" =
    let exit_state =
      "(module
        (memory (export \"mem\") 1)

        (func $main (export \"main\") (result i32)
          i32.const 4
          i32.const 10
          i32.store

          i32.const 8
          i32.const 20
          i32.store

          i32.const 4
          i32.load
        )
      )"
      |> analyze [0l] 0l
    in
    test_label "[memory.store-keeps-distinct-address.wat]";
    check_value exit_state
      (Variable.Var (Var.Return (0l, 0l)))
      (ValueSet (RIC.constant 10l))

  let%test "memory.store-load.non-singleton-value.wat" =
    let exit_state =
      "(module
        (memory (export \"mem\") 1)

        (func $main (export \"main\") (param $x i32) (result i32)
          i32.const 4
          local.get $x
          if (result i32)
            i32.const 10
          else
            i32.const 20
          end
          i32.store

          i32.const 4
          i32.load
        )
      )"
      |> analyze [0l] 0l
    in
    test_label "[memory.store-load.non-singleton-value.wat]";
    check_value exit_state
      (Variable.Var (Var.Return (0l, 0l)))
      (ValueSet (RIC.join (RIC.constant 10l) (RIC.constant 20l)))

  let%test "memory.store-load.with-offset.wat" =
    let exit_state =
      "(module
        (memory (export \"mem\") 1)

        (func $main (export \"main\") (result i32)
          i32.const 0
          i32.const 42
          i32.store offset=4

          i32.const 4
          i32.load
        )
      )"
      |> analyze [0l] 0l
    in
    test_label "[memory.store-load.with-offset.wat]";
    check_value exit_state
      (Variable.Var (Var.Return (0l, 0l)))
      (ValueSet (RIC.constant 42l))

  let%test "memory.store-load.with-load-offset.wat" =
    let exit_state =
      "(module
        (memory (export \"mem\") 1)

        (func $main (export \"main\") (result i32)
          i32.const 4
          i32.const 42
          i32.store

          i32.const 0
          i32.load offset=4
        )
      )"
      |> analyze [0l] 0l
    in
    test_label "[memory.store-load.with-load-offset.wat]";
    check_value exit_state
      (Variable.Var (Var.Return (0l, 0l)))
      (ValueSet (RIC.constant 42l))

  let%test "memory.store-load.non-singleton-address.wat" =
    let exit_state =
      "(module
        (memory (export \"mem\") 1)

        (func $main (export \"main\") (param $x i32) (result i32) (local $addr i32)
          local.get $x
          if (result i32)
            i32.const 4
          else
            i32.const 8
          end
          local.set $addr

          local.get $addr
          i32.const 42
          i32.store

          i32.const 4
          i32.load
        )
      )"
      |> analyze [0l] 0l
    in
    test_label "[memory.store-load.non-singleton-address.wat]";
    check_value exit_state
      (Variable.Var (Var.Return (0l, 0l)))
      (ValueSet Top)

  let%test "memory.weak-store-preserves-previous-value.wat" =
    let exit_state =
      "(module
        (memory (export \"mem\") 1)

        (func $main (export \"main\") (param $x i32) (result i32) (local $addr i32)
          i32.const 4
          i32.const 14
          i32.store

          local.get $x
          if (result i32)
            i32.const 4
          else
            i32.const 8
          end
          local.set $addr

          local.get $addr
          i32.const 42
          i32.store

          i32.const 4
          i32.load
        )
      )"
      |> analyze [0l] 0l
    in
    test_label "[memory.weak-store-preserves-previous-value.wat]";
    check_value exit_state
      (Variable.Var (Var.Return (0l, 0l)))
      (ValueSet RIC.(join (constant 14l) (constant 42l)))

  let%test "memory.overlapping-store-invalidates-previous-load.wat" =
    let exit_state =
      "(module
        (memory (export \"mem\") 1)

        (func $main (export \"main\") (result i32)
          i32.const 4
          i32.const 14
          i32.store

          i32.const 5
          i32.const 42
          i32.store

          i32.const 4
          i32.load
        )
      )"
      |> analyze [0l] 0l
    in
    test_label "[memory.overlapping-store-invalidates-previous-load.wat]";
    check_value exit_state
      (Variable.Var (Var.Return (0l, 0l)))
      (ValueSet Top)

  let%test "memory.store8-invalidates-i32-load.wat" =
    let exit_state =
      "(module
        (memory (export \"mem\") 1)

        (func $main (export \"main\") (result i32)
          i32.const 4
          i32.const 14
          i32.store

          i32.const 7
          i32.const 42
          i32.store8

          i32.const 4
          i32.load
        )
      )"
      |> analyze [0l] 0l
    in
    test_label "[memory.store8-invalidates-i32-load.wat]";
    check_value exit_state
      (Variable.Var (Var.Return (0l, 0l)))
      (ValueSet Top)

  let%test "memory.store8-disjoint-from-store.wat" =
    let exit_state =
      "(module
        (memory (export \"mem\") 1)

        (func $main (export \"main\") (result i32)
          i32.const 4
          i32.const 14
          i32.store

          i32.const 3
          i32.const 42
          i32.store8
          i32.const 8
          i32.const 42
          i32.store8

          i32.const 4
          i32.load
        )
      )"
      |> analyze [0l] 0l
    in
    test_label "[memory.store8-disjoint-from-store.wat]";
    check_value exit_state
      (Variable.Var (Var.Return (0l, 0l)))
      (ValueSet (RIC.constant 14l))

  let%test "memory.store16-invalidates-i32-load.wat" =
    let exit_state =
      "(module
        (memory (export \"mem\") 1)

        (func $main (export \"main\") (result i32)
          i32.const 4
          i32.const 14
          i32.store

          i32.const 3
          i32.const 42
          i32.store16

          i32.const 4
          i32.load
        )
      )"
      |> analyze [0l] 0l
    in
    test_label "[memory.store16-invalidates-i32-load.wat]";
    check_value exit_state
      (Variable.Var (Var.Return (0l, 0l)))
      (ValueSet Top)

  let%test "memory.store16-invalidates-i32-load.wat" =
    let exit_state =
      "(module
        (memory (export \"mem\") 1)

        (func $main (export \"main\") (result i32)
          i32.const 4
          i32.const 14
          i32.store

          i32.const 7
          i32.const 42
          i32.store16

          i32.const 4
          i32.load
        )
      )"
      |> analyze [0l] 0l
    in
    test_label "[memory.store16-invalidates-i32-load.wat]";
    check_value exit_state
      (Variable.Var (Var.Return (0l, 0l)))
      (ValueSet Top)

  let%test "memory.store16-disjoint.wat" =
    let exit_state =
      "(module
        (memory (export \"mem\") 1)

        (func $main (export \"main\") (result i32)
          i32.const 4
          i32.const 14
          i32.store

          i32.const 8
          i32.const 42
          i32.store16
          i32.const 2
          i32.const 42
          i32.store16

          i32.const 4
          i32.load
        )
      )"
      |> analyze [0l] 0l
    in
    test_label "[memory.store16-invalidates-i32-load.wat]";
    check_value exit_state
      (Variable.Var (Var.Return (0l, 0l)))
      (ValueSet (RIC.constant 14l))

  let%test "memory.load8-from-known-i32-is-top.wat" =
    let exit_state =
      "(module
        (memory (export \"mem\") 1)

        (func $main (export \"main\") (result i32)
          i32.const 4
          i32.const 14
          i32.store

          i32.const 4
          i32.load8_s
        )
      )"
      |> analyze [0l] 0l
    in
    test_label "[memory.load8-from-known-i32-is-top.wat]";
    check_value exit_state
      (Variable.Var (Var.Return (0l, 0l)))
      (ValueSet Top)

  let%test "memory.load16-from-known-i32-is-top.wat" =
    let exit_state =
      "(module
        (memory (export \"mem\") 1)

        (func $main (export \"main\") (result i32)
          i32.const 4
          i32.const 14
          i32.store

          i32.const 4
          i32.load16_s
        )
      )"
      |> analyze [0l] 0l
    in
    test_label "[memory.load16-from-known-i32-is-top.wat]";
    check_value exit_state
      (Variable.Var (Var.Return (0l, 0l)))
      (ValueSet Top)

  let%test "memory.store8-outside-i32-cell-keeps-load.wat" =
    let exit_state =
      "(module
        (memory (export \"mem\") 1)

        (func $main (export \"main\") (result i32)
          i32.const 4
          i32.const 14
          i32.store

          i32.const 8
          i32.const 42
          i32.store8

          i32.const 4
          i32.load
        )
      )"
      |> analyze [0l] 0l
    in
    test_label "[memory.store8-outside-i32-cell-keeps-load.wat]";
    check_value exit_state
      (Variable.Var (Var.Return (0l, 0l)))
      (ValueSet (RIC.constant 14l))

  let%test "memory.load.non-singleton-address-joins-known-cells.wat" =
    let exit_state =
      "(module
        (memory (export \"mem\") 1)

        (func $main (export \"main\") (param $x i32) (result i32) (local $addr i32)
          ;; mem[4] = 10
          i32.const 4
          i32.const 10
          i32.store

          ;; mem[8] = 20
          i32.const 8
          i32.const 20
          i32.store

          ;; addr = {4,8}
          local.get $x
          if (result i32)
            i32.const 4
          else
            i32.const 8
          end
          local.set $addr

          local.get $addr
          i32.load
        )
      )"
      |> analyze [0l] 0l
    in
    test_label "[memory.load.non-singleton-address-joins-known-cells.wat]";
    check_value exit_state
      (Variable.Var (Var.Return (0l, 0l)))
      (ValueSet RIC.(join (constant 10l) (constant 20l)))

  let%test "and.non-singleton-value.wat" =
    let exit_state =
      "(module
        (memory (export \"mem\") 1)

        (func $main (export \"main\") (param $x i32) (result i32) (local $l0 i32)
          local.get $x
          if (result i32)
            i32.const 10
          else
            i32.const 12
          end
          local.set $l0

          local.get $l0
          i32.const 3
          i32.and
        )
      )"
      |> analyze [0l] 0l
    in
    test_label "[and.non-singleton-value.wat]";
    check_value exit_state
      (Variable.Var (Var.Return (0l, 0l)))
      (ValueSet RIC.(join zero (constant 2l)))

  let%test "or.non-singleton-value.wat" =
    let exit_state =
      "(module
        (memory (export \"mem\") 1)

        (func $main (export \"main\") (param $x i32) (result i32) (local $l0 i32)
          local.get $x
          if (result i32)
            i32.const 8
          else
            i32.const 10
          end
          local.set $l0

          local.get $l0
          i32.const 3
          i32.or
        )
      )"
      |> analyze [0l] 0l
    in
    test_label "[or.non-singleton-value.wat]";
    check_value exit_state
      (Variable.Var (Var.Return (0l, 0l)))
      (ValueSet (RIC.constant 11l))

  (* let%test "xor.non-singleton-value.wat" =
    let exit_state =
      "(module
        (memory (export \"mem\") 1)

        (func $main (export \"main\") (param $x i32) (result i32) (local $l0 i32)
          local.get $x
          if (result i32)
            i32.const 10
          else
            i32.const 12
          end
          local.set $l0

          local.get $l0
          i32.const 3
          i32.xor
        )
      )"
      |> analyze [0l] 0l
    in
    test_label "[xor.non-singleton-value.wat]";
    check_value exit_state
      (Variable.Var (Var.Return (0l, 0l)))
      (ValueSet RIC.(join (constant 9l) (constant 15l))) *)

  (* let%test "shl.non-singleton-value.wat" =
    let exit_state =
      "(module
        (memory (export \"mem\") 1)

        (func $main (export \"main\") (param $x i32) (result i32) (local $l0 i32)
          ;; l0 = {3,5}
          local.get $x
          if (result i32)
            i32.const 3
          else
            i32.const 5
          end
          local.set $l0

          ;; 3 << 1 = 6
          ;; 5 << 1 = 10
          local.get $l0
          i32.const 1
          i32.shl
        )
      )"
      |> analyze [0l] 0l
    in
    test_label "[shl.non-singleton-value.wat]";
    check_value exit_state
      (Variable.Var (Var.Return (0l, 0l)))
      (ValueSet RIC.(join (constant 6l) (constant 10l))) *)

  let%test "shr_u.non-singleton-value.wat" =
    let exit_state =
      "(module
        (memory (export \"mem\") 1)

        (func $main (export \"main\") (param $x i32) (result i32) (local $l0 i32)
          ;; l0 = {8,12}
          local.get $x
          if (result i32)
            i32.const 8
          else
            i32.const 12
          end
          local.set $l0

          ;; 8 >>u 1 = 4
          ;; 12 >>u 1 = 6
          local.get $l0
          i32.const 1
          i32.shr_u
        )
      )"
      |> analyze [0l] 0l
    in
    test_label "[shr_u.non-singleton-value.wat]";
    check_value exit_state
      (Variable.Var (Var.Return (0l, 0l)))
      (ValueSet RIC.(join (constant 4l) (constant 6l)))

  let%test "shr_s.non-singleton-negative-value.wat" =
    let exit_state =
      "(module
        (memory (export \"mem\") 1)

        (func $main (export \"main\") (param $x i32) (result i32) (local $l0 i32)
          ;; l0 = {-8,-4}
          local.get $x
          if (result i32)
            i32.const -8
          else
            i32.const -4
          end
          local.set $l0

          ;; -8 >>s 1 = -4
          ;; -4 >>s 1 = -2
          local.get $l0
          i32.const 1
          i32.shr_s
        )
      )"
      |> analyze [0l] 0l
    in
    test_label "[shr_s.non-singleton-negative-value.wat]";
    check_value exit_state
      (Variable.Var (Var.Return (0l, 0l)))
      (ValueSet RIC.(join (constant (-4l)) (constant (-2l))))

  let%test "shift-left.relative-value.wat" =
    let exit_state =
      "(module
        (memory (export \"mem\") 1)

        (func $main (export \"main\") (param $x i32) (result i32)
          local.get $x
          i32.const 1
          i32.shl
        )
      )"
      |> analyze [0l] 0l
    in
    test_label "[shift-left.relative-value.wat]";
    check_value exit_state
      (Variable.Var (Var.Return (0l, 0l)))
      (ValueSet (RIC.ric (2l, NegInfinity, Infinity, ("", 0l))))

  let%test "shift-left.relative-value-by-two.wat" =
    let exit_state =
      "(module
        (memory (export \"mem\") 1)

        (func $main (export \"main\") (param $x i32) (result i32)
          local.get $x
          i32.const 2
          i32.shl
        )
      )"
      |> analyze [0l] 0l
    in
    test_label "[shift-left.relative-value-by-two.wat]";
    check_value exit_state
      (Variable.Var (Var.Return (0l, 0l)))
      (ValueSet (RIC.ric (4l, NegInfinity, Infinity, ("", 0l))))

  let%test "shift-left.relative-value-by-non-singleton.wat" =
    let exit_state =
      "(module
        (memory (export \"mem\") 1)

        (func $main (export \"main\") (param $x i32) (param $y i32) (result i32)
          local.get $x
          local.get $y
          i32.shl
        )
      )"
      |> analyze [0l] 0l
    in
    test_label "[shift-left.relative-value-by-non-singleton.wat]";
    check_value exit_state
      (Variable.Var (Var.Return (0l, 0l)))
      (ValueSet RIC.Top)

  let%test "shift-right-unsigned.relative-value.wat" =
    let exit_state =
      "(module
        (memory (export \"mem\") 1)

        (func $main (export \"main\") (param $x i32) (result i32)
          local.get $x
          i32.const 1
          i32.shr_u
        )
      )"
      |> analyze [0l] 0l
    in
    test_label "[shift-right-unsigned.relative-value.wat]";
    check_value exit_state
      (Variable.Var (Var.Return (0l, 0l)))
      (ValueSet RIC.positive_integers)

  let%test "shift-right-unsigned.relative-value>>30.wat" =
    let exit_state =
      "(module
        (memory (export \"mem\") 1)

        (func $main (export \"main\") (param $x i32) (result i32)
          local.get $x
          i32.const 30
          i32.shr_u
        )
      )"
      |> analyze [0l] 0l
    in
    test_label "[shift-right-unsigned.relative-value>>30.wat]";
    check_value exit_state
      (Variable.Var (Var.Return (0l, 0l)))
      (ValueSet RIC.(ric (1l, Int 0l, Int 3l, ("", 0l))))


  let%test "shift-right-unsigned.relative-value-by-two.wat" =
    let exit_state =
      "(module
        (memory (export \"mem\") 1)

        (func $main (export \"main\") (param $x i32) (result i32)
          local.get $x
          i32.const 2
          i32.shr_u
        )
      )"
      |> analyze [0l] 0l
    in
    test_label "[shift-right-unsigned.relative-value-by-two.wat]";
    check_value exit_state
      (Variable.Var (Var.Return (0l, 0l)))
      (ValueSet (RIC.ric (1l, Int 0l, Int 1073741823l, ("", 0l))))

  (* let%test "shift-right-signed.relative-value.wat" =
    let exit_state =
      "(module
        (memory (export \"mem\") 1)

        (func $main (export \"main\") (param $x i32) (result i32)
          local.get $x
          i32.const 1
          i32.shr_s
        )
      )"
      |> analyze [0l] 0l
    in
    test_label "[shift-right-signed.relative-value.wat]";
    check_value exit_state
      (Variable.Var (Var.Return (0l, 0l)))
      (ValueSet RIC.Top)

  let%test "shift-left-by-32-is-identity.wat" =
    let exit_state =
      "(module
        (memory (export \"mem\") 1)

        (func $main (export \"main\") (result i32)
          i32.const 42
          i32.const 32
          i32.shl
        )
      )"
      |> analyze [0l] 0l
    in
    test_label "[shift-left-by-32-is-identity.wat]";
    check_value exit_state
      (Variable.Var (Var.Return (0l, 0l)))
      (ValueSet (RIC.constant 42l)) *)

  let%test "shift-right-unsigned-by-32-is-identity.wat" =
    let exit_state =
      "(module
        (memory (export \"mem\") 1)

        (func $main (export \"main\") (result i32)
          i32.const 42
          i32.const 32
          i32.shr_u
        )
      )"
      |> analyze [0l] 0l
    in
    test_label "[shift-right-unsigned-by-32-is-identity.wat]";
    check_value exit_state
      (Variable.Var (Var.Return (0l, 0l)))
      (ValueSet (RIC.constant 42l))

  let%test "shift-left-by-33-is-shift-by-1.wat" =
    let exit_state =
      "(module
        (memory (export \"mem\") 1)

        (func $main (export \"main\") (result i32)
          i32.const 21
          i32.const 33
          i32.shl
        )
      )"
      |> analyze [0l] 0l
    in
    test_label "[shift-left-by-33-is-shift-by-1.wat]";
    check_value exit_state
      (Variable.Var (Var.Return (0l, 0l)))
      (ValueSet (RIC.constant 42l))

  let%test "shift-right-unsigned-by-33-is-shift-by-1.wat" =
    let exit_state =
      "(module
        (memory (export \"mem\") 1)

        (func $main (export \"main\") (result i32)
          i32.const 84
          i32.const 33
          i32.shr_u
        )
      )"
      |> analyze [0l] 0l
    in
    test_label "[shift-right-unsigned-by-33-is-shift-by-1.wat]";
    check_value exit_state
      (Variable.Var (Var.Return (0l, 0l)))
      (ValueSet (RIC.constant 42l))

  let%test "shift-right-signed-by-33-is-shift-by-1.wat" =
    let exit_state =
      "(module
        (memory (export \"mem\") 1)

        (func $main (export \"main\") (result i32)
          i32.const -84
          i32.const 33
          i32.shr_s
        )
      )"
      |> analyze [0l] 0l
    in
    test_label "[shift-right-signed-by-33-is-shift-by-1.wat]";
    check_value exit_state
      (Variable.Var (Var.Return (0l, 0l)))
      (ValueSet (RIC.constant (-42l)))

  let%test "shift-right-signed-negative-one.wat" =
    let exit_state =
      "(module
        (memory (export \"mem\") 1)

        (func $main (export \"main\") (result i32)
          i32.const -1
          i32.const 1
          i32.shr_s
        )
      )"
      |> analyze [0l] 0l
    in
    test_label "[shift-right-signed-negative-one.wat]";
    check_value exit_state
      (Variable.Var (Var.Return (0l, 0l)))
      (ValueSet (RIC.constant (-1l)))

  let%test "shift-right-unsigned-negative-one.wat" =
    let exit_state =
      "(module
        (memory (export \"mem\") 1)

        (func $main (export \"main\") (result i32)
          i32.const -1
          i32.const 1
          i32.shr_u
        )
      )"
      |> analyze [0l] 0l
    in
    test_label "[shift-right-unsigned-negative-one.wat]";
    check_value exit_state
      (Variable.Var (Var.Return (0l, 0l)))
      (ValueSet (RIC.constant 2147483647l))

  let%test "shift-right-signed-min-int.wat" =
    let exit_state =
      "(module
        (memory (export \"mem\") 1)

        (func $main (export \"main\") (result i32)
          i32.const -2147483648
          i32.const 1
          i32.shr_s
        )
      )"
      |> analyze [0l] 0l
    in
    test_label "[shift-right-signed-min-int.wat]";
    check_value exit_state
      (Variable.Var (Var.Return (0l, 0l)))
      (ValueSet (RIC.constant (-1073741824l)))

  let%test "memory.size.initial-page-count.wat" =
    let exit_state =
      "(module
        (memory (export \"mem\") 1)

        (func $main (export \"main\") (result i32)
          memory.size
        )
      )"
      |> analyze [0l] 0l
    in
    test_label "[memory.size.initial-page-count.wat]";
    check_value exit_state
      (Variable.Var (Var.Return (0l, 0l)))
      (ValueSet RIC.positive_integers)

  let%test "memory.grow.wat" =
    Value_set_options.show_intermediates := true;
    let exit_state =
      "(module
        (memory (export \"mem\") 1)

        (func $main (export \"main\") (result i32)
          i32.const 1
          memory.grow
        )
      )"
      |> analyze [0l] 0l
    in
    test_label "[memory.grow.wat]";
      check_value exit_state
        Variable.MemorySize
        (ValueSet RIC.(positive_integers + one))
    && check_value exit_state
        (Variable.Var (Var.Return (0l, 0l)))
        (ValueSet RIC.positive_integers)

  let%test "comparison-refinement-survives-arithmetic.wat" =
    Value_set_options.show_intermediates := false;
    let exit_state =
      "(module
        (memory (export \"mem\") 1)

        (func $main (export \"main\") (param $x i32) (result i32)
          (local $l0 i32)

          ;; l0 = {1,2}
          local.get $x
          if
            i32.const 1
            local.set $l0
          else
            i32.const 2
            local.set $l0
          end

          local.get $l0
          i32.const 1
          i32.eq
          if (result i32)
            ;; true branch: l0 = 1
            local.get $l0
            i32.const 10
            i32.add
          else
            ;; false branch: l0 = 2
            local.get $l0
            i32.const 20
            i32.add
          end
        )
      )"
      |> analyze [0l] 0l
    in
    test_label "[comparison-refinement-survives-arithmetic.wat]";
    check_value exit_state
      (Variable.Var (Var.Return (0l, 0l)))
      (ValueSet RIC.(join (constant 11l) (constant 22l)))

  let%test "comparison-refines-open-interval.wat" =
    let exit_state =
      "(module
        (memory (export \"mem\") 1)

        (func $main (export \"main\") (param $a i32) (param $b i32) (result i32)
          (local $x i32)

          ;; x = {-4,0,4} = 4[0,2]-4
          local.get $a
          if (result i32)
            i32.const -4
          else
            local.get $b
            if (result i32)
              i32.const 0
            else
              i32.const 4
            end
          end
          local.set $x

          ;; Keep only values satisfying -1 < x < 1.
          local.get $x
          i32.const -1
          i32.gt_s
          if (result i32)
            local.get $x
            i32.const 1
            i32.lt_s
            if (result i32)
              ;; Here x should be refined to 0.
              local.get $x
              i32.const 10
              i32.add
            else
              i32.const 100
            end
          else
            i32.const 100
          end
        )
      )"
      |> analyze [0l] 0l
    in
    test_label "[comparison-refines-open-interval.wat]";
    check_value exit_state
      (Variable.Var (Var.Return (0l, 0l)))
      (ValueSet RIC.(join (constant 10l) (constant 100l)))

  let%test "comparison-refines-open-interval-through-and.wat" =
    let exit_state =
      "(module
        (memory (export \"mem\") 1)

        (func $main (export \"main\") (param $a i32) (param $b i32) (result i32)
          (local $x i32)
          (local $gt_minus_one i32)
          (local $lt_one i32)

          ;; x = {-4,0,4} = 4[0,2]-4
          local.get $a
          if (result i32)
            i32.const -4
          else
            local.get $b
            if (result i32)
              i32.const 0
            else
              i32.const 4
            end
          end
          local.set $x

          ;; gt_minus_one = (-1 < x)
          i32.const -1
          local.get $x
          i32.lt_s
          local.set $gt_minus_one

          ;; lt_one = (x < 1)
          local.get $x
          i32.const 1
          i32.lt_s
          local.set $lt_one

          ;; (-1 < x) && (x < 1)
          local.get $gt_minus_one
          local.get $lt_one
          i32.and

          if (result i32)
            ;; Here x should be refined to 0.
            local.get $x
            i32.const 10
            i32.add
          else
            i32.const 100
          end
        )
      )"
      |> analyze [0l] 0l
    in
    test_label "[comparison-refines-open-interval-through-and.wat]";
    check_value exit_state
      (Variable.Var (Var.Return (0l, 0l)))
      (ValueSet RIC.(join (constant 10l) (constant 100l)))

  let%test "comparison-refines-outside-interval-through-or.wat" =
    let exit_state =
      "(module
        (memory (export \"mem\") 1)

        (func $main (export \"main\") (param $a i32) (param $b i32) (result i32)
          (local $x i32)
          (local $lt_minus_one i32)
          (local $gt_one i32)

          ;; x = {-4,0,4} = 4[0,2]-4
          local.get $a
          if (result i32)
            i32.const -4
          else
            local.get $b
            if (result i32)
              i32.const 0
            else
              i32.const 4
            end
          end
          local.set $x

          ;; lt_minus_one = x < -1
          local.get $x
          i32.const -1
          i32.lt_s
          local.set $lt_minus_one

          ;; gt_one = x > 1
          local.get $x
          i32.const 1
          i32.gt_s
          local.set $gt_one

          ;; (x < -1) || (x > 1)
          local.get $lt_minus_one
          local.get $gt_one
          i32.or

          if (result i32)
            ;; Here x should be refined to {-4,4}.
            local.get $x
            i32.const 10
            i32.add
          else
            ;; Here x should be refined to 0.
            i32.const 100
          end
        )
      )"
      |> analyze [0l] 0l
    in
    test_label "[comparison-refines-outside-interval-through-or.wat]";
    check_value exit_state
      (Variable.Var (Var.Return (0l, 0l)))
      (ValueSet RIC.(join (join (constant 6l) (constant 14l)) (constant 100l)))

  let%test "comparison-refines-through-xor.wat" =
    let exit_state =
      "(module
        (memory (export \"mem\") 1)

        (func $main (export \"main\") (param $a i32) (param $b i32) (result i32)
          (local $x i32)
          (local $lt_one i32)
          (local $gt_minus_one i32)

          ;; x = {-4,0,4}
          local.get $a
          if (result i32)
            i32.const -4
          else
            local.get $b
            if (result i32)
              i32.const 0
            else
              i32.const 4
            end
          end
          local.set $x

          ;; x < 1
          local.get $x
          i32.const 1
          i32.lt_s
          local.set $lt_one

          ;; x > -1
          local.get $x
          i32.const -1
          i32.gt_s
          local.set $gt_minus_one

          ;; Exactly one of these is true iff x = {-4,4}
          local.get $lt_one
          local.get $gt_minus_one
          i32.xor

          if (result i32)
            local.get $x
            i32.const 10
            i32.add
          else
            i32.const 100
          end
        )
      )"
      |> analyze [0l] 0l
    in
    test_label "[comparison-refines-through-xor.wat]";
    check_value exit_state
      (Variable.Var (Var.Return (0l, 0l)))
      (ValueSet RIC.(join (join (constant 6l) (constant 14l)) (constant 100l)))

  let%test "comparison-refines-through-eqz.wat" =
    let exit_state =
      "(module
        (memory (export \"mem\") 1)

        (func $main (export \"main\") (param $a i32) (param $b i32) (result i32)
          (local $x i32)
          (local $is_zero i32)

          ;; x = {-4,0,4}
          local.get $a
          if (result i32)
            i32.const -4
          else
            local.get $b
            if (result i32)
              i32.const 0
            else
              i32.const 4
            end
          end
          local.set $x

          ;; is_zero = (x == 0)
          local.get $x
          i32.const 0
          i32.eq
          local.set $is_zero

          ;; condition = not is_zero, so true branch keeps {-4,4}
          local.get $is_zero
          i32.eqz

          if (result i32)
            local.get $x
            i32.const 10
            i32.add
          else
            i32.const 100
          end
        )
      )"
      |> analyze [0l] 0l
    in
    test_label "[comparison-refines-through-eqz.wat]";
    check_value exit_state
      (Variable.Var (Var.Return (0l, 0l)))
      (ValueSet RIC.(join (join (constant 6l) (constant 14l)) (constant 100l)))

  let%test "comparison-refines-through-local-tee.wat" =
    let exit_state =
      "(module
        (memory (export \"mem\") 1)

        (func $main (export \"main\") (param $a i32) (param $b i32) (result i32)
          (local $x i32)
          (local $cond i32)

          ;; x = {-4,0,4}
          local.get $a
          if (result i32)
            i32.const -4
          else
            local.get $b
            if (result i32)
              i32.const 0
            else
              i32.const 4
            end
          end
          local.set $x

          ;; cond = (x == 0), using local.tee
          local.get $x
          i32.const 0
          i32.eq
          local.tee $cond

          if (result i32)
            ;; x should be refined to 0.
            local.get $x
            i32.const 10
            i32.add
          else
            i32.const 100
          end
        )
      )"
      |> analyze [0l] 0l
    in
    test_label "[comparison-refines-through-local-tee.wat]";
    check_value exit_state
      (Variable.Var (Var.Return (0l, 0l)))
      (ValueSet RIC.(join (constant 10l) (constant 100l)))

  let%test "f32_local.wat" =
    let exit_state =
      "(module
        (func $main (export \"main\") (result f32) (local $x f32)
          f32.const 3.1416
          local.set $x
          local.get $x
        )
      )"
      |> analyze [0l] 0l
    in
    test_label "[f32_local.wat]";
    check_value exit_state
      (Variable.Var (Var.Return (0l, 0l)))
      (ValueSet RIC.Top)

  let%test "param.wat" =
    let exit_state =
      "(module
        (func $main (export \"main\") (param $x i32) (result i32) (local $y i32)
          local.get $x
          local.set $y

          i32.const 14
          local.set $x

          local.get $y
          local.set $x

          local.get $x
        )
      )"
      |> analyze [0l] 0l
    in
    test_label "[unknown param.wat]";
    check_value exit_state
      (Variable.Var (Var.Return (0l, 0l)))
      (ValueSet RIC.(relative_ric "l0"))

  let%test "add_local_set.wat" =
    let exit_state =
      "(module
        (func $main (export \"main\") (result i32) (local $x i32)
          i32.const 14
          i32.const 16
          i32.add
          local.set $x
          local.get $x
        )
      )"
      |> analyze [0l] 0l
    in
    test_label "[add_local_set.wat]";
    check_value exit_state
      (Variable.Var (Var.Return (0l, 0l)))
      (ValueSet RIC.(constant 30l))

  let%test "memory.store large addresses.wat" =
    let exit_state =
      "(module
        (memory (export \"mem\") 65536)

        (func $main (export \"main\") 
          i32.const 0x7FFFFFFF
          i32.const 14
          i32.store
          i32.const 0x7FFFFFFF
          i32.load
          drop

          i32.const 0x7FFFFFFE
          i32.const 26
          i32.store
          i32.const 0x80000000
          i32.const 42
          i32.store

          i32.const 0x7FFFFFFF
          i32.load
          drop
        )
      )"
      |> analyze [0l] 0l
    in
    test_label "[memory.store-large-addresses.wat]";
    check_value exit_state
      (i_var 0l 4)
      (ValueSet (RIC.constant 14l))
    && check_value exit_state
      (i_var 0l 13)
      (ValueSet RIC.Top)

  let%test "count leading zeros.wat" =
    let exit_state =
      "(module
        (func $main (export \"main\") (param $x i32) (result i32)
          local.get $x
          if (result i32)
            i32.const 10
          else
            i32.const 2
          end
          i32.clz
        )
      )"
      |> analyze [0l] 0l
    in
    test_label "[count leading zeros.wat]";
    check_value exit_state
      (Variable.Var (Var.Return (0l, 0l)))
      (ValueSet (RIC.ric (2l, Int 0l, Int 1l, ("", 28l))))

  let%test "count trailing zeros.wat" =
    let exit_state =
      "(module
        (func $main (export \"main\") (param $x i32) (result i32)
          local.get $x
          if (result i32)
            i32.const 8
          else
            i32.const 10
          end
          i32.ctz
        )
      )"
      |> analyze [0l] 0l
    in
    test_label "[count trailing zeros.wat]";
    check_value exit_state
      (Variable.Var (Var.Return (0l, 0l)))
      (ValueSet (RIC.ric (2l, Int 0l, Int 1l, ("", 1l))))

  let%test "population count.wat" =
    let exit_state =
      "(module
        (func $main (export \"main\") (param $x i32) (result i32)
          local.get $x
          if (result i32)
            i32.const 8
          else
            i32.const 10
          end
          i32.popcnt
        )
      )"
      |> analyze [0l] 0l
    in
    test_label "[population count.wat]";
    check_value exit_state
      (Variable.Var (Var.Return (0l, 0l)))
      (ValueSet (RIC.ric (1l, Int 0l, Int 1l, ("", 1l))))

  let%test "memory.copy.wat" =
    let exit_state =
      "(module
        (memory (export \"mem\") 1)

        ;; Copy 12 bytes from address 0 to address 32.
        (func (export \"main\") (result i32)
          ;; Initialize memory[0..3]
          i32.const 0
          i32.const 42
          i32.store

          i32.const 4
          i32.const 43
          i32.store

          i32.const 8
          i32.const 44
          i32.store

          i32.const 32
          i32.const 45
          i32.store

          ;; memory.copy(dest=8, src=0, len=4)
          i32.const 32
          i32.const 0
          i32.const 12
          memory.copy

          ;; load what's been copied
          i32.const 32
          i32.load
        )
      )"
      |> analyze [0l] 0l
    in
    test_label "[memory.copy.wat]";
    check_value exit_state
      (Variable.Var (Var.Return (0l, 0l)))
      (ValueSet RIC.Top)

  let%test "store.records.store_operations.wat" =
    let exit_state =
      "(module
        (memory (export \"mem\") 1)

        (func $main (export \"main\") (result i32)
          i32.const 0
          i32.const 42
          i32.store

          i32.const 0
        )
      )"
      |> analyze [0l] 0l
    in
    test_label "[store.records.store_operations.wat]";
    let actual = Domain.store_operations_to_string exit_state in
    let passed = String.(actual = "-3; -2; -1; 0; 1; 2; 3") in
    Printf.printf "\tstore_operations: %s\n" actual;
    passed


  let%test "i32.load" =
    let exit_state =
      "(module
        (memory (export \"mem\") 1)

        (func $main (export \"main\") (param $x i32) (result i32)
          i32.const 0
          i32.const 14
          i32.store

          i32.const 4
          local.get $x
          i32.eqz
          i32.store

          i32.const 8
          i32.const 67
          i32.store

          local.get $x
          if (result i32)
            i32.const 0
          else
            i32.const 4
          end
          i32.load
        )
      )"
      |> analyze [0l] 0l
    in
    test_label "[i32.load]";
    check_value exit_state
      (Variable.Var (Var.Return (0l, 0l)))
      (ValueSet RIC.(ric (1l, Int 0l, Int 14l, ("", 0l))))

  let%test "i32.load->Top" =
    let exit_state =
      "(module
        (memory (export \"mem\") 1)

        (func $main (export \"main\") (param $x i32) (result i32)
          i32.const 0
          i32.const 14
          i32.store

          i32.const 4
          local.get $x
          i32.eqz
          i32.store

          local.get $x
          if (result i32)
            i32.const 0
          else
            i32.const 5
          end
          i32.load
        )
      )"
      |> analyze [0l] 0l
    in
    test_label "[i32.load->Top]";
    check_value exit_state
      (Variable.Var (Var.Return (0l, 0l)))
      (ValueSet RIC.Top)

  let%test "unreachable branch" =
    let exit_state =
      "(module
        (memory (export \"mem\") 1)

        (func $main (export \"main\") (param $x i32) (result i32)
          i32.const 0
          i32.eqz
          if (result i32)
            i32.const 14
          else
            i32.const 42
          end
        )
      )"
      |> analyze [0l] 0l
    in
    test_label "[unreachable branch]";
    check_value exit_state
      (Variable.Var (Var.Return (0l, 0l)))
      (ValueSet RIC.(constant 14l))

  let%test "unknown eqz" =
    let exit_state =
      "(module
        (memory (export \"mem\") 1)

        (func $main (export \"main\") (param $x i32) (result i32)
            local.get $x
            i32.eqz
        )
      )"
      |> analyze [0l] 0l
    in
    test_label "[unknown eqz]";
    check_value exit_state
      (Variable.Var (Var.Return (0l, 0l)))
      (Boolean {numeric_value=RIC.(join one zero); 
                true_or_false = Variable.Map.empty |> Variable.Map.set ~key:(Variable.Var (Var.Local 0)) ~data:{Boolean.True_or_false.true_=RIC.zero; false_=RIC.(relative_ric "l0")} })

  let%test "boolean join" =
    let exit_state =
      "(module
        (memory (export \"mem\") 1)

        (func $main (export \"main\") (param $y i32) (result i32) (local $x i32)
          local.get $y
          if 
            i32.const 14
            local.set $x
          else
            i32.const 0
            local.set $x
          end

          local.get $y
          if (result i32)
            local.get $x
            i32.eqz
          else
            local.get $x
            if (result i32)
              i32.const 26
            else
              i32.const 3
            end
            local.get $x
            i32.lt_s
          end
        )
      )"
      |> analyze [0l] 0l
    in
    test_label "[boolean join]";
    check_value exit_state
      (Variable.Var (Var.Return (0l, 0l)))
      (ValueSet RIC.(ric (1l, Int 0l, Int 1l, ("", 0l))))

  let%test "function_call.wat" =
    let exit_state =
      "(module
        (memory (export \"mem\") 1)
        (global $g (mut i32) (i32.const 0))

        ;; Function that increments g by 166, stores 14 at address g, and adds 42 to its argument
        (func $add42 (param $x i32) (result i32)
          ;; Increment global g by 166
          global.get $g
          i32.const 166
          i32.add
          global.set $g

          ;; store 14 at address 14
          ;; i32.const 42
          global.get $g
          i32.const 14
          i32.store

          ;; Add 42 to the argument and return
          local.get $x
          i32.const 42
          i32.add
          return)

        ;; Main function that increments global g and calls add42(10)
        (func $main (result i32)
          global.get $g
          i32.const 99
          i32.store

          ;; Call add42 with 10
          i32.const 10
          call $add42
          return)

        ;; Export the main function
        (export \"main\" (func $main))
      )"
      |> analyze_inter' ~fct_idx:1l
    in
    test_label "[function_call.wat]";
    check_value exit_state
      (Variable.Mem RIC.(ric (0l, Int 0l, Int 0l, ("g0", 166l))))
      (ValueSet RIC.(constant 14l))
    && check_value exit_state
      (Variable.Var (Var.Return (1l, 0l)))
      (ValueSet RIC.(constant 52l))
    && check_value exit_state
      (Variable.Var (Var.Global 0))
      (ValueSet RIC.(constant 166l + relative_ric "g0"))

  let%test "mutually_recursive_functions.wat" =
    let exit_state =
      "(module
        (memory (export \"mem\") 1)
        (global $g (mut i32) (i32.const 0))

        (func $is_even (param $x i32) (result i32)
          local.get $x
          i32.eqz
          if (result i32)
            i32.const 1
          else
            local.get $x
            i32.const 1
            i32.sub
            call $is_odd
          end
        )

        (func $is_odd (param $x i32) (result i32)
          local.get $x
          i32.eqz
          if (result i32)
            i32.const 0
          else
            local.get $x
            i32.const 1
            i32.sub
            call $is_even
          end
        )

        (func $main (result i32)
          i32.const 14
          call $is_even)

        ;; Export the main function
        (export \"main\" (func $main))
      )"
      |> analyze_inter' ~fct_idx:2l
    in
    test_label "[mutually recursive functions.wat]";
    check_value exit_state
      (Variable.Var (Var.Return (2l, 0l)))
      (ValueSet RIC.(join zero one))


  let%test "imported.wat" =
    let exit_state =
      "(module
        (import \"env\" \"random_function\"
          (func $random_function (param i32) (result i32)))

        (import \"env\" \"fd_close\"
          (func $fd_close (param i32) (result i32)))

        (memory (export \"mem\") 1)
        (global $g (mut i32) (i32.const 0))

        (func (export \"main\") (result i32)
          i32.const 42
          global.set $g

          i32.const 14
          global.get $g
          i32.store
          i32.const 14
          i32.load       ;; this value should be 42

          call $fd_close
          drop
          i32.const 14
          i32.load       ;; this value should be 42
          drop
          i32.const 14
          global.get $g
          i32.store
          i32.const 14
          i32.load       ;; this value should be 42

          call $random_function
          drop
          i32.const 14
          i32.load       ;; this value should be Top
          drop
          i32.const 14
          global.get $g
          i32.store
          i32.const 14
          i32.load       ;; this value should be Top
        )
      )"
      |> analyze_inter' ~fct_idx:2l
    in
    test_label "[imported.wat]";
    check_value exit_state
      (i_var 2l 6)
      (ValueSet RIC.(constant 42l))
    && check_value exit_state
      (i_var 2l 10)
      (ValueSet RIC.(constant 42l))
    && check_value exit_state
      (i_var 2l 16)
      (ValueSet RIC.(constant 42l))
    && check_value exit_state
      (i_var 2l 20)
      (ValueSet RIC.Top)
    && check_value exit_state
      (i_var 2l 26)
      (ValueSet RIC.Top)

  
    

  (* Add next test here *)

  (* let%test "keep false when working" = false *)

end)