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

  let i_var (f : int32) (id : int) : Variable.t = Variable.Var (Var {section = Function f; id})

  let check_value (state : Domain.t) (var : Variable.t) (value : Domain.Value.t) : bool =
    state
    |> Domain.get ~var
    |> Domain.Value.equal value
    || (Printf.printf "\tFailure: %s\n" (Domain.to_string state); false)


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
    && check_value exit_state (Variable.Var (Var.Return 0l)) (ValueSet (RIC.constant 14l))


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
    && check_value exit_state (Variable.Var (Var.Return 0l)) (ValueSet (RIC.constant 14l))

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
    && check_value exit_state (Variable.Var (Var.Return 0l)) (ValueSet (RIC.constant 42l))

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
    check_value exit_state (Variable.Var (Var.Return 0l)) (ValueSet (RIC.relative_ric "g0"))


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
    && check_value exit_state (Variable.Var (Var.Return 0l)) (ValueSet RIC.zero)

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
    && check_value exit_state (Variable.Var (Var.Return 0l)) (ValueSet (RIC.constant 20l))

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
      (Variable.Var (Var.Return 0l))
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
      (Variable.Var (Var.Return 0l))
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
      (Variable.Var (Var.Return 0l))
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
      (Variable.Var (Var.Return 0l))
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
      (Variable.Var (Var.Return 0l))
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
      (Variable.Var (Var.Return 0l))
      (ValueSet (RIC.join (RIC.constant 10l) (RIC.constant 20l)))

  let%test "keep false when working" = false

end)