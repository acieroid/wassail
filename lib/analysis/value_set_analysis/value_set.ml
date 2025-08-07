open Core
open Helpers

module Options = Value_set_options
module Domain = Abstract_store_domain
(* module RIC = Reduced_interval_congruence.RIC *)
module Transfer = Value_set_transfer.Make
module Summary = Value_set_summary
module Intra = Intra.Make(Transfer)
module Inter = Inter.Make(Intra)

let analyze_intra : Wasm_module.t -> Int32.t list -> (Summary.t * Domain.t Cfg.t option) Int32Map.t =
  Analysis_helpers.mk_intra
    (fun cfgs wasm_mod ->
      (Int32Map.map ~f:(fun x -> (x, None)) (Summary.initial_summaries cfgs wasm_mod `Bottom)))
    (fun data wasm_mod cfg ->
      Log.info
        (Printf.sprintf "---------- Value-set analysis of function %s ----------" (Int32.to_string cfg.idx));
      (* Run the value-set analysis *)
      let annotated_cfg = (* Relational.Transfer.dummy_annotate  *) cfg in
      let summaries = Int32Map.map data ~f:fst in
      let (result_cfg, value_set_summary) = Intra.analyze wasm_mod annotated_cfg summaries in
      (value_set_summary, Some result_cfg))

let annotate (wasm_mod : Wasm_module.t) (summaries : Summary.t Int32Map.t) (spec_cfg : Spec.t Cfg.t) : Domain.t Cfg.t =
  let rel_cfg = (* Relational.Transfer.dummy_annotate *) spec_cfg in
  fst (Intra.analyze wasm_mod rel_cfg summaries)

let analyse_inter : Wasm_module.t -> Int32.t list list -> (Spec.t Cfg.t * Abstract_store_domain.t Cfg.t * Summary.t) Int32Map.t =
  Analysis_helpers.mk_inter
    (fun _cfgs _wasm_mod -> Int32Map.empty)
    (fun wasm_mod scc cfgs_and_summaries ->
      Log.info
        (Printf.sprintf "---------- Value-set analysis of SCC {%s} ----------"
          (String.concat ~sep:", " (List.map (Int32Map.keys scc) ~f:Int32.to_string)));
      (* Run the value-set analysis *)
      let annotated_scc = scc in
      let summaries = Int32Map.mapi cfgs_and_summaries ~f:(fun ~key:_idx ~data:(_spec_cfg, _value_set_cfg, summary) -> summary) in
      let summaries' = List.fold_left wasm_mod.imported_funcs
          ~init:summaries
          ~f:(fun summaries (idx, name, (args, ret)) ->
              Int32Map.set summaries ~key:idx ~data:(Summary.of_import name wasm_mod.nglobals args ret)) in
      let _ =
        let oc = Out_channel.create ~append:true "store_types.txt" in
        Out_channel.close oc
      in
      let results = Inter.analyze wasm_mod annotated_scc summaries' in
      Int32Map.mapi results ~f:(fun ~key:idx ~data:(value_set_cfg, summary) ->
          let spec_cfg = Int32Map.find_exn scc idx in
          (spec_cfg, value_set_cfg, summary)))

module Test = struct
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
end


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