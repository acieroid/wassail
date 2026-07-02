open Core 
open Helpers

module RIC = Reduced_interval_congruence.RIC
module RICSet = Reduced_interval_congruence.RICSet
module ValueSet = Value_set_abstraction
module AbstractStore = Abstract_store_domain

type pointer_analysis = Value_set.pointer_analysis

let is_unreachable = Memory_deps.is_unreachable
let functions_potentially_called = Memory_deps.functions_potentially_called


(** Dependencies involving global variables and function calls.

    This module computes additional slicing dependencies that are not captured by
    the standard use-def chains alone. In particular, it accounts for two cases:

    - a [global.get] may depend on an earlier [call] if the called function may
      modify the global variable being read;
    - a [call] may depend on earlier [global.set] instructions whose definitions
      may be read by the called function. *)

(** Map from an instruction label to the set of instruction labels it depends on.

    A binding [l -> deps] means that the instruction labelled [l] must keep all
    instructions labelled by elements of [deps] in the slice. *)
type t = Instr.Label.Set.t Instr.Label.Map.t

(** Returns whether function [fct_index] may affect [global_var].

    When no pointer-analysis result is available, the answer is conservative:
    the function is assumed to potentially affect the global variable.

    When pointer-analysis summaries are available, the summary of the called
    function is inspected. The function is considered to affect [global_var]
    whenever the abstract value associated with this global after the call differs
    from the abstract value representing the unchanged initial global variable.

    This predicate is used when deciding whether a [global.get] should depend on
    a preceding call. *)
let function_may_affect_global_variable
    (pointer_analysis : pointer_analysis option)
    ~(global_var : Var.t)
    ~(fct_index : int32)
  : bool =
  pointer_analysis
  |>Option.value_map
    ~default:true
    ~f:(fun (_, _, summaries) ->
      let fct_summary = Int32Map.find_exn summaries fct_index in
      let global_value_after_call = AbstractStore.get fct_summary ~var:(Variable.Var global_var) in
      ValueSet.(global_value_after_call <> ValueSet (RIC.relative_ric (Var.to_string global_var))))

(** Computes dependencies from [global.get] instructions to preceding calls.

    For each [(global_get_label, global_idx)] in [global_gets], this function
    looks at the predecessor blocks of the block containing [global_get_label].
    If a predecessor block contains a direct call to a function that may affect
    the global variable [global_idx], then the [global.get] is made dependent on
    that call.

    The may-affect test is performed by {!function_may_affect_global_variable}.
    Without pointer-analysis information, this test is conservative and all
    preceding direct calls are considered possible dependencies.

    The result maps each [global.get] label to the set of call labels it depends
    on. *)
let globals_depend_on_calls
    (module_ : Wasm_module.t)
    (global_gets : (Instr.Label.t * int32) list)
    (cfg : Spec_domain.t Cfg.t)
    (pointer_analysis : pointer_analysis option)
  : t =
  List.fold global_gets 
        ~init:Instr.Label.Map.empty
        ~f:(fun dependencies (global_var_label, idx) ->
            let global_var = Var.Global (Int32.to_int_exn idx) in
            let block = Cfg.find_enclosing_block_exn cfg global_var_label in
            let predecessors = Cfg.all_predecessors cfg block in
            let deps = 
              List.fold predecessors ~init:Instr.Label.Set.empty
                ~f:(fun acc block ->
                        match block.content with
                        | Call { label = call_label; instr = CallDirect (_, _, fct_index); _ } ->
                          begin match pointer_analysis with
                          | None -> Instr.Label.Set.add acc call_label
                          | Some (cfg, _, _) ->
                            if is_unreachable cfg call_label then
                              (Log.info (fun () -> Printf.sprintf "unreachable call %s does not affect global variable %s"
                                                    (Instr.Label.to_string call_label) (Var.to_string global_var));
                              acc)
                            else if function_may_affect_global_variable pointer_analysis ~global_var ~fct_index then
                              (Log.info (fun () -> Printf.sprintf "fct call %s may affect global variable %s" 
                                                    (Instr.Label.to_string call_label) (Var.to_string global_var));
                              Instr.Label.Set.add acc call_label)
                            else
                              (Log.info (fun () -> Printf.sprintf "fct call %s does not affect global variable %s" 
                                                    (Instr.Label.to_string call_label) (Var.to_string global_var));
                              acc)
                          end
                        | Call { label = indirect_label; instr = CallIndirect (_, _, _, type_index); _ } ->
                          begin match pointer_analysis with
                          | None -> Instr.Label.Set.add acc indirect_label
                          | Some (cfg_pointers, cfg_spec, _) ->
                            if is_unreachable cfg_pointers indirect_label then
                              (Log.info (fun () -> Printf.sprintf "unreachable call_indirect %s does not affect global variable %s"
                                                    (Instr.Label.to_string indirect_label) (Var.to_string global_var));
                              acc)
                            else
                              functions_potentially_called
                                ~module_
                                ~indirect_label
                                ~type_index
                                ~cfg_pointers
                                ~cfg_spec
                              |> List.fold ~init:acc
                                ~f:(fun acc fct_index ->
                                  if Set.is_empty acc && function_may_affect_global_variable pointer_analysis ~global_var ~fct_index then
                                    (Log.info (fun () -> Printf.sprintf "call_indirect %s may affect global variable %s" 
                                                            (Instr.Label.to_string indirect_label) (Var.to_string global_var));
                                    Instr.Label.Set.add acc indirect_label)
                                  else
                                    acc)
                                  end
                        | _ -> acc)
            in
            Instr.Label.Map.update dependencies global_var_label
              ~f:(function
                  | None -> deps
                  | Some set -> Instr.Label.Set.union set deps))

(** Computes dependencies from call instructions to preceding global definitions.

    For each direct call, this function uses the global-read analysis to find
    which [global.set] instructions may define globals read by the called
    function. It then searches the predecessor blocks of the call and adds
    dependencies on matching earlier instructions.

    If the global-read information is [Top], the analysis is conservative and
    the call is made dependent on every instruction found in the relevant
    predecessor data blocks.

    If the global-read information is [NotTop globals_used], only predecessor
    instructions whose labels occur in [globals_used] are added as dependencies.

    The result maps each call label to the set of instruction labels it depends
    on because of global-variable reads performed by the callee. *)
let calls_depend_on_globals
    (global_deps : Global_read_domain.t Int32Map.t)
    (call_instructions : (Instr.Label.t * int32) list)
    (cfg : Spec_domain.t Cfg.t)
  : t =
  List.fold call_instructions
    ~init:Instr.Label.Map.empty
    ~f:(fun dependencies (call_label, idx) ->
        let globals_used_by_function = 
          match Int32Map.find global_deps idx with
          | Some globals -> globals
          | None -> Top
        in
        let block = Cfg.find_enclosing_block_exn cfg call_label in
        let predecessors = Cfg.all_predecessors cfg block in
        let instr_set =
          List.fold predecessors 
            ~init:Instr.Label.Set.empty 
            ~f:(fun acc block ->
                    match block.content with
                    | Data instrs' -> 
                      List.fold instrs' ~init:acc ~f:(fun acc instr ->
                        match globals_used_by_function with
                        | Top ->
                          begin match instr.instr with
                          | GlobalSet _ -> Instr.Label.Set.add acc instr.label
                          | _ -> acc
                          end
                        | NotTop globals_used ->
                          if Global_read_domain.GlobalInstruction.Set.mem_label globals_used instr.label then
                            Instr.Label.Set.add acc instr.label
                          else
                            acc)
                    | _ -> acc)
        in
        Instr.Label.Map.update dependencies call_label
          ~f:(function 
              | None -> instr_set 
              | Some set -> Instr.Label.Set.union set instr_set))


let indirect_calls_depend_on_globals
    (module_ : Wasm_module.t)
    (pointer_analysis : Value_set.pointer_analysis option)
    (global_deps : Global_read_domain.t Int32Map.t)
    (call_indirect_instructions : (Instr.Label.t * int32) list)
    (cfg : Spec_domain.t Cfg.t)
  : t =
  List.fold call_indirect_instructions
    ~init:Instr.Label.Map.empty
    ~f:(fun dependencies (indirect_label, type_index) ->
        (match pointer_analysis with
        | Some (cfg_pointers, cfg_spec, _) ->
          Memory_deps.functions_potentially_called
            ~module_
            ~indirect_label
            ~type_index
            ~cfg_pointers
            ~cfg_spec
        | None -> 
          Call_graph.indirect_call_targets module_ type_index)
        |> List.fold
          ~init:dependencies
          ~f:(fun dependencies idx ->
            let globals_used_by_function = 
              match Int32Map.find global_deps idx with
              | Some globals -> globals
              | None -> Top
            in
            let block = Cfg.find_enclosing_block_exn cfg indirect_label in
            let predecessors = Cfg.all_predecessors cfg block in
            let instr_set =
              List.fold predecessors ~init:Instr.Label.Set.empty 
                ~f:(fun acc block ->
                        match block.content with
                        | Data instrs' -> 
                          List.fold instrs' ~init:acc ~f:(fun acc instr ->
                            match globals_used_by_function with
                            | Top ->
                              begin match instr.instr with
                              | GlobalSet _ -> Instr.Label.Set.add acc instr.label
                              | _ -> acc
                              end
                            | NotTop globals_used ->
                              if Global_read_domain.GlobalInstruction.Set.mem_label globals_used instr.label then
                                Instr.Label.Set.add acc instr.label
                              else
                                acc)
                        | _ -> acc)
            in
            Instr.Label.Map.update dependencies indirect_label
              ~f:(function 
                  | None -> instr_set 
                  | Some set -> Instr.Label.Set.union set instr_set)))


(** Returns all [global.get] instructions in the current function.

    Each returned pair [(label, global_idx)] contains the label of the
    [global.get] instruction and the index of the global variable it reads. *)
let find_global_get_instructions
    (cfg_instructions : Spec_domain.t Instr.t Instr.Label.Map.t) 
  : (Instr.Label.t * int32) list =
  Instr.Label.Map.to_alist cfg_instructions
  |> List.filter_map ~f:(fun (label, instr) ->
      match instr with
      | Data { instr = GlobalGet global_idx; _ } -> Some (label, global_idx)
      | _ -> None)

(** Returns all direct call instructions in the current function.

    Each returned pair [(label, func_idx)] contains the label of the call
    instruction and the index of the called function.

    Indirect calls are currently ignored. *)
let find_call_instructions
    (cfg_instructions : Spec_domain.t Instr.t Instr.Label.Map.t)
  : (Instr.Label.t * int32) list =
  Instr.Label.Map.to_alist cfg_instructions
  |> List.filter_map ~f:(fun (label, instr) ->
      match instr with
      | Call { instr = CallDirect (_, _, func_idx); _ } -> Some (label, func_idx)
      | _ -> None)


let find_call_indirect_instructions
    (cfg_instructions : Spec_domain.t Instr.t Instr.Label.Map.t)
  : (Instr.Label.t * int32) list =
  Instr.Label.Map.to_alist cfg_instructions
  |> List.filter_map ~f:(fun (label, instr) ->
      match instr with
      | Call { instr = CallIndirect (_, _, _, type_index); _ } -> Some (label, type_index)
      | _ -> None)

(** Computes all additional global-related dependencies for the current function.

    This combines dependencies in both directions:

    - [global.get] instructions depending on earlier calls that may modify the
      global variable being read;
    - call instructions depending on earlier [global.set] instructions whose
      definitions may be read by the callee.

    [global_deps] contains the global-read summaries of functions in the module.
    [cfg] is the current function CFG, and [cfg_instructions] is the map used to
    discover the relevant [global.get] and direct [call] instructions. Optional
    [pointer_analysis] results are used to decide whether calls may affect a
    particular global variable; when absent, the analysis falls back to a
    conservative answer.

    The two dependency maps are merged by taking the union of dependency sets
    when both analyses produce dependencies for the same instruction label. *)
let global_dependencies 
    ~(module_ : Wasm_module.t)
    ~(global_deps : Global_read_domain.t Int32Map.t)
    ~(cfg : Spec_domain.t Cfg.t)
    ~(cfg_instructions : Spec_domain.t Instr.t Instr.Label.Map.t)
    ~(pointer_analysis : Value_set.pointer_analysis option)
  : t =
  let global_gets = find_global_get_instructions cfg_instructions in
  let call_instructions = find_call_instructions cfg_instructions in
  let call_indirect_instructions = find_call_indirect_instructions cfg_instructions in
  let global_call = globals_depend_on_calls module_ global_gets cfg pointer_analysis in
  let call_global = calls_depend_on_globals global_deps call_instructions cfg in
  let call_indirect_global = indirect_calls_depend_on_globals module_ pointer_analysis global_deps call_indirect_instructions cfg in
  Instr.Label.Map.merge global_call call_global
    ~f:(fun ~key:_ -> function 
                      | `Both (a, b) -> Some (Instr.Label.Set.union a b)
                      | `Left x -> Some x
                      | `Right x -> Some x)
  |> Instr.Label.Map.merge call_indirect_global
    ~f:(fun ~key:_ -> function 
                      | `Both (a, b) -> Some (Instr.Label.Set.union a b)
                      | `Left x -> Some x
                      | `Right x -> Some x)