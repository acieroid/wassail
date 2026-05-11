open Core 
open Helpers


(** Dependencies involving global variables and function calls.

    This module computes additional slicing dependencies that are not captured by
    the standard use-def chains alone. In particular, it accounts for two cases:

    - a [global.get] may depend on an earlier [call] if the called function may
      modify the global variable being read;
    - a [call] may depend on earlier instructions that define global variables
      read by the called function.

    The resulting map associates each instruction label with the labels of the
    instructions it additionally depends on. *)

(** Map from an instruction label to the set of instruction labels it depends on. *)
type t = Instr.Label.Set.t Instr.Label.Map.t

(** Return whether the function [fct_index] may affect [global_var].

    When no pointer analysis result is available, the answer is conservative:
    the function is assumed to potentially affect the global variable.

    When pointer analysis summaries are available, the summary of the called
    function is inspected. The function is considered to affect [global_var]
    whenever the value associated with this global after the call differs from
    the abstract value representing the unchanged initial global variable. *)
let function_may_affect_global_variable
    (pointer_analysis : (Value_set.Domain.t Cfg.t * Spec_domain.t Instr.t Instr.Label.Map.t * Value_set.Domain.t Int32Map.t) option)
    ~(global_var : Var.t)
    ~(fct_index : int32)
  : bool =
  match pointer_analysis with
  | None -> true
  | Some (_, _, summaries) ->
    let fct_summary = 
      Int32Map.find_exn summaries fct_index in
    let global_value_after_call = 
      Abstract_store_domain.get fct_summary ~var:(Variable.Var global_var) in
    not (Value_set_abstractions.equal global_value_after_call (ValueSet (Reduced_interval_congruence.RIC.ric (0l, Int 0l, Int 0l, (Var.to_string global_var,0l)))))

(** Compute dependencies from [global.get] instructions to preceding calls.

    For each [(global_get_label, global_idx)] in [global_gets], this function
    looks at the predecessor blocks of the block containing [global_get_label].
    If a predecessor contains a direct call to a function that may affect the
    global variable [global_idx], then the [global.get] is made dependent on
    that call.

    The result maps each [global.get] label to the set of call labels it depends
    on. *)
let globals_depend_on_calls
    (global_gets : (Instr.Label.t * int32) list)
    (cfg : Spec_domain.t Cfg.t)
    (pointer_analysis : (Abstract_store_domain.t Cfg.t * Spec_domain.t Instr.t Instr.Label.Map.t * Abstract_store_domain.t Int32Map.t) option)
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
                          if function_may_affect_global_variable
                              pointer_analysis
                              ~global_var
                              ~fct_index 
                          then
                            (Log.info ("fct " ^ Int32.to_string fct_index ^ " may affect global variable " ^ Var.to_string global_var);
                            Instr.Label.Set.add acc call_label)
                          else
                            (Log.info ("fct " ^ Int32.to_string fct_index ^ " does not affect global variable " ^ Var.to_string global_var);
                            acc)
                        | _ -> acc)
            in
            Instr.Label.Map.update dependencies global_var_label
              ~f:(function
                  | None -> deps
                  | Some set -> Instr.Label.Set.union set deps))

(** Compute dependencies from call instructions to preceding global definitions.

    For each direct call, this function uses the global-read analysis to find
    which global variables may be read by the called function. It then searches
    the predecessor blocks of the call and adds dependencies on earlier
    instructions that define those globals.

    If the global-read information is [Top], the analysis is conservative and
    the call is made dependent on every instruction found in the relevant
    predecessor data blocks.

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
        let globals_used_by_function = Int32Map.find_exn global_deps idx in
        let block = Cfg.find_enclosing_block_exn cfg call_label in
        let predecessors = Cfg.all_predecessors cfg block in
        let instr_set =
          List.fold predecessors ~init:Instr.Label.Set.empty 
            ~f:(fun acc block ->
                    match block.content with
                    | Data instrs' -> 
                      List.fold instrs' ~init:acc ~f:(fun acc instr ->
                        match globals_used_by_function with
                        | Top -> Instr.Label.Set.add acc instr.label
                        | NotTop globals_used ->
                          if Global_read_domain.GlobalInstruction.Set.mem globals_used instr.label then
                            Instr.Label.Set.add acc instr.label
                          else
                            acc)
                    | _ -> acc)
        in
        Instr.Label.Map.update dependencies call_label
          ~f:(function 
              | None -> instr_set 
              | Some set -> Instr.Label.Set.union set instr_set))

(** Return all [global.get] instructions in the current function, together with
    the index of the global variable they read. *)
let find_global_get_instructions
    (cfg_instructions : Spec_domain.t Instr.t Instr.Label.Map.t) 
  : (Instr.Label.t * int32) list =
  Instr.Label.Map.to_alist cfg_instructions
  |> List.filter_map ~f:(fun (label, instr) ->
      match instr with
      | Data { instr = GlobalGet global_idx; _ } -> Some (label, global_idx)
      | _ -> None)

(** Return all direct call instructions in the current function, together with
    the index of the function they call. *)
let find_call_instructions
    (cfg_instructions : Spec_domain.t Instr.t Instr.Label.Map.t)
  : (Instr.Label.t * int32) list =
  Instr.Label.Map.to_alist cfg_instructions
  |> List.filter_map ~f:(fun (label, instr) ->
      match instr with
      | Call { instr = CallDirect (_, _, func_idx); _ } -> Some (label, func_idx)
      (* TODO: add indirect calls *)
      | _ -> None)

(** Compute all additional global-related dependencies for the current function.

    This combines dependencies in both directions:

    - [global.get] instructions depending on earlier calls that may modify the
      global variable being read;
    - call instructions depending on earlier definitions of globals that may be
      read by the callee.

    The two dependency maps are merged by taking the union of dependency sets
    when both analyses produce dependencies for the same instruction label. *)
let global_dependencies 
    ~(global_deps : Global_read_domain.t Int32Map.t)
    ~(cfg : Spec_domain.t Cfg.t)
    ~(cfg_instructions : Spec_domain.t Instr.t Instr.Label.Map.t)
    ~(pointer_analysis : (Abstract_store_domain.t Cfg.t * Spec_domain.t Instr.t Instr.Label.Map.t * Abstract_store_domain.t Int32Map.t) option)
  : t =
  let global_gets = find_global_get_instructions cfg_instructions in
  let call_instructions = find_call_instructions cfg_instructions in
  let global_call = globals_depend_on_calls global_gets cfg pointer_analysis in
  let call_global = calls_depend_on_globals global_deps call_instructions cfg in
  Instr.Label.Map.merge global_call call_global
    ~f:(fun ~key:_ -> function 
                      | `Both (a, b) -> Some (Instr.Label.Set.union a b)
                      | `Left x -> Some x
                      | `Right x -> Some x)