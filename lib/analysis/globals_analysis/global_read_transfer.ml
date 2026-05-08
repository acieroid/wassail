open Core


(** Transfer functions for the global-read analysis.

    This module defines how abstract states are propagated through WebAssembly
    instructions in order to collect the set of global variables that may be
    read by each function. *)

(** Instantiation of the summary-based transfer interface for the global-read
    analysis. *)
module Make = struct
  (** Abstract state manipulated by the analysis. *)
  module State = Global_read_domain
  (** Function summaries used by the interprocedural analysis. *)
  module Summary = Global_read_summary

  (** Precomputed global-definition information used by the transfer functions.

      This reference is initialized once by [Global_read.analyze_inter] before
      launching the interprocedural analysis, via [set_global_defs]. It is then
      read by transfer functions (notably [data]) to resolve which definitions
      correspond to a given [global.get]. *)
  let global_defs : Global_defs.t ref = ref Global_defs.empty

  (** Stores the global-definition preanalysis for later use by transfer
      functions. *)
  let set_global_defs (defs : Global_defs.t) : unit =
    global_defs := defs


  (** Type of annotations expected on the input CFG. *)
  type annot_expected = Spec_domain.t

  (** Type of function summaries produced and consumed by this transfer. *)
  type summary = Summary.t

  (** Bottom abstract state. *)
  let bottom = State.bottom

  (** Initial abstract state for a function analysis. *)
  let init : Wasm_module.t -> Func_inst.t -> State.t = fun _ _ -> bottom

  (* let state_to_string = State.to_string *)

  (** Joins two abstract states. *)
  let join_state = Global_read_domain.join


  (** Transfer function for data instructions.

      A [global.get] instruction reads a global variable. Using the precomputed
      definitions from [global_defs], the transfer retrieves the set of variables
      that may define this global and joins them into the abstract state.

      Other data instructions leave the state unchanged. *)
  let data
      (_module_ : Wasm_module.t)
      (_cfg : annot_expected Cfg.t)
      (i : annot_expected Instr.labelled_data)
      (state : State.t)
    : State.t =
    match i.instr with
    | GlobalGet g ->
      (let global_var = Var.Global (Int32.to_int_exn g) in
        Log.info (Printf.sprintf "global.get %ld --- function uses variable %s" g (Var.to_string global_var));
        match Global_defs.get ~defs:!global_defs ~global_var with
        | None -> state
        | Some defs -> Global_read_domain.join state (NotTop defs))
    | _ -> state


  (** Transfer function for control instructions.

      Conditional branches propagate the same state to both successors.
      Unreachable instructions produce the bottom state. Other control
      instructions leave the state unchanged. *)
   let control
      (_module_ : Wasm_module.t) (* The wasm module (read-only) *)
      (_cfg : annot_expected Cfg.t) (* The CFG analized *)
      (i : annot_expected Instr.labelled_control) (* The instruction *)
      (state : State.t) (* the pre-state *)
    : [`Simple of State.t | `Branch of State.t * State.t] =
    match i.instr with
    | If _ | BrIf _ -> `Branch (state, state)
    | Unreachable -> `Simple Global_read_domain.bottom
    | _ -> `Simple state

  (** Merges incoming abstract states at the beginning of a basic block.

      Entry blocks start from [bottom]. Merge blocks and loop heads join all
      predecessor states. Non-merge blocks are expected to have a single
      predecessor state. *)
  let merge_flows
      (_module_ : Wasm_module.t) 
      (_cfg : annot_expected Cfg.t) 
      (block : annot_expected Basic_block.t)
      (predecessors : ('a Basic_block.t * State.t) list)
    : State.t =
    match predecessors with
    | [] -> bottom
    | _ ->
      begin match block.content with
      | Control { instr = Merge; _ }
      | Entry | Return _ ->
        let states' = List.map ~f:(fun (_, s) -> s) predecessors in
        List.reduce_exn states' ~f:join_state
      | Control _ ->
        begin match predecessors with
        | (_, s) :: [] -> s
        | _ -> failwith (Printf.sprintf "Invalid block with multiple input states: %d" block.idx)
        end
      |  _ -> 
        begin match predecessors with
        | (_, s) :: [] -> s
        | _ -> failwith (Printf.sprintf "Invalid block with multiple input states: %d" block.idx)
        end
      end

  (** Transfer function for imported function calls.

      Imported functions are handled conservatively and are assumed to read all
      global variables, hence the abstract state is set to [Top]. *)
  let imported
      (_module_ : Wasm_module.t)
      (_desc : Wasm_module.func_desc)
      (_annot_before : annot_expected)
      (_annot_after : annot_expected)
      (_state : State.t)
    : State.t =
    Global_read_domain.Top

  (** Applies the summary of a called function at a call site.

      The summary represents the set of globals that may be read by the callee,
      and is joined into the current abstract state. *)
  let apply_summary 
      (_module_ : Wasm_module.t)
      (f : Int32.t) 
      (_arity : int * int) 
      (_i : annot_expected Instr.labelled_call)
      (state : State.t)
      (summary : summary)
    : State.t =
    Log.info (Printf.sprintf "call %ld --- summary of function %ld:\t%s" f f (Global_read_summary.to_string summary));
    Summary.apply ~state ~summary


  (** Builds the summary of the currently analyzed function from its output
      state.

      If the function exit is unreachable according to the input annotations,
      the bottom summary is returned. Otherwise, the final abstract state
      (representing globals read by the function) is converted into a function
      summary. *)
  let summary (cfg : annot_expected Cfg.t) (out_state : State.t) : summary =
    let function_summary =
      match (Cfg.find_block_exn cfg cfg.exit_block).annotation_after with
      | Bottom ->
        (* The function exit is likely unreachable, so we use a bottom summary *)
        Summary.bottom
      | NotBottom _ ->
        Summary.make out_state
    in
    Log.info ("SUMMARY:\t" ^ Summary.to_string function_summary);
    function_summary


  (** Extracts the summary of a function from the analyzed CFG by reading the
      abstract state at the exit block and delegating to [summary]. *)
  let extract_summary 
      (_module_ : Wasm_module.t)
      (cfg : annot_expected Cfg.t)
      (analyzed_cfg : State.t Cfg.t)
    : summary =
    let out_state = (Cfg.find_block_exn analyzed_cfg cfg.exit_block).annotation_after in
    summary cfg out_state
end