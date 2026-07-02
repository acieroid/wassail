open Core


(** Transfer functions for the global-read analysis.

    This module defines how abstract states are propagated through WebAssembly
    instructions in order to collect the set of [global.set] instructions whose
    definitions may be required by each function, according to the globals that
    the function may read. *)

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
      definitions from [global_defs], the transfer retrieves the set of [global.set] 
      instructions that may define this global and joins them into the abstract state.

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
        Log.info (fun () -> Printf.sprintf "global.get %ld --- function uses variable %s" g (Var.to_string global_var));
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

      The summary represents the set of [global.set] instructions that may be
      required by the callee because of the globals it may read.
      It is then joined into the current abstract state. *)
  let apply_summary 
      (_module_ : Wasm_module.t)
      (f : Int32.t) 
      (_arity : int * int) 
      (_i : annot_expected Instr.labelled_call)
      (state : State.t)
      (summary : summary)
    : State.t =
    Log.info (fun () -> Printf.sprintf "call %ld --- summary of function %ld:\t%s" f f (Global_read_summary.to_string summary));
    Summary.apply ~state ~summary


  (** Builds the summary of the currently analyzed function from its output
      state.

      If the function exit is unreachable according to the input annotations,
      the bottom summary is returned. Otherwise, the final abstract state
      (representing required global definitions for the function) is converted 
      into a function summary. *)
  let summary (cfg : annot_expected Cfg.t) (out_state : State.t) : summary =
    let function_summary =
      match (Cfg.find_block_exn cfg cfg.exit_block).annotation_after with
      | Bottom ->
        (* The function exit is likely unreachable, so we use a bottom summary *)
        Summary.bottom
      | NotBottom _ ->
        Summary.make out_state
    in
    Log.info (fun () -> "SUMMARY:\t" ^ Summary.to_string function_summary);
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


let%test_module "Global-read transfer tests" = (module struct
  let%test "Global_read_transfer_tests" =
    print_endline "_______ _____________________________ _______\n        Global-read transfer function       \n------- ----------------------------- -------\n";
    true

  let%test "init is bottom" =
    let module_ = Wasm_module.of_string "(module (func))" in
    let func = List.hd_exn module_.funcs in
    Global_read_domain.equal
      (Make.init module_ func)
      Global_read_domain.bottom

  let%test "imported function returns top" =
    let module_ =
      Wasm_module.of_string
        "(module
           (import \"env\" \"f\" (func)))"
    in
    let desc = List.hd_exn module_.imported_funcs in
    let annot_before = Spec_domain.Bottom in
    let annot_after = Spec_domain.Bottom in
    Global_read_domain.equal
      (Make.imported module_ desc annot_before annot_after Global_read_domain.bottom)
      Global_read_domain.Top

  let%test "global.get adds corresponding global definitions" =
    let module_ =
      Wasm_module.of_string
        "(module
          (global $g0 (mut i32) (i32.const 0))
          (func $f
            global.get 0
            drop))"
    in
    let func = List.hd_exn module_.funcs in
    let cfg = Spec_analysis.analyze_intra1 module_ func.idx in
    let global_get =
      Cfg.all_instructions_list cfg
      |> List.find_map_exn ~f:(function
        | Data ({ instr = GlobalGet 0l; _ } as i) -> Some i
        | _ -> None)
    in
    let global_var = Var.Global 0 in
    let def_label : Instr.Label.t = { section = Function 0l; id = 42 } in
    let defs =
      Global_read_domain.GlobalInstruction.Set.singleton
        (Var.to_string global_var, def_label)
    in
    let global_defs = Var.Map.of_alist_exn [ global_var, defs ] in
    let initial_state = Global_read_domain.bottom in
    let expected_state = Global_read_domain.NotTop defs in

    print_endline "[global.get adds corresponding definition]";
    print_endline ("\tglobal.get reads: " ^ Var.to_string global_var);
    print_endline ("\tknown definitions: " ^ Global_read_domain.to_string expected_state);
    print_endline ("\tstate before: " ^ Global_read_domain.to_string initial_state);

    Make.set_global_defs global_defs;

    let result = Make.data module_ cfg global_get initial_state in

    print_endline ("\tstate after: " ^ Global_read_domain.to_string result);
    print_endline ("\texpected: " ^ Global_read_domain.to_string expected_state);

    Global_read_domain.equal result expected_state


  let%test "global.get with no global.set definitions leaves state unchanged" =
    let module_ =
      Wasm_module.of_string
        "(module
          (global $g0 (mut i32) (i32.const 0))
          (func $f
            global.get 0
            drop))"
    in
    let func = List.hd_exn module_.funcs in
    let cfg = Spec_analysis.analyze_intra1 module_ func.idx in
    let global_get =
      Cfg.all_instructions_list cfg
      |> List.find_map_exn ~f:(function
        | Data ({ instr = GlobalGet 0l; _ } as i) -> Some i
        | _ -> None)
    in
    let initial_state = Global_read_domain.bottom in

    print_endline "[global.get with no known definitions]";
    print_endline ("\tstate before: " ^ Global_read_domain.to_string initial_state);

    Make.set_global_defs Global_defs.empty;

    let result = Make.data module_ cfg global_get initial_state in

    print_endline ("\tstate after: " ^ Global_read_domain.to_string result);
    print_endline ("\texpected: " ^ Global_read_domain.to_string initial_state);

    Global_read_domain.equal result initial_state



  let%test "apply_summary joins callee summary into current state" =
    let module_ =
      Wasm_module.of_string
        "(module
          (func $callee)
          (func $caller
            call 0))"
    in
    let caller = List.nth_exn module_.funcs 1 in
    let cfg = Spec_analysis.analyze_intra1 module_ caller.idx in
    let call_instr =
      Cfg.all_instructions_list cfg
      |> List.find_map_exn ~f:(function
        | Call ({ instr = CallDirect _; _ } as i) -> Some i
        | _ -> None)
    in

    let state_label : Instr.Label.t =
      { section = Function 0l; id = 1 }
    in
    let summary_label : Instr.Label.t =
      { section = Function 0l; id = 2 }
    in

    let state_def = ("g0", state_label) in
    let summary_def = ("g1", summary_label) in

    let state =
      Global_read_domain.NotTop
        (Global_read_domain.GlobalInstruction.Set.singleton state_def)
    in

    let summary =
      Global_read_domain.NotTop
        (Global_read_domain.GlobalInstruction.Set.singleton summary_def)
    in

    let expected =
      Global_read_domain.NotTop
        (Global_read_domain.GlobalInstruction.Set.of_list
          [ state_def; summary_def ])
    in

    print_endline "[apply_summary joins caller state with callee summary]";
    print_endline
      ("\tcaller state before call: "
      ^ Global_read_domain.to_string state);

    print_endline
      ("\tcallee summary: "
      ^ Global_read_summary.to_string summary);

    print_endline
      ("\texpected merged state: "
      ^ Global_read_domain.to_string expected);

    let result =
      Make.apply_summary
        module_
        0l
        (0, 0)
        call_instr
        state
        summary
    in

    print_endline
      ("\tstate after applying summary: "
      ^ Global_read_domain.to_string result);

    Global_read_domain.equal result expected



  let%test "unreachable resets state to bottom" =
    let module_ =
      Wasm_module.of_string
        "(module
          (func $f
            unreachable))"
    in
    let func = List.hd_exn module_.funcs in
    let cfg = Spec_analysis.analyze_intra1 module_ func.idx in
    let unreachable =
      Cfg.all_instructions_list cfg
      |> List.find_map_exn ~f:(function
        | Control ({ instr = Unreachable; _ } as i) -> Some i
        | _ -> None)
    in

    let label : Instr.Label.t = { section = Function 0l; id = 1 } in
    let def = ("g0", label) in
    let state =
      Global_read_domain.NotTop
        (Global_read_domain.GlobalInstruction.Set.singleton def)
    in

    print_endline "[unreachable resets state to bottom]";
    print_endline ("\tstate before: " ^ Global_read_domain.to_string state);

    let result = Make.control module_ cfg unreachable state in

    match result with
    | `Simple result ->
      print_endline ("\tstate after: " ^ Global_read_domain.to_string result);
      print_endline ("\texpected: " ^ Global_read_domain.to_string Global_read_domain.bottom);
      Global_read_domain.equal result Global_read_domain.bottom
    | `Branch _ -> false


    
  let%test "if branches preserve the current state on both branches" =
    let module_ =
      Wasm_module.of_string
        "(module
          (func $f (param i32)
            local.get 0
            if
            end))"
    in
    let func = List.hd_exn module_.funcs in
    let cfg = Spec_analysis.analyze_intra1 module_ func.idx in
    let if_instr =
      Cfg.all_instructions_list cfg
      |> List.find_map_exn ~f:(function
        | Control ({ instr = If _; _ } as i) -> Some i
        | _ -> None)
    in

    let label : Instr.Label.t = { section = Function 0l; id = 1 } in
    let def = ("g0", label) in
    let state =
      Global_read_domain.NotTop
        (Global_read_domain.GlobalInstruction.Set.singleton def)
    in

    print_endline "[if branches preserve the current state on both branches]";
    print_endline ("\tstate before if: " ^ Global_read_domain.to_string state);

    let result = Make.control module_ cfg if_instr state in

    match result with
    | `Branch (then_state, else_state) ->
      print_endline
        ("\tthen-branch state: " ^ Global_read_domain.to_string then_state);
      print_endline
        ("\telse-branch state: " ^ Global_read_domain.to_string else_state);
      Global_read_domain.equal then_state state
      && Global_read_domain.equal else_state state
    | `Simple _ -> false
end)