open Core
open Helpers

module Make = struct
  module State = Taint_domain

  (** We need the variable names as annotations *)
  type annot_expected = Spec_domain.t

  (* This is a map from function index to:
     - the taint of its return value
     - the taint of its arguments
     XXX: fill it from the names of the imports/exports
   *)
  let taint_specifications : (Taint_domain.Taint.t * (Taint_domain.Taint.t list)) StringMap.t ref =
    let bottom = Taint_domain.Taint.Taints Var.Set.empty in
    ref (StringMap.of_alist_exn [
        ("fgets", (Taint_domain.Taint.Taints (Var.Set.singleton (Var.Other "fgets")),
                   [Taint_domain.Taint.Taints (Var.Set.singleton (Var.Other "fgets")); bottom; bottom]))
      ])

  (** In the initial state, we only set the taint for parameters and the globals. *)
  let init (module_ : Wasm_module.t) (funcinst : Func_inst.t) : State.t =
    let arg_types, _ = funcinst.typ in
    let global_types = Wasm_module.get_global_types module_ in
    Var.Map.of_alist_exn
      ((List.mapi arg_types ~f:(fun i _ -> (Var.Local i,
                                                Taint_domain.Taint.taint (Var.Local i)))) @
       (List.mapi global_types ~f:(fun i _ -> (Var.Global i,
                                                 Taint_domain.Taint.taint (Var.Global i)))))

  let bottom : State.t = Taint_domain.bottom

  type summary = Taint_summary.t

  let data
      (_module_ : Wasm_module.t)
      (_cfg : annot_expected Cfg.t)
      (i : annot_expected Instr.labelled_data)
      (state : State.t)
    : State.t =
    let ret (i : annot_expected Instr.labelled_data) : Var.t = match List.hd (Spec_domain.get_or_fail i.annotation_after).vstack with
      | Some r -> r
      | None -> failwith "Taint: no return value" in
    match i.instr with
    | Nop | MemorySize | Drop | MemoryGrow -> state
    | MemoryCopy | MemoryFill | MemoryInit _ -> state (* Not model entirely properly *)
    | RefIsNull | RefNull _ | RefFunc _ -> state
    | Select _ ->
      let ret = ret i in
      let (_c, v2, v1) = pop3 (Spec_domain.get_or_fail i.annotation_before).vstack in
      (* XXX: could improve precision by checking the constraints on c: if it is precisely zero/not-zero, we can only include v1 or v2 *)
      Taint_domain.add_taint_v (Taint_domain.add_taint_v state ret v1) ret v2
    | LocalGet l ->
      Taint_domain.add_taint_v state (ret i) (get_nth (Spec_domain.get_or_fail i.annotation_before).locals l)
    | LocalSet l ->
      Taint_domain.add_taint_v state (get_nth (Spec_domain.get_or_fail i.annotation_before).locals l) (pop (Spec_domain.get_or_fail i.annotation_before).vstack)
    | LocalTee l ->
      Taint_domain.add_taint_v
        (Taint_domain.add_taint_v state (get_nth (Spec_domain.get_or_fail i.annotation_before).locals l) (pop (Spec_domain.get_or_fail i.annotation_before).vstack))
        (ret i) (get_nth (Spec_domain.get_or_fail i.annotation_before).locals l)
    | GlobalGet g ->
      Taint_domain.add_taint_v state (ret i) (get_nth (Spec_domain.get_or_fail i.annotation_before).globals g)
    | GlobalSet g ->
      Taint_domain.add_taint_v state (get_nth (Spec_domain.get_or_fail i.annotation_before).globals g) (pop (Spec_domain.get_or_fail i.annotation_before).vstack)
    | Const _ -> state
    | Binary _ | Compare _ ->
      let v1, v2 = pop2 (Spec_domain.get_or_fail i.annotation_before).vstack in
      Taint_domain.add_taint_v
        (Taint_domain.add_taint_v state (ret i) v1)
        (ret i) v2
    | Unary _ | Test _ | Convert _ ->
      Taint_domain.add_taint_v state (ret i) (pop (Spec_domain.get_or_fail i.annotation_before).vstack)
    | Load { offset = _offset; _ } ->
      (* Simplest case: get the taint of the entire memory.
         Refined case: get the taint of the memory cells that can pointed to, according to the previous analysis stages (i.e., relational analysis) *)
      let _addr = pop (Spec_domain.get_or_fail i.annotation_before).vstack in
      let mem = (Spec_domain.get_or_fail i.annotation_before).memory in
      let all_locs = Var.OffsetMap.keys mem in
      (* Filter the memory location using results from the relational analysis if possible *)
      let locs = (* if !Taint_options.use_relational then
          (* We need to filter locs to only have the locs that can be loaded.
             This means for each loc, we can ask the relational domain if are_equal loc v (where v is the top of the stack.
             If some are truly equal, we know we can only keep these. Otherwise, if some maybe equal, then these have to be kept. *)
          let equal = List.filter all_locs ~f:(fun loc -> match Relational_domain.are_equal_with_offset (snd i.annotation_before) loc (addr, offset) with
              | (true, false) -> true
              | _ -> false) in
          if not (List.is_empty equal) then
            (* There are addresses that are definitely equal to addr, so we get their taint *)
            equal
          else
            (* No address is definitely equal to addr, so we take the ones that may be equal *)
            List.filter all_locs ~f:(fun loc -> match Relational_domain.are_equal_with_offset (snd i.annotation_before) loc (addr, offset) with
                | (true, _) -> true
                | _ -> false)
        else *)
          all_locs in
      (* Get the taint of possible memory location and their value.
         In practice, both the memory location and the value have the same taint
      *)
      let taints = List.map locs ~f:(fun (k, offset) ->
          (* Log.warn
            (Printf.sprintf "XXX: currently ignoring offsets in taints!!!\n--------------------\n--------------\n"); (* maybe only the values should be tainted, not the keys *) *)
          Taint_domain.Taint.join (Taint_domain.get_taint state k) (Taint_domain.get_taint state (Var.OffsetMap.find_exn mem (k, offset)))) in
      Taint_domain.add_taint
        state
        (ret i)
        (* ret is the join of all these taints *)
        (List.fold_left taints ~init:Taint_domain.Taint.bottom ~f:Taint_domain.Taint.join)
    | Store { offset = _offset; _ } ->
      (* Simplest case: set the taint for the entire memory
         Refined case: set the taint to the memory cells that can be pointed to, according to the previous analysis stages (i.e., relational analysis) *)
      let vval, _vaddr = pop2 (Spec_domain.get_or_fail i.annotation_before).vstack in
      let mem = (Spec_domain.get_or_fail i.annotation_after).memory in
      let all_locs = Var.OffsetMap.keys mem in
      (* Refine memory locations using relational innformation, if available *)
      let locs = (* if !Taint_options.use_relational then
          let equal = List.filter all_locs ~f:(fun loc -> match Relational_domain.are_equal_with_offset (snd i.annotation_before) loc (vaddr, offset) with
              | (true, false) -> true
              | _ -> false) in
          if not (List.is_empty equal) then
            (* There are addresses that are definitely equal to addr, so we get their taint *)
            equal
          else
            (* No address is definitely equal to addr, so we take the ones that may be equal *)
            List.filter all_locs ~f:(fun loc -> match Relational_domain.are_equal_with_offset (snd i.annotation_before) loc (vaddr, offset) with
                | (true, _) -> true
                | _ -> false)
        else *)
          all_locs in
      (* Set the taint of memory locations and the value to the taint of vval *)
      List.fold_left locs ~init:state ~f:(fun s (k, offset) ->
          (* Log.warn (Printf.sprintf "XXX: ignoring offsets!"); *)
          Taint_domain.add_taint_v (Taint_domain.add_taint_v s k vval)
            (Var.OffsetMap.find_exn mem (k, offset)) vval)

  let control
      (_module_ : Wasm_module.t) (* The wasm module (read-only) *)
      (_cfg : annot_expected Cfg.t) (* The CFG analyzed *)
      (i : annot_expected Instr.labelled_control) (* The instruction *)
      (state : State.t) (* The pre state *)
    : [ `Simple of State.t | `Branch of State.t * State.t ] =
    match i.instr with
    | Br _ -> `Simple state
    | BrIf _ | If _ -> `Branch (state, state)
    | Return -> `Simple state
    | Unreachable -> `Simple Taint_domain.bottom
    | _ -> `Simple state

  let apply_imported
      (_module : Wasm_module.t)
      (f : Int32.t)
      (_arity : int * int)
      (_i : annot_expected Instr.labelled_call)
      (state : State.t)
    : State.t =
    Log.warn (Printf.sprintf "No summary found for function %ld (imported function): assuming taint is preserved" f);
    state

  let apply_summary
      (module_ : Wasm_module.t)
      (f : Int32.t)
      (arity : int * int)
      (i : annot_expected Instr.labelled_call)
      (state : State.t)
      (summary : summary)
    : State.t =
    Log.info (Printf.sprintf "applying summary of function %ld" f);
    let spec_before = Spec_domain.get_or_fail i.annotation_before in
    let spec_after = Spec_domain.get_or_fail i.annotation_after in
    let args = List.take spec_before.vstack (fst arity) in
    let ret = if snd arity = 1 then List.hd spec_after.vstack else None in
    let taint_after_call = Taint_summary.apply
        summary
        state
        args
        spec_before.globals
        spec_after.globals
        (List.concat_map (Var.OffsetMap.to_alist spec_after.memory)
           ~f:(fun ((a, _offset), b) ->
               (* Log.warn (Printf.sprintf "XXX: ignoring offset\n"); *)
               [a; b])) ret in
    let export = List.find module_.exported_funcs ~f:(fun desc -> Int32.(desc.idx = f)) in
    match export with
    | Some desc ->
      begin match ret, StringMap.find !taint_specifications desc.name with
        | Some ret_var, Some (ret_taint, args_taint) ->
          (* This function returns a specific taint that we have to add to ret.
             Moreover, it might taint its argument, so we propagate this taint
             too. *)
          List.fold_left (List.zip_exn (List.rev args) args_taint)
            ~init:(Taint_domain.add_taint taint_after_call ret_var ret_taint)
            ~f:(fun state (var, taint) ->
                Printf.printf "adding taint %s to var %s\n" (Taint_domain.Taint.to_string taint) (Var.to_string var);
                Taint_domain.add_taint state var taint)
        | _ -> taint_after_call
      end
    | None ->
      taint_after_call

  let merge_flows (module_ : Wasm_module.t) (cfg : annot_expected Cfg.t) (block : annot_expected Basic_block.t) (states : (int * State.t) list) : State.t =
    let init_spec = (Spec_inference.init module_ (Wasm_module.get_funcinst module_ cfg.idx) (* , Relational_transfer.bottom_state (Cfg.map_annotations cfg ~f:(fun i -> fst (Instr.annotation_before i), fst (Instr.annotation_after i)))*) )  in
    match states with
    | [] -> bottom
    | _ ->
      (* one or multiple states *)
        begin match block.content with
          | Control { instr = Merge; _ } ->
            (* block is a control-flow merge *)
            let spec = Cfg.state_after_block cfg block.idx init_spec in
            let states' = List.map states ~f:(fun (idx, s) ->
                (* get the spec after that state *)
                let spec' = Cfg.state_after_block cfg idx init_spec in
                (* equate all different variables in the post-state with the ones in the pre-state *)
                List.fold_left (Spec_domain.extract_different_vars spec spec')
                  ~init:s
                  ~f:(fun s (x, y) ->
                      (* XXX: should it be x y or y x? *)
                      Taint_domain.add_taint_v s x y)) in
            (* And finally joins all the states *)
            List.reduce_exn states' ~f:State.join
          | _ ->
            (* Not a control-flow merge, there should be a single predecessor *)
            begin match states with
              | (_, s) :: [] -> s
              | _ -> failwith (Printf.sprintf "Invalid block with multiple input states: %d" block.idx)
            end
        end

  let extract_summary (module_ : Wasm_module.t) (cfg : annot_expected Cfg.t) (analyzed_cfg : State.t Cfg.t) : summary =
    let out_state = Cfg.state_after_block analyzed_cfg cfg.exit_block (init module_ (Wasm_module.get_funcinst module_ cfg.idx)) in
    Taint_summary.summary_of module_ cfg out_state

  let call_inter
      (_module : Wasm_module.t)
      (_cfg : annot_expected Cfg.t)
      (_instr : annot_expected Instr.labelled_call)
      (state : State.t)
    : State.t =
    (* This is a no-op, the important stuff happens in entry, because we need to
       know the number of locals of the function. This is something we know here
       for direct calls, but not for indirect calls. *)
    state

  let entry (_module_ : Wasm_module.t) (_cfg : annot_expected Cfg.t) (state : State.t) : State.t =
    state (* Everything is actually already done by spec analysis! We can just propagete the state *)

  let return (_module : Wasm_module.t) (_cfg : annot_expected Cfg.t) (_instr : annot_expected Instr.labelled_call) (_state_before_call : State.t) (state_after_call : State.t) : State.t =
    state_after_call

  let imported (_module_ : Wasm_module.t) (_desc : Wasm_module.func_desc) : State.t -> State.t =
    failwith "TODO: taint imported"

end
