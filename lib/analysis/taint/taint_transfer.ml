open Core
open Helpers

module Make = struct
  module State = Taint_domain

  (** We need the variable names as annotations *)
  type annot_expected = Spec_domain.t

  (** The taint specification for an imported function is the taint to add to each component *)
  type taint_specification = {
    return : Taint_domain.Taint.t;
    arguments : Taint_domain.Taint.t list;
    (* XXX: taint on globals and memory is not modeled yet *)
  }

  (* The taint specifications as a map from function name to specification.
     Stored as a reference so that it can be extended *)
  let taint_specifications : taint_specification StringMap.t ref=
    let bottom = Taint_domain.Taint.bottom in
    let of_var (v : Var.t) = Taint_domain.Taint.taint v in
    ref (StringMap.of_alist_exn [
      (* https://wasix.org/docs/api-reference/wasi/proc_exit *)
      "proc_exit", { return = bottom; arguments = [bottom] };
      (* https://wasix.org/docs/api-reference/wasi/fd_seek *)
      "fd_seek", { return = bottom; arguments = [of_var (Var.Other "fd_seek.fd");
                                                 bottom;
                                                 bottom;
                                                 of_var (Var.Other "fd_seek.newoffset")] };
      (* https://wasix.org/docs/api-reference/wasi/fd_write *)
      "fd_write", { return = bottom; arguments = [of_var (Var.Other "fd_write.fd");
                                                  bottom;
                                                  bottom;
                                                  bottom] };
      (* https://wasix.org/docs/api-reference/wasi/fd_read *)
      "fd_read", { return = bottom; arguments = [bottom;
                                                 of_var (Var.Other "fd_read.iovs");
                                                 bottom;
                                                 bottom] };
      (* https://wasix.org/docs/api-reference/wasi/fd_fdstat_get *)
      "fd_fdstat_get", { return = bottom; arguments = [bottom;
                                                       of_var (Var.Other "fd_fdstat_get.buf_ptr")] };
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
    Spec_domain.wrap i.annotation_after ~default:State.bottom ~f:(fun annotation_after ->
        Spec_domain.wrap i.annotation_before ~default:State.bottom ~f:(fun annotation_before ->
            let ret () =
              (* Wrapped in a function as we don't want to compute it when there's no return *)
              Option.value_exn ~error:(Error.of_string "Taint: no return value") (List.hd annotation_after.vstack) in
            match i.instr with
            | Nop | MemorySize | Drop | MemoryGrow -> state
            | MemoryCopy | MemoryFill | MemoryInit _ -> state (* Not model entirely properly *)
            | RefIsNull | RefNull _ | RefFunc _ -> state
            | Select _ ->
              let (_c, v2, v1) = pop3 annotation_before.vstack in
              (* XXX: could improve precision by checking the constraints on c: if it is precisely zero/not-zero, we can only include v1 or v2 *)
              Taint_domain.add_taint_v (Taint_domain.add_taint_v state (ret ()) v1) (ret ()) v2
            | LocalGet l ->
              Taint_domain.add_taint_v state (ret ()) (get_nth annotation_before.locals l)
            | LocalSet l ->
              Taint_domain.add_taint_v state (get_nth annotation_after.locals l) (pop annotation_before.vstack)
            | LocalTee l ->
              let new_local = get_nth annotation_after.locals l in
              Taint_domain.add_taint_v
                (Taint_domain.add_taint_v state new_local (pop annotation_before.vstack))
                (ret ()) new_local
            | GlobalGet g ->
              Taint_domain.add_taint_v state (ret ()) (get_nth annotation_before.globals g)
            | GlobalSet g ->
              Taint_domain.add_taint_v state (get_nth annotation_after.globals g) (pop annotation_before.vstack)
            | Const _ -> state
            | Binary _ | Compare _ ->
              let v1, v2 = pop2 annotation_before.vstack in
              Taint_domain.add_taint_v
                (Taint_domain.add_taint_v state (ret ()) v1)
                (ret ()) v2
            | Unary _ | Test _ | Convert _ ->
              Taint_domain.add_taint_v state (ret ()) (pop annotation_before.vstack)
            | Load _ ->
              let memory_taint =
                Var.OffsetMap.fold annotation_before.memory
                  ~init:(Taint_domain.get_taint state Taint_domain.untracked_memory)
                  ~f:(fun ~key:_ ~data:value taint ->
                      Taint_domain.Taint.join taint (Taint_domain.get_taint state value)) in
              Taint_domain.add_taint state (ret ()) memory_taint
            | Store { offset; _ } ->
              let _, address = pop2 annotation_before.vstack in
              Option.fold (Var.OffsetMap.find annotation_before.memory (address, offset))
                ~init:state ~f:(fun state previous ->
                    Taint_domain.add_taint_v state Taint_domain.untracked_memory previous)))

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

  let apply_summary
      (module_ : Wasm_module.t)
      (f : Int32.t)
      (arity : int * int)
      (i : annot_expected Instr.labelled_call)
      (state : State.t)
      (summary : summary)
    : State.t =
    Log.info (fun () -> Printf.sprintf "applying summary of function %ld" f);
    let spec_before = Spec_domain.get_or_fail i.annotation_before in
    let spec_after = Spec_domain.get_or_fail i.annotation_after in
    let stack = match i.instr with
      | CallDirect _ -> spec_before.vstack
      | CallIndirect _ -> List.drop spec_before.vstack 1 in
    let args = List.rev (List.take stack (fst arity)) in
    let ret = if snd arity = 1 then List.hd spec_after.vstack else None in
    let taint_after_call = Taint_summary.apply
        ~include_memory:(not (Wasm_module.is_imported module_ f))
        summary
        state
        args
        spec_before.globals
        spec_after.globals
        ret in
    taint_after_call

  let merge_flows
      (_module_ : Wasm_module.t)
      (_cfg : annot_expected Cfg.t)
      (block : annot_expected Basic_block.t)
      (predecessors : ('a Basic_block.t * State.t) list)
    : State.t =
    match predecessors with
    | [] -> bottom
    | _ ->
      (* one or multiple states *)
        begin match block.content with
          | Control { instr = Merge; _ }
          | Entry | Return _ ->
            (* block is a control-flow merge *)
            let spec = block.annotation_after in
            let states' = List.map predecessors ~f:(fun (pred_block, s) ->
                (* get the spec after that state *)
                let spec' = pred_block.annotation_after in
                (* Spec inference keeps only memory bindings common to all paths.
                   A write on any path can still taint a later load. *)
                let s' = match spec, spec' with
                  | Spec_domain.NotBottom joined, Spec_domain.NotBottom prior ->
                    Var.OffsetMap.fold prior.memory ~init:s ~f:(fun ~key ~data:value acc ->
                        if Var.OffsetMap.mem joined.memory key then
                          acc
                        else
                          Taint_domain.add_taint_v acc Taint_domain.untracked_memory value)
                  | _ -> s in
                (* equate all different variables in the post-state with the ones in the pre-state *)
                List.fold_left (Spec_domain.extract_different_vars spec spec')
                  ~init:s'
                  ~f:(fun s (x, y) ->
                      (* XXX: should it be x y or y x? *)
                      Taint_domain.add_taint_v s x y)) in
            (* And finally joins all the states *)
            List.reduce_exn states' ~f:State.join
          | _ ->
            (* Not a control-flow merge, there should be a single predecessor *)
            begin match predecessors with
              | (_, s) :: [] -> s
              | _ -> failwith (Printf.sprintf "Invalid block with multiple input states: %d" block.idx)
            end
        end

  let extract_summary (_module_ : Wasm_module.t) (cfg : annot_expected Cfg.t) (analyzed_cfg : State.t Cfg.t) : summary =
    let exit_block = Cfg.find_block_exn analyzed_cfg cfg.exit_block in
    Taint_summary.summary_of cfg exit_block.annotation_after

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
    state (* Everything is actually already done by spec analysis! We can just propagate the state *)

  let return (_module : Wasm_module.t) (_cfg : annot_expected Cfg.t) (_instr : annot_expected Instr.labelled_call) (_state_before_call : State.t) (state_after_call : State.t) : State.t =
    state_after_call

  let imported
      (_module : Wasm_module.t)
      (desc : Wasm_module.func_desc)
      (annot_before : annot_expected)
      (annot_after : annot_expected)
      (state : State.t)
    : State.t =
    Spec_domain.wrap annot_before ~default:State.bottom ~f:(fun annotation_before ->
    Spec_domain.wrap annot_after ~default:State.bottom ~f:(fun annotation_after ->
        match StringMap.find !taint_specifications desc.name with
        | None ->
          Log.warn (fun () -> Printf.sprintf "No specification found for imported function %s (index: %ld): assuming taint is preserved" desc.name desc.idx);
          state
        | Some spec ->
          (* Taint the return value *)
          let state = match List.hd annotation_after.vstack with
            | None -> state (* no return value to taint *)
            | Some ret_var ->
              Taint_domain.add_taint state ret_var spec.return in
          (* Taint the arguments *)
          let args = List.take annotation_before.vstack (List.length desc.arguments) |> List.rev in
          if List.length args <> List.length spec.arguments then
            failwith (Printf.sprintf "Improperly modeled function %s: got %d arguments, but modeled with %d"
                        desc.name (List.length args) (List.length spec.arguments))
          else
            let state = List.fold_left (List.zip_exn args spec.arguments)
                ~init:state
                ~f:(fun state (var, taint) ->
                    Taint_domain.add_taint state var taint) in
            state))

end
