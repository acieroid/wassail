open Core
open Helpers

module Make
  : Transfer.TRANSFER
  with type annot_expected = Spec.t
   and type summary = Taint_summary.t
   and type state = Taint_domain.t
   and module Cfg = Cfg.Cfg = struct
  module Cfg = Cfg.Cfg

  (** We need the variable names as annotations *)
  type annot_expected = Spec.t

  (** The state *)
  type state = Taint_domain.t
  [@@deriving sexp, compare, equal]

  (* This is a map from function index to:
     - the taint of its return value
     - the taint of its arguments
     TODO: fill it from the names of the imports/exports
   *)
  let taint_specifications : (Taint_domain.Taint.t * (Taint_domain.Taint.t list)) StringMap.t ref =
    let bottom = Taint_domain.Taint.Taints Var.Set.empty in
    ref (StringMap.of_alist_exn [
        ("fgets", (Taint_domain.Taint.Taints (Var.Set.singleton (Var.Other "fgets")),
                   [Taint_domain.Taint.Taints (Var.Set.singleton (Var.Other "fgets")); bottom; bottom]))
      ])

  (** In the initial state, we only set the taint for for parameters and the globals. *)
  let init_state (cfg : 'a Cfg.t) : state =
    Var.Map.of_alist_exn
      ((List.mapi (Cfg.arg_types cfg) ~f:(fun i _ -> (Var.Local i,
                                                Taint_domain.Taint.taint (Var.Local i)))) @
       (List.mapi (Cfg.global_types cfg) ~f:(fun i _ -> (Var.Global i,
                                                 Taint_domain.Taint.taint (Var.Global i)))))

  let bottom_state (_cfg : 'a Cfg.t) : state = Taint_domain.bottom

  let state_to_string (s : state) : string = Taint_domain.to_string s

  let join_state (s1 : state) (s2 : state) : state = Taint_domain.join s1 s2

  let widen_state (_s1 : state) (s2 : state) : state = s2 (* no widening *)

  type summary = Taint_summary.t

  let data_instr_transfer
      (_module_ : Wasm_module.t)
      (_cfg : annot_expected Cfg.t)
      (i : annot_expected Instr.labelled_data)
      (state : state)
    : state =
    let ret (i : annot_expected Instr.labelled_data) : Var.t = match List.hd (Spec.get_or_fail i.annotation_after).vstack with
      | Some r -> r
      | None -> failwith "Taint: no return value" in
    match i.instr with
    | Nop | MemorySize | Drop | MemoryGrow -> state
    | MemoryCopy | MemoryFill | MemoryInit _ -> state (* Not model entirely properly *)
    | RefIsNull | RefNull _ | RefFunc _ -> state
    | Select _ ->
      let ret = ret i in
      let (_c, v2, v1) = pop3 (Spec.get_or_fail i.annotation_before).vstack in
      (* TODO: could improve precision by checking the constraints on c: if it is precisely zero/not-zero, we can only include v1 or v2 *)
      Taint_domain.add_taint_v (Taint_domain.add_taint_v state ret v1) ret v2
    | LocalGet l ->
      Taint_domain.add_taint_v state (ret i) (get_nth (Spec.get_or_fail i.annotation_before).locals l)
    | LocalSet l ->
      Taint_domain.add_taint_v state (get_nth (Spec.get_or_fail i.annotation_before).locals l) (pop (Spec.get_or_fail i.annotation_before).vstack)
    | LocalTee l ->
      Taint_domain.add_taint_v
        (Taint_domain.add_taint_v state (get_nth (Spec.get_or_fail i.annotation_before).locals l) (pop (Spec.get_or_fail i.annotation_before).vstack))
        (ret i) (get_nth (Spec.get_or_fail i.annotation_before).locals l)
    | GlobalGet g ->
      Taint_domain.add_taint_v state (ret i) (get_nth (Spec.get_or_fail i.annotation_before).globals g)
    | GlobalSet g ->
      Taint_domain.add_taint_v state (get_nth (Spec.get_or_fail i.annotation_before).globals g) (pop (Spec.get_or_fail i.annotation_before).vstack)
    | Const _ -> state
    | Binary _ | Compare _ ->
      let v1, v2 = pop2 (Spec.get_or_fail i.annotation_before).vstack in
      Taint_domain.add_taint_v
        (Taint_domain.add_taint_v state (ret i) v1)
        (ret i) v2
    | Unary _ | Test _ | Convert _ ->
      Taint_domain.add_taint_v state (ret i) (pop (Spec.get_or_fail i.annotation_before).vstack)
    | Load { offset = _offset; _ } ->
      (* Simplest case: get the taint of the entire memory.
         Refined case: get the taint of the memory cells that can pointed to, according to the previous analysis stages (i.e., relational analysis) *)
      let _addr = pop (Spec.get_or_fail i.annotation_before).vstack in
      let mem = (Spec.get_or_fail i.annotation_before).memory in
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
            (Printf.sprintf "TODO: currently ignoring offsets in taints!!!\n--------------------\n--------------\n"); (* maybe only the values should be tainted, not the keys *) *)
          Taint_domain.Taint.join (Taint_domain.get_taint state k) (Taint_domain.get_taint state (Var.OffsetMap.find_exn mem (k, offset)))) in
      Taint_domain.add_taint
        state
        (ret i)
        (* ret is the join of all these taints *)
        (List.fold_left taints ~init:Taint_domain.Taint.bottom ~f:Taint_domain.Taint.join)
    | Store { offset = _offset; _ } ->
      (* Simplest case: set the taint for the entire memory
         Refined case: set the taint to the memory cells that can be pointed to, according to the previous analysis stages (i.e., relational analysis) *)
      let vval, _vaddr = pop2 (Spec.get_or_fail i.annotation_before).vstack in
      let mem = (Spec.get_or_fail i.annotation_after).memory in
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
          (* Log.warn (Printf.sprintf "TODO: ignoring offsets!"); *)
          Taint_domain.add_taint_v (Taint_domain.add_taint_v s k vval)
            (Var.OffsetMap.find_exn mem (k, offset)) vval)

  let control_instr_transfer
      (module_ : Wasm_module.t) (* The wasm module (read-only) *)
      (summaries : summary Int32Map.t) (* The summaries *)
      (_cfg : annot_expected Cfg.t) (* The CFG analyzed *)
      (i : annot_expected Instr.labelled_control) (* The instruction *)
      (state : state) (* The pre state *)
    : [`Simple of state | `Branch of state * state ] =
    let apply_summary (f : Int32.t) (arity : int * int) (state : state) : state =
      Log.info (Printf.sprintf "applying summary of function %ld" f);
      match Int32Map.find summaries f with
      | None ->
        if Int32.(f < module_.nfuncimports) then begin
          Log.warn (Printf.sprintf "No summary found for function %ld (imported function): assuming taint is preserved" f);
          state
        end else
          (* This function depend on another function that has not been analyzed yet, so it is part of some recursive loop. It will eventually stabilize *)
          state
      | Some summary ->
        let args = List.take (Spec.get_or_fail i.annotation_before).vstack (fst arity) in
        let ret = if snd arity = 1 then List.hd (Spec.get_or_fail i.annotation_after).vstack else None in
        let taint_after_call = Taint_summary.apply
            summary
            state
            args
            (Spec.get_or_fail i.annotation_before).globals
            (Spec.get_or_fail i.annotation_after).globals
            (List.concat_map (Var.OffsetMap.to_alist (Spec.get_or_fail i.annotation_after).memory)
               ~f:(fun ((a, _offset), b) ->
                   (* Log.warn (Printf.sprintf "TODO: ignoring offset\n"); *)
                   [a; b])) ret in
        let export = List.find module_.exported_funcs ~f:(fun (id, _, _) -> Int32.(id = f)) in
        match export with
        | Some (_, fname, _) ->
           Log.info (Printf.sprintf "function is named %s" fname);
           begin match ret, StringMap.find !taint_specifications fname with
           | Some ret_var, Some (ret_taint, args_taint) ->
              (* This function returns a specific taint that we have to add to ret.
               Moreover, it might taint its argument, so we propagate this taint too. *)
              Printf.printf "I found it\n";
              List.fold_left (List.zip_exn (List.rev args) args_taint)
                 ~init:(Taint_domain.add_taint taint_after_call ret_var ret_taint)
                 ~f:(fun state (var, taint) ->
                   Printf.printf "adding taint %s to var %s\n" (Taint_domain.Taint.to_string taint) (Var.to_string var);
                   Taint_domain.add_taint state var taint)
           | _ -> taint_after_call
           end
        | None ->
           taint_after_call
    in
    match i.instr with
    | Call (arity, _, f) -> `Simple (apply_summary f arity state)
    | CallIndirect (_, arity, _, typ) ->
      let targets = Call_graph.indirect_call_targets module_ typ in
      (* Apply the summaries *)
      `Simple (List.fold_left targets
        ~init:state
        ~f:(fun acc idx -> Taint_domain.join (apply_summary idx arity state) acc))
    | Br _ -> `Simple state
    | BrIf _ | If _ -> `Branch (state, state)
    | Return -> `Simple state
    | Unreachable -> `Simple Taint_domain.bottom
    | _ -> `Simple state

  let merge_flows (_module_ : Wasm_module.t) (cfg : annot_expected Cfg.t) (block : annot_expected Basic_block.t) (states : (int * state) list) : state =
    let init_spec = (Spec_inference.init_state cfg (* , Relational_transfer.bottom_state (Cfg.map_annotations cfg ~f:(fun i -> fst (Instr.annotation_before i), fst (Instr.annotation_after i)))*) )  in
    match states with
    | [] -> init_state cfg
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
                List.fold_left (Spec.extract_different_vars spec spec')
                  ~init:s
                  ~f:(fun s (x, y) ->
                      (* TODO: should it be x y or y x? *)
                      Taint_domain.add_taint_v s x y)) in
            (* And finally joins all the states *)
            List.reduce_exn states' ~f:join_state
          | _ ->
            (* Not a control-flow merge, there should be a single predecessor *)
            begin match states with
              | (_, s) :: [] -> s
              | _ -> failwith (Printf.sprintf "Invalid block with multiple input states: %d" block.idx)
            end
        end

  let extract_summary (cfg : annot_expected Cfg.t) (analyzed_cfg : state Cfg.t) : summary =
    let out_state = Cfg.state_after_block analyzed_cfg cfg.exit_block (init_state cfg) in
    Taint_summary.summary_of cfg out_state
end
