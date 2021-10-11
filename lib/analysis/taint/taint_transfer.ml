open Core_kernel
open Helpers

module Make (* : Transfer.TRANSFER *) = struct
  (** We need the variable names as annotations *)
  type annot_expected = (Spec.t * Relational_domain.t)

  (** The state *)
  type state = Taint_domain.t
  [@@deriving sexp, compare, equal]

  (** In the initial state, we only set the taint for for parameters and the globals. *)
  let init_state (cfg : 'a Cfg.t) : state =
    Var.Map.of_alist_exn
      ((List.mapi cfg.arg_types ~f:(fun i _ -> (Var.Local i,
                                                Taint_domain.Taint.taint (Var.Local i)))) @
       (List.mapi cfg.global_types ~f:(fun i _ -> (Var.Global i,
                                                 Taint_domain.Taint.taint (Var.Global i)))))

  let bottom_state (_cfg : 'a Cfg.t) : state = Taint_domain.bottom

  let state_to_string (s : state) : string = Taint_domain.to_string s

  let join_state (s1 : state) (s2 : state) : state = Taint_domain.join s1 s2

  let widen_state (_s1 : state) (s2 : state) : state = s2 (* no widening *)

  module SummaryManager = Summary.MakeManager(Taint_summary)
  type summary = Taint_summary.t

  let init_summaries s = SummaryManager.init s

  let data_instr_transfer
      (_module_ : Wasm_module.t)
      (_cfg : annot_expected Cfg.t)
      (i : annot_expected Instr.labelled_data)
      (state : state)
    : state =
    let ret (i : annot_expected Instr.labelled_data) : Var.t = match List.hd (fst i.annotation_after).vstack with
      | Some r -> r
      | None -> failwith "Taint: no return value" in
    match i.instr with
    | Nop | MemorySize | Drop | MemoryGrow -> state
    | Select ->
      let ret = ret i in
      let (_c, v2, v1) = pop3 (fst i.annotation_before).vstack in
      (* TODO: could improve precision by checking the constraints on c: if it is precisely zero/not-zero, we can only include v1 or v2 *)
      Taint_domain.add_taint_v (Taint_domain.add_taint_v state ret v1) ret v2
    | LocalGet l ->
      Taint_domain.add_taint_v state (ret i) (get_nth (fst i.annotation_before).locals l)
    | LocalSet l ->
      Taint_domain.add_taint_v state (get_nth (fst i.annotation_before).locals l) (pop (fst i.annotation_before).vstack)
    | LocalTee l ->
      Taint_domain.add_taint_v
        (Taint_domain.add_taint_v state (get_nth (fst i.annotation_before).locals l) (pop (fst i.annotation_before).vstack))
        (ret i) (get_nth (fst i.annotation_before).locals l)
    | GlobalGet g ->
      Taint_domain.add_taint_v state (ret i) (get_nth (fst i.annotation_before).globals g)
    | GlobalSet g ->
      Taint_domain.add_taint_v state (get_nth (fst i.annotation_before).globals g) (pop (fst i.annotation_before).vstack)
    | Const _ -> state
    | Binary _ | Compare _ ->
      let v1, v2 = pop2 (fst i.annotation_before).vstack in
      Taint_domain.add_taint_v
        (Taint_domain.add_taint_v state (ret i) v1)
        (ret i) v2
    | Unary _ | Test _ | Convert _ ->
      Taint_domain.add_taint_v state (ret i) (pop (fst i.annotation_before).vstack)
    | Load { offset; _ } ->
      (* Simplest case: get the taint of the entire memory.
         Refined case: get the taint of the memory cells that can pointed to, according to the previous analysis stages (i.e., relational analysis) *)
      let addr = pop (fst i.annotation_before).vstack in
      let mem = (fst i.annotation_before).memory in
      let all_locs = Var.OffsetMap.keys mem in
      (* Filter the memory location using results from the relational analysis if possible *)
      let locs = if !Taint_options.use_relational then
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
        else
          all_locs in
      (* Get the taint of possible memory location and their value.
         In practice, both the memory location and the value have the same taint
      *)
      let taints = List.map locs ~f:(fun (k, offset) ->
          Log.warn
            (Printf.sprintf "TODO: currently ignoring offsets in taints!!!\n--------------------\n--------------\n"); (* maybe only the values should be tainted, not the keys *)
          Taint_domain.Taint.join (Taint_domain.get_taint state k) (Taint_domain.get_taint state (Var.OffsetMap.find_exn mem (k, offset)))) in
      Taint_domain.add_taint
        state
        (ret i)
        (* ret is the join of all these taints *)
        (List.fold_left taints ~init:Taint_domain.Taint.bottom ~f:Taint_domain.Taint.join)
    | Store { offset; _ } ->
      (* Simplest case: set the taint for the entire memory
         Refined case: set the taint to the memory cells that can be pointed to, according to the previous analysis stages (i.e., relational analysis) *)
      let vval, vaddr = pop2 (fst i.annotation_before).vstack in
      let mem = (fst i.annotation_after).memory in
      let all_locs = Var.OffsetMap.keys mem in
      (* Refine memory locations using relational innformation, if available *)
      let locs = if !Taint_options.use_relational then
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
        else
          all_locs in
      (* Set the taint of memory locations and the value to the taint of vval *)
      List.fold_left locs ~init:state ~f:(fun s (k, offset) ->
          Log.warn (Printf.sprintf "TODO: ignoring offsets!");
          Taint_domain.add_taint_v (Taint_domain.add_taint_v s k vval)
            (Var.OffsetMap.find_exn mem (k, offset)) vval)

  let control_instr_transfer
      (module_ : Wasm_module.t) (* The wasm module (read-only) *)
      (_cfg : annot_expected Cfg.t) (* The CFG analyzed *)
      (i : annot_expected Instr.labelled_control) (* The instruction *)
      (state : state) (* The pre state *)
    : [`Simple of state | `Branch of state * state | `AnyState ] =
    let apply_summary (f : Int32.t) (arity : int * int) (state : state) : state =
      let summary = SummaryManager.get f in
      let args = List.take (fst i.annotation_before).vstack (fst arity) in
      let ret = if snd arity = 1 then List.hd (fst i.annotation_after).vstack else None in
      Taint_summary.apply summary state args (fst i.annotation_before).globals (fst i.annotation_after).globals (List.concat_map (Var.OffsetMap.to_alist (fst i.annotation_after).memory)
                                                                                                       ~f:(fun ((a, _offset), b) ->
                                                                                                           Log.warn (Printf.sprintf "TODO: ignoring offset\n");
                                                                                                           [a; b])) ret
    in
    match i.instr with
    | Call (arity, _, f) -> `Simple (apply_summary f arity state)
    | CallIndirect (arity, _, typ) ->
      (* Simplest case: all functions with the proper type can be called.
         Refined case: all functions that are deemed reachable by previous analysis stages (i.e., relational analysis) can be called *)
      let table = List.nth_exn module_.table_insts 0 in
      let funs = List.map (Table_inst.indices table) ~f:(fun idx -> (Table_inst.get table idx, idx)) in
      let ftype = Wasm_module.get_type module_ typ in
      assert (snd arity <= 1);
      (* These are all the functions with a valid type *)
      let funs_with_matching_type = List.filter_map funs ~f:(function
          | (Some fa, idx) ->
            if Stdlib.(ftype = (Wasm_module.get_func_type module_ fa)) then Some (fa, idx) else None
          | _ -> None) in
      let funs_to_apply = if !Taint_options.use_relational then
          let v = pop (fst i.annotation_before).vstack in
          List.filter funs_with_matching_type ~f:(fun (_, idx) ->
              (* Only keep the functions for which the index may match *)
              (* TODO: instead of fst, we could take the ones that are (true, false) first, and if there's none, take the ones that are (true, true) *)
              fst (Relational_domain.is_equal (snd i.annotation_before) v idx))
          else
            (* All functions with a matching types are applicable *)
            funs_with_matching_type in
      (* Apply the summaries *)
      `Simple (List.fold_left funs_to_apply
        ~init:state
        ~f:(fun acc (fa, _) ->
            Taint_domain.join (apply_summary fa arity state) acc))
    | Br _ -> `Simple state
    | BrIf _ | If _ -> `Branch (state, state)
    | Return -> `Simple state
    | Unreachable -> `AnyState
    | _ -> `Simple state

  let merge_flows (_module_ : Wasm_module.t) (cfg : annot_expected Cfg.t) (block : annot_expected Basic_block.t) (states : (int * state) list) : state =
    let init_spec = (Spec_inference.init_state cfg, Relational_transfer.bottom_state (Cfg.map_annotations cfg ~f:(fun i -> fst (Instr.annotation_before i), fst (Instr.annotation_after i)))) in
    match states with
    | [] -> init_state cfg
    | _ ->
      (* one or multiple states *)
        begin match block.content with
          | Control { instr = Merge; _ } ->
            (* block is a control-flow merge *)
            let spec = fst (Cfg.state_after_block cfg block.idx init_spec) in
            let states' = List.map states ~f:(fun (idx, s) ->
                (* get the spec after that state *)
                let spec' = fst (Cfg.state_after_block cfg idx init_spec) in
                (* equate all different variables in the post-state with the ones in the pre-state *)
                List.fold_left (Spec_inference.extract_different_vars spec spec')
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

  let summary (cfg : annot_expected Cfg.t) (out_state : state) : summary =
    let init_spec = (Spec_inference.init_state cfg, Relational_transfer.bottom_state (Cfg.map_annotations cfg ~f:(fun i -> fst (Instr.annotation_before i), fst (Instr.annotation_after i)))) in
    let exit_spec = fst (Cfg.state_after_block cfg cfg.exit_block init_spec) in
    Taint_summary.make cfg out_state
      (if List.length cfg.return_types = 1 then List.hd exit_spec.vstack else None)
      exit_spec.globals
      (List.concat_map (Var.OffsetMap.to_alist exit_spec.memory)
         ~f:(fun ((a, _), b) ->
             Log.warn
               (Printf.sprintf "ignoring offset");
             [a; b]))
end
