open Core_kernel

module Make = functor (Spec : Spec_inference.SPEC) (RelSpec : Relational_spec.SPEC) -> struct
  (* TODO: 
     1. Taint summaries need to preserve information about memory.
     2. When applied, taint summary should rely on relational results to apply on the memory
*)

  type state = Taint_domain.t
  [@@deriving compare]

  module SummaryManager = Summary.MakeManager(Taint_summary)
  type summary = Taint_summary.t

  (** Set to true to refine the analysis with the results from the relational analysis *)
  let use_relational = ref false

  (** In the initial state, we only set the taint for for parameters and the globals. *)
  let init_state (cfg : Cfg.t) : state =
    Spec_inference.VarMap.of_alist_exn
      ((List.mapi cfg.arg_types ~f:(fun i _ -> (Spec_inference.Local i,
                                                Taint_domain.taint (Spec_inference.Local i)))) @
       (List.mapi cfg.global_types ~f:(fun i _ -> (Spec_inference.Global i,
                                                 Taint_domain.taint (Spec_inference.Global i)))))

  let bottom_state (_cfg : Cfg.t) : state = Taint_domain.bottom

  let state_to_string (s : state) : string = Taint_domain.to_string s

  let join_state (s1 : state) (s2 : state) : state = Taint_domain.join s1 s2

  let widen (_s1 : state) (s2 : state) : state = s2 (* no widening *)

  let init_summaries s = SummaryManager.init s

  let data_instr_transfer (_module_ : Wasm_module.t) (_cfg : Cfg.t) (i : Instr.data Instr.labelled) (state : state) : state =
    match i.instr with
    | Nop | MemorySize | Drop | MemoryGrow -> state
    | Select ->
      let ret = Spec.ret i.label in
      let (_c, v2, v1) = Spec.pop3 (Spec.pre i.label).vstack in
      (* TODO: could improve precision by checking the constraints on c: if it is precisely zero/not-zero, we can only include v1 or v2 *)
      Taint_domain.add_taint_v (Taint_domain.add_taint_v state ret v1) ret v2
    | LocalGet l ->
      Taint_domain.add_taint_v state (Spec.ret i.label) (Spec.get_nth (Spec.pre i.label).locals l)
    | LocalSet l ->
      Taint_domain.add_taint_v state (Spec.get_nth (Spec.pre i.label).locals l) (Spec.pop (Spec.pre i.label).locals)
    | LocalTee l ->
      Taint_domain.add_taint_v
        (Taint_domain.add_taint_v state (Spec.get_nth (Spec.pre i.label).locals l) (Spec.pop (Spec.pre i.label).locals))
        (Spec.ret i.label) (Spec.get_nth (Spec.pre i.label).locals l)
    | GlobalGet g ->
        Taint_domain.add_taint_v state (Spec.ret i.label) (Spec.get_nth (Spec.pre i.label).globals g)
    | GlobalSet g ->
      Taint_domain.add_taint_v state (Spec.get_nth (Spec.pre i.label).globals g) (Spec.pop (Spec.pre i.label).globals)
    | Const _ -> state
    | Binary _ | Compare _ ->
      let v1, v2 = Spec.pop2 (Spec.pre i.label).vstack in
      Taint_domain.add_taint_v
        (Taint_domain.add_taint_v state (Spec.ret i.label) v1)
        (Spec.ret i.label) v2
    | Unary _ | Test _ | Convert _ ->
      Taint_domain.add_taint_v state (Spec.ret i.label) (Spec.pop (Spec.pre i.label).vstack)
    | Load { offset; _ } ->
      (* Simplest case: get the taint of the entire memory.
         Refined case: get the taint of the memory cells that can pointed to, according to the previous analysis stages (i.e., relational analysis) *)
      let addr = Spec.pop (Spec.pre i.label).vstack in
      let mem = (Spec.pre i.label).memory in
      let all_locs = Spec_inference.VarMap.keys mem in
      (* Filter the memory location using results from the relational analysis if possible *)
      let locs = if !use_relational then
          (* We need to filter locs to only have the locs that can be loaded.
             This means for each loc, we can ask the relational domain if are_equal loc v (where v is the top of the stack.
             If some are truly equal, we know we can only keep these. Otherwise, if some maybe equal, then these have to be kept. *)
          let equal = List.filter all_locs ~f:(fun loc -> match Relational_domain.are_equal_offset (RelSpec.pre i.label) loc addr offset with
              | (true, false) -> true
              | _ -> false) in
          if not (List.is_empty equal) then
            (* There are addresses that are definitely equal to addr, so we get their taint *)
            equal
          else
            (* No address is definitely equal to addr, so we take the ones that may be equal *)
            List.filter all_locs ~f:(fun loc -> match Relational_domain.are_equal_offset (RelSpec.pre i.label) loc addr offset with
                | (true, _) -> true
                | _ -> false)
        else
          all_locs in
      (* Get the taint of possible memory location and their value.
         In practice, both the memory location and the value have the same taint
      *)
      let taints = List.map locs ~f:(fun k -> Taint_domain.join_taint (Taint_domain.get_taint state k) (Taint_domain.get_taint state (Spec_inference.VarMap.find_exn mem k))) in
      Taint_domain.add_taint
        state
        (Spec.ret i.label)
        (* ret is the join of all these taints *)
        (List.fold_left taints ~init:Taint_domain.taint_bottom ~f:Taint_domain.join_taint)
    | Store { offset; _ } ->
      (* Simplest case: set the taint for the entire memory
         Refined case: set the taint to the memory cells that can be pointed to, according to the previous analysis stages (i.e., relational analysis) *)
      let vval, vaddr = Spec.pop2 (Spec.pre i.label).vstack in
      let mem = (Spec.post i.label).memory in
      let all_locs = Spec_inference.VarMap.keys mem in
      (* Refine memory locations using relational innformation, if available *)
      let locs = if !use_relational then
          let equal = List.filter all_locs ~f:(fun loc -> match Relational_domain.are_equal_offset (RelSpec.pre i.label) loc vaddr offset with
              | (true, false) -> true
              | _ -> false) in
          if not (List.is_empty equal) then
            (* There are addresses that are definitely equal to addr, so we get their taint *)
            equal
          else
            (* No address is definitely equal to addr, so we take the ones that may be equal *)
            List.filter all_locs ~f:(fun loc -> match Relational_domain.are_equal_offset (RelSpec.pre i.label) loc vaddr offset with
                | (true, _) -> true
                | _ -> false)
        else
          all_locs in
      (* Set the taint of memory locations and the value to the taint of vval *)
      List.fold_left locs ~init:state ~f:(fun s k ->
          Taint_domain.add_taint_v (Taint_domain.add_taint_v s k vval)
            (Spec_inference.VarMap.find_exn mem k) vval)

  let control_instr_transfer
      (module_ : Wasm_module.t) (* The wasm module (read-only) *)
      (_cfg : Cfg.t) (* The CFG analyzed *)
      (i : Instr.control Instr.labelled) (* The instruction *)
      (state : state) (* The pre state *)
    : [`Simple of state | `Branch of state * state] =
    let apply_summary (f : int) (arity : int * int) (state : state) : state =
      let summary = SummaryManager.get f in
      let args = List.take (Spec.pre i.label).vstack (fst arity) in
      let ret = if snd arity = 1 then List.hd (Spec.post i.label).vstack else None in
      let globals = (Spec.post i.label).globals in
      Taint_summary.apply summary state args globals ret
    in
    match i.instr with
    | Call (arity, f) ->
      `Simple (apply_summary f arity state)
    | CallIndirect (arity, typ) ->
      (* Simplest case: all functions with the proper type can be called.
         Refined case: all functions that are deemed reachable by previous analysis stages (i.e., relational analysis) can be called *)
      let table = List.nth_exn module_.tables 0 in
      let funs = List.map (Table_inst.indices table) ~f:(fun idx -> (Table_inst.get table idx, idx)) in
      let ftype = Wasm_module.get_type module_ typ in
      assert (snd arity <= 1);
      (* These are all the functions with a valid type *)
      let funs_with_matching_type = List.filter_map funs ~f:(function
          | (Some fa, idx) ->
            if Stdlib.(ftype = (Wasm_module.get_func_type module_ fa)) then Some (fa, idx) else None
          | _ -> None) in
      let funs_to_apply = if !use_relational then
          let v = Spec.pop (Spec.pre i.label).vstack in
          List.filter funs_with_matching_type ~f:(fun (_, idx) ->
              (* Only keep the functions for which the index may match *)
              (* TODO: instead of fst, we could take the ones that are (true, false) first, and if there's none, take the ones that are (true, true) *)
              fst (Relational_domain.is_equal (RelSpec.pre i.label) v idx))
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
    | Unreachable -> `Simple state
    | _ -> `Simple state

  let merge_flows (_module_ : Wasm_module.t) (cfg : Cfg.t) (block : Basic_block.t) (states : (int * state) list) : state =
    match states with
    | [] -> init_state cfg
    | _ ->
      (* one or multiple states *)
        begin match block.content with
          | ControlMerge ->
            (* block is a control-flow merge *)
            let spec = Spec.post_block block.idx in
            let states' = List.map states ~f:(fun (idx, s) ->
                (* get the spec after that state *)
                let spec' = Spec.post_block idx in
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

  let summary (cfg : Cfg.t) (out_state : state) : summary =
    Taint_summary.make cfg out_state
      (if List.length cfg.return_types = 1 then List.hd (Spec.post_block cfg.exit_block).vstack else None)
      (Spec.post_block cfg.exit_block).globals
      (List.concat_map (Spec_inference.VarMap.to_alist (Spec.post_block cfg.exit_block).memory)
         ~f:(fun (a, b) -> [a; b]))
end
