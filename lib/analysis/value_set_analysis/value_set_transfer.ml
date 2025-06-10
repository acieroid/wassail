open Core
open Helpers

module Make (*: Transfer.TRANSFER *) = struct
  (* type annot_expected = Abstract_store_domain.t is this what we want to print on the CFG in the dot file? *)
  type annot_expected = Spec.t

  (** The state *)
  type state = Abstract_store_domain.t
  [@@deriving sexp, compare, equal]

  let value_set_specification = () (* What is this supposed to do? *)

  let init_state (cfg : 'a Cfg.t) : state =
    Variable.Map.of_alist_exn (
      (List.mapi cfg.arg_types ~f:(fun i _ -> 
        let variable = Var.Local i in
        let var_name = Var.to_string variable in
        (Variable.Var variable, Reduced_interval_congruence.RIC.ric (0, Int 0, Int 0, (var_name, 0))))) @
      (List.mapi cfg.global_types ~f:(fun i _ -> 
        let variable = Var.Global i in
        let var_name = Var.to_string variable in
        (Variable.Var variable, Reduced_interval_congruence.RIC.ric (0, Int 0, Int 0, (var_name, 0))))))

  let bottom_state (_cfg : 'a Cfg.t) : state = Abstract_store_domain.bottom

  let state_to_string (s : state) : string = Abstract_store_domain.to_string s

  let join_state (s1 : state) (s2 : state) : state = Abstract_store_domain.join s1 s2

  let widen_state (s1 : state) (s2 : state) : state = Abstract_store_domain.widen s1 s2

  type summary = Value_set_summary.t  (* probably won't be needed *)

  let data_instr_transfer
      (_module_ : Wasm_module.t)
      (_cfg : annot_expected Cfg.t)
      (i : annot_expected Instr.labelled_data)
      (state : state)
    : state =
    (* Not sure if ret is right *)
    let ret (i : annot_expected Instr.labelled_data) : Variable.t = match List.hd (Spec.get_or_fail i.annotation_after).vstack with
      | Some r -> Variable.Var r
      | None -> failwith "nothing on the stack" in
    match i.instr with
    | Nop | MemorySize | Drop | MemoryGrow -> state
    | MemoryCopy | MemoryFill | MemoryInit _ -> state
    | RefIsNull | RefNull _ | RefFunc _ -> state
    | Select _ -> (* TODO: write this case *) state
    | LocalGet l -> 
      Abstract_store_domain.copy_value_set state 
        ~from:(Variable.Var (get_nth (Spec.get_or_fail i.annotation_before).locals l)) 
        ~to_:(ret i)
    | LocalSet l ->
      let variable = (Variable.Var (get_nth (Spec.get_or_fail i.annotation_before).locals l)) in
      let top_of_stack = pop (Spec.get_or_fail i.annotation_before).vstack in
      begin match top_of_stack with
      | Var.Const (Prim_value.I32 n) ->
        Abstract_store_domain.assign_constant_value state
        ~const:n
        ~to_:variable
      | Var.Const (Prim_value.F32 _) ->
        Abstract_store_domain.to_top_RIC state variable
      | Var.Const _ ->
        Abstract_store_domain.to_bottom_RIC state variable
      | _ ->
        Abstract_store_domain.copy_value_set state 
          ~from:(Variable.Var top_of_stack)
          ~to_:variable
      end
    | LocalTee l ->
      Abstract_store_domain.copy_value_set
        (let variable = (Variable.Var (get_nth (Spec.get_or_fail i.annotation_before).locals l)) in
          let top_of_stack = pop (Spec.get_or_fail i.annotation_before).vstack in
          begin match top_of_stack with
          | Var.Const (Prim_value.I32 n) ->
            Abstract_store_domain.assign_constant_value state
            ~const:n
            ~to_:variable
          | Var.Const (Prim_value.F32 _) ->
            Abstract_store_domain.to_top_RIC state variable
          | Var.Const _ ->
            Abstract_store_domain.to_bottom_RIC state variable
          | _ ->
            Abstract_store_domain.copy_value_set state 
              ~from:(Variable.Var top_of_stack)
              ~to_:variable
          end)
        ~from:(Variable.Var (get_nth (Spec.get_or_fail i.annotation_before).locals l))
        ~to_:(ret i)
    | GlobalGet g -> 
      Abstract_store_domain.copy_value_set state 
        ~from:(Variable.Var (get_nth (Spec.get_or_fail i.annotation_before).globals g)) 
        ~to_:(ret i)
    | GlobalSet g ->
      let variable = (Variable.Var (get_nth (Spec.get_or_fail i.annotation_before).globals g)) in
      let top_of_stack = pop (Spec.get_or_fail i.annotation_before).vstack in
      begin match top_of_stack with
      | Var.Const (Prim_value.I32 n) ->
        Abstract_store_domain.assign_constant_value state
        ~const:n
        ~to_:variable
      | Var.Const (Prim_value.F32 _) ->
        Abstract_store_domain.to_top_RIC state variable
      | Var.Const _ ->
        Abstract_store_domain.to_bottom_RIC state variable
      | _ ->
        Abstract_store_domain.copy_value_set state 
          ~from:(Variable.Var top_of_stack)
          ~to_:variable
      end
    | Const _ -> state
    | Binary _ | Compare _ -> (* TODO: write this case *) state
    | Unary _ | Test _ | Convert _ -> (* TODO: write this case *) state
    | Load { offset = _offset; _ } -> (* TODO: write this case *) state
    | Store { offset = _offset; _ } -> (* TODO: write this case *) state

  let control_instr_transfer
      (_module_ : Wasm_module.t) (* The wasm module (read-only) *)
      (_summaries : summary Int32Map.t) (* Probably won't need this *)
      (_cfg : annot_expected Cfg.t) (* The CFG analized *)
      (i : annot_expected Instr.labelled_control) (* The instruction *)
      (state : state) (* the pre-state *)
    : [`Simple of state | `Branch of state * state] =
    match i.instr with
    | Call _ -> failwith "function calls not yet implemented"
    | CallIndirect _ -> failwith "indirect calls not yet implemented"
    | Br _ -> `Simple state
    | BrIf _ | If _ -> `Branch (state, state) (* Not sure if that's right. What about if V1 < V2 ? *)
    | Return -> `Simple state
    | Unreachable -> `Simple  Abstract_store_domain.bottom
    | _ -> `Simple state

  let merge_flows 
      (_module_ : Wasm_module.t) 
      (_cfg : annot_expected Cfg.t) 
      (_block : annot_expected Basic_block.t)
      (_states : (int * state) list) 
    : state =
    (* TODO: Write the rest of this function *)
    bottom_state _cfg


  (** [summary cfg out_state] computes a taint summary for a function based on its final state and annotations at the function exit. *)
  let summary (cfg : annot_expected Cfg.t) (out_state : state) : summary =
    let init_spec = (Spec_inference.init_state cfg) in
    match Cfg.state_after_block cfg cfg.exit_block init_spec with
    | Bottom ->
      (* The function exit is likely unreachable, so we use a bottom summary *)
      { ret = None;
        globals = List.init (List.length cfg.global_types) ~f:(fun _ -> Reduced_interval_congruence.RIC.Bottom);
        mem = Abstract_store_domain.bottom; }
    | NotBottom exit_spec ->
      Value_set_summary.make cfg out_state
        (if List.length cfg.return_types = 1 then match (List.hd exit_spec.vstack) with | Some r -> Some (Variable.Var r) | None -> None else None)
        (List.map ~f:(fun v -> Variable.Var v) exit_spec.globals)
        [] (* TODO: complete this section *)
        (* (List.concat_map (Var.OffsetMap.to_alist exit_spec.memory)
           ~f:(fun ((a, _), b) -> [a; b])) *)

  (** [extract_summary cfg analyzed_cfg] extracts the taint summary from an analyzed control-flow graph. *)
  let extract_summary (cfg : annot_expected Cfg.t) (analyzed_cfg : state Cfg.t) : summary =
    let out_state = Cfg.state_after_block analyzed_cfg cfg.exit_block (init_state cfg) in
    summary cfg out_state
  

end