open Core
open Helpers
open Reduced_interval_congruence

module Make (*: Transfer.TRANSFER *) = struct
  module State = Abstract_store_domain
  module Summary = Value_set_summary
  module Value = State.Value

  type summary = Summary.t
  type ric = RIC.t
  type bitfield = Bitfield.t
  type boolean = Boolean.t
  let top = Value.top
  let bottom : State.t = State.bottom 
  
  (* We need the variable names as annotations *)
  type annot_expected = Spec_domain.t

  (** [init module_ funcinst] builds the initial value-set state for [funcinst].
      I32 arguments and globals receive relative RIC values, I32 locals start at
      zero, other values are set to top, and memory is initialized to top. *)
  let init (module_ : Wasm_module.t) (funcinst : Func_inst.t) : State.t =
    let arg_types, _ = funcinst.typ in
    let nb_of_arguments = Func_inst.nargs funcinst in
    let global_types = Wasm_module.get_global_types module_ in
    let init_variable (variable : Variable.t) : Variable.t * Value.t =
      let var_name = variable |> Variable.to_string in
      (variable, Value.ValueSet RIC.(relative_ric var_name))
    in
    { abstract_store =
        Variable.Map.of_alist_exn (
          (* arguments *)
          (List.mapi arg_types ~f:(fun i type_ -> 
            let variable = Variable.Var (Var.Local i) in
            match type_ with
            | I32 -> init_variable variable
            | _ -> (variable, top))) @
          (* gloabls *)
          (List.mapi global_types ~f:(fun i type_ -> 
            let variable = Variable.Var (Var.Global i) in
            match type_ with
            | I32 -> init_variable variable
            | _ -> (variable, top))) @
          (* locals *)
          (List.mapi funcinst.code.locals ~f:(fun i type_ -> 
            let variable = Variable.Var (Var.Local (i + nb_of_arguments)) in
            match type_ with
            | I32 -> (variable, Value.ValueSet RIC.zero)
            | _ -> (variable, top))) @
          [(Variable.entire_memory), top]);
      store_operations = RICSet.empty;
      unreachable = false }

  (** [join_state s1 s2] joins two abstract states. *)
  let join_state : State.t -> State.t -> State.t = State.join

  (** [is_this_the_value_of spec_state ~value ~of_this_variable] checks whether
      [value] is the current SSA value associated with the given local or global
      in [spec_state]. *)
  let is_this_the_value_of 
      (state : Spec_domain.SpecWithoutBottom.t) 
      ~(value : Variable.t) 
      ~(of_this_variable : Variable.t)
    : bool =
    match of_this_variable with
    | Var Var.Local l ->
      if l < List.length state.locals then
        Variable.(value = Var (Spec_inference.get (Int32.of_int_exn l) state.locals))
      else
        false
    | Var Var.Global g ->
      if g < List.length state.globals then
        Variable.(value = Var (Spec_inference.get (Int32.of_int_exn g) state.globals))
      else
        false
    | _ -> false

  (** [data module_ cfg i state] applies the value-set transfer function for one
      data instruction. It updates variables, memory, and traces according to
      the instruction semantics. *)
  let data
      (_module_ : Wasm_module.t)
      (_cfg : annot_expected Cfg.t)
      (i : annot_expected Instr.labelled_data)
      (state : State.t)
    : State.t =
    Print_trace.instruction i.line_number (Data i) state.unreachable;
    if state.unreachable then
      state
    else
      ((** [ret i] returns the SSA variable produced by data instruction [i]. *)
      let ret (i : annot_expected Instr.labelled_data) : Variable.t = 
        match (Spec_domain.get_or_fail i.annotation_after).vstack with
        | ret :: _ -> Variable.Var ret
        | [] -> assert false
      in
      (** [set variable] assigns the top stack value to [variable]. *)
      let set (variable : Variable.t) : State.t =
        let invalid_pointer_type (type_ : string) (variable : Variable.t) : State.t =
          Print_trace.invalid_pointer_type type_;
          State.to_top_RIC state variable
        in
        let top_of_stack = pop (Spec_domain.get_or_fail i.annotation_before).vstack in
        begin match top_of_stack with
        | Var.Const (Prim_value.I32 n) ->
          Print_trace.assign_const n variable;
          State.assign_constant_value state ~const:n ~to_:variable
        | Var.Const (Prim_value.F32 _) -> invalid_pointer_type "Float32" variable
        | Var.Const (Prim_value.I64 _) -> invalid_pointer_type "Int64" variable
        | Var.Const (Prim_value.F64 _) -> invalid_pointer_type "Float64" variable
        | Var.Local _ | Var.Global _ ->
          let vs = Value.ValueSet (RIC.relative_ric (Var.to_string top_of_stack)) in
          Print_trace.assign vs variable;
          State.set state ~var:variable ~vs
        | _ ->
          Print_trace.copy_value top_of_stack variable;
          State.copy_value_set state ~from:(Variable.Var top_of_stack) ~to_:variable
        end
      in
      match i.instr with
      | MemorySize -> State.set state ~var:(ret i) ~vs:(State.get state ~var:Variable.MemorySize)
      | MemoryGrow -> 
        (* TODO: memory.grow can fail and return -1!!!! *)
        state 
          |> State.set ~var:(ret i) ~vs:(State.get state ~var:Variable.MemorySize)
          |> State.set 
            ~var:Variable.MemorySize 
            ~vs:(Value.(
              (State.get state ~var:Variable.MemorySize)
              + (State.get state ~var:(Variable.Var (pop (Spec_domain.get_or_fail i.annotation_before).vstack)))))
      | Nop | Drop -> state
      | MemoryCopy -> 
        (* TODO: if needed, we could eventually increase precision. See 
           Abstract_store_domain.memory_copy for implementation stub *)
        { (state |> State.set ~var:Variable.entire_memory ~vs:top
                 |> State.set ~var:Variable.Accessed ~vs:top)
            with store_operations = RICSet.singleton RIC.Top }
      | MemoryInit _ | MemoryFill ->
        (* TODO: if needed, we could eventually increase precision. *)
        { (state |> State.set ~var:Variable.entire_memory ~vs:top)
            with store_operations = RICSet.singleton RIC.Top }
      | RefIsNull | RefNull _ | RefFunc _ -> state
      | Select _ ->
        let condition, x, y =
          (match (Spec_domain.get_or_fail i.annotation_before).vstack with
          | condition :: x :: y :: _ -> condition, x, y
          | _ -> assert false)
        in
        let cond_vs = State.get state ~var:(Variable.Var condition) in
        let x_vs = State.get state ~var:(Variable.Var x) in
        let y_vs = State.get state ~var:(Variable.Var y) in
        let result = 
          if Value.may_be_false cond_vs then
            x_vs 
            |> Value.join
                (if Value.may_be_true cond_vs then
                  y_vs
                else
                  Value.bottom)
          else
            if Value.may_be_true cond_vs then
              y_vs
            else
              (* bottom condition is unreachable *)
              (Log.error "Select: condition can't be true nor false"; assert false )
        in
        Print_trace.select cond_vs x_vs y_vs result;
        State.set state ~var:(ret i) ~vs:result
      | LocalGet l -> 
        Print_trace.get ~global:false l state ~get:(fun state var -> State.get state ~var);
        state
      | LocalSet l -> Variable.Var (Var.Local (Int32.to_int_exn l)) |> set
      | LocalTee l -> Variable.Var (Var.Local (Int32.to_int_exn l)) |> set
      | GlobalGet g -> 
        Print_trace.get ~global:true g state ~get:(fun state var -> State.get state ~var);
        state |> State.set ~var:(ret i) ~vs:(state |> State.get ~var:(Variable.Var (Var.Global (Int32.to_int_exn g))))
      | GlobalSet g -> Variable.Var (Var.Global (Int32.to_int_exn g)) |> set
      | Const c ->
        begin match c with
        | Prim_value.I32 _ -> state
        | _ -> Print_trace.non_i32 (); state
        end
      | Binary binop -> 
        let x, y = pop2 (Spec_domain.get_or_fail i.annotation_before).vstack in
        let result = ret i in
        begin match binop with
        | { op = ShrU; typ = I32 } -> State.shr_u state (Variable.Var x) (Variable.Var y) result
        | { op = ShrS; typ = I32 } -> State.shr_s state (Variable.Var x) (Variable.Var y) result
        | { op = Shl; typ = I32 } -> State.shl state (Variable.Var x) (Variable.Var y) result
        | { op = And; typ = I32 } -> State.and_ state (Variable.Var x) (Variable.Var y) result
        | { op = Or; typ = I32 } -> State.or_ state (Variable.Var x) (Variable.Var y) result
        | { op = Xor; typ = I32 } -> State.xor_ state (Variable.Var x) (Variable.Var y) result
        | { op = Add; typ = I32 } -> State.i32_add state (Variable.Var x) (Variable.Var y) result
        | { op = Sub; typ = I32 } -> State.i32_sub state ~subtract_this:(Variable.Var x) ~from:(Variable.Var y) result
        | _ -> (* other operations result in a pointer that can point anywhere *)
          Print_trace.imprecise_operation ();
          State.to_top_RIC state result
        end
      | Load load ->
        state |> State.load ~instruction:load ~annotation_before:i.annotation_before ~result:(ret i)
      | Store store ->
        state |> State.store ~instruction:store ~annotation_before:i.annotation_before
      | Compare comp ->
        let var2, var1 = pop2 (Spec_domain.get_or_fail i.annotation_before).vstack in
        let vs1 = State.get state ~var:(Variable.Var var1) in
        let vs2 = State.get state ~var:(Variable.Var var2) in
        let vs1, vs2 =
          begin match vs1, vs2 with
          | Boolean {numeric_value = vs1; _}, Boolean {numeric_value = vs2; _}
          | Boolean {numeric_value = vs1; _}, ValueSet vs2
          | ValueSet vs1, Boolean {numeric_value = vs2; _}
          | ValueSet vs1, ValueSet vs2 -> vs1, vs2
          | Bitfield bf, ValueSet vs2
          | Bitfield bf, Boolean {numeric_value = vs2; _} -> (RIC.of_bitfield bf), vs2
          | ValueSet vs1, Bitfield bf
          | Boolean {numeric_value = vs1; _}, Bitfield bf -> vs1, (RIC.of_bitfield bf)
          | Bitfield bf1, Bitfield bf2 -> RIC.of_bitfield bf1, RIC.of_bitfield bf2
          end
        in
        begin match comp with
        | {op = Ne; typ = I32} ->
          let result = Value.are_equal_or_not ~not_equal:true (var1, vs1) (var2, vs2) in
          Print_trace.comp vs1 vs2 result "!=";
          state |> State.set ~var:(ret i) ~vs:result
        | {op = Eq; typ = I32} ->
          let result = Value.are_equal_or_not (var1, vs1) (var2, vs2) in
          Print_trace.comp vs1 vs2 result "==";
          state |> State.set ~var:(ret i) ~vs:result
        | {op = LeS; typ = I32} -> 
          let result = Value.less_or_equal (var1, vs1) (var2, vs2) in
          Print_trace.comp vs1 vs2 result "≤";
          state |> State.set ~var:(ret i) ~vs:result
        | {op = LtS; typ = I32} ->
          let result = Value.less_than (var1, vs1) (var2, vs2) in
          Print_trace.comp vs1 vs2 result "<";
          state |> State.set ~var:(ret i) ~vs:result
        | {op = GeS; typ = I32} -> 
          let result = Value.less_or_equal (var2, vs2) (var1, vs1) in
          Print_trace.comp vs1 vs2 result "≥";
          state |> State.set ~var:(ret i) ~vs:result
        | {op = GtS; typ = I32} ->
          let result = Value.less_than (var2, vs2) (var1, vs1) in
          Print_trace.comp vs1 vs2 result ">";
          state |> State.set ~var:(ret i) ~vs:result
        | {op = LeU; typ = I32} -> 
          let result : Value.t = 
            if not (RIC.comparable_offsets vs1 vs2) || (not (String.is_empty (RIC.extract_relative_offset vs1))) then
              ValueSet RIC.(join one zero)
            else if (* neg <= pos *) RIC.(positive_part vs1 = Bottom && negative_part vs2 = Bottom) then
              Value.less_or_equal (var2, vs2) (var1, vs1)
            else if (* pos <= neg *) RIC.(negative_part vs1 = Bottom && positive_part vs2 = Bottom) then
              Value.less_or_equal (var2, vs2) (var1, vs1)
            else if (* pos <= pos *) RIC.(negative_part vs1 = Bottom && negative_part vs2 = Bottom) then
              Value.less_or_equal (var1, vs1) (var2, vs2)
            else if (* neg <= neg *) RIC.(positive_part vs1 = Bottom && positive_part vs2 = Bottom) then
              Value.less_or_equal (var1, vs1) (var2, vs2)
            else (* Mix of positives and negatives *)
              ValueSet RIC.(join one zero)
          in
          Print_trace.comp vs1 vs2 result "≤";
          state |> State.set ~var:(ret i) ~vs:result
        | {op = LtU; typ = I32} -> 
          let result : Value.t =
            if not (RIC.comparable_offsets vs1 vs2) || (not (String.is_empty (RIC.extract_relative_offset vs1))) then
              ValueSet RIC.(join one zero)
            else if (* neg < pos *) RIC.(positive_part vs1 = Bottom && negative_part vs2 = Bottom) then
              Value.less_than (var2, vs2) (var1, vs1)
            else if (* pos < neg *) RIC.(negative_part vs1 = Bottom && positive_part vs2 = Bottom) then
              Value.less_than (var2, vs2) (var1, vs1)
            else if (* pos < pos *) RIC.(negative_part vs1 = Bottom && negative_part vs2 = Bottom) then
              Value.less_than (var1, vs1) (var2, vs2)
            else if (* neg < neg *) RIC.(positive_part vs1 = Bottom && positive_part vs2 = Bottom) then
              Value.less_than (var1, vs1) (var2, vs2)
            else (* Mix of positives and negatives *)
              ValueSet RIC.(join one zero)
          in
          Print_trace.comp vs1 vs2 result "<";
          state |> State.set ~var:(ret i) ~vs:result
        | {op = GeU; typ = I32} -> 
          let result : Value.t = 
            if not (RIC.comparable_offsets vs1 vs2) || (not (String.is_empty (RIC.extract_relative_offset vs1))) then
              ValueSet RIC.(join one zero)
            else if (* neg >= pos *) RIC.(positive_part vs1 = Bottom && negative_part vs2 = Bottom) then
              Value.less_or_equal (var1, vs1) (var2, vs2)
            else if (* pos >= neg *) RIC.(negative_part vs1 = Bottom && positive_part vs2 = Bottom) then
              Value.less_or_equal (var1, vs1) (var2, vs2)
            else if (* pos >= pos *) RIC.(negative_part vs1 = Bottom && negative_part vs2 = Bottom) then
              Value.less_or_equal (var2, vs2) (var1, vs1)
            else if (* neg >= neg *) RIC.(positive_part vs1 = Bottom && positive_part vs2 = Bottom) then
              Value.less_or_equal (var2, vs2) (var1, vs1)
            else (* Mix of positives and negatives *)
              ValueSet RIC.(join one zero)
          in
          Print_trace.comp vs1 vs2 result "≥";
          state |> State.set ~var:(ret i) ~vs:result
        | {op = GtU; typ = I32} -> 
          let result : Value.t =
            if not (RIC.comparable_offsets vs1 vs2) || (not (String.is_empty (RIC.extract_relative_offset vs1))) then
              ValueSet RIC.(join one zero)
            else if (* neg > pos *) RIC.(positive_part vs1 = Bottom && negative_part vs2 = Bottom) then
              Value.less_than (var1, vs1) (var2, vs2)
            else if (* pos > neg *) RIC.(negative_part vs1 = Bottom && positive_part vs2 = Bottom) then
              Value.less_than (var1, vs1) (var2, vs2)
            else if (* pos > pos *) RIC.(negative_part vs1 = Bottom && negative_part vs2 = Bottom) then
              Value.less_than (var2, vs2) (var1, vs1)
            else if (* neg > neg *) RIC.(positive_part vs1 = Bottom && positive_part vs2 = Bottom) then
              Value.less_than (var2, vs2) (var1, vs1)
            else (* Mix of positives and negatives *)
              ValueSet RIC.(join one zero)
          in
          Print_trace.comp vs1 vs2 result ">";
          state |> State.set ~var:(ret i) ~vs:result
        | _ -> State.set state ~var:(ret i) ~vs:(ValueSet RIC.(join one zero))
        end
      | Test test -> 
        begin match test with
        | I32Eqz ->
          let var = pop (Spec_domain.get_or_fail i.annotation_before).vstack in
          let vs = State.get state ~var:(Variable.Var var) in
          state |> State.set ~var:(ret i) ~vs:(Value.eqz ~var:(Variable.Var var) vs)
        | I64Eqz -> state |> State.set ~var:(ret i) ~vs:(ValueSet RIC.(join one zero))
        end
      | Unary { typ = I64 | F32 | F64; _ }
      | Unary { op = ExtendS _; typ = I32; _ } ->
        state |> State.set ~var:(ret i) ~vs:top
      | Unary { op=(Neg|Abs|Ceil|Floor|Trunc|Nearest|Sqrt); _ } -> assert false
      | Unary { op = Clz; typ = I32; _ } ->
        State.unary_op state i.annotation_before Value.count_leading_zeros "CLZ" (ret i)
      | Unary { op = Ctz; typ = I32; _ } ->
        State.unary_op state i.annotation_before Value.count_trailing_zeros "CTZ" (ret i)
      | Unary { op = Popcnt; typ = I32; _ } ->
        State.unary_op state i.annotation_before Value.population_count "POP COUNT" (ret i)
      | Convert _ ->
        state |> State.set ~var:(ret i) ~vs:top)


  (** [apply_condition state ~condition spec_state] splits [state] according to
      a boolean condition. It returns the states for the true and false branches,
      applying the refinements carried by the boolean value. *)
  let apply_condition 
      (state : State.t) 
      ~(condition : Variable.t * Boolean.t)
      (spec_state : Spec_domain.SpecWithoutBottom.t) 
    : State.t * State.t =
    let var, boolean = condition in
    let state = { state with abstract_store = Variable.Map.remove state.abstract_store var } in
    let locals_and_globals = state |> State.extract_locals_and_globals in
    let true_ = 
      if Boolean.may_be_true boolean then
        let true_ =
          State.make_compatible 
            ~this_store:
              { state with abstract_store = (Variable.Map.map ~f:(fun x -> (Value.ValueSet x.true_)) boolean.true_or_false) }
            ~relative_to:state
        in
        { state with abstract_store =
            Variable.Map.fold true_.abstract_store
              ~init:state.abstract_store 
              ~f:(fun ~key ~data acc -> 
                let acc = Variable.Map.set ~key ~data acc in
                List.fold locals_and_globals
                  ~init:acc
                  ~f:(fun acc v -> 
                    if is_this_the_value_of spec_state ~value:key ~of_this_variable:v then
                      Variable.Map.set ~key:v ~data acc
                    else 
                      acc)) }
      else
        { State.bottom with unreachable = true }
    in
    let false_ = 
      if Boolean.may_be_false boolean then
        let false_ =
          State.make_compatible 
            ~this_store:
              { state with abstract_store = (Variable.Map.map ~f:(fun x -> (Value.ValueSet x.false_)) boolean.true_or_false) }
            ~relative_to:state
        in
        { state with abstract_store =
            Variable.Map.fold false_.abstract_store
              ~init:state.abstract_store 
              ~f:(fun ~key ~data acc -> 
                let acc = Variable.Map.set ~key ~data acc in
                List.fold locals_and_globals
                  ~init:acc
                  ~f:(fun acc v -> 
                    if is_this_the_value_of spec_state ~value:key ~of_this_variable:v then
                      Variable.Map.set ~key:v ~data acc
                    else
                      acc)) }
      else
        { State.bottom with unreachable = true }
    in
    true_, false_




  (** [control module_ cfg i state] applies the value-set transfer function for
      one control instruction. Branching instructions return refined true and
      false states; other instructions return a single successor state. *)
  let control
      (_module_ : Wasm_module.t) (* The wasm module (read-only) *)
      (cfg : annot_expected Cfg.t) (* The CFG analized *)
      (i : annot_expected Instr.labelled_control) (* The instruction *)
      (state : State.t) (* the pre-state *)
    : [`Simple of State.t | `Branch of State.t * State.t] =
    Print_trace.instruction i.line_number (Control i) state.unreachable;
    match i.instr with
    | Br _ -> `Simple state
    | BrIf _ | If _ -> 
      let condition = Variable.Var (pop (Spec_domain.get_or_fail i.annotation_before).vstack) in
      let boolean_value = state |> State.get ~var:condition in
      begin match boolean_value with
      | ValueSet vs -> 
        let false_ = 
          if RIC.may_be_false vs then
            state
          else
            { State.bottom with unreachable = true }
        and true_ = 
          if RIC.may_be_true vs then
            state
          else
            { State.bottom with unreachable = true }
        in
        `Branch (true_, false_)
      | Boolean boolean_value -> 
        `Branch (apply_condition state ~condition:(condition, boolean_value) (Spec_domain.get_or_fail i.annotation_after))
      | Bitfield bf ->
        let false_ = 
          if Bitfield.may_be_false bf then
            state
          else
            { State.bottom with unreachable = true }
        and true_ = 
          if Bitfield.may_be_true bf then
            state
          else
            { State.bottom with unreachable = true }
        in
        `Branch (true_, false_)
      end
    | Return -> 
      `Simple 
        ((Spec_domain.get_or_fail i.annotation_before).vstack
        |> List.mapi 
            ~f:(fun i r -> 
              Variable.Var (Var.Return (cfg.idx, Int32.of_int_exn i)), 
              state |> State.get ~var:(Variable.Var r))
        |> List.fold ~init:state ~f:(fun state (ret, value) -> 
          Print_trace.return ret value;
          state |> State.set ~var:ret ~vs:value))
    | Unreachable -> `Simple  { State.bottom with unreachable = true }
    | _ -> `Simple state

  (** [apply_summary module_ f arity i state summary] applies [summary] at a call
      to function [f], using the current stack arguments and return variables. *)
  let apply_summary 
      (_module_ : Wasm_module.t)
      (f : Int32.t) 
      (arity : int * int) 
      (i : annot_expected Instr.labelled_call)
      (state : State.t)
      (summary : summary)
    : State.t =
    Print_trace.apply_summary state State.to_string f summary Summary.to_string;
    let spec_before = Spec_domain.get_or_fail i.annotation_before in
    let args = List.take spec_before.vstack (fst arity) in
    let spec_after = Spec_domain.get_or_fail i.annotation_after in
    let return_variables = List.take spec_after.vstack (snd arity) in
    Value_set_summary.apply
      ~summary
      ~state
      ~args
      ~return_variables

  (** [merge_variables module_ cfg block predecessors state] assigns merged SSA
      variables at [block]. Values coming from unreachable predecessors are
      ignored. *)
  let merge_variables
      (module_ : Wasm_module.t)
      (cfg : annot_expected Cfg.t) 
      (block : annot_expected Basic_block.t)
      (predecessors : ('a Basic_block.t * State.t) list)
      (state : State.t)
    : State.t =
    Spec_inference.new_merge_variables_with_origin module_ cfg block
    |> List.filter_map ~f:(fun (pred_idx, old_var, merge_var) ->
      (* state at the end of predecessor block where [old_var] was created *)
      match List.find_exn predecessors ~f:(fun (pred_block, _) -> pred_block.idx = pred_idx) with
      | _, pred_state -> 
        if pred_state.unreachable then
          None
        else
          Some (Variable.Var merge_var, State.get state ~var:(Variable.Var old_var)))
    |> List.fold ~init:[] ~f:(fun acc (v, vs) -> 
      match List.Assoc.find acc ~equal:Variable.equal v with
      | None -> (v, vs) :: acc
      | Some existing_vs ->
        let acc = List.Assoc.remove acc ~equal:Variable.equal v in
        (v, Value.join vs existing_vs) :: acc)
    |> List.fold ~init:state ~f:(fun acc (v, vs) -> State.set acc ~var:v ~vs)


  (** [merge_flows module_ cfg block predecessors] computes the input state of
      [block] from its predecessor states. Merge, entry, and return blocks join
      their predecessors; ordinary blocks must have a single predecessor. *)
  let merge_flows 
      (module_ : Wasm_module.t) 
      (cfg : annot_expected Cfg.t) 
      (block : annot_expected Basic_block.t)
      (predecessors : ('a Basic_block.t * State.t) list)
    : State.t =
    match predecessors with
    | [] -> 
      Print_trace.start_of_function block.idx;
      bottom
    | _ ->
      begin match block.content with
      | Control { instr = Merge; _ }
      | Entry | Return _ ->
        Print_trace.control_block (Some cfg) block.idx;
        predecessors
        |> List.map ~f:snd
        |> List.reduce_exn ~f:join_state
        |> merge_variables module_ cfg block predecessors
      | Control _ ->
        Print_trace.control_block None block.idx;
        begin match predecessors with
        | [(_, s)] -> s
        | _ -> Log.error (Printf.sprintf "Invalid block with multiple input states: %d" block.idx); assert false
        end
      | _ -> 
        Print_trace.data_block block.idx;
        begin match predecessors with
        | [(_, s)] -> s
        | _ -> Log.error (Printf.sprintf "Invalid block with multiple input states: %d" block.idx); assert false
        end
      end
    
  (** [summary cfg out_state] builds the summary of [cfg] from its final state.
      If the exit annotation is bottom, the function summary is bottom. *)
  let summary (cfg : annot_expected Cfg.t) (out_state : State.t) : summary =
    let function_summary =
      match (Cfg.find_block_exn cfg cfg.exit_block).annotation_after with
      | Bottom ->
        (* The function exit is likely unreachable, so we use a bottom summary *)
        Summary.bottom cfg Var.Set.empty
      | NotBottom _ ->
        Summary.make out_state
    in
    Print_trace.summary out_state State.to_string function_summary Summary.to_string;
    function_summary

  (** [extract_summary module_ cfg analyzed_cfg] extracts the final abstract
      state of [cfg] from [analyzed_cfg] and turns it into a summary. *)
  let extract_summary 
      (_module_ : Wasm_module.t)
      (cfg : annot_expected Cfg.t)
      (analyzed_cfg : State.t Cfg.t)
    : summary =
    (Cfg.find_block_exn analyzed_cfg cfg.exit_block).annotation_after
    |> summary cfg

  (** [call_inter module_ cfg instr state] handles calls in the classical
      interprocedural analysis. It only logs the call and leaves [state]
      unchanged. *)
  let call_inter
      (_module_ : Wasm_module.t)
      (_cfg : annot_expected Cfg.t)
      (instr : annot_expected Instr.labelled_call)
      (state : State.t)
    : State.t =
    Print_trace.instruction instr.line_number (Call instr) state.unreachable;
    state
  
  (** [entry module_ cfg state] propagates the state at function entry. *)
  let entry (_module_ : Wasm_module.t) (_cfg : annot_expected Cfg.t) (state : State.t) : State.t =
    state (* Everything is actually already done by spec analysis! We can simply propagate the state *)

  (** [return module_ cfg instr state_before_call state_after_call] propagates the
      state after returning from a call. *)
  let return (_module : Wasm_module.t) (_cfg : annot_expected Cfg.t) (_instr : annot_expected Instr.labelled_call) (_state_before_call : State.t) (state_after_call : State.t) : State.t =
    state_after_call

  (** [imported module_ desc annot_before annot_after state] applies a conservative
      summary for an imported function. *)
  let imported
      (module_ : Wasm_module.t)
      (desc : Wasm_module.func_desc)
      (annot_before : annot_expected)
      (annot_after : annot_expected)
      (state : State.t)
    : State.t =
    Spec_domain.wrap annot_before ~default:State.bottom ~f:(fun _annotation_before ->
      Spec_domain.wrap annot_after ~default:State.bottom ~f:(fun _annotation_after ->
        let summary = Summary.of_import
                        desc.idx
                        desc.name
                        module_.nglobals
                        desc.arguments
                        desc.returns
        in
        let return_variables =
          match annot_after with
          | Bottom -> assert false
          | NotBottom spec_after ->
            desc.returns |> List.length |> List.take spec_after.vstack 
        in
        Summary.apply ~summary ~state ~args:[] ~return_variables
      )
    )
end