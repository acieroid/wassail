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

  (** [init cfg] initializes the abstract state using the function argument and global variables from [cfg]. *)
  (* let init (cfg : 'a Cfg.t) : state = *)
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
            | _ -> (variable, Value.top))) @
          (* locals *)
          (List.mapi funcinst.code.locals ~f:(fun i type_ -> 
            let variable = Variable.Var (Var.Local (i + nb_of_arguments)) in
            match type_ with
            | I32 -> (variable, Value.ValueSet RIC.zero)
            | _ -> (variable, top))) @
          [(Variable.entire_memory), Value.top]);
      store_operations = RICSet.empty;
      unreachable = false }

  let join_state : State.t -> State.t -> State.t = State.join

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

  (** [data_instr_transfer m cfg i state] performs the abstract transfer function for a data instruction [i] on [state]. *)
  let data
      (_module_ : Wasm_module.t)
      (_cfg : annot_expected Cfg.t)
      (i : annot_expected Instr.labelled_data)
      (state : State.t)
    : State.t =
    if state.unreachable then
      (Print_trace.print "%d:\t%s (unreachable)\n" i.line_number (Instr.data_to_string i.instr);
      state)
    else
      (Print_trace.print "%d:\t%s\n" i.line_number (Instr.data_to_string i.instr);
      let ret (i : annot_expected Instr.labelled_data) : Variable.t = 
        match (Spec_domain.get_or_fail i.annotation_after).vstack with
        | ret :: _ -> Variable.Var ret
        | [] -> assert false
      in
      let invalid_pointer_value (type_ : string) (variable : Variable.t) : State.t =
        Print_trace.print "\tinvalid pointer type: %s\n" type_;
        State.to_top_RIC state variable
      in
      let set (variable : Variable.t) : State.t =
        let top_of_stack = pop (Spec_domain.get_or_fail i.annotation_before).vstack in
        begin match top_of_stack with
        | Var.Const (Prim_value.I32 n) ->
          Print_trace.print "\tassigning constant value %ld to variable %s\n"
            n (Variable.to_string variable);
          State.assign_constant_value state ~const:n ~to_:variable
        | Var.Const (Prim_value.F32 _) -> invalid_pointer_value "Float32" variable
        | Var.Const (Prim_value.I64 _) -> invalid_pointer_value "Int64" variable
        | Var.Const (Prim_value.F64 _) -> invalid_pointer_value "Float64" variable
        | Var.Local _ | Var.Global _ ->
          let vs = Value.ValueSet (RIC.relative_ric (Var.to_string top_of_stack)) in
          Print_trace.print "\tassigning value-set %s to variable %s\n"
            (Value.to_string vs)
            (Variable.to_string variable);
          State.set state ~var:variable ~vs
        | _ ->
          Print_trace.print "\ttransferring value-set of %s to variable %s\n"
            (Var.to_string top_of_stack)
            (Variable.to_string variable);
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
            ~vs:(Value_set_abstraction.plus 
              (State.get state ~var:Variable.MemorySize)
              (State.get state ~var:(Variable.Var (pop (Spec_domain.get_or_fail i.annotation_before).vstack))))
      | Nop | Drop -> state
      | MemoryCopy -> 
        (* TODO: if needed, we could eventually increase precision. See 
           Abstract_store_domain.memory_copy for implementation stub *)
        Print_trace.print "\t\tnot yet implemented: this operation is treated conservatively.\n";
        { (state |> State.set ~var:Variable.entire_memory ~vs:Value.top
                 |> State.set ~var:Variable.Accessed ~vs:Value.top)
            with store_operations = RICSet.singleton RIC.Top }
      | MemoryInit _ | MemoryFill ->
        (* TODO: if needed, we could eventually increase precision. *)
        { (state |> State.set ~var:Variable.entire_memory ~vs:Value.top)
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
              (Log.error "Select: condition can't be true nor false";
              assert false )(* bottom condition is unreachable *)
        in
        Print_trace.print "\t\tcondition: %s\n\t\tvalue if false: %s\n\t\tvalue if true: %s\n\t\tresult: %s\n"
          (Value.to_string cond_vs)
          (Value.to_string x_vs)
          (Value.to_string y_vs)
          (Value.to_string result);
        State.set state ~var:(ret i) ~vs:result
      | LocalGet l -> 
        let variable = Variable.Var (Var.Local (Int32.to_int_exn l)) in
        Print_trace.print "\tretrieving variable %s: %s\n"
          (variable |> Variable.to_string)
          (state |> State.get ~var:variable |> Value.to_string);
        state
      | LocalSet l -> Variable.Var (Var.Local (Int32.to_int_exn l)) |> set
      | LocalTee l -> Variable.Var (Var.Local (Int32.to_int_exn l)) |> set
      | GlobalGet g -> 
        let variable = Variable.Var (Var.Global (Int32.to_int_exn g)) in
        Print_trace.print "\tretrieving variable %s: %s\n"
          (variable |> Variable.to_string)
          (state |> State.get ~var:variable |> Value.to_string);
        state
      | GlobalSet g -> Variable.Var (Var.Global (Int32.to_int_exn g)) |> set
      | Const c ->
        begin match c with
        | Prim_value.I32 _ -> state
        | _ -> Print_trace.print "\tnon-i32 constant: it is assumed that this value won't be used as a pointer\n"; state
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
        | { typ = I32; _ } 
        | _ -> (* other operations result in a pointer that can point anywhere *)
          Print_trace.print "\tthis type of binary operation results in a pointer that can point anywhere";
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
          Print_trace.print "\t%s == %s -> %s\n" (RIC.to_string vs1) (RIC.to_string vs2) (Value.to_string result);
          state |> State.set ~var:(ret i) ~vs:result
        | {op = Eq; typ = I32} ->
          let result = Value.are_equal_or_not (var1, vs1) (var2, vs2) in
          Print_trace.print "\t%s == %s -> %s\n" (RIC.to_string vs1) (RIC.to_string vs2) (Value.to_string result);
          state |> State.set ~var:(ret i) ~vs:result
        | {op = LeS; typ = I32} -> 
          let result = Value.less_or_equal (var1, vs1) (var2, vs2) in
          Print_trace.print "\t%s ≤ %s -> %s\n" (RIC.to_string vs1) (RIC.to_string vs2) (Value.to_string result);
          state |> State.set ~var:(ret i) ~vs:result
          (* less_or_equal (var1, vs1) (var2, vs2) *)
        | {op = LtS; typ = I32} ->
          let result = Value.less_than (var1, vs1) (var2, vs2) in
          Print_trace.print "\t%s < %s -> %s\n" (RIC.to_string vs1) (RIC.to_string vs2) (Value.to_string result);
          state |> State.set ~var:(ret i) ~vs:result
        | {op = GeS; typ = I32} -> 
          let result = Value.less_or_equal (var2, vs2) (var1, vs1) in
          Print_trace.print "\t%s ≥ %s -> %s\n" (RIC.to_string vs1) (RIC.to_string vs2) (Value.to_string result);
          state |> State.set ~var:(ret i) ~vs:result
        | {op = GtS; typ = I32} ->
          let result = Value.less_than (var2, vs2) (var1, vs1) in
          Print_trace.print "\t%s > %s -> %s\n" (RIC.to_string vs1) (RIC.to_string vs2) (Value.to_string result);
          state |> State.set ~var:(ret i) ~vs:result
        | {op = LeU; typ = I32} -> 
          Print_trace.print "\t%s ≤ %s" (RIC.to_string vs1) (RIC.to_string vs2);
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
          Print_trace.print "\t%s ≤ %s -> %s\n" (RIC.to_string vs1) (RIC.to_string vs2) (Value.to_string result);
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
          Print_trace.print "\t%s < %s -> %s\n" (RIC.to_string vs1) (RIC.to_string vs2) (Value.to_string result);
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
          Print_trace.print "\t%s ≥ %s -> %s\n" (RIC.to_string vs1) (RIC.to_string vs2) (Value.to_string result);
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
          Print_trace.print "\t%s > %s -> %s\n" (RIC.to_string vs1) (RIC.to_string vs2) (Value.to_string result);
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




  (** [control_instr_transfer m summaries cfg i state] performs the abstract transfer function for a control instruction [i]. *)
  let control
      (_module_ : Wasm_module.t) (* The wasm module (read-only) *)
      (* (summaries : summary Int32Map.t) *)
      (_cfg : annot_expected Cfg.t) (* The CFG analized *)
      (i : annot_expected Instr.labelled_control) (* The instruction *)
      (state : State.t) (* the pre-state *)
    : [`Simple of State.t | `Branch of State.t * State.t] =
    Print_trace.print "%d:\t%s %s\n" i.line_number (Instr.control_to_short_string i.instr) (if state.unreachable then "(unreachable)" else "");
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
        |> List.mapi ~f:(fun i r -> Variable.Var (Var.Return (Int32.of_int_exn i)), state |> State.get ~var:(Variable.Var r))
        |> List.fold ~init:state ~f:(fun state (ret, value) -> 
          Print_trace.print "\t\t%s: %s\n" (Variable.to_string ret) (Value.to_string value);
          state |> State.set ~var:ret ~vs:value))
    | Unreachable -> `Simple  { State.bottom with unreachable = true }
    | _ -> `Simple state

  let apply_summary 
      (module_ : Wasm_module.t)
      (f : Int32.t) 
      (arity : int * int) 
      (i : annot_expected Instr.labelled_call)
      (state : State.t)
      (summary : summary)
    : State.t =
    if !Value_set_options.print_trace then 
      (Log.info (Printf.sprintf "applying summary of function %ld" f);
      print_endline ("\tState before the call: " ^ Abstract_store_domain.to_string state);
      print_endline ("\tSummary of function " ^ Int32.to_string f ^ ":\n" ^ Value_set_summary.to_string summary));
    let spec_before = Spec_domain.get_or_fail i.annotation_before in
    let args = List.take spec_before.vstack (fst arity) in
    let spec_after = Spec_domain.get_or_fail i.annotation_after in
    let return_variable = if snd arity = 1 then List.hd spec_after.vstack else None in
    let value_set_after_call = Value_set_summary.apply
      ~summary
      ~state
      ~args
      ~return_variable in
    (* let export = List.find module_.exported_funcs ~f:(fun (id, _, _) -> Int32.(id = f)) in *)
    let export = List.find module_.exported_funcs ~f:(fun descr -> Int32.(descr.idx = f)) in
    match export with
    (* | Some (_, fname, _) -> *)
    | Some descr ->
      if !Value_set_options.print_trace then 
        (Log.info (Printf.sprintf "function is named %s" descr.name); 
        Log.warn "Exports have not been implemented yet!");
      value_set_after_call
    | None -> value_set_after_call

  (** [get_predecessors cfg block] returns the list of predecessor blocks for [block] in [cfg]. *)
  let get_direct_predecessors 
      (cfg : annot_expected Cfg.t) 
      (block : annot_expected Basic_block.t) 
    : annot_expected Basic_block.t list =
    let predecessors = Cfg.predecessors cfg block.idx in
    let predecessors = List.map predecessors ~f:(fun (idx, _) -> idx) in
    List.filter ~f:(fun blk -> List.mem predecessors blk.idx ~equal:Int.equal) 
                         (Cfg.all_predecessors cfg block)

  (** [get_previous_stacks cfg predecessors] returns the annotated stacks for a list of predecessor blocks. *)
  let get_previous_stacks
      (_cfg : annot_expected Cfg.t)
      (predecessors : annot_expected Basic_block.t list)
    : (int * Var.t list) list =
    let previous_annotations = 
      List.fold ~init:[] 
                ~f:(fun acc blk ->
                  (* (blk.idx, Cfg.state_after_block cfg blk.idx (Spec_inference.init_state cfg)) :: acc) *)
                  (blk.idx, blk.annotation_after) :: acc)
                predecessors
    in
    List.map 
      ~f:(fun x ->
        match x with
        | idx, Spec_domain.Bottom -> idx, []
        | idx, NotBottom x -> idx, x.vstack)
      previous_annotations

  let merge_variables
      (module_ : Wasm_module.t)
      (cfg : annot_expected Cfg.t) 
      (block : annot_expected Basic_block.t)
      (state : State.t)
      (predecessors : ('a Basic_block.t * State.t) list)
      : State.t =
    let merged_variables = 
      Spec_inference.new_merge_variables_with_origin module_ cfg block
      |> List.filter_map ~f:(fun (pred_idx, old_var, merge_var) ->
        (* state at the end of predecessor block where [old_var] was created *)
        match List.find_exn predecessors ~f:(fun (pred_block, _) -> pred_block.idx = pred_idx) with
        | _, pred_state -> 
          if pred_state.unreachable then
            None
          else
            Some (Variable.Var merge_var, Abstract_store_domain.get state ~var:(Variable.Var old_var)))
      |> List.fold ~init:[] ~f:(fun acc (v, vs) -> 
        match List.Assoc.find acc ~equal:Variable.equal v with
        | None -> (v, vs) :: acc
        | Some existing_vs ->
          let acc = List.Assoc.remove acc ~equal:Variable.equal v in
          (v, Abstract_store_domain.Value.join vs existing_vs) :: acc)
    in
    let str_list = List.map ~f:(fun (a,b) -> Variable.to_string a ^ "->" ^ Abstract_store_domain.Value.to_string b) merged_variables in
    if !Value_set_options.print_trace then print_endline ("\tmerged variables: [" ^ String.concat ~sep:"; " str_list ^ "]");
    List.fold merged_variables ~init:state ~f:(fun acc (v, vs) -> Abstract_store_domain.set acc ~var:v ~vs)


  (** [merge_flows m cfg block states] merges incoming flows into [block], handling stack merging at control points. *)
  let merge_flows 
      (module_ : Wasm_module.t) 
      (cfg : annot_expected Cfg.t) 
      (block : annot_expected Basic_block.t)
      (* (states : (int * State.t) list)  *)
      (predecessors : ('a Basic_block.t * State.t) list)
    : State.t =
    (* let current_function = 
      match (List.filter module_.funcs ~f:(fun func -> Int32.(func.idx = block.fidx))) with
      | [f] -> f
      | _ -> assert false in *)
    match predecessors with
    | [] -> 
      if !Value_set_options.print_trace then print_endline ("================ START OF FUNCTION ==================== DATA BLOCK #" ^ string_of_int block.idx);
      (* init_state cfg *)
      (* init module_ current_function *) bottom
    | _ ->
      begin match block.content with
      | Control { instr = Merge; _ }
      | Entry | Return _ ->
        if !Value_set_options.print_trace then print_endline ("======================================================= " ^ (if IntSet.mem cfg.loop_heads block.idx then "LOOP HEAD" else "MERGE") ^ ": Control block #" ^ string_of_int block.idx);
        let states' = List.map ~f:snd predecessors in
        (* Join all previous states: *)
        let new_state_without_merges = List.reduce_exn states' ~f:join_state in 
        merge_variables module_ cfg block new_state_without_merges predecessors
      | Control _ ->
        if !Value_set_options.print_trace then print_endline ("======================================================= CONTROL BLOCK #" ^ string_of_int block.idx);
        begin match predecessors with
        | (_, s) :: [] -> s
        | _ -> failwith (Printf.sprintf "Invalid block with multiple input states: %d" block.idx)
        end
      | _ -> 
        if !Value_set_options.print_trace then print_endline ("======================================================= DATA BLOCK #" ^ string_of_int block.idx);
        begin match predecessors with
        | (_, s) :: [] -> s
        | _ -> failwith (Printf.sprintf "Invalid block with multiple input states: %d" block.idx)
        end
      end
    


  (** [summary cfg out_state] computes the value-set summary for a function using its [cfg] and final [out_state]. *)
  (* TODO: refactor this into value_set_summary *)
  let summary (cfg : annot_expected Cfg.t) (out_state : State.t) : summary =
    if !Value_set_options.print_trace then print_endline "======================================================= SUMMARY";
    if !Value_set_options.print_trace then print_endline ("END STATE:\t" ^ Abstract_store_domain.to_string out_state);
    (* let init_spec = (Spec_inference.init_state cfg) in *)
    let function_summary =
      (* match Cfg.state_after_block cfg cfg.exit_block init_spec with *)
      match (Cfg.find_block_exn cfg cfg.exit_block).annotation_after with
      | Bottom ->
        (* The function exit is likely unreachable, so we use a bottom summary *)
        Value_set_summary.bottom cfg Var.Set.empty
      | NotBottom _ ->
        Value_set_summary.make out_state
    in
    if !Value_set_options.print_trace then print_endline ("SUMMARY:\n" ^ Value_set_summary.to_string function_summary);
    function_summary

  (** [extract_summary cfg analyzed_cfg] extracts a value-set summary from the final result of the analysis. *)
  let extract_summary 
      (_module_ : Wasm_module.t)
      (cfg : annot_expected Cfg.t)
      (analyzed_cfg : State.t Cfg.t)
    : summary =
    (* let out_state = Cfg.state_after_block analyzed_cfg cfg.exit_block (init_state cfg) in *)
    let out_state = (Cfg.find_block_exn analyzed_cfg cfg.exit_block).annotation_after in
    summary cfg out_state

  (** This is only used in a classical inter analysis. The state doesn't change upon calling a function. *)
  let call_inter
      (_module_ : Wasm_module.t)
      (_cfg : annot_expected Cfg.t)
      (instr : annot_expected Instr.labelled_call)
      (state : State.t)
    : State.t =
    let instr_string = Instr.call_to_string instr.instr in
    if !Value_set_options.print_trace then print_endline (string_of_int instr.line_number ^ ":\t" ^ instr_string);
    state
  
  let entry (_module_ : Wasm_module.t) (_cfg : annot_expected Cfg.t) (state : State.t) : State.t =
    state (* Everything is actually already done by spec analysis! We can just propagate the state *)

  let return (_module : Wasm_module.t) (_cfg : annot_expected Cfg.t) (_instr : annot_expected Instr.labelled_call) (_state_before_call : State.t) (state_after_call : State.t) : State.t =
    state_after_call

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
                        desc.name
                        module_.nglobals
                        desc.arguments
                        desc.returns
        in
        let return_variable =
          match annot_after with
          | Bottom -> assert false
          | NotBottom x ->
            if List.length desc.returns = 1 then 
              List.hd x.vstack 
            else 
              None 
        in
        Summary.apply
          ~summary
          ~state
          ~args:[]
          ~return_variable
        (* match StringMap.find !value_set_specifications desc.name with
        | None ->
          Log.warn (Printf.sprintf "No specification found for imported function %s (index: %ld): assuming abstract store is preserved" desc.name desc.idx);
          state
        | Some _spec ->
          (* TODO: implement this! *)
          Log.warn "Imported functions have not been implemented yet! It is assumed that the abstract store remains unchanged by the function call.";
          state *)
      )
    )

end