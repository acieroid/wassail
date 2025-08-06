open Core
open Helpers
open Reduced_interval_congruence

module Make (*: Transfer.TRANSFER *) = struct

  module RIC = Reduced_interval_congruence.RIC

  type annot_expected = Spec.t

  (** The state *)
  type state = Abstract_store_domain.t
  [@@deriving sexp, compare, equal]

  (** [value_set_specification] holds the (currently unused) specification for the value-set analysis. *)
  (* let value_set_specification = () *)

  (** [init_state cfg] initializes the abstract state using the function argument and global variables from [cfg]. *)
  let init_state (cfg : 'a Cfg.t) : state =
    let nb_of_arguments = List.length cfg.arg_types in
    { abstract_store =
        Variable.Map.of_alist_exn (
          (List.mapi cfg.arg_types ~f:(fun i _ -> 
            let variable = Var.Local i in
            let var_name = Var.to_string variable in
            (Variable.Var variable, Abstract_store_domain.Value.ValueSet (RIC.ric (0, Int 0, Int 0, (var_name, 0)))))) @
          (List.mapi cfg.global_types ~f:(fun i _ -> 
            let variable = Var.Global i in
            let var_name = Var.to_string variable in
            (Variable.Var variable, Abstract_store_domain.Value.ValueSet (RIC.ric (0, Int 0, Int 0, (var_name, 0)))))) @
          (List.mapi cfg.local_types ~f:(fun i _ -> 
            let variable = Var.Local (i + nb_of_arguments) in
            (Variable.Var variable, Abstract_store_domain.Value.ValueSet (RIC.ric (0, Int 0, Int 0, ("", 0)))))) @
          [(Variable.Mem RIC.Top), Abstract_store_domain.Value.ValueSet RIC.Top]);
      store_operations = RICSet.empty }

  (** [bottom_state cfg] returns the bottom abstract state. *)
  let bottom_state (_cfg : 'a Cfg.t) : state = Abstract_store_domain.bottom

  (** [state_to_string s] converts an abstract state [s] to its string representation. *)
  let state_to_string (s : state) : string = Abstract_store_domain.to_string s

  (** [join_state s1 s2] joins two abstract states [s1] and [s2]. *)
  let join_state (s1 : state) (s2 : state) : state = Abstract_store_domain.join s1 s2

  (** [widen_state s1 s2] performs widening between abstract states [s1] and [s2]. *)
  let widen_state (s1 : state) (s2 : state) : state = 
    let widened_state = Abstract_store_domain.widen s1 s2 in
    if !Value_set_options.print_trace then 
      (print_endline "\twidening:";
      print_endline ("\t\tstate1: " ^ Abstract_store_domain.to_string s1);
      print_endline ("\t\tstate2: " ^ Abstract_store_domain.to_string s2);
      print_endline ("\t\twidened state: " ^ Abstract_store_domain.to_string widened_state););
    if not (Abstract_store_domain.equal widened_state s1) then Intra.narrow_option := true;
    widened_state

  type summary = Value_set_summary.t

  let is_this_the_value_of 
      (state : Spec.SpecWithoutBottom.t) 
      ~(value : Variable.t) 
      ~(of_this_variable : Variable.t)
    : bool =
    match of_this_variable with
    | Var Var.Local l ->
      let value' = Variable.Var (Spec_inference.get (Option.value_exn (Int32.of_int l)) state.locals) in
      Variable.equal value value'
    | Var Var.Global g ->
      let value' = Variable.Var (Spec_inference.get (Option.value_exn (Int32.of_int g)) state.globals) in
      Variable.equal value value'
    | _ -> false

  (** [data_instr_transfer m cfg i state] performs the abstract transfer function for a data instruction [i] on [state]. *)
  let data_instr_transfer
      (_module_ : Wasm_module.t)
      (_cfg : annot_expected Cfg.t)
      (i : annot_expected Instr.labelled_data)
      (state : state)
    : state =
    if !Value_set_options.print_trace then print_endline (string_of_int i.line_number ^ ":\t" ^ Instr.data_to_string i.instr);
    let ret (i : annot_expected Instr.labelled_data) : Variable.t = 
      match List.hd (Spec.get_or_fail i.annotation_after).vstack with
      | Some r -> Variable.Var r
      | None -> failwith "nothing on the stack" in
    match i.instr with
    | Nop | MemorySize | Drop | MemoryGrow -> state
    | MemoryCopy | MemoryFill | MemoryInit _ -> state
    | RefIsNull | RefNull _ | RefFunc _ -> state
    | Select _ ->
      let x, y = (*pop2 (Spec.get_or_fail i.annotation_before).vstack in*)
        match (Spec.get_or_fail i.annotation_before).vstack with
        | _ :: x :: y :: _ -> x, y
        | _ -> failwith "not enough elements on the stack" in
      let x_vs, y_vs = Abstract_store_domain.get state ~var:(Variable.Var x), Abstract_store_domain.get state ~var:(Variable.Var y) in
      let joined_selection = Abstract_store_domain.Value.join x_vs y_vs in
      if !Value_set_options.print_trace then 
        Printf.printf "\tselecting from two options:\n\t\t%s   ⊔   %s   ->  %s\n"
          (Abstract_store_domain.Value.to_string x_vs)
          (Abstract_store_domain.Value.to_string y_vs) 
          (Abstract_store_domain.Value.to_string joined_selection);
        Abstract_store_domain.set state ~var:(ret i) ~vs:joined_selection
    | LocalGet l -> 
      let local_variable = Variable.Var (Var.Local (Int32.to_int_exn l)) in
      if !Value_set_options.print_trace then print_endline ("\t" ^ Variable.to_string local_variable ^ " (" ^ Variable.to_string (ret i) ^ ")");
      state
    | LocalSet l ->
      let variable = Variable.Var (Var.Local (Int32.to_int_exn l)) in
      let top_of_stack = pop (Spec.get_or_fail i.annotation_before).vstack in
        begin match top_of_stack with
        | Var.Const (Prim_value.I32 n) ->
          if !Value_set_options.print_trace then print_endline ("\tassigning constant value " ^ Int32.to_string n ^ " to local variable " ^ Variable.to_string variable);
          Abstract_store_domain.assign_constant_value state
          ~const:n
          ~to_:variable
        | Var.Const (Prim_value.F32 n) ->
          if !Value_set_options.print_trace then print_endline ("\t" ^ Variable.to_string variable ^ " = (Float32)" ^ Wasm.F32.to_string n ^ " -> not a valid pointer value: variable can point anywhere");
          Abstract_store_domain.to_top_RIC state variable
        | Var.Const (Prim_value.I64 n) ->
          if !Value_set_options.print_trace then print_endline ("\t" ^ Variable.to_string variable ^ " = (Int64)" ^ Int64.to_string n ^ " -> not a valid pointer value: variable can point anywhere");
          Abstract_store_domain.to_top_RIC state variable
        | Var.Const (Prim_value.F64 n) ->
          if !Value_set_options.print_trace then print_endline ("\t" ^ Variable.to_string variable ^ " = (Float64)" ^ Wasm.F64.to_string n ^ " -> not a valid pointer value: variable can point anywhere");
          Abstract_store_domain.to_top_RIC state variable
        | Var.Local _ | Var.Global _ ->
          let vs = Abstract_store_domain.Value.ValueSet (RIC.relative_ric (Var.to_string top_of_stack)) in
          if !Value_set_options.print_trace then print_endline ("\tassigning value-set " ^ Abstract_store_domain.Value.to_string vs ^ " to variable " ^ Variable.to_string variable);
          Abstract_store_domain.set state ~var:variable ~vs
        | _ ->
          if !Value_set_options.print_trace then print_endline ("\ttransfering value-set of " ^ Var.to_string top_of_stack ^ " to local variable " ^ Variable.to_string variable);
          Abstract_store_domain.copy_value_set state 
            ~from:(Variable.Var top_of_stack)
            ~to_:variable
        end
    | LocalTee l -> (* TODO: Refactor a common function for LocalSet and LocalTee *)
      let variable = Variable.Var (Var.Local (Int32.to_int_exn l)) in
      let top_of_stack = pop (Spec.get_or_fail i.annotation_before).vstack in
      begin match top_of_stack with
      | Var.Const (Prim_value.I32 n) ->
        if !Value_set_options.print_trace then print_endline ("\tadding constant value " ^ Int32.to_string n ^ " to local variable " ^ Variable.to_string variable);
        Abstract_store_domain.assign_constant_value state
        ~const:n
        ~to_:variable
      | Var.Const (Prim_value.F32 n) ->
        if !Value_set_options.print_trace then print_endline ("\t" ^ Variable.to_string variable ^ " = (Float32)" ^ Wasm.F32.to_string n ^ " -> not a valid pointer value: variable can point anywhere");
        Abstract_store_domain.to_top_RIC state variable
      | Var.Const (Prim_value.I64 n) ->
        if !Value_set_options.print_trace then print_endline ("\t" ^ Variable.to_string variable ^ " = (Int64)" ^ Int64.to_string n ^ " -> not a valid pointer value: variable can point anywhere");
        Abstract_store_domain.to_top_RIC state variable
      | Var.Const (Prim_value.F64 n) ->
        if !Value_set_options.print_trace then print_endline ("\t" ^ Variable.to_string variable ^ " = (Float64)" ^ Wasm.F64.to_string n ^ " -> not a valid pointer value: variable can point anywhere");
        Abstract_store_domain.to_top_RIC state variable
      | Var.Local _ | Var.Global _ ->
          let vs = Abstract_store_domain.Value.ValueSet (RIC.relative_ric (Var.to_string top_of_stack)) in
          if !Value_set_options.print_trace then print_endline ("\tassigning value-set " ^ Abstract_store_domain.Value.to_string vs ^ " to variable " ^ Variable.to_string variable);
          Abstract_store_domain.set state ~var:variable ~vs
      | _ ->
        if !Value_set_options.print_trace then print_endline ("\ttransfering value-set of " ^ Var.to_string top_of_stack ^ " to local variable " ^ Variable.to_string variable);
        Abstract_store_domain.copy_value_set state 
          ~from:(Variable.Var top_of_stack)
          ~to_:variable
      end
    | GlobalGet g -> 
      let global_variable = Variable.Var (Var.Global (Int32.to_int_exn g)) in
      if !Value_set_options.print_trace then print_endline ("\t" ^ Variable.to_string global_variable ^ " (" ^ Variable.to_string (ret i) ^ ")");
      if Variable.Map.mem state.abstract_store (ret i) then
        Abstract_store_domain.copy_value_set state
          ~from:(ret i)
          ~to_:global_variable
      else
        state
    | GlobalSet g ->
      let variable = Variable.Var (Var.Global (Int32.to_int_exn g)) in
      let top_of_stack = pop (Spec.get_or_fail i.annotation_before).vstack in
      (* let new_state = *)
        begin match top_of_stack with
        | Var.Const (Prim_value.I32 n) ->
          if !Value_set_options.print_trace then print_endline ("\tassigning constant value " ^ Int32.to_string n ^ " to global variable " ^ Variable.to_string variable);
          Abstract_store_domain.assign_constant_value state
          ~const:n
          ~to_:variable
        | Var.Const (Prim_value.F32 n) ->
          if !Value_set_options.print_trace then print_endline ("\t" ^ Variable.to_string variable ^ " = (Float32)" ^ Wasm.F32.to_string n ^ " -> not a valid pointer value: variable can point anywhere");
          Abstract_store_domain.to_top_RIC state variable
        | Var.Const (Prim_value.I64 n) ->
          if !Value_set_options.print_trace then print_endline ("\t" ^ Variable.to_string variable ^ " = (Int64)" ^ Int64.to_string n ^ " -> not a valid pointer value: variable can point anywhere");
          Abstract_store_domain.to_top_RIC state variable
        | Var.Const (Prim_value.F64 n) ->
          if !Value_set_options.print_trace then print_endline ("\t" ^ Variable.to_string variable ^ " = (Float64)" ^ Wasm.F64.to_string n ^ " -> not a valid pointer value: variable can point anywhere");
          Abstract_store_domain.to_top_RIC state variable
        | Var.Local _ | Var.Global _ ->
          let vs = Abstract_store_domain.Value.ValueSet (RIC.relative_ric (Var.to_string top_of_stack)) in
          if !Value_set_options.print_trace then print_endline ("\tassigning value-set " ^ Abstract_store_domain.Value.to_string vs ^ " to variable " ^ Variable.to_string variable);
          Abstract_store_domain.set state ~var:variable ~vs
        | _ ->
          if !Value_set_options.print_trace then print_endline ("\ttransfering value-set of " ^ Var.to_string top_of_stack ^ " to global variable " ^ Variable.to_string variable);
          Abstract_store_domain.copy_value_set state 
            ~from:(Variable.Var top_of_stack)
            ~to_:variable
        end
    | Const c ->
      begin match c with
      | Prim_value.I32 _ -> state
      | _ -> if !Value_set_options.print_trace then print_endline "\tnon i32 constant!"; state
      end
    | Binary binop -> 
      let x, y = pop2 (Spec.get_or_fail i.annotation_before).vstack in
      (* let result = pop (Spec.get_or_fail i.annotation_after).vstack in *)
      let result = ret i in
      begin match binop with
      | { op = Add; typ = I32 } -> (* i32 addition *) 
        if !Value_set_options.print_trace then print_endline ("\t" ^ Var.to_string x ^ " + " ^ Var.to_string y ^ " -> " ^ Variable.to_string result);
        Abstract_store_domain.i32_add state ~x:(Variable.Var x) ~y:(Variable.Var y) ~result
      | { op = Sub; typ = I32 } -> (* i32 subtraction *) 
        if !Value_set_options.print_trace then print_endline ("\t" ^ Var.to_string y ^ " - " ^ Var.to_string x ^ " -> " ^ Variable.to_string result);
        Abstract_store_domain.i32_sub state ~x:(Variable.Var x) ~y:(Variable.Var y) ~result
      | { typ = I32; _ } 
      | _ -> (* other operations result in a pointer that can point anywhere *)
        if !Value_set_options.print_trace then print_endline "\tthis type of binary operation results in a pointer that can point anywhere";
        Abstract_store_domain.to_top_RIC state result
      end
    | Load load ->
      let size = Option.value_exn (Int32.to_int (Memoryop.size load)) in
      let address = pop (Spec.get_or_fail i.annotation_before).vstack in
      let vs = Abstract_store_domain.get state ~var:(Variable.Var address) in
      (* Update accessed address *)
        let previously_accessed = Abstract_store_domain.get state ~var:Variable.Accessed in
        let vs_plus_offset =
          begin match load, vs with
          | { offset = offset; _ }, Abstract_store_domain.Value.ValueSet vs ->
            Abstract_store_domain.Value.ValueSet (RIC.add_offset vs offset)
          | _ -> failwith "Trying to use boolean as an address"
          end
        in
        let state = Abstract_store_domain.set state ~var:Variable.Accessed ~vs:(Abstract_store_domain.Value.join vs_plus_offset previously_accessed) 
        in
      (* begin match vs with
      | Abstract_store_domain.Value.ValueSet RIC.Top ->
        failwith "Trying to load from undefined (Top) address"
      | _ -> *)
        begin match load with
        | { typ = F32; _ }
        | { typ = F64; _ } -> Abstract_store_domain.to_top_RIC state (ret i)
        | { typ = I64; _ } -> Abstract_store_domain.to_top_RIC state (ret i)
        | { typ = I32; offset = offset; _ } ->
          if size = 4 then
            let is_stack = !Value_set_options.disjoint_stack
              && String.equal "g0" (Abstract_store_domain.Value.extract_relative_offset vs) in
            match vs with
            | Abstract_store_domain.Value.ValueSet vs ->
              let vs_plus_offset = RIC.add_offset vs offset in
              if !Value_set_options.print_trace then print_endline ("\tloading content at address " ^ RIC.to_string vs_plus_offset ^ " into variable " ^ Variable.to_string (ret i));
              let target_variable = 
                if is_stack then
                  Variable.Stack vs_plus_offset
                else
                  Variable.Mem vs_plus_offset in
              if !Value_set_options.print_trace then print_endline ("\ttarget variable: " ^ Variable.to_string target_variable);
              let all_mem_vars = Abstract_store_domain.extract_memory_variables state in
              let all_stack_vars = Abstract_store_domain.extract_stack_variables state in
              if !Value_set_options.print_trace then 
                (print_endline ("\tall memory variables in the current state: " ^ String.concat ~sep:", " (List.map ~f:Variable.to_string all_mem_vars));
                if !Value_set_options.disjoint_stack then 
                  print_endline ("\tall stack variables in the current state: " ^ String.concat ~sep:", " (List.map ~f:Variable.to_string all_stack_vars));
                );
              Abstract_store_domain.set state ~var:(ret i) ~vs:(Abstract_store_domain.get state ~var:target_variable)
            | Abstract_store_domain.Value.Boolean _ -> 
              (Log.warn "Using a boolean value as an address: loaded value is undefined";
              Abstract_store_domain.set state ~var:(ret i) ~vs:(Abstract_store_domain.get state ~var:Variable.entire_memory))
          else
            Abstract_store_domain.to_top_RIC state (ret i)
        end
      (* end *)
    | Store store ->
      Abstract_store_domain.store 
        ~state ~instruction:store ~annotation_before:(Some i.annotation_before) ~value:None ~address:None
    | Compare comp ->
      let var2, var1 = pop2 (Spec.get_or_fail i.annotation_before).vstack in
      let vs1 = 
        begin match var1 with
        | Var.Local _
        | Var.Global _ -> Abstract_store_domain.Value.ValueSet (RIC.relative_ric (Var.to_string var1))
        | _ -> Abstract_store_domain.get state ~var:(Variable.Var var1) 
        end in
      let vs2 = 
        begin match var1 with
        | Var.Local _
        | Var.Global _ -> Abstract_store_domain.Value.ValueSet (RIC.relative_ric (Var.to_string var2))
        | _ -> Abstract_store_domain.get state ~var:(Variable.Var var2) 
        end in
      begin match vs1, vs2 with
      | Boolean _, _ | _, Boolean _ -> Abstract_store_domain.set state ~var:(ret i) ~vs:(Boolean Variable.Map.empty)
      | ValueSet vs1, ValueSet vs2 ->
        begin match comp with
        | {op = LeU; typ = I32} -> 
          if !Value_set_options.print_trace then print_endline ("\t" ^ RIC.to_string vs1 ^ " ≤ " ^ RIC.to_string vs2);
          let vs2' = RIC.remove_lower_bound vs2 in
          let vs2'' = RIC.add_offset (RIC.remove_upper_bound vs2) 1 in
          let vs1_true = if RIC.comparable_offsets vs1 vs2' then RIC.meet vs1 vs2' else vs1 in
          let vs1_false = if RIC.comparable_offsets vs1 vs2' then RIC.meet vs1 vs2'' else vs1 in
          if !Value_set_options.print_trace then print_endline ("\ttrue: " ^ RIC.to_string vs1_true ^ "\n\tfalse: " ^ RIC.to_string vs1_false);
          begin match var1 with
          | Var.Const _ -> state
          | _ ->
            Abstract_store_domain.set 
              state 
              ~var:(ret i) 
              ~vs:(Boolean (Variable.Map.set Variable.Map.empty ~key:(Variable.Var var1) ~data:Boolean.{true_ = vs1_true; false_ = vs1_false}))
          end
        | {op = LtU; typ = I32} ->
          if !Value_set_options.print_trace then print_endline ("\t" ^ RIC.to_string vs1 ^ " < " ^ RIC.to_string vs2);
          let vs2' = RIC.add_offset (RIC.remove_lower_bound vs2) (-1) in
          let vs2'' = RIC.remove_upper_bound vs2 in
          let vs1_true = if RIC.comparable_offsets vs1 vs2' then RIC.meet vs1 vs2' else vs1 in
          let vs1_false = if RIC.comparable_offsets vs1 vs2' then RIC.meet vs1 vs2'' else vs1 in
          if !Value_set_options.print_trace then print_endline ("\ttrue: " ^ RIC.to_string vs1_true ^ "\n\tfalse: " ^ RIC.to_string vs1_false);
          begin match var1 with
          | Var.Const _ -> state
          | _ ->
            Abstract_store_domain.set 
              state 
              ~var:(ret i) 
              ~vs:(Boolean (Variable.Map.set Variable.Map.empty ~key:(Variable.Var var1) ~data:Boolean.{true_ = vs1_true; false_ = vs1_false}))
          end
        | {op = GeU; typ = I32} -> 
          if !Value_set_options.print_trace then print_endline ("\t" ^ RIC.to_string vs1 ^ " ≥ " ^ RIC.to_string vs2);
          let vs2' = RIC.remove_upper_bound vs2 in
          let vs2'' = RIC.add_offset (RIC.remove_lower_bound vs2) (-1) in
          let vs1_true = if RIC.comparable_offsets vs1 vs2' then RIC.meet vs1 vs2' else vs1 in
          let vs1_false = if RIC.comparable_offsets vs1 vs2' then RIC.meet vs1 vs2'' else vs1 in
          if !Value_set_options.print_trace then print_endline ("\ttrue: " ^ RIC.to_string vs1_true ^ "\n\tfalse: " ^ RIC.to_string vs1_false);
          begin match var1 with
          | Var.Const _ -> state
          | _ ->
            Abstract_store_domain.set 
              state 
              ~var:(ret i) 
              ~vs:(Boolean (Variable.Map.set Variable.Map.empty ~key:(Variable.Var var1) ~data:Boolean.{true_ = vs1_true; false_ = vs1_false}))
          end
        | {op = GtU; typ = I32} ->
          if !Value_set_options.print_trace then print_endline ("\t" ^ RIC.to_string vs1 ^ " > " ^ RIC.to_string vs2);
          let vs2' = RIC.add_offset (RIC.remove_upper_bound vs2) 1 in
          let vs2'' = RIC.remove_lower_bound vs2 in
          let vs1_true = if RIC.comparable_offsets vs1 vs2' then RIC.meet vs1 vs2' else vs1 in
          let vs1_false = if RIC.comparable_offsets vs1 vs2' then RIC.meet vs1 vs2'' else vs1 in
          if !Value_set_options.print_trace then print_endline ("\ttrue: " ^ RIC.to_string vs1_true ^ "\n\tfalse: " ^ RIC.to_string vs1_false);
          begin match var1 with
          | Var.Const _ -> state
          | _ ->
            Abstract_store_domain.set 
              state 
              ~var:(ret i) 
              ~vs:(Boolean (Variable.Map.set Variable.Map.empty ~key:(Variable.Var var1) ~data:Boolean.{true_ = vs1_true; false_ = vs1_false}))
          end
        | _ -> Abstract_store_domain.set state ~var:(ret i) ~vs:(Boolean Variable.Map.empty)
        end
      end
    | Test test -> 
      let var = pop (Spec.get_or_fail i.annotation_before).vstack in
      let vs = Abstract_store_domain.get state ~var:(Variable.Var var) in
      begin match test with
      | I32Eqz -> (* TODO: implement this *)
        begin match vs with
          | Boolean b -> 
            let state = 
              { Abstract_store_domain.abstract_store = 
                  Variable.Map.remove state.abstract_store (Variable.Var var);
                store_operations = state.store_operations } in (* TODO: check that this is sound *)
            Abstract_store_domain.set state ~var:(ret i) ~vs:(Abstract_store_domain.Value.Boolean (Boolean.not_ b))
          | ValueSet _ -> Abstract_store_domain.set state ~var:(ret i) ~vs:(Boolean Variable.Map.empty)
        end
      | _ -> Abstract_store_domain.set state ~var:(ret i) ~vs:(Boolean Variable.Map.empty)
      end
    | Unary _ | Convert _ -> (* TODO: write this case *) 
      Log.warn "unary and convert operations have not been implemented";
      state


  let apply_condition 
      (state : state) 
      ~(condition : Variable.t * Boolean.t)
      (spec_state : Spec.SpecWithoutBottom.t) 
      : state * state =
    let state = 
      { Abstract_store_domain.abstract_store = Variable.Map.remove state.abstract_store (fst condition);
        store_operations = state.store_operations } in
    let locals_and_globals = Abstract_store_domain.extract_locals_and_globals state in
    let true_ = 
      Abstract_store_domain.make_compatible 
        ~this_store:
          { Abstract_store_domain.abstract_store = (Variable.Map.map ~f:(fun x -> (Abstract_store_domain.Value.ValueSet x.true_)) (snd condition));
            store_operations = state.store_operations }
        ~relative_to:state in
    let true_ =
      { Abstract_store_domain.abstract_store =
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
                    acc));
        store_operations = state.store_operations }
    in
    let false_ = 
      Abstract_store_domain.make_compatible 
        ~this_store:
          { Abstract_store_domain.abstract_store = (Variable.Map.map ~f:(fun x -> (Abstract_store_domain.Value.ValueSet x.false_)) (snd condition));
            store_operations = state.store_operations }
        ~relative_to:state in
    let false_ =
      { Abstract_store_domain.abstract_store =
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
                    acc));
        store_operations = state.store_operations }
    in
    true_, false_




  (** [control_instr_transfer m summaries cfg i state] performs the abstract transfer function for a control instruction [i]. *)
  let control_instr_transfer
      (module_ : Wasm_module.t) (* The wasm module (read-only) *)
      (summaries : summary Int32Map.t)
      (_cfg : annot_expected Cfg.t) (* The CFG analized *)
      (i : annot_expected Instr.labelled_control) (* The instruction *)
      (state : state) (* the pre-state *)
    : [`Simple of state | `Branch of state * state] =
    if !Value_set_options.print_trace then print_endline (string_of_int i.line_number ^ ":\t" ^ Instr.control_to_string i.instr);
    let apply_summary (f : Int32.t) (arity : int * int) (state : state) : state =
      if !Value_set_options.print_trace then Log.info (Printf.sprintf "applying summary of function %ld" f);
      if !Value_set_options.print_trace then print_endline ("\tState before the call: " ^ Abstract_store_domain.to_string state);
      match Int32Map.find summaries f with
      | None ->
        if Int32.(f < module_.nfuncimports) then begin
          Log.warn (Printf.sprintf "No summary found for function %ld (imported function): assuming value-sets are preserved" f);
          state
        end else
          (if !Value_set_options.print_trace then Log.warn "This function depends on another function that has not been analyzed yet, so it is part of some recursive loop. It will eventually stabilize";
          state)
      | Some summary ->
        if !Value_set_options.print_trace then print_endline ("\tSummary of function " ^ Int32.to_string f ^ ": " ^ Value_set_summary.to_string summary);
        let args = List.take (Spec.get_or_fail i.annotation_before).vstack (fst arity) in
        let return_variable = if snd arity = 1 then List.hd (Spec.get_or_fail i.annotation_after).vstack else None in
        let value_set_after_call = Value_set_summary.apply
          ~summary
          ~state
          ~args
          ~return_variable in
        let export = List.find module_.exported_funcs ~f:(fun (id, _, _) -> Int32.(id = f)) in
        match export with
        | Some (_, fname, _) ->
          if !Value_set_options.print_trace then Log.info (Printf.sprintf "function is named %s" fname);
          if !Value_set_options.print_trace then Log.warn "Exports have not been implemented yet!";
          value_set_after_call
        | None -> value_set_after_call
    in
    match i.instr with
    | Call (arity, _, f) -> 
      if !Value_set_options.print_trace then print_endline ("\t(nb of arguments: " ^ string_of_int (fst arity) ^ ", nb of return values: " ^ string_of_int (snd arity) ^ ")");
      let new_store = apply_summary f arity state in
      let new_store = Abstract_store_domain.remove_pointers_to_top new_store in
      `Simple new_store
    | CallIndirect (_, arity, _, typ) ->
      let targets = Call_graph.indirect_call_targets module_ typ in
      (* Apply the summaries *)
      `Simple (List.fold_left targets
        ~init:Abstract_store_domain.bottom
        ~f:(fun acc idx -> Abstract_store_domain.join (apply_summary idx arity state) acc))
    | Br _ -> `Simple state
    | BrIf _ | If _ -> 
      let condition = Variable.Var (pop (Spec.get_or_fail i.annotation_before).vstack) in
      let boolean_value = Abstract_store_domain.get state ~var:condition in
      begin match boolean_value with
      | ValueSet _ -> `Branch (state, state)
      | Boolean boolean_value -> 
        let state_if_true, state_if_false = apply_condition state ~condition:(condition, boolean_value) (Spec.get_or_fail i.annotation_after)  in
        `Branch (state_if_true, state_if_false)
      end
    | Return -> 
      begin match (Spec.get_or_fail i.annotation_before).vstack with
      | [] -> `Simple state
      | top_of_stack :: _ ->
      (* let top_of_stack = pop (Spec.get_or_fail i.annotation_before).vstack in *)
        (* if !Value_set_options.print_trace then print_endline "return"; *)
        let vs = Abstract_store_domain.get state ~var:(Variable.Var top_of_stack) in
        if !Value_set_options.print_trace then print_endline ("\treturned value-set: " ^ Abstract_store_domain.Value.to_string vs);
        `Simple (Abstract_store_domain.set state ~var:(Variable.Var Var.Return) ~vs:vs)
      end
    | Unreachable -> `Simple  Abstract_store_domain.bottom
    | _ -> `Simple state


  (** [get_predecessors cfg block] returns the list of predecessor blocks for [block] in [cfg]. *)
  let get_predecessors 
      (cfg : annot_expected Cfg.t) 
      (block : annot_expected Basic_block.t) 
    : annot_expected Basic_block.t list =
    let predecessors = Cfg.predecessors cfg block.idx in
    List.filter ~f:(fun blk -> List.mem predecessors blk.idx ~equal:Int.equal) 
                         (Cfg.all_predecessors cfg block)

  (** [get_previous_stacks cfg predecessors] returns the annotated stacks for a list of predecessor blocks. *)
  let get_previous_stacks
      (cfg : annot_expected Cfg.t)
      (predecessors : annot_expected Basic_block.t list)
    : (int * Var.t list) list =
    let previous_annotations = 
      List.fold ~init:[] 
                ~f:(fun acc blk ->
                  (blk.idx, Cfg.state_after_block cfg blk.idx (Spec_inference.init_state cfg)) :: acc)
                predecessors
    in
    List.map 
      ~f:(fun x ->
        match x with
        | idx, Spec.Bottom -> idx, []
        | idx, NotBottom x -> idx, x.vstack)
      previous_annotations

  let merge_variables
      (cfg : annot_expected Cfg.t) 
      (block : annot_expected Basic_block.t)
      (state : state)
      : state =
    let merged_variables = List.rev (List.sort ~compare:(fun (v1, w1) (v2, w2) -> if Var.equal w1 w2 then Var.compare v1 v2 else Var.compare w1 w2) (Spec_inference.new_merge_variables cfg block)) in
    let merged_variables = List.map ~f:(fun (a, b) -> (Variable.Var b, Abstract_store_domain.get state ~var:(Variable.Var a))) merged_variables in
    let merged_variables =
      List.fold merged_variables ~init:[] ~f:(fun acc (v, vs) -> 
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
      (_module_ : Wasm_module.t) 
      (cfg : annot_expected Cfg.t) 
      (block : annot_expected Basic_block.t)
      (states : (int * state) list) 
    : state =
    match states with
    | [] -> 
      if !Value_set_options.print_trace then print_endline ("================ START OF FUNCTION ==================== DATA BLOCK #" ^ string_of_int block.idx);
      init_state cfg
    | _ ->
      begin match block.content with
      | Control { instr = Merge; _ } ->
        if !Value_set_options.print_trace then print_endline ("======================================================= " ^ (if IntSet.mem cfg.loop_heads block.idx then "LOOP HEAD" else "MERGE") ^ ": Control block #" ^ string_of_int block.idx);
        let states' = List.map ~f:(fun (_, s) -> s) states in
        (* Join all previous states: *)
        let new_state_without_merges = List.reduce_exn states' ~f:join_state in 
        merge_variables cfg block new_state_without_merges
      | Control _ ->
        if !Value_set_options.print_trace then print_endline ("======================================================= CONTROL BLOCK #" ^ string_of_int block.idx);
        begin match states with
        | (_, s) :: [] -> s
        | _ -> failwith (Printf.sprintf "Invalid block with multiple input states: %d" block.idx)
        end
      | _ -> 
        if !Value_set_options.print_trace then print_endline ("======================================================= DATA BLOCK #" ^ string_of_int block.idx);
        begin match states with
        | (_, s) :: [] -> s
        | _ -> failwith (Printf.sprintf "Invalid block with multiple input states: %d" block.idx)
        end
      end
    


  (** [summary cfg out_state] computes the value-set summary for a function using its [cfg] and final [out_state]. *)
  let summary (cfg : annot_expected Cfg.t) (out_state : state) : summary =
    if !Value_set_options.print_trace then print_endline "======================================================= SUMMARY";
    if !Value_set_options.print_trace then print_endline ("END STATE:\t" ^ Abstract_store_domain.to_string out_state);
    let init_spec = (Spec_inference.init_state cfg) in
    let function_summary =
      match Cfg.state_after_block cfg cfg.exit_block init_spec with
      | Bottom ->
        (* The function exit is likely unreachable, so we use a bottom summary *)
        Value_set_summary.bottom cfg Var.Set.empty
      | NotBottom _ ->
        Value_set_summary.make out_state
    in
    if !Value_set_options.print_trace then print_endline ("SUMMARY:\n" ^ Value_set_summary.to_string function_summary);
    function_summary

  (** [extract_summary cfg analyzed_cfg] extracts a value-set summary from the final result of the analysis. *)
  let extract_summary (cfg : annot_expected Cfg.t) (analyzed_cfg : state Cfg.t) : summary =
    let out_state = Cfg.state_after_block analyzed_cfg cfg.exit_block (init_state cfg) in
    summary cfg out_state
  

end