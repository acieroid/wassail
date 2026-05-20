open Core
open Helpers
open Reduced_interval_congruence

module Make (*: Transfer.TRANSFER *) = struct
  module State = Abstract_store_domain

  module RIC = Reduced_interval_congruence.RIC

  (* We need the variable names as annotations *)
  type annot_expected = Spec_domain.t


  (** The value-set specification for an imported function is an abstract store *)
  type value_set_specification = Abstract_store_domain.t

  (** The value-set specifications as a map from function name to specification.
      Stored as a reference so that it can be extended *)
  let value_set_specifications : value_set_specification StringMap.t ref =
    let _bottom = Abstract_store_domain.bottom in
    ref (StringMap.of_alist_exn [
      (* TODO: add specifications for a few common imported functions *)
    ])

  (** [init cfg] initializes the abstract state using the function argument and global variables from [cfg]. *)
  (* let init (cfg : 'a Cfg.t) : state = *)
  let init (module_ : Wasm_module.t) (funcinst : Func_inst.t) : State.t =
    let arg_types, _ = funcinst.typ in
    let nb_of_arguments = Func_inst.nargs funcinst in (*List.length arg_types in*)
    let global_types = Wasm_module.get_global_types module_ in
    { abstract_store =
        Variable.Map.of_alist_exn (
          (List.mapi arg_types ~f:(fun i _ -> 
            let variable = Var.Local i in
            let var_name = Var.to_string variable in
            (Variable.Var variable, Abstract_store_domain.Value.ValueSet (RIC.ric (0l, Int 0l, Int 0l, (var_name, 0l)))))) @
          (List.mapi global_types ~f:(fun i _ -> 
            let variable = Var.Global i in
            let var_name = Var.to_string variable in
            (Variable.Var variable, Abstract_store_domain.Value.ValueSet (RIC.ric (0l, Int 0l, Int 0l, (var_name, 0l)))))) @
          (List.mapi funcinst.code.locals ~f:(fun i _ -> 
            let variable = Var.Local (i + nb_of_arguments) in
            (Variable.Var variable, Abstract_store_domain.Value.ValueSet (RIC.ric (0l, Int 0l, Int 0l, ("", 0l)))))) @
          [(Variable.Mem RIC.Top), Abstract_store_domain.Value.ValueSet RIC.Top]);
      store_operations = RICSet.empty }

  (** [bottom_state cfg] returns the bottom abstract state. *)
  let bottom (*_cfg : 'a Cfg.t*) : State.t = Abstract_store_domain.bottom (* TODO: check that I don't need to add globals *)

  (** [state_to_string s] converts an abstract state [s] to its string representation. *)
  let state_to_string (s : State.t) : string = Abstract_store_domain.to_string s

  (** [join_state s1 s2] joins two abstract states [s1] and [s2]. *)
  let join_state (s1 : State.t) (s2 : State.t) : State.t = Abstract_store_domain.join s1 s2

  (** [widen_state s1 s2] performs widening between abstract states [s1] and [s2]. *)
  let widen_state (s1 : State.t) (s2 : State.t) : State.t = 
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
      (state : Spec_domain.SpecWithoutBottom.t) 
      ~(value : Variable.t) 
      ~(of_this_variable : Variable.t)
    : bool =
    (* print_endline ("is " ^ Variable.to_string value ^ " the value of " ^ Variable.to_string of_this_variable ^ " ?"); *)
    match of_this_variable with
    | Var Var.Local l ->
      if l < List.length state.locals then
        let value' = Variable.Var (Spec_inference.get (Option.value_exn (Int32.of_int l)) state.locals) in
        Variable.equal value value'
      else
        false
    | Var Var.Global g ->
      if g < List.length state.globals then
        let value' = Variable.Var (Spec_inference.get (Option.value_exn (Int32.of_int g)) state.globals) in
        Variable.equal value value'
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
    if !Value_set_options.print_trace then (
      print_endline (string_of_int i.line_number ^ ":\t" ^ Instr.data_to_string i.instr);
      (* if i.line_number = 172 then
        (print_endline "-------------------------------------------------------------------------------------------------------";
        let _ = In_channel.input_line_exn In_channel.stdin in
        ();) *)
    );
    let ret (i : annot_expected Instr.labelled_data) : Variable.t = 
      match List.hd (Spec_domain.get_or_fail i.annotation_after).vstack with
      | Some r -> Variable.Var r
      | None -> failwith "nothing on the stack" in
    match i.instr with
    (* TODO: is there a way to know the memory size? *)
    | MemorySize -> Abstract_store_domain.set state ~var:(ret i) ~vs:(Value_set_abstractions.ValueSet Top)
    | Nop | Drop | MemoryGrow -> state
    (* TODO: these 3 operations may modify memory content: *)
    | MemoryCopy | MemoryFill | MemoryInit _ -> state
    | RefIsNull | RefNull _ | RefFunc _ -> state
    | Select _ ->
      let x, y = (*pop2 (Spec_domain.get_or_fail i.annotation_before).vstack in*)
        match (Spec_domain.get_or_fail i.annotation_before).vstack with
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
      let top_of_stack = pop (Spec_domain.get_or_fail i.annotation_before).vstack in
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
      let top_of_stack = pop (Spec_domain.get_or_fail i.annotation_before).vstack in
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
      let top_of_stack = pop (Spec_domain.get_or_fail i.annotation_before).vstack in
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
                    if Abstract_store_domain.Value.equal vs (Abstract_store_domain.Value.ValueSet RIC.Top) then
                      (print_endline "SETTING GLOBAL TO TOP!!!!!!!!! press enter to continue";
                let _ = In_channel.input_line_exn In_channel.stdin in
                ());
          if !Value_set_options.print_trace then print_endline ("\tassigning value-set " ^ Abstract_store_domain.Value.to_string vs ^ " to variable " ^ Variable.to_string variable);
          Abstract_store_domain.set state ~var:variable ~vs
        | _ ->
          if !Value_set_options.print_trace then print_endline ("\ttransfering value-set of " ^ Var.to_string top_of_stack ^ " to global variable " ^ Variable.to_string variable);
                    let vs = Abstract_store_domain.Value.ValueSet (RIC.relative_ric (Var.to_string top_of_stack)) in
                    if Abstract_store_domain.Value.equal vs (Abstract_store_domain.Value.ValueSet RIC.Top) then
                      (print_endline "SETTING GLOBAL TO TOP!!!!!!!!! press enter to continue";
                    let _ = In_channel.input_line_exn In_channel.stdin in
                    ());
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
      let x, y = pop2 (Spec_domain.get_or_fail i.annotation_before).vstack in
      (* let result = pop (Spec_domain.get_or_fail i.annotation_after).vstack in *)

      let result = ret i in
      begin match binop with
      | { op = ShrU; typ = I32 } ->
        let x_value = Abstract_store_domain.get state ~var:(Variable.Var x) in
        let y_value = Abstract_store_domain.get state ~var:(Variable.Var y) in
        let result_value =
          begin match x_value, y_value with
          | ValueSet vs2, ValueSet vs1
          | ValueSet vs2, Boolean {numeric_value = vs1; _} 
          | Boolean {numeric_value = vs2; _}, ValueSet vs1 
          | Boolean {numeric_value = vs2; _}, Boolean {numeric_value = vs1; _} ->
            Abstract_store_domain.Value.ValueSet (RIC.shift_right_u vs1 vs2)
          | Boolean {numeric_value = vs2; _}, Bitfield bf1
          | ValueSet vs2, Bitfield bf1 -> ValueSet (RIC.of_bitfield (Bitfield.shift_right_unsigned bf1 (RIC.to_bitfield vs2)))
          | Bitfield bf2, ValueSet vs1
          | Bitfield bf2, Boolean {numeric_value = vs1; _} -> ValueSet (RIC.of_bitfield (Bitfield.shift_right_unsigned (RIC.to_bitfield vs1) bf2)) (* TODO : keep bitfield *)
          | Bitfield bf2, Bitfield bf1 -> Bitfield (Bitfield.shift_right_unsigned bf1 bf2)
          end in
        (* let () =
          print_endline ("result: " ^ Abstract_store_domain.Value.to_string result_value);
          let _ = In_channel.input_line_exn In_channel.stdin in
          () in *)
        if !Value_set_options.print_trace then 
          print_endline ("\t" ^ Var.to_string y ^ "(" ^ Abstract_store_domain.Value.to_string y_value ^ ") >> " ^ Var.to_string x ^ "(" ^ Abstract_store_domain.Value.to_string x_value ^ ") -> " ^ Variable.to_string result
            ^ "(" ^ State.Value.to_string result_value ^ ")");
        Abstract_store_domain.set state ~var:result ~vs:result_value
      | { op = Shl; typ = I32 } ->
        let x_value = Abstract_store_domain.get state ~var:(Variable.Var x) in
        let y_value = Abstract_store_domain.get state ~var:(Variable.Var y) in
        let result_value =
          begin match x_value, y_value with
          | ValueSet vs2, ValueSet vs1
          | ValueSet vs2, Boolean {numeric_value = vs1; _} 
          | Boolean {numeric_value = vs2; _}, ValueSet vs1 
          | Boolean {numeric_value = vs2; _}, Boolean {numeric_value = vs1; _} ->
            Abstract_store_domain.Value.ValueSet (RIC.shift_left vs1 vs2)
          | Boolean {numeric_value = vs2; _}, Bitfield bf1
          | ValueSet vs2, Bitfield bf1 -> ValueSet (RIC.of_bitfield (Bitfield.shift_left bf1 (RIC.to_bitfield vs2)))
          | Bitfield bf2, ValueSet vs1
          | Bitfield bf2, Boolean {numeric_value = vs1; _} -> ValueSet (RIC.of_bitfield (Bitfield.shift_left (RIC.to_bitfield vs1) bf2)) (* TODO : keep bitfield *)
          | Bitfield bf2, Bitfield bf1 -> Bitfield (Bitfield.shift_left bf1 bf2)
          end in
        if !Value_set_options.print_trace then 
          print_endline ("\t" ^ Var.to_string y ^ "(" ^ Abstract_store_domain.Value.to_string y_value ^ ") << " ^ Var.to_string x ^ "(" ^ Abstract_store_domain.Value.to_string x_value ^ ") -> " ^ Variable.to_string result
            ^ "(" ^ State.Value.to_string result_value ^ ")");
        (* let () =
          print_endline ("result: " ^ Abstract_store_domain.Value.to_string result_value);
          let _ = In_channel.input_line_exn In_channel.stdin in
          () in *)
        Abstract_store_domain.set state ~var:result ~vs:result_value
      | { op = Or; typ = I32 } ->
        let x_value = Abstract_store_domain.get state ~var:(Variable.Var x) in
        let y_value = Abstract_store_domain.get state ~var:(Variable.Var y) in
        if !Value_set_options.print_trace then print_endline ("\t" ^ Abstract_store_domain.Value.to_string x_value ^ " or " ^ Abstract_store_domain.Value.to_string y_value ^ " -> " ^ Variable.to_string result);
        let result_value =
          begin match x_value, y_value with
          | ValueSet vs1, ValueSet vs2
          | ValueSet vs1, Boolean {numeric_value = vs2; _}
          | Boolean {numeric_value = vs1; _}, ValueSet vs2 -> 
            (* Abstract_store_domain.Value.ValueSet (RIC.or_ vs1 vs2) *)
            Abstract_store_domain.Value.ValueSet (RIC.or_ vs1 vs2)
          | Boolean v1, Boolean v2 -> Boolean (Boolean.or_ v1 v2)
          | Bitfield bf, ValueSet vs
          | Bitfield bf, Boolean {numeric_value = vs; _}
          | ValueSet vs, Bitfield bf
          | Boolean {numeric_value = vs; _}, Bitfield bf -> ValueSet (RIC.of_bitfield (Bitfield.or_ (RIC.to_bitfield vs) bf))
          | Bitfield bf1, Bitfield bf2 -> Bitfield (Bitfield.or_ bf1 bf2)
          end in
        Abstract_store_domain.set state ~var:result ~vs:result_value
      | { op = And; typ = I32 } -> (* TODO: refactor function to Abstract_store_domain *)
        let x_value = Abstract_store_domain.get state ~var:(Variable.Var x) in
        let y_value = Abstract_store_domain.get state ~var:(Variable.Var y) in
        if !Value_set_options.print_trace then print_endline ("\t" ^ Var.to_string x ^ "(" ^ Abstract_store_domain.Value.to_string x_value ^ ") & " ^ Var.to_string y ^ "(" ^ Abstract_store_domain.Value.to_string y_value ^ ") -> " ^ Variable.to_string result);
        let result_value =
          begin match x_value, y_value with
          | ValueSet vs1, ValueSet vs2
          | ValueSet vs1, Boolean {numeric_value = vs2; _}
          | Boolean {numeric_value = vs1; _}, ValueSet vs2 -> 
            (* Abstract_store_domain.Value.ValueSet (RIC.and_ vs1 vs2) *)
            Abstract_store_domain.Value.ValueSet (RIC.and_ vs1 vs2)
          | Bitfield bf, ValueSet vs
          | Bitfield bf, Boolean {numeric_value = vs; _}
          | ValueSet vs, Bitfield bf
          | Boolean {numeric_value = vs; _}, Bitfield bf -> ValueSet (RIC.of_bitfield (Bitfield.and_ bf (RIC.to_bitfield vs)))
          | Bitfield bf1, Bitfield bf2 -> Bitfield (Bitfield.and_ bf1 bf2)
            (* let numeric_value = (RIC.and_ vs1 vs2) in
            begin match vs1, vs2 with
            (* PARITY CHECK *)
            | RIC {stride = 0l; lower_bound = Int 0l; upper_bound = Int 0l; offset = ("", 1l)}, vs2 ->
              let true_ = RIC.meet vs2 (RIC.ric (2l, NegInfinity, Infinity, ("", 1l))) in
              let false_ = RIC.meet vs2 (RIC.ric (2l, NegInfinity, Infinity, ("", 0l))) in
              let tf = (Variable.Map.set 
                          Variable.Map.empty 
                          ~key:(Variable.Var y) 
                          ~data:Boolean.{True_or_false.true_ = true_; false_ = false_}) in
              Abstract_store_domain.Value.Boolean {true_or_false = tf; numeric_value = numeric_value}
            | vs1, RIC {stride = 0l; lower_bound = Int 0l; upper_bound = Int 0l; offset = ("", 1l)} ->
              let true_ = RIC.meet vs1 (RIC.ric (2l, NegInfinity, Infinity, ("", 1l))) in
              let false_ = RIC.meet vs1 (RIC.ric (2l, NegInfinity, Infinity, ("", 0l))) in
              let tf = (Variable.Map.set 
                          Variable.Map.empty 
                          ~key:(Variable.Var x) 
                          ~data:Boolean.{True_or_false.true_ = true_; false_ = false_}) in
              Abstract_store_domain.Value.Boolean {true_or_false = tf; numeric_value = numeric_value}
            (* MULTIPLE OF 4 *)
            | RIC {stride = 0l; lower_bound = Int 0l; upper_bound = Int 0l; offset = ("", 3l)}, vs2 ->
              let true_ = vs2 in
              let false_ = RIC.meet vs2 (RIC.ric (4l, NegInfinity, Infinity, ("", 0l))) in
              let tf = (Variable.Map.set 
                          Variable.Map.empty 
                          ~key:(Variable.Var y) 
                          ~data:Boolean.{True_or_false.true_ = true_; false_ = false_}) in
              Abstract_store_domain.Value.Boolean {true_or_false = tf; numeric_value = numeric_value}
            | vs1, RIC {stride = 0l; lower_bound = Int 0l; upper_bound = Int 0l; offset = ("", 3l)} ->
              let true_ = vs1 in
              let false_ = RIC.meet vs1 (RIC.ric (4l, NegInfinity, Infinity, ("", 0l))) in
              let tf = (Variable.Map.set 
                          Variable.Map.empty 
                          ~key:(Variable.Var x) 
                          ~data:Boolean.{True_or_false.true_ = true_; false_ = false_}) in
              Abstract_store_domain.Value.Boolean {true_or_false = tf; numeric_value = numeric_value}
            (* MULTIPLE OF 8 *)
            | RIC {stride = 0l; lower_bound = Int 0l; upper_bound = Int 0l; offset = ("", 7l)}, vs2 ->
              let true_ = vs2 in
              let false_ = RIC.meet vs2 (RIC.ric (8l, NegInfinity, Infinity, ("", 0l))) in
              let tf = (Variable.Map.set 
                          Variable.Map.empty 
                          ~key:(Variable.Var y) 
                          ~data:Boolean.{True_or_false.true_ = true_; false_ = false_}) in
              Abstract_store_domain.Value.Boolean {true_or_false = tf; numeric_value = numeric_value}
            | vs1, RIC {stride = 0l; lower_bound = Int 0l; upper_bound = Int 0l; offset = ("", 7l)} ->
              let true_ = vs1 in
              let false_ = RIC.meet vs1 (RIC.ric (8l, NegInfinity, Infinity, ("", 0l))) in
              let tf = (Variable.Map.set 
                          Variable.Map.empty 
                          ~key:(Variable.Var x) 
                          ~data:Boolean.{True_or_false.true_ = true_; false_ = false_}) in
              Abstract_store_domain.Value.Boolean {true_or_false = tf; numeric_value = numeric_value}
            (* MULTIPLE OF 16 *)
            | RIC {stride = 0l; lower_bound = Int 0l; upper_bound = Int 0l; offset = ("", 15l)}, vs2 ->
              let true_ = vs2 in
              let false_ = RIC.meet vs2 (RIC.ric (16l, NegInfinity, Infinity, ("", 0l))) in
              let tf = (Variable.Map.set 
                          Variable.Map.empty 
                          ~key:(Variable.Var y) 
                          ~data:Boolean.{True_or_false.true_ = true_; false_ = false_}) in
              Abstract_store_domain.Value.Boolean {true_or_false = tf; numeric_value = numeric_value}
            | vs1, RIC {stride = 0l; lower_bound = Int 0l; upper_bound = Int 0l; offset = ("", 15l)} ->
              let true_ = vs1 in
              let false_ = RIC.meet vs1 (RIC.ric (16l, NegInfinity, Infinity, ("", 0l))) in
              let tf = (Variable.Map.set 
                          Variable.Map.empty 
                          ~key:(Variable.Var x) 
                          ~data:Boolean.{True_or_false.true_ = true_; false_ = false_}) in
              Abstract_store_domain.Value.Boolean {true_or_false = tf; numeric_value = numeric_value}
            | _ -> ValueSet numeric_value
            end *)
          | Boolean v1, Boolean v2 -> Boolean (Boolean.and_ v1 v2)
          end
        in
        if !Value_set_options.print_trace then print_endline ("\t" ^ Abstract_store_domain.Value.to_string result_value);
        Abstract_store_domain.set state ~var:result ~vs:result_value
      | { op = Xor; typ = I32 } -> (* TODO: refactor function to Abstract_store_domain *)
        (* let next_power_of_2 n = (* TODO: refactor function in Maths module *)
          if n < 1 then 
            1
          else
            let log2 = log (float_of_int (n + 1)) /. log 2.0 in
            int_of_float (2. ** (Float.round_up log2)) in *)
        let x_value = Abstract_store_domain.get state ~var:(Variable.Var x) in
        let y_value = Abstract_store_domain.get state ~var:(Variable.Var y) in
        if !Value_set_options.print_trace then print_endline ("\t" ^ Abstract_store_domain.Value.to_string x_value ^ " xor " ^ Abstract_store_domain.Value.to_string y_value ^ " -> " ^ Variable.to_string result);
        let result_value =
          begin match x_value, y_value with
          | ValueSet vs1, ValueSet vs2
          | ValueSet vs1, Boolean {numeric_value = vs2; _}
          | Boolean {numeric_value = vs1; _}, ValueSet vs2 -> 
            (* Abstract_store_domain.Value.ValueSet (RIC.xor_ vs1 vs2) *)
            Abstract_store_domain.Value.ValueSet (RIC.xor_ vs1 vs2)
          | Bitfield bf, ValueSet vs
          | Bitfield bf, Boolean {numeric_value = vs; _}
          | ValueSet vs, Bitfield bf
          | Boolean {numeric_value = vs; _}, Bitfield bf -> ValueSet (RIC.of_bitfield (Bitfield.xor_ bf (RIC.to_bitfield vs)))
          | Bitfield bf1, Bitfield bf2 -> Bitfield (Bitfield.xor_ bf1 bf2)
            (* let numeric_value = (RIC.xor_ vs1 vs2) in
            begin match vs1, vs2 with
            | RIC {stride = 0l; lower_bound = Int 0l; upper_bound = Int 0l; offset = ("", n)}, vs2 when Int32.(n >= 1l) -> 
              let false_ = RIC.meet vs2 (RIC.ric (0l, Int 0l, Int 0l, ("", n))) in
              let true_ = RIC.remove ~this:(RIC.ric (0l, Int 0l, Int 0l, ("", n))) ~from:vs2 in
              let true_ = List.fold true_ ~init:RIC.Bottom ~f:(fun acc r -> RIC.join acc r) in
              let tf = (Variable.Map.set 
                          Variable.Map.empty 
                          ~key:(Variable.Var y) 
                          ~data:Boolean.{True_or_false.true_ = true_; false_ = false_}) in
              Abstract_store_domain.Value.Boolean {true_or_false = tf; numeric_value = numeric_value}
            | vs1, RIC {stride = 0l; lower_bound = Int 0l; upper_bound = Int 0l; offset = ("", n)} when Int32.(n >= 1l) -> 
              let false_ = RIC.meet vs1 (RIC.ric (0l, Int 0l, Int 0l, ("", n))) in
              let true_ = RIC.remove ~this:(RIC.ric (0l, Int 0l, Int 0l, ("", n))) ~from:vs1 in
              let true_ = List.fold true_ ~init:RIC.Bottom ~f:(fun acc r -> RIC.join acc r) in
              let tf = (Variable.Map.set 
                          Variable.Map.empty 
                          ~key:(Variable.Var x) 
                          ~data:Boolean.{True_or_false.true_ = true_; false_ = false_}) in
              Abstract_store_domain.Value.Boolean {true_or_false = tf; numeric_value = numeric_value}
            | _ -> ValueSet numeric_value
            end *)
          | Boolean v1, Boolean v2 -> Boolean (Boolean.xor_ v1 v2)
          end
        in
        if !Value_set_options.print_trace then print_endline ("\t" ^ Abstract_store_domain.Value.to_string result_value);
        (* let () =
          let _ = In_channel.input_line_exn In_channel.stdin in
          () in *)
        Abstract_store_domain.set state ~var:result ~vs:result_value
      | { op = Add; typ = I32 } -> (* i32 addition *) 
        Abstract_store_domain.i32_add state ~x:(Variable.Var x) ~y:(Variable.Var y) ~result
      | { op = Sub; typ = I32 } -> (* i32 subtraction *) 
        Abstract_store_domain.i32_sub state ~x:(Variable.Var x) ~y:(Variable.Var y) ~result
      | { typ = I32; _ } 
      | _ -> (* other operations result in a pointer that can point anywhere *)
        if !Value_set_options.print_trace then print_endline "\tthis type of binary operation results in a pointer that can point anywhere";
        Abstract_store_domain.to_top_RIC state result
      end
    | Load load ->
      let size = Memoryop.size load in
      let address = pop (Spec_domain.get_or_fail i.annotation_before).vstack in
      if !Value_set_options.print_trace then
        print_endline ("\tAddress(" ^ Var.to_string address ^ ")");
      let vs = Abstract_store_domain.get state ~var:(Variable.Var address) in
      (* Update accessed address *)
        let previously_accessed = Abstract_store_domain.get state ~var:Variable.Accessed in
        let vs_plus_offset =
          begin match load, vs with
          | { offset = offset; _ }, Abstract_store_domain.Value.ValueSet vs ->
            let offset = Int32.of_int_exn offset in
            Abstract_store_domain.Value.ValueSet (RIC.add_offset vs offset)
          | _ -> failwith "Trying to use boolean as an address"
          end
        in
        (* TODO: maybe include all touched addresses depending on size? *)
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
          let offset = Int32.of_int_exn offset in
          if Int32.(size = 4l) then
            let is_stack = !Value_set_options.disjoint_stack
              && String.equal "g0" (Abstract_store_domain.Value.extract_relative_offset vs) in
            match vs with
            | Abstract_store_domain.Value.ValueSet vs -> (* TODO: factoriser ce qui suit:*)
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
            | Abstract_store_domain.Value.Boolean {numeric_value = vs; _} -> 
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
            | Bitfield bf ->
              let vs = RIC.of_bitfield bf in
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
          else
            Abstract_store_domain.to_top_RIC state (ret i)
        end
      (* end *)
    | Store store ->
      let store = 
        Abstract_store_domain.store 
          ~state ~instruction:store ~annotation_before:(Some i.annotation_before) ~value:None ~address:None
      in
      (* let () =
        let _ = In_channel.input_line_exn In_channel.stdin in
        () in *)
      store
    | Compare comp ->
      let var2, var1 = pop2 (Spec_domain.get_or_fail i.annotation_before).vstack in
      (* print_endline ("var1: " ^ Var.to_string var1 ^ ", var2: " ^ Var.to_string var2); *)
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
      (* begin match vs1, vs2 with
      | Boolean {numeric_value = vs1; _}, Boolean {numeric_value = vs2; _}
      | Boolean {numeric_value = vs1; _}, ValueSet vs2
      | ValueSet vs1, Boolean {numeric_value = vs2; _}
      | ValueSet vs1, ValueSet vs2 -> *)
        begin match comp with
        | {op = Eq; typ = I32} ->
          if !Value_set_options.print_trace then print_endline ("\t" ^ RIC.to_string vs1 ^ " == " ^ RIC.to_string vs2);
          let vs1_true = 
            if RIC.comparable_offsets vs1 vs2 then
              RIC.meet vs1 vs2
            else
              vs1
          in
          let vs1_false = 
            if RIC.comparable_offsets vs1 vs2 then
              let vs2' = RIC.complement vs2 in
              List.fold vs2' ~init:RIC.Bottom ~f:(fun acc x -> RIC.join acc (RIC.meet vs1 x))
            else
              vs1
          in
          let state =
            begin match var1 with
            (* | Var.Const _ -> state TODO: numeric value (0,1) *)
            | _ ->
              Abstract_store_domain.set 
                state 
                ~var:(ret i) 
                ~vs:(Boolean { Boolean.true_or_false = (Variable.Map.set 
                                                          Variable.Map.empty 
                                                          ~key:(Variable.Var var1) 
                                                          ~data:Boolean.{True_or_false.true_ = vs1_true; false_ = vs1_false});
                              numeric_value = RIC.ric (1l, Int 0l, Int 1l, ("", 0l)) })
            end in
          (* let () =
            print_endline ("result: " ^ Abstract_store_domain.Value.to_string (Abstract_store_domain.get state ~var:(ret i)));
            let _ = In_channel.input_line_exn In_channel.stdin in
            () in *)
          state
        | {op = LeU; typ = I32} -> 
          if !Value_set_options.print_trace then print_endline ("\t" ^ RIC.to_string vs1 ^ " ≤ " ^ RIC.to_string vs2);
          let vs2' = RIC.remove_lower_bound vs2 in
          let vs2'' = RIC.add_offset (RIC.remove_upper_bound vs2) 1l in
          let vs1_true = if RIC.comparable_offsets vs1 vs2' then RIC.meet vs1 vs2' else vs1 in
          let vs1_false = if RIC.comparable_offsets vs1 vs2' then RIC.meet vs1 vs2'' else vs1 in
          if !Value_set_options.print_trace then print_endline ("\ttrue: " ^ RIC.to_string vs1_true ^ "\n\tfalse: " ^ RIC.to_string vs1_false);
          begin match var1 with
          (* | Var.Const _ -> state TODO: numeric value (0,1) *)
          | _ ->
            Abstract_store_domain.set 
              state 
              ~var:(ret i) 
              ~vs:(Boolean { Boolean.true_or_false = (Variable.Map.set 
                                                        Variable.Map.empty 
                                                        ~key:(Variable.Var var1) 
                                                        ~data:Boolean.{True_or_false.true_ = vs1_true; false_ = vs1_false});
                             numeric_value = RIC.ric (1l, Int 0l, Int 1l, ("", 0l)) })
          end
        | {op = LtU; typ = I32} ->
          if !Value_set_options.print_trace then print_endline ("\t" ^ RIC.to_string vs1 ^ " < " ^ RIC.to_string vs2);
          let vs2' = RIC.add_offset (RIC.remove_lower_bound vs2) (-1l) in
          let vs2'' = RIC.remove_upper_bound vs2 in
          let vs1_true = if RIC.comparable_offsets vs1 vs2' then RIC.meet vs1 vs2' else vs1 in
          let vs1_false = if RIC.comparable_offsets vs1 vs2' then RIC.meet vs1 vs2'' else vs1 in
          if !Value_set_options.print_trace then print_endline ("\ttrue: " ^ RIC.to_string vs1_true ^ ",  false: " ^ RIC.to_string vs1_false);
          begin match var1 with
          (* | Var.Const _ -> state *)
          | _ ->
            Abstract_store_domain.set 
              state 
              ~var:(ret i) 
              ~vs:(Boolean { Boolean.true_or_false = (Variable.Map.set 
                                                        Variable.Map.empty 
                                                        ~key:(Variable.Var var1) 
                                                        ~data:Boolean.{True_or_false.true_ = vs1_true; false_ = vs1_false});
                             numeric_value = RIC.ric (1l, Int 0l, Int 1l, ("", 0l)) })
          end
        | {op = GeU; typ = I32} -> 
          if !Value_set_options.print_trace then print_endline ("\t" ^ RIC.to_string vs1 ^ " ≥ " ^ RIC.to_string vs2);
          let vs2' = RIC.remove_upper_bound vs2 in
          let vs2'' = RIC.add_offset (RIC.remove_lower_bound vs2) (-1l) in
          let vs1_true = if RIC.comparable_offsets vs1 vs2' then RIC.meet vs1 vs2' else vs1 in
          let vs1_false = if RIC.comparable_offsets vs1 vs2' then RIC.meet vs1 vs2'' else vs1 in
          if !Value_set_options.print_trace then print_endline ("\ttrue: " ^ RIC.to_string vs1_true ^ "\n\tfalse: " ^ RIC.to_string vs1_false);
          begin match var1 with
          (* | Var.Const _ -> state *)
          | _ ->
            Abstract_store_domain.set 
              state 
              ~var:(ret i) 
              ~vs:(Boolean { Boolean.true_or_false = (Variable.Map.set 
                                                        Variable.Map.empty 
                                                        ~key:(Variable.Var var1) 
                                                        ~data:Boolean.{True_or_false.true_ = vs1_true; false_ = vs1_false});
                             numeric_value = RIC.ric (1l, Int 0l, Int 1l, ("", 0l)) })
          end
        | {op = GtU; typ = I32} ->
          if !Value_set_options.print_trace then print_endline ("\t" ^ RIC.to_string vs1 ^ " > " ^ RIC.to_string vs2);
          let vs2' = RIC.add_offset (RIC.remove_upper_bound vs2) 1l in
          let vs2'' = RIC.remove_lower_bound vs2 in
          let vs1_true = if RIC.comparable_offsets vs1 vs2' then RIC.meet vs1 vs2' else vs1 in
          let vs1_false = if RIC.comparable_offsets vs1 vs2' then RIC.meet vs1 vs2'' else vs1 in
          if !Value_set_options.print_trace then print_endline ("\ttrue: " ^ RIC.to_string vs1_true ^ "\n\tfalse: " ^ RIC.to_string vs1_false);
          begin match var1 with
          (* | Var.Const _ -> state *)
          | _ ->
            Abstract_store_domain.set 
              state 
              ~var:(ret i) 
              ~vs:(Boolean { Boolean.true_or_false = (Variable.Map.set 
                                                        Variable.Map.empty 
                                                        ~key:(Variable.Var var1) 
                                                        ~data:Boolean.{True_or_false.true_ = vs1_true; false_ = vs1_false});
                             numeric_value = RIC.ric (1l, Int 0l, Int 1l, ("", 0l)) })
          end
        | _ -> Abstract_store_domain.set state ~var:(ret i) ~vs:(Boolean {Boolean.true_or_false = Variable.Map.empty; numeric_value = RIC.Top})
        (* end *)
      end
    | Test test -> 
      let var = pop (Spec_domain.get_or_fail i.annotation_before).vstack in
      let vs = Abstract_store_domain.get state ~var:(Variable.Var var) in
      begin match test with
      | I32Eqz ->
        Abstract_store_domain.set
          state
          ~var:(ret i)
          ~vs:(Value_set_abstractions.eqz ~var:(Variable.Var var) vs)
      | _ -> Abstract_store_domain.set state ~var:(ret i) ~vs:(Boolean {Boolean.true_or_false = Variable.Map.empty; numeric_value = RIC.Top})
      end
    | Unary { op = Clz; _ } ->
      let var = pop (Spec_domain.get_or_fail i.annotation_before).vstack in
      let vs = Abstract_store_domain.get state ~var:(Variable.Var var) in
      Abstract_store_domain.set
        state
        ~var:(ret i)
        ~vs:(Value_set_abstractions.count_leading_zeros vs)
    | Unary { op = Ctz; _ } ->
      let var = pop (Spec_domain.get_or_fail i.annotation_before).vstack in
      let vs = Abstract_store_domain.get state ~var:(Variable.Var var) in
      Abstract_store_domain.set
        state
        ~var:(ret i)
        ~vs:(Value_set_abstractions.count_trailing_zeros vs)
    | Unary { op = Popcnt; _ } ->
      let var = pop (Spec_domain.get_or_fail i.annotation_before).vstack in
      let vs = Abstract_store_domain.get state ~var:(Variable.Var var) in
      Abstract_store_domain.set
        state
        ~var:(ret i)
        ~vs:(Value_set_abstractions.population_count vs)
    | Unary { op = ExtendS _; _} ->
      Abstract_store_domain.set
        state
        ~var:(ret i)
        ~vs:(ValueSet RIC.Top)
    | Unary { typ = I32; _ } -> assert false (* No other unary operators on 32 bit integers *)
    | Unary _ ->
      Abstract_store_domain.set
        state
        ~var:(ret i)
        ~vs:(ValueSet RIC.Top)
    | Convert _ ->
      Abstract_store_domain.set state ~var:(ret i) ~vs:(Value_set_abstractions.ValueSet Top)


  let apply_condition 
      (state : State.t) 
      ~(condition : Variable.t * Boolean.t)
      (spec_state : Spec_domain.SpecWithoutBottom.t) 
      : State.t * State.t =
    let var, boolean = condition in
    let state = 
      { Abstract_store_domain.abstract_store = Variable.Map.remove state.abstract_store var;
        store_operations = state.store_operations } in
    let locals_and_globals = Abstract_store_domain.extract_locals_and_globals state in
    (* print_endline ("locals: " ^ List.to_string ~f:Var.to_string spec_state.locals);
    print_endline ("globals: " ^ List.to_string ~f:Var.to_string spec_state.globals);
    print_endline ("locals&globals: " ^ List.to_string ~f:Variable.to_string locals_and_globals); *)
    let true_ = 
      if Boolean.can_be_true boolean then
        let true_ =
          Abstract_store_domain.make_compatible 
            ~this_store:
              { Abstract_store_domain.abstract_store = (Variable.Map.map ~f:(fun x -> (Abstract_store_domain.Value.ValueSet x.true_)) boolean.true_or_false);
                store_operations = state.store_operations }
            ~relative_to:state in
        (* let true_ = *)
          { Abstract_store_domain.abstract_store =
              Variable.Map.fold true_.abstract_store
                ~init:state.abstract_store 
                ~f:(fun ~key ~data acc -> 
                  let acc = Variable.Map.set ~key ~data acc in
                  List.fold locals_and_globals
                    ~init:acc
                    ~f:(fun acc v -> 
                      (* print_endline ("v: " ^ Variable.to_string v); *)
                      if is_this_the_value_of spec_state ~value:key ~of_this_variable:v then
                        Variable.Map.set ~key:v ~data acc
                      else
                        acc));
            store_operations = state.store_operations }
      else
        Abstract_store_domain.bottom
    in
    let false_ = 
      if Boolean.can_be_false boolean then
        let false_ =
          Abstract_store_domain.make_compatible 
            ~this_store:
              { Abstract_store_domain.abstract_store = (Variable.Map.map ~f:(fun x -> (Abstract_store_domain.Value.ValueSet x.false_)) boolean.true_or_false);
                store_operations = state.store_operations }
            ~relative_to:state in
        (* let false_ = *)
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
      else
        Abstract_store_domain.bottom
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
    if !Value_set_options.print_trace then (
      print_endline (string_of_int i.line_number ^ ":\t" ^ Instr.control_to_short_string i.instr);
      if i.line_number = 172 then
        print_endline "-------------------------------------------------------------------------------------------------------"
    );
    (* let _apply_summary (f : Int32.t) (arity : int * int) (state : State.t) : State.t =
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
        let args = List.take (Spec_domain.get_or_fail i.annotation_before).vstack (fst arity) in
        let return_variable = if snd arity = 1 then List.hd (Spec_domain.get_or_fail i.annotation_after).vstack else None in
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
          if !Value_set_options.print_trace then Log.info (Printf.sprintf "function is named %s" descr.name);
          if !Value_set_options.print_trace then Log.warn "Exports have not been implemented yet!";
          value_set_after_call
        | None -> value_set_after_call *)
    (* in *)
    match i.instr with
    (* | Call (arity, _, f) -> 
      if !Value_set_options.print_trace then print_endline ("\t(nb of arguments: " ^ string_of_int (fst arity) ^ ", nb of return values: " ^ string_of_int (snd arity) ^ ")");
      let new_store = apply_summary f arity state in
      let new_store = Abstract_store_domain.remove_pointers_to_top new_store in
      `Simple new_store
    | CallIndirect (_, arity, _, typ) ->
      let targets = Call_graph.indirect_call_targets module_ typ in
      (* Apply the summaries *)
      `Simple (List.fold_left targets
        ~init:Abstract_store_domain.bottom
        ~f:(fun acc idx -> Abstract_store_domain.join (apply_summary idx arity state) acc)) *)
    | Br _ -> `Simple state
    | BrIf _ | If _ -> 
      let condition = Variable.Var (pop (Spec_domain.get_or_fail i.annotation_before).vstack) in
      let boolean_value = Abstract_store_domain.get state ~var:condition in
      (* print_endline ("boolean value: " ^ State.Value.to_string boolean_value); *)
      begin match boolean_value with
      | ValueSet vs -> 
        let false_ = 
          if RIC.may_be_false vs then
            state
          else
            Abstract_store_domain.bottom
        and true_ = 
          if RIC.may_be_true vs then
            state
          else
            Abstract_store_domain.bottom
        in
        `Branch (true_, false_)
      | Boolean boolean_value -> 
        let state_if_true, state_if_false = apply_condition state ~condition:(condition, boolean_value) (Spec_domain.get_or_fail i.annotation_after)  in
        `Branch (state_if_true, state_if_false)
      | Bitfield bf ->
        let false_ = 
          if Bitfield.may_be_false bf then
            state
          else
            Abstract_store_domain.bottom
        and true_ = 
          if Bitfield.may_be_true bf then
            state
          else
            Abstract_store_domain.bottom
        in
        `Branch (true_, false_)
      end
    | Return -> 
      begin match (Spec_domain.get_or_fail i.annotation_before).vstack with
      | [] -> `Simple state
      | top_of_stack :: _ ->
        let ret_var = Variable.Var ((Spec_domain.get_or_fail i.annotation_after).vstack |> pop) in
        let vs = Abstract_store_domain.get state ~var:(Variable.Var top_of_stack) in
        if !Value_set_options.print_trace then print_endline ("\treturned value-set: " ^ Abstract_store_domain.Value.to_string vs);
        `Simple (Abstract_store_domain.set state ~var:ret_var ~vs:vs)
        (* TODO: generalize for more than one return *)
      end
    | Unreachable -> `Simple  Abstract_store_domain.bottom
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
      (_module_ : Wasm_module.t)
      (cfg : annot_expected Cfg.t) 
      (block : annot_expected Basic_block.t)
      (state : State.t)
      : State.t =
    let merged_variables = List.rev (List.sort ~compare:(fun (v1, w1) (v2, w2) -> if Var.equal w1 w2 then Var.compare v1 v2 else Var.compare w1 w2) (Spec_inference.new_merge_variables _module_ cfg block)) in
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
        let states' = List.map ~f:(fun (_, s) -> s) predecessors in
        (* Join all previous states: *)
        let new_state_without_merges = List.reduce_exn states' ~f:join_state in 
        merge_variables module_ cfg block new_state_without_merges
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
      (_module : Wasm_module.t)
      (desc : Wasm_module.func_desc)
      (annot_before : annot_expected)
      (annot_after : annot_expected)
      (state : State.t)
    : State.t =
    Spec_domain.wrap annot_before ~default:State.bottom ~f:(fun _annotation_before ->
      Spec_domain.wrap annot_after ~default:State.bottom ~f:(fun _annotation_after ->
        match StringMap.find !value_set_specifications desc.name with
        | None ->
          Log.warn (Printf.sprintf "No specification found for imported function %s (index: %ld): assuming abstract store is preserved" desc.name desc.idx);
          state
        | Some _spec ->
          (* TODO: implement this! *)
          Log.warn "Imported functions have not been implemented yet! It is assumed that the abstract store remains unchanged by the function call.";
          state
      )
    )

end