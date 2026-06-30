open Core
open Helpers

let print_widening 
    (store1 : 'a)
    (store2 : 'a)
    (widened_state : 'a)
    (to_string : 'a -> string)
  : unit =
  let print_all = !Value_set_options.show_intermediates in
  (Value_set_options.show_intermediates := true;
  Printf.printf
     "\twidening:\n\t\tstate1: %s\n\t\tstate2: %s\n\t\twidened state: %s%!\n"
        (to_string store1)
        (to_string store2)
        (to_string widened_state);
  Value_set_options.show_intermediates := print_all)
let widening
    (store1 : 'a)
    (store2 : 'a)
    (widened_state : 'a)
    (to_string : 'a -> string)
  : unit =
  if !Value_set_options.print_trace then
    print_widening store1 store2 widened_state to_string
  else
    ()

let print_binop
    (lhs : Variable.t)
    (lhs_value : Value_set_abstraction.t)
    (symbol : string)
    (rhs : Variable.t)
    (rhs_value : Value_set_abstraction.t)
    (result : Variable.t)
    (result_value : Value_set_abstraction.t)
  : unit =
  Printf.printf 
    "\t%s(%s) %s %s(%s) -> %s(%s)%!\n"
    (Variable.to_string lhs)
    (Value_set_abstraction.to_string lhs_value)
    symbol
    (Variable.to_string rhs)
    (Value_set_abstraction.to_string rhs_value)
    (Variable.to_string result)
    (Value_set_abstraction.to_string result_value)
let binop
    (lhs : Variable.t)
    (lhs_value : Value_set_abstraction.t)
    (symbol : string)
    (rhs : Variable.t)
    (rhs_value : Value_set_abstraction.t)
    (result : Variable.t)
    (result_value : Value_set_abstraction.t)
  : unit =
  if !Value_set_options.print_trace then
    print_binop lhs lhs_value symbol rhs rhs_value result result_value
  else
    ()

let print_unary_op
    (op : string)
    (x : Value_set_abstraction.t)
    (y : Value_set_abstraction.t)
  : unit =
  Printf.printf 
    "\t%s (%s) -> %s%!\n" 
    op
    (Value_set_abstraction.to_string x)
    (Value_set_abstraction.to_string y)
let unop
    (op : string)
    (x : Value_set_abstraction.t)
    (y : Value_set_abstraction.t)
  : unit =
  if !Value_set_options.print_trace then
    print_unary_op op x y
  else
    ()

let print_load 
    (address : Var.t)
    (address_value : Value_set_abstraction.t)
    (offset : int)
    (address_plus_offset : Reduced_interval_congruence.RIC.t)
    (result : Variable.t)
    (loaded_value : Value_set_abstraction.t)
  : unit =
  Printf.printf 
    "\taddress: %s(%s)\n\toffset: %d\n\tloading content of %s into variable %s\n\tloaded value: %s%!\n"
    (Var.to_string address)
    (Value_set_abstraction.to_string address_value)
    offset
    Variable.(Mem address_plus_offset |> to_string)
    (Variable.to_string result)
    (Value_set_abstraction.to_string loaded_value)
let load
    (address : Var.t)
    (address_value : Value_set_abstraction.t)
    (offset : int)
    (address_plus_offset : Reduced_interval_congruence.RIC.t)
    (result : Variable.t)
    (loaded_value : Value_set_abstraction.t)
  : unit =
  if !Value_set_options.print_trace then
    print_load address address_value offset address_plus_offset result loaded_value
  else
    ()

let print_not_i32 () : unit = 
  Printf.printf "\tNot an i32 integer: shouldn't be used as a pointer%!\n"
let not_i32 () : unit =
  if !Value_set_options.print_trace then
    print_not_i32 ()
  else
    ()

let print_accessed_memory
    (accessed : Reduced_interval_congruence.RIC.accessed_memory)
  : unit =
  Printf.printf
    "\tfully accessed memory: %s\n\tpartially accessed memory: %s%!\n"
    (Reduced_interval_congruence.RIC.to_string accessed.fully)
    (accessed.partially |> List.map ~f:Reduced_interval_congruence.RIC.to_string |> String.concat ~sep:", ")
let print_store 
    (address : Var.t)
    (vs_address : Reduced_interval_congruence.RIC.t)
    (value : Var.t)
    (vs_value : Value_set_abstraction.t)
    (offset : int32)
    (accessed : Reduced_interval_congruence.RIC.accessed_memory)
  : unit =
  let memory_var = (Variable.Mem (Reduced_interval_congruence.RIC.add_offset vs_address offset)) in
  Printf.printf
    "\tvalue to be stored: %s(%s)\n\taddress: %s(%s)\n\tStoring value-set %s into variable %s%!\n"
    (Var.to_string value)
    (Value_set_abstraction.to_string vs_value)
    (Var.to_string address)
    (Reduced_interval_congruence.RIC.to_string vs_address)
    (Value_set_abstraction.to_string vs_value)
    (Variable.to_string memory_var);
  print_accessed_memory accessed;
  if Reduced_interval_congruence.RIC.((=) Bottom) accessed.fully then
    Printf.printf "\tNo update necessary.%!\n"
  else if Reduced_interval_congruence.RIC.is_singleton accessed.fully then
    Printf.printf "\tperforming STRONG update%!\n"
  else
    Printf.printf "\tperforming WEAK update%!\n"
  
let store 
    (address : Var.t)
    (vs_address : Reduced_interval_congruence.RIC.t)
    (value : Var.t)
    (vs_value : Value_set_abstraction.t)
    (offset : int32)
    (accessed : Reduced_interval_congruence.RIC.accessed_memory)
  : unit =
  if !Value_set_options.print_trace then
    print_store address vs_address value vs_value offset accessed
  else
    ()

let print_instruction 
    (line_number : int)
    (instr : 'a Instr.t)
    (unreachable : bool)
  : unit =
  Printf.printf 
    "%d:\t%s %s%!\n"
    line_number
    (match instr with
    | Data data -> Instr.data_to_string data.instr
    | Control c -> Instr.control_to_short_string c.instr
    | Call c -> Instr.call_to_string c.instr)
    (if unreachable then "(unreachable)" else "")
let instruction
    (line_number : int)
    (instr : 'a Instr.t)
    (unreachable : bool)
  : unit =
  if !Value_set_options.print_trace then
    print_instruction line_number instr unreachable
  else
    ()

let print_invalid_pointer_type (t : string) : unit =
  Printf.printf "\tinvalid pointer type: %s%!\n" t
let invalid_pointer_type (t : string) : unit =
  if !Value_set_options.print_trace then
    print_invalid_pointer_type t
  else
    ()

let print_const
    (cst : int32)
    (var : Variable.t)
  : unit =
  Printf.printf
    "\tassigning constant value %ld to variable %s%!\n"
    cst
    (Variable.to_string var)
let assign_const
    (cst : int32)
    (var : Variable.t)
  : unit =
  if !Value_set_options.print_trace then
    print_const cst var
  else
    ()

let print_assign
    (value : Value_set_abstraction.t)
    (var : Variable.t)
  : unit =
  Printf.printf
    "\tassigning value-set %s to variable %s%!\n"
    (Value_set_abstraction.to_string value)
    (Variable.to_string var)
let assign
    (value : Value_set_abstraction.t)
    (var : Variable.t)
  : unit =
  if !Value_set_options.print_trace then
    print_assign value var
  else
    ()

let print_copy_value
    (value : Var.t)
    (var : Variable.t)
  : unit =
  Printf.printf
    "\ttransferring value-set of %s to variable %s%!\n"
    (Var.to_string value)
    (Variable.to_string var)
let copy_value
    (value : Var.t)
    (var : Variable.t)
  : unit =
  if !Value_set_options.print_trace then
    print_copy_value value var
  else
    ()

let print_select
    (cond : Value_set_abstraction.t)
    (x : Value_set_abstraction.t)
    (y : Value_set_abstraction.t)
    (result : Value_set_abstraction.t)
  : unit =
  Printf.printf
    "\t\tcondition: %s\n\t\tvalue if false: %s\n\t\tvalue if true: %s\n\t\tresult: %s%!\n"
    (Value_set_abstraction.to_string cond)
    (Value_set_abstraction.to_string x)
    (Value_set_abstraction.to_string y)
    (Value_set_abstraction.to_string result)
let select
    (cond : Value_set_abstraction.t)
    (x : Value_set_abstraction.t)
    (y : Value_set_abstraction.t)
    (result : Value_set_abstraction.t)
  : unit =
  if !Value_set_options.print_trace then
    print_select cond x y result
  else
    ()


let print_get
    (global : bool)
    (variable : int32)
    (state : 'a)
    (get : 'a -> Variable.t -> Value_set_abstraction.t)
  : unit =
  let variable = 
    if global then
      Variable.Var (Var.Global (Int32.to_int_exn variable)) 
    else
      Variable.Var (Var.Local (Int32.to_int_exn variable)) in 
  let result = get state variable in
  Printf.printf 
    "\tretrieving variable %s: %s%!\n"
    (Variable.to_string variable)
    (Value_set_abstraction.to_string result)
let get
    ~(global : bool)
    (variable : int32)
    (state : 'a)
    ~(get : 'a -> Variable.t -> Value_set_abstraction.t)
  : unit =
  if !Value_set_options.print_trace then
    print_get global variable state get
  else
    ()

let print_non_i32 : unit -> unit =
  fun () -> Printf.printf "\tnon-i32 constant: it is assumed that this value won't be used as a pointer%!\n"
let non_i32 () : unit =
  if !Value_set_options.print_trace then
    print_non_i32 ()
  else
    ()

let print_imprecise : unit -> unit =
  fun () -> Printf.printf "\tthis type of binary operation results in a pointer that can point anywhere%!\n"
let imprecise_operation () : unit =
  if !Value_set_options.print_trace then
    print_imprecise ()
  else
    ()

let print_comp
    (val1 : Reduced_interval_congruence.RIC.t)
    (val2 : Reduced_interval_congruence.RIC.t)
    (result : Value_set_abstraction.t)
    (operator : string)
  : unit =
  Printf.printf
    "\t%s %s %s -> %s%!\n"
    (val1 |> Reduced_interval_congruence.RIC.to_string)
    operator
    (val2 |> Reduced_interval_congruence.RIC.to_string)
    (result |> Value_set_abstraction.to_string)
let comp
    (val1 : Reduced_interval_congruence.RIC.t)
    (val2 : Reduced_interval_congruence.RIC.t)
    (result : Value_set_abstraction.t)
    (operator : string)
  : unit =
  if !Value_set_options.print_trace then
    print_comp val1 val2 result operator
  else
    ()

let print_return
    (ret : Variable.t)
    (ret_value : Value_set_abstraction.t)
  : unit =
  Printf.printf
    "\t\t%s: %s%!\n" 
    (Variable.to_string ret) 
    (Value_set_abstraction.to_string ret_value)
let return
    (ret : Variable.t)
    (ret_value : Value_set_abstraction.t)
  : unit =
  if !Value_set_options.print_trace then
    print_return ret ret_value
  else
    ()


let print_start_of_function (idx : int) : unit =
  Printf.printf
    "================ START OF FUNCTION ==================== DATA BLOCK #%d%!\n" 
    idx
let start_of_function (idx : int) : unit =
  if !Value_set_options.print_trace then
    print_start_of_function idx
  else
    ()

let print_control_block (cfg : 'a Cfg.t option) (idx : int) : unit =
  Printf.printf
    "======================================================= %sCONTROL BLOCK #%d%!\n"
    (match cfg with
    | Some cfg -> 
      (if IntSet.mem cfg.loop_heads idx then "LOOP HEAD: " else "MERGE: ")
    | None -> "")
    idx
let control_block (cfg : 'a Cfg.t option) (idx : int) : unit =
  if !Value_set_options.print_trace then
    print_control_block cfg idx
  else
    ()


let print_data_block (idx : int) : unit =
  Printf.printf
    "======================================================= DATA BLOCK #%d%!\n"
    idx
let data_block (idx : int) : unit =
  if !Value_set_options.print_trace then
    print_data_block idx
  else
    ()


let print_apply_summary
    (state : 'state)
    (print_state : 'state -> string)
    (f_idx : int32)
    (summary : 'summary)
    (print_summary : 'summary -> string)
  : unit =
  Printf.printf
    "\tstate before the call: %s\n\tsummary of function %ld:%s%!\n"
    (print_state state)
    f_idx
    (print_summary summary)
let apply_summary
    (state : 'state)
    (print_state : 'state -> string)
    (f_idx : int32)
    (summary : 'summary)
    (print_summary : 'summary -> string)
  : unit =
  if !Value_set_options.print_trace then
    print_apply_summary state print_state f_idx summary print_summary
  else
    ()

let print_summary
    (state : 'state)
    (print_state : 'state -> string)
    (summary : 'summary)
    (print_summary_ : 'summary -> string)
  : unit =
  Printf.printf
    "======================================================= SUMMARY\nEND STATE:\t%s\nSUMMARY:%s%!\n"
    (print_state state)
    (print_summary_ summary)
let summary
    (state : 'state)
    (print_state : 'state -> string)
    (summary : 'summary)
    (print_summary_ : 'summary -> string)
  : unit =
  if !Value_set_options.print_trace then
    print_summary state print_state summary print_summary_
  else
    ()
