open Core
open Helpers

module Make (*: Transfer.TRANSFER *) = struct

  module RIC = Reduced_interval_congruence.RIC

  type annot_expected = Spec.t

  (** The state *)
  type state = Abstract_store_domain.t
  [@@deriving sexp, compare, equal]

  (** [value_set_specification] holds the (currently unused) specification for the value-set analysis. *)
  let value_set_specification = ()

  (** [init_state cfg] initializes the abstract state using the function argument and global variables from [cfg]. *)
  let init_state (cfg : 'a Cfg.t) : state =
    Variable.Map.of_alist_exn (
      (List.mapi cfg.arg_types ~f:(fun i _ -> 
        let variable = Var.Local i in
        let var_name = Var.to_string variable in
        (Variable.Var variable, RIC.ric (0, Int 0, Int 0, (var_name, 0))))) @
      (List.mapi cfg.global_types ~f:(fun i _ -> 
        let variable = Var.Global i in
        let var_name = Var.to_string variable in
        (Variable.Var variable, RIC.ric (0, Int 0, Int 0, (var_name, 0))))))

  (** [bottom_state cfg] returns the bottom abstract state. *)
  let bottom_state (_cfg : 'a Cfg.t) : state = Abstract_store_domain.bottom

  (** [state_to_string s] converts an abstract state [s] to its string representation. *)
  let state_to_string (s : state) : string = Abstract_store_domain.to_string s

  (** [join_state s1 s2] joins two abstract states [s1] and [s2]. *)
  let join_state (s1 : state) (s2 : state) : state = Abstract_store_domain.join s1 s2

  (** [join_loop_head s1 s2] joins two abstract states at the head of a loop. *)
  let join_loop_head (s1 : state) (s2 : state) : state = Abstract_store_domain.join_loop_head s1 s2

  (** [widen_state s1 s2] performs widening between abstract states [s1] and [s2]. *)
  let widen_state (s1 : state) (s2 : state) : state = 
    print_endline "\twidening:";
    print_endline ("\t\tstate1: " ^ Abstract_store_domain.to_string s1);
    print_endline ("\t\tstate2: " ^ Abstract_store_domain.to_string s2);
    let widened_state = Abstract_store_domain.widen s1 s2 in
    print_endline ("\t\twidened state: " ^ Abstract_store_domain.to_string widened_state);
    widened_state

  type summary = Value_set_summary.t  (* probably won't be needed *)

  (** [print_all_globals i] prints the first three global variables before instruction [i]. Used for debugging. *)
  let print_all_globals (i : annot_expected Instr.labelled_data) : unit = 
    let g0 = (Variable.Var (get_nth (Spec.get_or_fail i.annotation_before).globals (Int32.of_int_exn 0))) in
    let g1 = (Variable.Var (get_nth (Spec.get_or_fail i.annotation_before).globals (Int32.of_int_exn 1))) in
    let g2 = (Variable.Var (get_nth (Spec.get_or_fail i.annotation_before).globals (Int32.of_int_exn 2))) in
    print_endline ("g0:" ^ Variable.to_string g0);
    print_endline ("g1:" ^ Variable.to_string g1);
    print_endline ("g2:" ^ Variable.to_string g2)

  (** [remove_temporary_variable state var] removes temporary variable [var] from [state] if applicable. *)
  let remove_temporary_variable (state : state) (var : Var.t) : state =
    match var with
    | Var _ | Merge _ -> Variable.Map.remove state (Variable.Var var)
    | _ -> state

  (** [data_instr_transfer m cfg i state] performs the abstract transfer function for a data instruction [i] on [state]. *)
  let data_instr_transfer
      (_module_ : Wasm_module.t)
      (_cfg : annot_expected Cfg.t)
      (i : annot_expected Instr.labelled_data)
      (state : state)
    : state =
    let ret (i : annot_expected Instr.labelled_data) : Variable.t = 
      match List.hd (Spec.get_or_fail i.annotation_after).vstack with
      | Some r -> Variable.Var r
      | None -> failwith "nothing on the stack" in
    match i.instr with
    | Nop | MemorySize | Drop | MemoryGrow -> state
    | MemoryCopy | MemoryFill | MemoryInit _ -> state
    | RefIsNull | RefNull _ | RefFunc _ -> state
    | Select _ -> (* TODO: write this case. resulting variable will point to join of the two being selectd *) state
    | LocalGet l -> 
      let local_variable = Variable.Var (Var.Local (Int32.to_int_exn l)) in
      print_endline ("local.get " ^ (Int32.to_string l) ^ ":\t" ^ Variable.to_string local_variable);
      Abstract_store_domain.copy_value_set state 
        ~from:local_variable
        ~to_:(ret i)
    | LocalSet l ->
      print_endline ("local.set " ^ Int32.to_string l ^ ":");
      let variable = Variable.Var (Var.Local (Int32.to_int_exn l)) in
      let top_of_stack = pop (Spec.get_or_fail i.annotation_before).vstack in
      let new_state =
        begin match top_of_stack with
        | Var.Const (Prim_value.I32 n) ->
          print_endline ("\tadding constant value " ^ Int32.to_string n ^ " to local variable " ^ Variable.to_string variable);
          Abstract_store_domain.assign_constant_value state
          ~const:n
          ~to_:variable
        | Var.Const (Prim_value.F32 n) ->
          print_endline ("\t" ^ Variable.to_string variable ^ " = (Float32)" ^ Wasm.F32.to_string n ^ " -> not a valid pointer value: variable can point anywhere");
          Abstract_store_domain.to_top_RIC state variable
        | Var.Const (Prim_value.I64 n) ->
          print_endline ("\t" ^ Variable.to_string variable ^ " = (Int64)" ^ Int64.to_string n ^ " -> not a valid pointer value: variable can point anywhere");
          Abstract_store_domain.to_top_RIC state variable
        | Var.Const (Prim_value.F64 n) ->
          print_endline ("\t" ^ Variable.to_string variable ^ " = (Float64)" ^ Wasm.F64.to_string n ^ " -> not a valid pointer value: variable can point anywhere");
          Abstract_store_domain.to_top_RIC state variable
        | _ ->
          print_endline ("\ttransfering value-set of " ^ Var.to_string top_of_stack ^ " to local variable " ^ Variable.to_string variable);
          Abstract_store_domain.copy_value_set state 
            ~from:(Variable.Var top_of_stack)
            ~to_:variable
        end
      in
      remove_temporary_variable new_state top_of_stack
    | LocalTee l ->
      print_endline ("local.tee " ^ Int32.to_string l ^ ":");
      let variable = Variable.Var (Var.Local (Int32.to_int_exn l)) in
      Abstract_store_domain.copy_value_set
        (let top_of_stack = pop (Spec.get_or_fail i.annotation_before).vstack in
          begin match top_of_stack with
        | Var.Const (Prim_value.I32 n) ->
          print_endline ("\tadding constant value " ^ Int32.to_string n ^ " to local variable " ^ Variable.to_string variable);
          Abstract_store_domain.assign_constant_value state
          ~const:n
          ~to_:variable
        | Var.Const (Prim_value.F32 n) ->
          print_endline ("\t" ^ Variable.to_string variable ^ " = (Float32)" ^ Wasm.F32.to_string n ^ " -> not a valid pointer value: variable can point anywhere");
          Abstract_store_domain.to_top_RIC state variable
        | Var.Const (Prim_value.I64 n) ->
          print_endline ("\t" ^ Variable.to_string variable ^ " = (Int64)" ^ Int64.to_string n ^ " -> not a valid pointer value: variable can point anywhere");
          Abstract_store_domain.to_top_RIC state variable
        | Var.Const (Prim_value.F64 n) ->
          print_endline ("\t" ^ Variable.to_string variable ^ " = (Float64)" ^ Wasm.F64.to_string n ^ " -> not a valid pointer value: variable can point anywhere");
          Abstract_store_domain.to_top_RIC state variable
        | _ ->
          print_endline ("\ttransfering value-set of " ^ Var.to_string top_of_stack ^ " to local variable " ^ Variable.to_string variable);
          Abstract_store_domain.copy_value_set state 
            ~from:(Variable.Var top_of_stack)
            ~to_:variable
        end)
        ~from:variable
        ~to_:(ret i)
    | GlobalGet g -> 
      let global_variable = Variable.Var (Var.Global (Int32.to_int_exn g)) in
      print_endline ("Global.get " ^ (Int32.to_string g) ^ ":\t" ^ Variable.to_string global_variable);
      Abstract_store_domain.copy_value_set state 
        ~from:global_variable 
        ~to_:(ret i)
    | GlobalSet g ->
      print_endline ("global.set " ^ Int32.to_string g);
      let variable = Variable.Var (Var.Global (Int32.to_int_exn g)) in
      let top_of_stack = pop (Spec.get_or_fail i.annotation_before).vstack in
      let new_state =
        begin match top_of_stack with
        | Var.Const (Prim_value.I32 n) ->
          print_endline ("\tadding constant value " ^ Int32.to_string n ^ " to global variable " ^ Variable.to_string variable);
          Abstract_store_domain.assign_constant_value state
          ~const:n
          ~to_:variable
        | Var.Const (Prim_value.F32 n) ->
          print_endline ("\t" ^ Variable.to_string variable ^ " = (Float32)" ^ Wasm.F32.to_string n ^ " -> not a valid pointer value: variable can point anywhere");
          Abstract_store_domain.to_top_RIC state variable
        | Var.Const (Prim_value.I64 n) ->
          print_endline ("\t" ^ Variable.to_string variable ^ " = (Int64)" ^ Int64.to_string n ^ " -> not a valid pointer value: variable can point anywhere");
          Abstract_store_domain.to_top_RIC state variable
        | Var.Const (Prim_value.F64 n) ->
          print_endline ("\t" ^ Variable.to_string variable ^ " = (Float64)" ^ Wasm.F64.to_string n ^ " -> not a valid pointer value: variable can point anywhere");
          Abstract_store_domain.to_top_RIC state variable
        | _ ->
          print_endline ("\ttransfering value-set of " ^ Var.to_string top_of_stack ^ " to global variable " ^ Variable.to_string variable);
          Abstract_store_domain.copy_value_set state 
            ~from:(Variable.Var top_of_stack)
            ~to_:variable
        end
      in
      remove_temporary_variable new_state top_of_stack
    | Const c -> 
      let variable = (Variable.Var (pop (Spec.get_or_fail i.annotation_after).vstack)) in
      begin match c with
      | Prim_value.I32 _ -> print_endline ("i32.const " ^ Variable.to_string variable); state
      | _ -> print_endline ("Non i32 constant added to the stack: " ^ Variable.to_string variable); state
      end
    | Binary binop -> 
      let x, y = pop2 (Spec.get_or_fail i.annotation_before).vstack in
      let result = pop (Spec.get_or_fail i.annotation_after).vstack in
      let new_state =
        begin match binop with
        | { op = Add; typ = I32 } -> (* i32 addition *) 
          print_endline ("i32.add:\t" ^ Var.to_string x ^ " + " ^ Var.to_string y ^ " -> " ^ Var.to_string result);
          Abstract_store_domain.i32_add state ~x:(Variable.Var x) ~y:(Variable.Var y) ~result:(Variable.Var result)
        | { op = Sub; typ = I32 } -> (* i32 subtraction *) 
          print_endline ("i32.sub:\t" ^ Var.to_string y ^ " - " ^ Var.to_string x ^ " -> " ^ Var.to_string result);
          Abstract_store_domain.i32_sub state ~x:(Variable.Var x) ~y:(Variable.Var y) ~result:(Variable.Var result)
        | { typ = I32; _ } 
        | _ -> (* other operations result in a pointer that can point anywhere *)
          Abstract_store_domain.to_top_RIC state (Variable.Var result)
        end
      in
      remove_temporary_variable (remove_temporary_variable new_state x) y
    | Load load ->
      let address = pop (Spec.get_or_fail i.annotation_before).vstack in
      begin match load with
      | { typ = F32; _ }
      | { typ = I64; _ }
      | { typ = F64; _ } -> Abstract_store_domain.to_top_RIC state (ret i)
      | { typ = I32; offset = offset; _ } ->
        print_endline ("i32.load offset=" ^ string_of_int offset);
        let vs = Abstract_store_domain.get state ~var:(Variable.Var address) in
        let vs_plus_offset = RIC.add_offset vs offset in
        print_endline ("\tloading content at address " ^ RIC.to_string vs_plus_offset ^ " into stack variable " ^ Variable.to_string (ret i));
        let memory_variable = Variable.Mem vs_plus_offset in
        print_endline ("\ttarget memory variable: " ^ Variable.to_string memory_variable);
        let all_mem_vars = Abstract_store_domain.extract_memory_variables state in
        print_endline ("\tall memory variables in the current state: " ^ String.concat ~sep:", " (List.map ~f:Variable.to_string all_mem_vars));
        Abstract_store_domain.set state ~var:(ret i) ~vs:(Abstract_store_domain.get state ~var:memory_variable)
      end
    | Store store ->
      let value, address = pop2 (Spec.get_or_fail i.annotation_before).vstack in
      let vs_address = Abstract_store_domain.get state ~var:(Variable.Var address) in
      let vs_value = Abstract_store_domain.get state ~var:(Variable.Var value) in
      let new_state =
        begin match store with
        | { typ = I32; offset = offset; _ } ->
          print_endline ("i32.store offset=" ^ string_of_int offset);
          print_endline ("\tstoring value-set " ^ RIC.to_string vs_value ^ " into memory variable " ^ Variable.to_string (Variable.Mem (RIC.add_offset vs_address offset)));
          let accessed = RIC.accessed ~value_set:(RIC.add_offset vs_address offset) ~size:4 in
          print_endline ("\tfully accessed memory: " ^ RIC.to_string accessed.fully);
          print_endline ("\tpartially accessed memory: " ^ List.to_string ~f:RIC.to_string accessed.partially);
          let new_state = Abstract_store_domain.update_accessed_vars state accessed in
          if RIC.is_singleton (accessed.fully) then
            (print_endline "\tperforming STRONG update!";
            Abstract_store_domain.set new_state ~var:(Variable.Mem accessed.fully) ~vs:vs_value) (* STRONG update *)
          else
            (print_endline "\tperforming weak update :(";
            Abstract_store_domain.weak_update new_state ~previous_state:state ~var:(Variable.Mem accessed.fully) ~vs:vs_value)
        | { typ = F32; offset = offset; _ } ->
          let accessed = RIC.accessed ~value_set:(RIC.add_offset vs_address offset) ~size:4 in
          Abstract_store_domain.update_accessed_vars state accessed
        | { typ = I64; offset = offset; _ }
        | { typ = F64; offset = offset; _ } -> 
          let accessed = RIC.accessed ~value_set:(RIC.add_offset vs_address offset) ~size:8 in
          Abstract_store_domain.update_accessed_vars state accessed
        end
      in
      remove_temporary_variable (remove_temporary_variable new_state value) address
    | Compare _ -> (* TODO: write this case *) state
    | Unary _ | Test _ | Convert _ -> (* TODO: write this case *) state

  (** [control_instr_transfer m summaries cfg i state] performs the abstract transfer function for a control instruction [i]. *)
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
    | Return -> 
      let top_of_stack = pop (Spec.get_or_fail i.annotation_before).vstack in
      print_endline "return";
      let vs = Abstract_store_domain.get state ~var:(Variable.Var top_of_stack) in
      print_endline ("\treturned value-set: " ^ RIC.to_string vs);
      let new_state = Abstract_store_domain.set state ~var:(Variable.Var Var.Return) ~vs:vs in
      `Simple (remove_temporary_variable new_state top_of_stack)
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

  (** [get_nth_state n states] returns the abstract state of block [n] from [states]. *)
  let rec get_nth_state (n : int) (states : (int * state) list) : state =
    match states with
    | [] -> Abstract_store_domain.bottom
    | (i, state) :: _ when i = n -> state
    | _ :: states -> get_nth_state n states

  (** [get_previous_stack_value_sets cfg block states] collects and joins value-sets from the stack across predecessor blocks. *)
  let get_previous_stack_value_sets 
      (cfg : annot_expected Cfg.t) 
      (block : annot_expected Basic_block.t)  
      (states : (int * state) list)
    : RIC.t list =
    let predecessors = get_predecessors cfg block in
    let previous_stacks = get_previous_stacks cfg predecessors in
    let previous_value_sets = 
      List.map 
        ~f:(fun (idx, stk) ->
          List.map 
            ~f:(fun var ->
              let state = get_nth_state idx states in
              Abstract_store_domain.get state ~var:(Variable.Var var))
            stk)
        previous_stacks in
    match previous_value_sets with
    | [] -> []
    | first_stack :: rest ->
      List.fold ~init:first_stack ~f:(fun acc x -> List.map2_exn ~f:RIC.join acc x) rest 


  (** [merge_flows m cfg block states] merges incoming flows into [block], handling stack merging at control points. *)
  let merge_flows 
      (_module_ : Wasm_module.t) 
      (cfg : annot_expected Cfg.t) 
      (block : annot_expected Basic_block.t)
      (states : (int * state) list) 
    : state =
    match states with
    | [] -> 
      print_endline ("================ START OF FUNCTION ==================== DATA BLOCK #" ^ string_of_int block.idx);
      init_state cfg
    | _ ->
      begin match block.content with
      | Control { instr = Merge; _ } ->
        let states' = List.map ~f:(fun (_, s) -> s) states in
        (* Join all previous states: *)
        let join_state =
          if IntSet.mem cfg.loop_heads block.idx then
            join_loop_head
          else
            join_state
        in
        let new_state_without_merges = List.reduce_exn states' ~f:join_state in 
        let merged_annotation = (Cfg.state_before_block cfg block.idx (Spec_inference.init_state cfg)) in
        let merged_stack =
          match merged_annotation with
          | Spec.Bottom -> []
          | NotBottom annot -> annot.vstack in
        (* Merge stack elements that need to be merged: *)
        print_endline ("======================================================= " ^ (if IntSet.mem cfg.loop_heads block.idx then "LOOP HEAD" else "MERGE") ^ ": Control block #" ^ string_of_int block.idx);
        print_endline ("\tmerged stack: " ^ List.to_string ~f:Var.to_string merged_stack);
        let previous_value_sets = get_previous_stack_value_sets cfg block states in
        print_endline ("\tprevious stack value-sets (joined from all branches): " ^ List.to_string ~f:RIC.to_string previous_value_sets);
        assert (List.length merged_stack = List.length previous_value_sets);
        List.fold2_exn 
          ~init:new_state_without_merges
          ~f:(fun state var vs -> 
            match var with
            | Merge (i, _) when i = block.idx -> Abstract_store_domain.set state ~var:(Variable.Var var) ~vs:vs
            | _ -> state)
          merged_stack
          previous_value_sets
      | Control _ ->
        print_endline ("======================================================= CONTROL BLOCK #" ^ string_of_int block.idx);
        begin match states with
        | (_, s) :: [] -> s
        | _ -> failwith (Printf.sprintf "Invalid block with multiple input states: %d" block.idx)
        end
      | _ -> 
        print_endline ("======================================================= DATA BLOCK #" ^ string_of_int block.idx);
        begin match states with
        | (_, s) :: [] -> s
        | _ -> failwith (Printf.sprintf "Invalid block with multiple input states: %d" block.idx)
        end
      end
    


  (** [summary cfg out_state] computes the value-set summary for a function using its [cfg] and final [out_state]. *)
  let summary (cfg : annot_expected Cfg.t) (out_state : state) : summary =
    let init_spec = (Spec_inference.init_state cfg) in
    match Cfg.state_after_block cfg cfg.exit_block init_spec with
    | Bottom ->
      (* The function exit is likely unreachable, so we use a bottom summary *)
      { ret = None;
        globals = List.init (List.length cfg.global_types) ~f:(fun _ -> RIC.Bottom);
        mem = Abstract_store_domain.bottom; }
    | NotBottom exit_spec ->
      Value_set_summary.make cfg out_state
        (if List.length cfg.return_types = 1 then match (List.hd exit_spec.vstack) with | Some r -> Some (Variable.Var r) | None -> None else None)
        (List.map ~f:(fun v -> Variable.Var v) exit_spec.globals)
        [] (* TODO: complete this section *)
        (* (List.concat_map (Var.OffsetMap.to_alist exit_spec.memory)
           ~f:(fun ((a, _), b) -> [a; b])) *)

  (** [extract_summary cfg analyzed_cfg] extracts a value-set summary from the final result of the analysis. *)
  let extract_summary (cfg : annot_expected Cfg.t) (analyzed_cfg : state Cfg.t) : summary =
    let out_state = Cfg.state_after_block analyzed_cfg cfg.exit_block (init_state cfg) in
    summary cfg out_state
  

end