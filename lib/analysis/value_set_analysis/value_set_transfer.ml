open Core
open Helpers

module Make (*: Transfer.TRANSFER *) = struct

  module RIC = Reduced_interval_congruence.RIC

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
        (Variable.Var variable, RIC.ric (0, Int 0, Int 0, (var_name, 0))))) @
      (List.mapi cfg.global_types ~f:(fun i _ -> 
        let variable = Var.Global i in
        let var_name = Var.to_string variable in
        (Variable.Var variable, RIC.ric (0, Int 0, Int 0, (var_name, 0))))))

  let bottom_state (_cfg : 'a Cfg.t) : state = Abstract_store_domain.bottom

  let state_to_string (s : state) : string = Abstract_store_domain.to_string s

  let join_state (s1 : state) (s2 : state) : state = Abstract_store_domain.join s1 s2

  let widen_state (s1 : state) (s2 : state) : state = Abstract_store_domain.widen s1 s2

  type summary = Value_set_summary.t  (* probably won't be needed *)

  let print_all_globals (i : annot_expected Instr.labelled_data) : unit = 
    let g0 = (Variable.Var (get_nth (Spec.get_or_fail i.annotation_before).globals (Int32.of_int_exn 0))) in
    let g1 = (Variable.Var (get_nth (Spec.get_or_fail i.annotation_before).globals (Int32.of_int_exn 1))) in
    let g2 = (Variable.Var (get_nth (Spec.get_or_fail i.annotation_before).globals (Int32.of_int_exn 2))) in
    print_endline ("g0:" ^ Variable.to_string g0);
    print_endline ("g1:" ^ Variable.to_string g1);
    print_endline ("g2:" ^ Variable.to_string g2)


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
      let local_variable = Variable.Var (Var.Local (Int32.to_int_exn l)) in
      print_endline ("Local.get " ^ (Int32.to_string l)); print_endline ("\t" ^ Variable.to_string local_variable);
      Abstract_store_domain.copy_value_set state 
        ~from:local_variable
        ~to_:(ret i)
    | LocalSet l ->
      print_endline ("local.set " ^ Int32.to_string l);
      let variable = Variable.Var (Var.Local (Int32.to_int_exn l)) in
      let top_of_stack = pop (Spec.get_or_fail i.annotation_before).vstack in
      begin match top_of_stack with
      | Var.Const (Prim_value.I32 n) ->
        print_endline ("\tadding constant value " ^ Int32.to_string n ^ " to local variable " ^ Variable.to_string variable);
        Abstract_store_domain.assign_constant_value state
        ~const:n
        ~to_:variable
      | Var.Const (Prim_value.F32 _) ->
        Abstract_store_domain.to_top_RIC state variable
      | Var.Const _ ->
        Abstract_store_domain.to_bottom_RIC state variable
      | _ ->
        print_endline ("\ttransfering value-set of " ^ Var.to_string top_of_stack ^ " to local variable " ^ Variable.to_string variable);
        Abstract_store_domain.copy_value_set state 
          ~from:(Variable.Var top_of_stack)
          ~to_:variable
      end
    | LocalTee l ->
      let variable = Variable.Var (Var.Local (Int32.to_int_exn l)) in
      Abstract_store_domain.copy_value_set
        (let top_of_stack = pop (Spec.get_or_fail i.annotation_before).vstack in
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
        ~from:variable
        ~to_:(ret i)
    | GlobalGet g -> 
      let global_variable = Variable.Var (Var.Global (Int32.to_int_exn g)) in
      print_endline ("Global.get " ^ (Int32.to_string g)); print_endline ("\t" ^ Variable.to_string global_variable);
      Abstract_store_domain.copy_value_set state 
        ~from:global_variable 
        ~to_:(ret i)
    | GlobalSet g ->
      print_endline ("global.set " ^ Int32.to_string g);
      let variable = Variable.Var (Var.Global (Int32.to_int_exn g)) in
      let top_of_stack = pop (Spec.get_or_fail i.annotation_before).vstack in
      begin match top_of_stack with
      | Var.Const (Prim_value.I32 n) ->
        print_endline ("\tadding constant value " ^ Int32.to_string n ^ " to global variable " ^ Variable.to_string variable);
        Abstract_store_domain.assign_constant_value state
        ~const:n
        ~to_:variable
      | Var.Const (Prim_value.F32 _) ->
        Abstract_store_domain.to_top_RIC state variable
      | Var.Const _ ->
        Abstract_store_domain.to_bottom_RIC state variable
      | _ ->
        print_endline ("\ttransfering value-set of " ^ Var.to_string top_of_stack ^ " to global variable " ^ Variable.to_string variable);
        Abstract_store_domain.copy_value_set state 
          ~from:(Variable.Var top_of_stack)
          ~to_:variable
      end
    | Const c -> 
      print_endline "constant added to the stack: ";
      let variable = (Variable.Var (pop (Spec.get_or_fail i.annotation_after).vstack)) in
      print_endline ("\t" ^ Variable.to_string variable);
      begin match c with
      | Prim_value.I32 n -> print_endline ("\ti32 detected: " ^ Int32.to_string n); state
      | _ -> state
      end
    | Binary binop -> 
      let x, y = pop2 (Spec.get_or_fail i.annotation_before).vstack in
      let result = pop (Spec.get_or_fail i.annotation_after).vstack in
      begin match binop with
      | { op = Add; typ = I32 } -> (* i32 addition *) 
        print_endline ("i32.add " ^ Var.to_string x ^ " + " ^ Var.to_string y ^ " -> " ^ Var.to_string result);
        Abstract_store_domain.i32_add state ~x:(Variable.Var x) ~y:(Variable.Var y) ~result:(Variable.Var result)
      | { op = Sub; typ = I32 } -> (* i32 subtraction *) 
        print_endline ("i32.sub " ^ Var.to_string y ^ " - " ^ Var.to_string x ^ " -> " ^ Var.to_string result);
        Abstract_store_domain.i32_sub state ~x:(Variable.Var x) ~y:(Variable.Var y) ~result:(Variable.Var result)
      | { typ = I32; _ } 
      | { typ = F32; _ } -> (* other 32 bit operations result in a pointer that can point anywhere *)
        Abstract_store_domain.to_top_RIC state (Variable.Var result)
      | _ -> state (* i64 or f64 operations do not concern pointers *)
        (* Abstract_store_domain.to_bottom_RIC state (Variable.Var result) why include them? *)
      end
    | Load load ->
      let address = pop (Spec.get_or_fail i.annotation_before).vstack in
      let result = pop (Spec.get_or_fail i.annotation_after).vstack in
      begin match load with
      | { typ = F32; _ } -> Abstract_store_domain.to_top_RIC state (Variable.Var result) (* TODO: is this even useful ?*)
      | { typ = I64; _ }
      | { typ = F64; _ } -> state
      | { typ = I32; offset = offset; _ } ->
        (* TODO: turn this into a function in Abstract_store_domain *)
        print_endline ("i32.load offset=" ^ string_of_int offset);
        let vs =
          begin match address with
          | Const (Prim_value.I32 addr) ->
            RIC.ric (0, Int 0, Int 0, ("", Int32.to_int_exn addr))
          | v ->
            Abstract_store_domain.get state ~var:(Variable.Var v)
          end
        in
        let vs_plus_offset = RIC.add_offset vs offset in
        print_endline ("\tloading content at address " ^ RIC.to_string vs_plus_offset ^ " into stack variable: " ^ Var.to_string result);
        let memory_variable = Variable.Mem vs_plus_offset in
        print_endline ("\tmemory variable: " ^ Variable.to_string memory_variable);
        let all_mem_vars = Abstract_store_domain.extract_memory_variables state in
        print_endline ("\tall memory variable in the current state: " ^ List.to_string ~f:Variable.to_string all_mem_vars);
        let fully_accessed = 
          List.filter ~f:(fun x -> 
                        match x with 
                        | Variable.Mem r -> 
                          (not (RIC.comparable_offsets r vs_plus_offset))
                          || not (RIC.disjoint r vs_plus_offset)
                        | _ -> assert false)
                      all_mem_vars 
        in
        print_endline ("\tfully accessed memory variables: " ^ List.to_string ~f:Variable.to_string fully_accessed);
        let vs = 
          if Variable.is_covered ~by:fully_accessed memory_variable then
            List.fold ~init:RIC.Bottom
                      ~f:(fun acc x -> 
                        match x with
                        | Variable.Mem _ -> RIC.join acc (Abstract_store_domain.get state ~var:x)
                        | _ -> assert false) 
                      fully_accessed             
          else 
            RIC.Top (* if some of memory_variable's addresses have yet to be assigned a value,
                       there is a level of uncertainty that forces us to assign TOP to this variable *)
        in
        Abstract_store_domain.set state ~var:(Variable.Var result) ~vs:vs
      end
    | Store store ->
      let address, value = pop2 (Spec.get_or_fail i.annotation_before).vstack in
      let vs1 = 
        begin match address with
        | Const (Prim_value.I32 addr) -> RIC.ric (0, Int 0, Int 0, ("", Int32.to_int_exn addr))
        | _ -> Abstract_store_domain.get state ~var:(Variable.Var address)
        end
      in
      let vs2 = 
        begin match value with
        | Const (Prim_value.I32 addr) -> RIC.ric (0, Int 0, Int 0, ("", Int32.to_int_exn addr))
        | Var _ -> Abstract_store_domain.get state ~var:(Variable.Var value)
        | _ -> assert false
        end
      in
      begin match store with
      | { typ = I32; offset = offset; _ } ->
        print_endline ("i32.store offset=" ^ string_of_int offset);
        print_endline ("\tstoring value-set " ^ RIC.to_string vs2 ^ " into memory variable " ^ Variable.to_string (Variable.Mem (RIC.add_offset vs1 offset)));
        let accessed = RIC.accessed ~value_set:(RIC.add_offset vs1 offset) ~size:4 in
        print_endline ("\tfully accessed memory: " ^ RIC.to_string accessed.fully);
        print_endline ("\tpartially accessed memory: " ^ List.to_string ~f:RIC.to_string accessed.partially);
        let new_state = Abstract_store_domain.update_accessed_vars state accessed in
        if RIC.is_singleton (accessed.fully) then
          (print_endline "\tperforming STRONG update!";
          Abstract_store_domain.set new_state ~var:(Variable.Mem accessed.fully) ~vs:vs2) (* STRONG update *)
        else
          (print_endline "\tperforming weak update :(";
          Abstract_store_domain.weak_update new_state ~previous_state:state ~var:(Variable.Mem accessed.fully) ~vs:vs2)
      | { typ = F32; offset = offset; _ } ->
        let accessed = RIC.accessed ~value_set:(RIC.add_offset vs1 offset) ~size:4 in
        Abstract_store_domain.update_accessed_vars state accessed
      | { typ = I64; offset = offset; _ }
      | { typ = F64; offset = offset; _ } -> 
        let accessed = RIC.accessed ~value_set:(RIC.add_offset vs1 offset) ~size:8 in
        Abstract_store_domain.update_accessed_vars state accessed
      end
    | Compare _ -> (* TODO: write this case *) state
    | Unary _ | Test _ | Convert _ -> (* TODO: write this case *) state

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



  let rec get_nth_state (n : int) (states : (int * state) list) : state =
    match states with
    | [] -> Abstract_store_domain.bottom
    | (i, state) :: _ when i = n -> state
    | _ :: states -> get_nth_state n states


  let merge_flows 
      (_module_ : Wasm_module.t) 
      (cfg : annot_expected Cfg.t) 
      (block : annot_expected Basic_block.t)
      (states : (int * state) list) 
    : state =
    (* let init_spec = (Spec_inference.init_state cfg) in *)
    let block_idx = block.idx in
    match states with
    | [] -> init_state cfg
    | _ ->
      begin match block.content with
      | Control { instr = Merge; _ } ->
        (* let spec = Cfg.state_after_block cfg block.idx init_spec in *)
        let states' = List.map ~f:(fun (_, s) -> s) states in
        let new_state_without_merges = List.reduce_exn states' ~f:join_state in (* Join all previous states *)
        (* Merge stack elements that need to be merged *)
        let merge_variables = Var.Set.to_list (Var.Set.of_list (List.map ~f:snd (Spec_inference.new_merge_variables cfg block))) in
        let nb_of_merge_variables = List.length merge_variables in
        print_endline ("MERGE: Control block #" ^ string_of_int block_idx);
        print_endline ("\tMerge variables: " ^ Var.list_to_string merge_variables);
        let predecessors = Cfg.predecessors cfg block.idx in
        let predecessors = 
          List.filter ~f:(fun blk -> List.mem predecessors blk.idx ~equal:Int.equal) 
                      (Cfg.all_predecessors cfg block) in
        let previous_annotations = 
          List.fold ~init:[] 
                    ~f:(fun acc blk ->
                      (blk.idx, Cfg.state_after_block cfg blk.idx (Spec_inference.init_state cfg)) :: acc)
                    predecessors in
        let previous_stacks =
          List.map ~f:(fun x ->
            match x with
            | idx, Spec.Bottom -> idx, []
            | idx, NotBottom x -> idx, List.take (x.vstack) nb_of_merge_variables
          )
          previous_annotations in
        let previous_value_sets = 
          List.map 
            ~f:(fun (idx, stk) ->
              List.map 
                ~f:(fun var ->
                  let state = get_nth_state idx states in
                  Abstract_store_domain.get state ~var:(Variable.Var var))
                stk)
            previous_stacks in
        let previous_value_sets = 
          begin match previous_value_sets with
          | [] -> []
          | first_stack :: rest ->
            List.fold ~init:first_stack ~f:(fun acc x -> List.map2_exn ~f:RIC.join acc x) rest 
          end
        in
        (* print_endline ("previous value-sets: " ^ List.to_string ~f:RIC.to_string previous_value_sets); *)
        let merge_variables = List.take merge_variables (List.length previous_value_sets) in
        assert (List.length previous_value_sets = List.length merge_variables);
        List.fold2_exn 
          ~init:new_state_without_merges
          ~f:(fun state var vs -> Abstract_store_domain.set state ~var:(Variable.Var var) ~vs:vs)
          merge_variables
          previous_value_sets
      | _ -> 
        begin match states with
        | (_, s) :: [] -> s
        | _ -> failwith (Printf.sprintf "Invalid block with multiple input states: %d" block.idx)
        end
      end
    (* TODO: Write the rest of this function *)
    


  (** [summary cfg out_state] computes a taint summary for a function based on its final state and annotations at the function exit. *)
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

  (** [extract_summary cfg analyzed_cfg] extracts the taint summary from an analyzed control-flow graph. *)
  let extract_summary (cfg : annot_expected Cfg.t) (analyzed_cfg : state Cfg.t) : summary =
    let out_state = Cfg.state_after_block analyzed_cfg cfg.exit_block (init_state cfg) in
    summary cfg out_state
  

end