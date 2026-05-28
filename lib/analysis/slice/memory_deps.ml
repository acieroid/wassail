open Core
open Helpers

type t = Instr.Label.Set.t Instr.Label.Map.t (* Map from instruction to its memory dependencies *)


let find_store_address_and_size 
    (cfg : Spec_domain.t Instr.t Instr.Label.Map.t) 
    (label : Instr.Label.t) 
  : Var.t * int32 =
  let instruction = label |> Instr.Label.Map.find_exn cfg in
  let store_size =
    match instruction with
    | Data { instr = Store {typ = I32; _}; _ }
    | Data { instr = Store {typ = F32; _}; _ } -> 4l
    | Data { instr = Store {typ = I64; _}; _ }
    | Data { instr = Store {typ = F64; _}; _ } -> 8l
    | _ -> assert false in
  let annotation_before = instruction |> Instr.annotation_before in
  let (_, address) = pop2 (Spec_domain.get_or_fail annotation_before).vstack in
  address, store_size

let find_load_address_and_size 
    (cfg : Spec_domain.t Instr.t Instr.Label.Map.t) 
    (label : Instr.Label.t) 
  : Var.t * int32 =
  let instruction = label |> Instr.Label.Map.find_exn cfg in
  let load_size =
    match instruction with
    | Data { instr = Load {typ = I32; _}; _ }
    | Data { instr = Load {typ = F32; _}; _ } -> 4l
    | Data { instr = Load {typ = I64; _}; _ }
    | Data { instr = Load {typ = F64; _}; _ } -> 8l
    | _ -> assert false in
  let annotation_before = instruction |> Instr.annotation_before in
  pop (Spec_domain.get_or_fail annotation_before).vstack, load_size

let load_depends_on_store 
    (pointer_analysis : Value_set.pointer_analysis option)
    ~(load_label : Instr.Label.t)
    ~(store_label : Instr.Label.t)
    ~(store_offset : int)
    ~(load_offset : int)
  : Instr.Label.t option =
  match pointer_analysis with
  | None -> Some store_label
  | Some (cfg_pointers, cfg_spec, _) ->
    let store_address, store_size = find_store_address_and_size cfg_spec store_label in
    let store_address_value_set = 
      Abstract_store_domain.find_value_set cfg_pointers ~label:store_label ~var:store_address 
      |> (Value_set_abstractions.plus (ValueSet (Reduced_interval_congruence.RIC.ric (0l, Int 0l, Int 0l, ("", Int32.of_int_exn store_offset)))))
    in
    let load_address, load_size = find_load_address_and_size cfg_spec load_label in
    let load_address_value_set = 
      Abstract_store_domain.find_value_set cfg_pointers ~label:load_label ~var:load_address 
      |> (Value_set_abstractions.plus (ValueSet (Reduced_interval_congruence.RIC.ric (0l, Int 0l, Int 0l, ("", Int32.of_int_exn load_offset)))))
    in

    let overlap = Value_set_abstractions.may_overlap ~store_size ~load_size ~store_vs:store_address_value_set ~load_vs:load_address_value_set in
    if overlap then Some store_label else None
  

let rec extract_nth_argument
    (stack : Var.t list)
    (arity : int)
    (n : int)
  : Var.t =
  match stack, arity, n with
  | _, a, n when n >= a -> 
    failwith "function arity doesn't match argument index"
  | s, a, n when n = a - 1 ->
    pop s
  | _ :: s, a, n -> extract_nth_argument s (a-1) n
  | _ -> assert false


let tokenize (s : string) : string list =
  let is_digit c = Char.(c >= '0' && c <= '9') in
  let is_letter c = Char.(c >= 'a' && c <= 'z') in
  let len = String.length s in
  let rec aux i acc =
    if i >= len then List.rev acc
    else
      let c = s.[i] in
      if is_letter c then
        aux (i + 1) ((String.make 1 c) :: acc)
      else if Char.equal c '+' || Char.equal c '-' then
        aux (i + 1) ((String.make 1 c) :: acc)
      else if is_digit c then
        let j = ref i in
        while !j < len && is_digit s.[!j] do
          incr j
        done;
        let number = String.sub s ~pos:i ~len:(!j - i) in
        aux !j (number :: acc)
      else
        failwith "Unexpected character"
  in
  aux 0 []


let rec find_parameter_value_set 
    ?(is_call_indirect : bool = false)
    (pointer_analysis : Value_set.pointer_analysis option)
    (label : Instr.Label.t) 
    (vars : string list)
  : Value_set_abstractions.t =
  match pointer_analysis with
  | None -> assert false
  | Some (cfg_pointers, spec, _) ->
    let block = Cfg.find_enclosing_block_exn cfg_pointers label in
    let call_instr =
      match block.content with
      | Call call_instr ->  call_instr
      | _ -> failwith "label does not belong to a call instruction"
    in
    let store = call_instr.annotation_before in
    match vars with
    | [] -> Value_set_abstractions.ValueSet Reduced_interval_congruence.RIC.Bottom
    | "l" :: idx :: "+" :: rest
    | "l" :: idx :: rest ->
      (let fct_arity =
        match call_instr.instr with
        | CallDirect ((x,_), _, _)
        | CallIndirect (_, (x,_), _, _) -> x
      in
      let stack_before_call =
        (label |> Instr.Label.Map.find_exn spec 
               |> Instr.annotation_before
               |> Spec_domain.get_or_fail).vstack
        |> if is_call_indirect then List.tl_exn else fun x -> x
      in
      let var = Variable.Var (extract_nth_argument stack_before_call fct_arity (int_of_string idx)) in
      let vs = Abstract_store_domain.get store ~var in
      Value_set_abstractions.plus (find_parameter_value_set pointer_analysis label rest) vs) 
    | "g" :: idx :: "+" :: rest
    | "g" :: idx :: rest ->
      let vs = Abstract_store_domain.get store ~var:(Variable.Var (Var.Global (int_of_string idx))) in
      Value_set_abstractions.plus (find_parameter_value_set pointer_analysis label rest) vs
    | "n" :: "e" :: "g" :: lg :: idx :: rest ->
      Value_set_abstractions.minus 
        (find_parameter_value_set pointer_analysis label rest)
        (find_parameter_value_set pointer_analysis label [lg; idx])
    | lst ->
      (print_endline (String.concat ~sep:";" lst); failwith "not yet implemented")
    

let call_depends_on_store
    ?(is_call_indirect : bool = false)
    ~(pointer_analysis : Value_set.pointer_analysis option)
    ~(call_label : Instr.Label.t)
    ~(store_label : Instr.Label.t)
    ~(fct_index : int32)
    ~(store_offset : int)
    (() : unit)
  : Instr.Label.t option =
  match pointer_analysis with
  | Some (cfg_pointers, spec, summaries) ->
    let fct_summary = Int32Map.find_exn summaries fct_index in
    let accessed_memory = Abstract_store_domain.get fct_summary ~var:(Variable.Accessed) in
    let accessed_memory =
      match accessed_memory with
      | ValueSet RIC {stride; lower_bound; upper_bound; offset=("", o)} -> 
        Value_set_abstractions.ValueSet (Reduced_interval_congruence.RIC.ric (stride, lower_bound, upper_bound, ("", o)))
      | ValueSet RIC {stride; lower_bound; upper_bound; offset=(relative_offset, o)} ->
        find_parameter_value_set ~is_call_indirect pointer_analysis call_label (tokenize relative_offset) 
        |>  (Value_set_abstractions.i32_add
              (Value_set_abstractions.ValueSet 
                (Reduced_interval_congruence.RIC.ric (stride, lower_bound, upper_bound, ("", o)))))
      | ValueSet Bottom -> ValueSet Bottom
      | _ -> accessed_memory
    in
    let store_address, store_size = find_store_address_and_size spec store_label in
    let store_address_value_set = 
      Abstract_store_domain.find_value_set 
        cfg_pointers ~label:store_label ~var:store_address 
      |> (Value_set_abstractions.plus (ValueSet (Reduced_interval_congruence.RIC.ric (0l, Int 0l, Int 0l, ("", Int32.of_int_exn store_offset)))))
    in
    let overlap = Value_set_abstractions.may_overlap ~store_size ~load_size:1l ~store_vs:store_address_value_set ~load_vs:accessed_memory in
    if overlap then Some store_label else None
  | None -> Some store_label



let load_depends_on_call
    ~(pointer_analysis : Value_set.pointer_analysis option)
    ~(load_label : Instr.Label.t)
    ~(load_offset : int)
    ~(call_label : Instr.Label.t)
    ~(fct_index : int32)
  : Instr.Label.Set.t =
  match pointer_analysis with
  | None -> Instr.Label.Set.singleton call_label
  | Some (cfg_pointers, spec, summaries) ->
    let load_address, load_size = find_load_address_and_size spec load_label in
    let load_address_value_set =
      Abstract_store_domain.find_value_set cfg_pointers ~label:load_label ~var:load_address 
      |> (Value_set_abstractions.plus (ValueSet (Reduced_interval_congruence.RIC.ric (0l, Int 0l, Int 0l, ("", Int32.of_int_exn load_offset)))))
    in
    let function_summary = Int32Map.find_exn summaries fct_index in
    let affected_memory_addresses = function_summary.store_operations in
    let overlap =
      List.fold 
        (Reduced_interval_congruence.RICSet.to_list affected_memory_addresses)
        ~init:false
        ~f:(fun acc address ->
          acc || 
          let store_vs =
            match address with
            | RIC { offset = ("", _); _ } -> Value_set_abstractions.ValueSet address
            | RIC { stride; lower_bound; upper_bound; offset=(relative_offset, o)} ->
              find_parameter_value_set pointer_analysis call_label (tokenize relative_offset)
              |> (Value_set_abstractions.i32_add
                   (Value_set_abstractions.ValueSet 
                     (Reduced_interval_congruence.RIC.ric (stride, lower_bound, upper_bound, ("", o)))))
            | _ -> ValueSet address
          in
          Value_set_abstractions.may_overlap ~store_size:1l ~load_size ~store_vs ~load_vs:load_address_value_set) in
    if overlap then Instr.Label.Set.singleton call_label else Instr.Label.Set.empty


let functions_potentially_called 
    ~(module_ : Wasm_module.t)
    ~(indirect_label : Instr.Label.t)
    ~(type_index : int32)
    ~(cfg_pointers : Abstract_store_domain.t Cfg.t)
    ~(cfg_spec : (Instr.Label.t, Spec_domain.t Instr.t, 'a) Map_intf.Map.t)
  : int32 list =
  let call_indirect_index =
      pop (indirect_label 
            |> Instr.Label.Map.find_exn cfg_spec 
            |> Instr.annotation_before
            |> Spec_domain.get_or_fail).vstack in
    let call_indirect_index_value =
      Abstract_store_domain.find_value_set cfg_pointers ~label:indirect_label ~var:call_indirect_index in
    Call_graph.indirect_call_targets module_ type_index
      |> List.filter ~f:(fun idx ->
        Value_set_abstractions.meet
          call_indirect_index_value
          (ValueSet (Reduced_interval_congruence.RIC.of_int32 idx))
        |> Value_set_abstractions.equal Value_set_abstractions.bottom
        |> not)

let load_depends_on_call_indirect
    ~(module_ : Wasm_module.t)
    ~(pointer_analysis : Value_set.pointer_analysis option)
    ~(load_label : Instr.Label.t)
    ~(load_offset : int)
    ~(call_indirect_label : Instr.Label.t)
    ~(type_index : int32)
  : Instr.Label.Set.t =
  match pointer_analysis with
  | None -> Instr.Label.Set.singleton call_indirect_label
  | Some (cfg_pointers, cfg_spec, _summaries) ->
    functions_potentially_called
      ~module_
      ~indirect_label:call_indirect_label
      ~type_index
      ~cfg_pointers
      ~cfg_spec
    |> List.fold ~init:Instr.Label.Set.empty
      ~f:(fun acc fct_index ->
        Instr.Label.Set.union acc
        (load_depends_on_call
          ~pointer_analysis
          ~load_label
          ~load_offset
          ~call_label:call_indirect_label
          ~fct_index))




let globals_modified (summary : Abstract_store_domain.t) : String.Set.t =
  summary 
    |> Abstract_store_domain.extract_global_values
    |> Map.filteri
      ~f:(fun ~key ~data ->
        not (Reduced_interval_congruence.RIC.equal data (Reduced_interval_congruence.RIC.relative_ric key))) 
    |> Map.to_alist 
    |> List.fold ~init:[] ~f:(fun acc (g,_) -> g :: acc)
    |> String.Set.of_list


let call_depends_on_call
    ~(global_deps : Global_read_domain.t Int32Map.t)
    ~(pointer_analysis : Value_set.pointer_analysis option)
    ~(call_label : Instr.Label.t)
    ~(depends_on_this_call : Instr.Label.t)
    ~(fct_1_index : int32)
    ~(fct_2_index : int32)
  : Instr.Label.Set.t =
  match pointer_analysis with
  | None -> Instr.Label.Set.singleton depends_on_this_call
  | Some (_, _, summaries) ->
    let fct_1_summary = Int32Map.find_exn summaries fct_1_index in
    let fct_2_summary = Int32Map.find_exn summaries fct_2_index in
    let addresses_read_by_fct1 : Value_set_abstractions.t = 
      let tmp_address = Abstract_store_domain.get fct_1_summary ~var:Variable.Accessed in
      match tmp_address with
      | Value_set_abstractions.ValueSet RIC { offset = ("", _); _ } -> tmp_address
      | ValueSet RIC {stride; lower_bound; upper_bound; offset=(relative_offset, o)} ->
        find_parameter_value_set pointer_analysis call_label (tokenize relative_offset)
        |> (Value_set_abstractions.i32_add
              (Value_set_abstractions.ValueSet 
                (Reduced_interval_congruence.RIC.ric (stride, lower_bound, upper_bound, ("", o)))))
      | _ -> tmp_address
    in
    let addresses_affected_by_fct2 = 
      fct_2_summary.store_operations 
      |> Reduced_interval_congruence.RICSet.to_list
      |> List.map ~f:(fun address ->
        match address with
        | RIC {offset = ("", _); _} -> address
        | RIC {stride; lower_bound; upper_bound; offset=(relative_offset, o)} ->
          let tmp_vs =
            find_parameter_value_set pointer_analysis depends_on_this_call (tokenize relative_offset)
            |> (Value_set_abstractions.i32_add
                (Value_set_abstractions.ValueSet
                  (Reduced_interval_congruence.RIC.ric (stride, lower_bound, upper_bound, ("", o)))))
          in
          (match tmp_vs with
          | ValueSet tmp_vs -> tmp_vs
          | Boolean { numeric_value; _ } -> numeric_value
          | Bitfield bf -> Reduced_interval_congruence.RIC.of_bitfield bf)
        | _ -> address)
      |> Reduced_interval_congruence.RICSet.of_list
    in
    let memory_overlap =
      addresses_affected_by_fct2
        |> Reduced_interval_congruence.RICSet.to_list
        |> List.fold ~init:false
          ~f:(fun acc addr ->
            (* TODO: refine load_size. We now approximate for the worst case (i64.load) *)
            acc || Value_set_abstractions.may_overlap ~store_size:1l ~load_size:8l ~store_vs:(ValueSet addr) ~load_vs:addresses_read_by_fct1)
    in
    if memory_overlap then
      Instr.Label.Set.singleton depends_on_this_call
    else
      let globals_modified_by_fct2 = globals_modified fct_2_summary in
      if Set.is_empty globals_modified_by_fct2 then
        Instr.Label.Set.empty
      else
        match Int32Map.find_exn global_deps fct_1_index with
        | Top -> Instr.Label.Set.singleton depends_on_this_call
        | NotTop globals_read_by_fct1 ->
          if Global_read_domain.to_variable_names globals_read_by_fct1
              |> Set.inter globals_modified_by_fct2 
              |> Set.is_empty 
          then
            Instr.Label.Set.empty
          else
            Instr.Label.Set.singleton depends_on_this_call

    
let call_indirect_depends_on_call
    ~(module_ : Wasm_module.t)
    ~(global_deps : Global_read_domain.t Int32Map.t)
    ~(pointer_analysis : Value_set.pointer_analysis option)
    ~(depends_on_this_call : Instr.Label.t)
    ~(call_idx_number : int32)
    ~(indirect_label : Instr.Label.t)
    ~(type_index : int32)
  : Instr.Label.Set.t =
  match pointer_analysis with
  | None -> Instr.Label.Set.singleton depends_on_this_call
  | Some (cfg_pointers, cfg_spec, _) ->
    let targets = functions_potentially_called
      ~module_
      ~indirect_label
      ~type_index
      ~cfg_pointers
      ~cfg_spec
    in
    (* if List.is_empty targets then (Log.error "indirect call index doesn't match any function"; failwith "invalid program: indirect call index doesn't match any function"); *)
    List.fold_left targets
      ~init:Instr.Label.Set.empty
      ~f:(fun acc idx -> Instr.Label.Set.union acc 
        (call_depends_on_call
          ~global_deps
          ~pointer_analysis
          ~call_label:indirect_label
          ~depends_on_this_call
          ~fct_1_index:idx
          ~fct_2_index:call_idx_number
        )
      )
                


let call_depends_on_call_indirect
    ~(module_ : Wasm_module.t)
    ~(global_deps : Global_read_domain.t Int32Map.t)
    ~(pointer_analysis : Value_set.pointer_analysis option)
    ~(type_index : int32)
    ~(call_label : Instr.Label.t)
    ~(indirect_label : Instr.Label.t)
    ~(call_index : int32)
  : Instr.Label.Set.t =
  match pointer_analysis with
  | None -> Instr.Label.Set.singleton indirect_label
  | Some (cfg_pointers, cfg_spec, _) ->
    let targets = functions_potentially_called
      ~module_
      ~indirect_label
      ~type_index
      ~cfg_pointers
      ~cfg_spec
    in
    List.fold_left targets
      ~init:Instr.Label.Set.empty
      ~f:(fun acc fct_index ->
        Instr.Label.Set.union acc
        (call_depends_on_call
          ~global_deps
          ~pointer_analysis
          ~call_label
          ~depends_on_this_call:indirect_label
          ~fct_1_index:call_index
          ~fct_2_index:fct_index
        )
      )


let call_indirect_depends_on_call_indirect
    ~(module_ : Wasm_module.t)
    ~(global_deps : Global_read_domain.t Int32Map.t)
    ~(pointer_analysis : Value_set.pointer_analysis option)
    ~(dependency_label : Instr.Label.t)
    ~(dependency_type_index : int32)
    ~(dependent_label : Instr.Label.t)
    ~(dependent_type_index : int32)
  : Instr.Label.Set.t =
  match pointer_analysis with
  | None -> Instr.Label.Set.singleton dependency_label
  | Some (cfg_pointers, cfg_spec, _) ->
    let dependency_targets = functions_potentially_called
      ~module_
      ~indirect_label:dependency_label
      ~type_index:dependency_type_index
      ~cfg_pointers
      ~cfg_spec
    and dependent_targets = functions_potentially_called
      ~module_
      ~indirect_label:dependent_label
      ~type_index:dependent_type_index
      ~cfg_pointers
      ~cfg_spec
    in
    dependency_targets
    |> List.fold ~init:Instr.Label.Set.empty
      ~f:(fun acc dependency_index ->
        dependent_targets
        |> List.fold ~init:acc
          ~f:(fun acc dependent_index ->
            Instr.Label.Set.union acc
              (call_depends_on_call
                ~global_deps
                ~pointer_analysis
                ~call_label:dependent_label
                ~depends_on_this_call:dependency_label
                ~fct_1_index:dependent_index
                ~fct_2_index:dependency_index)
          )
      )


let call_indirect_depends_on_store
    ~(module_ : Wasm_module.t)
    ~(pointer_analysis : Value_set.pointer_analysis option)
    ~(store_label : Instr.Label.t)
    ~(store_offset : int)
    ~(indirect_label : Instr.Label.t)
    ~(type_index : int32)
  : Instr.Label.t option =
  match pointer_analysis with
  | None -> Some store_label
  | Some (cfg_pointers, cfg_spec, _) ->
    functions_potentially_called
      ~module_
      ~indirect_label
      ~type_index
      ~cfg_pointers
      ~cfg_spec
    |> List.fold ~init:None
      ~f:(fun acc fct_index ->
        match acc with
        | None -> call_depends_on_store ~is_call_indirect:true ~pointer_analysis ~call_label:indirect_label ~store_label ~fct_index ~store_offset ()
        | _ -> acc)







let make 
    (module_ : Wasm_module.t)
    (global_deps : Global_read_domain.t Int32Map.t)
    (pointer_analysis : Value_set.pointer_analysis option)
    (cfg : Spec_domain.t Cfg.t) 
  : t =
  let instrs = Cfg.all_instructions cfg in
  (* Instructions that are load or call depend on all stores/calls that may have been executed before, hence on all stores contained in a predecessor of the current node in the CFG *)
  let loads_and_calls = Instr.Label.Map.keys
      (Instr.Label.Map.filter instrs ~f:(fun i -> match i with
           | Call { instr = CallDirect _ ; _ } -> true
           | Call { instr = CallIndirect _ ; _ } -> true
           | Data { instr = Load _ ; _ } -> true
           | Data { instr = MemoryCopy; _ } -> true
           | _ -> false)) in
(* <<<<<<< HEAD *)
  Instr.Label.Map.of_alist_exn (List.map loads_and_calls ~f:(fun label ->
    let enclosing_block = Cfg.find_enclosing_block_exn cfg label in
    let predecessors = Cfg.all_predecessors cfg enclosing_block in
    (label, List.fold_left predecessors ~init:Instr.Label.Set.empty ~f:(fun acc pred_block ->
          Instr.Label.Set.union acc
            (match pred_block.content with
            (* Let's see what depends on a call operation *)
            | Call { instr = CallDirect (_, _, i); label = call_label; _ } -> 
              begin match Instr.Label.Map.find_exn instrs label with
              | Data { instr = Load {offset=load_offset; _}; _ } ->
                load_depends_on_call ~pointer_analysis ~load_label:label ~load_offset ~call_label ~fct_index:i
              | Call { instr = CallDirect (_, _, fct_1_index); label = dependent_label; _ } ->
                call_depends_on_call
                  ~global_deps
                  ~pointer_analysis
                  ~call_label:dependent_label
                  ~depends_on_this_call:call_label
                  ~fct_1_index
                  ~fct_2_index:i
              (* TODO: | Call { instr = CallIndirect (table_idx, _, _, _); _ } -> *)
              | Call { instr = CallIndirect (_, _, _, type_index); label = indirect_label; _ } ->
                call_indirect_depends_on_call
                  ~module_
                  ~global_deps
                  ~pointer_analysis
                  ~depends_on_this_call:call_label
                  ~call_idx_number:i
                  ~indirect_label
                  ~type_index
              | _ -> Instr.Label.Set.singleton call_label
              end
            (* Let's see what depends on a call_indirect operation *)
            | Call { instr = CallIndirect (_, _, _, type_index); label = call_indirect_label; _ } -> 
              begin match Instr.Label.Map.find_exn instrs label with
              | Data { instr = Load {offset=load_offset; _}; _ } ->
                load_depends_on_call_indirect
                  ~module_
                  ~pointer_analysis
                  ~load_label:label
                  ~load_offset
                  ~call_indirect_label
                  ~type_index
              | Call { instr = CallDirect (_, _, call_index); label = call_label; _ } ->
                call_depends_on_call_indirect
                  ~module_
                  ~global_deps
                  ~pointer_analysis
                  ~type_index
                  ~call_label
                  ~indirect_label:call_indirect_label
                  ~call_index
              | Call { instr = CallIndirect (_, _, _, dependent_type_index); label = dependent_label; _ } ->
                call_indirect_depends_on_call_indirect
                  ~module_
                  ~global_deps
                  ~pointer_analysis
                  ~dependency_label:call_indirect_label
                  ~dependency_type_index:type_index
                  ~dependent_label
                  ~dependent_type_index
              | _ -> Instr.Label.Set.singleton call_indirect_label
              end
            | Control _ | Entry | Return _ | Imported _ -> Instr.Label.Set.empty
            | Data instrs' ->
              let instrs' =
                if Int.equal pred_block.idx enclosing_block.idx then
                  List.take_while instrs' ~f:(fun i -> not (Instr.Label.equal i.label label)) 
                else
                  instrs'
                in
                Instr.Label.Set.of_list (List.filter_map instrs' ~f:(function
                  (* Let's see what depends on a store operation *)
                  | { instr = Store {offset=store_offset; _}; label = store_label; _ } -> (
                    match Instr.Label.Map.find_exn instrs label with
                    (* Load instr depends on store: *)
                    | Data { instr = Load {offset=load_offset; _}; _ } -> 
                      load_depends_on_store pointer_analysis ~load_label:label ~store_label ~store_offset ~load_offset
                    (* Call depends on store: *)
                    | Call { instr = CallDirect (_, _, i); _ } ->
                      call_depends_on_store ~pointer_analysis ~call_label:label ~store_label:store_label ~fct_index:i
                      ~store_offset ()
                    (* call_indirect depends on store: *)
                    | Call { instr = CallIndirect (_, _, _, type_index); label = indirect_label; _ } ->
                      call_indirect_depends_on_store
                        ~module_
                        ~pointer_analysis
                        ~store_label
                        ~store_offset
                        ~indirect_label
                        ~type_index
                    | _ ->  Some store_label)
                  | _ -> None)))))))
(* =======
  let deps = Instr.Label.Map.of_alist_exn (List.map loads_and_calls ~f:(fun label ->
      let enclosing_block = Cfg.find_enclosing_block_exn cfg label in
      let predecessors = Cfg.all_predecessors cfg enclosing_block in
      let is_in_loop = List.exists predecessors ~f:(fun p ->
          List.exists (Cfg.Edges.from cfg.edges p.idx) ~f:(fun (pred_of_p, _) ->
              Int.equal pred_of_p enclosing_block.idx)) in
      let get_store_label : 'a Instr.labelled_data -> Instr.Label.t option  = (function
          | { instr = Store _ | MemoryCopy | MemoryFill | MemoryInit _ ; label = sl; _ } -> Some sl
          | _ -> None) in
      (label, List.fold_left predecessors ~init:Instr.Label.Set.empty ~f:(fun acc pred_block ->
           Instr.Label.Set.union acc
             (match pred_block.content with
             | Call { instr = CallDirect _; label = cl; _ } -> Instr.Label.Set.singleton cl
             | Call { instr = CallIndirect _; label = cl; _ } -> Instr.Label.Set.singleton cl
             | Control _ | Entry | Return _ | Imported _ -> Instr.Label.Set.empty
             | Data block_instrs ->
               if Int.equal pred_block.idx enclosing_block.idx && not is_in_loop then
                 (* Same block: only include stores that appear before the load/call, unless there is a loop (the block can reach itself) *)
                 let preceding = List.take_while block_instrs
                     ~f:(fun i -> not (Instr.Label.equal i.label label)) in
                 Instr.Label.Set.of_list (List.filter_map preceding ~f:get_store_label)
               else
                 Instr.Label.Set.of_list (List.filter_map block_instrs ~f:get_store_label)))))) in
  deps
>>>>>>> master *)

let deps_for (deps : t) (instr : Instr.Label.t) : Instr.Label.Set.t =
  match Instr.Label.Map.find deps instr with
  | Some instrs -> instrs
  | None -> Instr.Label.Set.empty

module Test = struct
  let%test "mem-dep with memory" =
    let open Instr.Label.Test in
    let module_ = Wasm_module.of_string "(module
  (type (;0;) (func (param i32) (result i32)))
  (func (;test;) (type 0) (param i32) (result i32)
    memory.size     ;; Instr 0
    memory.size     ;; Instr 1
    i32.store       ;; Instr 2
    memory.size     ;; Instr 3
    i32.load)       ;; Instr 4
  (memory (;0;) 2))" in
    let cfg = Spec_analysis.analyze_intra1 module_ 0l in
    let deps = make module_ Int32Map.empty None cfg in
    let actual = deps_for deps (lab 4) in
    let expected = Instr.Label.Set.singleton (lab 2) in
    Instr.Label.Set.check_equality ~actual ~expected
  let%test "non dep with memory with store after load in the same block" =
    let open Instr.Label.Test in
    let module_ = Wasm_module.of_string "(module
  (type (;0;) (func (param i32) (result)))
  (func (;test;) (type 0) (param i32) (result)
    memory.size     ;; Instr 0
    i32.load        ;; Instr 1
    memory.size     ;; Instr 2
    i32.store)      ;; Instr 3 (should not be a dependency of the preceding load in the same block)
  (memory (;0;) 2))" in
    let cfg = Spec_analysis.analyze_intra1 module_ 0l in
    let deps = make module_ Int32Map.empty None cfg in
    let actual = deps_for deps (lab 1) in
    let expected = Instr.Label.Set.empty in
    Instr.Label.Set.check_equality ~actual ~expected
  let%test "dep with memory with store after load in the same block, in a loop" =
    let open Instr.Label.Test in
    let module_ = Wasm_module.of_string "(module
  (type (;0;) (func (param i32) (result)))
  (func (;test;) (type 0) (param i32) (result)
    loop              ;; Instr 0
      memory.size     ;; Instr 1
      i32.load        ;; Instr 2
      memory.size     ;; Instr 3
      i32.store       ;; Instr 4 (should be a dependency of the preceding load in the same block)
      br 0
    end)
  (memory (;0;) 2))" in
    let cfg = Spec_analysis.analyze_intra1 module_ 0l in
    let deps = make module_ Int32Map.empty None cfg in
    let actual = deps_for deps (lab 2) in
    print_endline ("actual deps: " ^ Instr.Label.Set.to_string actual);
    let expected = Instr.Label.Set.singleton (lab 4) in
    Instr.Label.Set.check_equality ~actual ~expected
  let%test "mem-dep with call" =
    let open Instr.Label.Test in
    let module_ = Wasm_module.of_string "(module
  (type (;0;) (func (param i32) (result i32 i32 i32)))
  (type (;1;) (func))
  (func (;test;) (type 0) (param i32) (result i32 i32 i32)
    memory.size     ;; Instr 0
    memory.size     ;; Instr 1
    call 1       ;; Instr 2
    memory.size     ;; Instr 3
    i32.load)       ;; Instr 4
  (func (;1;) (type 1))
  (memory (;0;) 2))" in
    let cfg = Spec_analysis.analyze_intra1 module_ 0l in
    let deps = make module_ Int32Map.empty None cfg in
    let actual = deps_for deps (lab 4) in
    let expected = Instr.Label.Set.singleton (lab 2) in
    Instr.Label.Set.check_equality ~actual ~expected

(* <<<<<<< HEAD *)
(* end *)
(* ======= *)
  let%test "memory.fill is a memory writer" =
    let open Instr.Label.Test in
    let module_ = Wasm_module.of_string "(module
  (type (;0;) (func (result i32)))
  (func (;test;) (type 0) (result i32)
    i32.const 0   ;; Instr 0: dst
    i32.const 0   ;; Instr 1: val
    i32.const 0   ;; Instr 2: len
    memory.fill   ;; Instr 3: writes memory
    i32.const 0   ;; Instr 4: load address
    i32.load)     ;; Instr 5: must depend on Instr 3
  (memory (;0;) 2))" in
    let cfg = Spec_analysis.analyze_intra1 module_ 0l in
    let deps = make module_ Int32Map.empty None cfg in
    let actual = deps_for deps (lab 5) in
    let expected = Instr.Label.Set.singleton (lab 3) in
    Instr.Label.Set.check_equality ~actual ~expected

  let%test "memory.init is a memory writer" =
    let open Instr.Label.Test in
    let module_ = Wasm_module.of_string "(module
  (type (;0;) (func (result i32)))
  (func (;test;) (type 0) (result i32)
    i32.const 0     ;; Instr 0: dst
    i32.const 0     ;; Instr 1: src offset in data segment
    i32.const 0     ;; Instr 2: len
    memory.init 0   ;; Instr 3: writes memory
    i32.const 0     ;; Instr 4: load address
    i32.load)       ;; Instr 5: must depend on Instr 3
  (memory (;0;) 2)
  (data \"xxxx\"))" in
    let cfg = Spec_analysis.analyze_intra1 module_ 0l in
    let deps = make module_ Int32Map.empty None cfg in
    let actual = deps_for deps (lab 5) in
    let expected = Instr.Label.Set.singleton (lab 3) in
    Instr.Label.Set.check_equality ~actual ~expected

  let%test "memory.copy is a memory writer" =
    let open Instr.Label.Test in
    let module_ = Wasm_module.of_string "(module
  (type (;0;) (func (result i32)))
  (func (;test;) (type 0) (result i32)
    i32.const 0   ;; Instr 0: dst
    i32.const 0   ;; Instr 1: src
    i32.const 0   ;; Instr 2: len
    memory.copy   ;; Instr 3: writes memory
    i32.const 0   ;; Instr 4: load address
    i32.load)     ;; Instr 5: must depend on Instr 3
  (memory (;0;) 2))" in
    let cfg = Spec_analysis.analyze_intra1 module_ 0l in
    let deps = make module_ Int32Map.empty None cfg in
    let actual = deps_for deps (lab 5) in
    let expected = Instr.Label.Set.singleton (lab 3) in
    Instr.Label.Set.check_equality ~actual ~expected

  let%test "memory.copy is a memory reader" =
    let open Instr.Label.Test in
    let module_ = Wasm_module.of_string "(module
  (type (;0;) (func))
  (func (;test;) (type 0)
    i32.const 0   ;; Instr 0: store address
    i32.const 42  ;; Instr 1: store value
    i32.store     ;; Instr 2: writes memory
    i32.const 0   ;; Instr 3: dst
    i32.const 0   ;; Instr 4: src
    i32.const 0   ;; Instr 5: len
    memory.copy)  ;; Instr 6: reads memory — must depend on Instr 2
  (memory (;0;) 2))" in
    let cfg = Spec_analysis.analyze_intra1 module_ 0l in
    let deps = make module_ Int32Map.empty None cfg in
    let actual = deps_for deps (lab 6) in
    let expected = Instr.Label.Set.singleton (lab 2) in
    Instr.Label.Set.check_equality ~actual ~expected

end
(* >>>>>>> master *)
