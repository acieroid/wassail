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
    (pointer_analysis : (Value_set.Domain.t Cfg.t * Spec_domain.t Instr.t Instr.Label.Map.t * Value_set.Domain.t Int32Map.t) option)
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
      Abstract_store_domain.find_value_set cfg_pointers store_label store_address 
      |> (Value_set_abstractions.plus (ValueSet (Reduced_interval_congruence.RIC.ric (0l, Int 0l, Int 0l, ("", Int32.of_int_exn store_offset)))))
    in
    let load_address, load_size = find_load_address_and_size cfg_spec load_label in
    let load_address_value_set = 
      Abstract_store_domain.find_value_set cfg_pointers load_label load_address 
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
    (pointer_analysis : (Value_set.Domain.t Cfg.t * Spec_domain.t Instr.t Instr.Label.Map.t * Value_set.Domain.t Int32Map.t) option)
    (label : Instr.Label.t) 
    (vars : string list)
  : Value_set_abstractions.t =
  let () = print_endline (String.concat ~sep:"" vars) in
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
        | CallDirect ((x,_), _, _) -> x
        | _ -> assert false
      in
      let stack_before_call =
        (label |> Instr.Label.Map.find_exn spec 
               |> Instr.annotation_before
               |> Spec_domain.get_or_fail).vstack
      in
      let var = Variable.Var (extract_nth_argument stack_before_call fct_arity (int_of_string idx)) in
      let vs = Abstract_store_domain.get store ~var in
      Value_set_abstractions.plus (find_parameter_value_set pointer_analysis label rest) vs) 
    | "g" :: idx :: "+" :: rest ->
    (* | "g" :: idx :: rest -> *)
      let vs = Abstract_store_domain.get store ~var:(Variable.Var (Var.Global (int_of_string idx))) in
      Value_set_abstractions.plus (find_parameter_value_set pointer_analysis label rest) vs
    | "n" :: "e" :: "g" :: lg :: idx :: rest ->
      Value_set_abstractions.minus 
        (find_parameter_value_set pointer_analysis label rest)
        (find_parameter_value_set pointer_analysis label [lg; idx])
    | lst -> 
      (print_endline (String.concat ~sep:";" lst); failwith "not yet implemented")
    

let call_depends_on_store
    (pointer_analysis : (Value_set.Domain.t Cfg.t * Spec_domain.t Instr.t Instr.Label.Map.t * Value_set.Domain.t Int32Map.t) option)
    ~(call_label : Instr.Label.t)
    ~(store_label : Instr.Label.t)
    ~(fct_index : int32)
    ~(store_offset : int)
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
        let offset_vs = find_parameter_value_set pointer_analysis call_label (tokenize relative_offset) in
        let () = print_endline ("relative_offset: " ^ Value_set_abstractions.to_string offset_vs) in
        let vs = Value_set_abstractions.i32_add
          (Value_set_abstractions.ValueSet (Reduced_interval_congruence.RIC.ric (stride, lower_bound, upper_bound, ("", o))))
          offset_vs in
        let () = print_endline ("addresses accessed by function: " ^ Value_set_abstractions.to_string vs) in
        vs
      | ValueSet Bottom -> ValueSet Bottom
      | _ -> accessed_memory
    in
    let store_address, store_size = find_store_address_and_size spec store_label in
    let store_address_value_set = 
      Abstract_store_domain.find_value_set 
        cfg_pointers store_label store_address 
      |> (Value_set_abstractions.plus (ValueSet (Reduced_interval_congruence.RIC.ric (0l, Int 0l, Int 0l, ("", Int32.of_int_exn store_offset)))))
    in
    let overlap = Value_set_abstractions.may_overlap ~store_size ~load_size:1l ~store_vs:store_address_value_set ~load_vs:accessed_memory in
    if overlap then Some store_label else None
  | None -> Some store_label



let load_depends_on_call
    ~(pointer_analysis : (Value_set.Domain.t Cfg.t * Spec_domain.t Instr.t Instr.Label.Map.t * Value_set.Domain.t Int32Map.t) option)
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
      Abstract_store_domain.find_value_set cfg_pointers load_label load_address 
      |> (Value_set_abstractions.plus (ValueSet (Reduced_interval_congruence.RIC.ric (0l, Int 0l, Int 0l, ("", Int32.of_int_exn load_offset)))))
    in
    let function_summary = Int32Map.find_exn summaries fct_index in
    let affected_memory_addresses = function_summary.store_operations in
    let overlap =
      List.fold 
        (Reduced_interval_congruence.RICSet.to_list affected_memory_addresses)
        ~init:false
        ~f:(fun acc address ->
          acc || Value_set_abstractions.may_overlap ~store_size:1l ~load_size ~store_vs:(ValueSet address) ~load_vs:load_address_value_set) in
    if overlap then Instr.Label.Set.singleton call_label else Instr.Label.Set.empty






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
    ~(pointer_analysis : (Value_set.Domain.t Cfg.t * Spec_domain.t Instr.t Instr.Label.Map.t * Value_set.Domain.t Int32Map.t) option)
    ~(depend_on_this_call : Instr.Label.t)
    ~(fct_1_index : int32)
    ~(fct_2_index : int32)
  : Instr.Label.Set.t =
  match pointer_analysis with
  | None -> Instr.Label.Set.singleton depend_on_this_call
  | Some (_, _, summaries) ->
    let fct_1_summary = Int32Map.find_exn summaries fct_1_index in
    let fct_2_summary = Int32Map.find_exn summaries fct_2_index in
    let addresses_read_by_fct1 = Abstract_store_domain.get fct_1_summary ~var:Variable.Accessed in
    let addresses_affected_by_fct2 = fct_2_summary.store_operations in
    let memory_overlap =
      addresses_affected_by_fct2
        |> Reduced_interval_congruence.RICSet.to_list
        |> List.fold ~init:false
          ~f:(fun acc addr ->
            (* TODO: refine load_size. We now approximate for the worst case (i64.load) *)
            acc || Value_set_abstractions.may_overlap ~store_size:1l ~load_size:8l ~store_vs:(ValueSet addr) ~load_vs:addresses_read_by_fct1)
    in
    if memory_overlap then
      Instr.Label.Set.singleton depend_on_this_call
    else
      let globals_modified_by_fct2 = globals_modified fct_2_summary in
      if Set.is_empty globals_modified_by_fct2 then
        Instr.Label.Set.empty
      else
        match Int32Map.find_exn global_deps fct_1_index with
        | Top -> Instr.Label.Set.singleton depend_on_this_call
        | NotTop globals_read_by_fct1 ->
          if Global_read_domain.to_variable_names globals_read_by_fct1
              |> Set.inter globals_modified_by_fct2 
              |> Set.is_empty 
          then
            Instr.Label.Set.empty
          else
            Instr.Label.Set.singleton depend_on_this_call

    
    







let make 
    (* (module_ : Wasm_module.t)  (probably needed for CallIndirect instructions) *)
    (global_deps : Global_read_domain.t Int32Map.t)
    (pointer_analysis : (Value_set.Domain.t Cfg.t * Spec_domain.t Instr.t Instr.Label.Map.t * Value_set.Domain.t Int32Map.t) option)
    (cfg : Spec_domain.t Cfg.t) 
  : t =
  let instrs = Cfg.all_instructions cfg in
  (* Instructions that are load or call depend on all stores/calls that may have been executed before, hence on all stores contained in a predecessor of the current node in the CFG *)
  let loads_and_calls = Instr.Label.Map.keys
      (Instr.Label.Map.filter instrs ~f:(fun i -> match i with
           | Call { instr = CallDirect _ ; _ } -> true
           | Call { instr = CallIndirect _ ; _ } -> true
           | Data { instr = Load _ ; _ } -> true
           | _ -> false)) in
  Instr.Label.Map.of_alist_exn (List.map loads_and_calls ~f:(fun label ->
    let block = Cfg.find_enclosing_block_exn cfg label in
    let predecessors = Cfg.all_predecessors cfg block in
    (label, List.fold_left predecessors ~init:Instr.Label.Set.empty ~f:(fun acc block ->
          Instr.Label.Set.union acc
            (match block.content with
            (* Let's see what depends on a call operation *)
            | Call { instr = CallDirect (_, _, i); label = call_label; _ } -> 
              begin match Instr.Label.Map.find_exn instrs label with
              | Data { instr = Load {offset=load_offset; _}; _ } ->
                load_depends_on_call ~pointer_analysis ~load_label:label ~load_offset ~call_label ~fct_index:i
              | Call { instr = CallDirect (_, _, fct_1_index); _ } ->
                call_depends_on_call
                  ~global_deps
                  ~pointer_analysis
                  ~depend_on_this_call:call_label
                  ~fct_1_index
                  ~fct_2_index:i
              (* TODO: | Call { instr = CallIndirect (table_idx, _, _, _); _ } -> *)
              | _ -> Instr.Label.Set.singleton call_label
              end
            | Call { instr = CallIndirect _; label; _ } -> Instr.Label.Set.singleton label (* TODO *)
            | Control _ | Entry | Return _ | Imported _ -> Instr.Label.Set.empty
            | Data instrs' ->
                Instr.Label.Set.of_list (List.filter_map instrs' ~f:(function
                  (* Let's see what depends on a store operation *)
                  | { instr = Store {offset=store_offset; _}; label = store_label; _ } -> (
                    match Instr.Label.Map.find_exn instrs label with
                    (* Load instr depends on store: *)
                    | Data { instr = Load {offset=load_offset; _}; _ } -> 
                      load_depends_on_store pointer_analysis ~load_label:label ~store_label ~store_offset ~load_offset
                    (* Call depends on store: *)
                    | Call { instr = CallDirect (_, _, i); _ } ->
                      call_depends_on_store pointer_analysis ~call_label:label ~store_label:store_label ~fct_index:i
                      ~store_offset
                    | _ ->  Some store_label)
                  | _ -> None)))))))

let deps_for (deps : t) (instr : Instr.Label.t) : Instr.Label.Set.t =
  match Instr.Label.Map.find deps instr with
  | Some instrs -> instrs
  | None -> Instr.Label.Set.empty

(* module Test = struct
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
    let deps = make cfg in
    let actual = deps_for deps (lab 4) in
    let expected = Instr.Label.Set.singleton (lab 2) in
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
    let deps = make cfg in
    let actual = deps_for deps (lab 4) in
    let expected = Instr.Label.Set.singleton (lab 2) in
    Instr.Label.Set.check_equality ~actual ~expected

end *)
