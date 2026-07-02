open Core
open Helpers

type t = Instr.Label.Set.t Instr.Label.Map.t (* Map from instruction to its memory dependencies *)

module RIC = Reduced_interval_congruence.RIC
module RICSet = Reduced_interval_congruence.RICSet
module ValueSet = Value_set_abstraction
module AbstractStore = Abstract_store_domain

type pointer_analysis = Value_set.pointer_analysis

(** Returns whether the instruction at [label] is unreachable according to the
    pointer analysis.

    The function retrieves the abstract state immediately before the
    instruction and returns the value of its [unreachable] flag.

    Raises [Failure] if [label] does not correspond to an instruction in
    [cfg].
*)
let is_unreachable (cfg : AbstractStore.t Cfg.t) (label : Instr.Label.t) : bool =
  ((cfg |> Cfg.all_instructions |> Cfg.find_instr_exn) label |> Instr.annotation_before).unreachable

(** Returns the number of bytes read or written by a load/store instruction.

    For unpacked operations, the size is determined by the instruction type
    (4 bytes for [i32]/[f32], 8 bytes for [i64]/[f64]). For packed operations,
    the size is derived from the pack mode (e.g. [i32.load8_u] -> 1 byte,
    [i64.load32_s] -> 4 bytes).

    Raises [Assert_failure] if called on an instruction that is neither a load
    nor a store.
*)
let find_memory_op_size
    (instruction : Spec_domain.t Instr.t) 
  : int32 =
  match instruction with
  | Data { instr = Store {typ = I32; pack; _}; _ }
  | Data { instr = Store {typ = F32; pack; _}; _ } 
  | Data { instr = Load {typ = I32; pack; _}; _ }
  | Data { instr = Load {typ = F32; pack; _}; _ } -> 
    begin match pack with
    | None -> 4l
    | Some (size, _) -> Int32.((size |> Memoryop.pack_size_to_int |> Int32.of_int_exn) / 8l)
    end
  | Data { instr = Store {typ = I64; pack; _}; _ }
  | Data { instr = Store {typ = F64; pack; _}; _ }
  | Data { instr = Load {typ = I64; pack; _}; _ }
  | Data { instr = Load {typ = F64; pack; _}; _ } -> 
    begin match pack with
    | None -> 8l
    | Some (size, _) -> Int32.((size |> Memoryop.pack_size_to_int |> Int32.of_int_exn) / 8l)
    end
  | _ -> assert false

(** Returns the address operand and access size of a store instruction.

    The returned address corresponds to the value popped from the stack by the
    store instruction (the second value popped, after the value being stored).
    The access size is given in bytes and accounts for packed store operations.

    Raises [Assert_failure] if [label] does not refer to a store instruction or
    if its abstract state is unavailable.
*)
let find_store_address_and_size 
    (cfg : Spec_domain.t Instr.t Instr.Label.Map.t) 
    (label : Instr.Label.t) 
  : Var.t * int32 =
  let instruction = label |> Instr.Label.Map.find_exn cfg in
  let store_size = instruction |> find_memory_op_size in
  let annotation_before = instruction |> Instr.annotation_before in
  let (_, address) = pop2 (Spec_domain.get_or_fail annotation_before).vstack in
  address, store_size

(** Returns the address operand and access size of a load instruction.

    The returned address corresponds to the value popped from the stack by the
    load instruction. The access size is given in bytes and accounts for packed
    load operations.

    Raises [Assert_failure] if [label] does not refer to a load instruction or
    if its abstract state is unavailable.
*)
let find_load_address_and_size 
    (cfg : Spec_domain.t Instr.t Instr.Label.Map.t) 
    (label : Instr.Label.t) 
  : Var.t * int32 =
  let instruction = label |> Instr.Label.Map.find_exn cfg in
  let load_size = instruction |> find_memory_op_size in
  let annotation_before = instruction |> Instr.annotation_before in
  pop (Spec_domain.get_or_fail annotation_before).vstack, load_size

(** Returns [Some store_label] if the load may read a byte written by the store.

    When pointer-analysis information is unavailable, the dependency is kept
    conservatively. Otherwise, the function retrieves the abstract address and
    access size of both instructions, adds the static memory offsets encoded in
    the load/store operations, and checks whether the resulting memory ranges
    may overlap.

    Returns [None] when pointer analysis proves that the store and load access
    disjoint memory ranges.
*)
let load_depends_on_store 
    (pointer_analysis : pointer_analysis option)
    ~(load_label : Instr.Label.t)
    ~(store_label : Instr.Label.t)
    ~(store_offset : int)
    ~(load_offset : int)
  : Instr.Label.t option =
  match pointer_analysis with
  | None -> Some store_label
  | Some (cfg_pointers, cfg_spec, _) ->
    if is_unreachable cfg_pointers store_label then
      None
    else
      let store_address, store_size = find_store_address_and_size cfg_spec store_label in
      let store_address_value_set = 
        AbstractStore.find_value_set cfg_pointers ~label:store_label ~var:store_address 
        |> (ValueSet.((+) (constant_int store_offset)))
      in
      let load_address, load_size = find_load_address_and_size cfg_spec load_label in
      let load_address_value_set = 
        AbstractStore.find_value_set cfg_pointers ~label:load_label ~var:load_address 
        |> ValueSet.((+) (constant_int load_offset))
      in

      let overlap = ValueSet.may_overlap ~store_size ~load_size ~store_vs:store_address_value_set ~load_vs:load_address_value_set in
      if overlap then Some store_label else None
  

(** Returns the [n]-th argument of a function call from the operand stack.

    The stack is assumed to contain exactly the function's arguments (followed
    by older stack values). Argument 0 is the first function parameter, even
    though it is deeper on the operand stack than later arguments.

    Raises [Failure] if [n] is not a valid argument index.
*)
let rec extract_nth_argument
    (stack : Var.t list)
    (arity : int)
    (n : int)
  : Var.t =
  match stack, arity, n with
  | _, a, n when n >= a -> 
    Log.error (fun () -> "function arity doesn't match argument index");
    failwith "function arity doesn't match argument index"
  | s, a, n when n = a - 1 -> pop s
  | _ :: s, a, n -> extract_nth_argument s (a-1) n
  | _ -> assert false

(** Splits a symbolic relative-offset expression into a list of tokens.

    The tokenizer recognizes lowercase identifiers, decimal integers, and the
    ['+'] and ['-'] operators. For example,
    ["l0+g1-8"] is tokenized as [["l"; "0"; "+"; "g"; "1"; "-"; "8"]].

    Raises [Failure] if the input contains an unexpected character.
*)
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

(** Evaluates a symbolic relative-offset expression at a call site.

    The expression is provided as a token list (typically produced by
    [tokenize]) and may refer to:
    - function parameters ([l<i>]),
    - global variables ([g<i>]),
    - additions ([+]), and
    - negation/subtraction ([neg]).

    Parameter references are resolved from the operand stack immediately before
    the call, while global references are resolved from the abstract store
    before the call instruction. The resulting abstract value set is obtained by
    recursively evaluating the expression.

    When [is_call_indirect] is [true], the function accounts for the extra table
    index argument present on the operand stack before an indirect call.

    Unknown or unsupported expressions conservatively evaluate to [ValueSet.top].

    Raises [Failure] if [label] does not correspond to a call instruction.
*)
let find_parameter_value_set 
    ?(is_call_indirect : bool = false)
    (pointer_analysis : pointer_analysis option)
    (label : Instr.Label.t) 
    (vars : string list)
  : ValueSet.t =
  match pointer_analysis with
  | None -> assert false
  | Some (cfg_pointers, spec, _) ->
    let block = Cfg.find_enclosing_block_exn cfg_pointers label in
    let call_instr =
      match block.content with
      | Call call_instr ->  call_instr
      | _ -> Log.error (fun () -> "label does not belong to a call instruction"); 
        failwith "label does not belong to a call instruction"
    in
    let store = call_instr.annotation_before in
    let rec aux = function 
      | [] -> ValueSet.ValueSet RIC.zero
      | "l" :: idx :: "+" :: rest
      | "l" :: idx :: rest ->
        (let fct_arity =
          match call_instr.instr with
          | CallDirect ((x,_), _, _)
          | CallIndirect (_, (x,_), _, _) -> x
        in
        let stack_before_call =
          (label |> Instr.Label.Map.find_exn spec |> Instr.annotation_before |> Spec_domain.get_or_fail).vstack
          |> if is_call_indirect then List.tl_exn else fun x -> x
        in
        let var = Variable.Var (extract_nth_argument stack_before_call fct_arity (int_of_string idx)) in
        let vs = AbstractStore.get store ~var in
        ValueSet.(aux rest + vs)) 
      | "g" :: idx :: "+" :: rest
      | "g" :: idx :: rest ->
        let vs = AbstractStore.get store ~var:(Variable.Var (Var.Global (int_of_string idx))) in
        ValueSet.(aux rest + vs)
      | "n" :: "e" :: "g" :: lg :: idx :: rest ->
        ValueSet.(aux rest - aux [lg; idx])
      | lst ->
        Log.warn (fun () -> Printf.sprintf "no known variables correspond to this relative offset: %s" (String.concat ~sep:";" lst));
        ValueSet.top
    in aux vars
    

(** Returns [Some store_label] if the call may read memory written by the store.

    Without pointer-analysis information, the dependency is kept conservatively.
    Otherwise, the function uses the callee summary identified by [fct_index] to
    retrieve the memory addresses that may be accessed by the call, instantiates
    any relative offset in that summary at the call site, and checks whether the
    resulting read addresses may overlap the memory range written by the store.

    When [is_call_indirect] is [true], relative offsets that depend on function
    parameters are interpreted using the operand-stack layout of an indirect
    call, where the table index is present above the actual arguments.

    Returns [None] if the store is unreachable, or if pointer analysis proves
    that the store and the call access disjoint memory ranges.
*)
let call_depends_on_store
    ?(is_call_indirect : bool = false)
    ~(pointer_analysis : pointer_analysis option)
    ~(call_label : Instr.Label.t)
    ~(store_label : Instr.Label.t)
    ~(fct_index : int32)
    ~(store_offset : int)
    (() : unit)
  : Instr.Label.t option =
  match pointer_analysis with
  | Some (cfg_pointers, spec, summaries) ->
    if is_unreachable cfg_pointers store_label then
      None
    else
      let fct_summary = Int32Map.find_exn summaries fct_index in
      let accessed_memory = AbstractStore.get fct_summary ~var:(Variable.Accessed) in
      let accessed_memory =
        match accessed_memory with
        | ValueSet RIC {offset=("", _); _} -> accessed_memory
        | ValueSet RIC {offset=(relative_offset, _); _} ->
          find_parameter_value_set ~is_call_indirect pointer_analysis call_label (tokenize relative_offset) 
          |>  ValueSet.((+) (remove_relative_offset accessed_memory))
        | ValueSet Bottom -> ValueSet Bottom
        | _ -> accessed_memory
      in
      let store_address, store_size = find_store_address_and_size spec store_label in
      let store_address_value_set = 
        cfg_pointers
        |> AbstractStore.find_value_set ~label:store_label ~var:store_address 
        |> ValueSet.((+) (constant_int store_offset))
      in
      let overlap = ValueSet.may_overlap ~store_size ~load_size:1l ~store_vs:store_address_value_set ~load_vs:accessed_memory in
      if overlap then Some store_label else None
  | None -> Some store_label



(** Returns [Some call_label] if the load may read memory written by the call.

    Without pointer-analysis information, the dependency is kept
    conservatively. Otherwise, the function retrieves the abstract address read
    by the load, instantiates each memory location that may be written by the
    callee summary identified by [fct_index], and checks whether any of these
    writes may overlap the memory range read by the load.

    When [is_call_indirect] is [true], relative offsets in the callee summary
    that depend on function parameters are instantiated using the operand-stack
    layout of an indirect call, where the table index is present above the
    actual arguments.

    Returns [None] if the call is unreachable, or if pointer analysis proves
    that the call cannot write any byte read by the load.
*)
let load_depends_on_call
    ~(pointer_analysis : pointer_analysis option)
    ~(load_label : Instr.Label.t)
    ~(load_offset : int)
    ~(call_label : Instr.Label.t)
    ?(is_call_indirect : bool = false)
    ~(fct_index : int32)
    (() : unit)
  : Instr.Label.t option =
  match pointer_analysis with
  | None -> Some call_label
  | Some (cfg_pointers, spec, summaries) ->
    if is_unreachable cfg_pointers call_label then
      None
    else
    let load_address, load_size = find_load_address_and_size spec load_label in
    let load_address_value_set =
      AbstractStore.find_value_set cfg_pointers ~label:load_label ~var:load_address 
      |> ValueSet.((+) (constant_int load_offset))
    in
    let function_summary = Int32Map.find_exn summaries fct_index in
    let affected_memory_addresses = function_summary.store_operations in
    let overlap =
      List.fold 
        (RICSet.to_list affected_memory_addresses)
        ~init:false
        ~f:(fun acc address ->
          acc || 
          let store_vs =
            match address with
            | RIC { offset = ("", _); _ } -> ValueSet.ValueSet address
            | RIC { offset=(relative_offset, _); _ } ->
              find_parameter_value_set ~is_call_indirect pointer_analysis call_label (tokenize relative_offset)
              |> ValueSet.((+) (remove_relative_offset (ValueSet address)))
            | _ -> ValueSet address
          in
          ValueSet.may_overlap ~store_size:1l ~load_size ~store_vs ~load_vs:load_address_value_set) in
    if overlap then Some call_label else None


(** Returns the functions that may be called by a [call_indirect] instruction.

    The possible targets are first restricted using the static type constraint
    of the indirect call, represented by [type_index]. When pointer analysis
    gives a non-relative numeric value for the table index operand, the function
    further filters those targets by keeping only table entries whose indices
    may overlap that value.

    If the table index value is too imprecise, is represented as a bitfield, or
    still contains a symbolic relative offset, the function falls back to all
    statically possible indirect-call targets for [type_index].

    Raises if [indirect_label] cannot be found in [cfg_spec], if its abstract
    state is unavailable, or if the module does not contain a table instance.
*)
let functions_potentially_called 
    ~(module_ : Wasm_module.t)
    ~(indirect_label : Instr.Label.t)
    ~(type_index : int32)
    ~(cfg_pointers : AbstractStore.t Cfg.t)
    ~(cfg_spec : (Instr.Label.t, Spec_domain.t Instr.t, 'a) Map_intf.Map.t)
  : int32 list =
  let call_indirect_index =
      pop (indirect_label 
            |> Instr.Label.Map.find_exn cfg_spec 
            |> Instr.annotation_before
            |> Spec_domain.get_or_fail).vstack in
    let call_indirect_index_value =
      AbstractStore.find_value_set cfg_pointers ~label:indirect_label ~var:call_indirect_index in
    let indirect_call_targets = Call_graph.indirect_call_targets module_ type_index in
    if call_indirect_index_value |> ValueSet.extract_relative_offset |> String.is_empty then
      match call_indirect_index_value with
      | Bitfield _ -> indirect_call_targets
      | Boolean {numeric_value = r; _}
      | ValueSet r when String.(RIC.extract_relative_offset r <> "") -> indirect_call_targets
      | Boolean {numeric_value = r; _}
      | ValueSet r -> 
        let table = module_.table_insts |> List.hd_exn in
        table
        |> Table_inst.indices
        |> List.filter ~f:(fun idx -> RIC.(meet (constant idx) r <> Bottom))
        |> List.filter_map ~f:(fun idx -> Table_inst.get table idx)
        |> List.filter ~f:(fun idx -> List.mem indirect_call_targets idx ~equal:Int32.(=))
    else
      indirect_call_targets


(** Returns [Some call_indirect_label] if the load may read memory written by
    one of the possible targets of the indirect call.

    Without pointer-analysis information, the dependency is kept
    conservatively. Otherwise, the function first computes the functions that
    may be reached by the [call_indirect] instruction, then checks whether the
    load may depend on any of those targets using [load_depends_on_call].

    Relative offsets in callee summaries are instantiated using the actual
    arguments of the indirect call.

    Returns [None] if the indirect call is unreachable, or if pointer analysis
    proves that none of its possible targets can write any byte read by the
    load.
*)
let load_depends_on_call_indirect
    ~(module_ : Wasm_module.t)
    ~(pointer_analysis : pointer_analysis option)
    ~(load_label : Instr.Label.t)
    ~(load_offset : int)
    ~(call_indirect_label : Instr.Label.t)
    ~(type_index : int32)
  : Instr.Label.t option =
  match pointer_analysis with
  | None -> Some call_indirect_label
  | Some (cfg_pointers, cfg_spec, _summaries) ->
    if is_unreachable cfg_pointers call_indirect_label then
      None
    else
      functions_potentially_called
        ~module_
        ~indirect_label:call_indirect_label
        ~type_index
        ~cfg_pointers
        ~cfg_spec
      |> List.fold ~init:None
        ~f:(fun acc fct_index ->
          match acc with
          | Some _ -> acc
          | None ->
            load_depends_on_call
              ~pointer_analysis
              ~load_label
              ~load_offset
              ~call_label:call_indirect_label
              ~is_call_indirect:true
              ~fct_index
              ())




(** Returns the names of the global variables modified by a function.

    The function compares the abstract value of each global in the function
    summary against its initial symbolic value. A global is considered modified
    if its abstract value differs from the default relative value associated
    with that global.
*)
let globals_modified (summary : AbstractStore.t) : String.Set.t =
  summary 
    |> AbstractStore.extract_global_values
    |> Map.filteri ~f:(fun ~key ~data -> RIC.(data <> RIC.relative_ric key))
    |> Map.to_alist 
    |> List.fold ~init:[] ~f:(fun acc (g,_) -> g :: acc)
    |> String.Set.of_list


(** Returns [Some depends_on_this_call] if [call_label] may depend on a previous
    call.

    The function checks whether the function called at [call_label], identified
    by [fct_1_index], may observe an effect produced by the previous call
    [depends_on_this_call], identified by [fct_2_index].

    Without pointer-analysis information, the dependency is kept conservatively.
    Otherwise, the dependency is kept if:
    - the memory read by [fct_1_index] may overlap memory written by
      [fct_2_index], or
    - [fct_1_index] may read a global modified by [fct_2_index].

    Relative offsets in function summaries are instantiated at the corresponding
    call site. [dependant_is_indirect] indicates that [call_label] is an
    indirect call, while [dependency_is_indirect] indicates that
    [depends_on_this_call] is an indirect call.

    Returns [None] if [depends_on_this_call] is unreachable, or if pointer
    analysis proves that the two calls are independent.
*)
let call_depends_on_call
    ~(global_deps : Global_read_domain.t Int32Map.t)
    ~(pointer_analysis : pointer_analysis option)
    ~(call_label : Instr.Label.t)
    ?(dependant_is_indirect : bool = false)
    ~(depends_on_this_call : Instr.Label.t)
    ?(dependency_is_indirect : bool = false)
    ~(fct_1_index : int32)
    ~(fct_2_index : int32)
    (() : unit)
  : Instr.Label.t option =
  match pointer_analysis with
  | None -> Some depends_on_this_call
  | Some (cfg_pointers, _, summaries) ->
    if is_unreachable cfg_pointers depends_on_this_call then
      None
    else
      let fct_1_summary = Int32Map.find_exn summaries fct_1_index in
      let fct_2_summary = Int32Map.find_exn summaries fct_2_index in
      let addresses_read_by_fct1 : ValueSet.t = 
        let tmp_address = AbstractStore.get fct_1_summary ~var:Variable.Accessed in
        match tmp_address with
        | ValueSet.ValueSet RIC { offset = ("", _); _ } -> tmp_address
        | ValueSet RIC { offset=(relative_offset, _); _ } ->
          find_parameter_value_set ~is_call_indirect:dependant_is_indirect pointer_analysis call_label (tokenize relative_offset)
          |> ValueSet.((+) (remove_relative_offset tmp_address))
        | _ -> tmp_address
      in
      let addresses_affected_by_fct2 = 
        fct_2_summary.store_operations 
        |> RICSet.to_list
        |> List.map ~f:(fun address ->
          match address with
          | RIC {offset = ("", _); _} -> address
          | RIC {offset=(relative_offset, _); _} ->
            let tmp_vs =
              find_parameter_value_set 
                ~is_call_indirect:dependency_is_indirect 
                pointer_analysis 
                depends_on_this_call 
                (tokenize relative_offset)
              |> ValueSet.((+) (remove_relative_offset (ValueSet address)))
            in
            (match tmp_vs with
            | ValueSet tmp_vs
            | Boolean { numeric_value = tmp_vs; _ } -> tmp_vs
            | Bitfield bf -> RIC.of_bitfield bf)
          | _ -> address)
      in
      let memory_overlap =
        addresses_affected_by_fct2
          |> List.fold ~init:false
            ~f:(fun acc addr ->
              (* TODO: refine load_size. We now approximate for the worst case (i64.load) *)
              acc || ValueSet.may_overlap ~store_size:1l ~load_size:8l ~store_vs:(ValueSet addr) ~load_vs:addresses_read_by_fct1)
      in
      if memory_overlap then
        Some depends_on_this_call
      else
        let globals_modified_by_fct2 = globals_modified fct_2_summary in
        if Set.is_empty globals_modified_by_fct2 then
          None
        else
          match Int32Map.find_exn global_deps fct_1_index with
          | Top -> Some depends_on_this_call
          | NotTop globals_read_by_fct1 ->
            if Global_read_domain.to_variable_names globals_read_by_fct1
                |> Set.inter globals_modified_by_fct2 
                |> Set.is_empty 
            then
              None
            else
              Some depends_on_this_call


(** Returns [Some depends_on_this_call] if the indirect call may depend on a
    previous direct call.

    The function resolves the possible targets of the indirect call, then checks
    whether any of those target functions may observe an effect produced by the
    direct call [depends_on_this_call].

    Without pointer-analysis information, the dependency is kept
    conservatively.

    Returns [None] if the previous call is unreachable, or if pointer analysis
    proves that none of the possible indirect-call targets depend on it.
*)
let call_indirect_depends_on_call
    ~(module_ : Wasm_module.t)
    ~(global_deps : Global_read_domain.t Int32Map.t)
    ~(pointer_analysis : pointer_analysis option)
    ~(depends_on_this_call : Instr.Label.t)
    ~(call_idx_number : int32)
    ~(indirect_label : Instr.Label.t)
    ~(type_index : int32)
  : Instr.Label.t option =
  match pointer_analysis with
  | None -> Some depends_on_this_call
  | Some (cfg_pointers, cfg_spec, _) ->
    if is_unreachable cfg_pointers depends_on_this_call then
      None
    else
      functions_potentially_called
        ~module_
        ~indirect_label
        ~type_index
        ~cfg_pointers
        ~cfg_spec
      |> List.find_map 
        ~f:(fun idx ->
          call_depends_on_call
            ~global_deps
            ~pointer_analysis
            ~call_label:indirect_label
            ~dependant_is_indirect:true
            ~depends_on_this_call
            ~fct_1_index:idx
            ~fct_2_index:call_idx_number
            ())
                

(** Returns [Some indirect_label] if a direct call may depend on a previous
    indirect call.

    The function resolves the possible targets of the previous indirect call,
    then checks whether the direct call [call_label] may observe an effect
    produced by any of those targets.

    Without pointer-analysis information, the dependency is kept
    conservatively.

    Returns [None] if the indirect call is unreachable, or if pointer analysis
    proves that the direct call does not depend on any possible target of the
    indirect call.
*)
let call_depends_on_call_indirect
    ~(module_ : Wasm_module.t)
    ~(global_deps : Global_read_domain.t Int32Map.t)
    ~(pointer_analysis : pointer_analysis option)
    ~(type_index : int32)
    ~(call_label : Instr.Label.t)
    ~(indirect_label : Instr.Label.t)
    ~(call_index : int32)
  : Instr.Label.t option =
  match pointer_analysis with
  | None -> Some indirect_label
  | Some (cfg_pointers, cfg_spec, _) ->
    if is_unreachable cfg_pointers indirect_label then
      None
    else
      functions_potentially_called
        ~module_
        ~indirect_label
        ~type_index
        ~cfg_pointers
        ~cfg_spec
      |> List.find_map
        ~f:(fun fct_index ->
          call_depends_on_call
            ~global_deps
            ~pointer_analysis
            ~call_label
            ~depends_on_this_call:indirect_label
            ~dependency_is_indirect:true
            ~fct_1_index:call_index
            ~fct_2_index:fct_index
            ()
        )


(** Returns [Some dependency_label] if an indirect call may depend on a previous
    indirect call.

    The function resolves the possible targets of both indirect calls, then
    checks whether any possible target of [dependent_label] may observe an
    effect produced by any possible target of [dependency_label].

    Without pointer-analysis information, the dependency is kept
    conservatively.

    Returns [None] if the previous indirect call is unreachable, or if pointer
    analysis proves that no target pair creates a dependency.
*)
let call_indirect_depends_on_call_indirect
    ~(module_ : Wasm_module.t)
    ~(global_deps : Global_read_domain.t Int32Map.t)
    ~(pointer_analysis : pointer_analysis option)
    ~(dependency_label : Instr.Label.t)
    ~(dependency_type_index : int32)
    ~(dependent_label : Instr.Label.t)
    ~(dependent_type_index : int32)
  : Instr.Label.t option =
  match pointer_analysis with
  | None -> Some dependency_label
  | Some (cfg_pointers, cfg_spec, _) ->
    if is_unreachable cfg_pointers dependency_label then
      None
    else
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
      |> List.find_map 
        ~f:(fun dependency_index ->
          dependent_targets
          |> List.find_map 
            ~f:(fun dependent_index ->
              call_depends_on_call
                ~global_deps
                ~pointer_analysis
                ~call_label:dependent_label
                ~dependant_is_indirect:true
                ~depends_on_this_call:dependency_label
                ~dependency_is_indirect:true
                ~fct_1_index:dependent_index
                ~fct_2_index:dependency_index
                ()))


(** Returns [Some store_label] if the indirect call may read memory written by
    the store.

    The function resolves the possible targets of the indirect call, then checks
    whether any of those target functions may read memory written by the store
    using [call_depends_on_store].

    Without pointer-analysis information, the dependency is kept
    conservatively.

    Returns [None] if the store is unreachable, or if pointer analysis proves
    that none of the possible targets of the indirect call can read memory
    written by the store.
*)
let call_indirect_depends_on_store
    ~(module_ : Wasm_module.t)
    ~(pointer_analysis : pointer_analysis option)
    ~(store_label : Instr.Label.t)
    ~(store_offset : int)
    ~(indirect_label : Instr.Label.t)
    ~(type_index : int32)
  : Instr.Label.t option =
  match pointer_analysis with
  | None -> Some store_label
  | Some (cfg_pointers, cfg_spec, _) ->
    if is_unreachable cfg_pointers store_label then
      None
    else
      functions_potentially_called
        ~module_
        ~indirect_label
        ~type_index
        ~cfg_pointers
        ~cfg_spec
      |> List.find_map 
        ~f:(fun fct_index ->
          call_depends_on_store
            ~is_call_indirect:true
            ~pointer_analysis
            ~call_label:indirect_label
            ~store_label
            ~fct_index
            ~store_offset
            ())







let make 
    (module_ : Wasm_module.t)
    (global_deps : Global_read_domain.t Int32Map.t)
    (pointer_analysis : Value_set.pointer_analysis option)
    (cfg : Spec_domain.t Cfg.t) 
  : t =
  let instrs = Cfg.all_instructions cfg in
  let loads_and_calls = Instr.Label.Map.keys
      (Instr.Label.Map.filter instrs ~f:(fun i -> match i with
           | Call { instr = CallDirect _ ; _ } -> true
           | Call { instr = CallIndirect _ ; _ } -> true
           | Data { instr = Load _ ; _ } -> true
           | Data { instr = MemoryCopy; _ } -> true
           | _ -> false)) in
  Instr.Label.Map.of_alist_exn (List.map loads_and_calls ~f:(fun label ->
    let enclosing_block = Cfg.find_enclosing_block_exn cfg label in
    let predecessors = Cfg.all_predecessors cfg enclosing_block in
    let is_in_loop = List.exists predecessors ~f:(fun p ->
          List.exists (Cfg.Edges.from cfg.edges p.idx) ~f:(fun (pred_of_p, _) ->
              Int.equal pred_of_p enclosing_block.idx)) in
    (label, List.fold_left predecessors ~init:Instr.Label.Set.empty ~f:(fun acc pred_block ->
          Instr.Label.Set.union acc
            (match pred_block.content with
            (* Let's see what depends on a call operation: *)
            | Call { instr = CallDirect (_, _, i); label = call_label; _ } -> 
              begin match Instr.Label.Map.find_exn instrs label with
              | Data { instr = Load {offset=load_offset; _}; _ } ->
                load_depends_on_call
                  ~pointer_analysis
                  ~load_label:label
                  ~load_offset
                  ~call_label
                  ~fct_index:i
                  ()
                |> Option.value_map ~default:Instr.Label.Set.empty ~f:Instr.Label.Set.singleton
              | Call { instr = CallDirect (_, _, fct_1_index); label = dependent_label; _ } ->
                call_depends_on_call
                  ~global_deps
                  ~pointer_analysis
                  ~call_label:dependent_label
                  ~depends_on_this_call:call_label
                  ~fct_1_index
                  ~fct_2_index:i
                  ()
                |> Option.value_map ~default:Instr.Label.Set.empty ~f:Instr.Label.Set.singleton
              | Call { instr = CallIndirect (_, _, _, type_index); label = indirect_label; _ } ->
                call_indirect_depends_on_call
                  ~module_
                  ~global_deps
                  ~pointer_analysis
                  ~depends_on_this_call:call_label
                  ~call_idx_number:i
                  ~indirect_label
                  ~type_index
                |> Option.value_map ~default:Instr.Label.Set.empty ~f:Instr.Label.Set.singleton
              | _ -> Instr.Label.Set.singleton call_label
              end
            (* Let's see what depends on a call_indirect operation: *)
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
                |> Option.value_map ~default:Instr.Label.Set.empty ~f:Instr.Label.Set.singleton
              | Call { instr = CallDirect (_, _, call_index); label = call_label; _ } ->
                call_depends_on_call_indirect
                  ~module_
                  ~global_deps
                  ~pointer_analysis
                  ~type_index
                  ~call_label
                  ~indirect_label:call_indirect_label
                  ~call_index
                |> Option.value_map ~default:Instr.Label.Set.empty ~f:Instr.Label.Set.singleton
              | Call { instr = CallIndirect (_, _, _, dependent_type_index); label = dependent_label; _ } ->
                call_indirect_depends_on_call_indirect
                  ~module_
                  ~global_deps
                  ~pointer_analysis
                  ~dependency_label:call_indirect_label
                  ~dependency_type_index:type_index
                  ~dependent_label
                  ~dependent_type_index
                |> Option.value_map ~default:Instr.Label.Set.empty ~f:Instr.Label.Set.singleton
              | _ -> Instr.Label.Set.singleton call_indirect_label
              end
            | Control _ | Entry | Return _ | Imported _ -> Instr.Label.Set.empty
            | Data instrs' ->
              let instrs' =
                if Int.equal pred_block.idx enclosing_block.idx && not is_in_loop then
                  (* Same block: only include stores that appear before the load/call, unless there is a loop (the block can reach itself) *)
                  List.take_while instrs' ~f:(fun i -> not (Instr.Label.equal i.label label)) 
                else
                  instrs'
                in
                Instr.Label.Set.of_list (List.filter_map instrs' ~f:(function
                  (* Let's see what depends on a store operation: *)
                  | { instr = Store {offset=store_offset; _}; label = store_label; _ } ->
                    begin match Instr.Label.Map.find_exn instrs label with
                    (* Load instr depends on store: *)
                    | Data { instr = Load {offset=load_offset; _}; _ } -> 
                      load_depends_on_store pointer_analysis ~load_label:label ~store_label ~store_offset ~load_offset
                    | Call { instr = CallDirect (_, _, i); _ } ->
                      call_depends_on_store ~pointer_analysis ~call_label:label ~store_label:store_label ~fct_index:i
                      ~store_offset ()
                    | Call { instr = CallIndirect (_, _, _, type_index); label = indirect_label; _ } ->
                      call_indirect_depends_on_store
                        ~module_
                        ~pointer_analysis
                        ~store_label
                        ~store_offset
                        ~indirect_label
                        ~type_index
                    | Data { instr = MemoryCopy; _ }  ->  Some store_label
                    | _ -> assert false (* Not a load or call *)
                    end
                  (* Without precise pointer analysis, all loads and calls depend on memory operations: *)
                  | { instr = MemoryFill; label = store_label; _ }
                  | { instr = MemoryInit _; label = store_label; _ }
                  | { instr = MemoryCopy; label = store_label; _ } -> Some store_label
                  | _ -> None)))))))

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
