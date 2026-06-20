open Core
open Reduced_interval_congruence
module Value = Value_set_abstraction
open Helpers

type ric = RIC.t
type boolean = Boolean.t

(* TODO: put this somewhere else *)
let (|>>) (c, i : 'a * 'b) (f : 'a -> 'b -> 'c) : 'c = f c i

(** Abstract store for the value-set analysis.

    [abstract_store] maps variables to abstract values.
    [store_operations] records memory regions that may have been written since
    the current function entry or summary extraction point.
    [unreachable] marks control-flow states that cannot currently be reached;
    transfer functions should usually leave such states unchanged. *)
type t = {
  abstract_store : Value.t Variable.Map.t;
  store_operations : RICSet.t;
  unreachable : bool
}
[@@deriving sexp, compare]

(** [extract_memory_variables store] returns the memory variables present in [store]. *)
let extract_memory_variables (store : t) : Variable.t list =
  Variable.Map.extract_memory_variables store.abstract_store

(** [extract_locals_and_globals store] returns the local and global variables present in [store]. *)
let extract_locals_and_globals (store : t) : Variable.t list =
  Variable.Map.extract_locals_and_globals store.abstract_store

(** [get store ~var] returns [var]'s abstract value.

    Missing special variables are interpreted by default. Missing locals and globals
    are treated as out of scope and return [Bottom]. Missing memory variables are
    reconstructed from covered memory regions when possible, and return [Top]
    otherwise. *)
let get (store : t) ~(var : Variable.t) : Value.t =
  match Variable.Map.find store.abstract_store var with
    | Some value -> value
    | None ->
      (match var with
      | Accessed -> ValueSet RIC.Bottom
      | MemorySize -> ValueSet RIC.positive_integers
      | Var Const I32 n -> ValueSet (RIC.constant n)
      | Var Const _ | Var Other _ -> ValueSet RIC.Top
      | Var _ -> ValueSet RIC.Bottom
      | Mem _ ->
        let mems = extract_memory_variables store in
        if Variable.is_covered ~by:mems var then
          mems
          |> List.fold 
              ~init:(Value.ValueSet RIC.Bottom)
              ~f:(fun acc m -> 
                if Variable.share_addresses m var then
                  (Variable.Map.find_exn store.abstract_store m)
                  |> Value.join acc
                else if Variable.comparable_offsets m var then
                  acc
                else
                  ValueSet RIC.Top)
        else
          ValueSet RIC.Top)

(** [equal store1 store2] returns [true] iff [store1] and [store2]
    represent the same abstract state.

    Equality is semantic rather than structural: variables missing from one
    store are interpreted through [get], so two stores can be equal even if
    their internal memory partitioning or explicit bindings differ. Memory
    write summaries ([store_operations]) must also be equal. *)
let equal (store1 : t) (store2 : t) : bool =
  (store1.abstract_store |> Variable.Map.key_set,
    store2.abstract_store |> Variable.Map.key_set)
  |>> Set.union
  |> Set.fold ~init:true ~f:(fun acc var -> acc && Value.(get store1 ~var = get store2 ~var))
  &&
  RICSet.equal store1.store_operations store2.store_operations
  

(** [extract_global_values store] returns the RIC value of each global variable in [store]. *)
let extract_global_values (store : t) : RIC.t String.Map.t =
  store.abstract_store
  |> Variable.Map.extract_locals_and_globals
  |> List.filter 
    ~f:(fun var -> 
        match var with
        | Variable.Var Var.Global _ -> true
        | _ -> false)
  |> List.map 
    ~f:(fun var -> 
      Variable.to_string var,
      match get store ~var with
      | ValueSet vs -> vs
      | Boolean b -> b.numeric_value
      | Bitfield bf -> RIC.of_bitfield bf)
  |> List.fold
    ~init:String.Map.empty
    ~f:(fun acc (key, data) -> Map.set acc ~key ~data)

(** [extract_argument_values store ~args] maps callee local names to the RIC values
    passed as arguments. *)
let extract_argument_values (store : t) ~(args : Var.t list) : RIC.t String.Map.t =
  (args |> List.length |> List.init ~f:(fun i -> Var.(to_string (Local i))),
   args |> List.rev |> List.map ~f:(fun var -> 
                                      match get store ~var:(Variable.Var var) with
                                      | Value.ValueSet vs -> vs
                                      | Boolean b -> b.numeric_value
                                      | Bitfield bf -> RIC.of_bitfield bf))
  |>> List.fold2_exn
      ~init:String.Map.empty
      ~f:(fun acc key data -> Map.set acc ~key ~data)

(** [_to_string bindings] converts abstract-value bindings to a string. *)
let _to_string : Value.t Variable.Map.t -> string =
  Variable.Map.to_string ~f:Value.to_string

(** [to_string store] converts [store] to a string.

    Unless [show_intermediates] is enabled, only memory, local, global, and return
    variables are shown. *)
let to_string (vs : t) : string = 
  if !Value_set_options.show_intermediates then
    _to_string vs.abstract_store
  else
    vs.abstract_store
    |> Variable.Map.filter_keys
      ~f:(fun var ->
          match var with
          | Mem _ | Var Var.Global _ | Var Var.Local _ | Var Var.Return _ -> true
          | _ -> false)
    |> _to_string

(** [to_string_without_bottoms store] returns a string that omits bottom value sets
    and bottom bitfields. *)
let to_string_without_bottoms (vs : t) : string =
  let restricted = 
    Variable.Map.filter vs.abstract_store 
      ~f:(fun d ->
        match d with
        | Boolean _ -> true
        | ValueSet d -> RIC.(d <> Bottom)
        | Bitfield bf -> Bitfield.(bf <> Bottom)) in
    to_string { abstract_store = restricted; store_operations = vs.store_operations; unreachable = vs.unreachable }

let store_operations_to_string (s : t) : string =
  s.store_operations
  |> RICSet.to_list
  |> List.map ~f:RIC.to_string
  |> String.concat ~sep:"; "

(** [to_dot_string store] formats [store] as a table row for dot output. *)
let to_dot_string (s : t) : string =
    Printf.sprintf "<tr><td></td><td>%s</td></tr>" (to_string s)

(** [update_all store vars value] sets each variable in [vars] to [value]. *)
let update_all (store : t) (vars : Variable.Set.t) (new_value : Value.t) : t =
  { abstract_store = (Variable.Map.update_all store.abstract_store vars new_value);
    store_operations = store.store_operations;
    unreachable = store.unreachable }

(** [make_compatible ~this_store ~relative_to] splits memory variables in
    [this_store] so they align with the memory partition of [relative_to]. *)
let make_compatible ~(this_store : t) ~(relative_to : t) : t = 
  { abstract_store = (Variable.Map.make_compatible ~this:this_store.abstract_store ~relative_to:relative_to.abstract_store ~get:(fun s -> get {abstract_store = s; store_operations = RICSet.empty; unreachable = true}));
    store_operations = this_store.store_operations;
    unreachable = this_store.unreachable }

(** [filter_relative_offsets store rel_offset] keeps only memory variables whose
    symbolic relative offset is [rel_offset]. Non-memory variables are kept. *)
let filter_relative_offsets (store : t) (rel_offset : string) : t =
  let abstract_store =
    Variable.Map.filter_keys store.abstract_store
      ~f:(fun var ->
        match var with
        | Mem RIC {offset = (v, _); _} -> String.(v = rel_offset)
        | _ -> true)
  in
  { abstract_store = abstract_store; store_operations = store.store_operations; unreachable = store.unreachable }

(** [truncate_memory_var store ~var ~accessed_addresses] removes the accessed
    addresses from memory variable [var] and keeps the untouched regions with
    [var]'s previous value. *)
let truncate_memory_var (store : t) ~(var : Variable.t) ~(accessed_addresses : RIC.accessed_memory) : t =
  let vs = get store ~var:var in
  let abstract_store = Variable.Map.remove store.abstract_store var in
  let accessed = accessed_addresses.fully :: accessed_addresses.partially in
  let untouched_variables =
    (if Variable.is_linear_memory var then
      let untouched_addresses =
        match var with
        | Var _ | Accessed | MemorySize -> assert false
        | Mem addr ->
            (List.fold 
              ~init:[addr]
              ~f:(fun acc x -> List.concat (List.map ~f:(fun y -> RIC.remove ~this:x ~from:y) acc)) 
              accessed)
      in
      Variable.Set.of_list (List.map ~f:(fun x -> Variable.Mem x) untouched_addresses)
    else
      assert false) in
  update_all 
    { abstract_store = abstract_store; store_operations = store.store_operations; unreachable = store.unreachable } 
    untouched_variables 
    vs

(** [set store ~var ~vs] sets [var] to [vs].

    When [var] is a memory variable, overlapping memory regions are removed after
    making the store compatible with [var]. Memory updates also discard regions
    with a different symbolic relative offset. *)
let set (store : t) ~(var : Variable.t) ~(vs : Value.t) : t =
  let store = 
    if Variable.is_linear_memory var then 
      let store = 
        make_compatible 
          ~this_store:store 
          ~relative_to:
            { abstract_store = 
                (Variable.Map.empty |> Variable.Map.set ~key:var ~data:vs);
              store_operations = RICSet.empty;
              unreachable = true }
      in
        { abstract_store = 
            Variable.Map.filter_keys 
              ~f:(fun v -> not (Variable.share_addresses v var)) 
              store.abstract_store;
          store_operations = store.store_operations;
          unreachable = store.unreachable }
    else 
      store
  in
  let is_invalid =
    Variable.Map.fold store.abstract_store ~init:false ~f:(fun ~key:k ~data:_ acc ->
      acc ||
      match k, var with
      | Var _, _ | _, Var _ | Accessed, _ | _, Accessed | MemorySize, _ | _, MemorySize -> false
      | Mem _, Mem _ ->
        Variable.(k <> var && share_addresses k var))
  in
  if is_invalid then
    (Log.error "trying to update a memory variable that overlaps with other memory variables"; assert false)
  else
    let store = 
      if Variable.is_linear_memory var then
          filter_relative_offsets store (Variable.get_relative_offset var)
      else
        store
    in
    { abstract_store = Variable.Map.set store.abstract_store ~key:var ~data:vs; 
      store_operations = store.store_operations;
      unreachable = store.unreachable }

(** [remove_pointers_to_top store] removes memory bindings mapped to [Top].

    If no memory binding remains, the whole memory is mapped to [Top]. *)
let remove_pointers_to_top (store : t) : t =
  let store =
    { abstract_store = 
        Variable.Map.filteri store.abstract_store ~f:(fun ~key ~data -> 
          (not (Variable.is_linear_memory key)) || (not (Value.equal (ValueSet RIC.Top) data)));
      store_operations = store.store_operations;
      unreachable = store.unreachable } in
  if List.is_empty (extract_memory_variables store) then
    set store ~var:Variable.entire_memory ~vs:(ValueSet RIC.Top)
  else
    store

(** [widen store1 store2] widens [store1] with respect to [store2]. *)
let widen (store1 : t) (store2 : t) : t =
  if store2.unreachable then
    store1
  else
    let widened_state =
      let store1 = make_compatible ~this_store:store1 ~relative_to:store2 in
      let store2 = make_compatible ~this_store:store2 ~relative_to:store1 in
      let store =
        Variable.Map.merge store1.abstract_store store2.abstract_store ~f:(fun ~key:k v ->
          match k, v with
          | _, `Both (ValueSet x, ValueSet y) -> Some (Value.ValueSet (RIC.widen x ~relative_to:y))
          | _, `Both _ -> Some (Value.ValueSet RIC.Top)
          | Mem _, `Left ValueSet x -> Some (Value.ValueSet (RIC.widen x ~relative_to:RIC.Top))
          | Var _, `Left ValueSet x -> Some (Value.ValueSet (RIC.widen x ~relative_to:RIC.Bottom))
          | Mem _, `Right ValueSet y -> Some (Value.ValueSet (RIC.widen RIC.Top ~relative_to:y))
          | Var _, `Right ValueSet y -> Some (Value.ValueSet (RIC.widen RIC.Bottom ~relative_to:y))
          | _ -> Some (Value.ValueSet RIC.Top))
      in
      { abstract_store = store; 
        store_operations = Set.union store1.store_operations store2.store_operations; 
        unreachable = store1.unreachable }
      |> remove_pointers_to_top 
    in
    if not (equal widened_state store1) then 
      (Intra.narrow_option := true;
      Print_trace.widening store1 store2 widened_state to_string);
    widened_state

(** [bottom] is the bottom store. Its linear memory is explicitly mapped to
    [Bottom]. *)
let bottom : t = 
  { abstract_store = Variable.Map.empty; store_operations = RICSet.empty; unreachable = false }
  |> set ~var:Variable.entire_memory ~vs:(ValueSet RIC.Bottom)

(** [to_top_RIC store var] sets [var] to [RIC.Top] and normalizes top memory bindings. *)
let to_top_RIC (store : t) (var : Variable.t) : t =
  remove_pointers_to_top 
    { abstract_store = (Variable.Map.set store.abstract_store ~key:var ~data:(Value.ValueSet RIC.Top));
      store_operations = store.store_operations;
      unreachable = store.unreachable }

(** [join store1 store2] computes the least upper bound of two stores.

    Memory partitions are made compatible before joining. Missing value-set
    bindings are recovered with [get]. Mixed abstract-value kinds join to [Top]. *)
let join (store1 : t) (store2 : t) : t =
  if store1.unreachable && store2.unreachable then
    bottom
  else if store1.unreachable || equal store1 bottom then
    store2
  else if store2.unreachable || equal store2 bottom then
    store1
  else
    let store1 = make_compatible ~this_store:store1 ~relative_to:store2 in
    let store2 = make_compatible ~this_store:store2 ~relative_to:store1 in
    { abstract_store =
      Variable.Map.merge store1.abstract_store store2.abstract_store ~f:(fun ~key:var value -> 
          match value with
          | `Both (x, y) -> Some (Value.join x y)
          | _ -> Some (Value.join (get store1 ~var) (get store2 ~var)));
      store_operations = RICSet.union store1.store_operations store2.store_operations;
      unreachable = false }
    |> remove_pointers_to_top

(** [truncate_accessed_memory store accessed_addresses] removes accessed regions from
    all memory variables in [store]. *)
let truncate_accessed_memory (accessed_addresses : RIC.accessed_memory) (store : t) : t =
  let memory_vars = extract_memory_variables store in
  match memory_vars with
  | x :: _ when Variable.(entire_memory = x) -> store
  | _ ->
    let store = 
      List.fold 
        ~init:store 
        ~f:(fun store var -> truncate_memory_var store ~var:var ~accessed_addresses:accessed_addresses) memory_vars 
    in
    if List.is_empty (extract_memory_variables store) then 
      set store ~var:Variable.entire_memory ~vs:(ValueSet RIC.Top)
    else
      store

(** [weak_update store ~previous_state ~var ~vs] weakly updates memory variable
    [var] with [vs].

    For each memory region in [previous_state] that overlaps [var], the
    overlapping region is updated with the join of [vs] and the previous value. *)
let weak_update (store : t) ~(previous_state : t) ~(var : Variable.t) ~(vs : Value.t) : t =
  let address = 
    match var with
    | Var _ | Accessed | MemorySize -> assert false
    | Mem address -> address
  in
  let memory_variables = extract_memory_variables previous_state in
  let affected_variables = 
    List.filter ~f:(fun (v, _) -> Variable.(v <> Mem RIC.Bottom))
      (List.map ~f:(fun v -> 
          match v with 
          | Var _ | Accessed | MemorySize -> assert false
          | Mem addr -> (Variable.Mem (RIC.meet addr address)), (get previous_state ~var:v))
        memory_variables)
  in
  let store = List.fold ~init:store
                        ~f:(fun store (v, prev_vs) -> 
                          let vs = Value.join vs prev_vs in
                          set store ~var:v ~vs)
                        affected_variables
  in
  if List.is_empty (extract_memory_variables store) then
    set store ~var:Variable.entire_memory ~vs:(ValueSet RIC.Top)
  else
    store

(** [assign_constant_value store ~const ~to_] assigns the singleton value [const]
    to [to_]. *)
let assign_constant_value (store : t) ~(const : int32) ~(to_ : Variable.t): t =
  set store ~var:to_ ~vs:(ValueSet (RIC.constant const))

(** [copy_value_set store ~from ~to_] copies [from]'s abstract value to [to_].

    Constants are immutable, so copying to a constant is ignored. *)
let copy_value_set (store : t) ~(from : Variable.t) ~(to_ : Variable.t) : t =
  match to_ with
  | Var Var.Const _ -> store
  | _ -> set store ~var:to_ ~vs:(get store ~var:from)

(** [i32_binary_op store ~lhs ~rhs ~result ~symbol ~eval] evaluates a
    binary i32 operation on [lhs] and [rhs], stores the result in [result],
    and emits a trace using [symbol].

    [eval] receives the abstract values of [lhs] and [rhs] and computes the
    resulting abstract value. *)
let binary_op
    (symbol : string)
    (eval : Value.t -> Value.t -> Value.t)
    (store : t)
    (lhs : Variable.t)
    (rhs : Variable.t)
    (result : Variable.t)
  : t =
  let lhs_value = get store ~var:lhs in
  let rhs_value = get store ~var:rhs in
  let result_value = eval lhs_value rhs_value in
  Print_trace.binop lhs lhs_value symbol rhs rhs_value result result_value;
  set store ~var:result ~vs:result_value

(** [i32_add store x y result] assigns [x + y] to [result]. *)
let i32_add = binary_op "+" Value.(+)

(** [i32_sub store ~subtract_this ~from result] assigns [from - subtract_this]
    to [result]. *)
let i32_sub (store : t) ~(subtract_this : Variable.t) ~(from : Variable.t) (result : Variable.t) : t =
  binary_op "-" (fun from subtract_this -> Value.(from - subtract_this)) store from subtract_this result

(** [shift op ric_shift bitfield_shift store x y result] applies the shift
    operation [op] to [y] using [x] as the shift amount, stores the result in
    [result], and emits a trace. *)
let shift 
    (op_string : string)
    (op : Value.t -> Value.t -> Value.t)
    (store : t)
    (x : Variable.t)
    ~(shift_amount : Variable.t)
    (result : Variable.t)
  : t =
  binary_op op_string op store x shift_amount result

(** [shr_u store x y result] assigns the logical right shift [y >>> x] to [result]. *)
let shr_u = shift ">>u" Value.(>>.)

(** [shr_s store x y result] assigns the arithmetic right shift [y >> x] to [result]. *)
let shr_s = shift ">>s" Value.(>>-)

(** [shl store x y result] assigns the left shift [y << x] to [result]. *)
let shl = shift "<<" Value.(<<)

(** [and_ store x y result] assigns the bitwise/logical conjunction
    [x AND y] to [result]. *)
let and_ = binary_op "and" Value.(&.)

(** [or_ store x y result] assigns the bitwise/logical disjunction
    [x OR y] to [result]. *)
let or_ = binary_op "or" Value.(|.)

(** [xor_ store x y result] assigns the bitwise/logical exclusive OR
    [x XOR y] to [result]. *)
let xor_ = binary_op "xor" Value.(<+>)

(** [access_memory store ~addresses] adds [addresses] to the set of accessed
    addresses tracked by the store. *)
let access_memory (store : t) ~(addresses : Value.t) : t =
  let previously_accessed = get store ~var:(Variable.Accessed) in
  let new_accessed_memory = Value.join previously_accessed addresses in
  set store ~var:(Variable.Accessed) ~vs:new_accessed_memory

(** [update_memory_size store ~summary] updates the memory-size value using the
    memory-size information contained in [summary].

    When both values are represented as value sets, their deltas are added.
    Otherwise, the result falls back to a conservative positive-integer value. *)
let update_memory_size (store : t) ~(summary : t) : t =
  match (get store ~var:Variable.MemorySize), (get summary ~var:Variable.MemorySize) with
  | ValueSet (RIC r1), ValueSet (RIC r2) ->
    if Maths.ExtendedInt.(r1.upper_bound <> Infinity || r2.upper_bound <> Infinity) then 
      (Log.error "Unexpected finite memory-size upper bound";
      store |> set ~var:Variable.MemorySize ~vs:(ValueSet RIC.positive_integers))
    else
      store |> set ~var:Variable.MemorySize ~vs:(ValueSet RIC.(RIC r1 + RIC r2))
  | ValueSet (RIC r), _
  | _, ValueSet (RIC r) ->
    store |> set ~var:Variable.MemorySize ~vs:(ValueSet RIC.(RIC r + positive_integers))
  | _ ->
    store |> set ~var:Variable.MemorySize ~vs:(ValueSet RIC.positive_integers)
    
(** [store ~instruction ?annotation_before ?value ?address state] interprets a
    Wasm store instruction.

    The stored value and target address are read either from
    [annotation_before]'s value stack or from the explicit [value] and [address]
    arguments.

    I32 stores keep precise values only for 4-byte stores; other store types
    conservatively write [Top]. Singleton target addresses are updated strongly,
    while non-singleton addresses are updated weakly. Affected memory regions are
    recorded in [store_operations]. *)
let store
    ~(instruction : Memoryop.t) 
    ?(annotation_before : Spec_domain.t option) 
    ?(value : Var.t option)
    ?(address : Var.t option)
    (state : t) 
  : t =
  if state.unreachable then
    state
  else
    let value, address = 
      match annotation_before, value, address with
      | Some annotation_before, None, None -> pop2 (Spec_domain.get_or_fail annotation_before).vstack 
      | None, Some value, Some address -> value, address
      | _ -> assert false
    in
    let vs_address =
      match state |> get ~var:(Variable.Var address) with
      | ValueSet vs -> vs
      | Boolean b -> b.numeric_value
      | Bitfield bf -> RIC.of_bitfield bf
    in
    let vs_value = get state ~var:(Variable.Var value) in
    let size = Memoryop.size instruction in
    begin match instruction with
    | { typ = I32; offset = offset; _ } ->
      let offset = Int32.of_int_exn offset in
      let vs_value = if Int32.(size = 4l) then vs_value else Value.ValueSet RIC.Top in
      let accessed = RIC.accessed ~value_set:(RIC.add_offset vs_address offset) ~size in
      Print_trace.store address vs_address value vs_value offset accessed;
      let added_store_operations = 
        (accessed.fully :: accessed.partially)
        |> RICSet.of_list
        |> RICSet.filter ~f:(fun addr -> RIC.(addr <> Bottom)) 
      in
      let new_state = state |> truncate_accessed_memory accessed in
      let new_state = { new_state with 
                        store_operations = RICSet.union added_store_operations new_state.store_operations } in
      let variable_to_update = Variable.Mem accessed.fully in
      if RIC.((=) Bottom) accessed.fully then
        (* Nothing to update *)
        new_state
      else if RIC.is_singleton accessed.fully then
        (* Strong update *)
        new_state |> set ~var:variable_to_update ~vs:vs_value
      else
        (* Weak update *)
        new_state |> weak_update ~previous_state:state ~var:variable_to_update ~vs:vs_value
    | { typ = F32; offset = offset; _ } ->
      let offset = Int32.of_int_exn offset in
      let accessed = RIC.accessed ~value_set:(RIC.add_offset vs_address offset) ~size in
      Print_trace.store address vs_address value vs_value offset accessed;
      let added_store_operations = 
        (accessed.fully :: accessed.partially)
        |> RICSet.of_list
        |> RICSet.filter ~f:(fun addr -> RIC.(addr <> Bottom)) 
      in
      { state with store_operations = RICSet.union added_store_operations state.store_operations }
      |> truncate_accessed_memory accessed
    | { typ = I64; offset = offset; _ }
    | { typ = F64; offset = offset; _ } ->
      let offset = Int32.of_int_exn offset in
      let accessed = RIC.accessed ~value_set:(RIC.add_offset vs_address offset) ~size in
      Print_trace.store address vs_address value vs_value offset accessed;
      let added_store_operations = 
        (accessed.fully :: accessed.partially)
        |> RICSet.of_list
        |> RICSet.filter ~f:(fun addr -> RIC.(addr <> Bottom))
      in
      { state with store_operations = RICSet.union added_store_operations state.store_operations }
      |> truncate_accessed_memory accessed
    end
    |> remove_pointers_to_top

  (** [load state ~instruction ~annotation_before ~result] interprets a Wasm
    load instruction.

    The load address is taken from the top of the value stack and adjusted by
    the instruction offset. The accessed address is recorded in
    [Variable.Accessed].

    Only 4-byte [i32.load] instructions are considered pointer material. Other
    load types assign [RIC.Top] to [result].

    For [i32.load], the abstract value stored at the computed memory address is
    loaded into [result]. *)
let load 
    (state : t) 
    ~(instruction : Memoryop.t) 
    ~(annotation_before : Spec_domain.t) 
    ~(result : Variable.t)
  : t =
  let size = Memoryop.size instruction in
  let address = pop (Spec_domain.get_or_fail annotation_before).vstack in
  let address_value = get state ~var:(Variable.Var address) in
  let address_plus_offset =
    match address_value with
    | ValueSet vs
    | Boolean {numeric_value = vs; _} -> RIC.(add_offset vs (Int32.of_int_exn instruction.offset))
    | Bitfield bf -> RIC.(add_offset (of_bitfield bf) (Int32.of_int_exn instruction.offset))
  in
  (* Update accessed address *)
  let previously_accessed = get state ~var:Variable.Accessed in
  let state = set state ~var:Variable.Accessed ~vs:(Value.join (ValueSet address_plus_offset) previously_accessed) 
  in
  if Int32.(size <> 4l) || not (Type.equal instruction.typ I32) then
    (* loaded value is not pointer material *)
    (Print_trace.not_i32 ();
    to_top_RIC state result)
  else
    let loaded_value = get state ~var:(Variable.Mem address_plus_offset) in
    Print_trace.load address address_value instruction.offset address_plus_offset result loaded_value;
    set state ~var:result ~vs:loaded_value


(** [unary_op state annotation_before op op_string result] applies the unary
    operation [op] to the value at the top of the stack and stores the result in
    [result]. *)
let unary_op 
    (state : t) 
    (annotation_before : Spec_domain.t) 
    (op : Value.t -> Value.t) 
    (op_string : string)
    (return : Variable.t)
  : t =
  let var = pop (Spec_domain.get_or_fail annotation_before).vstack in
  let vs = get state ~var:(Variable.Var var) in
  let result = op vs in
  Print_trace.unop op_string vs result;
  state |> set ~var:return ~vs:result

(** [memory_copy state annotation_before] interprets Wasm [memory.copy].

    Precise memory-copy semantics are not implemented yet. This function is
    currently unused by the analysis and exists only as a placeholder for a
    future implementation. *)
  let memory_copy (state : t) (annotation_before : Spec_domain.t) : t =
  let _dest, _src, len =
    match (Spec_domain.get_or_fail annotation_before).vstack with
    | dest :: src :: len :: _ -> 
      state |> get ~var:(Variable.Var dest),
      state |> get ~var:(Variable.Var src),
      state |> get ~var:(Variable.Var len)
    | _ -> assert false
  in
  if not (Value.is_singleton len && len |> Value.extract_relative_offset |> String.is_empty) then
    (* Hard to precisely track which addresses have been affected. *)
    { (state |> set ~var:Variable.entire_memory ~vs:Value.top
             |> set ~var:Variable.Accessed ~vs:Value.top)
        with store_operations = RICSet.singleton RIC.Top }
  else (* the size of copied memory is well known *)
    failwith "not yet implemented"

(** [find_value_set cfg ~label ~var] returns [var]'s abstract value before the
    instruction identified by [label]. *)
let find_value_set
    (cfg : t Cfg.t)
    ~(label : Instr.Label.t) 
    ~(var : Var.t)
  : Value.t =
  let block = Cfg.find_enclosing_block_exn cfg label in
  let store =
    match block.content with
    | Data instrs -> 
      (List.find_exn instrs ~f:(fun i ->
        Instr.Label.equal i.label label)).annotation_before
    | Call call_instr -> call_instr.annotation_before
    | _ -> failwith "label does not belong to a data or call block"
  in
  let vs = get store ~var:(Variable.Var var) in
  vs





(*
TTTTTTTTTTTTTTTTTTTTTTTEEEEEEEEEEEEEEEEEEEEEE   SSSSSSSSSSSSSSS TTTTTTTTTTTTTTTTTTTTTTT   SSSSSSSSSSSSSSS 
T:::::::::::::::::::::TE::::::::::::::::::::E SS:::::::::::::::ST:::::::::::::::::::::T SS:::::::::::::::S
T:::::::::::::::::::::TE::::::::::::::::::::ES:::::SSSSSS::::::ST:::::::::::::::::::::TS:::::SSSSSS::::::S
T:::::TT:::::::TT:::::TEE::::::EEEEEEEEE::::ES:::::S     SSSSSSST:::::TT:::::::TT:::::TS:::::S     SSSSSSS
TTTTTT  T:::::T  TTTTTT  E:::::E       EEEEEES:::::S            TTTTTT  T:::::T  TTTTTTS:::::S            
        T:::::T          E:::::E             S:::::S                    T:::::T        S:::::S            
        T:::::T          E::::::EEEEEEEEEE    S::::SSSS                 T:::::T         S::::SSSS         
        T:::::T          E:::::::::::::::E     SS::::::SSSSS            T:::::T          SS::::::SSSSS    
        T:::::T          E:::::::::::::::E       SSS::::::::SS          T:::::T            SSS::::::::SS  
        T:::::T          E::::::EEEEEEEEEE          SSSSSS::::S         T:::::T               SSSSSS::::S 
        T:::::T          E:::::E                         S:::::S        T:::::T                    S:::::S
        T:::::T          E:::::E       EEEEEE            S:::::S        T:::::T                    S:::::S
      TT:::::::TT      EE::::::EEEEEEEE:::::ESSSSSSS     S:::::S      TT:::::::TT      SSSSSSS     S:::::S
      T:::::::::T      E::::::::::::::::::::ES::::::SSSSSS:::::S      T:::::::::T      S::::::SSSSSS:::::S
      T:::::::::T      E::::::::::::::::::::ES:::::::::::::::SS       T:::::::::T      S:::::::::::::::SS 
      TTTTTTTTTTT      EEEEEEEEEEEEEEEEEEEEEE SSSSSSSSSSSSSSS         TTTTTTTTTTT       SSSSSSSSSSSSSSS   
*)


let%test_module "abstract store tests" = (module struct

  let test_label name = Printf.sprintf "%-45s" name
  
  let%test "Abstract_store_tests" =
    print_endline "_______ _____________________ _______\n        Abstract Store Domain        \n------- --------------------- -------\n";
    true
  
  let var1 = Variable.Var (Var.Global 0)
  let var2 = Variable.Var (Var.Local 0)
  let var3 = Variable.mem (0l, Int 0l, Int 0l, ("", 4l))

  let vs =
    {abstract_store = Variable.Map.empty; store_operations = RICSet.empty; unreachable = false }
    |> set ~var:var1 ~vs:(Value.ValueSet (RIC.ric (2l, Int 0l, Int 3l, ("", 4l))))
    |> set ~var:var2 ~vs:(Value.ValueSet (RIC.Bottom))
    |> set ~var:var3 ~vs:(Value.ValueSet (RIC.Top))

  let%test "to_string includes all bindings" =
    let actual = to_string vs in
    let expected =
      "[l0 ↦ ⊥;  g0 ↦ 2[0,3]+4;  mem[4] ↦ ⊤]"
    in
    let passed = String.equal actual expected in
    print_endline (Printf.sprintf "%s %s%s"
      (test_label "[AbstractStore.to_string]")
      actual
      (if passed then "" else Printf.sprintf " (expected %s)" expected));
    passed

  let%test "to_string_without_bottoms omits Bottoms" =
    let actual = to_string_without_bottoms vs in
    let expected =
      "[g0 ↦ 2[0,3]+4;  mem[4] ↦ ⊤]"
    in
    let passed = String.equal actual expected in
    print_endline (Printf.sprintf "%s %s   ->   %s  %s"
      (test_label "[AbstractStore.to_string_without_bottoms]")
      (to_string vs)
      actual
      (if passed then "" else Printf.sprintf " (expected %s)" expected));
    passed

  let%test "join abstract stores" =
    let var1 = Variable.Var (Var.Global 0) in
    let var2 = Variable.Var (Var.Local 0) in
    let store1 =
      Variable.Map.empty
      |> Variable.Map.set ~key:var1 ~data:(Value.ValueSet (RIC.ric (2l, Int 0l, Int 2l, ("", 0l))))
      |> Variable.Map.set ~key:var2 ~data:(Value.ValueSet (RIC.ric (3l, Int 1l, Int 3l, ("", 0l))))
    in
    let store2 =
      Variable.Map.empty
      |> Variable.Map.set ~key:var1 ~data:(Value.ValueSet (RIC.ric (2l, Int 1l, Int 4l, ("", 0l))))
      |> Variable.Map.set ~key:var2 ~data:(Value.ValueSet (RIC.ric (3l, Int 2l, Int 5l, ("", 0l))))
    in
    let joined = join {abstract_store = store1; store_operations = RICSet.empty; unreachable = false}
                    {abstract_store = store2; store_operations = RICSet.empty; unreachable = false} in
    let expected =
      { abstract_store =
          Variable.Map.empty
          |> Variable.Map.set ~key:var1 ~data:(Value.ValueSet (RIC.ric (2l, Int 0l, Int 4l, ("", 0l))))
          |> Variable.Map.set ~key:var2 ~data:(Value.ValueSet (RIC.ric (3l, Int 1l, Int 5l, ("", 0l))));
        store_operations = RICSet.empty;
        unreachable = false }
      |> set ~var:Variable.entire_memory ~vs:(Value.ValueSet (RIC.Top));
    in
    let passed = equal joined expected in
    print_endline (Printf.sprintf "%s %s   join   %s   ->   %s%s"
      (test_label "[AbstractStore.join]")
      (to_string {abstract_store = store1; store_operations = RICSet.empty; unreachable = false})
      (to_string {abstract_store = store2; store_operations = RICSet.empty; unreachable = false})
      (to_string joined)
      (if passed then "" else Printf.sprintf " (expected %s)" (to_string expected)));
    passed

  let%test "join abstract stores 2" =
    let var1 = Variable.Var (Var.Local 0) in
    let var2 = Variable.entire_memory in
    let store1 =
      { abstract_store =
          Variable.Map.empty
          |> Variable.Map.set ~key:var1 ~data:(Value.ValueSet (RIC.ric (0l, Int 0l, Int 0l, ("", 3l))))
          |> Variable.Map.set ~key:var2 ~data:(Value.ValueSet RIC.Top);
        store_operations = RICSet.empty;
        unreachable = false }
    in
    let store2 =
      { abstract_store =
          Variable.Map.empty
          |> Variable.Map.set ~key:var2 ~data:(Value.ValueSet RIC.Bottom);
        store_operations = RICSet.empty;
        unreachable = false }
    in
    let joined = join store1 store2 in
    let expected =
      { abstract_store =
          Variable.Map.empty
          |> Variable.Map.set ~key:var1 ~data:(Value.ValueSet (RIC.ric (0l, Int 0l, Int 0l, ("", 3l))))
          |> Variable.Map.set ~key:var2 ~data:(Value.ValueSet RIC.Top);
        store_operations = RICSet.empty;
        unreachable = false }
    in
    let passed = equal joined expected in
    print_endline (Printf.sprintf "%s %s   join   %s   ->   %s%s"
      (test_label "[AbstractStore.join]")
      (to_string store1)
      (to_string store2)
      (to_string joined)
      (if passed then "" else Printf.sprintf " (expected %s)" (to_string expected)));
    passed

  let%test "join abstract stores 3" =
    let var1 = Variable.Var (Var.Local 0) in
    let var2 = Variable.Var (Var.Local 3) in
    let store1 =
      { abstract_store =
          Variable.Map.empty
          |> Variable.Map.set ~key:var1 ~data:(Value.ValueSet (RIC.ric (0l, Int 0l, Int 0l, ("", 3l))))
          |> Variable.Map.set ~key:var2 ~data:(Value.ValueSet (RIC.ric (0l, Int 0l, Int 0l, ("", 6l))));
        store_operations = RICSet.empty;
        unreachable = false }
    in
    let store2 =
      { abstract_store =
          Variable.Map.empty
          |> Variable.Map.set ~key:var2 ~data:(Value.ValueSet RIC.Bottom);
        store_operations = RICSet.empty;
        unreachable = false }
    in
    let joined = join store1 store2 in
    let expected =
      { abstract_store =
          Variable.Map.empty
          |> Variable.Map.set ~key:var1 ~data:(Value.ValueSet (RIC.ric (0l, Int 0l, Int 0l, ("", 3l))))
          |> Variable.Map.set ~key:var2 ~data:(Value.ValueSet (RIC.ric (0l, Int 0l, Int 0l, ("", 6l))))
          |> Variable.Map.set ~key:Variable.entire_memory ~data:(Value.ValueSet RIC.Top);
        store_operations = RICSet.empty;
        unreachable = false }
    in
    let passed = equal joined expected in
    print_endline (Printf.sprintf "%s %s   join   %s   ->   %s%s"
      (test_label "[AbstractStore.join]")
      (to_string store1)
      (to_string store2)
      (to_string joined)
      (if passed then "" else Printf.sprintf " (expected %s)" (to_string expected)));
    passed

  let%test "truncate memory var retains untouched regions" =
    let addr = RIC.ric (1l, Int 0l, Int 10l, ("", 0l)) in
    let mem_var = Variable.Mem addr in
    let vs = Value.ValueSet (RIC.ric (1l, Int 0l, Int 1l, ("", 0l))) in
    let store = { abstract_store = Variable.Map.singleton mem_var vs; store_operations = RICSet.empty; unreachable = false } in
    let accessed = { RIC.fully = RIC.ric (1l, Int 1l, Int 2l, ("", 0l)); partially = [] } in
    let truncated = truncate_memory_var store ~var:mem_var ~accessed_addresses:accessed in
    let expected = set { abstract_store = (Variable.Map.singleton (Variable.mem (0l, Int 0l, Int 0l, ("", 0l))) vs); store_operations = RICSet.empty; unreachable = false } ~var:(Variable.mem (1l, Int 3l, Int 10l, ("", 0l))) ~vs in
    let passed = equal expected truncated in
    print_endline (Printf.sprintf "%s %s truncate %s (accessed %s) -> %s%s"
      (test_label "[AbstractStore.truncate_memory_var]")
      (to_string store)
      (Variable.to_string mem_var)
      (RIC.to_string accessed.fully)
      (to_string truncated)
      (if passed then "" else Printf.sprintf " (expected %s)" (to_string expected)));
    passed

  let%test "truncate memory 2" =
    let addr = RIC.ric (1l, Int 0l, Int 10l, ("", 0l)) in
    let mem_var = Variable.Mem addr in
    let vs = Value.ValueSet (RIC.ric (1l, Int 0l, Int 1l, ("", 0l))) in
    let store = { abstract_store = Variable.Map.singleton mem_var vs; store_operations = RICSet.empty; unreachable = false } in
    let accessed = { RIC.fully = RIC.ric (1l, Int 1l, Int 2l, ("", 100l)); partially = [] } in
    let truncated = truncate_memory_var store ~var:mem_var ~accessed_addresses:accessed in
    let passed = equal store truncated in
    print_endline (Printf.sprintf "%s %s truncate %s (accessed %s) -> %s%s"
      (test_label "[AbstractStore.truncate_memory_var]")
      (to_string store)
      (Variable.to_string mem_var)
      (RIC.to_string accessed.fully)
      (to_string truncated)
      (if passed then "" else Printf.sprintf " (expected %s)" (to_string store)));
    passed


  let%test "weak_update" =
    let addr1 = RIC.ric (1l, Int 0l, Int 1l, ("", 0l)) in
    let addr2 = RIC.ric (1l, Int 3l, Int 4l, ("", 0l)) in
    let mem1 = Variable.Mem addr1 in
    let mem2 = Variable.Mem addr2 in
    let prev_state =
      { abstract_store =
          Variable.Map.empty
          |> Variable.Map.set ~key:mem1 ~data:(Value.ValueSet (RIC.ric (0l, Int 0l, Int 4l, ("", 42l))))
          |> Variable.Map.set ~key:mem2 ~data:(Value.ValueSet (RIC.ric (0l, Int 2l, Int 6l, ("", 36l))));
        store_operations = RICSet.empty; unreachable = false }
    in
    let store = { abstract_store = Variable.Map.empty; store_operations = RICSet.empty; unreachable = false } in
    let new_val = RIC.ric (0l, Int 2l, Int 6l, ("", 0l)) in
    let new_mem = Variable.mem (1l, Int 1l, Int 3l, ("", 0l)) in
    let updated = weak_update store ~previous_state:prev_state ~var:new_mem ~vs:(ValueSet new_val) in
    let expected =
      { abstract_store =
          Variable.Map.empty
          |> Variable.Map.set ~key:(Variable.mem (0l, Int 0l, Int 0l, ("", 1l))) ~data:(Value.ValueSet (RIC.ric (42l, Int 0l, Int 1l, ("", 0l))))
          |> Variable.Map.set ~key:(Variable.mem (0l, Int 0l, Int 0l, ("", 3l))) ~data:(Value.ValueSet (RIC.ric (36l, Int 0l, Int 1l, ("", 0l))));
        store_operations = RICSet.empty; unreachable = false }
    in
    let passed = equal expected updated in
    print_endline (Printf.sprintf "%s %s weak_update %s = %s -> %s%s"
      (test_label "[AbstractStore.weak_update]")
      (to_string store)
      (to_string prev_state)
      (Variable.to_string new_mem)
      (to_string updated)
      (if passed then "" else Printf.sprintf " (expected %s)" (to_string expected)));
    passed

  let%test "get: get memory var in bottom state" =
    let var = Variable.mem (1l, Int 0l, Int 2l, ("", 0l)) in
    let store = bottom in
    let result = get store ~var:var in
    let expected = Value.ValueSet RIC.Bottom in
    let passed = Value.equal result expected in
    print_endline (Printf.sprintf "%s get %s[%s] -> %s%s"
      (test_label "[AbstractStore.get]")
      (to_string store)
      (Variable.to_string var)
      (Value.to_string result)
      (if passed then "" else Printf.sprintf " (expected %s)" (Value.to_string expected)));
    passed

  let%test "get: variable not in store but covered by two others" =
    let var1 = Variable.mem (1l, Int 0l, Int 2l, ("", 0l)) in
    let var2 = Variable.mem (1l, Int 3l, Int 5l, ("", 0l)) in
    let target = Variable.mem (1l, Int 2l, Int 3l, ("", 0l)) in
    let vs1 = RIC.ric (1l, Int 0l, Int 2l, ("", 0l)) in
    let vs1' = Value.ValueSet vs1 in
    let vs2 = RIC.ric (2l, Int 3l, Int 5l, ("", 0l)) in
    let vs2' = Value.ValueSet vs2 in
    let store =
      { abstract_store = Variable.Map.empty; store_operations = RICSet.empty; unreachable = false }
      |> set ~var:var1 ~vs:vs1'
      |> set ~var:var2 ~vs:vs2'
    in
    let result = get store ~var:target in
    let expected = Value.ValueSet (RIC.join vs1 vs2) in
    let passed = Value.equal result expected in
    print_endline (Printf.sprintf "%s get %s[%s] -> %s%s"
      (test_label "[AbstractStore.get]")
      (to_string store)
      (Variable.to_string target)
      (Value.to_string result)
      (if passed then "" else Printf.sprintf " (expected %s)" (Value.to_string expected)));
    passed

  let%test "get: variable not in store and not covered by others" =
    let var1 = Variable.mem (1l, Int 0l, Int 2l, ("", 0l)) in
    let var2 = Variable.mem (1l, Int 4l, Int 5l, ("", 0l)) in
    let target = Variable.mem (1l, Int 2l, Int 3l, ("", 0l)) in
    let vs1 = RIC.ric (1l, Int 0l, Int 2l, ("", 0l)) in
    let vs1' = Value.ValueSet (vs1) in
    let vs2 = RIC.ric (2l, Int 3l, Int 5l, ("", 0l)) in
    let vs2' = Value.ValueSet (vs2) in
    let store =
      { abstract_store = Variable.Map.empty; store_operations = RICSet.empty; unreachable = false }
      |> set ~var:var1 ~vs:vs1'
      |> set ~var:var2 ~vs:vs2'
    in
    let result = get store ~var:target in
    let expected = Value.ValueSet RIC.Top in
    let passed = Value.equal result expected in
    print_endline (Printf.sprintf "%s get %s[%s] -> %s%s"
      (test_label "[AbstractStore.get]")
      (to_string store)
      (Variable.to_string target)
      (Value.to_string result)
      (if passed then "" else Printf.sprintf " (expected %s)" (Value.to_string expected)));
    passed

  let%test "make two stores compatible to join" =
    let m1 = Variable.Mem (RIC.ric (1l, Int 1l, Int 4l, ("", 0l))) in
    let vs1 = RIC.ric (0l, Int 0l, Int 0l, ("", 42l)) in
    let m2 = Variable.Mem (RIC.ric (2l, Int 0l, Int 4l, ("", 0l))) in
    let vs2 = RIC.ric (0l, Int 0l, Int 0l, ("", 36l)) in
    let empty_store = { abstract_store = Variable.Map.empty; store_operations = RICSet.empty; unreachable = false } in
    let store1 = set empty_store ~var:m1 ~vs:(Value.ValueSet vs1) in
    let store2 = set empty_store ~var:m2 ~vs:(Value.ValueSet vs2) in
    let store1c = make_compatible ~this_store:store1 ~relative_to:store2 in
    let store2c = make_compatible ~this_store:store2 ~relative_to:store1 in
    let joined = join store1c store2c in
    let expected = { abstract_store = Variable.Map.empty; store_operations = RICSet.empty; unreachable = false }
      |> set ~var:(Variable.Mem (RIC.ric (2l, Int 0l, Int 1l, ("", 2l)))) ~vs:(Value.ValueSet (RIC.ric (6l, Int 0l, Int 1l, ("", 36l)))) in
    let passed = equal joined expected in
    print_endline (Printf.sprintf "%s %s make_compatible %s -> %s%s"
      (test_label "[AbstractStore.make_compatible]")
      (to_string store1)
      (to_string store2)
      (to_string joined)
      (if passed then "" else Printf.sprintf " (expected %s)" (to_string expected)));
    passed

  let%test "join: Mem variables with incompatible offsets" =
    let mem1 = Variable.Mem (RIC.ric (1l, Int 0l, Int 4l, ("stack", 0l))) in
    let mem2 = Variable.Mem (RIC.ric (1l, Int 0l, Int 4l, ("heap", 0l))) in
    let vs1 = RIC.ric (1l, Int 0l, Int 4l, ("stack", 0l)) in
    let vs2 = RIC.ric (1l, Int 0l, Int 4l, ("heap", 0l)) in
    let store1 = { abstract_store = Variable.Map.singleton mem1 (Value.ValueSet vs1); store_operations = RICSet.empty; unreachable = false } in
    let store2 = { abstract_store = Variable.Map.singleton mem2 (Value.ValueSet vs2); store_operations = RICSet.empty; unreachable = false }  in
    let joined = join store1 store2 in
    print_endline "[join: incompatible Mem offsets]";
    print_endline ("\tstore1: " ^ to_string store1);
    print_endline ("\tstore2: " ^ to_string store2);
    print_endline ("\tjoined: " ^ to_string joined);
    Value.equal (Value.ValueSet RIC.Top) (get joined ~var:mem2)

  let%test "set: update Mem[(\"b\" + 0)] in presence of Mem[(\"a\" + 0)]" =
    let var_a = Variable.mem (0l, Int 0l, Int 0l, ("a", 0l)) in
    let var_b = Variable.mem (0l, Int 0l, Int 0l, ("b", 0l)) in
    let vs_a = RIC.ric (1l, Int 0l, Int 2l, ("a", 0l)) in
    let vs_b = RIC.ric (1l, Int 0l, Int 2l, ("b", 0l)) in
    let store = { abstract_store = Variable.Map.singleton var_a (Value.ValueSet vs_a); store_operations = RICSet.empty; unreachable = false } in
    let updated_store = set store ~var:var_b ~vs:(Value.ValueSet vs_b) in
    print_endline "[set: different symbolic offsets]";
    print_endline ("\tinitial store: " ^ to_string store);
    print_endline ("\tupdating value-set of variable " ^ Variable.to_string var_b);
    print_endline ("\tupdated store: " ^ to_string updated_store);
    Variable.Map.length updated_store.abstract_store = 1 &&
    Value.equal (get updated_store ~var:var_a) (Value.ValueSet RIC.Top) &&
    Value.equal (get updated_store ~var:var_b) (Value.ValueSet vs_b)

  let%test "set: set memory variable in bottom state" =
    let var = Variable.mem (1l, Int 0l, Int 2l, ("", 0l)) in
    let vs = RIC.ric (1l, Int 0l, Int 2l, ("", 0l)) in
    let store = bottom |> set ~var:var ~vs:(Value.ValueSet vs) in
    print_endline ("[set: bottom state]\n\tInitial state: " ^ to_string bottom ^ "\n\tstore after setting a value in bottom store: " ^ to_string store ^ "\n\tvariable to set: " ^ Variable.to_string var
      ^ "\n\tvalue-set to set it to: " ^ RIC.to_string vs);
    true

  let%test "set: set memory variable in bottom state (relative address)" =
    let var = Variable.mem (1l, Int 0l, Int 2l, ("a", 0l)) in
    let vs = RIC.ric (1l, Int 0l, Int 2l, ("", 0l)) in
    let store = bottom |> set ~var:var ~vs:(Value.ValueSet vs) in
    print_endline ("[set: bottom state (relative address)]\n\tInitial state: " ^ to_string bottom ^ "\n\tstore after setting a value in bottom store: " ^ to_string store ^ "\n\tvariable to set: " ^ Variable.to_string var
      ^ "\n\tvalue-set to set it to: " ^ RIC.to_string vs);
    true

  let%test "get: exact variable binding" =
    let var = Variable.Var (Var.Local 1) in
    let expected = Value.ValueSet (RIC.ric (2l, Int 0l, Int 4l, ("", 0l))) in
    let store =
      { abstract_store = Variable.Map.empty; store_operations = RICSet.empty; unreachable = false }
      |> set ~var ~vs:expected
    in
    let actual = get store ~var in
    let passed = Value.equal actual expected in
    print_endline (Printf.sprintf "%s get store:[%s]  var:[%s] -> %s%s"
      (test_label "[AbstractStore.get]")
      (to_string store)
      (Variable.to_string var)
      (Value.to_string actual)
      (if passed then "" else Printf.sprintf " (expected %s)" (Value.to_string expected)));
    passed

    let%test "get: i32 constant defaults to singleton" =
      let var = Variable.Var (Var.Const (Prim_value.I32 42l)) in
      let store = { abstract_store = Variable.Map.empty; store_operations = RICSet.empty; unreachable = false } in
      let actual = get store ~var in
      let expected = Value.ValueSet (RIC.constant 42l) in
      let passed = Value.equal actual expected in
      print_endline (Printf.sprintf "%s get %s[%s] -> %s%s"
        (test_label "[AbstractStore.get]")
        (to_string store)
        (Variable.to_string var)
        (Value.to_string actual)
        (if passed then "" else Printf.sprintf " (expected %s)" (Value.to_string expected)));
      passed

  let%test "copy_value_set: copies local value" =
    let from = Variable.Var (Var.Local 0) in
    let to_ = Variable.Var (Var.Local 1) in
    let expected = Value.ValueSet (RIC.ric (4l, Int 0l, Int 3l, ("", 2l))) in
    let store =
      { abstract_store = Variable.Map.empty; store_operations = RICSet.empty; unreachable = false }
      |> set ~var:from ~vs:expected
    in
    let actual_store = copy_value_set store ~from ~to_ in
    let actual = get actual_store ~var:to_ in
    let passed = Value.equal actual expected in
    print_endline (Printf.sprintf "%s copy [%s] from %s to %s -> [%s]%s"
      (test_label "[AbstractStore.copy_value_set]")
      (to_string store)
      (Variable.to_string from)
      (Variable.to_string to_)
      (to_string actual_store)
      (if passed then "" else Printf.sprintf " (expected %s)" (Value.to_string expected)));
    passed

  let%test "copy_value_set: ignores copy to constant" =
    let from = Variable.Var (Var.Local 0) in
    let to_ = Variable.Var (Var.Const (Prim_value.I32 42l)) in
    let value = Value.ValueSet (RIC.ric (4l, Int 0l, Int 3l, ("", 2l))) in
    let store =
      { abstract_store = Variable.Map.empty; store_operations = RICSet.empty; unreachable = false }
      |> set ~var:from ~vs:value
    in
    let actual_store = copy_value_set store ~from ~to_ in
    let passed = equal actual_store store in
    print_endline (Printf.sprintf "%s copy %s from %s to %s   ->   %s%s"
      (test_label "[AbstractStore.copy_value_set]")
      (to_string store)
      (Variable.to_string from)
      (Variable.to_string to_)
      (to_string actual_store)
      (if passed then "" else Printf.sprintf " (expected %s)" (to_string store)));
    passed

  

  let%test "assign_constant_value: assigns singleton to local" =
    let to_ = Variable.Var (Var.Local 2) in
    let store = { abstract_store = Variable.Map.empty; store_operations = RICSet.empty; unreachable = false } in
    let actual_store = assign_constant_value store ~const:42l ~to_ in
    let actual = get actual_store ~var:to_ in
    let expected = Value.ValueSet (RIC.constant 42l) in
    let passed = Value.equal actual expected in
    print_endline (Printf.sprintf "%s assign 42 to %s in %s   ->   %s%s"
      (test_label "[AbstractStore.assign_constant_value]")
      (Variable.to_string to_)
      (to_string store)
      (to_string actual_store)
      (if passed then "" else Printf.sprintf " (expected %s)" (Value.to_string expected)));
    passed

  let%test "copy_value_set: copies Bottom from out-of-scope variable" =
    let from = Variable.Var (Var.Local 99) in
    let to_ = Variable.Var (Var.Local 1) in
    let store = { abstract_store = Variable.Map.empty; store_operations = RICSet.empty; unreachable = false } in
    let actual_store = copy_value_set store ~from ~to_ in
    let actual = get actual_store ~var:to_ in
    let expected = Value.ValueSet RIC.Bottom in
    let passed = Value.equal actual expected in
    print_endline (Printf.sprintf "%s copy %s from %s to %s   ->   %s%s"
      (test_label "[AbstractStore.copy_value_set]")
      (to_string store)
      (Variable.to_string from)
      (Variable.to_string to_)
      (to_string actual_store)
      (if passed then "" else Printf.sprintf " (expected %s)" (Value.to_string expected)));
    passed

  let%test "i32_add: local plus constant" =
    let x = Variable.Var (Var.Local 0) in
    let y = Variable.Var (Var.Const (Prim_value.I32 8l)) in
    let result = Variable.Var (Var.Local 1) in
    let x_value = Value.ValueSet (RIC.ric (2l, Int 0l, Int 3l, ("", 4l))) in
    let store =
      { abstract_store = Variable.Map.empty; store_operations = RICSet.empty; unreachable = false }
      |> set ~var:x ~vs:x_value
    in
    let actual_store = i32_add store x y result in
    let actual = get actual_store ~var:result in
    let expected = Value.ValueSet (RIC.ric (2l, Int 0l, Int 3l, ("", 12l))) in
    let passed = Value.equal actual expected in
    print_endline (Printf.sprintf "%s %s + %s in %s   ->   %s%s"
      (test_label "[AbstractStore.i32_add]")
      (Variable.to_string x)
      (Variable.to_string y)
      (to_string store)
      (to_string actual_store)
      (if passed then "" else Printf.sprintf " (expected %s)" (Value.to_string expected)));
    passed

  let%test "i32_sub: local minus constant" =
    let from = Variable.Var (Var.Local 0) in
    let subtract_this = Variable.Var (Var.Const (Prim_value.I32 3l)) in
    let result = Variable.Var (Var.Local 1) in
    let from_value = Value.ValueSet (RIC.ric (2l, Int 2l, Int 5l, ("", 0l))) in
    let store =
      { abstract_store = Variable.Map.empty; store_operations = RICSet.empty; unreachable = false }
      |> set ~var:from ~vs:from_value
    in
    let actual_store = i32_sub store ~subtract_this ~from result in
    let actual = get actual_store ~var:result in
    let expected = Value.ValueSet (RIC.ric (2l, Int 2l, Int 5l, ("", -3l))) in
    let passed = Value.equal actual expected in
    print_endline (Printf.sprintf "%s %s - %s in %s   ->   %s%s"
      (test_label "[AbstractStore.i32_sub]")
      (Variable.to_string from)
      (Variable.to_string subtract_this)
      (to_string store)
      (to_string actual_store)
      (if passed then "" else Printf.sprintf " (expected %s)" (Value.to_string expected)));
    passed

  let%test "i32_add: two constants" =
    let x = Variable.Var (Var.Const (Prim_value.I32 10l)) in
    let y = Variable.Var (Var.Const (Prim_value.I32 32l)) in
    let result = Variable.Var (Var.Local 0) in
    let store = { abstract_store = Variable.Map.empty; store_operations = RICSet.empty; unreachable = false } in
    let actual_store = i32_add store x y result in
    let actual = get actual_store ~var:result in
    let expected = Value.ValueSet (RIC.constant 42l) in
    let passed = Value.equal actual expected in
    print_endline (Printf.sprintf "%s %s + %s in %s   ->   %s%s"
      (test_label "[AbstractStore.i32_add]")
      (Variable.to_string x)
      (Variable.to_string y)
      (to_string store)
      (to_string actual_store)
      (if passed then "" else Printf.sprintf " (expected %s)" (Value.to_string expected)));
    passed

  let%test "i32_sub: two constants" =
    let from = Variable.Var (Var.Const (Prim_value.I32 50l)) in
    let subtract_this = Variable.Var (Var.Const (Prim_value.I32 8l)) in
    let result = Variable.Var (Var.Local 0) in
    let store = { abstract_store = Variable.Map.empty; store_operations = RICSet.empty; unreachable = false } in
    let actual_store = i32_sub store ~subtract_this ~from result in
    let actual = get actual_store ~var:result in
    let expected = Value.ValueSet (RIC.constant 42l) in
    let passed = Value.equal actual expected in
    print_endline (Printf.sprintf "%s %s - %s in %s   ->   %s%s"
      (test_label "[AbstractStore.i32_sub]")
      (Variable.to_string from)
      (Variable.to_string subtract_this)
      (to_string store)
      (to_string actual_store)
      (if passed then "" else Printf.sprintf " (expected %s)" (Value.to_string expected)));
    passed

  let%test "access_memory: first access from empty store" =
    let store = { abstract_store = Variable.Map.empty; store_operations = RICSet.empty; unreachable = false } in
    let addresses = Value.ValueSet (RIC.constant 4l) in
    let actual_store = access_memory store ~addresses in
    let actual = get actual_store ~var:Variable.Accessed in
    let expected = addresses in
    let passed = Value.equal actual expected in
    print_endline (Printf.sprintf "%s access %s in %s   ->   %s%s"
      (test_label "[AbstractStore.access_memory]")
      (Value.to_string addresses)
      (to_string store)
      (Value.to_string actual)
      (if passed then "" else Printf.sprintf " (expected %s)" (Value.to_string expected)));
    passed

  let%test "update_memory_size: singleton plus singleton" =
    let store =
      { abstract_store = Variable.Map.empty; store_operations = RICSet.empty; unreachable = false }
      |> set ~var:Variable.MemorySize ~vs:(Value.ValueSet (RIC.constant 5l))
    in
    let summary =
      { abstract_store = Variable.Map.empty; store_operations = RICSet.empty; unreachable = false }
      |> set ~var:Variable.MemorySize ~vs:(Value.ValueSet (RIC.constant 2l))
    in
    let actual_store = update_memory_size store ~summary in
    let actual = get actual_store ~var:Variable.MemorySize in
    let expected = Value.ValueSet (RIC.positive_integers) in
    let passed = Value.equal actual expected in
    print_endline (Printf.sprintf "%s (size=%s) + summary (size=%s)   ->   %s%s"
      (test_label "[AbstractStore.update_memory_size]")
      (Value.to_string (get store ~var:Variable.MemorySize))
      (Value.to_string (get summary ~var:Variable.MemorySize))
      (Value.to_string actual)
      (if passed then "" else Printf.sprintf " (expected %s)" (Value.to_string expected)));
    passed

  let%test "extract_global_values: extracts globals only" =
    let global0 = Variable.Var (Var.Global 0) in
    let global1 = Variable.Var (Var.Global 1) in
    let local0 = Variable.Var (Var.Local 0) in

    let value0 = RIC.constant 10l in
    let value1 = RIC.ric (2l, Int 0l, Int 3l, ("", 4l)) in

    let store =
      { abstract_store = Variable.Map.empty; store_operations = RICSet.empty; unreachable = false }
      |> set ~var:global0 ~vs:(Value.ValueSet value0)
      |> set ~var:global1 ~vs:(Value.ValueSet value1)
      |> set ~var:local0 ~vs:(Value.ValueSet (RIC.constant 99l))
    in

    let actual = extract_global_values store in
    let expected =
      String.Map.empty
      |> Map.set ~key:(Variable.to_string global0) ~data:value0
      |> Map.set ~key:(Variable.to_string global1) ~data:value1
    in

    let passed = Map.equal RIC.equal actual expected in
    print_endline (Printf.sprintf "%s %s   ->   %s%s"
      (test_label "[AbstractStore.extract_global_values]")
      (to_string store)
      (Map.to_alist actual
       |> List.map ~f:(fun (k, v) -> Printf.sprintf "%s ↦ %s" k (RIC.to_string v))
       |> String.concat ~sep:"; ")
      (if passed then "" else Printf.sprintf " (expected %s)"
        (Map.to_alist expected
         |> List.map ~f:(fun (k, v) -> Printf.sprintf "%s ↦ %s" k (RIC.to_string v))
         |> String.concat ~sep:"; ")));
    passed

  let%test "extract_argument_values: maps caller args to callee locals" =
    let arg0 = Var.Local 4 in
    let arg1 = Var.Local 7 in

    let value0 = RIC.constant 10l in
    let value1 = RIC.constant 20l in

    let store =
      { abstract_store = Variable.Map.empty; store_operations = RICSet.empty; unreachable = false }
      |> set ~var:(Variable.Var arg0) ~vs:(Value.ValueSet value0)
      |> set ~var:(Variable.Var arg1) ~vs:(Value.ValueSet value1)
    in

    let actual = extract_argument_values store ~args:[arg0; arg1] in

    let expected =
      String.Map.empty
      |> Map.set ~key:(Var.to_string (Var.Local 0)) ~data:value1
      |> Map.set ~key:(Var.to_string (Var.Local 1)) ~data:value0
    in

    let passed = Map.equal RIC.equal actual expected in
    print_endline (Printf.sprintf "%s %s with args [%s; %s]   ->   %s%s"
      (test_label "[AbstractStore.extract_argument_values]")
      (to_string store)
      (Var.to_string arg0)
      (Var.to_string arg1)
      (Map.to_alist actual
       |> List.map ~f:(fun (k, v) -> Printf.sprintf "%s ↦ %s" k (RIC.to_string v))
       |> String.concat ~sep:"; ")
      (if passed then "" else Printf.sprintf " (expected %s)"
        (Map.to_alist expected
         |> List.map ~f:(fun (k, v) -> Printf.sprintf "%s ↦ %s" k (RIC.to_string v))
         |> String.concat ~sep:"; ")));
    passed

  let%test "access_memory: joins multiple accesses" =
    let store = { abstract_store = Variable.Map.empty; store_operations = RICSet.empty; unreachable = false } in
    let addresses1 = Value.ValueSet (RIC.constant 4l) in
    let addresses2 = Value.ValueSet (RIC.constant 8l) in

    let actual_store =
      store
      |> access_memory ~addresses:addresses1
      |> access_memory ~addresses:addresses2
    in

    let actual = get actual_store ~var:Variable.Accessed in
    let expected = Value.join addresses1 addresses2 in
    let passed = Value.equal actual expected in

    print_endline (Printf.sprintf "%s access %s then %s   ->   %s%s"
      (test_label "[AbstractStore.access_memory]")
      (Value.to_string addresses1)
      (Value.to_string addresses2)
      (Value.to_string actual)
      (if passed then "" else Printf.sprintf " (expected %s)" (Value.to_string expected)));
    passed

  let%test "remove_pointers_to_top: replaces only-Top memory with entire memory Top" =
    let mem = Variable.mem (1l, Int 0l, Int 3l, ("", 0l)) in
    let local = Variable.Var (Var.Local 0) in
    let local1 = Variable.Var (Var.Local 1) in
    let local_value = Value.ValueSet (RIC.constant 42l) in

    let store =
      { abstract_store = Variable.Map.empty; store_operations = RICSet.empty; unreachable = false }
      |> set ~var:mem ~vs:(Value.ValueSet RIC.Top)
      |> set ~var:local ~vs:local_value
      |> set ~var:local1 ~vs:(Value.ValueSet RIC.Top)
    in

    let actual = remove_pointers_to_top store in

    let expected =
      { abstract_store = Variable.Map.empty; store_operations = RICSet.empty; unreachable = false }
      |> set ~var:Variable.entire_memory ~vs:(Value.ValueSet RIC.Top)
      |> set ~var:local ~vs:local_value
      |> set ~var:local1 ~vs:(Value.ValueSet RIC.Top)
    in

    let passed = equal actual expected in
    print_endline (Printf.sprintf "%s %s   ->   %s%s"
      (test_label "[AbstractStore.remove_pointers_to_top]")
      (to_string store)
      (to_string actual)
      (if passed then "" else Printf.sprintf " (expected %s)" (to_string expected)));
    passed

  let%test "remove_pointers_to_top: keeps non-Top memory bindings" =
    let mem_top = Variable.mem (1l, Int 0l, Int 3l, ("", 0l)) in
    let mem_precise = Variable.mem (1l, Int 4l, Int 7l, ("", 0l)) in
    let precise_value = Value.ValueSet (RIC.constant 42l) in

    let store =
      { abstract_store = Variable.Map.empty; store_operations = RICSet.empty; unreachable = false }
      |> set ~var:mem_top ~vs:(Value.ValueSet RIC.Top)
      |> set ~var:mem_precise ~vs:precise_value
    in

    let actual = remove_pointers_to_top store in

    let expected =
      { abstract_store = Variable.Map.empty; store_operations = RICSet.empty; unreachable = false }
      |> set ~var:mem_precise ~vs:precise_value
    in

    let passed = equal actual expected in
    print_endline (Printf.sprintf "%s %s   ->   %s%s"
      (test_label "[AbstractStore.remove_pointers_to_top]")
      (to_string store)
      (to_string actual)
      (if passed then "" else Printf.sprintf " (expected %s)" (to_string expected)));
    passed

  let%test "to_top_RIC: local becomes Top" =
    let local = Variable.Var (Var.Local 0) in
    let other = Variable.Var (Var.Local 1) in

    let store =
      { abstract_store = Variable.Map.empty; store_operations = RICSet.empty; unreachable = false }
      |> set ~var:local ~vs:(Value.ValueSet (RIC.constant 42l))
      |> set ~var:other ~vs:(Value.ValueSet (RIC.constant 7l))
    in

    let actual = to_top_RIC store local in

    let expected =
      { abstract_store = Variable.Map.empty; store_operations = RICSet.empty; unreachable = false }
      |> set ~var:local ~vs:(Value.ValueSet RIC.Top)
      |> set ~var:other ~vs:(Value.ValueSet (RIC.constant 7l))
      |> set ~var:Variable.entire_memory ~vs:(Value.ValueSet RIC.Top)
    in

    let passed = equal actual expected in
    print_endline (Printf.sprintf "%s %s, var = %s   ->   %s%s"
      (test_label "[AbstractStore.to_top_RIC]")
      (to_string store)
      (Variable.to_string local)
      (to_string actual)
      (if passed then "" else Printf.sprintf " (expected %s)" (to_string expected)));
    passed

  let%test "truncate_accessed_vars: removes accessed addresses from memory bindings" =
    let mem = Variable.mem (1l, Int 0l, Int 5l, ("", 0l)) in
    let value = Value.ValueSet (RIC.constant 42l) in

    let store =
      { abstract_store = Variable.Map.empty; store_operations = RICSet.empty; unreachable = false }
      |> set ~var:mem ~vs:value
    in

    let accessed =
      { RIC.fully = RIC.ric (1l, Int 2l, Int 3l, ("", 0l));
        partially = [] }
    in

    let actual =     truncate_accessed_memory accessed store in

    let expected =
      { abstract_store = Variable.Map.empty; store_operations = RICSet.empty; unreachable = false }
      |> set ~var:(Variable.mem (1l, Int 0l, Int 1l, ("", 0l))) ~vs:value
      |> set ~var:(Variable.mem (1l, Int 4l, Int 5l, ("", 0l))) ~vs:value
    in

    let passed = equal actual expected in
    print_endline (Printf.sprintf "%s %s accessed %s   ->   %s%s"
      (test_label "[AbstractStore.truncate_accessed_vars]")
      (to_string store)
      (RIC.to_string accessed.fully)
      (to_string actual)
      (if passed then "" else Printf.sprintf " (expected %s)" (to_string expected)));
    passed

  let%test "truncate_accessed_vars: entire memory stays unchanged" =
    let store =
      { abstract_store = Variable.Map.empty; store_operations = RICSet.empty; unreachable = false }
      |> set ~var:Variable.entire_memory ~vs:(Value.ValueSet RIC.Top)
    in

    let accessed =
      { RIC.fully = RIC.ric (1l, Int 2l, Int 3l, ("", 0l));
        partially = [] }
    in

    let actual = truncate_accessed_memory accessed store in
    let expected = store in

    let passed = equal actual expected in
    print_endline (Printf.sprintf "%s %s accessed %s   ->   %s%s"
      (test_label "[AbstractStore.truncate_accessed_vars]")
      (to_string store)
      (RIC.to_string accessed.fully)
      (to_string actual)
      (if passed then "" else Printf.sprintf " (expected %s)" (to_string expected)));
    passed

  let%test "truncate_accessed_vars: removes fully and partially accessed addresses" =
    let mem = Variable.mem (1l, Int 0l, Int 7l, ("", 0l)) in
    let value = Value.ValueSet (RIC.constant 42l) in

    let store =
      { abstract_store = Variable.Map.empty; store_operations = RICSet.empty; unreachable = false }
      |> set ~var:mem ~vs:value
    in

    let accessed =
      { RIC.fully = RIC.ric (1l, Int 2l, Int 3l, ("", 0l));
        partially = [RIC.ric (1l, Int 5l, Int 5l, ("", 0l))] }
    in

    let actual = truncate_accessed_memory accessed store in

    let expected =
      { abstract_store = Variable.Map.empty; store_operations = RICSet.empty; unreachable = false }
      |> set ~var:(Variable.mem (1l, Int 0l, Int 1l, ("", 0l))) ~vs:value
      |> set ~var:(Variable.mem (0l, Int 0l, Int 0l, ("", 4l))) ~vs:value
      |> set ~var:(Variable.mem (1l, Int 6l, Int 7l, ("", 0l))) ~vs:value
    in

    let passed = equal actual expected in
    print_endline (Printf.sprintf "%s %s accessed fully %s, partially [%s]   ->   %s%s"
      (test_label "[AbstractStore.truncate_accessed_vars]")
      (to_string store)
      (RIC.to_string accessed.fully)
      (accessed.partially |> List.map ~f:RIC.to_string |> String.concat ~sep:"; ")
      (to_string actual)
      (if passed then "" else Printf.sprintf " (expected %s)" (to_string expected)));
    passed

  let%test "set: memory update keeps untouched regions" =
    let old_mem = Variable.mem (1l, Int 0l, Int 5l, ("", 0l)) in
    let new_mem = Variable.mem (1l, Int 2l, Int 3l, ("", 0l)) in

    let old_value = Value.ValueSet (RIC.constant 42l) in
    let new_value = Value.ValueSet (RIC.constant 99l) in

    let store =
      { abstract_store = Variable.Map.empty; store_operations = RICSet.empty; unreachable = false }
      |> set ~var:old_mem ~vs:old_value
    in

    let actual = set store ~var:new_mem ~vs:new_value in

    let expected =
      { abstract_store = Variable.Map.empty; store_operations = RICSet.empty; unreachable = false }
      |> set ~var:(Variable.mem (1l, Int 0l, Int 1l, ("", 0l))) ~vs:old_value
      |> set ~var:new_mem ~vs:new_value
      |> set ~var:(Variable.mem (1l, Int 4l, Int 5l, ("", 0l))) ~vs:old_value
    in

    let passed = equal actual expected in
    print_endline (Printf.sprintf "%s %s set %s ↦ %s   ->   %s%s"
      (test_label "[AbstractStore.set]")
      (to_string store)
      (Variable.to_string new_mem)
      (Value.to_string new_value)
      (to_string actual)
      (if passed then "" else Printf.sprintf " (expected %s)" (to_string expected)));
    passed

  let%test "set: memory update fully replaces covered region" =
    let old_mem = Variable.mem (1l, Int 0l, Int 5l, ("", 0l)) in
    let new_mem = Variable.mem (1l, Int 0l, Int 5l, ("", 0l)) in

    let old_value = Value.ValueSet (RIC.constant 42l) in
    let new_value = Value.ValueSet (RIC.constant 99l) in

    let store =
      { abstract_store = Variable.Map.empty; store_operations = RICSet.empty; unreachable = false }
      |> set ~var:old_mem ~vs:old_value
    in

    let actual = set store ~var:new_mem ~vs:new_value in

    let expected =
      { abstract_store = Variable.Map.empty; store_operations = RICSet.empty; unreachable = false }
      |> set ~var:new_mem ~vs:new_value
    in

    let passed = equal actual expected in
    print_endline (Printf.sprintf "%s %s set %s ↦ %s   ->   %s%s"
      (test_label "[AbstractStore.set]")
      (to_string store)
      (Variable.to_string new_mem)
      (Value.to_string new_value)
      (to_string actual)
      (if passed then "" else Printf.sprintf " (expected %s)" (to_string expected)));
    passed

  let%test "set: disjoint memory update keeps both regions" =
    let mem1 = Variable.mem (1l, Int 0l, Int 2l, ("", 0l)) in
    let mem2 = Variable.mem (1l, Int 4l, Int 6l, ("", 0l)) in

    let value1 = Value.ValueSet (RIC.constant 42l) in
    let value2 = Value.ValueSet (RIC.constant 99l) in

    let store =
      { abstract_store = Variable.Map.empty; store_operations = RICSet.empty; unreachable = false }
      |> set ~var:mem1 ~vs:value1
    in

    let actual = set store ~var:mem2 ~vs:value2 in

    let expected =
      { abstract_store = Variable.Map.empty; store_operations = RICSet.empty; unreachable = false }
      |> set ~var:mem1 ~vs:value1
      |> set ~var:mem2 ~vs:value2
    in

    let passed = equal actual expected in
    print_endline (Printf.sprintf "%s %s set %s ↦ %s   ->   %s%s"
      (test_label "[AbstractStore.set]")
      (to_string store)
      (Variable.to_string mem2)
      (Value.to_string value2)
      (to_string actual)
      (if passed then "" else Printf.sprintf " (expected %s)" (to_string expected)));
    passed

  let%test "set: memory update discards different symbolic offset" =
    let mem_a = Variable.mem (0l, Int 0l, Int 0l, ("a", 0l)) in
    let mem_b = Variable.mem (0l, Int 0l, Int 0l, ("b", 0l)) in

    let value_a = Value.ValueSet (RIC.constant 42l) in
    let value_b = Value.ValueSet (RIC.constant 99l) in

    let store =
      { abstract_store = Variable.Map.empty; store_operations = RICSet.empty; unreachable = false }
      |> set ~var:mem_a ~vs:value_a
    in

    let actual = set store ~var:mem_b ~vs:value_b in

    let expected =
      { abstract_store = Variable.Map.empty; store_operations = RICSet.empty; unreachable = false }
      |> set ~var:mem_b ~vs:value_b
    in

    let passed = equal actual expected in
    print_endline (Printf.sprintf "%s %s set %s ↦ %s   ->   %s%s"
      (test_label "[AbstractStore.set]")
      (to_string store)
      (Variable.to_string mem_b)
      (Value.to_string value_b)
      (to_string actual)
      (if passed then "" else Printf.sprintf " (expected %s)" (to_string expected)));
    passed

  let%test "filter_relative_offsets: keeps matching memory offset and non-memory variables" =
    let mem_a = Variable.mem (0l, Int 0l, Int 0l, ("a", 0l)) in
    let mem_b = Variable.mem (0l, Int 0l, Int 0l, ("b", 0l)) in
    let local = Variable.Var (Var.Local 0) in

    let value_a = Value.ValueSet (RIC.constant 10l) in
    let value_b = Value.ValueSet (RIC.constant 20l) in
    let local_value = Value.ValueSet (RIC.constant 42l) in

    let store =
      { abstract_store =
          Variable.Map.empty
          |> Variable.Map.set ~key:mem_a ~data:value_a
          |> Variable.Map.set ~key:mem_b ~data:value_b
          |> Variable.Map.set ~key:local ~data:local_value;
        store_operations = RICSet.empty; unreachable = false }
    in

    let actual = filter_relative_offsets store "a" in

    let expected =
      { abstract_store =
          Variable.Map.empty
          |> Variable.Map.set ~key:mem_a ~data:value_a
          |> Variable.Map.set ~key:local ~data:local_value;
        store_operations = RICSet.empty; unreachable = false }
    in

    let passed = equal actual expected in
    print_endline (Printf.sprintf "%s %s keep offset \"a\"   ->   %s%s"
      (test_label "[AbstractStore.filter_relative_offsets]")
      (to_string store)
      (to_string actual)
      (if passed then "" else Printf.sprintf " (expected %s)" (to_string expected)));
    passed

  let%test "get: missing memory size defaults to positive integers" =
    let store = { abstract_store = Variable.Map.empty; store_operations = RICSet.empty; unreachable = false } in

    let actual = get store ~var:Variable.MemorySize in
    let expected = Value.ValueSet RIC.positive_integers in
    let passed = Value.equal actual expected in

    print_endline (Printf.sprintf "%s get %s[%s]   ->   %s%s"
      (test_label "[AbstractStore.get]")
      (to_string store)
      (Variable.to_string Variable.MemorySize)
      (Value.to_string actual)
      (if passed then "" else Printf.sprintf " (expected %s)" (Value.to_string expected)));
    passed

  let%test "get: missing accessed defaults to Bottom" =
    let store = { abstract_store = Variable.Map.empty; store_operations = RICSet.empty; unreachable = false } in

    let actual = get store ~var:Variable.Accessed in
    let expected = Value.ValueSet RIC.Bottom in
    let passed = Value.equal actual expected in

    print_endline (Printf.sprintf "%s get %s[%s]   ->   %s%s"
      (test_label "[AbstractStore.get]")
      (to_string store)
      (Variable.to_string Variable.Accessed)
      (Value.to_string actual)
      (if passed then "" else Printf.sprintf " (expected %s)" (Value.to_string expected)));
    passed
  
  let%test "widen: widens matching local bindings" =
    let local = Variable.Var (Var.Local 0) in

    let store1 =
      { abstract_store = Variable.Map.empty; store_operations = RICSet.empty; unreachable = false }
      |> set ~var:local ~vs:(Value.ValueSet (RIC.ric (1l, Int 0l, Int 3l, ("", 0l))))
    in

    let store2 =
      { abstract_store = Variable.Map.empty; store_operations = RICSet.empty; unreachable = false }
      |> set ~var:local ~vs:(Value.ValueSet (RIC.ric (1l, Int 0l, Int 5l, ("", 0l))))
    in

    let actual = widen store1 store2 in

    let expected =
      { abstract_store = Variable.Map.empty; store_operations = RICSet.empty; unreachable = false }
      |> set ~var:local ~vs:(Value.ValueSet (RIC.widen
        (RIC.ric (1l, Int 0l, Int 3l, ("", 0l)))
        ~relative_to:(RIC.ric (1l, Int 0l, Int 5l, ("", 0l)))))
      |> set ~var:Variable.entire_memory ~vs:(Value.ValueSet (RIC.Top))
    in

    let passed = equal actual expected in
    print_endline (Printf.sprintf "%s %s widen %s   ->   %s%s"
      (test_label "[AbstractStore.widen]")
      (to_string store1)
      (to_string store2)
      (to_string actual)
      (if passed then "" else Printf.sprintf " (expected %s)" (to_string expected)));
    passed

  let%test "widen: local missing from second store" =
    let local = Variable.Var (Var.Local 0) in

    let value = RIC.ric (1l, Int 0l, Int 3l, ("", 0l)) in

    let store1 =
      { abstract_store = Variable.Map.empty; store_operations = RICSet.empty; unreachable = false }
      |> set ~var:local ~vs:(Value.ValueSet value)
    in

    let store2 =
      { abstract_store = Variable.Map.empty; store_operations = RICSet.empty; unreachable = false }
    in

    let actual = widen store1 store2 in

    let expected =
      { abstract_store = Variable.Map.empty; store_operations = RICSet.empty; unreachable = false }
      |> set ~var:local ~vs:(Value.ValueSet (RIC.widen value ~relative_to:RIC.Bottom))
      |> set ~var:Variable.entire_memory ~vs:(Value.ValueSet (RIC.Top))
    in

    let passed = equal actual expected in
    print_endline (Printf.sprintf "%s %s widen %s   ->   %s%s"
      (test_label "[AbstractStore.widen]")
      (to_string store1)
      (to_string store2)
      (to_string actual)
      (if passed then "" else Printf.sprintf " (expected %s)" (to_string expected)));
    passed

  let%test "widen: local missing from first store" =
    let local = Variable.Var (Var.Local 0) in

    let value = RIC.ric (1l, Int 0l, Int 3l, ("", 0l)) in

    let store1 =
      { abstract_store = Variable.Map.empty; store_operations = RICSet.empty; unreachable = false }
    in

    let store2 =
      { abstract_store = Variable.Map.empty; store_operations = RICSet.empty; unreachable = false }
      |> set ~var:local ~vs:(Value.ValueSet value)
    in

    let actual = widen store1 store2 in

    let expected =
      { abstract_store = Variable.Map.empty; store_operations = RICSet.empty; unreachable = false }
      |> set ~var:local ~vs:(Value.ValueSet (RIC.widen RIC.Bottom ~relative_to:value))
      |> set ~var:Variable.entire_memory ~vs:(Value.ValueSet (RIC.Top))
    in

    let passed = equal actual expected in
    print_endline (Printf.sprintf "%s %s widen %s   ->   %s%s"
      (test_label "[AbstractStore.widen]")
      (to_string store1)
      (to_string store2)
      (to_string actual)
      (if passed then "" else Printf.sprintf " (expected %s)" (to_string expected)));
    passed

  let%test "widen: memory missing from second store widens toward Top" =
    let mem = Variable.mem (1l, Int 0l, Int 3l, ("", 0l)) in
    let value = RIC.ric (1l, Int 0l, Int 3l, ("", 0l)) in

    let store1 =
      { abstract_store = Variable.Map.empty; store_operations = RICSet.empty; unreachable = false }
      |> set ~var:mem ~vs:(Value.ValueSet value)
    in

    let store2 =
      { abstract_store = Variable.Map.empty; store_operations = RICSet.empty; unreachable = false }
    in

    let actual = widen store1 store2 in

    let expected =
      { abstract_store = Variable.Map.empty; store_operations = RICSet.empty; unreachable = false }
      |> set ~var:Variable.entire_memory ~vs:(Value.ValueSet (RIC.Top))
    in

    let passed = equal actual expected in
    print_endline (Printf.sprintf "%s %s widen %s   ->   %s%s"
      (test_label "[AbstractStore.widen]")
      (to_string store1)
      (to_string store2)
      (to_string actual)
      (if passed then "" else Printf.sprintf " (expected %s)" (to_string expected)));
    passed

  let%test "widen: memory missing from first store widens from Top" =
    let mem = Variable.mem (1l, Int 0l, Int 3l, ("", 0l)) in
    let value = RIC.ric (1l, Int 0l, Int 3l, ("", 0l)) in

    let store1 =
      { abstract_store = Variable.Map.empty; store_operations = RICSet.empty; unreachable = false }
    in

    let store2 =
      { abstract_store = Variable.Map.empty; store_operations = RICSet.empty; unreachable = false }
      |> set ~var:mem ~vs:(Value.ValueSet value)
    in

    let actual = widen store1 store2 in

    let expected =
      { abstract_store = Variable.Map.empty; store_operations = RICSet.empty; unreachable = false }
      |> set ~var:Variable.entire_memory ~vs:(Value.ValueSet (RIC.widen RIC.Top ~relative_to:value))
    in

    let passed = equal actual expected in
    print_endline (Printf.sprintf "%s %s widen %s   ->   %s%s"
      (test_label "[AbstractStore.widen]")
      (to_string store1)
      (to_string store2)
      (to_string actual)
      (if passed then "" else Printf.sprintf " (expected %s)" (to_string expected)));
    passed

  let%test "store: i32 strong update at singleton address" =
    let address = Variable.Var (Var.Local 0) in
    let value = Variable.Var (Var.Local 1) in

    let state =
      { abstract_store = Variable.Map.empty; store_operations = RICSet.empty; unreachable = false }
      |> set ~var:address ~vs:(Value.ValueSet (RIC.constant 4l))
      |> set ~var:value ~vs:(Value.ValueSet (RIC.constant 42l))
    in

    let instruction =
      { Memoryop.typ = I32;
        offset = 0;
        pack = None;
        align = 2l }
    in

    let actual =
      store
        ~instruction
        ~value:(Var.Local 1)
        ~address:(Var.Local 0)
        state
    in

    let expected =
      let store_operations =
        RICSet.empty |> RICSet.add ~ric:(RIC.constant 1l)
                     |> RICSet.add ~ric:(RIC.constant 2l)
                     |> RICSet.add ~ric:(RIC.constant 3l)
                     |> RICSet.add ~ric:(RIC.constant 4l)
                     |> RICSet.add ~ric:(RIC.constant 5l)
                     |> RICSet.add ~ric:(RIC.constant 6l)
                     |> RICSet.add ~ric:(RIC.constant 7l) in
      { abstract_store = Variable.Map.empty; store_operations; unreachable = false }
      |> set ~var:address ~vs:(Value.ValueSet (RIC.constant 4l))
      |> set ~var:value ~vs:(Value.ValueSet (RIC.constant 42l))
      |> set ~var:(Variable.mem (0l, Int 0l, Int 0l, ("", 4l)))
          ~vs:(Value.ValueSet (RIC.constant 42l))
    in

    Value_set_options.show_intermediates := true;
    let passed = equal actual expected in
    print_endline (Printf.sprintf "%s store %s at %s in %s   ->   %s%s"
      (test_label "[AbstractStore.store]")
      (Variable.to_string value)
      (Variable.to_string address)
      (to_string state)
      (to_string actual)
      (if passed then "" else Printf.sprintf " (expected %s)" (to_string expected)));
    passed

  let%test "store: i32 store8 at singleton address stores Top" =
    let address = Variable.Var (Var.Local 0) in
    let value = Variable.Var (Var.Local 1) in

    let state =
      { abstract_store = Variable.Map.empty; store_operations = RICSet.empty; unreachable = false }
      |> set ~var:address ~vs:(Value.ValueSet (RIC.constant 4l))
      |> set ~var:value ~vs:(Value.ValueSet (RIC.constant 42l))
      |> set ~var:(Variable.Mem (RIC.constant 1l)) ~vs:(Value.ValueSet (RIC.constant 42l))
    in

    let instruction =
      { Memoryop.typ = I32;
        offset = 0;
        pack = Some (Memoryop.Pack8, Memoryop.SX);
        align = 0l }
    in

    let actual =
      store
        ~instruction
        ~value:(Var.Local 1)
        ~address:(Var.Local 0)
        state
    in

    let expected =
      let store_operations =
        RICSet.empty
        |> RICSet.add ~ric:(RIC.constant 1l)
        |> RICSet.add ~ric:(RIC.constant 2l)
        |> RICSet.add ~ric:(RIC.constant 3l)
        |> RICSet.add ~ric:(RIC.constant 4l) in
      { abstract_store = Variable.Map.empty; store_operations; unreachable = false }
      |> set ~var:address ~vs:(Value.ValueSet (RIC.constant 4l))
      |> set ~var:value ~vs:(Value.ValueSet (RIC.constant 42l))
      |> set ~var:(Variable.entire_memory) ~vs:(Value.ValueSet RIC.Top)
    in

    let passed = equal actual expected in
    print_endline (Printf.sprintf "%s store8 %s at %s in %s   ->   %s%s\n\t\t\t\tactual store_operations: {%s}\n\t\t\t\texpected store_operations: {%s}"
      (test_label "[AbstractStore.store]")
      (Variable.to_string value)
      (Variable.to_string address)
      (to_string state)
      (to_string actual)
      (if passed then "" else Printf.sprintf " (expected %s)" (to_string expected))
      (store_operations_to_string actual)
      (store_operations_to_string expected));
    passed

  let%test "store: i32 store16 at singleton address stores Top" =
    let address = Variable.Var (Var.Local 0) in
    let value = Variable.Var (Var.Local 1) in

    let state =
      { abstract_store = Variable.Map.empty; store_operations = RICSet.empty; unreachable = false }
      |> set ~var:address ~vs:(Value.ValueSet (RIC.constant 4l))
      |> set ~var:value ~vs:(Value.ValueSet (RIC.constant 42l))
    in

    let instruction =
      { Memoryop.typ = I32;
        offset = 0;
        pack = Some (Memoryop.Pack16, Memoryop.SX);
        align = 1l }
    in

    let actual =
      store
        ~instruction
        ~value:(Var.Local 1)
        ~address:(Var.Local 0)
        state
    in

    let store_operations =
      RICSet.empty
      |> RICSet.add ~ric:(RIC.constant 1l)
      |> RICSet.add ~ric:(RIC.constant 2l)
      |> RICSet.add ~ric:(RIC.constant 3l)
      |> RICSet.add ~ric:(RIC.constant 4l)
      |> RICSet.add ~ric:(RIC.constant 5l)
    in

    let expected =
      { abstract_store = Variable.Map.empty; store_operations; unreachable = false }
      |> set ~var:address ~vs:(Value.ValueSet (RIC.constant 4l))
      |> set ~var:value ~vs:(Value.ValueSet (RIC.constant 42l))
      |> set ~var:(Variable.Mem RIC.Top)
          ~vs:(Value.ValueSet RIC.Top)
    in

    let passed = equal actual expected in
    print_endline (Printf.sprintf "%s store16 %s at %s in %s   ->   %s%s\n\t\t\t\tactual store_operations: {%s}\n\t\t\t\texpected store_operations: {%s}"
      (test_label "[AbstractStore.store]")
      (Variable.to_string value)
      (Variable.to_string address)
      (to_string state)
      (to_string actual)
      (if passed then "" else Printf.sprintf " (expected %s)" (to_string expected))
      (store_operations_to_string actual)
      (store_operations_to_string expected));
    passed

  let%test "store: i32 strong update truncates overwritten memory" =
    let address = Variable.Var (Var.Local 0) in
    let value = Variable.Var (Var.Local 1) in
    let old_mem = Variable.mem (1l, Int 0l, Int 10l, ("", 0l)) in

    let old_value = Value.ValueSet (RIC.constant 11l) in
    let new_value = Value.ValueSet (RIC.constant 42l) in

    let state =
      { abstract_store = Variable.Map.empty; store_operations = RICSet.empty; unreachable = false }
      |> set ~var:address ~vs:(Value.ValueSet (RIC.constant 4l))
      |> set ~var:value ~vs:new_value
      |> set ~var:old_mem ~vs:old_value
    in

    let instruction =
      { Memoryop.typ = I32;
        offset = 0;
        pack = None;
        align = 2l }
    in

    let actual =
      store
        ~instruction
        ~value:(Var.Local 1)
        ~address:(Var.Local 0)
        state
    in

    let store_operations =
      RICSet.empty
      |> RICSet.add ~ric:(RIC.constant 1l)
      |> RICSet.add ~ric:(RIC.constant 2l)
      |> RICSet.add ~ric:(RIC.constant 3l)
      |> RICSet.add ~ric:(RIC.constant 4l)
      |> RICSet.add ~ric:(RIC.constant 5l)
      |> RICSet.add ~ric:(RIC.constant 6l)
      |> RICSet.add ~ric:(RIC.constant 7l)
    in

    let expected =
      { abstract_store = Variable.Map.empty; store_operations; unreachable = false }
      |> set ~var:address ~vs:(Value.ValueSet (RIC.constant 4l))
      |> set ~var:value ~vs:new_value
      |> set ~var:(Variable.mem (1l, Int 0l, Int 0l, ("", 0l))) ~vs:old_value
      |> set ~var:(Variable.mem (0l, Int 0l, Int 0l, ("", 4l))) ~vs:new_value
      |> set ~var:(Variable.mem (1l, Int 8l, Int 10l, ("", 0l))) ~vs:old_value
    in

    let passed = equal actual expected in
    print_endline (Printf.sprintf "%s store %s at %s in %s   ->   %s%s\n\t\t\t\tactual store_operations: {%s}\n\t\t\t\texpected store_operations: {%s}"
      (test_label "[AbstractStore.store]")
      (Variable.to_string value)
      (Variable.to_string address)
      (to_string state)
      (to_string actual)
      (if passed then "" else Printf.sprintf " (expected %s)" (to_string expected))
      (store_operations_to_string actual)
      (store_operations_to_string expected));
    passed

  let%test "store: i32 store16 truncates affected memory" =
    let address = Variable.Var (Var.Local 0) in
    let value = Variable.Var (Var.Local 1) in
    let old_mem = Variable.mem (1l, Int 0l, Int 8l, ("", 0l)) in

    let old_value = Value.ValueSet (RIC.constant 11l) in
    let new_value = Value.ValueSet (RIC.constant 42l) in

    let state =
      { abstract_store = Variable.Map.empty; store_operations = RICSet.empty; unreachable = false }
      |> set ~var:address ~vs:(Value.ValueSet (RIC.constant 4l))
      |> set ~var:value ~vs:new_value
      |> set ~var:old_mem ~vs:old_value
    in

    let instruction =
      { Memoryop.typ = I32;
        offset = 0;
        pack = Some (Memoryop.Pack16, Memoryop.SX);
        align = 1l }
    in

    let actual =
      store
        ~instruction
        ~value:(Var.Local 1)
        ~address:(Var.Local 0)
        state
    in

    let store_operations =
      RICSet.empty
      |> RICSet.add ~ric:(RIC.constant 1l)
      |> RICSet.add ~ric:(RIC.constant 2l)
      |> RICSet.add ~ric:(RIC.constant 3l)
      |> RICSet.add ~ric:(RIC.constant 4l)
      |> RICSet.add ~ric:(RIC.constant 5l)
    in

    let expected =
      { abstract_store = Variable.Map.empty; store_operations; unreachable = false }
      |> set ~var:address ~vs:(Value.ValueSet (RIC.constant 4l))
      |> set ~var:value ~vs:new_value
      |> set ~var:(Variable.mem (0l, Int 0l, Int 0l, ("", 0l))) ~vs:old_value
      |> set ~var:(Variable.mem (1l, Int 6l, Int 8l, ("", 0l))) ~vs:old_value
    in

    let passed = equal actual expected in
    print_endline (Printf.sprintf "%s store16 %s at %s in %s   ->   %s%s\n\t\t\t\tactual store_operations: {%s}\n\t\t\t\texpected store_operations: {%s}"
      (test_label "[AbstractStore.store]")
      (Variable.to_string value)
      (Variable.to_string address)
      (to_string state)
      (to_string actual)
      (if passed then "" else Printf.sprintf " (expected %s)" (to_string expected))
      (store_operations_to_string actual)
      (store_operations_to_string expected));
    passed

  let%test "store: i32 store at non-singleton address performs weak update" =
    let address = Variable.Var (Var.Local 0) in
    let value = Variable.Var (Var.Local 1) in

    let mem4 = Variable.mem (0l, Int 0l, Int 0l, ("", 4l)) in
    let mem8 = Variable.mem (0l, Int 0l, Int 0l, ("", 8l)) in

    let old_value4 = RIC.constant 10l in
    let old_value8 = RIC.constant 20l in
    let new_value = RIC.constant 42l in

    let state =
      { abstract_store = Variable.Map.empty; store_operations = RICSet.empty; unreachable = false }
      |> set ~var:address ~vs:(Value.ValueSet (RIC.ric (4l, Int 0l, Int 1l, ("", 4l))))
      |> set ~var:value ~vs:(Value.ValueSet new_value)
      |> set ~var:mem4 ~vs:(Value.ValueSet old_value4)
      |> set ~var:mem8 ~vs:(Value.ValueSet old_value8)
    in

    let instruction =
      { Memoryop.typ = I32;
        offset = 0;
        pack = None;
        align = 2l }
    in

    let actual =
      store
        ~instruction
        ~value:(Var.Local 1)
        ~address:(Var.Local 0)
        state
    in

    let store_operations =
      RICSet.empty
      |> RICSet.add ~ric:(RIC.ric (4l, Int 0l, Int 1l, ("", 4l)))
      |> RICSet.add ~ric:(RIC.ric (4l, Int 0l, Int 2l, ("", 1l)))
      |> RICSet.add ~ric:(RIC.ric (4l, Int 0l, Int 2l, ("", 2l)))
      |> RICSet.add ~ric:(RIC.ric (4l, Int 0l, Int 2l, ("", 3l)))
    in

    let expected =
      { abstract_store = Variable.Map.empty; store_operations; unreachable = false }
      |> set ~var:address ~vs:(Value.ValueSet (RIC.ric (4l, Int 0l, Int 1l, ("", 4l))))
      |> set ~var:value ~vs:(Value.ValueSet new_value)
      |> set ~var:mem4 ~vs:(Value.ValueSet (RIC.join old_value4 new_value))
      |> set ~var:mem8 ~vs:(Value.ValueSet (RIC.join old_value8 new_value))
    in

    let passed = equal actual expected in
    print_endline (Printf.sprintf "%s store %s at %s in %s   ->   %s%s\n\t\t\t\tactual store_operations: {%s}\n\t\t\t\texpected store_operations: {%s}"
      (test_label "[AbstractStore.store]")
      (Variable.to_string value)
      (Variable.to_string address)
      (to_string state)
      (to_string actual)
      (if passed then "" else Printf.sprintf " (expected %s)" (to_string expected))
      (store_operations_to_string actual)
      (store_operations_to_string expected));
    passed

  let%test "store: i32 store erases old values in accessed memory" =
    let address = Variable.Var (Var.Local 0) in
    let value = Variable.Var (Var.Local 1) in

    let old_mem = Variable.mem (1l, Int 0l, Int 10l, ("", 0l)) in
    let old_value = Value.ValueSet (RIC.constant 99l) in
    let new_value = Value.ValueSet (RIC.constant 42l) in

    let state =
      { abstract_store = Variable.Map.empty; store_operations = RICSet.empty; unreachable = false }
      |> set ~var:address ~vs:(Value.ValueSet (RIC.constant 4l))
      |> set ~var:value ~vs:new_value
      |> set ~var:old_mem ~vs:old_value
    in

    let instruction =
      { Memoryop.typ = I32;
        offset = 0;
        pack = None;
        align = 2l }
    in

    let actual =
      store
        ~instruction
        ~value:(Var.Local 1)
        ~address:(Var.Local 0)
        state
    in

    let store_operations =
      RICSet.empty
      |> RICSet.add ~ric:(RIC.constant 1l)
      |> RICSet.add ~ric:(RIC.constant 2l)
      |> RICSet.add ~ric:(RIC.constant 3l)
      |> RICSet.add ~ric:(RIC.constant 4l)
      |> RICSet.add ~ric:(RIC.constant 5l)
      |> RICSet.add ~ric:(RIC.constant 6l)
      |> RICSet.add ~ric:(RIC.constant 7l)
    in

    let expected =
      { abstract_store = Variable.Map.empty; store_operations; unreachable = false }
      |> set ~var:address ~vs:(Value.ValueSet (RIC.constant 4l))
      |> set ~var:value ~vs:new_value
      |> set ~var:(Variable.mem (0l, Int 0l, Int 0l, ("", 0l)))
          ~vs:old_value
      |> set ~var:(Variable.mem (1l, Int 8l, Int 10l, ("", 0l)))
          ~vs:old_value
      |> set ~var:(Variable.mem (0l, Int 0l, Int 0l, ("", 4l)))
          ~vs:new_value
    in

    let passed = equal actual expected in
    print_endline (Printf.sprintf "%s store %s at %s in %s   ->   %s%s\n\t\t\t\tactual store_operations: {%s}\n\t\t\t\texpected store_operations: {%s}"
      (test_label "[AbstractStore.store]")
      (Variable.to_string value)
      (Variable.to_string address)
      (to_string state)
      (to_string actual)
      (if passed then "" else Printf.sprintf " (expected %s)" (to_string expected))
      (store_operations_to_string actual)
      (store_operations_to_string expected));
    passed

  let%test "store: i32 store applies memory offset" =
    let address = Variable.Var (Var.Local 0) in
    let value = Variable.Var (Var.Local 1) in

    let old_mem = Variable.mem (1l, Int 0l, Int 20l, ("", 0l)) in
    let old_value = Value.ValueSet (RIC.constant 11l) in
    let new_value = Value.ValueSet (RIC.constant 42l) in

    let state =
      { abstract_store = Variable.Map.empty; store_operations = RICSet.empty; unreachable = false }
      |> set ~var:address ~vs:(Value.ValueSet (RIC.constant 4l))
      |> set ~var:value ~vs:new_value
      |> set ~var:old_mem ~vs:old_value
    in

    let instruction =
      { Memoryop.typ = I32;
        offset = 8;
        pack = None;
        align = 2l }
    in

    let actual =
      store
        ~instruction
        ~value:(Var.Local 1)
        ~address:(Var.Local 0)
        state
    in

    let store_operations =
      RICSet.empty
      |> RICSet.add ~ric:(RIC.constant 9l)
      |> RICSet.add ~ric:(RIC.constant 10l)
      |> RICSet.add ~ric:(RIC.constant 11l)
      |> RICSet.add ~ric:(RIC.constant 12l)
      |> RICSet.add ~ric:(RIC.constant 13l)
      |> RICSet.add ~ric:(RIC.constant 14l)
      |> RICSet.add ~ric:(RIC.constant 15l)
    in

    let expected =
      { abstract_store = Variable.Map.empty; store_operations; unreachable = false }
      |> set ~var:address ~vs:(Value.ValueSet (RIC.constant 4l))
      |> set ~var:value ~vs:new_value
      |> set ~var:(Variable.mem (1l, Int 0l, Int 8l, ("", 0l))) ~vs:old_value
      |> set ~var:(Variable.mem (0l, Int 0l, Int 0l, ("", 12l))) ~vs:new_value
      |> set ~var:(Variable.mem (1l, Int 16l, Int 20l, ("", 0l))) ~vs:old_value
    in

    let passed = equal actual expected in
    print_endline (Printf.sprintf "%s store %s at %s with offset %d in %s   ->   %s%s\n\t\t\t\tactual store_operations: {%s}\n\t\t\t\texpected store_operations: {%s}"
      (test_label "[AbstractStore.store]")
      (Variable.to_string value)
      (Variable.to_string address)
      instruction.offset
      (to_string state)
      (to_string actual)
      (if passed then "" else Printf.sprintf " (expected %s)" (to_string expected))
      (store_operations_to_string actual)
      (store_operations_to_string expected));
    passed

  let%test "store: i32 store at Bottom address does not update memory" =
    let address = Variable.Var (Var.Local 0) in
    let value = Variable.Var (Var.Local 1) in

    let old_mem = Variable.mem (1l, Int 0l, Int 3l, ("", 0l)) in
    let old_value = Value.ValueSet (RIC.constant 99l) in
    let new_value = Value.ValueSet (RIC.constant 42l) in

    let state =
      { abstract_store = Variable.Map.empty; store_operations = RICSet.empty; unreachable = false }
      |> set ~var:address ~vs:(Value.ValueSet RIC.Bottom)
      |> set ~var:value ~vs:new_value
      |> set ~var:old_mem ~vs:old_value
    in

    let instruction =
      { Memoryop.typ = I32;
        offset = 0;
        pack = None;
        align = 2l }
    in

    let actual =
      store
        ~instruction
        ~value:(Var.Local 1)
        ~address:(Var.Local 0)
        state
    in

    let expected = state in

    let passed = equal actual expected in
    print_endline (Printf.sprintf "%s store %s at %s in %s   ->   %s%s\n\t\t\t\tactual store_operations: {%s}\n\t\t\t\texpected store_operations: {%s}"
      (test_label "[AbstractStore.store]")
      (Variable.to_string value)
      (Variable.to_string address)
      (to_string state)
      (to_string actual)
      (if passed then "" else Printf.sprintf " (expected %s)" (to_string expected))
      (store_operations_to_string actual)
      (store_operations_to_string expected));
    passed

  let%test "store: f32 store erases accessed memory without recording Top" =
    let address = Variable.Var (Var.Local 0) in
    let value = Variable.Var (Var.Local 1) in

    let old_mem = Variable.mem (1l, Int 0l, Int 10l, ("", 0l)) in
    let old_value = Value.ValueSet (RIC.constant 99l) in
    let new_value = Value.ValueSet (RIC.constant 42l) in

    let state =
      { abstract_store = Variable.Map.empty; store_operations = RICSet.empty; unreachable = false }
      |> set ~var:address ~vs:(Value.ValueSet (RIC.constant 4l))
      |> set ~var:value ~vs:new_value
      |> set ~var:old_mem ~vs:old_value
    in

    let instruction =
      { Memoryop.typ = F32;
        offset = 0;
        pack = None;
        align = 2l }
    in

    let actual =
      store
        ~instruction
        ~value:(Var.Local 1)
        ~address:(Var.Local 0)
        state
    in

    let store_operations =
      RICSet.empty
      |> RICSet.add ~ric:(RIC.constant 1l)
      |> RICSet.add ~ric:(RIC.constant 2l)
      |> RICSet.add ~ric:(RIC.constant 3l)
      |> RICSet.add ~ric:(RIC.constant 4l)
      |> RICSet.add ~ric:(RIC.constant 5l)
      |> RICSet.add ~ric:(RIC.constant 6l)
      |> RICSet.add ~ric:(RIC.constant 7l)
    in

    let expected =
      { abstract_store = Variable.Map.empty; store_operations; unreachable = false }
      |> set ~var:address ~vs:(Value.ValueSet (RIC.constant 4l))
      |> set ~var:value ~vs:new_value
      |> set ~var:(Variable.mem (0l, Int 0l, Int 0l, ("", 0l))) ~vs:old_value
      |> set ~var:(Variable.mem (1l, Int 8l, Int 10l, ("", 0l))) ~vs:old_value
    in

    let passed = equal actual expected in
    print_endline (Printf.sprintf "%s f32.store %s at %s in %s   ->   %s%s\n\t\t\t\tactual store_operations: {%s}\n\t\t\t\texpected store_operations: {%s}"
      (test_label "[AbstractStore.store]")
      (Variable.to_string value)
      (Variable.to_string address)
      (to_string state)
      (to_string actual)
      (if passed then "" else Printf.sprintf " (expected %s)" (to_string expected))
      (store_operations_to_string actual)
      (store_operations_to_string expected));
    passed

  let%test "store: i64 store erases 8-byte accessed memory without recording Top" =
    let address = Variable.Var (Var.Local 0) in
    let value = Variable.Var (Var.Local 1) in

    let old_mem = Variable.mem (1l, Int 0l, Int 12l, ("", 0l)) in
    let old_value = Value.ValueSet (RIC.constant 99l) in
    let new_value = Value.ValueSet (RIC.constant 42l) in

    let state =
      { abstract_store = Variable.Map.empty; store_operations = RICSet.empty; unreachable = false }
      |> set ~var:address ~vs:(Value.ValueSet (RIC.constant 4l))
      |> set ~var:value ~vs:new_value
      |> set ~var:old_mem ~vs:old_value
    in

    let instruction =
      { Memoryop.typ = I64;
        offset = 0;
        pack = None;
        align = 3l }
    in

    let actual =
      store
        ~instruction
        ~value:(Var.Local 1)
        ~address:(Var.Local 0)
        state
    in

    let store_operations =
      RICSet.empty
      |> RICSet.add ~ric:(RIC.constant 1l)
      |> RICSet.add ~ric:(RIC.constant 2l)
      |> RICSet.add ~ric:(RIC.constant 3l)
      |> RICSet.add ~ric:(RIC.constant 4l)
      |> RICSet.add ~ric:(RIC.constant 5l)
      |> RICSet.add ~ric:(RIC.constant 6l)
      |> RICSet.add ~ric:(RIC.constant 7l)
      |> RICSet.add ~ric:(RIC.constant 8l)
      |> RICSet.add ~ric:(RIC.constant 9l)
      |> RICSet.add ~ric:(RIC.constant 10l)
      |> RICSet.add ~ric:(RIC.constant 11l)
    in

    let expected =
      { abstract_store = Variable.Map.empty; store_operations; unreachable = false }
      |> set ~var:address ~vs:(Value.ValueSet (RIC.constant 4l))
      |> set ~var:value ~vs:new_value
      |> set ~var:(Variable.mem (0l, Int 0l, Int 0l, ("", 0l))) ~vs:old_value
      |> set ~var:(Variable.mem (0l, Int 0l, Int 0l, ("", 12l))) ~vs:old_value
    in

    let passed = equal actual expected in
    print_endline (Printf.sprintf "%s i64.store %s at %s in %s   ->   %s%s\n\t\t\t\tactual store_operations: {%s}\n\t\t\t\texpected store_operations: {%s}"
      (test_label "[AbstractStore.store]")
      (Variable.to_string value)
      (Variable.to_string address)
      (to_string state)
      (to_string actual)
      (if passed then "" else Printf.sprintf " (expected %s)" (to_string expected))
      (store_operations_to_string actual)
      (store_operations_to_string expected));
    passed

  let%test "store: i32 store preserves previous store operations" =
    let address = Variable.Var (Var.Local 0) in
    let value = Variable.Var (Var.Local 1) in

    let previous_store_operations =
      RICSet.empty
      |> RICSet.add ~ric:(RIC.constant 100l)
    in

    let state =
      { abstract_store = Variable.Map.empty;
        store_operations = previous_store_operations; unreachable = false }
      |> set ~var:address ~vs:(Value.ValueSet (RIC.constant 4l))
      |> set ~var:value ~vs:(Value.ValueSet (RIC.constant 42l))
    in

    let instruction =
      { Memoryop.typ = I32;
        offset = 0;
        pack = None;
        align = 2l }
    in

    let actual =
      store
        ~instruction
        ~value:(Var.Local 1)
        ~address:(Var.Local 0)
        state
    in

    let expected_store_operations =
      previous_store_operations
      |> RICSet.add ~ric:(RIC.constant 1l)
      |> RICSet.add ~ric:(RIC.constant 2l)
      |> RICSet.add ~ric:(RIC.constant 3l)
      |> RICSet.add ~ric:(RIC.constant 4l)
      |> RICSet.add ~ric:(RIC.constant 5l)
      |> RICSet.add ~ric:(RIC.constant 6l)
      |> RICSet.add ~ric:(RIC.constant 7l)
    in

    let expected =
      { abstract_store = Variable.Map.empty;
        store_operations = expected_store_operations; unreachable = false }
      |> set ~var:address ~vs:(Value.ValueSet (RIC.constant 4l))
      |> set ~var:value ~vs:(Value.ValueSet (RIC.constant 42l))
      |> set ~var:(Variable.mem (0l, Int 0l, Int 0l, ("", 4l)))
          ~vs:(Value.ValueSet (RIC.constant 42l))
    in

    let passed = equal actual expected in
    print_endline (Printf.sprintf "%s store %s at %s in %s   ->   %s%s\n\t\t\t\tactual store_operations: {%s}\n\t\t\t\texpected store_operations: {%s}"
      (test_label "[AbstractStore.store]")
      (Variable.to_string value)
      (Variable.to_string address)
      (to_string state)
      (to_string actual)
      (if passed then "" else Printf.sprintf " (expected %s)" (to_string expected))
      (store_operations_to_string actual)
      (store_operations_to_string expected));
    passed

  let%test "store: i64 store32 erases 4-byte accessed memory without recording Top" =
    let address = Variable.Var (Var.Local 0) in
    let value = Variable.Var (Var.Local 1) in

    let old_mem = Variable.mem (1l, Int 0l, Int 10l, ("", 0l)) in
    let old_value = Value.ValueSet (RIC.constant 99l) in
    let new_value = Value.ValueSet (RIC.constant 42l) in

    let state =
      { abstract_store = Variable.Map.empty; store_operations = RICSet.empty; unreachable = false }
      |> set ~var:address ~vs:(Value.ValueSet (RIC.constant 4l))
      |> set ~var:value ~vs:new_value
      |> set ~var:old_mem ~vs:old_value
    in

    let instruction =
      { Memoryop.typ = I64;
        offset = 0;
        pack = Some (Memoryop.Pack32, Memoryop.SX);
        align = 2l }
    in

    let actual =
      store
        ~instruction
        ~value:(Var.Local 1)
        ~address:(Var.Local 0)
        state
    in

    let store_operations =
      RICSet.empty
      |> RICSet.add ~ric:(RIC.constant 1l)
      |> RICSet.add ~ric:(RIC.constant 2l)
      |> RICSet.add ~ric:(RIC.constant 3l)
      |> RICSet.add ~ric:(RIC.constant 4l)
      |> RICSet.add ~ric:(RIC.constant 5l)
      |> RICSet.add ~ric:(RIC.constant 6l)
      |> RICSet.add ~ric:(RIC.constant 7l)
    in

    let expected =
      { abstract_store = Variable.Map.empty; store_operations; unreachable = false }
      |> set ~var:address ~vs:(Value.ValueSet (RIC.constant 4l))
      |> set ~var:value ~vs:new_value
      |> set ~var:(Variable.mem (0l, Int 0l, Int 0l, ("", 0l))) ~vs:old_value
      |> set ~var:(Variable.mem (1l, Int 8l, Int 10l, ("", 0l))) ~vs:old_value
    in

    let passed = equal actual expected in
    print_endline (Printf.sprintf "%s i64.store32 %s at %s in %s   ->   %s%s\n\t\t\t\tactual store_operations: {%s}\n\t\t\t\texpected store_operations: {%s}"
      (test_label "[AbstractStore.store]")
      (Variable.to_string value)
      (Variable.to_string address)
      (to_string state)
      (to_string actual)
      (if passed then "" else Printf.sprintf " (expected %s)" (to_string expected))
      (store_operations_to_string actual)
      (store_operations_to_string expected));
    passed

  let%test "store: weak update joins with multiple overlapping memory cells" =
    let address = Variable.Var (Var.Local 0) in
    let value = Variable.Var (Var.Local 1) in

    let mem4 = Variable.mem (0l, Int 0l, Int 0l, ("", 4l)) in
    let mem8 = Variable.mem (0l, Int 0l, Int 0l, ("", 8l)) in
    let mem12 = Variable.mem (0l, Int 0l, Int 0l, ("", 12l)) in

    let old_value4 = RIC.constant 10l in
    let old_value8 = RIC.constant 20l in
    let old_value12 = RIC.constant 30l in
    let new_value = RIC.constant 42l in

    let state =
      { abstract_store = Variable.Map.empty; store_operations = RICSet.empty; unreachable = false }
      |> set ~var:address ~vs:(Value.ValueSet (RIC.ric (4l, Int 0l, Int 2l, ("", 4l))))
      |> set ~var:value ~vs:(Value.ValueSet new_value)
      |> set ~var:mem4 ~vs:(Value.ValueSet old_value4)
      |> set ~var:mem8 ~vs:(Value.ValueSet old_value8)
      |> set ~var:mem12 ~vs:(Value.ValueSet old_value12)
    in

    let instruction =
      { Memoryop.typ = I32;
        offset = 0;
        pack = None;
        align = 2l }
    in

    let actual =
      store
        ~instruction
        ~value:(Var.Local 1)
        ~address:(Var.Local 0)
        state
    in

    let store_operations =
      RICSet.empty
      |> RICSet.add ~ric:(RIC.ric (4l, Int 0l, Int 2l, ("", 4l)))
      |> RICSet.add ~ric:(RIC.ric (4l, Int 0l, Int 3l, ("", 1l)))
      |> RICSet.add ~ric:(RIC.ric (4l, Int 0l, Int 3l, ("", 2l)))
      |> RICSet.add ~ric:(RIC.ric (4l, Int 0l, Int 3l, ("", 3l)))
    in

    let expected =
      { abstract_store = Variable.Map.empty; store_operations; unreachable = false }
      |> set ~var:address ~vs:(Value.ValueSet (RIC.ric (4l, Int 0l, Int 2l, ("", 4l))))
      |> set ~var:value ~vs:(Value.ValueSet new_value)
      |> set ~var:mem4 ~vs:(Value.ValueSet (RIC.join old_value4 new_value))
      |> set ~var:mem8 ~vs:(Value.ValueSet (RIC.join old_value8 new_value))
      |> set ~var:mem12 ~vs:(Value.ValueSet (RIC.join old_value12 new_value))
    in

    let passed = equal actual expected in
    print_endline (Printf.sprintf "%s store %s at %s in %s   ->   %s%s\n\t\t\t\tactual store_operations: {%s}\n\t\t\t\texpected store_operations: {%s}"
      (test_label "[AbstractStore.store]")
      (Variable.to_string value)
      (Variable.to_string address)
      (to_string state)
      (to_string actual)
      (if passed then "" else Printf.sprintf " (expected %s)" (to_string expected))
      (store_operations_to_string actual)
      (store_operations_to_string expected));
    passed
end)