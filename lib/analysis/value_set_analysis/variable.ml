(** {1:variable Variables for value-set/pointer analysis}

    This module defines the notion of a program {e variable} used by the
    value‑set / pointer analysis. A variable can be:
    {ul
      {- a concrete program variable ({!Var.t});}
      {- a memory block over a reduced interval–congruence ({!RIC.t});}
      {- a stack block, modelled like memory but tracked independently;}
      {- the synthetic marker {!Accessed}, collecting addresses read by a function.}}

    {2:addr Address model (RIC)}
    Memory and stack regions are represented by {!Reduced_interval_congruence.RIC.t}.
    A region carries: a {b stride}, integer {b lower} and {b upper}
    bounds (as {!Maths.ExtendedInt.t}), and a symbolic {b offset} (e.g. "g0" for the
    stack pointer) paired with an extra integer displacement.

    {2:conv Conventions}
    - Bounds may be {e infinite} using {!Maths.ExtendedInt.Infinity} and
      {!Maths.ExtendedInt.NegInfinity}.
    - A region equal to {!Reduced_interval_congruence.RIC.Bottom} denotes the empty set.
    - "Comparable offsets" means offsets refer to the same symbolic base.

    {2:fmt Printing}
    Values implement human‑readable printers so that test logs are uniform.
    See {!to_string}, {!Set.S.to_string} and {!Map.to_string}.

    {2:ex Examples}
    - [Mem (RIC.ric (1l, Int 0l, Int 3l, ("", 0l)))] prints as a byte‑range in linear memory.
    - [Stack ...] is printed with the relative offset base stripped (e.g. [stack[...]]).
*)

open Core

open Reduced_interval_congruence

open Maths


module T = struct
  (** {2:sum The variable kind}
      A variable is one of:
      - [Var v] — a concrete program variable (local or global).
      - [Mem ric] — a linear‑memory region described by {!RIC.t}.
      - [Stack ric] — a stack region (modeled by {!RIC.t}) tracked separately.
      - [Accessed] — synthetic accumulator of all addresses {e read} by a function. *)
  type t = 
    | Var of Var.t
    | Mem of RIC.t
    | Stack of RIC.t (* used when considering the stack disjoint from the rest of the memory *)
    | Accessed (* used to store all the addresses that may have been read by a function *)
  [@@deriving sexp, compare, equal]

  (** [to_string v] renders a variable in a compact, stable format suited for logs.
      {ul
        {- [Var x] prints as [x];}
        {- [Mem r] prints as [mem[<r>]];}
        {- [Stack r] prints as [stack[<r>]] with relative offset base removed;}
        {- [Accessed] prints as [Accessed_memory].}}
      @param var the variable to pretty‑print
      @return a printable representation. *)
  let to_string (var : t) : string =
    match var with 
    | Var v -> Var.to_string v
    | Mem ric -> "mem[" ^ RIC.to_string ric ^ "]"
    | Stack ric -> "stack[" ^ RIC.to_string (RIC.remove_relative_offset ric) ^"]"
    | Accessed -> "Accessed_memory"

  (** Smart constructor for a memory variable from raw RIC components.
      @param ric a quadruple [(stride, lower, upper, (offset_base, offset_delta))]
      @return [Mem (RIC.ric ric)]. *)
  let mem (ric : int32 * ExtendedInt.t * ExtendedInt.t * (string * int32)) : t =
    Mem (RIC.ric ric)

  (** The variable denoting the whole linear memory region. *)
  let entire_memory = Mem RIC.Top

  (** The variable denoting the whole stack region. *)
  let entire_stack = Stack RIC.Top

  (** [is_linear_memory v] is [true] iff [v] denotes a linear‑memory region. *)
  let is_linear_memory (v : t) : bool =
    match v with
    | Mem _ -> true
    | _ -> false

  (** [is_stack v] is [true] iff [v] denotes a stack region. *)
  let is_stack (v : t) : bool =
    match v with
    | Stack _ -> true
    | _ -> false

  (** [is_global v] is [true] iff [v] is a global program variable. *)
  let is_global (v : t) : bool =
    match v with
    | Var Var.Global _ -> true
    | _ -> false

  (** [get_address v] extracts the underlying {!RIC.t} when [v] is [Mem] or [Stack].
      Returns {!RIC.Bottom} for non‑addressed variants. *)
  let get_address (var : t) : RIC.t =
    match var with
    | Mem address -> address
    | Stack address -> address
    | _ -> RIC.Bottom

  (** [get_relative_offset v] returns the symbolic offset base used by [v].
      For stack variables the base is always ["g0"]. Returns [""] when not applicable. *)
  let get_relative_offset (v : t) : string =
    match v with
    | Mem RIC {offset = (offset, _); _} -> offset
    | Stack _ -> "g0"
    | _ -> ""

  (** Join a list of variables with a comma for debugging output. *)
  let list_to_string (vars : t list) : string = String.concat ~sep:", " (List.map vars ~f:to_string)

  (** [is_infinite v] is [true] when [v] denotes an unbounded region (either bound is
      {!Maths.ExtendedInt.Infinity}/{!Maths.ExtendedInt.NegInfinity}, or the region is [Top]). *)
  let is_infinite (v : t) : bool =
    match v with
    | Var _ -> false
    | Mem RIC.Top -> true
    | Mem RIC {stride = _; lower_bound = l; upper_bound = u; offset = _}
        when ExtendedInt.equal l NegInfinity || ExtendedInt.equal u Infinity -> true
    | Stack RIC {stride = _; lower_bound = l; upper_bound = u; offset = _}
        when ExtendedInt.equal l NegInfinity || ExtendedInt.equal u Infinity -> true
    | _ -> false

  (** [is_finite v] is [true] when [v] is not infinite. *)
  let is_finite (var : t) : bool = not (is_infinite var)

  (** [is_singleton v] is [true] when [v] denotes at most one address (i.e. [lower = upper]
      for addressed variants). *)
  let is_singleton (var : t) : bool =
    match var with
    | Mem RIC {lower_bound = l; upper_bound = u; _} when not (ExtendedInt.equal l u) -> false
    | Stack RIC {lower_bound = l; upper_bound = u; _} when not (ExtendedInt.equal l u) -> false
    | _ -> true

  (** [share_addresses v1 v2] checks whether two addressed variables overlap.
      Only [Mem]/[Mem] and [Stack]/[Stack] are considered; other combinations return [false].
      Internally this is [meet ≠ Bottom]. *)
  let share_addresses (var1 : t) (var2 : t) : bool =
    match var1, var2 with
    | Mem ric1, Mem ric2 -> not (RIC.equal RIC.Bottom (RIC.meet ric1 ric2))
    | Stack ric1, Stack ric2 -> not (RIC.equal RIC.Bottom (RIC.meet ric1 ric2))
    | _ -> false

  (** [remove ~these_addresses ~from] subtracts a region from an addressed variable.
      @return a list of residual addressed variables covering [from \ these_addresses].
      Non‑addressed variants are left unchanged. *)
  let remove ~(these_addresses : RIC.t) ~(from : t) : t list =
    let address = get_address from in
    let truncated_addresses = RIC.remove ~this:these_addresses ~from:address in
    if is_linear_memory from then
      List.map truncated_addresses ~f:(fun addr -> Mem addr)
    else
      List.map truncated_addresses ~f:(fun addr -> Stack addr)

  (** Iterated version of {!remove} for a list of regions. Order is left‑to‑right. *)
  let remove_all ~(these_addresses_list : RIC.t list) ~(from : t) : t list =
    List.fold these_addresses_list ~init:[from] ~f:(fun acc ric ->
      List.concat_map acc ~f:(fun var -> remove ~these_addresses:ric ~from:var)
    )

  (** [comparable_offsets v1 v2] is [true] iff both are memory variables whose
      offsets refer to the same symbolic base; non‑addressed variants return [true]. *)
  let comparable_offsets (v1 : t) (v2 : t) : bool =
    match v1, v2 with
    | Mem addr1, Mem addr2 -> RIC.comparable_offsets addr1 addr2
    | _ -> true

  (** [is_covered ~by v] holds when the union of the addressed variables in [by]
      completely covers [v]. The check is performed by successive set differences
      against {!RIC.t} regions and emptiness testing. *)
  let is_covered ~(by : t list) (v : t) : bool =
    let not_covered =
      match v with
      | Mem v_addr ->
        List.fold ~init:[v_addr]
          ~f:(fun acc x ->
            match x with
            | Var _ | Stack _ | Accessed -> acc
            | Mem addr -> 
              List.concat (List.map ~f:(fun y -> RIC.remove ~this:addr ~from:y) acc))
          by
      | Stack v_addr -> 
        List.fold ~init:[v_addr]
          ~f:(fun acc x ->
            match x with
            | Var _ | Mem _ | Accessed -> acc
            | Stack addr -> 
              List.concat (List.map ~f:(fun y -> RIC.remove ~this:addr ~from:y) acc))
          by
      | _ -> assert false
    in
    let not_covered = List.filter ~f:(fun x -> not (RIC.equal RIC.Bottom x)) not_covered in
    List.is_empty not_covered

  (** Rewrite the symbolic offset base of [var] using concrete values from [actual_values].
      For [Stack], the resulting symbolic base {b must} remain ["g0"]; otherwise the
      function raises [Failure] with an explanatory message. *)
  let update_relative_offset ~(var : t) ~(actual_values : RIC.t String.Map.t) : t =
    match var with
    | Var _ | Accessed -> var
    | Mem address ->
      let new_address = RIC.update_relative_offset ~ric_:address ~actual_values in
      Mem new_address
    | Stack address ->
      let new_address = RIC.update_relative_offset ~ric_:address ~actual_values in
      let new_stack_var = Stack new_address in
      begin match new_stack_var with
      | Stack RIC {offset = (o, _); _} -> 
        if String.equal "g0" o then
          new_stack_var
        else
          failwith "Inconsistent stack pointer. This analysis only works when global variable g0 is used as stack pointer."
      | Stack RIC.Top -> new_stack_var
      | _ -> assert false
      end
end
include T

module Set = struct
  include Set
  (** {2:set A set of variables}
      Wrapper around {!Set.Make} with helpers for printing in tests. *)
  module S = struct
    include Set.Make(T)
    (** Comma‑separated printer for sets of variables. *)
    let to_string (v : t) : string =
      String.concat ~sep:"," (List.map ~f:to_string (Set.to_list v))

    (** Cardinality of a set. *)
    let cardinal (v : t) : int =
      List.length (Set.to_list v)
  end
  include Set
  include S
  include Test.Helpers(S)

  let of_option (v : T.t option) : t =
    match v with
    | Some v -> singleton v
    | None -> empty
end

module Map = struct
  include Map
  include Map.Make(T)
  (** {2:map Maps keyed by variables}
      Convenience printers and utilities to extract typed views of a store. *)
  (** Pretty‑print a map as [k ↦ v] pairs using a value printer. *)
  let to_string (m : 'a t) (f : 'a -> string) : string =
    String.concat ~sep:", " (List.map (Map.to_alist m) ~f:(fun (k, v) -> Printf.sprintf "%s ↦ %s" (to_string k) (f v)))

  (** Return all keys that denote linear‑memory regions. *)
  let extract_memory_variables (store : 'a t) : T.t list =
    let all_vars = keys store in
    List.filter ~f:T.is_linear_memory all_vars

  (** Return all keys that denote stack regions. *)
  let extract_stack_variables (store : 'a t) : T.t list =
    let all_vars = keys store in
    List.filter ~f:T.is_stack all_vars

  (** Return all keys that are locals or globals. *)
  let extract_locals_and_globals (store : 'a t) : T.t list =
    let all_vars = keys store in
    List.filter ~f:(fun v -> match v with | Var Var.Local _ | Var Var.Global _ -> true | _ -> false) all_vars

  (** [update_all store vars new_value] sets [new_value] for all [vars] in [store]. *)
  let update_all (store : 'a t) (vars : Set.t) (new_value : 'a) : 'a t =
    Set.fold vars ~init:store ~f:(fun acc v -> set acc ~key:v ~data:new_value)

  (** {3:compat Partition stores using common regions}
      [make_compatible ~this ~relative_to ~get] refines [this] so that memory/stack keys
      are split wherever they overlap with keys in [relative_to]. Overlaps are computed
      via {!RIC.meet}; splits preserve values using [get] to fetch the existing payload.
      The result contains only disjoint addressed keys also present (as partitions) in
      the other store. *)
  let make_compatible ~(this : 'a t) ~(relative_to : 'a t) ~(get : 'a t -> var:T.t -> 'a) : 'a t =
    let store1 = this in
    let store2 = relative_to in
    let mems1 = extract_memory_variables store1 in
    let mems2 = extract_memory_variables store2 in
    let new_store =
      List.fold
        ~init:store1
        ~f:(fun store m2 ->
          match m2 with
          | T.Mem addr_m2 -> 
            List.fold
              ~init:store
              ~f:(fun store m1 ->
                match m1 with
                | T.Mem addr_m1 ->
                  let met_addrs = RIC.meet addr_m2 addr_m1 in
                  if RIC.equal RIC.Bottom met_addrs then
                    store
                  else
                    let new_addresses = met_addrs :: (RIC.remove ~this:met_addrs ~from:addr_m1) in
                    let new_mem_vars = List.map ~f:(fun addr -> T.Mem addr) new_addresses in
                    let vs = get store ~var:m1 in
                    let store = remove store m1 in
                    update_all store (Set.of_list new_mem_vars) vs
                | _ -> store )
              mems1
          | _ -> store)
        mems2
    in
    let stack1 = extract_stack_variables new_store in
    let stack2 = extract_stack_variables store2 in
    List.fold
      ~init:new_store
      ~f:(fun store m2 ->
        match m2 with
        | T.Stack addr_m2 -> 
          List.fold
            ~init:store
            ~f:(fun store m1 ->
              match m1 with
              | T.Stack addr_m1 ->
                let met_addrs = RIC.meet addr_m2 addr_m1 in
                if RIC.equal RIC.Bottom met_addrs then
                  store
                else
                  let new_addresses = met_addrs :: (RIC.remove ~this:met_addrs ~from:addr_m1) in
                  let new_stack_vars = List.map ~f:(fun addr -> T.Stack addr) new_addresses in
                  let vs = get store ~var:m1 in
                  let store = remove store m1 in
                  update_all store (Set.of_list new_stack_vars) vs
              | _ -> store )
            stack1
        | _ -> store)
      stack2
end





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

let%test_module "Variable tests" = (module struct
  let mem addr lo hi offset =
    mem (Int32.of_int_exn addr, Int (Int32.of_int_exn lo), Int (Int32.of_int_exn hi), offset)

  let%test "Tests on variable module" =
    print_endline "_______ _______________ _______\n        Variable module        \n------- --------------- -------\n";
    true

  let%test "share_addresses yes" =
    let v1 = mem 1 0 4 ("", 0l) in
    let v2 = mem 1 2 6 ("", 0l) in
    let actual = share_addresses v1 v2 in
    let expected = true in
    let context = Printf.sprintf "%s, %s" (to_string v1) (to_string v2) in
    print_endline (Printf.sprintf "[Variable.share_addresses] %s -> %s (expected %s)" context (Bool.to_string actual) (Bool.to_string expected));
    actual

  let%test "share_addresses no" =
    let v1 = mem 2 0 4 ("", 0l) in
    let v2 = mem 2 0 4 ("", 1l) in
    let actual = share_addresses v1 v2 in
    let expected = false in
    let context = Printf.sprintf "%s, %s" (to_string v1) (to_string v2) in
    print_endline (Printf.sprintf "[Variable.share_addresses] %s -> %s (expected %s)" context (Bool.to_string actual) (Bool.to_string expected));
    not actual

  let%test "remove full overlap" =
    let from = mem 1 0 4 ("", 0l) in
    let these = RIC.ric (1l, Int (-1l), Int 6l, ("", 0l)) in
    let actual = remove ~these_addresses:these ~from in
    let expected = [] in
    print_endline (Printf.sprintf "[Variable.remove] these:%s from:%s -> [%s] (expected [%s])"
      (RIC.to_string these) (to_string from)
      (String.concat ~sep:", " (List.map ~f:to_string actual))
      (String.concat ~sep:", " (List.map ~f:to_string expected)));
    List.equal equal actual expected

  let%test "remove some overlap" =
    let from = mem 1 0 4 ("", 0l) in
    let these = RIC.ric (2l, Int (0l), Int 1l, ("", 0l)) in
    let actual = remove ~these_addresses:these ~from in
    let expected = [mem 2 0 1 ("", 1l); mem 0 0 0 ("", 4l)] in
    print_endline (Printf.sprintf "[Variable.remove] these:%s from:%s -> [%s] (expected [%s])"
      (RIC.to_string these) (to_string from)
      (String.concat ~sep:", " (List.map ~f:to_string actual))
      (String.concat ~sep:", " (List.map ~f:to_string expected)));
    List.equal equal actual expected

  let%test "remove no overlap" =
    let from = mem 1 0 4 ("", 0l) in
    let these = RIC.ric (1l, Int 5l, Int 8l, ("", 0l)) in
    let actual = remove ~these_addresses:these ~from in
    let expected = [from] in
    print_endline (Printf.sprintf "[Variable.remove] these:%s from:%s -> [%s] (expected [%s])"
      (RIC.to_string these) (to_string from)
      (String.concat ~sep:", " (List.map ~f:to_string actual))
      (String.concat ~sep:", " (List.map ~f:to_string expected)));
    List.equal equal actual expected

  let%test "remove list of addresses" =
    let from = mem 1 0 4 ("", 0l) in
    let these = [RIC.ric (2l, Int (0l), Int 1l, ("", 0l)); RIC.ric (0l, Int 0l, Int 0l, ("", 3l)); RIC.ric (1l, Int 0l, Int 3l, ("", 37l))] in
    let actual = remove_all ~these_addresses_list:these ~from in
    let expected = [mem 0 0 0 ("", 1l); mem 0 0 0 ("", 4l)] in
    print_endline (Printf.sprintf "[Variable.remove_all] these:%s from:%s -> [%s] (expected [%s])"
      (String.concat ~sep:"; " (List.map ~f:RIC.to_string these))
      (to_string from)
      (String.concat ~sep:", " (List.map ~f:to_string actual))
      (String.concat ~sep:", " (List.map ~f:to_string expected)));
    List.equal equal actual expected

  let%test "remove list of incompatible addresses" =
    let from = mem 1 0 4 ("", 0l) in
    let these = [RIC.ric (2l, Int (0l), Int 1l, ("a", 0l)); RIC.ric (0l, Int 0l, Int 0l, ("", 3l)); RIC.ric (1l, Int 0l, Int 3l, ("a", 37l))] in
    let actual = remove_all ~these_addresses_list:these ~from in
    let expected = [] in
    print_endline (Printf.sprintf "[Variable.remove_all] these:%s from:%s -> [%s] (expected [%s])"
      (String.concat ~sep:"; " (List.map ~f:RIC.to_string these))
      (to_string from)
      (String.concat ~sep:", " (List.map ~f:to_string actual))
      (String.concat ~sep:", " (List.map ~f:to_string expected)));
    List.equal equal actual expected

  let%test "is_covered fully" =
    let target = mem 1 0 2 ("", 0l) in
    let cover1 = mem 1 0 1 ("", 0l) in
    let cover2 = mem 1 2 3 ("", 0l) in
    let actual = is_covered ~by:[cover1; cover2] target in
    let expected = true in
    let context = Printf.sprintf "%s by:[%s]" (to_string target) (String.concat ~sep:", " (List.map ~f:to_string [cover1; cover2])) in
    print_endline (Printf.sprintf "[Variable.is_covered] %s -> %s (expected %s)" context (Bool.to_string actual) (Bool.to_string expected));
    actual

  let%test "is_covered partially" =
    let target = mem 1 0 2 ("", 0l) in
    let cover1 = mem 1 0 0 ("", 0l) in
    let cover2 = mem 1 2 3 ("", 20l) in
    let actual = is_covered ~by:[cover1] target in
    let expected = false in
    let context = Printf.sprintf "%s by:[%s]" (to_string target) (String.concat ~sep:", " (List.map ~f:to_string [cover1; cover2])) in
    print_endline (Printf.sprintf "[Variable.is_covered] %s -> %s (expected %s)" context (Bool.to_string actual) (Bool.to_string expected));
    not actual

  let%test "update_relative_offset" =
    let original = mem 1 0 2 ("rel", 1l) in
    let values = String.Map.of_alist_exn [("rel", RIC.ric (1l, Int 10l, Int 10l, ("x", 0l)))] in
    let updated = update_relative_offset ~var:original ~actual_values:values in
    let actual =
      match updated with
      | Mem RIC { offset = ("x", new_off); _ } -> Int32.(new_off = 1l + 10l)
      | _ -> false
    in
    let expected = true in
    let context = Printf.sprintf "%s -> %s" (to_string original) (to_string updated) in
    print_endline (Printf.sprintf "[Variable.update_relative_offset] %s -> %s (expected %s)" context (Bool.to_string actual) (Bool.to_string expected));
    actual

  let%test "update_relative_offset_2" =
    let original = mem 1 0 2 ("rel", 1l) in
    let values = String.Map.of_alist_exn [("rel", RIC.ric (1l, Int 10l, Int 11l, ("", 0l)))] in
    let updated = update_relative_offset ~var:original ~actual_values:values in
    let actual =
      match updated with
      | Mem RIC { lower_bound = Int 0l; upper_bound = Int 3l; offset = ("", 11l); _ } -> true
      | _ -> false
    in
    let expected = true in
    let context = Printf.sprintf "%s -> %s" (to_string original) (to_string updated) in
    print_endline (Printf.sprintf "[Variable.update_relative_offset] %s -> %s (expected %s)" context (Bool.to_string actual) (Bool.to_string expected));
    actual
end)