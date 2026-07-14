(** {1:variable Variables for value-set analysis}

    This module defines the variables used as keys in the value-set analysis store.
    A variable can be a program variable, a linear-memory region,
    a synthetic read-address accumulator, or the current memory size.

    Memory regions are described by a reduced interval-congruence ({!RIC.t})
    and may include symbolic offset bases.

    The module also provides helpers for overlap checks, region subtraction,
    relative-offset substitution, and map/set manipulation. *)

open Core

open Reduced_interval_congruence

open Maths


module T = struct
  (** Kind of variable tracked by the value-set analysis. *)
  type t = 
    | Var of Var.t
    | Mem of RIC.t
    | Accessed (* addresses that may have been read by a function *)
    | MemorySize (* memory size, in WebAssembly pages *)
  [@@deriving sexp, compare, equal]

  let (=) = equal
  let (<>) (x : t) (y : t) : bool = not (x = y)

  (** [to_string v] returns a compact representation of [v] for logs and tests. *)
  let to_string (var : t) : string =
    match var with 
    | Var v -> Var.to_string v
    | Mem ric -> "mem[" ^ RIC.to_string ric ^ "]"
    | Accessed -> "Accessed_memory"
    | MemorySize -> "Memory_size"

  (** [mem ric] builds a memory variable from raw RIC components. *)
  let mem (ric : int32 * ExtendedInt.t * ExtendedInt.t * (string * int32)) : t =
    Mem (RIC.ric ric)

  (** Variable representing the whole linear memory. *)
  let entire_memory = Mem RIC.Top

  (** [is_linear_memory v] is [true] iff [v] is a memory region. *)
  let is_linear_memory (v : t) : bool =
    match v with
    | Mem _ -> true
    | _ -> false

  (** [is_global v] is [true] iff [v] is a global program variable. *)
  let is_global (v : t) : bool =
    match v with
    | Var Var.Global _ -> true
    | _ -> false

  (** [get_address v] returns the address region of [v], or [RIC.Bottom] if [v] has none. *)
  let get_address (var : t) : RIC.t =
    match var with
    | Mem address -> address
    | _ -> RIC.Bottom

  (** [get_relative_offset v] returns the symbolic offset base of [v].
    Non-memory variables return [""]. *)
  let get_relative_offset (v : t) : string =
    match v with
    | Mem addr -> RIC.extract_relative_offset addr
    | _ -> ""

  (** [is_infinite v] is [true] iff [v] is an unbounded memory region. *)
  let is_infinite (v : t) : bool =
    match v with
    | Var _ -> false
    | Mem RIC.Top -> true
    | Mem RIC {lower_bound = l; upper_bound = u; _} ->
      ExtendedInt.(l = NegInfinity || u = Infinity)
    | _ -> false

  (** [is_finite v] is [true] iff [v] is not infinite. *)
  let is_finite (var : t) : bool = not (is_infinite var)

  (** [share_addresses v1 v2] is [true] iff [v1] and [v2] overlap as memory regions. *)
  let share_addresses (var1 : t) (var2 : t) : bool =
    match var1, var2 with
    | Mem ric1, Mem ric2 -> RIC.(meet ric1 ric2 <> Bottom)
    | _ -> false

  (** [remove ~these_addresses ~from] removes [these_addresses] from the address region
      of [from] and returns the remaining variables. *)
  let remove ~(these_addresses : RIC.t) ~(from : t) : t list =
    RIC.remove 
      ~this:these_addresses 
      ~from:(get_address from)
    |> List.map ~f:(fun addr -> Mem addr)

  (** [remove_all ~these_addresses_list ~from] removes each region from [from] in order. *)
  let remove_all ~(these_addresses_list : RIC.t list) ~(from : t) : t list =
    these_addresses_list
    |> List.fold
        ~init:[from] 
        ~f:(fun acc ric ->
          List.concat_map acc ~f:(fun var -> remove ~these_addresses:ric ~from:var))

  (** [comparable_offsets v1 v2] checks whether two memory variables use comparable
      symbolic offsets. Non-memory pairs are considered comparable. *)
  let comparable_offsets (v1 : t) (v2 : t) : bool =
    match v1, v2 with
    | Mem addr1, Mem addr2 -> RIC.comparable_offsets addr1 addr2
    | _ -> true

  (** [is_covered ~by v] is [true] iff the memory region [v] is fully covered by
    the memory variables in [by]. Non-memory variables are ignored. *)
  let is_covered ~(by : t list) (v : t) : bool =
    (match v with
    | Mem v_addr ->
      by
      |> List.fold 
          ~init:[v_addr]
          ~f:(fun not_covered_yet covering_var ->
            match covering_var with
            | Var _ | Accessed | MemorySize -> not_covered_yet
            | Mem addr -> 
              not_covered_yet
              |> List.map ~f:(fun y -> RIC.remove ~this:addr ~from:y)
              |> List.concat)
    | _ -> assert false)
    |> List.filter ~f:(fun x -> RIC.(x <> Bottom))
    |> List.is_empty

  (** [update_relative_offset ~var ~actual_values] substitutes symbolic offset bases in [var]
      using [actual_values]. The returned boolean records whether a substituted base was
      associated with a non-singleton RIC. *)
  let update_relative_offset ~(var : t) ~(actual_values : RIC.t String.Map.t) : t * bool =
    match var with
    | Var _ | Accessed | MemorySize -> var, false
    | Mem address ->
      let used_non_singleton_relative =
        address
        |> RIC.extract_relative_offset
        |> String.split ~on:'+'
        |> List.map ~f:(fun str -> String.substr_replace_all str ~pattern:"neg" ~with_:"")
        |> List.fold ~init:false 
                     ~f:(fun acc v -> acc || (String.(v <> "") && (Map.find_exn actual_values v |> RIC.is_singleton |> not)))
      in
      (* if used_non_singleton_relative then Log.warn (fun () -> "used relative offset that is not a singleton"); *)
      Mem (RIC.update_relative_offset ~ric_:address ~actual_values), used_non_singleton_relative
end
include T

module Set = struct
  include Set
  (** Set of variables with test-friendly helpers. *)
  module S = struct
    include Set.Make(T)

    (** [to_string s] prints the variables in [s], separated by commas. *)
    let to_string (v : t) : string =
      v |> Set.to_list |> List.to_string ~f:to_string 
  end
  include Set
  include S
  include Test.Helpers(S)
end

module Map = struct
  include Map
  include Map.Make(T)

  (** Maps keyed by analysis variables. *)
  let to_string (m : 'a t) ~(f : 'a -> string) : string =
    "[" ^
    (m
    |> Map.to_alist
    |> List.map ~f:(fun (k, v) ->
      if String.equal (to_string k)  (f v) then
        to_string k
      else
        Printf.sprintf "%s ↦ %s" (to_string k) (f v))
    |> String.concat ~sep:";  ")
    ^ "]"

  (** [extract_memory_variables store] returns the memory keys of [store]. *)
  let extract_memory_variables (store : 'a t) : T.t list =
    store |> keys |> List.filter ~f:is_linear_memory

  (** [extract_locals_and_globals store] returns local and global variable keys. *)
  let extract_locals_and_globals (store : 'a t) : T.t list =
    store |> keys |> List.filter ~f:(fun v -> match v with | Var Var.Local _ | Var Var.Global _ -> true | _ -> false)

  (** [update_all store vars value] sets every variable in [vars] to [value]. *)
  let update_all (store : 'a t) (vars : Set.t) (new_value : 'a) : 'a t =
    vars |> Set.fold ~init:store ~f:(fun acc v -> set acc ~key:v ~data:new_value)

  (** [make_compatible ~this ~relative_to ~get] splits memory keys in [this]
    wherever they overlap memory keys in [relative_to]. Newly created keys
    keep the value returned by [get]. *)
  let make_compatible ~(this : 'a t) ~(relative_to : 'a t) ~(get : 'a t -> var:T.t -> 'a) : 'a t =
    let store1 = this in
    let store2 = relative_to in
    let mems2 = extract_memory_variables store2 in
    mems2 |> List.fold
              ~init:store1
              ~f:(fun new_store1 m2 ->
                let mems1 = extract_memory_variables new_store1 in
                match m2 with
                | Mem addr_m2 -> 
                  mems1 |> List.fold
                            ~init:new_store1
                            ~f:(fun store m1 ->
                              match m1 with
                              | Mem addr_m1 ->
                                let met_addrs = RIC.meet addr_m2 addr_m1 in
                                if RIC.(met_addrs = Bottom) then
                                  store
                                else
                                  let new_mem_vars = 
                                    met_addrs :: (RIC.remove ~this:met_addrs ~from:addr_m1)
                                    |> List.map ~f:(fun addr -> Mem addr) in
                                  let vs = get store ~var:m1 in
                                  let store = remove store m1 in
                                  update_all store (Set.of_list new_mem_vars) vs
                              | _ -> store )
                | _ -> new_store1)
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

  let test_label name = Printf.sprintf "%-34s" name

  let%test "Tests on variable module" =
    print_endline "_______ _______________ _______\n        Variable module        \n------- --------------- -------\n";
    true

  let%test "share_addresses yes" =
    let v1 = mem 1 0 4 ("", 0l) in
    let v2 = mem 1 2 6 ("", 0l) in
    let actual = share_addresses v1 v2 in
    let expected = true in
    print_endline (Printf.sprintf "%s %s, %s -> %s (expected %s)"
      (test_label "[Variable.share_addresses]")
      (to_string v1)
      (to_string v2)
      (Bool.to_string actual)
      (Bool.to_string expected));
    Bool.equal actual expected

  let%test "share_addresses no" =
    let v1 = mem 2 0 4 ("", 0l) in
    let v2 = mem 2 0 4 ("", 1l) in
    let actual = share_addresses v1 v2 in
    let expected = false in
    print_endline (Printf.sprintf "%s %s, %s -> %s (expected %s)"
      (test_label "[Variable.share_addresses]")
      (to_string v1)
      (to_string v2)
      (Bool.to_string actual)
      (Bool.to_string expected));
    Bool.equal actual expected

  let%test "remove full overlap" =
    let from = mem 1 0 4 ("", 0l) in
    let these = RIC.ric (1l, Int (-1l), Int 6l, ("", 0l)) in
    let actual = remove ~these_addresses:these ~from in
    let expected = [] in
    print_endline (Printf.sprintf "%s these:%s from:%s -> [%s] (expected [%s])"
      (test_label "[Variable.remove]")
      (RIC.to_string these) (to_string from)
      (String.concat ~sep:", " (List.map ~f:to_string actual))
      (String.concat ~sep:", " (List.map ~f:to_string expected)));
    List.equal equal actual expected

  let%test "remove some overlap" =
    let from = mem 1 0 4 ("", 0l) in
    let these = RIC.ric (2l, Int (0l), Int 1l, ("", 0l)) in
    let actual = remove ~these_addresses:these ~from in
    let expected = [mem 2 0 1 ("", 1l); mem 0 0 0 ("", 4l)] in
    print_endline (Printf.sprintf "%s these:%s from:%s -> [%s] (expected [%s])"
      (test_label "[Variable.remove]")
      (RIC.to_string these) (to_string from)
      (String.concat ~sep:", " (List.map ~f:to_string actual))
      (String.concat ~sep:", " (List.map ~f:to_string expected)));
    List.equal equal actual expected

  let%test "remove no overlap" =
    let from = mem 1 0 4 ("", 0l) in
    let these = RIC.ric (1l, Int 5l, Int 8l, ("", 0l)) in
    let actual = remove ~these_addresses:these ~from in
    let expected = [from] in
    print_endline (Printf.sprintf "%s these:%s from:%s -> [%s] (expected [%s])"
      (test_label "[Variable.remove]")
      (RIC.to_string these) (to_string from)
      (String.concat ~sep:", " (List.map ~f:to_string actual))
      (String.concat ~sep:", " (List.map ~f:to_string expected)));
    List.equal equal actual expected

  let%test "remove list of addresses" =
    let from = mem 1 0 4 ("", 0l) in
    let these = [RIC.ric (2l, Int (0l), Int 1l, ("", 0l)); RIC.ric (0l, Int 0l, Int 0l, ("", 3l)); RIC.ric (1l, Int 0l, Int 3l, ("", 37l))] in
    let actual = remove_all ~these_addresses_list:these ~from in
    let expected = [mem 0 0 0 ("", 1l); mem 0 0 0 ("", 4l)] in
    print_endline (Printf.sprintf "%s these:%s from:%s -> [%s] (expected [%s])"
      (test_label "[Variable.remove_all]")
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
    print_endline (Printf.sprintf "%s these:%s from:%s -> [%s] (expected [%s])"
      (test_label "[Variable.remove_all]")
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
    print_endline (Printf.sprintf "%s %s by:[%s] -> %s (expected %s)"
      (test_label "[Variable.is_covered]")
      (to_string target)
      (String.concat ~sep:", " (List.map ~f:to_string [cover1; cover2]))
      (Bool.to_string actual)
      (Bool.to_string expected));
    Bool.equal actual expected

  let%test "is_covered partially" =
    let target = mem 1 0 2 ("", 0l) in
    let cover1 = mem 1 0 0 ("", 0l) in
    let cover2 = mem 1 2 3 ("", 20l) in
    let actual = is_covered ~by:[cover1] target in
    let expected = false in
    print_endline (Printf.sprintf "%s %s by:[%s] -> %s (expected %s)"
      (test_label "[Variable.is_covered]")
      (to_string target)
      (String.concat ~sep:", " (List.map ~f:to_string [cover1; cover2]))
      (Bool.to_string actual)
      (Bool.to_string expected));
    Bool.equal actual expected

  let%test "update_relative_offset" =
    let original = mem 1 0 2 ("rel", 1l) in
    let values = String.Map.of_alist_exn [("rel", RIC.ric (1l, Int 10l, Int 10l, ("x", 0l)))] in
    let actual_var, actual_flag = update_relative_offset ~var:original ~actual_values:values in
    let expected_var = mem 1 0 2 ("x", 11l) in
    let expected_flag = false in
    print_endline (Printf.sprintf "%s %s -> (%s, %s) (expected (%s, %s))"
      (test_label "[Variable.update_relative_offset]")
      (to_string original)
      (to_string actual_var)
      (Bool.to_string actual_flag)
      (to_string expected_var)
      (Bool.to_string expected_flag));
    equal actual_var expected_var && Bool.equal actual_flag expected_flag

  let%test "update_relative_offset_2" =
    let original = mem 1 0 2 ("rel", 1l) in
    let values = String.Map.of_alist_exn [("rel", RIC.ric (1l, Int 10l, Int 11l, ("", 0l)))] in
    let actual_var, actual_flag = update_relative_offset ~var:original ~actual_values:values in
    let expected_var = mem 1 0 3 ("", 11l) in
    let expected_flag = true in
    print_endline (Printf.sprintf "%s %s with rel:%s -> (%s, %s) (expected (%s, %s))"
      (test_label "[Variable.update_relative_offset]")
      (to_string original)
      (RIC.to_string (Map.find_exn values "rel"))
      (to_string actual_var)
      (Bool.to_string actual_flag)
      (to_string expected_var)
      (Bool.to_string expected_flag));
    equal actual_var expected_var && Bool.equal actual_flag expected_flag

  let%test "update_relative_offset non-memory variable" =
    let original = Var (Var.Local 0) in
    let values = String.Map.of_alist_exn [("rel", RIC.ric (1l, Int 10l, Int 10l, ("", 0l)))] in
    let actual_var, actual_flag = update_relative_offset ~var:original ~actual_values:values in
    let expected_var = original in
    let expected_flag = false in
    print_endline (Printf.sprintf "%s %s with rel:%s -> (%s, %s) (expected (%s, %s))"
      (test_label "[Variable.update_relative_offset]")
      (to_string original)
      (RIC.to_string (Map.find_exn values "rel"))
      (to_string actual_var)
      (Bool.to_string actual_flag)
      (to_string expected_var)
      (Bool.to_string expected_flag));
    equal actual_var expected_var && Bool.equal actual_flag expected_flag

  let%test "update_relative_offset no relative offset" =
    let original = mem 1 0 2 ("", 5l) in
    let values = String.Map.of_alist_exn [("rel", RIC.ric (1l, Int 10l, Int 10l, ("", 0l)))] in
    let actual_var, actual_flag = update_relative_offset ~var:original ~actual_values:values in
    let expected_var = original in
    let expected_flag = false in
    print_endline (Printf.sprintf "%s %s with rel:%s -> (%s, %s) (expected (%s, %s))"
      (test_label "[Variable.update_relative_offset]")
      (to_string original)
      (RIC.to_string (Map.find_exn values "rel"))
      (to_string actual_var)
      (Bool.to_string actual_flag)
      (to_string expected_var)
      (Bool.to_string expected_flag));
    equal actual_var expected_var && Bool.equal actual_flag expected_flag

  let%test "update_relative_offset singleton concrete relative" =
    let original = mem 1 0 2 ("rel", 4l) in
    let values = String.Map.of_alist_exn [("rel", RIC.ric (1l, Int 7l, Int 7l, ("", 0l)))] in
    let actual_var, actual_flag = update_relative_offset ~var:original ~actual_values:values in
    let expected_var = mem 1 0 2 ("", 11l) in
    let expected_flag = false in
    print_endline (Printf.sprintf "%s %s with rel:%s -> (%s, %s) (expected (%s, %s))"
      (test_label "[Variable.update_relative_offset]")
      (to_string original)
      (RIC.to_string (Map.find_exn values "rel"))
      (to_string actual_var)
      (Bool.to_string actual_flag)
      (to_string expected_var)
      (Bool.to_string expected_flag));
    equal actual_var expected_var && Bool.equal actual_flag expected_flag

  let%test "update_relative_offset negative singleton relative" =
    let original = mem 1 0 2 ("negrel", 20l) in
    let values = String.Map.of_alist_exn [("rel", RIC.ric (1l, Int 7l, Int 7l, ("", 0l)))] in
    let actual_var, actual_flag = update_relative_offset ~var:original ~actual_values:values in
    let expected_var = mem 1 0 2 ("", 13l) in
    let expected_flag = false in
    print_endline (Printf.sprintf "%s %s with rel:%s -> (%s, %s) (expected (%s, %s))"
      (test_label "[Variable.update_relative_offset]")
      (to_string original)
      (RIC.to_string (Map.find_exn values "rel"))
      (to_string actual_var)
      (Bool.to_string actual_flag)
      (to_string expected_var)
      (Bool.to_string expected_flag));
    equal actual_var expected_var && Bool.equal actual_flag expected_flag

  let%test "update_relative_offset non-singleton symbolic relative" =
    let original = mem 1 0 2 ("rel", 4l) in
    let values = String.Map.of_alist_exn [("rel", RIC.ric (1l, Int 7l, Int 9l, ("x", 0l)))] in
    let actual_var, actual_flag = update_relative_offset ~var:original ~actual_values:values in
    let expected_var = mem 1 0 4 ("x", 11l) in
    let expected_flag = true in
    print_endline (Printf.sprintf "%s %s with rel:%s -> (%s, %s) (expected (%s, %s))"
      (test_label "[Variable.update_relative_offset]")
      (to_string original)
      (RIC.to_string (Map.find_exn values "rel"))
      (to_string actual_var)
      (Bool.to_string actual_flag)
      (to_string expected_var)
      (Bool.to_string expected_flag));
    equal actual_var expected_var && Bool.equal actual_flag expected_flag

  let%test "update_relative_offset negative non-singleton concrete relative" =
    let original = mem 1 0 2 ("negrel", 20l) in
    let values = String.Map.of_alist_exn [("rel", RIC.ric (1l, Int 7l, Int 9l, ("", 0l)))] in
    let actual_var, actual_flag = update_relative_offset ~var:original ~actual_values:values in
    let expected_var = mem 1 0 4 ("", 11l) in
    let expected_flag = true in
    print_endline (Printf.sprintf "%s %s with rel:%s -> (%s, %s) (expected (%s, %s))"
      (test_label "[Variable.update_relative_offset]")
      (to_string original)
      (RIC.to_string (Map.find_exn values "rel"))
      (to_string actual_var)
      (Bool.to_string actual_flag)
      (to_string expected_var)
      (Bool.to_string expected_flag));
    equal actual_var expected_var && Bool.equal actual_flag expected_flag

  let%test "update_relative_offset multiple singleton relatives" =
    let original = mem 1 0 2 ("rel+other", 5l) in
    let values =
      String.Map.of_alist_exn
        [ ("rel", RIC.ric (1l, Int 7l, Int 7l, ("", 0l)))
        ; ("other", RIC.ric (1l, Int 3l, Int 3l, ("", 0l)))
        ]
    in
    let actual_var, actual_flag = update_relative_offset ~var:original ~actual_values:values in
    let expected_var = mem 1 0 2 ("", 15l) in
    let expected_flag = false in
    print_endline (Printf.sprintf "%s %s with rel:%s, other:%s -> (%s, %s) (expected (%s, %s))"
      (test_label "[Variable.update_relative_offset]")
      (to_string original)
      (RIC.to_string (Map.find_exn values "rel"))
      (RIC.to_string (Map.find_exn values "other"))
      (to_string actual_var)
      (Bool.to_string actual_flag)
      (to_string expected_var)
      (Bool.to_string expected_flag));
    equal actual_var expected_var && Bool.equal actual_flag expected_flag

  let%test "make_compatible no overlap" =
    let m1 = mem 1 0 2 ("", 0l) in
    let m2 = mem 1 10 12 ("", 0l) in
    let this = Map.of_alist_exn [(m1, RIC.ric (0l, Int 0l, Int 0l, ("", 42l)))] in
    let relative_to = Map.of_alist_exn [(m2, RIC.Top)] in
    let actual = Map.make_compatible ~this ~relative_to ~get:(fun store ~var -> Map.find_exn store var) in
    let expected = this in
    print_endline (Printf.sprintf "%s this:(%s) relative_to:(%s)   ->   %s   (expected %s)"
      (test_label "[Variable.Map.make_compatible]")
      (Map.to_string this ~f:RIC.to_string)
      (Map.to_string relative_to ~f:RIC.to_string)
      (Map.to_string actual ~f:RIC.to_string)
      (Map.to_string expected ~f:RIC.to_string));
    Map.equal RIC.equal actual expected

  let%test "make_compatible partial overlap" =
    let m1 = mem 1 0 4 ("", 0l) in
    let m2 = mem 1 2 3 ("", 0l) in
    let value = RIC.ric (0l, Int 0l, Int 0l, ("", 42l)) in
    let this = Map.of_alist_exn [(m1, value)] in
    let relative_to = Map.of_alist_exn [(m2, RIC.Top)] in
    let actual = Map.make_compatible ~this ~relative_to ~get:(fun store ~var -> Map.find_exn store var) in
    let expected =
      Map.of_alist_exn
        [ (mem 1 2 3 ("", 0l), value)
        ; (mem 1 0 1 ("", 0l), value)
        ; (mem 1 4 4 ("", 0l), value)
        ]
    in
    print_endline (Printf.sprintf "%s this:(%s) relative_to:(%s)   ->   %s   (expected %s)"
      (test_label "[Variable.Map.make_compatible]")
      (Map.to_string this ~f:RIC.to_string)
      (Map.to_string relative_to ~f:RIC.to_string)
      (Map.to_string actual ~f:RIC.to_string)
      (Map.to_string expected ~f:RIC.to_string));
    Map.equal RIC.equal actual expected

  let%test "make_compatible full overlap" =
    let m = mem 1 0 4 ("", 0l) in
    let value = RIC.ric (0l, Int 0l, Int 0l, ("", 42l)) in
    let this = Map.of_alist_exn [(m, value)] in
    let relative_to = Map.of_alist_exn [(m, RIC.Top)] in
    let actual = Map.make_compatible ~this ~relative_to ~get:(fun store ~var -> Map.find_exn store var) in
    let expected = this in
    print_endline (Printf.sprintf "%s this:(%s) relative_to:(%s)   ->   %s   (expected %s)"
      (test_label "[Variable.Map.make_compatible]")
      (Map.to_string this ~f:RIC.to_string)
      (Map.to_string relative_to ~f:RIC.to_string)
      (Map.to_string actual ~f:RIC.to_string)
      (Map.to_string expected ~f:RIC.to_string));
    Map.equal RIC.equal actual expected

  let%test "make_compatible left overlap" =
    let m1 = mem 1 0 4 ("", 0l) in
    let m2 = mem 1 0 2 ("", 0l) in
    let value = RIC.ric (0l, Int 0l, Int 0l, ("", 42l)) in
    let this = Map.of_alist_exn [(m1, value)] in
    let relative_to = Map.of_alist_exn [(m2, RIC.Top)] in
    let actual = Map.make_compatible ~this ~relative_to ~get:(fun store ~var -> Map.find_exn store var) in
    let expected =
      Map.of_alist_exn
        [ (mem 1 0 2 ("", 0l), value)
        ; (mem 1 3 4 ("", 0l), value)
        ]
    in
    print_endline (Printf.sprintf "%s this:(%s) relative_to:(%s)   ->   %s   (expected %s)"
      (test_label "[Variable.Map.make_compatible]")
      (Map.to_string this ~f:RIC.to_string)
      (Map.to_string relative_to ~f:RIC.to_string)
      (Map.to_string actual ~f:RIC.to_string)
      (Map.to_string expected ~f:RIC.to_string));
    Map.equal RIC.equal actual expected

  let%test "make_compatible right overlap" =
    let m1 = mem 1 0 4 ("", 0l) in
    let m2 = mem 1 2 4 ("", 0l) in
    let value = RIC.ric (0l, Int 0l, Int 0l, ("", 42l)) in
    let this = Map.of_alist_exn [(m1, value)] in
    let relative_to = Map.of_alist_exn [(m2, RIC.Top)] in
    let actual = Map.make_compatible ~this ~relative_to ~get:(fun store ~var -> Map.find_exn store var) in
    let expected =
      Map.of_alist_exn
        [ (mem 1 2 4 ("", 0l), value)
        ; (mem 1 0 1 ("", 0l), value)
        ]
    in
    print_endline (Printf.sprintf "%s this:(%s) relative_to:(%s)   ->   %s   (expected %s)"
      (test_label "[Variable.Map.make_compatible]")
      (Map.to_string this ~f:RIC.to_string)
      (Map.to_string relative_to ~f:RIC.to_string)
      (Map.to_string actual ~f:RIC.to_string)
      (Map.to_string expected ~f:RIC.to_string));
    Map.equal RIC.equal actual expected

  let%test "make_compatible preserves unrelated memory" =
    let m1 = mem 1 0 4 ("", 0l) in
    let m2 = mem 1 2 3 ("", 0l) in
    let unrelated = mem 1 10 12 ("", 0l) in
    let value1 = RIC.ric (0l, Int 0l, Int 0l, ("", 42l)) in
    let value2 = RIC.ric (0l, Int 0l, Int 0l, ("", 99l)) in
    let this = Map.of_alist_exn [(m1, value1); (unrelated, value2)] in
    let relative_to = Map.of_alist_exn [(m2, RIC.Top)] in
    let actual = Map.make_compatible ~this ~relative_to ~get:(fun store ~var -> Map.find_exn store var) in
    let expected =
      Map.of_alist_exn
        [ (mem 1 2 3 ("", 0l), value1)
        ; (mem 1 0 1 ("", 0l), value1)
        ; (mem 1 4 4 ("", 0l), value1)
        ; (unrelated, value2)
        ]
    in
    print_endline (Printf.sprintf "%s this:(%s) relative_to:(%s)   ->   %s   (expected %s)"
      (test_label "[Variable.Map.make_compatible]")
      (Map.to_string this ~f:RIC.to_string)
      (Map.to_string relative_to ~f:RIC.to_string)
      (Map.to_string actual ~f:RIC.to_string)
      (Map.to_string expected ~f:RIC.to_string));
    Map.equal RIC.equal actual expected

  let%test "make_compatible multiple relative memories" =
    let m = mem 1 0 6 ("", 0l) in
    let split1 = mem 1 1 2 ("", 0l) in
    let split2 = mem 1 4 5 ("", 0l) in
    let value = RIC.one in
    let this = Map.of_alist_exn [(m, value)] in
    let relative_to = Map.of_alist_exn [(split1, RIC.Top); (split2, RIC.Top)] in
    let actual = Map.make_compatible ~this ~relative_to ~get:(fun store ~var -> Map.find_exn store var) in
    let expected =
      Map.of_alist_exn
        [ (mem 1 1 2 ("", 0l), value)
        ; (mem 1 4 5 ("", 0l), value)
        ; (mem 0 0 0 ("", 0l), value)
        ; (mem 0 0 0 ("", 3l), value)
        ; (mem 0 0 0 ("", 6l), value)
        ]
    in
    print_endline (Printf.sprintf "%s this:(%s) relative_to:(%s)   ->   %s   (expected %s)"
      (test_label "[Variable.Map.make_compatible]")
      (Map.to_string this ~f:RIC.to_string)
      (Map.to_string relative_to ~f:RIC.to_string)
      (Map.to_string actual ~f:RIC.to_string)
      (Map.to_string expected ~f:RIC.to_string));
    Map.equal RIC.equal actual expected

  let%test "make_compatible splits multiple memories in this" =
    let m1 = mem 1 0 4 ("", 0l) in
    let m2 = mem 1 10 14 ("", 0l) in
    let split = mem 1 2 12 ("", 0l) in
    let value = RIC.ric (0l, Int 0l, Int 0l, ("", 1l)) in
    let this = Map.of_alist_exn [(m1, value); (m2, value)] in
    let relative_to = Map.of_alist_exn [(split, RIC.Top)] in
    let actual = Map.make_compatible ~this ~relative_to ~get:(fun store ~var -> Map.find_exn store var) in
    let expected =
      Map.of_alist_exn
        [ (mem 1 2 4 ("", 0l), value)
        ; (mem 1 0 1 ("", 0l), value)
        ; (mem 1 10 12 ("", 0l), value)
        ; (mem 1 13 14 ("", 0l), value)
        ]
    in
    print_endline (Printf.sprintf "%s this:(%s) relative_to:(%s)   ->   %s   (expected %s)"
      (test_label "[Variable.Map.make_compatible]")
      (Map.to_string this ~f:RIC.to_string)
      (Map.to_string relative_to ~f:RIC.to_string)
      (Map.to_string actual ~f:RIC.to_string)
      (Map.to_string expected ~f:RIC.to_string));
    Map.equal RIC.equal actual expected

  let%test "make_compatible incompatible relative offsets" =
    let m1 = mem 1 0 4 ("x", 0l) in
    let m2 = mem 1 2 3 ("y", 0l) in
    let value = RIC.ric (0l, Int 0l, Int 0l, ("", 1l)) in
    let this = Map.of_alist_exn [(m1, value)] in
    let relative_to = Map.of_alist_exn [(m2, RIC.Top)] in
    let actual = Map.make_compatible ~this ~relative_to ~get:(fun _ ~var:_ -> value) in
    let expected = this in
    print_endline (Printf.sprintf "%s this:(%s) relative_to:(%s)   ->   %s   (expected %s)"
      (test_label "[Variable.Map.make_compatible]")
      (Map.to_string this ~f:RIC.to_string)
      (Map.to_string relative_to ~f:RIC.to_string)
      (Map.to_string actual ~f:RIC.to_string)
      (Map.to_string expected ~f:RIC.to_string));
    Map.equal RIC.equal actual expected

  let%test "make_compatible symbolic overlap" =
    let m1 = mem 1 0 4 ("x", 0l) in
    let m2 = mem 1 2 3 ("x", 0l) in
    let value = RIC.ric (0l, Int 0l, Int 0l, ("", 42l)) in
    let this = Map.of_alist_exn [(m1, value)] in
    let relative_to = Map.of_alist_exn [(m2, RIC.Top)] in
    let actual = Map.make_compatible ~this ~relative_to ~get:(fun _ ~var:_ -> value) in
    let expected =
      Map.of_alist_exn
        [ (mem 1 2 3 ("x", 0l), value)
        ; (mem 1 0 1 ("x", 0l), value)
        ; (mem 0 0 0 ("x", 4l), value)
        ]
    in
    print_endline (Printf.sprintf "%s this:(%s) relative_to:(%s)   ->   %s   (expected %s)"
      (test_label "[Variable.Map.make_compatible]")
      (Map.to_string this ~f:RIC.to_string)
      (Map.to_string relative_to ~f:RIC.to_string)
      (Map.to_string actual ~f:RIC.to_string)
      (Map.to_string expected ~f:RIC.to_string));
    Map.equal RIC.equal actual expected
    

  let%test "make_compatible symbolic overlap with offset" =
    let m1 = mem 1 0 4 ("x", 5l) in
    let m2 = mem 1 2 3 ("x", 5l) in
    let value = RIC.ric (0l, Int 0l, Int 0l, ("", 42l)) in
    let this = Map.of_alist_exn [(m1, value)] in
    let relative_to = Map.of_alist_exn [(m2, RIC.Top)] in
    let actual = Map.make_compatible ~this ~relative_to ~get:(fun _ ~var:_ -> value) in
    let expected =
      Map.of_alist_exn
        [ (mem 1 2 3 ("x", 5l), value)
        ; (mem 1 0 1 ("x", 5l), value)
        ; (mem 0 0 0 ("x", 9l), value)
        ]
    in
    print_endline (Printf.sprintf "%s this:(%s) relative_to:(%s)   ->   %s   (expected %s)"
      (test_label "[Variable.Map.make_compatible]")
      (Map.to_string this ~f:RIC.to_string)
      (Map.to_string relative_to ~f:RIC.to_string)
      (Map.to_string actual ~f:RIC.to_string)
      (Map.to_string expected ~f:RIC.to_string));
    Map.equal RIC.equal actual expected

  let%test "comparable_offsets same relative offset" =
    let v1 = mem 1 0 4 ("x", 0l) in
    let v2 = mem 1 2 6 ("x", 10l) in
    let actual = comparable_offsets v1 v2 in
    let expected = true in
    print_endline (Printf.sprintf "%s %s, %s -> %s (expected %s)"
      (test_label "[Variable.comparable_offsets]")
      (to_string v1)
      (to_string v2)
      (Bool.to_string actual)
      (Bool.to_string expected));
    Bool.equal actual expected

  let%test "comparable_offsets different relative offsets" =
    let v1 = mem 1 0 4 ("x", 0l) in
    let v2 = mem 1 2 6 ("y", 0l) in
    let actual = comparable_offsets v1 v2 in
    let expected = false in
    print_endline (Printf.sprintf "%s %s, %s -> %s (expected %s)"
      (test_label "[Variable.comparable_offsets]")
      (to_string v1)
      (to_string v2)
      (Bool.to_string actual)
      (Bool.to_string expected));
    Bool.equal actual expected

  let%test "is_infinite finite memory" =
    let v = mem 1 0 4 ("", 0l) in
    let actual = is_infinite v in
    let expected = false in
    print_endline (Printf.sprintf "%s %s -> %s (expected %s)"
      (test_label "[Variable.is_infinite]")
      (to_string v)
      (Bool.to_string actual)
      (Bool.to_string expected));
    Bool.equal actual expected

  let%test "is_covered with overlap between cover variables" =
    let target = mem 1 0 4 ("", 0l) in
    let cover1 = mem 1 0 2 ("", 0l) in
    let cover2 = mem 1 2 4 ("", 0l) in
    let actual = is_covered ~by:[cover1; cover2] target in
    let expected = true in
    print_endline (Printf.sprintf "%s %s by:[%s] -> %s (expected %s)"
      (test_label "[Variable.is_covered]")
      (to_string target)
      (String.concat ~sep:", " (List.map ~f:to_string [cover1; cover2]))
      (Bool.to_string actual)
      (Bool.to_string expected));
    Bool.equal actual expected

  let%test "is_covered symbolic addresses" =
    let target = mem 1 0 4 ("x", 0l) in
    let cover1 = mem 1 0 1 ("x", 0l) in
    let cover2 = mem 1 2 4 ("x", 0l) in
    let actual = is_covered ~by:[cover1; cover2] target in
    let expected = true in
    print_endline (Printf.sprintf "%s %s by:[%s] -> %s (expected %s)"
      (test_label "[Variable.is_covered]")
      (to_string target)
      (String.concat ~sep:", " (List.map ~f:to_string [cover1; cover2]))
      (Bool.to_string actual)
      (Bool.to_string expected));
    Bool.equal actual expected

  let%test "is_covered symbolic addresses with gap" =
    let target = mem 1 0 4 ("x", 0l) in
    let cover1 = mem 1 0 1 ("x", 0l) in
    let cover2 = mem 1 3 4 ("x", 0l) in
    let actual = is_covered ~by:[cover1; cover2] target in
    let expected = false in
    print_endline (Printf.sprintf "%s %s by:[%s] -> %s (expected %s)"
      (test_label "[Variable.is_covered]")
      (to_string target)
      (String.concat ~sep:", " (List.map ~f:to_string [cover1; cover2]))
      (Bool.to_string actual)
      (Bool.to_string expected));
    Bool.equal actual expected

  let%test "is_covered ignores non-memory variables" =
    let target = mem 1 0 4 ("", 0l) in
    let cover = mem 1 0 2 ("", 0l) in
    let actual =
      is_covered
        ~by:[cover; Var (Var.Local 0); Accessed; MemorySize]
        target
    in
    let expected = false in
    print_endline (Printf.sprintf "%s %s by:[%s] -> %s (expected %s)"
      (test_label "[Variable.is_covered]")
      (to_string target)
      (String.concat ~sep:", "
        (List.map ~f:to_string [cover; Var (Var.Local 0); Accessed; MemorySize]))
      (Bool.to_string actual)
      (Bool.to_string expected));
    Bool.equal actual expected

  let%test "share_addresses symbolic overlap" =
    let v1 = mem 1 0 4 ("x", 0l) in
    let v2 = mem 1 2 6 ("x", 0l) in
    let actual = share_addresses v1 v2 in
    let expected = true in
    print_endline (Printf.sprintf "%s %s, %s -> %s (expected %s)"
      (test_label "[Variable.share_addresses]")
      (to_string v1)
      (to_string v2)
      (Bool.to_string actual)
      (Bool.to_string expected));
    Bool.equal actual expected

  let%test "share_addresses symbolic no overlap" =
    let v1 = mem 1 0 4 ("x", 0l) in
    let v2 = mem 1 5 8 ("x", 0l) in
    let actual = share_addresses v1 v2 in
    let expected = false in
    print_endline (Printf.sprintf "%s %s, %s -> %s (expected %s)"
      (test_label "[Variable.share_addresses]")
      (to_string v1)
      (to_string v2)
      (Bool.to_string actual)
      (Bool.to_string expected));
    Bool.equal actual expected

  let%test "share_addresses non-memory variable" =
    let v1 = Var (Var.Local 0) in
    let v2 = mem 1 0 4 ("", 0l) in
    let actual = share_addresses v1 v2 in
    let expected = false in
    print_endline (Printf.sprintf "%s %s, %s -> %s (expected %s)"
      (test_label "[Variable.share_addresses]")
      (to_string v1)
      (to_string v2)
      (Bool.to_string actual)
      (Bool.to_string expected));
    Bool.equal actual expected

  let%test "remove_all overlapping removals" =
    let from = mem 1 0 6 ("", 0l) in
    let these =
      [ RIC.ric (1l, Int 1l, Int 3l, ("", 0l))
      ; RIC.ric (1l, Int 3l, Int 5l, ("", 0l))
      ]
    in
    let actual = remove_all ~these_addresses_list:these ~from in
    let expected = [mem 0 0 0 ("", 0l); mem 0 0 0 ("", 6l)] in
    print_endline (Printf.sprintf "%s these:%s from:%s -> [%s] (expected [%s])"
      (test_label "[Variable.remove_all]")
      (String.concat ~sep:"; " (List.map ~f:RIC.to_string these))
      (to_string from)
      (String.concat ~sep:", " (List.map ~f:to_string actual))
      (String.concat ~sep:", " (List.map ~f:to_string expected)));
    List.equal equal actual expected

  let%test "remove_all empty removal list" =
    let from = mem 1 0 4 ("", 0l) in
    let these = [] in
    let actual = remove_all ~these_addresses_list:these ~from in
    let expected = [from] in
    print_endline (Printf.sprintf "%s these:%s from:%s -> [%s] (expected [%s])"
      (test_label "[Variable.remove_all]")
      (String.concat ~sep:"; " (List.map ~f:RIC.to_string these))
      (to_string from)
      (String.concat ~sep:", " (List.map ~f:to_string actual))
      (String.concat ~sep:", " (List.map ~f:to_string expected)));
    List.equal equal actual expected

  let%test "remove_all full coverage by pieces" =
    let from = mem 1 0 4 ("", 0l) in
    let these =
      [ RIC.ric (1l, Int 0l, Int 1l, ("", 0l))
      ; RIC.ric (1l, Int 2l, Int 4l, ("", 0l))
      ]
    in
    let actual = remove_all ~these_addresses_list:these ~from in
    let expected = [] in
    print_endline (Printf.sprintf "%s these:%s from:%s -> [%s] (expected [%s])"
      (test_label "[Variable.remove_all]")
      (String.concat ~sep:"; " (List.map ~f:RIC.to_string these))
      (to_string from)
      (String.concat ~sep:", " (List.map ~f:to_string actual))
      (String.concat ~sep:", " (List.map ~f:to_string expected)));
    List.equal equal actual expected

  let%test "remove_all symbolic removals" =
    let from = mem 1 0 6 ("x", 0l) in
    let these =
      [ RIC.ric (1l, Int 1l, Int 2l, ("x", 0l))
      ; RIC.ric (1l, Int 4l, Int 5l, ("x", 0l))
      ]
    in
    let actual = remove_all ~these_addresses_list:these ~from in
    let expected =
      [ mem 0 0 0 ("x", 0l)
      ; mem 0 0 0 ("x", 3l)
      ; mem 0 0 0 ("x", 6l)
      ]
    in
    print_endline (Printf.sprintf "%s these:%s from:%s -> [%s] (expected [%s])"
      (test_label "[Variable.remove_all]")
      (String.concat ~sep:"; " (List.map ~f:RIC.to_string these))
      (to_string from)
      (String.concat ~sep:", " (List.map ~f:to_string actual))
      (String.concat ~sep:", " (List.map ~f:to_string expected)));
    List.equal equal actual expected

  let%test "update_all updates selected variables" =
    let v1 = Var (Var.Local 0) in
    let v2 = Var (Var.Local 1) in
    let v3 = Var (Var.Local 2) in
    let old_value = RIC.ric (0l, Int 0l, Int 0l, ("", 1l)) in
    let new_value = RIC.ric (0l, Int 0l, Int 0l, ("", 42l)) in
    let store = Map.of_alist_exn [(v1, old_value); (v2, old_value); (v3, old_value)] in
    let vars = Set.of_list [v1; v3] in
    let actual = Map.update_all store vars new_value in
    let expected = Map.of_alist_exn [(v1, new_value); (v2, old_value); (v3, new_value)] in
    print_endline (Printf.sprintf "%s store:(%s) vars:(%s) value:%s -> %s (expected %s)"
      (test_label "[Variable.Map.update_all]")
      (Map.to_string store ~f:RIC.to_string)
      (Set.to_string vars)
      (RIC.to_string new_value)
      (Map.to_string actual ~f:RIC.to_string)
      (Map.to_string expected ~f:RIC.to_string));
    Map.equal RIC.equal actual expected
end)