(** This module defines variables used in pointer analysis, which can be either program variables
    or memory blocks identified by base addresses and offsets.

    Memory blocks can be either at absolute addresses (e.g., linear memory in WebAssembly)
    or at relative symbolic addresses (e.g., stack or global base identifiers).
    It also includes a function to check for overlap between two memory blocks,
    and a string conversion function to render variables in human-readable form. *)

open Core

open Reduced_interval_congruence

open Maths


module T = struct
  (** A variable is either:
      - [Var v]: a regular program variable
      - [Mem ric]: a memory block as defined by the [ric] module *)
  type t = 
    | Var of Var.t
    | Mem of RIC.t
    | Stack of RIC.t (* used when considering the stack disjoint from the rest of the memory *)
    (* | Affected used to store all the addresses that have been affected by a function *)
    | Accessed (* used to store all the addresses that have been read by a function *)
  [@@deriving sexp, compare, equal]

  (** Converts a variable to a human-readable string representation. Memory blocks are printed as ranges. *)
  let to_string (var : t) : string =
    match var with 
    | Var v -> Var.to_string v
    | Mem ric -> "mem[" ^ RIC.to_string ric ^ "]"
    | Stack ric -> "stack[" ^ RIC.to_string (RIC.remove_relative_offset ric) ^"]"
    (* | Affected -> "Affected_memory" *)
    | Accessed -> "Accessed_memory"

  let mem (ric : int * ExtendedInt.t * ExtendedInt.t * (string * int)) : t =
    Mem (RIC.ric ric)

  let entire_memory = Mem RIC.Top

  let entire_stack = Stack RIC.Top

  let is_linear_memory (v : t) : bool =
    match v with
    | Mem _ -> true
    | _ -> false
  
  let is_stack (v : t) : bool =
    match v with
    | Stack _ -> true
    | _ -> false

  let is_global (v : t) : bool =
    match v with
    | Var Var.Global _ -> true
    | _ -> false

  let get_address (var : t) : RIC.t =
    match var with
    | Mem address -> address
    | Stack address -> address
    | _ -> RIC.Bottom

  let get_relative_offset (v : t) : string =
    match v with
    | Mem RIC {offset = (offset, _); _} -> offset
    | Stack _ -> "g0"
    | _ -> ""

  let list_to_string (vars : t list) : string = String.concat ~sep:", " (List.map vars ~f:to_string)

  let is_infinite (v : t) : bool =
    match v with
    | Var _ -> false
    | Mem RIC.Top -> true
    | Mem RIC {stride = _; lower_bound = l; upper_bound = u; offset = _}
        when ExtendedInt.equal l NegInfinity || ExtendedInt.equal u Infinity -> true
    | Stack RIC {stride = _; lower_bound = l; upper_bound = u; offset = _}
        when ExtendedInt.equal l NegInfinity || ExtendedInt.equal u Infinity -> true
    | _ -> false

  let is_finite (var : t) : bool = not (is_infinite var)

  let is_singleton (var : t) : bool =
    match var with
    | Mem RIC {lower_bound = l; upper_bound = u; _} when not (ExtendedInt.equal l u) -> false
    | Stack RIC {lower_bound = l; upper_bound = u; _} when not (ExtendedInt.equal l u) -> false
    | _ -> true

  let share_addresses (var1 : t) (var2 : t) : bool =
    match var1, var2 with
    | Mem ric1, Mem ric2 -> not (RIC.equal RIC.Bottom (RIC.meet ric1 ric2))
    | Stack ric1, Stack ric2 -> not (RIC.equal RIC.Bottom (RIC.meet ric1 ric2))
    | _ -> false

  let remove ~(these_addresses : RIC.t) ~(from : t) : t list =
    let address = get_address from in
    let truncated_addresses = RIC.remove ~this:these_addresses ~from:address in
    List.map truncated_addresses ~f:(fun addr -> Mem addr)

  let remove_all ~(these_addresses_list : RIC.t list) ~(from : t) : t list =
    List.fold these_addresses_list ~init:[from] ~f:(fun acc ric ->
      List.concat_map acc ~f:(fun var -> remove ~these_addresses:ric ~from:var)
    )

  let comparable_offsets (v1 : t) (v2 : t) : bool =
    match v1, v2 with
    | Mem addr1, Mem addr2 -> RIC.comparable_offsets addr1 addr2
    | _ -> true

  (* TODO: Test this function *)
  let is_covered ~(by : t list) (v : t) : bool =
    let not_covered =
      match v with
      | Mem v_addr ->
        (* let v_addr = match v with | Mem r -> r | _ -> assert false in *)
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
  module S = struct
    include Set.Make(T)
    let to_string (v : t) : string =
      String.concat ~sep:"," (List.map ~f:to_string (Set.to_list v))
    
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
  let to_string (m : 'a t) (f : 'a -> string) : string =
    String.concat ~sep:", " (List.map (Map.to_alist m) ~f:(fun (k, v) -> Printf.sprintf "%s ↦ %s" (to_string k) (f v)))

  (** [extract_memory_variables store] returns all memory-related variables in [store]. *)
  let extract_memory_variables (store : 'a t) : T.t list =
    let all_vars = keys store in
    List.filter ~f:T.is_linear_memory all_vars

  let extract_stack_variables (store : 'a t) : T.t list =
    let all_vars = keys store in
    List.filter ~f:T.is_stack all_vars

  let extract_locals_and_globals (store : 'a t) : T.t list =
    let all_vars = keys store in
    List.filter ~f:(fun v -> match v with | Var Var.Local _ | Var Var.Global _ -> true | _ -> false) all_vars

  (** [update_all store vars ric] sets [ric] for each variable in [vars] within [store]. *)
  let update_all (store : 'a t) (vars : Set.t) (new_value : 'a) : 'a t =
    Set.fold vars ~init:store ~f:(fun acc v -> set acc ~key:v ~data:new_value)

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
    mem (addr, Int lo, Int hi, offset)

  let%test "Tests on variable module" =
    print_endline "_______ _______________ _______\n        Variable module        \n------- --------------- -------\n";
    true

  (* let%test "to_singletons simple" =
    let v = mem 0 0 0 ("stack", 0) in
    let result = to_singletons v in
    print_endline ("[Variable.to_singletons]     " ^ to_string v ^ " -> " ^ String.concat ~sep:", " (List.map ~f:to_string result));
    List.length result = 1

  let%test "to_singletons multiple" =
    let v = mem 1 0 4 ("stack", 0) in
    let result = to_singletons v in
    print_endline ("[Variable.to_singletons]     " ^ to_string v ^ " -> " ^ String.concat ~sep:", " (List.map ~f:to_string result));
    List.length result = 5 *)

  let%test "share_addresses yes" =
    let v1 = mem 1 0 4 ("", 0) in
    let v2 = mem 1 2 6 ("", 0) in
    let result = share_addresses v1 v2 in
    print_endline ("[Variable.share_addresses]     " ^ to_string v1 ^ ", " ^ to_string v2 ^ " -> " ^ Bool.to_string result);
    result

  let%test "share_addresses no" =
    let v1 = mem 2 0 4 ("", 0) in
    let v2 = mem 2 0 4 ("", 1) in
    let result = share_addresses v1 v2 in
    print_endline ("[Variable.share_addresses]     " ^ to_string v1 ^ ", " ^ to_string v2 ^ " -> " ^ Bool.to_string result);
    not result

  let%test "remove full overlap" =
    let from = mem 1 0 4 ("", 0) in
    let these = RIC.ric (1, Int (-1), Int 6, ("", 0)) in
    let result = remove ~these_addresses:these ~from in
    print_endline ("[Variable.remove]     these:" ^ RIC.to_string these ^ " from:" ^ to_string from ^ " -> [" ^ String.concat ~sep:", " (List.map ~f:to_string result) ^ "]");
    List.equal equal result []

  let%test "remove some overlap" =
    let from = mem 1 0 4 ("", 0) in
    let these = RIC.ric (2, Int (0), Int 1, ("", 0)) in
    let result = remove ~these_addresses:these ~from in
    print_endline ("[Variable.remove]     these:" ^ RIC.to_string these ^ " from:" ^ to_string from ^ " -> [" ^ String.concat ~sep:", " (List.map ~f:to_string result) ^ "]");
    List.equal equal result [mem 2 0 1 ("", 1); mem 0 0 0 ("", 4)]

  let%test "remove no overlap" =
    let from = mem 1 0 4 ("", 0) in
    let these = RIC.ric (1, Int 5, Int 8, ("", 0)) in
    let result = remove ~these_addresses:these ~from in
    print_endline ("[Variable.remove]     these:" ^ RIC.to_string these ^ " from:" ^ to_string from ^ " -> [" ^ String.concat ~sep:", " (List.map ~f:to_string result) ^ "]");
     List.equal equal result [from]

  let%test "remove list of addresses" =
    let from = mem 1 0 4 ("", 0) in
    let these = [RIC.ric (2, Int (0), Int 1, ("", 0)); RIC.ric (0, Int 0, Int 0, ("", 3)); RIC.ric (1, Int 0, Int 3, ("", 37))] in
    let result = remove_all ~these_addresses_list:these ~from in
    print_endline ("[Variable.remove_all]     these: [" ^ String.concat ~sep:"; " (List.map ~f:RIC.to_string these) ^ "] from:" ^ to_string from ^ " -> [" ^ String.concat ~sep:", " (List.map ~f:to_string result) ^ "]");
    List.equal equal result [mem 0 0 0 ("", 1); mem 0 0 0 ("", 4)]

  let%test "remove list of incompatible addresses" =
    let from = mem 1 0 4 ("", 0) in
    let these = [RIC.ric (2, Int (0), Int 1, ("a", 0)); RIC.ric (0, Int 0, Int 0, ("", 3)); RIC.ric (1, Int 0, Int 3, ("a", 37))] in
    let result = remove_all ~these_addresses_list:these ~from in
    print_endline ("[Variable.remove_all]     these: [" ^ String.concat ~sep:"; " (List.map ~f:RIC.to_string these) ^ "] from:" ^ to_string from ^ " -> [" ^ String.concat ~sep:", " (List.map ~f:to_string result) ^ "]");
    List.equal equal result []

  let%test "is_covered fully" =
    let target = mem 1 0 2 ("", 0) in
    let cover1 = mem 1 0 1 ("", 0) in
    let cover2 = mem 1 2 3 ("", 0) in
    let result = is_covered ~by:[cover1; cover2] target in
    print_endline ("[Variable.is_covered]     " ^ to_string target ^ " by:[" ^ List.to_string ~f:to_string [cover1; cover2] ^ "] -> " ^ Bool.to_string result);
    result

  let%test "is_covered partially" =
    let target = mem 1 0 2 ("", 0) in
    let cover1 = mem 1 0 0 ("", 0) in
    let cover2 = mem 1 2 3 ("", 20) in
    let result = is_covered ~by:[cover1] target in
    print_endline ("[Variable.is_covered]     " ^ to_string target ^ " by:[" ^ List.to_string ~f:to_string [cover1; cover2] ^ "] -> " ^ Bool.to_string result);
    not result

  let%test "update_relative_offset" =
    let original = mem 1 0 2 ("rel", 1) in
    let values = String.Map.of_alist_exn [("rel", RIC.ric (1, Int 10, Int 10, ("x", 0)))] in
    let updated = update_relative_offset ~var:original ~actual_values:values in
    print_endline ("[Variable.update_relative_offset]     " ^ to_string original ^ " -> " ^ to_string updated);
    match updated with
    | Mem RIC { offset = ("x", new_off); _ } -> new_off = 1 + 10
    | _ -> false

  let%test "update_relative_offset_2" =
    let original = mem 1 0 2 ("rel", 1) in
    let values = String.Map.of_alist_exn [("rel", RIC.ric (1, Int 10, Int 11, ("", 0)))] in
    let updated = update_relative_offset ~var:original ~actual_values:values in
    print_endline ("[Variable.update_relative_offset]     " ^ to_string original ^ " -> " ^ to_string updated);
    match updated with
    | Mem RIC { lower_bound = Int 0; upper_bound = Int 3; offset = ("", 11); _ } -> true
    | _ -> false
end)