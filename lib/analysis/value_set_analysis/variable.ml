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
    (* | Mem of Memory_block.t *)
    | Mem of Reduced_interval_congruence.RIC.t
  [@@deriving sexp, compare, equal]

  (** Converts a variable to a human-readable string representation. Memory blocks are printed as ranges. *)
  let to_string (var : t) : string =
    match var with 
    | Var v -> Var.to_string v
    | Mem ric -> "mem[" ^ RIC.to_string ric ^ "]"

  let mem (ric : int * ExtendedInt.t * ExtendedInt.t * (string * int)) : t =
    Mem (RIC.ric ric)

  let is_linear_memory (v : t) : bool =
    match v with
    | Var _ -> false
    | Mem _ -> true

  let get_relative_offset (v : t) : string =
    match v with
    | Mem RIC {offset = (offset, _); _} -> offset
    | _ -> ""

  let list_to_string (vars : t list) : string = String.concat ~sep:", " (List.map vars ~f:to_string)

  let is_infinite (v : t) : bool =
    match v with
    | Var _ -> false
    | Mem RIC.Top -> true
    | Mem RIC {stride = _; lower_bound = l; upper_bound = u; offset = _}
        when ExtendedInt.equal l NegInfinity || ExtendedInt.equal u Infinity -> true
    | _ -> false

  let is_finite (var : t) : bool = not (is_infinite var)

  let is_singleton (var : t) : bool =
    match var with
    | Mem RIC {lower_bound = l; upper_bound = u; _} when not (ExtendedInt.equal l u) -> false
    | _ -> true


  let rec to_singletons (var : t) : t list =
    assert (is_finite var);
    match var with
    | Var _ -> [var]
    | Mem Bottom -> []
    | Mem _ when is_singleton var -> [var]
    | Mem RIC {stride = s; lower_bound = Int l; upper_bound = u; offset = (v, o)} ->
      let new_singleton = mem (0, Int 0, Int 0, (v, s * l + o)) in
      let leftovers = mem (s, Int (l + 1), u, (v, o)) in
      new_singleton :: to_singletons leftovers
    | _ -> assert false

  let share_addresses (var1 : t) (var2 : t) : bool =
    match var1, var2 with
    | Mem ric1, Mem ric2 -> not (RIC.equal RIC.Bottom (RIC.meet ric1 ric2))
    | _ -> false

  let remove ~(these_addresses : RIC.t) ~(from : t) : t list =
    match these_addresses, from with
    | _, Var _ -> [from]
    | Top, _ -> []
    | Bottom, _ -> [from] 
    | ric1, Mem ric2 when RIC.is_subset ric2 ~of_:(RIC.meet ric1 ric2) -> []
    | ric1, Mem ric2 when not (RIC.comparable_offsets ric1 ric2) -> [from]
    | _, Mem _ when is_finite from -> 
      let singletons = to_singletons from in
      List.filter ~f:(
        fun v -> 
          match v with
          | Mem ric -> RIC.equal RIC.Bottom (RIC.meet ric these_addresses)
          | _ -> false
        ) singletons

    | _ -> failwith "not implemented yet"

  let comparable_offsets (v1 : t) (v2 : t) : bool =
    match v1, v2 with
    | Mem addr1, Mem addr2 -> RIC.comparable_offsets addr1 addr2
    | _ -> true

  (* TODO: Test this function *)
  let is_covered ~(by : t list) (v : t) : bool =
    let v_addr = match v with | Mem r -> r | _ -> assert false in
    let not_covered = List.fold ~init:[v_addr]
                                ~f:(fun acc x ->
                                  match x with
                                  | Var _ -> acc
                                  | Mem addr -> 
                                    List.concat (List.map ~f:(fun y -> RIC.remove ~this:addr ~from:y) acc))
                                by in
    let not_covered = List.filter ~f:(fun x -> not (RIC.equal RIC.Bottom x)) not_covered in
    List.is_empty not_covered
    
end
include T

module Map = struct
  include Map
  include Map.Make(T)
  let to_string (m : 'a t) (f : 'a -> string) : string =
    String.concat ~sep:", " (List.map (Map.to_alist m) ~f:(fun (k, v) -> Printf.sprintf "%s ↦ %s" (to_string k) (f v)))
end

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

  let%test "to_singletons simple" =
    let v = mem 0 0 0 ("stack", 0) in
    let result = to_singletons v in
    print_endline ("[Variable.to_singletons]     " ^ to_string v ^ " -> " ^ String.concat ~sep:", " (List.map ~f:to_string result));
    List.length result = 1

  let%test "to_singletons multiple" =
    let v = mem 1 0 4 ("stack", 0) in
    let result = to_singletons v in
    print_endline ("[Variable.to_singletons]     " ^ to_string v ^ " -> " ^ String.concat ~sep:", " (List.map ~f:to_string result));
    List.length result = 5

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

  let%test "remove no overlap" =
    let from = mem 1 0 4 ("", 0) in
    let these = RIC.ric (1, Int 5, Int 8, ("", 0)) in
    let result = remove ~these_addresses:these ~from in
    print_endline ("[Variable.remove]     these:" ^ RIC.to_string these ^ " from:" ^ to_string from ^ " -> [" ^ String.concat ~sep:", " (List.map ~f:to_string result) ^ "]");
    List.length result = 5

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
end)