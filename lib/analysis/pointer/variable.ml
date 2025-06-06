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
    | Mem ric ->
      "mem[" ^ RIC.to_string ric ^ "]"

  let list_to_string (vars : t list) : string = String.concat ~sep:", " (List.map vars ~f:to_string)

  let is_infinite (v : t) : bool =
    match v with
    | Var _ -> false
    | Mem RIC.Top -> true
    | Mem RIC {stride = _; lower_bound = l; upper_bound = u; offset = _}
        when ExtendedInt.equal l NegInfinity || ExtendedInt.equal u Infinity -> true
    | _ -> false

  let is_finite (var : t) : bool = not (is_infinite var)

  let rec to_singletons (var : t) : t list =
    assert (is_finite var);
    match var with
    | Var _ -> [var]
    | Mem Bottom -> []
    | Mem RIC {stride = s; lower_bound = Int l; upper_bound = u; offset = (v, o)} ->
      let new_singleton = Mem (RIC.ric (0, Int 0, Int 0, (v, s * l + o))) in
      let leftovers = Mem (RIC.ric (s, Int (l + s), u, (v, o))) in
      new_singleton :: to_singletons leftovers
    | _ -> assert false

  let share_addresses (var1 : t) (var2 : t) : bool =
    match var1, var2 with
    | Mem ric1, Mem ric2 -> not (RIC.equal RIC.Bottom  (RIC.meet ric1 ric2))
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




  (* include Comparable.Make(struct
    type nonrec t = t
    [@@deriving compare, sexp, equal]
  end) *)
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