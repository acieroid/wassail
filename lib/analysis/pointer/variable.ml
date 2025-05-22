(** This module defines variables used in pointer analysis, which can be either program variables
    or memory blocks identified by base addresses and offsets.

    Memory blocks can be either at absolute addresses (e.g., linear memory in WebAssembly)
    or at relative symbolic addresses (e.g., stack or global base identifiers).
    It also includes a function to check for overlap between two memory blocks,
    and a string conversion function to render variables in human-readable form. *)

open Core

(** A variable is either:
    - [Var v]: a regular program variable
    - [Mem block]: a memory block as defined by the [Memory_block] module *)
type t = 
  | Var of Var.t
  | Mem of Memory_block.t
[@@deriving sexp, compare, equal]

(** Converts a variable to a human-readable string representation. Memory blocks are printed as ranges. *)
let to_string (var : t) : string =
  match var with 
  | Var v -> Var.to_string v
  | Mem mem_block ->
    "mem" ^ Memory_block.to_string mem_block

include Comparable.Make(struct
  type nonrec t = t
  [@@deriving compare, sexp]
end)




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