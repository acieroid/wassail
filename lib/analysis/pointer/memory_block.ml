(** This module defines a representation of memory blocks and operations over them
    for static analysis of WebAssembly (or similar) programs.
*)

open Core 

(** A memory block *)
type t = {
  address : string * int;
  size : int
}
[@@deriving sexp, compare, equal]

let make (addr : string * int) (size : int) : t =
  {address = addr; size = size}

(** Returns a string representation of a memory block [t] in readable format. *)
let to_string (block : t) : string =
  let address, upper_bound =
    match block.address with
    | ("", a) -> string_of_int a, string_of_int (a + block.size - 1)
    | (var, a) -> var ^ "+" ^ string_of_int a, var ^ "+" ^ string_of_int (a + block.size - 1)
  in
  "[" ^ address ^ "," ^ upper_bound ^ "]"

let print_address (addr : string * int) : string =
  match addr with 
  | ("", i) -> string_of_int i 
  | (v, 0) -> v
  | (v, i) -> v ^ (if i < 0 then "-" else "+") ^ string_of_int (abs i)

let rec extract_addresses (memory_block : t) : (string * int) list =
  match memory_block with 
  | {address = (_, _); size = 0} -> []
  | {address = (v, i); size = s} -> (v, (i + s -1)) :: extract_addresses {address = (v, i); size = s - 1}

  














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



let%test_module "Memory_block tests" = (module struct
  let%test "extract_addresses with size 4 (Relative address)" =
  let block = { address = ("x", 0); size = 4 } in
  let addresses = extract_addresses block in
  let printed = List.map addresses ~f:print_address in
  let expected = ["x+3"; "x+2"; "x+1"; "x"] in
  List.equal String.equal printed expected

  let%test "extract_addresses with size 4 (absolute address)" =
  let block = { address = ("", 4); size = 4 } in
  let addresses = extract_addresses block in
  let printed = List.map addresses ~f:print_address in
  let expected = ["7"; "6"; "5"; "4"] in
  List.equal String.equal printed expected
end)