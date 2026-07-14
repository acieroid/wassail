open Core
open Helpers

(** Summary domain for the global-read analysis.

    A summary represents the set of [global.set] instructions whose definitions
    may be required by a function, according to the globals that the function may
    read.

    Summaries are used to propagate this information across function calls. *)


(** Type of summaries: an abstract set of [global.set] instructions that may be
    required by a function. *)
type t = Global_read_domain.t
let equal = Global_read_domain.equal



(** Pretty-printer for summaries. *)
let to_string : t -> string = 
  function summary -> "global.set instructions potentially required: " ^ Global_read_domain.to_string summary


(** Bottom summary: no global variable is read. *)
let bottom : t = Global_read_domain.bottom


(** [initial_summaries cfgs module_ typ] initializes summaries for imported
    functions. Each imported function is conservatively assumed to read all
    global variables. *)
let initial_summaries 
    (_cfgs : 'a Cfg.t Int32Map.t) 
    (module_ : Wasm_module.t)
    (_typ : [`Bottom | `Top]) 
  : t Int32Map.t =
  let used_globals = Global_read_domain.Top in
  List.fold_left module_.imported_funcs
    ~init:Int32Map.empty
    ~f:(fun summaries desc ->
        Int32Map.set summaries ~key:desc.idx ~data:used_globals)


(** Builds a summary from an abstract state. Here, the summary is identical
    to the state. *)
let make (state : t) : t = state


(** [apply ~summary ~state] applies a summary at a call site by joining it
    with the current abstract state. This propagates information about globals
    read by the callee into the caller's state. *)
let apply ~(summary : t) ~(state : Global_read_domain.t) : Global_read_domain.t =
  Global_read_domain.join summary state

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


let%test_module "Global-read tests" = (module struct
  let%test "imported functions initially get Top summaries" =
    let module_ =
        Wasm_module.of_string
        "(module
            (import \"env\" \"f\" (func))
            (import \"env\" \"g\" (func)))"
    in
    let summaries =
        initial_summaries Int32Map.empty module_ `Top
    in
    Int32Map.for_all summaries ~f:(Global_read_domain.equal Global_read_domain.Top)  
end)