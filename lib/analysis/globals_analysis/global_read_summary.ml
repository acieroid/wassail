open Core
open Helpers

(** Summary domain for the global-read analysis.

    A summary represents the set of global variables that may be read by a
    function. Summaries are used to propagate information across function calls. *)


(** Type of summaries: a set of global variables that may be read. *)
type t = Global_read_domain.t


(** Pretty-printer for summaries. *)
let to_string : t -> string = 
  function summary -> "global.set instructions potentially used: " ^ Global_read_domain.to_string summary


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