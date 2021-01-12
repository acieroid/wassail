(* A function summary *)
open Core_kernel
open Helpers


module type SUMMARY_T = sig
  type t
  type state
  val to_string : t -> string
  val bottom : 'a Cfg.t -> Var.Set.t -> t
  val top : 'a Cfg.t -> Var.Set.t -> t
  val initial_summaries : 'a Cfg.t Int32Map.t -> Wasm_module.t -> [`Bottom | `Top ] -> t Int32Map.t
end

module MakeManager = functor (Summary : SUMMARY_T) -> struct
  let summaries : Summary.t Int32Map.t ref = ref Int32Map.empty

  let init (sums : Summary.t Int32Map.t) : unit = summaries := sums

  let get (f : Int32.t) : Summary.t = match Int32Map.find !summaries f with
    | Some s -> s
    | None -> failwith (Printf.sprintf "Summary: can't find the summary of function %s" (Int32.to_string f))
end
