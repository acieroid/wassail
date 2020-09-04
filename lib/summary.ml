(* A function summary *)
open Core_kernel
open Helpers


module type SUMMARY_T = sig
  type t
  type state
  val to_string : t -> string
  val bottom : 'a Cfg.t -> Var.t list -> t
  val top : 'a Cfg.t -> Var.t list -> t
  val initial_summaries : 'a Cfg.t IntMap.t -> Wasm_module.t -> [`Bottom | `Top ] -> t IntMap.t
end

module MakeManager = functor (Summary : SUMMARY_T) -> struct
  let summaries : Summary.t IntMap.t ref = ref IntMap.empty

  let init (sums : Summary.t IntMap.t) : unit = summaries := sums

  let get (f : int) : Summary.t = IntMap.find_exn !summaries f
end
