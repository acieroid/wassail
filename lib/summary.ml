(* A function summary *)
open Core_kernel
open Helpers


module type SUMMARY_T = sig
  type t
  type state
  val to_string : t -> string
  val bottom : Cfg.t -> Spec_inference.var list -> t
  val top : Cfg.t -> Spec_inference.var list -> t
  val of_import : int -> string -> Type.t list -> Type.t list -> t
  val initial_summaries : Cfg.t IntMap.t -> Wasm_module.t -> [`Bottom | `Top ] -> t IntMap.t
  val apply : t -> state -> string list -> string option -> state
end

module MakeManager = functor (Summary : SUMMARY_T) -> struct
  let summaries : Summary.t IntMap.t ref = ref IntMap.empty

  let init (sums : Summary.t IntMap.t) : unit = summaries := sums

  let get (f : int) : Summary.t = IntMap.find_exn !summaries f
end
