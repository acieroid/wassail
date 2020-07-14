open Core_kernel
open Helpers

type state = Relational_domain.t

module type SPEC_DATA = sig
  val instr_data : (state * state) IntMap.t
end

module type SPEC = sig
  val pre : Instr.label -> state
  val post : Instr.label -> state
end

module Spec (Data : SPEC_DATA) : SPEC = struct
  let pre (label : Instr.label) : state = fst (IntMap.find_exn Data.instr_data label)
  let post (label : Instr.label) : state = snd (IntMap.find_exn Data.instr_data label)
end
