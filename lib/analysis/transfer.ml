(* TODO: there would be 3 cases of transfer functions:
   1. fully intra-procedural: no summary, no `Multiple
   2. summary-based interprocedural: summary type and "apply_summary" function for call instruction
   3. classic interprocedural: call and return functions *)
module type STATE = sig
  type t
  (** Checks equality between two states *)
  val equal : t -> t -> bool
  (** States should be comparable (i.e., ordered) *)
  val compare : t -> t -> int
  (** Converts a state to its textual representation *)
  val to_string : t -> string
  (** Joins two states *)
  val join : t -> t -> t
  (** Performs widening, relying on the old state and the new state *)
  val widen : t -> t -> t
end

module type TRANSFER_BASE = sig
  module Cfg : Cfg_base.CFG_LIKE

  (** The annotations expected by the analysis *)
  type annot_expected

  (** The state of the analysis *)
  module State : STATE

  (** The bottom state *)
  val bottom : annot_expected Cfg.t -> State.t
  (** The initial state *)
  val init : annot_expected Cfg.t -> State.t

  (** Transfer function for data instructions *)
  val data
    : Wasm_module.t (* the wasm module *)
    -> annot_expected Cfg.t (* the CFG of the function to analyze *)
    -> annot_expected Instr.labelled_data (* the data instruction to analyze *)
    -> State.t (* the state before this instruction *)
    -> State.t (* the resulting state after applying the transfer function *)

  (** Merges flows for blocks that have multiple predecessors *)
  val merge_flows
    : Wasm_module.t (* the wasm module *)
    -> annot_expected Cfg.t (* the CFG *)
    -> annot_expected Basic_block.t (* the basic block for which flows should be merged *)
    -> (Cfg.BlockIdx.t * State.t) list (* the predecessor flows, as pairs of block indices and state *)
    -> State.t (* the state resulting from the merge *)

  (** Transfer function for control instructions *)
  val control
    : Wasm_module.t (* the wasm module *)
    -> annot_expected Cfg.t (* the CFG of the function to analyze *)
    -> annot_expected Instr.labelled_control (* the control instruction to analyze *)
    -> State.t (* the state before this instruction *)
    (* the resulting state after applying the transfer function *)
    -> [ `Simple of State.t (* This is the state for the only successor *)
       | `Branch of State.t * State.t (* In case of br_if, there is one "true" and one "false" successor *)
       | `Multiple of State.t list ] (* In other branching cases (br_table), there can be an unbounded number of successors *)
end

module type INTRA_ONLY_TRANSFER = sig
  include TRANSFER_BASE with module Cfg = Cfg.Cfg

  val call
    : Wasm_module.t
    -> annot_expected Cfg.t
    -> annot_expected Instr.labelled_call
    -> State.t
    -> [ `Simple of State.t | `Multiple of State.t list ]
end

(** The expected interface for a transfer function of a summary-based analysis *)
module type SUMMARY_TRANSFER = sig
  (** It relies on regular (intraprocedural) CFGs *)
  include TRANSFER_BASE with module Cfg = Cfg.Cfg

  (** The type of summaries used by the analysis (can be unit) *)
  type summary

  (** Apply an imported function, given its function index, arity, and state before calling it *)
  val apply_imported : Wasm_module.t -> Int32.t -> (int * int) -> annot_expected Instr.labelled_call -> State.t -> State.t

  (** Apply a summary, given the function index, arity, state before calling it, and the summary *)
  val apply_summary : Wasm_module.t -> Int32.t -> (int * int) -> annot_expected Instr.labelled_call -> State.t -> summary -> State.t

  (** Extract a fully-analyzed CFG into a summary *)
  val extract_summary : annot_expected Cfg.t -> State.t Cfg.t -> summary

end

module type CLASSICAL_INTER_TRANSFER = sig
  include TRANSFER_BASE with module Cfg = Icfg

  (** Transfer function for call instructions *)
  val call
    : Wasm_module.t (* the wasm module *)
    -> annot_expected Cfg.t (* the CFG of the function to analyze *)
    -> annot_expected Instr.labelled_call (* the call instruction to analyze *)
    -> State.t (* the state before this instruction *)
    (* the resulting state after applying the transfer function *)
    -> [ `Simple of State.t (* This is the state for the only successor *)
       | `Multiple of State.t list ] (* In other branching cases (br_table, call_indirect), there can be an unbounded number of successors *)
end
