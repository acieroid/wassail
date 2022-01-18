open Helpers

(** The expected interface for a transfer function *)
module type TRANSFER = sig
  (** The annotations expected by the analysis *)
  type annot_expected

  (** The state of the analysis *)
  type state

  (** The type of summaries used by the analysis (can be unit) *)
  type summary

  (** Checks equality between two states *)
  val equal_state : state -> state -> bool

  (** States should be comparable (i.e., ordered) *)
  val compare_state : state -> state -> int

  (** The initial state *)
  val init_state : annot_expected Cfg.t -> state

  (** The bottom state *)
  val bottom_state : annot_expected Cfg.t -> state

  (** Convert a state to its textual representation *)
  val state_to_string : state -> string

  (** Joins two states of the analysis *)
  val join_state : state -> state -> state

  (** Widening operator *)
  val widen_state : state -> state -> state

  (** Transfer function for control instructions *)
  val control_instr_transfer
    : Wasm_module.t (* the wasm module *)
    -> summary Int32Map.t (* the summaries of other functions *)
    -> annot_expected Cfg.t (* the CFG of the function to analyze *)
    -> annot_expected Instr.labelled_control (* the control instruction to analyze *)
    -> state (* the state before this instruction *)
    -> [`Simple of state | `Branch of state * state ] (* the resulting state after applying the transfer function *)

  (** Transfer function for data instructions *)
  val data_instr_transfer
    : Wasm_module.t (* the wasm module *)
    -> annot_expected Cfg.t (* the CFG of the function to analyze *)
    -> annot_expected Instr.labelled_data (* the data instruction to analyze *)
    -> state (* the state before this instruction *)
    -> state (* the resulting state after applying the transfer function *)

  (** Merges flows for blocks that have multiple predecessors *)
  val merge_flows
    : Wasm_module.t (* the wasm module *)
    -> annot_expected Cfg.t (* the CFG *)
    -> annot_expected Basic_block.t (* the basic block for which flows should be merged *)
    -> (int * state) list (* the predecessor flows, as pairs of block indices and state *)
    -> state (* the state resulting from the merge *)

  (** Extract a fully-analyzed CFG into a summary *)
  val extract_summary : annot_expected Cfg.t -> state Cfg.t -> summary
end

