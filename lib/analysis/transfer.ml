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
  (** The annotations expected by the analysis *)
  type annot_expected

  (** The state of the analysis *)
  module State : STATE

  (** The bottom state *)
  val bottom : State.t
  (** The initial state for a function. Used for the initial state of the main function in an inter analysis *)
  val init : Wasm_module.t -> Func_inst.t -> State.t

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
    -> (annot_expected Basic_block.t * State.t) list (* the predecessor flows, as pairs of block indices and state *)
    -> State.t (* the state resulting from the merge *)

  (** Transfer function for control instructions *)
  val control
    : Wasm_module.t (* the wasm module *)
    -> annot_expected Cfg.t (* the CFG of the function to analyze *)
    -> annot_expected Instr.labelled_control (* the control instruction to analyze *)
    -> State.t (* the state before this instruction *)
    (* the resulting state after applying the transfer function *)
    -> [ `Simple of State.t (* This is the state for the only successor *)
       | `Branch of State.t * State.t ] (* In case of br_if, there is one "true" and one "false" successor *)

end

module type INTRA_ONLY_TRANSFER = sig
  include TRANSFER_BASE

  val call
    : Wasm_module.t
    -> annot_expected Cfg.t
    -> annot_expected Instr.labelled_call
    -> State.t
    -> State.t
end

(** The expected interface for a transfer function of a summary-based analysis *)
module type SUMMARY_TRANSFER = sig
  (** It relies on regular (intraprocedural) CFGs *)
  include TRANSFER_BASE

  (** The type of summaries used by the analysis (can be unit) *)
  type summary

  (** Apply an imported function, given its function index, arity, and state before calling it *)
  val apply_imported : Wasm_module.t -> Int32.t -> (int * int) -> annot_expected Instr.labelled_call -> State.t -> State.t

  (** Apply a summary, given the function index, arity, state before calling it, and the summary *)
  val apply_summary : Wasm_module.t -> Int32.t -> (int * int) -> annot_expected Instr.labelled_call -> State.t -> summary -> State.t

  (** Extract a fully-analyzed CFG into a summary *)
  val extract_summary : Wasm_module.t -> annot_expected Cfg.t -> State.t Cfg.t -> summary

end

module type CLASSICAL_INTER_TRANSFER = sig
  include TRANSFER_BASE

  (** Transfer function for call instructions. Most likely a no-op in all cases.*)
  val call_inter
    : Wasm_module.t (* the wasm module *)
    -> annot_expected Cfg.t (* the CFG of the function *)
    -> annot_expected Instr.labelled_call (* the call instruction to analyze *)
    -> State.t (* the state before this instruction *)
    -> State.t (* the resulting state after applying the transfer function *)

  (** Transfer function for function entry. This is where we usually adapt the
      stack etc. The CFG given is the one of the callee *)
  val entry
    : Wasm_module.t (* the wasm module *)
    -> annot_expected Cfg.t (* the CFG of the function *)
    -> State.t (* the state after the call instruction, before going into the function *)
    -> State.t (* the resulting state after applying the transfer function *)

  (** Transfer function for function exit. This is where we restore the stack
      etc. The CFG given is the one of the caller.*)
  val return
    : Wasm_module.t (* the wasm module *)
    -> annot_expected Cfg.t (* the CFG of the function *)
    -> annot_expected Instr.labelled_call (* the corresponding call instruction that brought us in this function *)
    -> State.t (* The state before the call. Often useful to have *)
    -> State.t (* The state after the call, before the return to restore the stack *)
    -> State.t (* the resulting state after applying the transfer function *)

  (** Transfer function for imported functions *)
  val imported
    : Wasm_module.t (* the wasm module *)
    -> Wasm_module.func_desc (* the imported function *)
    -> State.t (* the state at the entry of the function *)
    -> State.t (* the state resulting from the function *)
end
