open Core_kernel
open Helpers

module Edge : sig
  type t = int * bool option
  [@@deriving sexp, compare, equal]
  val to_string : t -> string
  module Set : Set.S with type Elt.t = t
end

module Edges : sig
  type t = Edge.Set.t IntMap.t
  [@@deriving sexp, compare, equal]

  (** Find an edge between two nodes *)
  val find_exn : t -> int -> int -> Edge.t

  (** Return all edges going out of a given node *)
  val from : t -> int -> Edge.t list

  (** Add an edge from a given node *)
  val add : t -> int -> Edge.t -> t

  (** Remove an edge, irrespective of the condition *)
  val remove : t -> int -> int -> t

  (** Remove all edges from a given node *)
  val remove_from : t -> int -> t
end

type 'a t = {
  (* Is this function exported or not? *)
  exported: bool;
  (* The name of the function *)
  name: string;
  (* The index of this CFG *)
  idx: Int32.t;
  (* Types of globals (they are not specific to this CFG, but useful to have here) *)
  global_types: Type.t list;
  (* Types of arguments *)
  arg_types: Type.t list;
  (* Types of locals *)
  local_types: Type.t list;
  (* Types of return values *)
  return_types: Type.t list;
  (* All basic blocks contained in this CFG, indexed in a map by their index *)
  basic_blocks: 'a Basic_block.t IntMap.t;
  (* The edges between basic blocks (forward direction) *)
  edges: Edges.t;
  (* The edges between basic blocks (backward direction) *)
  back_edges: Edges.t;
  (* The entry block *)
  entry_block: int;
  (* The exit block *)
  exit_block: int;
  (* The loop heads *)
  loop_heads: IntSet.t;
}

(** Equality on CFGs *)
val equal : ('a -> 'a -> bool) -> 'a t -> 'a t -> bool

(** Return the call dependencies of a CFG, i.e., the indices of the functions that are directly called within this CFG.
    This ignores indirect calls *)
(* val dependencies : 'a t -> int list *)

(** Return the string representation of a CFG *)
val to_string : 'a t -> string

(** Return the DOT graph representatino of a CFG *)
val to_dot : ?annot_str:('a -> string) -> ?extra_data:string -> 'a t -> string

(** Find a basic block given its index *)
val find_block : 'a t -> int -> 'a Basic_block.t option

(** Find a basic block given its index. Throw an exception if the block is not found *)
val find_block_exn : 'a t -> int -> 'a Basic_block.t

(** Extract the successors of a block in the CFG, given its index.
    Return the successors as a list of their indices *)
val successors : 'a t -> int -> int list

(** Extract the predecessors of a block in the CFG, given its index.
    Return the predecessors as a list of their indices *)
val predecessors : 'a t -> int -> int list

(** Extract the outgoing edges of a block *)
val outgoing_edges : 'a t -> int -> Edge.t list

(** Extract the incoming edges of a block *)
val incoming_edges : 'a t -> int -> Edge.t list

(** Find the functions called in this CFG *)
val callees : 'a t -> Int32Set.t

(** Find the callers of this function *)
val callers : 'a t Int32Map.t -> 'a t -> Int32Set.t

(** Return all instructions contained in the CFG *)
val all_instructions : 'a t -> 'a Instr.t Instr.Label.Map.t

(** Return all instructions contained in the CFG as a list, in no particular order *)
val all_instructions_list : 'a t -> 'a Instr.t list

(** Find an instruction given its label *)
val find_instr_exn : 'a Instr.t Instr.Label.Map.t -> Instr.Label.t -> 'a Instr.t

(** Return all instructions contained within this CFG, as well as the block containing them *)
val find_enclosing_block_exn : 'a t -> Instr.Label.t -> 'a Basic_block.t

(** Return all basic blocks within this CFG (in no particular ordr) *)
val all_blocks : 'a t -> 'a Basic_block.t list

(** Return all merge blocks contained within this CFG (in no particular order) *)
val all_merge_blocks : 'a t -> 'a Basic_block.t list

(** Return all the indices of the blocks within this CFG *)
val all_block_indices : 'a t -> IntSet.t

(** Return the labels of all the instructions contained within this CFG *)
val all_instruction_labels : 'a t -> Instr.Label.Set.t

(** Return all annotations used in this CFG *)
val all_annots : 'a t -> 'a list

(** Map a function over all the annotations *)
val map_annotations : 'a t -> f:('a Instr.t -> 'b * 'b) -> 'b t

(** Find the state at the beginning of a block in the CFG *)
val state_before_block : 'a t -> int -> 'a

(** Find state at the end of a block in the CFG *)
val state_after_block : 'a t -> int -> 'a

(** Replace a block without changing the shape of the CFG.
    The block being replaced is the one that has the same index a the given block that replaces it. *)
val replace_block : 'a t -> 'a Basic_block.t -> 'a t

(** Remove a block from the CFG, adapting the edges.
    The block removed is specified by its index *)
val remove_block_rewrite_edges : 'a t -> int -> 'a t

(** Insert a block between two other blocks (specified by their index) in the CFG.
    Edges are rerouted to go through the new block *)
val insert_block_between : 'a t -> int -> int -> 'a Basic_block.t -> 'a t

(** Check if there is an edge between two nodes of the CFG *)
val has_edge : 'a t -> int -> int -> bool

(** Converts a CFG to a Func_inst.t *)
val to_func_inst : 'a t -> Func_inst.t
