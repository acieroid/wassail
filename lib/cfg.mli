open Helpers

type edge = (int * bool option) list
type 'a t = {
  (* Is this function exported or not? *)
  exported: bool;
  (* The name of the function *)
  name: string;
  (* The index of this CFG *)
  idx: int;
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
  edges: edge IntMap.t;
  (* The edges between basic blocks (backward direction) *)
  back_edges: edge IntMap.t;
  (* The edges data *)
  (*  edges_data: EdgeDataIntMap.t; *)
  (* The entry block *)
  entry_block: int;
  (* The exit block *)
  exit_block: int;
  (* The loop heads *)
  loop_heads: IntSet.t;
}

val equal : ('a -> 'a -> bool) -> 'a t -> 'a t -> bool

val dependencies : 'a t -> int list

val to_string : 'a t -> string

val to_dot : 'a t -> ('a -> string) -> string

val find_block_exn : 'a t -> int -> 'a Basic_block.t

val successors : 'a t -> int -> int list

val predecessors : 'a t -> int -> (int * bool option) list

val callees : 'a t -> IntSet.t

val callers : 'a t IntMap.t -> 'a t -> IntSet.t

val all_block_indices : 'a t -> IntSet.t

val all_instructions : 'a t -> 'a Instr.t list

val all_merge_blocks : 'a t -> 'a Basic_block.t list

val all_instruction_labels : 'a t -> IntSet.t

val all_annots : 'a t -> 'a list

val annotate : 'a t -> ('b * 'b) IntMap.t -> ('b * 'b) IntMap.t -> 'b t

val add_annotation : 'a t -> ('b * 'b) IntMap.t -> ('b * 'b) IntMap.t -> ('a * 'b) t
