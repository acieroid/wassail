type 'a block_content =
  | Control of ('a Instr.control, 'a) Instr.labelled
  | Data of (Instr.data, 'a) Instr.labelled list

type 'a t = {
  idx: int;
  content: 'a block_content;
}

val equal : ('a -> 'a -> bool) -> 'a t -> 'a t -> bool

val compare : ('a -> 'a -> int) -> 'a t -> 'a t -> int

val to_string : ?annot_str:('a -> string) -> 'a t -> string

val to_dot : ?annot_str:('a -> string) -> 'a t -> string

val all_direct_instruction_labels : 'a t -> Instr.Label.Set.t

val all_instruction_labels : 'a t -> Instr.Label.Set.t

val all_annots : 'a t -> 'a list

val map_annotations : 'a t -> f:('a Instr.t -> 'b * 'b) -> 'b t

val clear_annotation : 'a t -> unit t

val is_merge : 'a t -> bool
