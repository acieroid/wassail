type 'a block_content =
  | Control of ('a Instr.control, 'a) Instr.labelled
  | Data of (Instr.data, 'a) Instr.labelled list
  | ControlMerge

type 'a t = {
  idx: int;
  content: 'a block_content;
  annotation_before: 'a;
  annotation_after: 'a;
}

val equal : ('a -> 'a -> bool) -> 'a t -> 'a t -> bool

val compare : ('a -> 'a -> int) -> 'a t -> 'a t -> int

val to_string : 'a t -> ('a -> string) -> string

val to_dot : 'a t -> ('a -> string) -> string

val all_instructions : 'a t -> 'a Instr.t list

val all_instruction_labels : 'a t -> Instr.Label.Set.t

val all_annots : 'a t -> 'a list

val map_annotations : 'a t -> fblock:('a t -> 'b * 'b) -> finstr:('a Instr.t -> 'b * 'b) -> 'b t

val clear_annotation : 'a t -> unit t


