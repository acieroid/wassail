open Helpers

type 'a block_content =
  | Control of ('a Instr.control, 'a) Instr.labelled
  | Data of (Instr.data, 'a) Instr.labelled list
  | ControlMerge

type 'a t = {
  idx: int;
  content: 'a block_content;
}

val equal : ('a -> 'a -> bool) -> 'a t -> 'a t -> bool

val compare : ('a -> 'a -> int) -> 'a t -> 'a t -> int

val to_string : 'a t -> ('a -> string) -> string

val to_dot : 'a t -> ('a -> string) -> string

val annotate : 'a t -> ('b * 'b) IntMap.t -> 'b t
