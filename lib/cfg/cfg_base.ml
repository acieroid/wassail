open Core
open Helpers

module Edge = struct
  module T = struct
    (* An edge has an id and can be annotated with a boolean (to indicate a then/else) *)
    type t = int * bool option
    [@@deriving sexp, compare, equal]
  end
  include T
  let to_string (e : t) : string = string_of_int (fst e)
  module Set = struct
    include Set
    include Set.Make(T)
  end
end

module Edges = struct
  module T = struct
    type t = Edge.Set.t IntMap.t
    [@@deriving sexp, compare, equal]
  end
  include T
  let from (edges : t) (idx : int) : Edge.t list =
    match IntMap.find edges idx with
    | Some es -> Edge.Set.to_list es
    | None -> []

  let find_exn (edges : t) (src : int) (dst : int) : Edge.t =
    List.find_exn (from edges src) ~f:(fun (idx, _) -> idx = dst)

  let add (edges : t) (from : int) (edge : Edge.t) : t =
    IntMap.update edges from ~f:(function
        | None -> Edge.Set.singleton edge
        | Some es -> Edge.Set.add es edge)

  let remove (edges : t) (from : int) (to_ : int) : t =
    IntMap.update edges from ~f:(function
        | None -> Edge.Set.empty
        | Some es -> Edge.Set.filter es ~f:(fun (dst, _) -> not (dst = to_)))

  let remove_from (edges : t) (from : int) : t =
    IntMap.remove edges from

end

module type CFG_LIKE = sig
  type 'a t

  val name : 'a t -> string

  val equal : ('a -> 'a -> bool) -> 'a t -> 'a t -> bool

  val all_block_indices : 'a t -> IntSet.t

  val all_instruction_labels : 'a t -> Instr.Label.Set.t

  val find_block_exn : 'a t -> int -> 'a Basic_block.t

  val incoming_edges : 'a t -> int -> Edge.t list

  val is_loop_head : 'a t -> int -> bool

  val successors : 'a t -> int -> int list

  val entry : 'a t -> int

  val map_annotations : 'a t ->  f:('a Instr.t -> 'b * 'b) -> 'b t

  val callees : 'a t -> Int32Set.t

  val callers : 'a t Int32Map.t -> 'a t -> Int32Set.t

  val global_types : 'a t -> Type.t list

  val arg_types : 'a t -> Type.t list

  val local_types : 'a t -> Type.t list

  val return_types : 'a t -> Type.t list

  val find_enclosing_block : 'a t -> Instr.Label.t -> Instr.Label.t option

  val find_nth_parent_block : 'a t -> Instr.Label.t -> int32 -> Instr.Label.t option

  val block_arity : 'a t -> Instr.Label.t -> int * int

  val is_loop_exn : 'a t -> Instr.Label.t -> bool

  val exit_block : 'a t -> int

  val predecessors : 'a t -> int -> int list

  val state_after_block : 'a t -> int -> 'a -> 'a

  val find_enclosing_block_exn : 'a t -> Instr.Label.t -> 'a Basic_block.t
end
