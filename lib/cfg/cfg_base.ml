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

  module BlockIdx : sig
    type t
    val to_string : t -> string
    module Set : Set.S with type Elt.t = t
    module Map : Map.S with type Key.t = t
  end

  val find_block_exn : 'a t -> BlockIdx.t -> 'a Basic_block.t

  val is_loop_head : 'a t -> BlockIdx.t -> bool

  val predecessors : 'a t -> BlockIdx.t -> (BlockIdx.t * bool option) list

  val successors : 'a t -> BlockIdx.t -> BlockIdx.t list

  val entry_block : 'a t -> BlockIdx.t

  val map_annotations : 'a t ->  f:('a Instr.t -> 'b * 'b) -> 'b t

end
