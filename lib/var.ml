open Core_kernel

module T = struct
  type t =
    | Var of int
    | Local of int (* nth local *)
    | Global of int (* nth global *)
    | Merge of int * int
    | Return
    | Hole
  [@@deriving sexp, compare, equal]
  type with_offset = t * int
  [@@deriving sexp, compare, equal]
end
include T

let to_string (v : t) : string = match v with
  | Var n -> Printf.sprintf "i%d" n
  | Local n -> Printf.sprintf "l%d" n
  | Global n -> Printf.sprintf "g%d" n
  | Merge (idx, n) -> Printf.sprintf "m%d_%d" idx n
  | Return -> "ret"
  | Hole -> "_"

module OffsetMap = Map.Make(struct
    type t = with_offset
    [@@deriving sexp, compare, equal]
  end)
module Map = Map.Make(T)
module Set = Set.Make(T)
