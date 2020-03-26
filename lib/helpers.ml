open Core_kernel

module I = struct
  type t = int
  [@@deriving sexp, compare]
end

module IntMap = Map.Make(I)
module IntSet = Set.Make(I)

module IPair = struct
  type t = (int * int)
  [@@deriving sexp, compare]
end

module IntPairSet = Set.Make(IPair)
