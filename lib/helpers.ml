open Core_kernel

module I = struct
  type t = int
  [@@deriving sexp, compare]
end

module S = struct
  type t = string
  [@@deriving sexp, compare]
end

module IntSet = Set.Make(I)

module ValueListIntMap = struct
  module IntMap = Map.Make(I)

  type t = Value.t list IntMap.t
end

module IntMap = Map.Make(I)

module StringMap = Map.Make(S)

module IPair = struct
  type t = (int * int)
  [@@deriving sexp, compare]
end

module IntPairSet = Set.Make(IPair)
