open Core_kernel

(** Integers *)
module I = struct
  type t = int
  [@@deriving sexp, compare, equal]
end

(** Strings *)
module S = struct
  type t = string
  [@@deriving sexp, compare, equal]
end

(** Sets of integers *)
module IntSet = Set.Make(I)

(** Maps of integers *)
module IntMap = Map.Make(I)

(** Maps of strings *)
module StringMap = Map.Make(S)

