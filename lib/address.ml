open Core_kernel

(** These are the addresses. This needs to be further improved *)
module T = struct
  type t = int (* XXX: abstract it, but how? Also, depend on which address (function address are fine with int) *)
  [@@deriving sexp, compare, yojson]
end
include T
