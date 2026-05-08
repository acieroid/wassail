open Core

(** Abstract domain used by the global-read analysis.

    A value of this domain represents the set of global variables that may have
    been read. Each global is represented as a {!Var.Global} value inside a
    {!Var.Set.t}. *)

(** Set of global variables read by the analyzed code. *)
type t = Var.Set.t

(** Equality over abstract states. *)
let equal : t -> t -> bool = Var.Set.equal

(** Total ordering over abstract states. *)
let compare : t -> t -> int = Var.Set.compare

(** [add ~globals ~used_global] returns [globals] extended with the global
    variable identified by [used_global]. *)
let add ~(globals : t) ~(used_global : int) : t =
  Var.Set.add globals (Var.Global used_global)

(** Least element of the domain: no global variable has been read. *)
let bottom : t = Var.Set.empty

(** Pretty-printer for abstract states. *)
let to_string (globals : t) : string =
  "[" ^
  String.concat ~sep:", " (List.map (Var.Set.to_list globals) ~f:Var.to_string)
  ^ "]"

(** Least upper bound of two abstract states. *)
let join : t -> t -> t = Var.Set.union

(** Greatest lower bound of two abstract states. *)
let meet : t -> t -> t = Var.Set.inter

(** Widening operator. Since the domain is finite, widening is simply [join]. *)
let widen : t -> t -> t = join