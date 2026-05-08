open Core

(** Abstract domain used by the global-read analysis.

    A value of this domain represents the set of global variables that may have
    been read. Each global is represented as a {!Var.Global} value inside a
    {!Var.Set.t}. *)

(** Set of global variables read by the analyzed code. *)
type t = Top | NotTop of Instr.Label.Set.t

(** Equality over abstract states. *)
let equal (x : t) (y : t) : bool = 
  match x, y with
  | Top, Top -> true
  | NotTop x, NotTop y -> Instr.Label.Set.equal x y
  | _ -> false

(** Total ordering over abstract states. *)
let compare (x : t) (y : t) : int = 
  match x, y with
  | NotTop x, NotTop y -> Instr.Label.Set.compare x y
  | Top, Top -> 0
  | Top, NotTop _ -> 1
  | NotTop _, Top -> -1

(** [add ~globals ~used_global] returns [globals] extended with the global
    variable identified by [used_global]. *)
(* let add ~(globals : t) ~(used_global : int) : t =
  Var.Set.add globals (Var.Global used_global) *)

(** Least element of the domain: no global variable has been read. *)
let bottom : t = NotTop Instr.Label.Set.empty

(** Pretty-printer for abstract states. *)
let to_string (globals : t) : string =
  match globals with
  | Top -> "[ all global definitions ]"
  | NotTop globals ->
    "[" ^
    String.concat ~sep:", " (List.map (Var.Set.to_list globals) ~f:Instr.Label.to_string)
    ^ "]"

(** Least upper bound of two abstract states. *)
let join (x : t) (y : t) : t = 
  match x, y with
  | Top, _ | _, Top -> Top
  | NotTop x, NotTop y -> NotTop (Var.Set.union x y)

(** Greatest lower bound of two abstract states. *)
let meet (x : t) (y : t) : t = 
  match x, y with
  | Top, a | a, Top -> a
  | NotTop x, NotTop y -> NotTop (Var.Set.inter x y)

(** Widening operator. Since the domain is finite, widening is simply [join]. *)
let widen : t -> t -> t = join