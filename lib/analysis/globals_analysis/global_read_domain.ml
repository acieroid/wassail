open Core

(** Abstract domain used by the global-read analysis.

    A value of this domain represents the set of [global.set] instructions whose
    definitions may be read. Each instruction is represented as a
    {!GlobalInstruction.t}, which stores both the textual name of the global
    variable and the label of the corresponding instruction.

    The distinguished value [Top] represents an unknown set of definitions and
    is used when the analysis must conservatively assume that any global
    definition may be read. *)

(** Representation of a global definition instruction.

    A value [(name, label)] identifies the [global.set] instruction at [label]
    and records the textual name of the global variable it defines. Keeping the
    name next to the label makes it possible to recover the affected global even
    when only a set of definition instructions is available. *)
module GlobalInstruction = struct
  module T = struct
    type t = string * Instr.Label.t
    [@@deriving sexp, compare, equal]

    (** Pretty-prints a global definition instruction as [label(name)]. *)
    let to_string (s, i : t) : string =
      Instr.Label.to_string i ^ "(" ^ s ^ ")"
  end
  include T

  (* module Set = struct
    module T = struct
      include Set.Make(T)
      (** Pretty-prints a set of global definition instructions. *)
      let to_string (t : t) : string = String.concat ~sep:"," (List.map ~f:T.to_string (Set.to_list t))

      (** Returns [true] when [set] contains a global definition instruction with
          label [instr], independently of the recorded global name. *)
      let mem_label (set : t) (instr : Instr.Label.t) : bool =
        set |> Set.to_list
            |> List.fold ~init:false ~f:(fun acc (_, instr') -> acc || Instr.Label.equal instr instr')
    end
    include Set
    include T
  end *)
  module Set = struct
    include Set.Make(T)

    let to_string (t : t) : string =
      String.concat ~sep:", "
        (List.map ~f:T.to_string (Set.to_list t))

    let mem_label (set : t) (instr : Instr.Label.t) : bool =
      set
      |> Set.to_list
      |> List.exists ~f:(fun (_, instr') ->
            Instr.Label.equal instr instr')
  end
end

(** Set of global definitions read by the analyzed code, represented by the
    corresponding [global.set] instructions, or [Top] when the set is unknown. *)
type t = Top | NotTop of GlobalInstruction.Set.t

(** Pretty-printer for abstract states.

    [Top] is displayed as the set of all global definitions. A finite state is
    displayed as the list of [global.set] instructions it contains, including
    both their labels and the names of the globals they define. *)
let to_string (globals : t) : string =
  match globals with
  | Top -> "[ all global definitions may be used ]"
  | NotTop globals ->
    "[" ^ GlobalInstruction.Set.to_string globals ^ "]"
    (* "[" ^
    String.concat ~sep:", "
      (List.map (Set.to_list globals)
         ~f:GlobalInstruction.to_string)
    ^ "]" *)

(** Equality over abstract states. *)
let equal (x : t) (y : t) : bool = 
  match x, y with
  | Top, Top -> true
  | NotTop x, NotTop y -> GlobalInstruction.Set.equal x y
    (* | NotTop x, NotTop y -> GlobalInstruction.Set.compare x y = 0 *)
  | _ -> false

(** Total ordering over abstract states. *)
let compare (x : t) (y : t) : int = 
  match x, y with
  | NotTop x, NotTop y -> GlobalInstruction.Set.compare x y
  | Top, Top -> 0
  | Top, NotTop _ -> 1
  | NotTop _, Top -> -1

(** Least element of the domain: no global definition has been read. *)
let bottom : t = NotTop GlobalInstruction.Set.empty



(** Least upper bound of two abstract states. *)
let join (x : t) (y : t) : t = 
  match x, y with
  | Top, _ | _, Top -> Top
  | NotTop x, NotTop y -> NotTop (Set.union x y)

(** Greatest lower bound of two abstract states. *)
let meet (x : t) (y : t) : t = 
  match x, y with
  | Top, a | a, Top -> a
  | NotTop x, NotTop y -> NotTop (Set.inter x y)

(** Widening operator. Since the domain is finite, widening is simply [join]. *)
let widen : t -> t -> t = join


(** Extracts the names of the global variables defined by the instructions in
    [globals].

    This function discards the instruction labels and returns only the textual
    global names stored in the {!GlobalInstruction.t} values. *)
let to_variable_names 
  (globals : GlobalInstruction.Set.t) : String.Set.t =
  globals
  |> Set.to_list
  |> List.map ~f:(fun (var_name, _) -> var_name)
  |> String.Set.of_list