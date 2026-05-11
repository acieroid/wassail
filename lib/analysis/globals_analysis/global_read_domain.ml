open Core

(** Abstract domain used by the global-read analysis.

    A value of this domain represents the set of labels of [global.set]
    instructions whose definitions may be read. The distinguished value [Top]
    represents an unknown set of definitions and is used when the analysis must
    conservatively assume that any global definition may be read. *)



module GlobalInstruction = struct
  module T = struct
    type t = string * Instr.Label.t
    [@@deriving sexp, compare, equal]

    let to_string (s, i : t) : string =
      Instr.Label.to_string i ^ "(" ^ s ^ ")"
  end
  include T

  module Set = struct
    module T = struct
      include Set.Make(T)
      let to_string (t : t) : string = String.concat ~sep:"," (List.map ~f:T.to_string (Set.to_list t))

      let mem (set : t) (instr : Instr.Label.t) : bool =
        set |> Set.to_list
            |> List.fold ~init:false ~f:(fun acc (_, instr') -> acc || Instr.Label.equal instr instr')
    end
    include Set
    include T
  end
end

(** Set of global definitions read by the analyzed code, represented by the
    labels of the corresponding [global.set] instructions, or [Top] when the
    set is unknown. *)
type t = Top | NotTop of GlobalInstruction.Set.t

(** Equality over abstract states. *)
let equal (x : t) (y : t) : bool = 
  match x, y with
  | Top, Top -> true
  | NotTop x, NotTop y -> GlobalInstruction.Set.equal x y
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

(** Pretty-printer for abstract states.

    [Top] is displayed as the set of all global definitions. A finite state is
    displayed as the list of labels of the [global.set] instructions it
    contains. *)
let to_string (globals : t) : string =
  match globals with
  | Top -> "[ all global definitions may be used ]"
  | NotTop globals ->
    "[" ^ String.concat ~sep:", " (List.map (Var.Set.to_list globals) ~f:GlobalInstruction.to_string) ^ "]"

(** Least upper bound of two abstract states. *)
let join (x : t) (y : t) : t = 
  match x, y with
  | Top, _ | _, Top -> Top
  | NotTop x, NotTop y -> NotTop (GlobalInstruction.Set.union x y)

(** Greatest lower bound of two abstract states. *)
let meet (x : t) (y : t) : t = 
  match x, y with
  | Top, a | a, Top -> a
  | NotTop x, NotTop y -> NotTop (GlobalInstruction.Set.inter x y)

(** Widening operator. Since the domain is finite, widening is simply [join]. *)
let widen : t -> t -> t = join


let to_variable_names 
  (globals : GlobalInstruction.Set.t) : String.Set.t =
  globals
    |> Instr.Label.Set.to_list
    |> List.map ~f:(fun (var_name, _) -> var_name)
    |> String.Set.of_list