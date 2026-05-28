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

    [Top] is displayed as an unknown set of global definitions, meaning that all
    global definitions may be read. A finite state is displayed as the list of 
    [global.set] instructions it contains, including both their labels and the 
    names of the globals they define. *)
let to_string (globals : t) : string =
  match globals with
  | Top -> "[ all global definitions may be read ]"
  | NotTop globals ->
    "[" ^ GlobalInstruction.Set.to_string globals ^ "]"

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


(*
TTTTTTTTTTTTTTTTTTTTTTTEEEEEEEEEEEEEEEEEEEEEE   SSSSSSSSSSSSSSS TTTTTTTTTTTTTTTTTTTTTTT   SSSSSSSSSSSSSSS 
T:::::::::::::::::::::TE::::::::::::::::::::E SS:::::::::::::::ST:::::::::::::::::::::T SS:::::::::::::::S
T:::::::::::::::::::::TE::::::::::::::::::::ES:::::SSSSSS::::::ST:::::::::::::::::::::TS:::::SSSSSS::::::S
T:::::TT:::::::TT:::::TEE::::::EEEEEEEEE::::ES:::::S     SSSSSSST:::::TT:::::::TT:::::TS:::::S     SSSSSSS
TTTTTT  T:::::T  TTTTTT  E:::::E       EEEEEES:::::S            TTTTTT  T:::::T  TTTTTTS:::::S            
        T:::::T          E:::::E             S:::::S                    T:::::T        S:::::S            
        T:::::T          E::::::EEEEEEEEEE    S::::SSSS                 T:::::T         S::::SSSS         
        T:::::T          E:::::::::::::::E     SS::::::SSSSS            T:::::T          SS::::::SSSSS    
        T:::::T          E:::::::::::::::E       SSS::::::::SS          T:::::T            SSS::::::::SS  
        T:::::T          E::::::EEEEEEEEEE          SSSSSS::::S         T:::::T               SSSSSS::::S 
        T:::::T          E:::::E                         S:::::S        T:::::T                    S:::::S
        T:::::T          E:::::E       EEEEEE            S:::::S        T:::::T                    S:::::S
      TT:::::::TT      EE::::::EEEEEEEE:::::ESSSSSSS     S:::::S      TT:::::::TT      SSSSSSS     S:::::S
      T:::::::::T      E::::::::::::::::::::ES::::::SSSSSS:::::S      T:::::::::T      S::::::SSSSSS:::::S
      T:::::::::T      E::::::::::::::::::::ES:::::::::::::::SS       T:::::::::T      S:::::::::::::::SS 
      TTTTTTTTTTT      EEEEEEEEEEEEEEEEEEEEEE SSSSSSSSSSSSSSS         TTTTTTTTTTT       SSSSSSSSSSSSSSS   
*)


let%test_module "Global-read tests" = (module struct
  let%test "Global_read_tests" =
    print_endline "_______ __________________ _______\n        Global-read domain        \n------- ------------------ -------\n";
    true

  let%test "bottom is the empty set of global definitions" =
    print_endline "[bottom is the empty set of global definitions]";
    equal bottom (NotTop GlobalInstruction.Set.empty)

  let%test "join unions finite sets" =
    let l1 : Instr.Label.t = { section = Function 0l; id = 1 }
    and l2 : Instr.Label.t = { section = Function 0l; id = 2 }
    and l3 : Instr.Label.t = { section = Function 0l; id = 3 } in
    let g1 = ("g0", l1)
    and g2 = ("g1", l2)
    and g3 = ("g0", l3) in
    let x = NotTop (GlobalInstruction.Set.of_list [g1; g2]) in
    let y = NotTop (GlobalInstruction.Set.of_list [g2; g3]) in
    print_endline (Printf.sprintf "[Joining two finite sets of global definitions]\t %s, %s  --join-->  %s" 
                    (to_string x) (to_string y) (to_string (join x y)));
    equal
      (join x y)
      (NotTop (GlobalInstruction.Set.of_list [ g1; g2; g3 ]))

  let%test "join with top returns top" =
    let l1 : Instr.Label.t = { section = Function 0l; id = 1 } in
    let g1 = ("g0", l1) in
    let x = NotTop (GlobalInstruction.Set.singleton g1) in
    print_endline (Printf.sprintf "[join with Top]\t %s, %s  --join-->  %s" 
                    (to_string x) (to_string Top) (to_string (join x Top)));
    equal (join Top x) Top && equal (join x Top) Top


  let%test "meet intersects finite sets" =
    let l1 : Instr.Label.t = { section = Function 0l; id = 1 }
    and l2 : Instr.Label.t = { section = Function 0l; id = 2 }
    and l3 : Instr.Label.t = { section = Function 0l; id = 3 } in
    let g1 = ("g0", l1)
    and g2 = ("g1", l2)
    and g3 = ("g0", l3) in
    let x = NotTop (GlobalInstruction.Set.of_list [ g1; g2 ]) in
    let y = NotTop (GlobalInstruction.Set.of_list [ g2; g3 ]) in
    print_endline (Printf.sprintf "[meet of two finite sets of global definitions]\t %s, %s  --meet-->  %s" 
                    (to_string x) (to_string y) (to_string (meet x y)));
    equal
      (meet x y)
      (NotTop (GlobalInstruction.Set.singleton g2))


  let%test "meet with top returns the other operand" =
    let l1 : Instr.Label.t = { section = Function 0l; id = 1 } in
    let g1 = ("g0", l1) in
    let x = NotTop (GlobalInstruction.Set.singleton g1) in
    print_endline (Printf.sprintf "[meet with Top]\t %s, %s  --meet-->  %s" 
                    (to_string x) (to_string Top) (to_string (meet x Top)));
    equal (meet Top x) x && equal (meet x Top) x


  let%test "to_variable_names extracts unique variable names" =
    let l1 : Instr.Label.t = { section = Function 0l; id = 1 }
    and l2 : Instr.Label.t = { section = Function 0l; id = 2 }
    and l3 : Instr.Label.t = { section = Function 0l; id = 3 } in
    let globals =
      GlobalInstruction.Set.of_list
        [ ("g0", l1); ("g0", l2); ("g2", l3) ]
    in
    print_endline (Printf.sprintf "[to_variable_names]\t %s  --to_var_names-->  %s" 
                    (GlobalInstruction.Set.to_string globals) (to_variable_names globals |> Set.to_list |> String.concat ~sep:", "));
    Set.equal
      (to_variable_names globals)
      (String.Set.of_list [ "g0"; "g2" ])

  let%test "mem_label finds label regardless of global name" =
    let l1 : Instr.Label.t = { section = Function 0l; id = 1 } in
    let set =
      GlobalInstruction.Set.of_list [ ("g0", l1) ]
    in
    print_endline (Printf.sprintf "[mem_labels]\t Set: %s, label: %s  --mem_label-->  %s" 
                    (GlobalInstruction.Set.to_string set)
                    (Instr.Label.to_string l1)
                    (Bool.to_string (GlobalInstruction.Set.mem_label set l1)));
    GlobalInstruction.Set.mem_label set l1

  let%test "mem_label returns false for absent label" =
    let l1 : Instr.Label.t = { section = Function 0l; id = 1 }
    and l2 : Instr.Label.t = { section = Function 0l; id = 2 } in
    let set =
      GlobalInstruction.Set.of_list [ ("g0", l1) ]
    in
    print_endline (Printf.sprintf "[mem_labels]\t Set: %s, label: %s  --mem_label-->  %s" 
                    (GlobalInstruction.Set.to_string set)
                    (Instr.Label.to_string l2)
                    (Bool.to_string (GlobalInstruction.Set.mem_label set l2)));
    not (GlobalInstruction.Set.mem_label set l2)

  let%test "compare agrees with equal" =
    let l1 : Instr.Label.t = { section = Function 0l; id = 1 } in
    let x =
      NotTop (GlobalInstruction.Set.of_list [ ("g0", l1) ])
    in
    print_endline (Printf.sprintf "[compare vs equal]\t compare %s %s  <--equivalent to-->  equal %s %s" 
                    (to_string x) (to_string x) (to_string x) (to_string x));
    equal x x && Int.equal (compare x x) 0
  
end)