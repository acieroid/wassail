open Core_kernel

module Use = struct
  module T = struct
    (** Uses of a variable occur from instructions *)
    type t = {
      label : Instr.Label.t; (** The label of the instruction using a variable*)
      var: Var.t; (** The variable used *)
    }
    [@@deriving sexp, equal, compare]
    let to_string (use : t) : string =
      Printf.sprintf "use(%s, %s)" (Instr.Label.to_string use.label) (Var.to_string use.var)
  end
  include T

  module Set = Set.Make(T)
  module Map = Map.Make(T)
  let make (label : Instr.Label.t) (var : Var.t) = { label; var; }
  let to_dot_label (cfg : 'a Cfg.t) (use : t) = Printf.sprintf "block%d:instr%s" (Cfg.find_enclosing_block_exn cfg use.label).idx (Instr.Label.to_string use.label)
end

module Def = struct
  module T = struct
    (** Definitions can occur in multiple places
        - As a result from an instruction (e.g., i32.add defines a new variable)
        - In merge nodes
        - At the entry of a function (e.g., for local, global, and memory variables) *)
    type t =
      | Instruction of Instr.Label.t * Var.t (* Label of the instruction and variable defined (there can be more than one definition per instruction) *)
      | Entry of Var.t (* Only the variable defined (because use-def is intraprocedural, we know the function) *)
      | Constant of Prim_value.t (* A constant does not really have a definition *)
    [@@deriving equal, compare]

    let to_string (def : t) : string = match def with
      | Instruction (n, v) -> Printf.sprintf "idef(%s, %s)" (Instr.Label.to_string n) (Var.to_string v)
      | Entry v -> Printf.sprintf "edef(%s)" (Var.to_string v)
      | Constant v -> Printf.sprintf "const(%s)" (Prim_value.to_string v)
    let to_dot_label (cfg : 'a Cfg.t) (def : t) : string = match def with
      | Instruction (n, _) -> Printf.sprintf "block%d:instr%s" (Cfg.find_enclosing_block_exn cfg n).idx (Instr.Label.to_string n)
      | Entry v -> Printf.sprintf "entry_var:%s" (Var.to_string v)
      | Constant v -> Printf.sprintf "const:%s" (Prim_value.to_string v)
  end
  include T
end

module UseDefChains = struct
  (** Use-definition chains, as a mapping from uses (as the index of the instruction that uses the variable, and the variable name) to their definition (as the index of the instruction that defines the variable)*)
  module T = struct
    type t = Def.t Use.Map.t
    [@@deriving compare, equal]

    (** Convert a use-def map to its string representation *)
    let to_string (m : t) : string =
      String.concat ~sep:", " (List.map (Use.Map.to_alist m) ~f:(fun (k, v) -> Printf.sprintf "%s -> %s" (Use.to_string k) (Def.to_string v)))
  end
  include T
  include Test.Helpers(T)

  (** The empty use-def map *)
  let empty : t = Use.Map.empty

  (** Add an element to the use-def map *)
  let add (m : t) (use : Use.t) (def : Def.t) : t =
    match Use.Map.add m ~key:use ~data:def with
    | `Duplicate -> failwith (Printf.sprintf "Cannot have more than one definition for a use in use-def chains, when adding %s ->%s to %s" (Use.to_string use) (Def.to_string def) (to_string m))
    | `Ok r -> r

  (** Gets an element from the use-def map *)
  let get (m : t) (use : Use.t) : Def.t =
    match Use.Map.find m use with
    | Some def -> def
    | None -> failwith "use-def lookup did not find a definition for a use"
end

(** Compute data dependence from the CFG of a function, and return the following elements:
    1. A map from variables to their definitions
    2. A map from variables to their uses
    3. A map of use-def chains *)
let make (cfg : Spec.t Cfg.t) : (Def.t Var.Map.t * Use.Set.t Var.Map.t * UseDefChains.t) =
  (* To construct the use-def map, we walk over each instruction, and collect uses and defines.
     There is exactly one define per variable.
     e.g., [] i32.const 0 [x] defines x
           [x, y] i32.add [z] defines z, uses x, y
     On top of being defined by instructions, variable may be defined in merge nodes, and may be
     defined at the entry node of a function.

     However, there can be more than one use for a variable!

     We compute the following maps while walking over the instructions:
       uses: (int list) Var.Map.t (* The list of instructions that use a given variable *)
       def: int Var.Map.t (* The instruction that defines a given variable *)

     From this, it is a matter of folding over use to construct the DefUseMap:
       Given a use of v at instruction lab, add key:(lab, v) data:(lookup defs v) *)
  (* The defs map will map variables to their definition.
     Because we are in SSA, there can only be one instruction that define a variable *)
  let defs: Def.t Var.Map.t = Var.Map.empty in
  (* The uses map will map variables to all of its uses *)
  let uses: Use.Set.t Var.Map.t = Var.Map.empty in
  (* Add definitions for all locals, globals, and memory variables *)
  let defs =
    let entry_spec = Cfg.state_before_block cfg cfg.entry_block (Spec_inference.init_state cfg) in
    let vars = Spec_inference.vars_of entry_spec in
    Var.Set.fold vars ~init:defs ~f:(fun defs var ->
        match Var.Map.add defs ~key:var ~data:(Def.Entry var) with
        | `Duplicate -> failwith "use_def: more than one entry definition for a variable"
        | `Ok r -> r) in
  let all_instrs = Cfg.all_instructions_list cfg in
  (* Add definitions for all constants *)
  let defs =
    let all_vars : Var.Set.t = List.fold_left all_instrs
        ~init:Var.Set.empty
        ~f:(fun acc instr -> Var.Set.union acc
               (Var.Set.union
                  (Spec.vars_of (Instr.annotation_before instr))
                  (Spec.vars_of (Instr.annotation_after instr)))) in
    Var.Set.fold all_vars ~init:defs ~f:(fun defs var ->
        match var with
        | Var.Const n -> begin match Var.Map.add defs ~key:var ~data:(Def.Constant n)  with
            | `Duplicate -> defs (* already in the set *)
            | `Ok r -> r
          end
        | _ -> defs) in
  (* For each instruction, update the defs and uses map *)
  let (defs, uses) = List.fold_left all_instrs
      ~init:(defs, uses)
      ~f:(fun (defs, uses) instr ->
          (* Add definitions introduced by this instruction *)
          let defs = List.fold_left (Spec_inference.instr_def cfg instr) ~init:defs ~f:(fun defs var ->
              match Var.Map.add defs ~key:var ~data:(Def.Instruction (Instr.label instr, var)) with
              | `Duplicate -> failwith (Printf.sprintf "use_def: duplicate define of %s in instruction %s, was already defined at %s"
                                          (Var.to_string var) (Instr.to_string instr ~annot_str:Spec.to_string)
                                          (Def.to_string (Var.Map.find_exn defs var)))
              | `Ok r -> r) in
          (* Add uses introduced by this instruction *)
          let uses = List.fold_left (Spec_inference.instr_use cfg instr) ~init:uses ~f:(fun uses var ->
              Log.debug (Printf.sprintf "instruction %s uses %s\n" (Instr.to_string instr ~annot_str:Spec.to_string) (Var.to_string var));
              Var.Map.update uses var ~f:(function
                  | Some v -> Use.Set.add v { label = Instr.label instr; var }
                  | None -> Use.Set.singleton { label = Instr.label instr; var })) in
          (defs, uses)) in
  (* From the defs and uses map, it is a matter of folding over use to construct the DefUseMap:
     Given a use of v at instruction lab, add key:(lab, v) data:(lookup defs v) *)
  let udchains = Var.Map.fold uses ~init:UseDefChains.empty ~f:(fun ~key:var ~data:uses map ->
      Use.Set.fold uses ~init:map ~f:(fun map use ->
          UseDefChains.add map use (match Var.Map.find defs var with
              | Some v -> v
              | None -> failwith (Printf.sprintf "Use-def chain incorrect: could not find def of variable %s" (Var.to_string var))))) in
  (defs, uses, udchains)

(** Return the edges that can be used to annotate a CFG with data dependencies *)
let annotate (cfg : Spec.t Cfg.t) : string =
  let (_, _, chains) = make cfg in
  String.concat ~sep:"\n"
    (List.map (Use.Map.keys chains)
       ~f:(fun use ->
          Printf.sprintf "%s -> %s [color=blue]" (Use.to_dot_label cfg use) (Def.to_dot_label cfg (UseDefChains.get chains use))))

module Test = struct
  let%test "simplest ud chain" =
    let open Instr.Label.Test in
    let module_ = Wasm_module.of_string "(module
  (type (;0;) (func (param i32) (result i32)))
  (func (;test;) (type 0) (param i32) (result i32)
    memory.size ;; Instr 0 [i0] defines i0, uses nothing
    memory.size ;; Instr 1 [i1] defines i1, uses nothing
    i32.add     ;; Instr 2 [i2] defines i2, uses i0 and i1
                ;; return block: defines ret, uses i2
    )
  )" in
    let cfg = Spec_analysis.analyze_intra1 module_ 0l in
    let _, _, actual = make cfg in
    let expected = Use.Map.of_alist_exn [(Use.make (lab 2) (Var.Var (lab 0)), Def.Instruction (lab 0, (Var.Var (lab 0))));
                                         (Use.make (lab 2) (Var.Var (lab 1)), Def.Instruction (lab 1, Var.Var (lab 1)));
                                         (Use.make (merge 1) (Var.Var (lab 2)), Def.Instruction (lab 2, Var.Var (lab 2)))] in
    UseDefChains.check_equality ~actual:actual ~expected:expected

  let%test "ud-chain with locals" =
    let open Instr.Label.Test in
    let module_ = Wasm_module.of_string "(module
  (type (;0;) (func (param i32 i32) (result i32)))
  (func (;test;) (type 0) (param i32 i32) (result i32)
    local.get 0 ;; Instr 0
    local.get 1 ;; Instr 1
    i32.add)    ;; Instr 2
  )" in
    let cfg = Spec_analysis.analyze_intra1 module_ 0l in
    let _, _, actual = make cfg in
    let expected = Use.Map.of_alist_exn [(Use.make (lab 0) (Var.Local 0), Def.Entry (Var.Local 0));
                                         (Use.make (lab 1) (Var.Local 1), Def.Entry (Var.Local 1));
                                         (Use.make (lab 2) (Var.Local 0), Def.Entry (Var.Local 0));
                                         (Use.make (lab 2) (Var.Local 1), Def.Entry (Var.Local 1));
                                         (Use.make (merge 1) (Var.Var (lab 2)), Def.Instruction (lab 2, Var.Var (lab 2)))] in
    UseDefChains.check_equality ~actual:actual ~expected:expected

  let%test "use-def with merge blocks" =
    let open Instr.Label.Test in
    let module_ = Wasm_module.of_string "(module
  (type (;0;) (func (param i32) (result i32)))
  (func (;test;) (type 0) (param i32) (result i32)
    memory.size     ;; Instr 0 [i0]
    if (result i32) ;; Instr 1 [] uses i0
      memory.size   ;; Instr 2 [i2]
    else
      memory.size   ;; Instr 3 [i3]
    end
    ;; At this point we have a merge block, merging i2 and i3 into m4_1
    memory.size     ;; Instr 4
    i32.add)        ;; Instr 5
    ;; Final merge block: i5 -> ret
  )" in
    let cfg = Spec_analysis.analyze_intra1 module_ 0l in
    let _, _, actual = make cfg in
    let expected = Use.Map.of_alist_exn [(Use.make (lab 1) (Var.Var (lab 0)), Def.Instruction (lab 0, Var.Var (lab 0)));
                                         (Use.make (lab 5) (Var.Var (lab 4)), Def.Instruction (lab 4, Var.Var (lab 4)));
                                         (Use.make (lab 5) (Var.Merge (4, 1)), Def.Instruction (merge 4, Var.Merge (4, 1)));
                                         (Use.make (merge 4) (Var.Var (lab 2)), Def.Instruction (lab 2, Var.Var (lab 2)));
                                         (Use.make (merge 4) (Var.Var (lab 3)), Def.Instruction (lab 3, Var.Var (lab 3)));
                                         (Use.make (merge 6) (Var.Var (lab 5)), Def.Instruction (lab 5, Var.Var (lab 5)))] in
    UseDefChains.check_equality ~actual:actual ~expected:expected

  let%test "use-def with memory" =
    let open Instr.Label.Test in
    let module_ = Wasm_module.of_string "(module
  (type (;0;) (func (param i32) (result i32)))
  (func (;test;) (type 0) (param i32) (result i32)
    memory.size     ;; Instr 0, Var 0
    memory.size     ;; Instr 1, Var 1
    i32.store       ;; Instr 2, i0+0 mapped to i1 (no new var!)
    memory.size     ;; Instr 3, Var 3
    memory.size     ;; Instr 4, Var 4
    i32.store)       ;; Instr 5, i3+0 mapped to i4 (no new var!)
  )" in
    let cfg = Spec_analysis.analyze_intra1 module_ 0l in
    let _, _, actual = make cfg in
    let expected = Use.Map.of_alist_exn [(Use.make (lab 2) (Var.Var (lab 0)), Def.Instruction (lab 0, Var.Var (lab 0)));
                                         (Use.make (lab 2) (Var.Var (lab 1)), Def.Instruction (lab 1, Var.Var (lab 1)));
                                         (Use.make (lab 5) (Var.Var (lab 3)), Def.Instruction (lab 3, Var.Var (lab 3)));
                                         (Use.make (lab 5) (Var.Var (lab 4)), Def.Instruction (lab 4, Var.Var (lab 4)))] in
    UseDefChains.check_equality ~actual:actual ~expected:expected

end
