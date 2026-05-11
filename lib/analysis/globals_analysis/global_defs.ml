open Core

(** Maps each global variable to the set of [global.set] instructions that may define it.

    Each definition is represented as a {!Global_read_domain.GlobalInstruction.t},
    which pairs a textual identifier of the global (typically obtained via {!Var.to_string})
    with the label of the corresponding instruction. *)
type t = Global_read_domain.GlobalInstruction.Set.t Var.Map.t

(** Computes the global definitions present in [module_].

    The result maps each global variable to the set of all [global.set]
    instructions that may assign to it across all functions of the module.

    Internally, each function is analyzed independently using an intra-procedural
    specification analysis (via {!Spec_analysis.analyze_intra1}), and all
    instructions in the resulting CFG are inspected.

    For each encountered [global.set g], a definition entry is added for the
    corresponding {!Var.Global} variable.

    During the analysis of each function, global propagation
    ({!Spec_inference.propagate_globals}) is temporarily disabled to avoid
    interference with definition collection. Its original value is restored
    immediately after analyzing each function. *)
let make (module_ : Wasm_module.t) : t =
  let initial_propagation = !Spec_inference.propagate_globals in
  let functions = module_.funcs in
  List.fold functions 
    ~init:Var.Map.empty
    ~f:(fun acc func ->
      Spec_inference.propagate_globals := false;
      let cfg = Spec_analysis.analyze_intra1 module_ func.idx in
      Spec_inference.propagate_globals := initial_propagation;
      List.fold 
        (Cfg.all_instructions_list cfg)
        ~init:acc
        ~f:(fun acc instr ->
          match instr with
          | Data { label; instr=(GlobalSet g); _ } ->
            let global = Var.Global (Int32.to_int_exn g) in
            Var.Map.update acc global ~f:(function
              | None -> Global_read_domain.GlobalInstruction.Set.singleton (Var.to_string global, label)
              | Some defs -> Var.Set.add defs (Var.to_string global, label))
          | _ -> acc
        )
      )
      
(** The empty set of global definitions. *)
let empty : t = Var.Map.empty

(** Returns the set of [global.set] instructions that may define [global_var],
    if any are known in [defs].

    Each element of the returned set contains both the global identifier
    (as a string) and the label of the defining instruction. *)
let get ~(defs : t) ~(global_var : Var.t) : Global_read_domain.GlobalInstruction.Set.t option =
  Var.Map.find defs global_var