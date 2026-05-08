open Core

(** Maps each global variable to the set of labels of the [global.set]
    instructions that may define it. *)
type t = Instr.Label.Set.t Var.Map.t

(** Computes the global definitions present in [module_].

    The result associates each global variable with the labels of all
    [global.set] instructions that assign to it.

    This analysis temporarily disables global propagation in
    {!Spec_inference.propagate_globals}. The original value of [propagate_globals] is restored
    after the first intra-procedural analysis is launched. *)
let make (module_ : Wasm_module.t) : t =
  let initial_propagation = !Spec_inference.propagate_globals in
  Spec_inference.propagate_globals := false;
  let functions = module_.funcs in
  List.fold functions 
    ~init:Var.Map.empty
    ~f:(fun acc func ->
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
              | None -> Instr.Label.Set.singleton label
              | Some defs -> Var.Set.add defs label)
          | _ -> acc
        )
      )
    
(** The empty set of global definitions. *)
let empty : t = Var.Map.empty

(** Returns the labels of the [global.set] instructions defining [global_var],
    if any are known in [defs]. *)
let get ~(defs : t) ~(global_var : Var.t) : Instr.Label.Set.t option =
  Var.Map.find defs global_var