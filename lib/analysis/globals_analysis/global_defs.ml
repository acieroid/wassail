open Core

type t = Var.Set.t Var.Map.t

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
            let def_var = Var.Var label in
            let global = Var.Global (Int32.to_int_exn g) in
            Var.Map.update acc global ~f:(function
              | None -> Var.Set.singleton def_var
              | Some defs -> Var.Set.add defs def_var)
          | _ -> acc
        )
      )

let to_string (defs : t) : string =
  if Var.Map.is_empty defs then
    "No globals are defined in this program"
  else
    Var.Map.to_string defs Var.Set.to_string
    
let empty : t = Var.Map.empty


let get ~(defs : t) ~(global_var : Var.t) : Var.Set.t option =
  Var.Map.find defs global_var