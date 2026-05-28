open Core

(** Maps each global variable to the set of [global.set] instructions that may define it.

    Each definition is represented as a {!Global_read_domain.GlobalInstruction.t},
    which pairs a textual identifier of the global (typically obtained via {!Var.to_string})
    with the label of the corresponding instruction. *)
type t = Global_read_domain.GlobalInstruction.Set.t Var.Map.t

(** Computes the global definitions present in [module_].

    The result maps each global variable to the set of all [global.set]
    instructions that syntactically assign to it across all functions of the module.

    Internally, each function is analyzed independently using an intra-procedural
    specification analysis (via {!Spec_analysis.analyze_intra1}), and all
    instructions in the resulting CFG are inspected.

    For each encountered [global.set g], a definition entry is added for the
    corresponding {!Var.Global} variable.

    During the analysis of each function, global propagation
    ({!Spec_inference.propagate_globals}) is temporarily disabled to avoid
    interference with definition collection. Its original value is restored
    after each successful function analysis. *)
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
              | Some defs -> Set.add defs (Var.to_string global, label))
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

(* 
let%test_module "abstract store tests" = (module struct
  let%test "Global_defs_tests" =
    print_endline "_______ _____________________ _______\n        Global definitions        \n------- --------------------- -------\n";
    true

  let%test "global defs from a sample wat file" =
    let module_ : Wasm_module.t = 
      "(module
        ;; Three mutable globals
        (global $g0 (mut i32) (i32.const 0))
        (global $g1 (mut i32) (i32.const 10))
        (global $g2 (mut i32) (i32.const 100))
        (global $g3 (mut i32) (i32.const 1000))

        ;; First function
        (func $f1
          i32.const 1
          global.set $g0

          i32.const 2
          global.set $g1

          i32.const 3
          global.set $g0
        )

        ;; Second function
        (func $f2
          i32.const 20
          global.set $g1

          i32.const 30
          global.set $g2

          i32.const 40
          global.set $g1
        )
      )"
    |> Wasm_module.of_string in
    let global_defs = module_ |> make in
    let g0_1 : Instr.Label.t = { section = Function 0l; id = 1 } in
    let g0_2 : Instr.Label.t = { section = Function 0l; id = 5 } in
    let g0_defs : Global_read_domain.GlobalInstruction.Set.t = (Global_read_domain.GlobalInstruction.Set.of_list [("g0", g0_1); ("g0", g0_2)]) in
    let g1_1 : Instr.Label.t = { section = Function 0l; id = 3 } in
    let g1_2 : Instr.Label.t = { section = Function 1l; id = 1 } in
    let g1_3 : Instr.Label.t = { section = Function 1l; id = 5 } in
    let g1_defs : Global_read_domain.GlobalInstruction.Set.t = (Global_read_domain.GlobalInstruction.Set.of_list [("g1", g1_1); ("g1", g1_2); ("g1", g1_3)]) in
    let g2 : Instr.Label.t = { section = Function 1l; id = 3 } in
    let g2_defs : Global_read_domain.GlobalInstruction.Set.t = (Global_read_domain.GlobalInstruction.Set.singleton ("g2", g2)) in
    let expected = Var.Map.of_alist_exn [Var.Global 0, g0_defs; Var.Global 1, g1_defs; Var.Global 2, g2_defs] in
    print_endline (Wasm_module.to_string module_);
    print_endline "Global definitions:";
    print_endline (Var.Map.to_string global_defs Global_read_domain.GlobalInstruction.Set.to_string);
    not (Var.Map.equal global_defs)
end) *)
