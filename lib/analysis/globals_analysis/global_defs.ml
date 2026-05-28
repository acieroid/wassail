open Core

(** Maps each global variable to the set of [global.set] instructions that may
    define it.

    Each definition is represented as a
    {!Global_read_domain.GlobalInstruction.t}, which pairs:
    - a textual identifier of the global (typically obtained via
      {!Var.to_string});
    - the label of the corresponding [global.set] instruction. *)
type t = Global_read_domain.GlobalInstruction.Set.t Var.Map.t


(** Structural equality between two global-definition maps. *)
let equal (x : t) (y : t) : bool =
  Var.Map.equal Global_read_domain.GlobalInstruction.Set.equal x y

(** Computes the set of syntactic global definitions present in [module_].

    The resulting map associates each global variable with all
    [global.set] instructions that may assign to it anywhere in the module.

    Each function is analyzed independently using
    {!Spec_analysis.analyze_intra1} in order to obtain its CFG annotated with
    instruction labels. All instructions of the CFG are then traversed, and
    every encountered [global.set g] contributes a definition for the
    corresponding {!Var.Global} variable.

    During the analysis of each function,
    {!Spec_inference.propagate_globals} is temporarily disabled to avoid
    interference with definition collection.

    The original value of {!Spec_inference.propagate_globals} is restored after
    each function analysis, even if the analysis raises an exception. *)
let make (module_ : Wasm_module.t) : t =
  let initial_propagation = !Spec_inference.propagate_globals in
  let functions = module_.funcs in
  List.fold functions 
    ~init:Var.Map.empty
    ~f:(fun acc func ->
      Spec_inference.propagate_globals := false;
      let cfg = 
        Exn.protect
          ~f:(fun () ->
            Spec_analysis.analyze_intra1 module_ func.idx)
          ~finally:(fun () ->
            Spec_inference.propagate_globals := initial_propagation)
      in
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
      
(** The empty mapping of global definitions. *)
let empty : t = Var.Map.empty

(** Returns the set of [global.set] instructions associated with
    [global_var], if any are known in [defs].

    Each returned element contains:
    - the textual identifier of the global;
    - the label of a corresponding defining instruction. *)
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


let%test_module "Global definitions tests" = (module struct
  let%test "Global_defs_tests" =
    print_endline "_______ _____________________ _______\n        Global definitions        \n------- --------------------- -------\n";
    true

  let%test "global defs from a sample wat file" =
    print_endline "[Testing global definitions in a sample wat file]";
    let global_defs : t = 
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
    |> Wasm_module.of_string
    |> make in
    let g0_1 : Instr.Label.t = { section = Function 0l; id = 1 } in
    let g0_2 : Instr.Label.t = { section = Function 0l; id = 5 } in
    let g0_defs : Global_read_domain.GlobalInstruction.Set.t = (Global_read_domain.GlobalInstruction.Set.of_list [("g0", g0_1); ("g0", g0_2)]) in
    let g1_1 : Instr.Label.t = { section = Function 0l; id = 3 } in
    let g1_2 : Instr.Label.t = { section = Function 1l; id = 1 } in
    let g1_3 : Instr.Label.t = { section = Function 1l; id = 5 } in
    let g1_defs : Global_read_domain.GlobalInstruction.Set.t = (Global_read_domain.GlobalInstruction.Set.of_list [("g1", g1_1); ("g1", g1_2); ("g1", g1_3)]) in
    let g2 : Instr.Label.t = { section = Function 1l; id = 3 } in
    let g2_defs : Global_read_domain.GlobalInstruction.Set.t = (Global_read_domain.GlobalInstruction.Set.singleton ("g2", g2)) in
    let expected : t = Var.Map.of_alist_exn [Var.Global 0, g0_defs; Var.Global 1, g1_defs; Var.Global 2, g2_defs] in
    print_endline "\tGlobal definitions:";
    print_endline ("\t" ^ Var.Map.to_string global_defs Global_read_domain.GlobalInstruction.Set.to_string);
    equal expected global_defs

  let%test "no global defs" =
    print_endline "[No global definitions]";
    let global_defs =
      "(module
        (global $g0 (mut i32) (i32.const 0))
        (func $f
          global.get $g0
          drop))"
      |> Wasm_module.of_string
      |> make
    in
    equal empty global_defs
  

  let%test "get unknown global returns none" =
    print_endline "[get returns None for unknown global]";
    let defs = empty in
    Option.is_none (get ~defs ~global_var:(Var.Global 0))

  let%test "get known global returns definitions" =
    print_endline "[get returns the right label set]";
    let label = { Instr.Label.section = Function 0l; id = 1 } in
    let defs_set =
      Global_read_domain.GlobalInstruction.Set.singleton ("g0", label)
    in
    let defs =
      Var.Map.of_alist_exn [ Var.Global 0, defs_set ]
    in
    match get ~defs ~global_var:(Var.Global 0) with
    | Some actual -> Global_read_domain.GlobalInstruction.Set.equal defs_set actual
    | None -> false
end)
