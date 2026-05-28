open Helpers
open Core

(** Interprocedural analysis that computes, for each WebAssembly function, the
    [global.set] definitions that may be required during its execution,
    according to the globals that may be read.

    The analysis is summary-based: each function is associated with a summary
    describing the [global.set] instructions whose definitions may be observed by
    reads performed by the function, either directly or through calls. These
    summaries are propagated through the call graph until a fixpoint is reached.

    This module is used to determine which global definitions a call instruction
    may depend on. This information can then be used by the slicer to avoid adding
    dependencies from a call to unrelated [global.set] instructions. *)

(** Abstract domain used to represent the set of [global.set] definitions that
    may be required because of global reads.

    Finite elements contain sets of {!Global_read_domain.GlobalInstruction.t};
    [Top] represents the case where any global definition may be required. *)
module Domain = Global_read_domain

(** Transfer functions used by the intraprocedural analyzer.

    The transfer module updates the current abstract state when global reads are
    encountered and uses the precomputed global-definition map supplied through
    {!Global_read_transfer.Make.set_global_defs}. *)
module Transfer = Global_read_transfer.Make

(** Function summaries produced and consumed by the global-read analysis.

    In this analysis, a summary is the same kind of information as an abstract
    state: it describes which [global.set] definitions may be required during a function's
    execution, according to the globals that may be read. *)
module Summary = Global_read_summary

(** Summary-based intraprocedural analyzer instantiated with the global-read
    transfer functions.

    This analyzer computes a global-read abstract state at each instruction of a
    single function, while consulting the currently available summaries for calls. *)
module Intra = Intra.MakeSummaryBased(Transfer)

(** Summary-based interprocedural analyzer built from the intraprocedural
    global-read analysis.

    It repeatedly invokes the intraprocedural analyzer over SCCs of the call graph
    until function summaries stabilize. *)
module Inter = Inter.MakeSummaryBased(Transfer)(Intra)


(** [analyze_inter wasm_mod sccs] runs the summary-based interprocedural
    global-read analysis over the strongly connected components [sccs] of
    [wasm_mod].

    Before the SCC traversal starts, {!Global_defs.make} is run once as a
    preanalysis. The resulting map associates each global variable with the
    [global.set] instructions that may define it. This map is then passed to the
    transfer functions before each SCC is analyzed.

    For each SCC, the analyzer collects the summaries already computed for other
    functions, adds conservative [Top] summaries for imported functions, and then
    analyzes the functions in the SCC until their summaries stabilize.

    The returned map associates each function index with the following information:

    - its original specification-annotated CFG;
    - the CFG annotated with global-read abstract states;
    - the final global-read summary for the function. *)
let analyze_inter : Wasm_module.t -> Int32.t list list -> (Spec_domain.t Cfg.t * Global_read_domain.t Cfg.t * Summary.t) Int32Map.t =
  Analysis_helpers.mk_inter_with_preanalysis
    (fun wasm_mod ~cfgs:_ -> Global_defs.make wasm_mod)
    (fun _ _ -> Int32Map.empty)
    (fun global_defs wasm_mod ~cfgs:scc ~summaries:cfgs_and_summaries ->
      Log.info
        (Printf.sprintf "-------------------- Global-read analysis of SCC {%s} --------------------"
          (String.concat ~sep:", " (List.map (Int32Map.keys scc) ~f:Int32.to_string)));
      (* Run the global-read analysis. *)
      let annotated_scc = scc in
      let summaries = Int32Map.mapi cfgs_and_summaries ~f:(fun ~key:_idx ~data:(_, _, summary) -> summary) in
      let summaries' = List.fold_left wasm_mod.imported_funcs
                          ~init:summaries
                          ~f:(fun summaries desc ->
                              Int32Map.set summaries ~key:desc.idx ~data:Global_read_domain.Top) in
      Transfer.set_global_defs global_defs;
      let results = Inter.analyze wasm_mod ~cfgs:annotated_scc ~summaries:summaries' in
      Int32Map.mapi results ~f:(fun ~key:idx ~data:(global_read_cfg, summary) ->
          let spec_cfg = Int32Map.find_exn scc idx in
          (spec_cfg, global_read_cfg, summary)))

(** Computes the global-read summary of every non-imported function in [module_].

    This is the main entry point for clients that only need the final summaries,
    rather than the intermediate CFG annotations produced by {!analyze_inter}.

    The function first builds the call graph of [module_], derives an SCC-based
    analysis schedule, runs {!analyze_inter}, and finally projects away the CFGs
    to keep only the summary associated with each function index. *)
let function_global_deps (module_ : Wasm_module.t) : Summary.t Int32Map.t =
  let cg = module_ |> Call_graph.make in
  let schedule = Call_graph.analysis_schedule cg module_.nfuncimports in
  let global_deps = analyze_inter module_ schedule in
  Int32Map.map global_deps ~f:(fun (_,_,summary) -> summary)




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
    print_endline "_______ ____________________ _______\n        Global-read analysis       \n------- -------------------- -------\n";
    true

  let%test "direct global.get adds corresponding global definitions" =
    let module_ = Wasm_module.of_string
    "(module
      (global $g0 (mut i32) (i32.const 0))
      (global $g1 (mut i32) (i32.const 10))
      (func
        global.get $g0
        drop
      )
      (func
        i32.const 14
        global.set $g1
        i32.const 1
        global.set $g0
      )
      (func
        i32.const 2
        global.set $g0
      )
    )"
    in
    let analysis = analyze_inter module_ [[0l;1l;2l]] in
    let (_, _, fct0_summary) = Int32Map.find_exn analysis 0l in
    let global_defs = Global_defs.make module_ in
    let expected : Domain.t = NotTop (Int32Map.find_exn global_defs (Var.Global 0)) in
    print_endline "[summary of a function]";
    print_endline ("\tsummary of function 0: " ^ Summary.to_string fct0_summary);
    print_endline ("\texpected: " ^ Summary.to_string expected);
    Summary.equal fct0_summary expected

  let%test "transitive call dependency" =
    let module_ = Wasm_module.of_string
    "(module
      (global $g0 (mut i32) (i32.const 0))
      (global $g1 (mut i32) (i32.const 10))
      (func $callee
        global.get $g0
        drop
        i32.const 55
        global.set $g1
      )
      (func $caller
        call $callee
      )
      (func
        i32.const 1
        global.set $g0
        i32.const 14
        global.set $g1
      )
    )"
    in
    let analysis = analyze_inter module_ [[2l;0l;1l]] in
    let (_, _, fct1_summary) = Int32Map.find_exn analysis 1l in
    let global_defs = Global_defs.make module_ in
    let expected : Domain.t = NotTop (Int32Map.find_exn global_defs (Var.Global 0)) in
    print_endline "[transitive call dependency]";
    print_endline ("\tsummary of function 1: " ^ Summary.to_string fct1_summary);
    print_endline ("\texpected: " ^ Summary.to_string expected);
    Summary.equal fct1_summary expected

  let%test "recursive SCC stabilization" =
    let module_ = Wasm_module.of_string
    "(module
      (global $g0 (mut i32) (i32.const 0))
      (global $g1 (mut i32) (i32.const 10))
      (func $f
        call $g
      )
      (func $g
        global.get 0
        drop
        call $f
        i32.const 14
        global.set $g1
      )
      (func
        i32.const 1
        global.set 0
      )
    )"
    in
    let analysis = analyze_inter module_ [[2l;1l;0l]] in
    let (_, _, fct0_summary) = Int32Map.find_exn analysis 0l in
    let (_, _, fct1_summary) = Int32Map.find_exn analysis 1l in
    let global_defs = Global_defs.make module_ in
    let expected : Domain.t = NotTop (Int32Map.find_exn global_defs (Var.Global 0)) in
    print_endline "[recursive SCC stabilization]";
    print_endline ("\tsummary of function 0: " ^ Summary.to_string fct0_summary);
    print_endline ("\tsummary of function 1: " ^ Summary.to_string fct1_summary);
    print_endline ("\texpected: " ^ Summary.to_string expected);
    Summary.equal fct0_summary expected && Summary.equal fct1_summary expected

  let%test "imported function produces Top" =
    let module_ = Wasm_module.of_string
    "(module
      (import \"env\" \"foo\" (func $foo))
      (global $g0 (mut i32) (i32.const 0))
      (global $g1 (mut i32) (i32.const 10))
      (func
        call $foo
      )
    )"
    in
    let analysis = analyze_inter module_ [[2l;1l;0l]] in
    let (_, _, fct1_summary) = Int32Map.find_exn analysis 1l in
    let expected : Domain.t = Top in
    print_endline "[imported function produces Top]";
    print_endline ("\tsummary of function 1: " ^ Summary.to_string fct1_summary);
    print_endline ("\texpected: " ^ Summary.to_string expected);
    Summary.equal fct1_summary expected

  let%test "function with no global reads" =
    let module_ = Wasm_module.of_string
    "(module
      (import \"env\" \"foo\" (func $foo))
      (global $g0 (mut i32) (i32.const 0))
      (global $g1 (mut i32) (i32.const 10))
      (func
        global.get $g0
        global.set $g1
      )
      (func
        i32.const 42
        global.set $g0
      )
    )"
    in
    let analysis = analyze_inter module_ [[0l;1l;2l]] in
    let (_, _, fct2_summary) = Int32Map.find_exn analysis 2l in
    let expected : Domain.t = Summary.bottom in
    print_endline "[function with no global reads]";
    print_endline ("\tsummary of function 2: " ^ Summary.to_string fct2_summary);
    print_endline ("\texpected: " ^ Summary.to_string expected);
    Summary.equal expected fct2_summary

  let%test "multiple calls merge summaries" =
    let module_ = Wasm_module.of_string
    "(module
      (global $g0 (mut i32) (i32.const 0))
      (global $g1 (mut i32) (i32.const 10))
      (func $f
        global.get 0
        drop
        global.get 0
        drop
      )
      (func $g
        global.get 1
        drop
      )
      (func $h
        call $f
        call $g
      )
      (func
        i32.const 1
        global.set 0
      )
      (func
        i32.const 2
        global.set 1
      )
    )"
    in
    let analysis = analyze_inter module_ [[2l;1l;0l]] in
    let (_, _, fct2_summary) = Int32Map.find_exn analysis 2l in
    let global_defs = Global_defs.make module_ in
    let expected : Domain.t = NotTop (Int32Map.find_exn global_defs (Var.Global 0)) 
      |> Domain.join (NotTop (Int32Map.find_exn global_defs (Var.Global 1))) in
    print_endline "[multiple calls merge summaries]";
    print_endline ("\tsummary of function 2: " ^ Summary.to_string fct2_summary);
    print_endline ("\texpected: " ^ Summary.to_string expected);
    Summary.equal fct2_summary expected


  let%test "global.get with no global.set" =
    let module_ = Wasm_module.of_string
    "(module
      (global $g0 (mut i32) (i32.const 0))
      (global $g1 (mut i32) (i32.const 10))
      (func
        global.get $g0
        drop
        global.get $g0
        drop
      )
      (func
        i32.const 14
        global.set $g1
      )
    )"
    in
    let analysis = analyze_inter module_ [[1l;0l]] in
    let (_, _, fct0_summary) = Int32Map.find_exn analysis 0l in
    let expected : Domain.t = Domain.bottom in
    print_endline "[global.get with no global.set]";
    print_endline ("\tsummary of function 0: " ^ Summary.to_string fct0_summary);
    print_endline ("\texpected: " ^ Summary.to_string expected);
    Summary.equal fct0_summary expected
end)