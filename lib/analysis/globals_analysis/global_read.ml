open Helpers
open Core

(** Interprocedural analysis that computes, for each WebAssembly function, the
    global definitions that may be read during its execution.

    The analysis is summary-based: each function is associated with a summary
    describing the [global.set] instructions whose definitions may be observed by
    reads performed by the function, either directly or through calls. These
    summaries are propagated through the call graph until a fixpoint is reached.

    This module is used to determine which global definitions a call instruction
    may depend on. This information can then be used by the slicer to avoid adding
    dependencies from a call to unrelated [global.set] instructions. *)

(** Abstract domain used to represent the set of global definitions that may be
    read.

    Finite elements contain sets of {!Global_read_domain.GlobalInstruction.t};
    [Top] represents the case where any global definition may be read. *)
module Domain = Global_read_domain

(** Transfer functions used by the intraprocedural analyzer.

    The transfer module updates the current abstract state when global reads are
    encountered and uses the precomputed global-definition map supplied through
    {!Global_read_transfer.Make.set_global_defs}. *)
module Transfer = Global_read_transfer.Make

(** Function summaries produced and consumed by the global-read analysis.

    In this analysis, a summary is the same kind of information as an abstract
    state: it describes which global definitions may be read during a function's
    execution. *)
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

    The returned map associates each function index with:

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
  let globals_usage = analyze_inter module_ schedule in
  Int32Map.map globals_usage ~f:(fun (_,_,summary) -> summary)