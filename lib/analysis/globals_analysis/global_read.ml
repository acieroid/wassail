open Helpers
open Core

(** Interprocedural analysis that computes, for each WebAssembly function, the set of
    global variables that may be read during its execution.

    The analysis is summary-based: each function is associated with a summary
    describing the globals that may be accessed, and these summaries are propagated
    through the call graph until a fixpoint is reached. *)

(** Abstract domain used to represent the set of globals that may be read. *)
module Domain = Global_read_domain

(** Transfer functions used by the intraprocedural analyzer. *)
module Transfer = Global_read_transfer.Make

(** Function summaries produced and consumed by the global-read analysis. *)
module Summary = Global_read_summary

(** Summary-based intraprocedural analyzer instantiated with the global-read
    transfer functions. *)
module Intra = Intra.MakeSummaryBased(Transfer)

(** Summary-based interprocedural analyzer built from the intraprocedural
    global-read analysis. *)
module Inter = Inter.MakeSummaryBased(Transfer)(Intra)


(** [analyze_inter wasm_mod sccs] runs the summary-based interprocedural
    global-read analysis over the strongly connected components [sccs] of
    [wasm_mod].

    For each SCC, the analyzer collects the summaries already computed for other
    functions, adds conservative summaries for imported functions, and then analyzes
    the functions in the SCC until their summaries stabilize.

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
        (Printf.sprintf "---------- Global-read analysis of SCC {%s} ----------"
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

let function_global_deps (module_ : Wasm_module.t) : Summary.t Int32Map.t =
  let cg = module_ |> Call_graph.make in
  let schedule = Call_graph.analysis_schedule cg module_.nfuncimports in
  let globals_usage = analyze_inter module_ schedule in
  Int32Map.map globals_usage ~f:(fun (_,_,summary) -> summary)