open Core

let report_time (msg : string) (t0 : Time_float.t) (t1 : Time_float.t) : unit =
  Printf.printf "Time_float for '%s': %s\n%!" msg (Time_float.Span.to_string (Time_float.diff t1 t0))

let () =
  Wassail.Log.enable_info ();
  Command_unix.run ~version:"0.0"
    (Command.group ~summary:"Static analysis of WebAssembly"
       [

       (* General utilities that only required to load the WebAssembly files *)
         "load", Utils.load
       ; "imports", Utils.imports
       ; "exports", Utils.exports
       ; "instructions", Utils.instructions
       ; "sizes", Utils.sizes
       ; "mem-imports", Utils.mem_imports
       ; "mem-exports", Utils.mem_exports
       ; "function-instruction-labels", Utils.function_instruction_labels
       ; "function-body", Utils.function_body
       ; "functions", Utils.functions
       ; "count", Utils.count

       (* Utilities that require building the CFGs *)
       ; "cfg", Cfg.cfg
       ; "cfg-adjlist", Cfg.cfg_adjlist
       ; "cfgs", Cfg.cfgs
       ; "icfg", Cfg.icfg

       ; "dependencies", Slicing.dependencies
       ; "postdom", Slicing.postdom
       ; "cdg" , Slicing.cdg

       (* Utilities that requires building the call graph *)
       ; "callgraph", Callgraph.callgraph
       ; "callgraph-adjlist", Callgraph.callgraph_adjlist
       ; "reduced-callgraph", Callgraph.reduced_callgraph
       ; "schedule", Callgraph.schedule

       ; "generate", Generation.generate
       ; "dump", Generation.dump

       (* Stack specification analysis *)
       ; "spec-inference", Analysis.spec_inference
       ; "spec-inference-inter", Analysis.spec_inference_inter

       (* Taint analysis *)
       ; "taint-cfg", Analysis.taint_cfg
       ; "taint-intra", Analysis.taint_intra
       ; "taint-inter", Analysis.taint_inter
       ; "taint-from-exported-to-imported", Analysis.taint_flow_from_exported_to_imported
       ; "taint-from-sources-to-sinks", Analysis.taint_flow_from_sources_to_sinks
       ; "taintcall-cfg", Analysis.taintcall_cfg
       ; "find-indirect-calls", Analysis.find_indirect_calls

       (* Slicing *)
       ; "slice", Slicing.slice_line_number
       ; "evaluate-slicing", Slicing_evaluation.evaluate
       ; "gen-slice-specific", Slicing_evaluation.gen_slice_specific
       ; "find-criterion", Slicing_evaluation.find_criterion
       ; "count-in-slice", Slicing_evaluation.count_in_slice
       ])
