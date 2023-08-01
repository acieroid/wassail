open Core

module Time = Time_float (* Time is deprecated in Core, we should use Time_float instead *)

let report_time (msg : string) (t0 : Time.t) (t1 : Time.t) : unit =
  Printf.printf "Time for '%s': %s\n%!" msg (Time.Span.to_string (Time.diff t1 t0))

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
       ; "function-instructions", Utils.function_instructions
       ; "functions", Utils.functions
       ; "count", Utils.count

       (* Utilities that require building the CFGs *)
       ; "cfg", Cfg.cfg
       ; "cfgs", Cfg.cfgs

       ; "dependencies", Slicing.dependencies
       ; "postdom", Slicing.postdom
       ; "cdg" , Slicing.cdg

       (* Utilities that requires building the call graph *)
       ; "callgraph", Callgraph.callgraph
       ; "reduced-callgraph", Callgraph.reduced_callgraph
       ; "schedule", Callgraph.schedule

       ; "generate", Generation.generate
       ; "dump", Generation.dump

       (* Other *)
       ; "spec-inference", Analysis.spec_inference
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
       ; "count-in-slice", Slicing_evaluation.count_in_slice
       ])
