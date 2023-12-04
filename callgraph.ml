open Core
open Wassail
open Utils

let callgraph =
  Command.basic
    ~summary:"Generate the call graph for the module from file [in], outputs as DOT to file [out]"
    Command.Let_syntax.(
      let%map_open file_in = anon ("in" %: string)
      and file_out = anon ("out" %: string) in
      fun () ->
        let wasm_mod = Wasm_module.of_file file_in in
        let cg = Call_graph.make wasm_mod in
        let contains_table_import = Option.is_some (
            List.find wasm_mod.imports ~f:(fun import -> match import.idesc with
                | TableImport _ -> true
                | _ -> false)) in
        if contains_table_import then
          Log.warn "Call graph generation cannot deal with imported tables if they are used for indirect calls";
        Out_channel.with_file file_out
          ~f:(fun ch ->
              Out_channel.output_string ch (Call_graph.to_dot cg)))

let callgraph_adjlist =
  Command.basic
    ~summary:"Generate the call graph for the module from file [in], outputs in a textual representation to file [out]"
    Command.Let_syntax.(
      let%map_open file_in = anon ("in" %: string)
      and file_out = anon ("out" %: string) in
      fun() ->
        let wasm_mod = Wasm_module.of_file file_in in
        let cg = Call_graph.make wasm_mod in
        Out_channel.with_file file_out
          ~f:(fun ch ->
              Out_channel.output_string ch (Call_graph.to_adjlist cg)))

let reduced_callgraph =
  Command.basic
    ~summary:"Generate the call graph for the module from file [in], only considering functions reachable from [fidx], outputs as DOT to file [out]"
    Command.Let_syntax.(
     let%map_open file_in = anon ("in" %: string)
     and fidx = anon ("fidx" %: int32)
     and file_out = anon ("out" %: string) in
     fun () ->
       let wasm_mod = Wasm_module.of_file file_in in
       let cg = Call_graph.make wasm_mod in
       let filtered_cg = Call_graph.keep_reachable cg (Int32Set.singleton fidx) in
       Out_channel.with_file file_out
         ~f:(fun ch -> Out_channel.output_string ch (Call_graph.to_dot filtered_cg)))

let schedule =
  Command.basic
    ~summary:"Generate the analysis schedule for the module from file [in]"
    Command.Let_syntax.(
      let%map_open file_in = anon ("in" %: string) in
      fun () ->
        let wasm_mod = Wasm_module.of_file file_in in
        let cg = Call_graph.make wasm_mod in
        let schedule = Call_graph.analysis_schedule cg wasm_mod.nfuncimports in
        List.iter schedule ~f:(fun elems ->
            Printf.printf "%s " (String.concat ~sep:"," (List.map elems ~f:Int32.to_string)));
        Printf.printf "\n")
