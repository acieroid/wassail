open Core
open Wassail
open Utils

let mk_intra (desc : string) (analysis : Wasm_module.t -> Int32.t list -> 'a Int32Map.t) (print : Int32.t -> 'a -> unit) =
  Command.basic
    ~summary:desc
    Command.Let_syntax.(
    let%map_open filename = anon ("file" %: string)
    and funs = anon (sequence ("funs" %: int32)) in
    fun () ->
      let results = analysis (Wasm_module.of_file filename) funs in
      Int32Map.iteri results ~f:(fun ~key:id ~data:summary -> print id summary))

let mk_summary_inter (desc : string) (analysis : Wasm_module.t -> Int32.t list list -> 'a Int32Map.t) (print : Int32.t -> 'a -> unit) =
  Command.basic
    ~summary:desc
    Command.Let_syntax.(
      let%map_open filename = anon ("file" %: string)
      and sccs = anon (sequence ("funs" %: int32_comma_separated_list)) in
      fun () ->
        let results = analysis (Wasm_module.of_file filename) sccs in
        Int32Map.iteri results ~f:(fun ~key:id ~data: summary -> print id summary))

let mk_classical_inter (desc : string) (analysis : Wasm_module.t -> Int32.t -> 'a) (print : string -> 'a -> unit) =
  Command.basic
    ~summary:desc
    Command.Let_syntax.(
      let%map_open filename = anon ("file" %: string)
      and fidx = anon ("fidx" %: int32)
      and file_out = anon ("file_out" %: string) in
      fun () ->
        let results = analysis (Wasm_module.of_file filename) fidx in
        print file_out results)

let spec_inference =
  Command.basic
    ~summary:"Annotate the CFG with the inferred variables"
    Command.Let_syntax.(
      let%map_open filename = anon ("file" %: string)
      and funs = anon (sequence ("funs" %: int32))
      and timings = flag "-timings" no_arg ~doc:" Report time spent parsing, building CFGs, analyzing, rendering DOT, and writing DOT files"
      and no_dot = flag "-no-dot" no_arg ~doc:" Run the analysis without rendering or writing DOT files" in
      fun () ->
        let (module_, parse_stats), parse_time =
          timed (fun () ->
              if timings then
                Wasm_module.of_file_with_stats filename
              else
                Wasm_module.of_file filename, { read = 0.0; decode = 0.0; convert = 0.0 })
        in
        let requested_defined_funs =
          List.filter funs ~f:(fun fid -> Int32.(fid >= module_.nfuncimports))
        in
        let cfgs, cfg_time =
          timed (fun () ->
              List.fold_left requested_defined_funs
                ~init:Int32Map.empty
                ~f:(fun cfgs fid ->
                    Int32Map.set cfgs ~key:fid ~data:(Cfg_builder.build module_ fid)))
        in
        let analyzed = ref 0 in
        let analysis_time = ref 0.0 in
        let analysis_fixpoint_time = ref 0.0 in
        let annotation_time = ref 0.0 in
        let results =
          List.fold_left funs
            ~init:Int32Map.empty
            ~f:(fun results fid ->
                if Int32.(fid < module_.nfuncimports) then
                  results
                else
                  let cfg = match Int32Map.find cfgs fid with
                    | Some r -> r
                    | None -> failwith "spec-inference: can't find CFG" in
                  let annotated_cfg, elapsed =
                    timed (fun () ->
                        if timings then begin
                          let module R = Spec_inference.Intra.Result in
                          let to_state (before, after) =
                            (R.to_state (R.Simple before), R.to_state after)
                          in
                          let analysis_result, elapsed =
                            timed (fun () -> Spec_inference.Intra.analyze_ module_ cfg ())
                          in
                          analysis_fixpoint_time := !analysis_fixpoint_time +. elapsed;
                          let annotated_cfg, elapsed =
                            timed (fun () ->
                                Cfg.map_annotations cfg
                                  ~instrs:(fun instr ->
                                      to_state (match Instr.Label.Map.find analysis_result.instrs (Instr.label instr) with
                                          | Some (before, after) -> (before, after)
                                          | None -> (Spec_inference.bottom, R.Uninitialized)))
                                  ~blocks:(fun bidx ->
                                      to_state (match Cfg.BlockIdx.Map.find analysis_result.blocks bidx with
                                          | Some (before, after) -> (before, after)
                                          | None -> (Spec_inference.bottom, R.Uninitialized))))
                          in
                          annotation_time := !annotation_time +. elapsed;
                          annotated_cfg
                        end else
                          Spec_inference.Intra.analyze module_ cfg ())
                  in
                  incr analyzed;
                  analysis_time := !analysis_time +. elapsed;
                  Int32Map.set results ~key:fid ~data:annotated_cfg)
        in
        let dot_time = ref 0.0 in
        let write_time = ref 0.0 in
        if not no_dot then
          Int32Map.iteri results ~f:(fun ~key:fid ~data:annotated_cfg ->
              let dot, elapsed =
                timed (fun () -> Cfg.to_dot annotated_cfg ~annot_str:Spec_domain.to_dot_string)
              in
              dot_time := !dot_time +. elapsed;
              let (), elapsed =
                timed (fun () ->
                    let file_out = Printf.sprintf "%ld.dot" fid in
                    Out_channel.with_file file_out
                      ~f:(fun ch ->
                          Out_channel.output_string ch dot))
              in
              write_time := !write_time +. elapsed);
        if timings then begin
          let total = parse_time +. cfg_time +. !analysis_time +. !dot_time +. !write_time in
          eprintf
            "spec_inference timings: requested=%d analyzed=%d parse=%.6fs cfg=%.6fs analysis=%.6fs analysis_fixpoint=%.6fs annotation=%.6fs dot=%.6fs write=%.6fs measured_total=%.6fs\nparse timings: read=%.6fs decode=%.6fs convert=%.6fs\n%!"
            (List.length funs)
            !analyzed
            parse_time
            cfg_time
            !analysis_time
            !analysis_fixpoint_time
            !annotation_time
            !dot_time
            !write_time
            total
            parse_stats.read
            parse_stats.decode
            parse_stats.convert
        end)

let spec_inference_inter =
  mk_classical_inter "Annotate the ICFG with the inferred variables"
    (fun module_ fidx ->
       Spec_analysis.analyze_inter_classical module_ fidx)
    (fun file_out icfg ->
       Out_channel.with_file file_out
         ~f:(fun ch ->
             Out_channel.output_string ch (ICFG.to_dot icfg ~annot_str:Spec_domain.to_dot_string)))

let taint_intra =
  mk_intra "Just like `intra`, but only performs the taint analysis" Taint.analyze_intra
    (fun fid data ->
       Printf.printf "function %ld: %s\n" fid (Taint.Summary.to_string (fst data)))

let taint_cfg =
  Command.basic
    ~summary:"Generate a DOT file representing the taint-annotated CFG of function [fid] from the wasm file [in], in file [out]"
    Command.Let_syntax.(
      let%map_open file_in = anon ("in" %: string) and
      file_out = anon ("out" %: string) and
      funs = anon (sequence ("funs" %: int32)) in
      fun () ->
        Spec_inference.use_const := false;
        let results = Taint.analyze_intra (Wasm_module.of_file file_in) funs in
        (* We only output the latest analyzed CFG *)
        let annotated_cfg = Option.value_exn (snd (Int32Map.find_exn results (List.last_exn funs))) in
        output_to_file file_out (Cfg.to_dot annotated_cfg ~annot_str:Taint.Domain.only_non_id_to_string))

let taint_inter =
  mk_summary_inter "Performs summary-based interprocedural taint analysis of a set of functions in file [file]. [funs] is a list of comma-separated function ids, e.g., to analyze function 1, then analyze both function 2 and 3 as part of the same fixpoint computation, [funs] is 1 2,3. The full schedule for any file can be computed using the `schedule` target."
    Taint.analyze_inter
    (fun fid (_, _, summary) -> Printf.printf "function %ld: %s\n" fid (Taint.Summary.to_string summary))

let taint_inter_classical =
  mk_classical_inter "Perform classical interprocedural taint analysis from a given entry point"
    (fun module_ fidx ->
       Taint.analyze_inter_classical module_ fidx)
    (fun file_out icfg ->
       Out_channel.with_file file_out
         ~f:(fun ch ->
             Out_channel.output_string ch (ICFG.to_dot icfg ~annot_str:Taint.Domain.to_dot_string)))

let find_indirect_calls =
  Command.basic
    ~summary:"Find call_indirect instructions and shows the function in which they appear as well as their label"
    Command.Let_syntax.(
      let%map_open filename = anon ("file" %: string) in
      fun () ->
        let module_ = Wasm_module.of_file filename in
        Wasm_module.iter_defined_funcs module_ ~f:(fun finst ->
            let cfg = Spec_analysis.analyze_intra1 module_ finst.idx in
            let indirect_calls = Slicing.find_call_indirect_instructions cfg in
            List.iter indirect_calls ~f:(fun label ->
                Printf.printf "function %ld, instruction %s\n" finst.idx (Instr.Label.to_string label))))

let taint_flow_from_exported_to_imported =
  Command.basic
    ~summary:"Detects unsafe flows from exported functions to imported functions"
    Command.Let_syntax.(
      let%map_open filename = anon ("file" %: string) in
      fun () ->
        let module_ = Wasm_module.of_file filename in
        let _ = Taintcall.detect_flows_from_exported_to_imported module_ in
        ())

let taint_flow_from_sources_to_sinks =
  Command.basic
    ~summary:"Detects unsafe flows from a list of sources to a list of defined sinks"
    Command.Let_syntax.(
      let%map_open filename = anon ("file" %: string)
      and source_names = anon ("sources" %: string_comma_separated_list)
      and sink_names = anon ("sinks" %: string_comma_separated_list) in
      fun () ->
        let module_ = Wasm_module.of_file filename in
        let _ = Taintcall.detect_flows_from_sources_to_sinks module_ (StringSet.of_list source_names) (StringSet.of_list sink_names) in
        ())

let taintcall_cfg =
  Command.basic
    ~summary:"Performs a inter-procedural taintcall analysis and displays the results for the given CFGs"
    Command.Let_syntax.(
      let%map_open filename = anon ("file" %: string)
      and cfgs = anon ("cfgs" %: int32_comma_separated_list) in
      fun () ->
        let module_ = Wasm_module.of_file filename in
        let cg = Call_graph.make module_ in
        let schedule = Call_graph.analysis_schedule cg module_.nfuncimports in
        let results = Taintcall.analyze_inter module_ schedule in
        List.iter cfgs ~f:(fun idx ->
            let (_, cfg, _) = Int32Map.find_exn results idx in
            let file_out = Printf.sprintf "%ld.dot" idx in
            Out_channel.with_file file_out
              ~f:(fun ch ->
                  Out_channel.output_string ch (Cfg.to_dot cfg ~annot_str:Taintcall.Domain.to_string))))
