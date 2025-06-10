(** 
  This module defines CLI commands for performing various intra- and inter-procedural analyses 
  on WebAssembly (Wasm) modules. It includes taint analysis, CFG annotation, detection of 
  indirect calls, and information flow checking from exports to imports and from user-defined 
  sources to sinks. 
*)

open Core
open Wassail
open Utils

(** 
  [mk_intra desc analysis print] creates a command-line command for running an intra-procedural 
  analysis over specified Wasm functions. The [analysis] function is applied to the loaded module 
  and list of function IDs. The result is printed using the [print] function.
*)
let mk_intra (desc : string) (analysis : Wasm_module.t -> Int32.t list -> 'a Int32Map.t) (print : Int32.t -> 'a -> unit) =
  Command.basic
    ~summary:desc
    Command.Let_syntax.(
    let%map_open filename = anon ("file" %: string)
    and funs = anon (sequence ("funs" %: int32)) in
    fun () ->
      let results = analysis (Wasm_module.of_file filename) funs in
      Int32Map.iteri results ~f:(fun ~key:id ~data:summary -> print id summary))


(** 
  Command that infers variable specifications and annotates the control-flow graph (CFG) 
  of each function. The annotated CFG is saved as a DOT file named [fid.dot]. 
*)
let spec_inference =
  mk_intra "Annotate the CFG with the inferred variables"
    (Analysis_helpers.mk_intra (fun _ _ -> Int32Map.empty) (fun _ _ annotated_cfg -> annotated_cfg))
    (fun fid annotated_cfg ->
       let file_out = Printf.sprintf "%ld.dot" fid in
       Out_channel.with_file file_out
         ~f:(fun ch ->
             Out_channel.output_string ch (Cfg.to_dot annotated_cfg ~annot_str:Spec.to_dot_string)))

(** 
  Command that runs intra-procedural taint analysis on each specified function and prints 
  the resulting taint summary.
*)
let taint_intra =
  mk_intra "Just like `intra`, but only performs the taint analysis" Taint.analyze_intra
    (fun fid data ->
      Printf.printf "function %ld: %s\n" fid (Taint.Summary.to_string (fst data)))

let value_set_intra =
  mk_intra "intra-procedural value-set analysis" Value_set.analyze_intra
    (fun fid data ->
      Printf.printf "function %ld: %s\n" fid (Value_set.Summary.to_string (fst data)))

(** 
  Command that performs intra-procedural taint analysis and outputs the taint-annotated CFG 
  of the last function in the input list to the specified DOT file.
*)
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

(* let relational_intra =
  mk_intra "Perform intra-procedural analyses of functions defined in the wat file [file]. The functions analyzed correspond to the sequence of arguments [funs], for example intra foo.wat 1 2 1 analyzes function 1, followed by 2, and then re-analyzes 1 (which can produce different result, if 1 depends on 2)" Relational.analyze_intra
    (fun fid summary ->
       Printf.printf "function %ld: %s" fid (Relational.Summary.to_string summary))

let reltaint_intra =
  mk_intra "Perform intra-procedural analyses of functions defined in the wat file [file]. The functions analyzed correspond to the sequence of arguments [funs], for example intra foo.wat 1 2 1 analyzes function 1, followed by 2, and then re-analyzes 1 (which can produce different result, if 1 depends on 2)" Reltaint.analyze_intra
    (fun fid summary ->
       Printf.printf "function %ld: %s, %s" fid (Relational.Summary.to_string (fst summary)) (Taint.Summary.to_string (snd summary))) *)

(** 
  [mk_inter desc analysis print] creates a command-line command for inter-procedural 
  analysis using a provided schedule (a list of list of function IDs). 
  The [analysis] is applied and results are printed with [print].
*)
let mk_inter (desc : string) (analysis : Wasm_module.t -> Int32.t list list -> 'a Int32Map.t) (print : Int32.t -> 'a -> unit) =
  Command.basic
    ~summary:desc
    Command.Let_syntax.(
      let%map_open filename = anon ("file" %: string)
      and sccs = anon (sequence ("funs" %: int32_comma_separated_list)) in
      fun () ->
        let results = analysis (Wasm_module.of_file filename) sccs in
        Int32Map.iteri results ~f:(fun ~key:id ~data: summary -> print id summary))

(** 
  Command that performs inter-procedural taint analysis according to a specified schedule 
  of function groups. Prints a summary of the results for each function.
*)
let taint_inter =
  mk_inter "Performs inter analysis of a set of functions in file [file]. [funs] is a list of comma-separated function ids, e.g., to analyze function 1, then analyze both function 2 and 3 as part of the same fixpoint computation, [funs] is 1 2,3. The full schedule for any file can be computed using the `schedule` target."
    Taint.analyze_inter
    (fun fid (_, _, summary) -> Printf.printf "function %ld: %s\n" fid (Taint.Summary.to_string summary))

(** 
  Command that detects and reports unsafe information flows from exported functions 
  to imported functions.
*)
let taint_flow_from_exported_to_imported =
  Command.basic
    ~summary:"Detects unsafe flows from exported functions to imported functions"
    Command.Let_syntax.(
      let%map_open filename = anon ("file" %: string) in
      fun () ->
        let module_ = Wasm_module.of_file filename in
        let _ = Taintcall.detect_flows_from_exported_to_imported module_ in
        ())

(** 
  Command that detects and reports unsafe flows from a user-specified list of source 
  function names to a list of sink function names.
*)
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

(** 
  Command that runs an inter-procedural taintcall analysis and outputs DOT files for the 
  CFGs of the specified functions, annotated with taint information.
*)
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

(** 
  Command that searches for [call_indirect] instructions in each function of a Wasm module 
  and prints the function index and label for each indirect call site found.
*)
let find_indirect_calls =
  Command.basic
    ~summary:"Find call_indirect instructions and shows the function in which they appear as well as their label"
    Command.Let_syntax.(
      let%map_open filename = anon ("file" %: string) in
      fun () ->
        let module_ = Wasm_module.of_file filename in
        List.iter module_.funcs ~f:(fun finst ->
            let cfg = Spec_analysis.analyze_intra1 module_ finst.idx in
            let indirect_calls = Slicing.find_call_indirect_instructions cfg in
            List.iter indirect_calls ~f:(fun label ->
                Printf.printf "function %ld, instruction %s\n" finst.idx (Instr.Label.to_string label))))
