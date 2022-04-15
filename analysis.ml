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


let spec_inference =
  mk_intra "Annotate the CFG with the inferred variables"
    (Analysis_helpers.mk_intra (fun _ _ -> Int32Map.empty) (fun _ _ annotated_cfg -> annotated_cfg))
    (fun fid annotated_cfg ->
       let file_out = Printf.sprintf "%ld.dot" fid in
       Out_channel.with_file file_out
         ~f:(fun ch ->
             Out_channel.output_string ch (Cfg.to_dot annotated_cfg ~annot_str:Spec.to_dot_string)))

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


let relational_intra =
  mk_intra "Perform intra-procedural analyses of functions defined in the wat file [file]. The functions analyzed correspond to the sequence of arguments [funs], for example intra foo.wat 1 2 1 analyzes function 1, followed by 2, and then re-analyzes 1 (which can produce different result, if 1 depends on 2)" Relational.analyze_intra
    (fun fid summary ->
       Printf.printf "function %ld: %s" fid (Relational.Summary.to_string summary))

let reltaint_intra =
  mk_intra "Perform intra-procedural analyses of functions defined in the wat file [file]. The functions analyzed correspond to the sequence of arguments [funs], for example intra foo.wat 1 2 1 analyzes function 1, followed by 2, and then re-analyzes 1 (which can produce different result, if 1 depends on 2)" Reltaint.analyze_intra
    (fun fid summary ->
       Printf.printf "function %ld: %s, %s" fid (Relational.Summary.to_string (fst summary)) (Taint.Summary.to_string (snd summary)))

let mk_inter (desc : string) (analysis : Wasm_module.t -> Int32.t list list -> 'a Int32Map.t) (print : Int32.t -> 'a -> unit) =
  Command.basic
    ~summary:desc
    Command.Let_syntax.(
      let%map_open filename = anon ("file" %: string)
      and sccs = anon (sequence ("funs" %: int32_comma_separated_list)) in
      fun () ->
        let results = analysis (Wasm_module.of_file filename) sccs in
        Int32Map.iteri results ~f:(fun ~key:id ~data: summary -> print id summary))

let taint_inter =
  mk_inter "Performs inter analysis of a set of functions in file [file]. [funs] is a list of comma-separated function ids, e.g., to analyze function 1, then analyze both function 2 and 3 as part of the same fixpoint computation, [funs] is 1 2,3. The full schedule for any file can be computed using the `schedule` target."
    Taint.analyze_inter
    (fun fid (_, _, summary) -> Printf.printf "function %ld: %s\n" fid (Taint.Summary.to_string summary))

let taint_to_sinks =
  Command.basic
    ~summary:"Detects unsafe flows from exported function arguments to list of defined sinks"
    Command.Let_syntax.(
      let%map_open filename = anon ("file" %: string)
      and sink_names = anon (sequence ("funs" %: string)) in
      fun () ->
        let module_ = Wasm_module.of_file filename in
        let sink_indices = Taint.find_sinks_from_names module_ (String.Set.of_list sink_names) in
        let _ = Taintcall.detect_unsafe_calls_from_exported_to_sinks module_ sink_indices in
        ())

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
