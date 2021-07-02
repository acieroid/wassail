open Core
open Wassail

type slicing_result = {
  function_sliced: int32;
  slicing_criterion: Instr.Label.t;
  initial_number_of_instrs : int;
  slice_size_before_adaptation : int;
  slice_size_after_adaptation : int;
  preanalysis_time : Time.Span.t;
  slicing_time : Time.Span.t;
}

type ignored_reason =
  | NoFunction
  | NoInstruction of int32

type result =
  | Success of slicing_result
  | Ignored of ignored_reason
  | SliceExtensionError of int32 * Instr.Label.t * string
  | SliceError of int32 * Instr.Label.t * string
  | CfgError of int32 * string
  | LoadError of string

let all_labels (instrs : 'a Instr.t list) : Instr.Label.Set.t =
  List.fold_left instrs
    ~init:Instr.Label.Set.empty
    ~f:(fun acc instr ->
        Instr.Label.Set.union acc (Instr.all_labels_no_merge instr))

let time (f : unit -> 'a) : 'a * Time.Span.t =
  let t0 = Time.now () in
  let res = f () in
  let t1 = Time.now () in
  (res, Time.diff t1 t0)

let slices (filename : string) (criterion_selection : [`Random | `All | `Last ]) : result list =
  try
    Spec_inference.propagate_globals := false;
    Spec_inference.propagate_locals := false;
    Spec_inference.use_const := false;
    let wasm_mod = Wasm_module.of_file filename in
    let funcs = wasm_mod.funcs in
    if List.is_empty funcs then
      [Ignored NoFunction]
    else
      List.concat_map funcs ~f:(fun func ->
          let labels = Instr.Label.Set.to_array (all_labels func.code.body) in
          if Array.length labels = 0 then
            [Ignored (NoInstruction func.idx)]
          else
            try
              let (cfg, preanalysis_time) = time (fun () -> Cfg.without_empty_nodes_with_no_predecessors (Spec_analysis.analyze_intra1 wasm_mod func.idx)) in
              let cfg_instructions = Cfg.all_instructions cfg in
              List.map (match criterion_selection with
                  | `Random -> [Array.get labels (Random.int (Array.length labels))]
                  | `All -> Array.to_list labels
                  | `Last -> [Array.last labels])
                ~f:(fun slicing_criterion ->
                    let t0 = Time.now () in
                    try
                      let instrs_to_keep = Slicing.instructions_to_keep cfg cfg_instructions slicing_criterion in
                      try
                        let sliced_func = Slicing.slice_alternative_to_funcinst cfg ~instrs:(Some instrs_to_keep) cfg_instructions slicing_criterion in
                        let t1 = Time.now () in
                        let slicing_time = Time.diff t1 t0 in
                        let sliced_labels = all_labels sliced_func.code.body in
                        (* Printf.printf "fun %ld:%s -- initial: %s, before: %s, after: %d\n" func.idx (Instr.Label.to_string slicing_criterion) (Instr.Label.Set.to_string (all_labels func.code.body)) (Instr.Label.Set.to_string instrs_to_keep) (Instr.Label.Set.length sliced_labels); *)

                        Success {
                          function_sliced = func.idx;
                          slicing_criterion;
                          initial_number_of_instrs = Array.length labels;
                          slice_size_before_adaptation = Instr.Label.Set.length instrs_to_keep;
                          slice_size_after_adaptation = Instr.Label.Set.length sliced_labels;
                          preanalysis_time;
                          slicing_time;
                        }
                      with e -> SliceExtensionError (func.idx, slicing_criterion, Exn.to_string e)
                    with e -> SliceError (func.idx, slicing_criterion, Exn.to_string e))
            with e -> [CfgError (func.idx, Exn.to_string e)])
  with e -> [LoadError (Exn.to_string e)]


let prefix : string ref = ref "."

let output (file : string) (fields : string list) =
  Out_channel.with_file (Printf.sprintf "%s/%s" !prefix file) ~append:true ~f:(fun ch ->
      Out_channel.output_string ch (Printf.sprintf "%s\n" (String.concat ~sep:"," fields)))

let evaluate_files (files : string list) (criterion_selection : [`All | `Random | `Last]) : unit =
  let total = List.length files in
  List.iteri files ~f:(fun i filename ->
      Printf.printf "\r%d/%d %s%!" i total filename;
      List.iter (slices filename criterion_selection) ~f:(function
          | Success r ->
            output "data.txt" [filename; (* 0 *)
                               Int32.to_string r.function_sliced; (* 1 *)
                               Instr.Label.to_string r.slicing_criterion; (* 2 *)
                               string_of_int r.initial_number_of_instrs; (* 3 *)
                               string_of_int r.slice_size_before_adaptation; (* 4 *)
                               string_of_int r.slice_size_after_adaptation; (* 5 *)
                               string_of_float (Time.Span.to_ms r.preanalysis_time); (* 6 *)
                               string_of_float (Time.Span.to_ms r.slicing_time)] (* 7 *)
      | Ignored NoFunction ->
        output "nofunction.txt" [filename]
      | Ignored (NoInstruction f) ->
        output "noinstruction.txt" [filename; Int32.to_string f]
      | SliceExtensionError (f, criterion, reason) ->
        output "error.txt" [filename; Int32.to_string f; Instr.Label.to_string criterion;
                            "slice-extension"; reason]
      | SliceError (f, criterion, reason) ->
        output "error.txt" [filename; Int32.to_string f; Instr.Label.to_string criterion;
                            "slice"; reason]
      | CfgError (f, reason) ->
        output "error.txt" [filename; Int32.to_string f; "-1";
                            "cfg"; reason]
      | LoadError _ ->
        output "loaderror.txt" [filename]))

let evaluate =
  Command.basic
    ~summary:"Evalate the slicer on a set of benchmarks, listed in the file given as argument"
    Command.Let_syntax.(
      let%map_open filelist = anon ("filelist" %: string)
      and prefix_opt = flag "-p" (optional string) ~doc:"prefix for where to save the results file (default: current directory)"
      and all = flag "-a" no_arg ~doc:"slice on all functions, for all slicing criterion"
      and last = flag "-l" no_arg ~doc:"slice on all functions, for the last slicing criterion" in
      fun () ->
        begin match prefix_opt with
        | None -> ()
        | Some p -> prefix := p
        end;
        evaluate_files (In_channel.read_lines filelist) (if all then `All else if last then `Last else `Random))

