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
  | SliceExtensionError of int32 * Instr.Label.t * int * string
  | SliceError of int32 * Instr.Label.t * int * string
  | CfgError of int32 * int * string
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
    Printf.printf "loading...\n%!";
    let wasm_mod = Wasm_module.of_file filename in
    let funcs = wasm_mod.funcs in
    if List.is_empty funcs then
      [Ignored NoFunction]
    else
      let first_idx = (List.hd_exn funcs).idx in
      let nfuncs = List.length funcs in
      List.concat_map funcs ~f:(fun func ->
          let labels = Instr.Label.Set.to_array (all_labels func.code.body) in
          if Array.length labels = 0 then
            [Ignored (NoInstruction func.idx)]
          else
            try
              Printf.printf "[%s fun %ld/%ld] building CFG...\n%!" filename func.idx Int32.(first_idx + (of_int_exn nfuncs));
              let (cfg, preanalysis_time) = time (fun () -> Cfg.without_empty_nodes_with_no_predecessors (Spec_analysis.analyze_intra1 wasm_mod func.idx)) in
              Printf.printf "getting instructions...\n%!";
              let cfg_instructions = Cfg.all_instructions cfg in
              List.map (match criterion_selection with
                  | `Random -> [Array.get labels (Random.int (Array.length labels))]
                  | `All -> Array.to_list labels
                  | `Last -> [Array.last labels])
                ~f:(fun slicing_criterion ->
                    let t0 = Time.now () in
                    try
                      Printf.printf "slicing...\n%!";
                      let instrs_to_keep = Slicing.instructions_to_keep cfg cfg_instructions slicing_criterion in
                      try
                        Printf.printf "reconstructing...\n%!";
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
                      with e -> SliceExtensionError (func.idx, slicing_criterion, Array.length labels, Exn.to_string_mach e)
                    with e -> SliceError (func.idx, slicing_criterion, Array.length labels, Exn.to_string_mach e))
            with e -> [CfgError (func.idx, Array.length labels, Exn.to_string_mach e)])
  with e -> [LoadError (Exn.to_string e)]


let prefix : string ref = ref "."

let output (file : string) (fields : string list) =
  Out_channel.with_file (Printf.sprintf "%s/%s" !prefix file) ~append:true ~f:(fun ch ->
      Out_channel.output_string ch (Printf.sprintf "%s\n" (String.concat ~sep:"," fields)))

let evaluate_files (files : string list) (criterion_selection : [`All | `Random | `Last]) : unit =
  let total = List.length files in
  List.iteri files ~f:(fun i filename ->
      Printf.printf "\r%d/%d %s\n" i total filename;
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
      | SliceExtensionError (f, criterion, size, reason) ->
        output "error.txt" [filename; (* 0 *)
                            Int32.to_string f; (* 1 *)
                            Instr.Label.to_string criterion; (* 2 *)
                            string_of_int size; (* 3 *)
                            "slice-extension"; (* 4 *)
                            reason] (* 5 *)
      | SliceError (f, criterion, size, reason) ->
        output "error.txt" [filename; (* 0 *)
                            Int32.to_string f; (* 1 *)
                            Instr.Label.to_string criterion; (* 2 *)
                            string_of_int size; (* 3 *)
                            "slice"; (* 4 *)
                            reason] (* 5 *)
      | CfgError (f, size, reason) ->
        output "error.txt" [filename; (* 0 *)
                            Int32.to_string f; (* 1 *)
                            "-1"; (* 2 *)
                            string_of_int size; (* 3 *)
                            "cfg"; (* 4 *)
                            reason] (* 5 *)
      | LoadError _ ->
        output "loaderror.txt" [filename]))

let evaluate =
  Command.basic
    ~summary:"Evaluate the slicer on a set of benchmarks, listed in the file given as argument"
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


(* Generate a slice for printf function calls with a specific string *)
let generate_slice (filename : string) (output_file : string) =
  let pattern = "\norbs:" in
  Spec_inference.propagate_globals := false;
  Spec_inference.propagate_locals := false;
  Spec_inference.use_const := false;
  let wasm_mod = Wasm_module.of_file filename in
  (* Find the specific string in the data section *)
  let str_pos = match List.find_map wasm_mod.data ~f:(fun segment ->
      match String.substr_index segment.init ~pattern with
      | Some idx -> begin match segment.offset with
        | (Instr.Data {instr = Instr.Const (Prim_value.I32 n); _}) :: [] -> Some (Int32.(n + (of_int_exn idx)))
        | _ -> failwith "unsupported segment offset"
        end
      | None -> None) with
  | Some idx -> idx
  | None -> failwith "cannot find pattern in any data segment" in
  Printf.printf "data segment: %ld\n" str_pos;
  (* Find the index of the printf function (it should be exported, otherwise it is unnamed) *)
  let printf_export_idx = match List.find_map wasm_mod.exported_funcs ~f:(fun (idx, name, _type) ->
      if String.equal name "printf" then
        Some idx
      else
        None) with
  | Some idx -> idx
  | None -> failwith "cannot find printf in exported functions" in
  Spec_inference.propagate_globals := false;
  Spec_inference.propagate_locals := false;
  Printf.printf "printf id: %ld\n" printf_export_idx;
  (* Find the function and slicing criterion: it should call printf with the string's position in the data segment as argument *)
  let (function_idx, slicing_criterion) = match List.find_map wasm_mod.funcs ~f:(fun func ->
      Spec_inference.use_const := true;
      let cfg = Cfg.without_empty_nodes_with_no_predecessors (Spec_analysis.analyze_intra1 wasm_mod func.idx) in
      List.find_map (Cfg.all_instructions_list cfg) ~f:(function
          | Instr.Control ({instr = Call (_, _, idx); annotation_before; label; _ }) when Int32.(idx = printf_export_idx) ->
            begin match annotation_before.vstack with
              | _ :: first_arg :: _ when Var.equal first_arg (Var.Const (Prim_value.I32 str_pos)) -> Some (func.idx, label)
              | _ -> None
            end
          | _ -> None)) with
  | Some r -> r
  | None -> failwith "cannot find function to slice" in
  Printf.printf "function: %ld, slicing criterion: %s\n" function_idx (Instr.Label.to_string slicing_criterion);
  (* Perform the actual slicing *)
  Spec_inference.use_const := false;
  let cfg = Cfg.without_empty_nodes_with_no_predecessors (Spec_analysis.analyze_intra1 wasm_mod function_idx) in
  let funcinst = Slicing.slice_alternative_to_funcinst cfg (Cfg.all_instructions cfg) slicing_criterion in
  let module_ = Wasm_module.replace_func wasm_mod function_idx funcinst in
  Out_channel.with_file output_file
    ~f:(fun ch -> Out_channel.output_string ch (Wasm_module.to_string module_))

let gen_slice_specific =
  Command.basic
    ~summary:"Generate a slice for a specific slicing criterion"
    Command.Let_syntax.(
      let%map_open file = anon ("file" %: string)
      and output = anon ("output" %: string) in
      fun () ->
        generate_slice file output)
          
    
