open Core
open Wassail

let single_file : bool ref = ref false

type slicing_result = {
  function_sliced : int32;
  slicing_criterion : Instr.Label.t;
  slicing_instruction : string;
  criterion_opcode : string;
  initial_number_of_instrs : int;
  slice_size_without_pointers : int;
  slice_size : int;
  slice_size_difference : int;
  improved : bool;
  baseline_slice_ratio : float;
  vsa_slice_ratio : float;
  extra_reduction_ratio : float;
  cfg_time : Time_float.Span.t;
  spec_time : Time_float.Span.t;
  vsa_time : Time_float.Span.t;
  control_time_without_pointers : Time_float.Span.t;
  control_time : Time_float.Span.t;
  data_time_without_pointers : Time_float.Span.t;
  data_time : Time_float.Span.t;
  mem_time_without_pointers : Time_float.Span.t;
  mem_time : Time_float.Span.t;
  global_time_without_pointers : Time_float.Span.t;
  global_time : Time_float.Span.t;
  instr_to_keep_time_without_pointers : Time_float.Span.t;
  instr_to_keep_time : Time_float.Span.t;
  slicing_time_without_pointers : Time_float.Span.t;
  slicing_time : Time_float.Span.t;
  baseline_total_time : Time_float.Span.t;
  vsa_total_time : Time_float.Span.t;
  total_time_difference : Time_float.Span.t;
  total_time_ratio : float
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


let all_labels_no_blocks (instrs : 'a Instr.t list) : Instr.Label.Set.t =
  List.fold_left instrs
    ~init:Instr.Label.Set.empty
    ~f:(fun acc instr ->
        Instr.Label.Set.union acc (Instr.all_labels_no_blocks_no_merge instr))

let is_const_instruction (instr : 'a Instr.t) : bool =
  let instr_string = Instr.to_string instr in
  String.is_prefix instr_string ~prefix:"i32.const"
  || String.is_prefix instr_string ~prefix:"i64.const"
  || String.is_prefix instr_string ~prefix:"f32.const"
  || String.is_prefix instr_string ~prefix:"f64.const"


let is_unreachable (instr : 'a Instr.t) : bool =
  instr 
  |> Instr.to_string 
  |> String.is_substring ~substring:"unreachable"

let non_const_non_unreachable_labels
    (instructions_map : 'a Instr.t Instr.Label.Map.t)
    (labels : Instr.Label.t array)
  : Instr.Label.t array =
  labels
  |> Array.filter ~f:(fun label ->
      match Cfg.find_instr instructions_map label with
      | None -> false
      | Some instr -> not (is_const_instruction instr || is_unreachable instr))

let time (f : unit -> 'a) : 'a * Time_float.Span.t =
  let t0 = Time_float.now () in
  let res = f () in
  let t1 = Time_float.now () in
  (res, Time_float.diff t1 t0)

let opcode_of_instruction_string (instruction : string) : string =
  match String.split instruction ~on:' ' with
  | [] -> "<unknown>"
  | opcode :: _ -> opcode

let prefix : string ref = ref "."

let output ?(append : bool = true) (file : string) (fields : string list) =
  Out_channel.with_file (Printf.sprintf "%s/%s" !prefix file) ~append ~f:(fun ch ->
      Out_channel.output_string ch (Printf.sprintf "%s\n" (String.concat ~sep:"," fields)));
  if !single_file && String.is_suffix file ~suffix:"error.csv" then
    failwith (Printf.sprintf "Analysis of file %s resulted in an error." file)

let output_if_missing_or_empty (file : string) (fields : string list) =
  let path = Printf.sprintf "%s/%s" !prefix file in
  match Sys_unix.file_exists path with
  | `No -> output ~append:false file fields
  | `Unknown -> output ~append:false file fields
  | `Yes ->
    match Core_unix.stat path with
    | stat when Int64.(stat.st_size = 0L) -> output ~append:false file fields
    | _ -> ()

let output_slicing_result filename = function
  | Success r ->
    output (Filename.basename filename ^ ".data.csv") [filename; (* 0 *)
                                    Int32.to_string r.function_sliced; (* 1 *)
                                    Instr.Label.to_string r.slicing_criterion; (* 2 *)
                                    r.slicing_instruction; (* 3 *)
                                    r.criterion_opcode; (* 4 *)
                                    string_of_int r.initial_number_of_instrs; (* 5 *)
                                    string_of_int r.slice_size_without_pointers; (* 6 *)
                                    string_of_int r.slice_size; (* 7 *)
                                    string_of_int r.slice_size_difference; (* 8 *)
                                    string_of_bool r.improved; (* 9 *)
                                    string_of_float r.baseline_slice_ratio; (* 10 *)
                                    string_of_float r.vsa_slice_ratio; (* 11 *)
                                    string_of_float r.extra_reduction_ratio; (* 12 *)
                                    string_of_float (Time_float.Span.to_ms r.cfg_time); (* 13 *)
                                    string_of_float (Time_float.Span.to_ms r.spec_time); (* 14 *)
                                    string_of_float (Time_float.Span.to_ms r.vsa_time); (* 15 *)
                                    string_of_float (Time_float.Span.to_ms r.control_time_without_pointers); (* 16 *)
                                    string_of_float (Time_float.Span.to_ms r.control_time); (* 17 *)
                                    string_of_float (Time_float.Span.to_ms r.data_time_without_pointers); (* 18 *)
                                    string_of_float (Time_float.Span.to_ms r.data_time); (* 19 *)
                                    string_of_float (Time_float.Span.to_ms r.mem_time_without_pointers); (* 20 *)
                                    string_of_float (Time_float.Span.to_ms r.mem_time); (* 21 *)
                                    string_of_float (Time_float.Span.to_ms r.global_time_without_pointers); (* 22 *)
                                    string_of_float (Time_float.Span.to_ms r.global_time); (* 23 *)
                                    string_of_float (Time_float.Span.to_ms r.instr_to_keep_time_without_pointers); (* 24 *)
                                    string_of_float (Time_float.Span.to_ms r.instr_to_keep_time); (* 25 *)
                                    string_of_float (Time_float.Span.to_ms r.slicing_time_without_pointers); (* 26 *)
                                    string_of_float (Time_float.Span.to_ms r.slicing_time); (* 27 *)
                                    string_of_float (Time_float.Span.to_ms r.baseline_total_time); (* 28 *)
                                    string_of_float (Time_float.Span.to_ms r.vsa_total_time); (* 29 *)
                                    string_of_float (Time_float.Span.to_ms r.total_time_difference); (* 30 *)
                                    string_of_float r.total_time_ratio] (* 31 *)
  | Ignored NoFunction ->
    output "nofunction.txt" [filename]
  | Ignored (NoInstruction f) ->
    output "noinstruction.txt" [filename; Int32.to_string f]
  | SliceExtensionError (f, criterion, size, reason) ->
    output "error.csv" [filename; (* 0 *)
                        Int32.to_string f; (* 1 *)
                        Instr.Label.to_string criterion; (* 2 *)
                        string_of_int size; (* 3 *)
                        "slice-extension"; (* 4 *)
                        reason] (* 5 *)
  | SliceError (f, criterion, size, reason) ->
    output "error.csv" [filename; (* 0 *)
                        Int32.to_string f; (* 1 *)
                        Instr.Label.to_string criterion; (* 2 *)
                        string_of_int size; (* 3 *)
                        "slice"; (* 4 *)
                        reason] (* 5 *)
  | CfgError (f, size, reason) ->
    output "error.csv" [filename; (* 0 *)
                        Int32.to_string f; (* 1 *)
                        "-1"; (* 2 *)
                        string_of_int size; (* 3 *)
                        "cfg"; (* 4 *)
                        reason] (* 5 *)
  | LoadError _ ->
    output "loaderror.txt" [filename]


let slices ~(fid : int option) (filename : string) (criterion_selection : [`Random of int | `All | `Last ]) : unit =
  try
    Spec_inference.propagate_globals := false;
    Spec_inference.propagate_locals := false;
    Spec_inference.use_const := false;
    let module_ = Wasm_module.of_file filename in
    let funcs = module_.funcs in
    if List.is_empty funcs then
      output_slicing_result filename (Ignored NoFunction)
    else
      let funcs =
        match fid with
        | None ->
          (let sample_size = (float_of_int (List.length funcs) *. 0.96) 
                              /. ((0.01 *. (float_of_int (List.length funcs - 1))) +. 1.92)
                              |> int_of_float in
            if sample_size < 30 then
              funcs
            else
              let funcs_array = funcs |> Array.of_list in
              (for i = 0 to sample_size - 1 do
                let tmp = funcs_array.(i) in
                let j = i + Random.int (Array.length funcs_array - i) in
                funcs_array.(i) <- funcs_array.(j);
                funcs_array.(j) <- tmp;
              done;
              Array.sub funcs_array ~pos:0 ~len:sample_size |> Array.to_list))
        | Some fid -> funcs |> List.filter ~f:(fun f -> Int32.to_int_exn f.idx = fid)
      in
      List.iter funcs ~f:(fun func ->
        let labels = func.code.body |> all_labels_no_blocks |> Instr.Label.Set.to_array in
        try
          let t0 = Time_float.now () in
          let cfg_raw = Cfg_builder.build module_ func.idx in
          let cfg_time = Time_float.diff (Time_float.now ()) t0 in
          let t0 = Time_float.now () in
          let cfg = Spec_inference.Intra.analyze module_ cfg_raw () in
          let spec_time = Time_float.diff (Time_float.now ()) t0 in
          let cfg_instructions = Cfg.all_instructions cfg in
          let labels_without_constants_no_unreachable = non_const_non_unreachable_labels cfg_instructions labels in
          if Array.length labels_without_constants_no_unreachable = 0 then
            output_slicing_result filename (Ignored (NoInstruction func.idx))
          else
            let t0 = Time_float.now () in
            let pointer_analysis = Some (Value_set.run_pointer_analysis module_ cfg_raw func.idx) in
            let vsa_time = Time_float.diff (Time_float.now ()) t0 in
            let preanalysis_without_pointers = Slicing.preanalysis module_ cfg cfg_instructions None in
            let preanalysis = Slicing.preanalysis module_ cfg cfg_instructions pointer_analysis in
            List.iter 
              (match criterion_selection with
                | `Random n -> 
                  if n >= (Array.length labels_without_constants_no_unreachable) then 
                    Array.to_list labels_without_constants_no_unreachable 
                  else
                    (for i = 0 to n - 1 do
                      let tmp = labels_without_constants_no_unreachable.(i) in
                      let j = i + Random.int (Array.length labels_without_constants_no_unreachable - i) in
                      labels_without_constants_no_unreachable.(i) <- labels_without_constants_no_unreachable.(j);
                      labels_without_constants_no_unreachable.(j) <- tmp;
                    done;
                    Array.sub labels_without_constants_no_unreachable ~pos:0 ~len:n |> Array.to_list)
                | `All -> Array.to_list labels_without_constants_no_unreachable
                | `Last -> [Array.last labels_without_constants_no_unreachable])
              ~f:(fun slicing_criterion ->
                  try
                    let instrs_to_keep_without_pointers, (control_time_without_pointers, data_time_without_pointers, mem_time_without_pointers, global_time_without_pointers, instr_to_keep_time_without_pointers) = 
                      Slicing.instructions_to_keep 
                        module_ 
                        cfg 
                        cfg_instructions 
                        preanalysis_without_pointers 
                        (Instr.Label.Set.singleton slicing_criterion) in
                    try
                      let instrs_to_keep, (control_time, data_time, mem_time, global_time, instr_to_keep_time) = 
                        Slicing.instructions_to_keep 
                          module_ 
                          cfg 
                          cfg_instructions 
                          preanalysis
                          (Instr.Label.Set.singleton slicing_criterion) in
                      try
                        let t0 = Time_float.now () in
                        let sliced_func_without_pointers = Slicing.slice_to_funcinst module_ cfg ~instrs:(Some instrs_to_keep_without_pointers) cfg_instructions (Instr.Label.Set.singleton slicing_criterion) None in
                        let t1 = Time_float.now () in
                        let sliced_func = Slicing.slice_to_funcinst module_ cfg ~instrs:(Some instrs_to_keep) cfg_instructions (Instr.Label.Set.singleton slicing_criterion) None in
                        let t2 = Time_float.now () in
                        let slicing_time_without_pointers = Time_float.diff t1 t0 in
                        let slicing_time = Time_float.diff t2 t1 in
                        let sliced_labels_without_pointers = all_labels sliced_func_without_pointers.code.body in
                        let sliced_labels = all_labels sliced_func.code.body in
                        (* Printf.printf "fun %ld:%s -- initial: %s, before: %s, after: %d\n" func.idx (Instr.Label.to_string slicing_criterion) (Instr.Label.Set.to_string (all_labels func.code.body)) (Instr.Label.Set.to_string instrs_to_keep) (Instr.Label.Set.length sliced_labels); *)
                        let slice_size_without_pointers = Instr.Label.Set.length sliced_labels_without_pointers in
                        let slice_size = Instr.Label.Set.length sliced_labels in
                        let slice_size_difference = slice_size_without_pointers - slice_size in
                        let improved = slice_size_difference > 0 in
                        let baseline_slice_ratio = float_of_int slice_size_without_pointers /. float_of_int (Array.length labels) in
                        let vsa_slice_ratio = float_of_int slice_size /. float_of_int (Array.length labels) in
                        let extra_reduction_ratio = float_of_int slice_size_difference /. (float_of_int slice_size_without_pointers) in
                        let baseline_total_time =
                          cfg_time
                            |> Time_float.Span.(+) spec_time
                            |> Time_float.Span.(+) control_time_without_pointers
                            |> Time_float.Span.(+) data_time_without_pointers
                            |> Time_float.Span.(+) mem_time_without_pointers
                            |> Time_float.Span.(+) global_time_without_pointers
                            |> Time_float.Span.(+) instr_to_keep_time_without_pointers
                            |> Time_float.Span.(+) slicing_time_without_pointers
                        in
                        let vsa_total_time =
                          cfg_time
                            |> Time_float.Span.(+) spec_time
                            |> Time_float.Span.(+) vsa_time
                            |> Time_float.Span.(+) control_time
                            |> Time_float.Span.(+) data_time 
                            |> Time_float.Span.(+) mem_time
                            |> Time_float.Span.(+) global_time
                            |> Time_float.Span.(+) instr_to_keep_time
                            |> Time_float.Span.(+) slicing_time
                        in
                        let total_time_difference =
                          Time_float.Span.(vsa_total_time - baseline_total_time)
                        in
                        let total_time_ratio =
                          Time_float.Span.to_ms vsa_total_time /. Time_float.Span.to_ms baseline_total_time
                        in
                        let slicing_instruction =
                          match Cfg.find_instr cfg_instructions slicing_criterion with
                          | None -> "<instruction not found>"
                          | Some instr -> Instr.to_string instr
                        in
                        let criterion_opcode = opcode_of_instruction_string slicing_instruction in
                        output_slicing_result filename (Success {
                            function_sliced = func.idx;
                            slicing_criterion;
                            slicing_instruction;
                            criterion_opcode;
                            initial_number_of_instrs = Array.length labels;
                            slice_size_without_pointers;
                            slice_size;
                            slice_size_difference;
                            improved;
                            baseline_slice_ratio;
                            vsa_slice_ratio;
                            extra_reduction_ratio;
                            cfg_time; 
                            spec_time;
                            vsa_time;
                            control_time_without_pointers;
                            control_time;
                            data_time_without_pointers;
                            data_time;
                            mem_time_without_pointers;
                            mem_time;
                            global_time_without_pointers;
                            global_time;
                            instr_to_keep_time_without_pointers;
                            instr_to_keep_time;
                            slicing_time_without_pointers;
                            slicing_time;
                            baseline_total_time;
                            vsa_total_time;
                            total_time_difference;
                            total_time_ratio;
                          })
                      with e ->
                        output_slicing_result filename (SliceExtensionError (func.idx, slicing_criterion, Array.length labels_without_constants_no_unreachable, Exn.to_string_mach e))
                    with e ->
                      output_slicing_result filename 
                        (SliceError (func.idx, slicing_criterion, Array.length labels_without_constants_no_unreachable, "Exception raised when calculating instructions to keep WITH pointer analysis: " ^ Exn.to_string_mach e))
                  with e ->
                    output_slicing_result filename 
                      (SliceError (func.idx, slicing_criterion, Array.length labels_without_constants_no_unreachable, "Exception raised when calculating instructions to keep WITHOUT pointer analysis: " ^ Exn.to_string_mach e)))
        with e -> output_slicing_result filename (CfgError (func.idx, Array.length labels, Exn.to_string_mach e)))
  with e -> output_slicing_result filename (LoadError (Exn.to_string e))

let evaluate_file ~(fid : int option) (filename : string) (criterion_selection : [`All | `Random of int | `Last]) : unit =
  slices ~fid filename criterion_selection



let rec wat_files_in_directory (dirname : string) : string list =
  match Sys_unix.is_directory dirname with
  | `Yes ->
    Sys_unix.ls_dir dirname
    |> List.concat_map ~f:(fun file ->
        let path = Filename.concat dirname file in
        match Sys_unix.is_directory path with
        | `Yes -> wat_files_in_directory path
        | `No ->
          if Filename.check_suffix file ".wat" || Filename.check_suffix file ".wasm" then
            [path]
          else
            []
        | `Unknown -> [])
    |> List.sort ~compare:String.compare
  | `No -> failwith (Printf.sprintf "%s is not a directory" dirname)
  | `Unknown -> failwith (Printf.sprintf "cannot access directory %s" dirname)

let set_output_directory (input : string) (output_folder : string) : unit =
  let current_dir =
    match Sys_unix.is_directory input with
    | `Yes -> if String.is_suffix input ~suffix:"/" then input else input ^ "/"
    | _ -> Filename.dirname input ^ "/"
  in
  match Sys_unix.is_directory (current_dir ^ output_folder) with
  | `Yes -> prefix := current_dir ^ output_folder
  | `No -> 
    begin match Sys_unix.file_exists (current_dir ^ output_folder) with
    | `Yes | `Unknown -> failwith (Printf.sprintf "%s exists but is not a directory" (current_dir ^ output_folder))
    | _ -> prefix := current_dir ^ output_folder; Core_unix.mkdir !prefix; 
    end
  | `Unknown -> prefix := current_dir ^ output_folder; Core_unix.mkdir !prefix


let initialize_output_file (filename : string) : unit =
  output_if_missing_or_empty filename ["file name"; (* 0 *)
                                        "function"; (* 1 *)
                                        "slicing criterion"; (* 2 *)
                                        "criterion instruction"; (* 3 *)
                                        "criterion opcode"; (* 4 *)
                                        "initial number of instrs"; (* 5 *)
                                        "slice size without pointers"; (* 6 *)
                                        "slice size"; (* 7 *)
                                        "slice size difference"; (* 8 *)
                                        "improved"; (* 9 *)
                                        "baseline slice ratio"; (* 10 *)
                                        "vsa slice ratio"; (* 11 *)
                                        "extra_reduction_ratio"; (* 12 *)
                                        "cfg_time (ms)"; (* 13 *)
                                        "spec_time (ms)"; (* 14 *)
                                        "vsa_time (ms)"; (* 15 *)
                                        "control_time_without_pointers (ms)"; (* 16 *)
                                        "control_time (ms)"; (* 17 *)
                                        "data_time_without_pointers (ms)"; (* 18 *)
                                        "data_time (ms)"; (* 19 *)
                                        "mem_time_without_pointers (ms)"; (* 20 *)
                                        "mem_time (ms)"; (* 21 *)
                                        "global_time_without_pointers (ms)"; (* 22 *)
                                        "global_time (ms)"; (* 23 *)
                                        "instr_to_keep_time_without_pointers (ms)"; (* 24 *)
                                        "instr_to_keep_time (ms)"; (* 25 *)
                                        "slicing_time_without_pointers (ms)"; (* 26 *)
                                        "slicing_time (ms)"; (* 27 *)
                                        "baseline_total_time (ms)"; (* 28 *)
                                        "vsa_total_time (ms)"; (* 29 *)
                                        "total_time_difference (ms)"; (* 30 *)
                                        "total_time_ratio"] (* 31 *)


let evaluate =
  Command.basic
    ~summary:"Evaluate slicer on a .wat/.wasm file, or all .wat/.wasm files in a directory"
    Command.Let_syntax.(
      let%map_open filename = anon ("path" %: string)
      and func = flag "-f" (optional int) ~doc:"index of a specific function to analyze (default: all)"
      and prefix_opt = flag "-p" (optional string) ~doc:"prefix for where to save the results file (default: current directory)"
      and all = flag "-a" no_arg ~doc:"slice on all functions, for all slicing criteria"
      and last = flag "-l" no_arg ~doc:"slice on all functions, for the last slicing criterion" 
      and random = flag "-r" (optional int) ~doc:"N slice on a statistically significant subset of functions, for N random slicing criteria (default: 1 criterion)"
      and seed = flag "-seed" (optional int) ~doc:"N initialize the random number generator with seed N"
      in
      fun () ->
        begin match seed with
        | Some s -> Random.init s
        | None -> Random.init 14
        end;
        begin match prefix_opt with
        | None -> set_output_directory filename ""
        | Some p -> set_output_directory filename p;
        end;
        let criterion_selection = 
          match random with
            | Some n -> `Random n
            | None -> if all then `All else if last then `Last else `Random 1 in
        if String.is_suffix filename ~suffix:".wat" || String.is_suffix filename ~suffix:".wasm" then
          (* analyzing a single file *)
          (single_file := true;
          initialize_output_file (Filename.basename filename ^ ".data.csv");
          evaluate_file ~fid:func filename criterion_selection)
        else
          (* analyzing a folder *)
          (single_file := false;
          wat_files_in_directory filename
          |> List.iter ~f:(fun file ->
              initialize_output_file (Filename.basename file ^ ".data.csv");
              Printf.printf "Processing %s\n%!" file;
              evaluate_file ~fid:None file criterion_selection))
    )