open Core
open Wassail

type slicing_result = {
  function_sliced : int32;
  slicing_criterion : Instr.Label.t;
  initial_number_of_instrs : int;
  slice_size_without_pointers : int;
  slice_size : int;
  slice_size_difference : int;
  reduction_factor : float;
  cfg_time : Time_float.Span.t;
  spec_time : Time_float.Span.t;
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
  total_time_difference : Time_float.Span.t
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

let time (f : unit -> 'a) : 'a * Time_float.Span.t =
  let t0 = Time_float.now () in
  let res = f () in
  let t1 = Time_float.now () in
  (res, Time_float.diff t1 t0)

let prefix : string ref = ref "."

let output ?(append : bool = true) (file : string) (fields : string list) =
  Out_channel.with_file (Printf.sprintf "%s/%s" !prefix file) ~append ~f:(fun ch ->
      Out_channel.output_string ch (Printf.sprintf "%s\n" (String.concat ~sep:"," fields)))

let output_slicing_result filename = function
  | Success r ->
    output "data.csv" [filename; (* 0 *)
                       Int32.to_string r.function_sliced; (* 1 *)
                       Instr.Label.to_string r.slicing_criterion; (* 2 *)
                       string_of_int r.initial_number_of_instrs; (* 3 *)
                       string_of_int r.slice_size_without_pointers; (* 4 *)
                       string_of_int r.slice_size; (* 5 *)
                       string_of_int r.slice_size_difference; (* 6 *)
                       string_of_float r.reduction_factor; (* 7 *)
                       string_of_float (Time_float.Span.to_ms r.cfg_time); (* 8 *)
                       string_of_float (Time_float.Span.to_ms r.spec_time); (* 9 *)
                       string_of_float (Time_float.Span.to_ms r.control_time_without_pointers); (* 10 *)
                       string_of_float (Time_float.Span.to_ms r.control_time); (* 11 *)
                       string_of_float (Time_float.Span.to_ms r.data_time_without_pointers); (* 12 *)
                       string_of_float (Time_float.Span.to_ms r.data_time); (* 13 *)
                       string_of_float (Time_float.Span.to_ms r.mem_time_without_pointers); (* 14 *)
                       string_of_float (Time_float.Span.to_ms r.mem_time); (* 15 *)
                       string_of_float (Time_float.Span.to_ms r.global_time_without_pointers); (* 16 *)
                       string_of_float (Time_float.Span.to_ms r.global_time); (* 17 *)
                       string_of_float (Time_float.Span.to_ms r.instr_to_keep_time_without_pointers); (* 18 *)
                       string_of_float (Time_float.Span.to_ms r.instr_to_keep_time); (* 19 *)
                       string_of_float (Time_float.Span.to_ms r.slicing_time_without_pointers); (* 20 *)
                       string_of_float (Time_float.Span.to_ms r.slicing_time);
                       string_of_float (Time_float.Span.to_ms r.total_time_difference)] (* 21 *)
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


let slices (filename : string) (criterion_selection : [`Random of int | `All | `Last ]) : unit =
  try
    Spec_inference.propagate_globals := false;
    Spec_inference.propagate_locals := false;
    Spec_inference.use_const := false;
    let module_ = Wasm_module.of_file filename in
    let funcs = module_.funcs in
    if List.is_empty funcs then
      output_slicing_result filename (Ignored NoFunction)
    else
      List.iteri funcs ~f:(fun _i func ->
        let labels = func.code.body 
          |> all_labels_no_blocks
          |> Instr.Label.Set.to_array
          (* Instr.Label.Set.to_array (all_labels func.code.body) *)
        in
        if Array.length labels = 0 then
          output_slicing_result filename (Ignored (NoInstruction func.idx))
        else
          try
            let t0 = Time_float.now () in
            let cfg_raw = Cfg_builder.build module_ func.idx in
            let cfg_time = Time_float.diff (Time_float.now ()) t0 in
            let t0 = Time_float.now () in
            let cfg = Spec_inference.Intra.analyze module_ cfg_raw () in
            let spec_time = Time_float.diff (Time_float.now ()) t0 in
            let cfg_instructions = Cfg.all_instructions cfg in
            let pointer_analysis = Some (Value_set.run_pointer_analysis module_ cfg_raw func.idx) in
            let preanalysis_without_pointers = Slicing.preanalysis module_ cfg cfg_instructions None in
            let preanalysis = Slicing.preanalysis module_ cfg cfg_instructions pointer_analysis in
            List.iter 
              (match criterion_selection with
                | `Random n -> 
                  if n >= (Array.length labels) then Array.to_list labels else
                    (for i = 0 to n - 1 do
                      let tmp = labels.(i) in
                      let j = i + Random.int (Array.length labels - i) in
                      labels.(i) <- labels.(j);
                      labels.(j) <- tmp;
                    done;
                    Array.sub labels ~pos:0 ~len:n |> Array.to_list)
                | `All -> Array.to_list labels
                | `Last -> [Array.last labels])
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
                        let reduction_factor = float_of_int slice_size_difference /. (float_of_int slice_size_without_pointers) in
                        let total_time_difference =
                          control_time 
                            |> Time_float.Span.(+) data_time 
                            |> Time_float.Span.(+) mem_time
                            |> Time_float.Span.(+) global_time
                            |> Time_float.Span.(+) instr_to_keep_time
                            |> Time_float.Span.(+) slicing_time
                            |> Time_float.Span.(-) control_time_without_pointers
                            |> Time_float.Span.(-) data_time_without_pointers
                            |> Time_float.Span.(-) mem_time_without_pointers
                            |> Time_float.Span.(-) global_time_without_pointers
                            |> Time_float.Span.(-) instr_to_keep_time_without_pointers
                            |> Time_float.Span.(-) slicing_time_without_pointers
                        in
                        output_slicing_result filename (Success {
                            function_sliced = func.idx;
                            slicing_criterion;
                            initial_number_of_instrs = Array.length labels;
                            slice_size_without_pointers;
                            slice_size;
                            slice_size_difference;
                            reduction_factor;
                            cfg_time; 
                            spec_time;
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
                            total_time_difference;
                          })
                      with e ->
                        output_slicing_result filename (SliceExtensionError (func.idx, slicing_criterion, Array.length labels, Exn.to_string_mach e))
                    with e ->
                      output_slicing_result filename 
                        (SliceError (func.idx, slicing_criterion, Array.length labels, "Exception raised when calculating instructions to keep WITH pointer analysis: " ^ Exn.to_string_mach e))
                  with e ->
                    output_slicing_result filename 
                      (SliceError (func.idx, slicing_criterion, Array.length labels, "Exception raised when calculating instructions to keep WITHOUT pointer analysis: " ^ Exn.to_string_mach e)))
          with e -> output_slicing_result filename (CfgError (func.idx, Array.length labels, Exn.to_string_mach e)))
  with e -> output_slicing_result filename (LoadError (Exn.to_string e))

let evaluate_file (filename : string) (criterion_selection : [`All | `Random of int | `Last]) : unit =
  slices filename criterion_selection



let wat_files_in_directory (dirname : string) : string list =
  match Sys_unix.is_directory dirname with
  | `Yes -> 
    Sys_unix.ls_dir dirname
    |> List.filter ~f:(fun file -> Filename.check_suffix file ".wat" (*|| Filename.check_suffix file ".wasm"*))
    |> List.map ~f:(Filename.concat dirname)
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

let evaluate =
  Command.basic
    ~summary:"Evaluate slicer for all .wat/.wasm files in a directory, or one file when --single option is toggled on"
    Command.Let_syntax.(
      let%map_open filename = anon ("path" %: string)
      and prefix_opt = flag "-p" (optional string) ~doc:"prefix for where to save the results file (default: current directory)"
      and all = flag "-a" no_arg ~doc:"slice on all functions, for all slicing criteria"
      and last = flag "-l" no_arg ~doc:"slice on all functions, for the last slicing criterion" 
      and random = flag "-r" (optional int) ~doc:"N slice on all functions, for N random slicing criteria (default: 1 criterion)"
      and single = flag "--single" no_arg ~doc:"evaluate a single file"
      in
      fun () ->
        begin match prefix_opt with
        | None -> set_output_directory filename ""
        | Some p -> set_output_directory filename p;
        end;
        output ~append:false "data.csv" ["file name"; (* 0 *)
                       "function"; (* 1 *)
                       "slicing criterion"; (* 2 *)
                       "initial number of instrs"; (* 3 *)
                       "slice size without pointers"; (* 4 *)
                       "slice size"; (* 5 *)
                       "slice size difference"; (* 6 *)
                       "reduction factor"; (* 7 *)
                       "cfg_time (ms)"; (* 8 *)
                       "spec_time (ms)"; (* 9 *)
                       "control_time_without_pointers (ms)"; (* 10 *)
                       "control_time (ms)"; (* 11 *)
                       "data_time_without_pointers (ms)"; (* 12 *)
                       "data_time (ms)"; (* 13 *)
                       "mem_time_without_pointers (ms)"; (* 14 *)
                       "mem_time (ms)"; (* 15 *)
                       "global_time_without_pointers (ms)"; (* 16 *)
                       "global_time (ms)"; (* 17 *)
                       "instr_to_keep_time_without_pointers (ms)"; (* 18 *)
                       "instr_to_keep_time (ms)"; (* 19 *)
                       "slicing_time_without_pointers (ms)"; (* 20 *)
                       "slicing_time (ms)";
                       "total_time_difference (ms)"] (* 21 *);
        let criterion_selection = 
          match random with
            | Some n -> `Random n
            | None -> if all then `All else if last then `Last else `Random 1 in
        if single then
          evaluate_file filename criterion_selection
        else
          wat_files_in_directory filename
          |> List.iter ~f:(fun file -> evaluate_file file criterion_selection)
    )