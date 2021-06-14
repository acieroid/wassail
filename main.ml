open Core
open Wassail

let int32 = Command.Arg_type.create Int32.of_string

let int32_comma_separated_list =
  Command.Arg_type.create (fun ids ->
      List.map (String.split ids ~on:',') ~f:Int32.of_string)

let load =
  Command.basic
    ~summary:"Load a module and quits"
    Command.Let_syntax.(
      let%map_open file_in = anon ("in" %: string) in
      fun () ->
        try
          let _module : Wasm_module.t = Wasm_module.of_file file_in in
          Printf.printf "Successfully loaded %s\n" file_in
        with e -> Printf.printf "Error when loading %s: %s\n" file_in (Exn.to_string e); exit 1)

let imports =
  Command.basic
    ~summary:"List functions imported by a WebAssembly module"
    Command.Let_syntax.(
      let%map_open file_in = anon ("in" %: string) in
      fun () ->
        let wasm_mod = Wasm_module.of_file file_in in
        List.iter wasm_mod.imported_funcs ~f:(fun (idx, name, ftype) ->
            Printf.printf "%ld\t%s\t%s\n"
              idx name (Type.funtype_to_string ftype)))

let exports =
  Command.basic
    ~summary:"List functions exported by a WebAssembly module"
    Command.Let_syntax.(
      let%map_open file_in = anon ("in" %: string) in
      fun () ->
        let wasm_mod = Wasm_module.of_file file_in in
        List.iter wasm_mod.exported_funcs ~f:(fun (idx, name, ftype) ->
            Printf.printf "%ld\t%s\t%s\n"
              idx name (Type.funtype_to_string ftype)))

let mem_exports =
  Command.basic
    ~summary:"Outputs the number of memories exported by this module"
    Command.Let_syntax.(
      let%map_open file_in = anon ("in" %: string) in
      fun () ->
        let wasm_mod = Wasm_module.of_file file_in in
        let count = List.count wasm_mod.exports ~f:(fun export -> match export.edesc with
            | MemoryExport _ -> true
            | _ -> false) in
        Printf.printf "%d\n" count)

let mem_imports =
  Command.basic
    ~summary:"Outputs the number of memories exported by this module"
    Command.Let_syntax.(
      let%map_open file_in = anon ("in" %: string) in
      fun () ->
        let wasm_mod = Wasm_module.of_file file_in in
        let count = List.count wasm_mod.imports ~f:(fun import -> match import.idesc with
            | MemoryImport _ -> true
            | _ -> false) in
        Printf.printf "%d\n" count)

let functions =
  Command.basic
    ~summary:"Returns the indices of functions of a WebAssembly modules"
    Command.Let_syntax.(
      let%map_open file_in = anon ("in" %: string) in
      fun () ->
        let wasm_mod = Wasm_module.of_file file_in in
        List.iter wasm_mod.funcs
          ~f:(fun f -> Printf.printf "%ld\n" f.idx))

let function_instructions =
  Command.basic
    ~summary:"Returns the labels of instructions of a given function"
    Command.Let_syntax.(
      let%map_open file_in = anon ("in" %: string)
      and fidx = anon ("fidx" %: int32) in
      fun () ->
        let wasm_mod = Wasm_module.of_file file_in in
        let labels = List.fold_left
            (List.find_exn wasm_mod.funcs ~f:(fun f -> Int32.(f.idx = fidx))).code.body
            ~init:Instr.Label.Set.empty
            ~f:(fun acc instr ->
                Instr.Label.Set.union acc (Instr.all_labels instr)) in
        Instr.Label.Set.iter labels ~f:(fun label ->
            Printf.printf "%s\n" (Instr.Label.to_string label)))

let instructions =
  Command.basic
    ~summary:"List instructions used by a WebAssembly module, and how many time each instruction appears"
    Command.Let_syntax.(
      let%map_open file_in = anon ("in" %: string) in
      fun () ->
        let wasm_mod = Wasm_module.of_file file_in in
        StringMap.iteri (Instruction_counter.count wasm_mod) ~f:(fun ~key:instr ~data:count ->
            Printf.printf "%d\t%s\n" count instr))

let sizes =
  Command.basic
    ~summary:"Output the size (in bytes) of each section of a WebAssembly module"
    Command.Let_syntax.(
      let%map_open file_in = anon ("in" %: string) in
      fun () ->
        let wasm_mod = Wasm_module.of_file file_in in
        let sizes = Sizes.generate_binary wasm_mod None in
        Printf.printf "%d\ttype\n" sizes.type_section;
        Printf.printf "%d\timport\n" sizes.import_section;
        Printf.printf "%d\tfunc\n" sizes.func_section;
        Printf.printf "%d\ttable\n" sizes.table_section;
        Printf.printf "%d\tmemory\n" sizes.memory_section;
        Printf.printf "%d\tglobal\n" sizes.global_section;
        Printf.printf "%d\texport\n" sizes.export_section;
        Printf.printf "%d\tstart\n" sizes.start_section;
        Printf.printf "%d\telem\n" sizes.elem_section;
        Printf.printf "%d\tcode\n" sizes.code_section;
        Printf.printf "%d\tdata\n" sizes.data_section)

let cfg =
  Command.basic
    ~summary:"Generate a DOT file representing the CFG of function [fid] from the wat file [in], in file [out]"
    Command.Let_syntax.(
      let%map_open file_in = anon ("in" %: string)
      and fid = anon ("fid" %: int32)
      and file_out = anon ("out" %: string) in
      fun () ->
        let wasm_mod = Wasm_module.of_file file_in in
        if Int32.(fid < wasm_mod.nfuncimports) then
          Printf.printf "Can't build CFG for function %ld: it is an imported function" fid
        else
          let cfg = Cfg.without_empty_nodes_with_no_predecessors (Cfg_builder.build wasm_mod fid) in
          Out_channel.with_file file_out
            ~f:(fun ch ->
                Out_channel.output_string ch (Cfg.to_dot cfg)))

let cfgs =
  Command.basic
    ~summary:"Generate DOT files representing the CFG of each function defined in the wat file [in], and outputs them in the directory [out_dir]"
    Command.Let_syntax.(
      let%map_open file_in = anon ("in" %: string)
      and out_dir = anon ("out_dir" %: string) in
      fun () ->
        let wasm_mod = Wasm_module.of_file file_in in
        Core.Unix.mkdir_p out_dir;
        List.iteri wasm_mod.funcs
          ~f:(fun i _ ->
              let faddr = Int32.(wasm_mod.nfuncimports + (Int32.of_int_exn i)) in
              let cfg = Cfg_builder.build wasm_mod faddr in
              Out_channel.with_file (Printf.sprintf "%s/%ld.dot" out_dir faddr)
                ~f:(fun ch ->
                    Out_channel.output_string ch (Cfg.to_dot cfg))))

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

let count_vars =
  mk_intra "Count the number of program variables generated for a function"
    (Analysis_helpers.mk_intra (fun _ _ -> Int32Map.empty)
       (fun _ wasm_mod annotated_cfg ->
          let module CountVarsIntra = Intra.Make(struct
              type annot_expected = Spec_inference.state
              type state = (Var.Set.t * int)
              [@@deriving equal, compare, sexp]
              type summary = state
              let init_summaries _ = ()
              let init_state _ = (Var.Set.empty, 0)
              let bottom_state _ = (Var.Set.empty, 0)
              let state_to_string _ = ""
              let join_state (s1, n1) (s2, n2) = (Var.Set.union s1 s2, max n1 n2)
              let widen_state = join_state
              let extract_vars (st : Spec.t) : Var.Set.t =
                Var.Set.filter ~f:(function
                    | Merge _ -> false
                    | _ -> true)
                  (Var.Set.union (Var.Set.of_list st.vstack)
                     (Var.Set.union (Var.Set.of_list st.locals)
                        (Var.Set.union (Var.Set.of_list st.globals)
                           (Var.Set.of_list (List.concat_map (Var.OffsetMap.to_alist st.memory) ~f:(fun ((a, _), b) -> [a; b]))))))
              let transfer before after (vars, n) =
                ((Var.Set.union vars
                    (Var.Set.union (extract_vars before) (extract_vars after))),
                 (max n (Var.Set.length (extract_vars after))))
              let control_instr_transfer (_mod : Wasm_module.t) (_cfg : annot_expected Cfg.t) (i : annot_expected Instr.labelled_control) (vars, n) =
                `Simple (transfer i.annotation_before i.annotation_after (vars, n))
              let data_instr_transfer (_mod : Wasm_module.t) (_cfg : annot_expected Cfg.t) (i : annot_expected Instr.labelled_data) (vars, n) =
                transfer i.annotation_before i.annotation_after (vars, n)
              let merge_flows _mod cfg _block (states : (int * state) list) =
                List.fold_left (List.map states ~f:snd) ~init:(bottom_state cfg) ~f:join_state
              let summary _cfg st = st
            end) in
          let result = CountVarsIntra.analyze wasm_mod annotated_cfg in
          let (vars, n) = CountVarsIntra.final_state annotated_cfg result in
          Printf.printf "Vars: %d, max: %d\n" (Var.Set.length vars) n;
          (vars, n)))
    (fun fid summary ->
       Printf.printf "Vars %ld: %d, max: %d\n" fid (Var.Set.length (fst summary)) (snd summary))

let taint_intra =
  mk_intra "Just like `intra`, but only performs the taint analysis" Taint.analyze_intra
    (fun fid summary ->
       Printf.printf "function %ld: %s\n" fid (Taint.Summary.to_string summary))

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
    (fun fid summary -> Printf.printf "function %ld: %s\n" fid (Taint.Summary.to_string summary))

let report_time (msg : string) (t0 : Time.t) (t1 : Time.t) : unit =
  Printf.printf "Time for '%s': %s\n%!" msg (Time.Span.to_string (Time.diff t1 t0))

let dependencies =
  Command.basic
    ~summary:"Produce a PDG for a given function"
    Command.Let_syntax.(
      let%map_open filename = anon ("file" %: string)
      and funidx = anon ("fun" %: int32)
      and dot_filename = anon ("out" %: string) in
      fun () ->
        Spec_inference.propagate_globals := false;
        Spec_inference.propagate_locals := false;
        Spec_inference.use_const := false;
        Printf.printf "parsing module%!\n";
        let module_ = Wasm_module.of_file filename in
        Printf.printf "spec analysis%!\n";
        let cfg = Spec_analysis.analyze_intra1 module_ funidx in
        Printf.printf "outputting PDG to %s\n" dot_filename;
        let use_def_annot = (Use_def.annotate cfg) in
        let control_annot = (Control_deps.annotate_exact (Cfg.without_empty_nodes_with_no_predecessors cfg)) in
        Out_channel.with_file dot_filename
          ~f:(fun ch ->
              Out_channel.output_string ch (Cfg.to_dot cfg
                                              ~annot_str:Spec.to_dot_string
                                              ~extra_data:(use_def_annot ^ control_annot))))

let cdg =
  Command.basic
    ~summary:"Produce a CDG for a given function"
    Command.Let_syntax.(
      let%map_open filename = anon ("file" %: string)
      and funidx = anon ("fun" %: int32)
      and dot_filename = anon ("out" %: string) in
      fun () ->
        Spec_inference.propagate_globals := false;
        Spec_inference.propagate_locals := false;
        Spec_inference.use_const := false;
        let module_ = Wasm_module.of_file filename in
        let cfg = Spec_analysis.analyze_intra1 module_ funidx in
        let control_annot = (Control_deps.annotate_exact cfg) in
        Out_channel.with_file dot_filename
          ~f:(fun ch ->
              Out_channel.output_string ch (Cfg.to_dot cfg
                                              ~include_edges:false
                                              ~extra_data:control_annot)))
let postdom =
  Command.basic
    ~summary:"Visualize the post-dominator tree of a function"
    Command.Let_syntax.(
      let%map_open filename = anon ("file" %: string)
      and funidx = anon ("fun" %: int32)
      and dot_filename = anon ("out" %: string) in
      fun () ->
        let module_ = Wasm_module.of_file filename in
        let cfg = Spec_analysis.analyze_intra1 module_ funidx in
        let tree : Tree.t = Dominance.cfg_post_dominator cfg in
        Out_channel.with_file dot_filename
          ~f:(fun ch ->
              Out_channel.output_string ch (Tree.to_dot tree)))

let slice_cfg =
  Command.basic
    ~summary:"Slice a CFG at the given instruction"
    Command.Let_syntax.(
      let%map_open filename = anon ("file" %: string)
      and funidx = anon ("fun" %: int32)
      and instr = anon ("instr" %: int)
      and dot_filename = anon ("out" %: string) in
      fun () ->
        Spec_inference.propagate_globals := false;
        Spec_inference.propagate_locals := false;
        Spec_inference.use_const := false;
        let module_ = Wasm_module.of_file filename in
        let cfg = Cfg.without_empty_nodes_with_no_predecessors (Spec_analysis.analyze_intra1 module_ funidx) in
        let slicing_criterion = Instr.Label.{ section = Function funidx; id = instr } in
        let sliced_cfg = Slicing.slice cfg slicing_criterion in
        Printf.printf "spec inference\n";
        let annotated_sliced_cfg = Spec_inference.Intra.analyze module_ sliced_cfg in
        let annot_deps = false in
        let use_def_annot = if annot_deps then (Use_def.annotate annotated_sliced_cfg) else "" in
           let control_annot = if annot_deps then (Control_deps.annotate annotated_sliced_cfg) else "" in
        Printf.printf "outputting dot\n";
        Out_channel.with_file dot_filename
          ~f:(fun ch ->
              Out_channel.output_string ch (Cfg.to_dot annotated_sliced_cfg
                                              ~annot_str:Spec.to_dot_string
                                              ~extra_data:(use_def_annot ^ control_annot)));
    )

let slice =
  Command.basic
    ~summary:"Produce an executable program after slicing the given function at the given slicing criterion"
    Command.Let_syntax.(
      let%map_open filename = anon ("file" %: string)
      and funidx = anon ("fun" %: int32)
      and instr = anon ("instr" %: int)
      and outfile = anon ("output" %: string) in
      fun () ->
        Spec_inference.propagate_globals := false;
        Spec_inference.propagate_locals := false;
        Spec_inference.use_const := false;
        Log.enable_debug ();
        let module_ = Wasm_module.of_file filename in
        Log.debug "Performing specification analysis";
        let cfg = Cfg.without_empty_nodes_with_no_predecessors (Spec_analysis.analyze_intra1 module_ funidx) in
        let slicing_criterion = Instr.Label.{ section = Function funidx; id = instr } in
        Log.debug "Performing slicing";
        let instructions = Slicing.slice_alternative (Cfg.body (Cfg.clear_annotations cfg)) (Slicing.instructions_to_keep cfg slicing_criterion) in
        Log.debug "Producing module";
        let module_ = Wasm_module.replace_func module_ funidx
            { idx = cfg.idx;
              name = Some cfg.name;
              type_idx = cfg.type_idx;
              typ = (cfg.arg_types, cfg.return_types);
              code = { locals = cfg.local_types; body = instructions } }
        in
        Log.debug "Saving to file";
        Out_channel.with_file outfile
          ~f:(fun ch -> Out_channel.output_string ch (Wasm_module.to_string module_)))

let random_slice_report =
  Command.basic
    ~summary:"Computes a slice of a random function with a random slicing criterion from the given file, outputs the size of the slice as a percentage of the size of the original function, in terms of number of instructions"
    Command.Let_syntax.(
      Log.enable_debug ();
      let%map_open filename = anon ("file" %: string) in
      let all_labels (instrs : 'a Instr.t list) : Instr.Label.Set.t =
        List.fold_left instrs
            ~init:Instr.Label.Set.empty
            ~f:(fun acc instr ->
                Instr.Label.Set.union acc (Instr.all_labels_no_blocks instr)) in
      fun () ->
        try
          Spec_inference.propagate_globals := false;
          Spec_inference.propagate_locals := false;
          Spec_inference.use_const := false;
          let wasm_mod = Wasm_module.of_file filename in
          let funcs = Array.of_list wasm_mod.funcs in
          if Array.length funcs = 0 then
            begin Printf.printf "ignored\t%s\tno function\n" filename; exit 1 end;
          let func = Array.get funcs (Random.int (Array.length funcs)) in
          (* We don't support unreachable expressions *)
          if Option.is_some (List.find func.code.body ~f:(Instr.contains ~f:(function
              | Instr.Control { instr = Unreachable; _ } -> true
              | _ -> false))) then
            begin Printf.printf "ignored\t%s\t%ld\tunreachable\n" filename func.idx; exit 1 end;
          let labels = Instr.Label.Set.to_array (all_labels func.code.body) in
          if Array.length labels = 0 then
            begin Printf.printf "ignored\t%s\t%ld\tno instruction\n" filename func.idx; exit 1 end;
          let slicing_criterion = Array.get labels (Random.int (Array.length labels)) in
          try
            let cfg = Cfg.without_empty_nodes_with_no_predecessors (Spec_analysis.analyze_intra1 wasm_mod func.idx) in
            let instrs_to_keep = Slicing.instructions_to_keep cfg slicing_criterion in
            let sliced_cfg = Slicing.slice cfg ~instrs:instrs_to_keep slicing_criterion in
            let sliced_func = Codegen.cfg_to_func_inst sliced_cfg in
            let sliced_labels = all_labels sliced_func.code.body in
            Printf.printf "success\t%s\t%ld\t%s\t%d/%d\t%d/%d\n"
              filename
              func.idx
              (Instr.Label.to_string slicing_criterion)
              (Instr.Label.Set.length instrs_to_keep)
              (Array.length labels)
              (Instr.Label.Set.length sliced_labels)
              (Array.length labels)
          with e -> begin Printf.printf "error\t%s\t%ld\t%s\t%s\n" filename func.idx (Instr.Label.to_string slicing_criterion) (Exn.to_string e); exit 1 end
        with
        | Failure s -> begin Printf.printf "error\t%s\t-\t-\t%s\n" filename s; exit 1 end
        | e -> begin Printf.printf "error\t%s\t-\t-\t%s\n" filename (Exn.to_string e); exit 1 end)

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

let generate =
  Command.basic
    ~summary:"Generate a WebAssembly module from a single function"
    Command.Let_syntax.(
      let%map_open filename = anon ("file" %: string)
      and funs = anon ("funs" %: int32_comma_separated_list)
      and outfile = anon ("out" %: string) in
      fun () ->
        let module_ = Wasm_module.of_file filename in
        let module_ = List.fold_left module_.funcs ~init:module_ ~f:(fun m f ->
            if List.mem funs f.idx ~equal:Int32.(=)then
              let cfg = Cfg_builder.build module_ f.idx in
              Wasm_module.replace_func m f.idx (Codegen.cfg_to_func_inst cfg)
            else
              Wasm_module.remove_func m f.idx) in
        Out_channel.with_file outfile
          ~f:(fun ch ->
              Out_channel.output_string ch (Wasm_module.to_string module_)))

let () =
  Command.run ~version:"0.0"
    (Command.group ~summary:"Static analysis of WebAssembly"
       [

       (* General utilities that only required to load the WebAssmbly files *)
         "load", load
       ; "imports", imports
       ; "exports", exports
       ; "instructions", instructions
       ; "sizes", sizes
       ; "mem-imports", mem_imports
       ; "mem-exports", mem_exports
       ; "function-instructions", function_instructions
       ; "functions", functions

       (* Utilities that require building the CFGs *)
       ; "cfg", cfg
       ; "cfgs", cfgs

       ; "dependencies", dependencies
       ; "postdom", postdom
       ; "cdg" , cdg

       (* Utilities that requires building the call graph *)
       ; "callgraph", callgraph
       ; "schedule", schedule

       ; "generate", generate

       (* Other *)
       ; "spec-inference", spec_inference
       ; "count-vars", count_vars
       ; "taint-intra", taint_intra
       ; "taint-inter", taint_inter
       ; "reltaint-intra", reltaint_intra
       ; "relational-intra", relational_intra
       ; "slice-cfg", slice_cfg
       ; "slice", slice
       ; "random-slice", random_slice_report
       ; "find-indirect-calls", find_indirect_calls])
