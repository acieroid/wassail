open Core
open Wassail

let cfg =
  Command.basic
    ~summary:"Generate a DOT file representing the CFG of function [fid] from the wat file [in], in file [out]"
    Command.Let_syntax.(
      let%map_open file_in = anon ("in" %: string)
      and fid = anon ("fid" %: int)
      and file_out = anon ("out" %: string) in
      fun () ->
        let wasm_mod = Wasm_module.of_file file_in in
        let nimports = List.length wasm_mod.imported_funcs in
        if fid < nimports then
          Printf.printf "Can't build CFG for function %d: it is an imported function" fid
        else
          let cfg = Cfg_builder.build fid wasm_mod in
          Out_channel.with_file file_out
            ~f:(fun ch ->
                Out_channel.output_string ch (Cfg.to_dot cfg (fun () -> ""))))

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
              let faddr = wasm_mod.nimports + i in
              let cfg = Cfg_builder.build faddr wasm_mod in
              Out_channel.with_file (Printf.sprintf "%s/%d.dot" out_dir faddr)
                ~f:(fun ch ->
                    Out_channel.output_string ch (Cfg.to_dot cfg (fun () -> "")))))

let callgraph =
  Command.basic
    ~summary:"Generate the call graph for the module from file [in], outputs as DOT to file [out]"
    Command.Let_syntax.(
      let%map_open file_in = anon ("in" %: string)
      and file_out = anon ("out" %: string) in
      fun () ->
        let wasm_mod = Wasm_module.of_file file_in in
        let cg = Call_graph.make wasm_mod in
        let nimports = List.length wasm_mod.imported_funcs in
        let schedule = Call_graph.analysis_schedule cg nimports in
        List.iter schedule ~f:(fun elems ->
            Printf.printf "%s " (String.concat ~sep:"," (List.map elems ~f:string_of_int)));
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
        let nimports = List.length wasm_mod.imported_funcs in
        let schedule = Call_graph.analysis_schedule cg nimports in
        List.iter schedule ~f:(fun elems ->
            Printf.printf "%s " (String.concat ~sep:"," (List.map elems ~f:string_of_int)));
        Printf.printf "\n")


let mk_intra (desc : string) (analysis : Wasm_module.t -> int list -> 'a IntMap.t) (print : int -> 'a -> unit) =
  Command.basic
    ~summary:desc
    Command.Let_syntax.(
    let%map_open filename = anon ("file" %: string)
    and funs = anon (sequence ("funs" %: int)) in
    fun () ->
      let results = analysis (Wasm_module.of_file filename) funs in
      IntMap.iteri results ~f:(fun ~key:id ~data:summary -> print id summary))

let spec_inference =
  mk_intra "Annotate the CFG with the inferred variables"
    (Analysis_helpers.mk_intra (fun _ _ -> IntMap.empty) (fun _ _ annotated_cfg -> annotated_cfg))
    (fun fid annotated_cfg ->
       let file_out = Printf.sprintf "%d.dot" fid in
       Out_channel.with_file file_out
         ~f:(fun ch ->
             Out_channel.output_string ch (Cfg.to_dot annotated_cfg Spec.to_dot_string)))

let count_vars =
  mk_intra "Count the number of program variables generated for a function"
    (Analysis_helpers.mk_intra (fun _ _ -> IntMap.empty)
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
          let (vars, n) = CountVarsIntra.final_state result in
          Printf.printf "Vars: %d, max: %d\n" (Var.Set.length vars) n;
          (vars, n)))
    (fun fid summary ->
       Printf.printf "Vars %d: %d, max: %d\n" fid (Var.Set.length (fst summary)) (snd summary))

let taint_intra =
  mk_intra "Just like `intra`, but only performs the taint analysis" Taint.analyze_intra
    (fun fid summary ->
       Printf.printf "function %d: %s\n" fid (Taint.Summary.to_string summary))

let relational_intra =
  mk_intra "Perform intra-procedural analyses of functions defined in the wat file [file]. The functions analyzed correspond to the sequence of arguments [funs], for example intra foo.wat 1 2 1 analyzes function 1, followed by 2, and then re-analyzes 1 (which can produce different result, if 1 depends on 2)" Relational.analyze_intra
    (fun fid summary ->
       Printf.printf "function %d: %s" fid (Relational.Summary.to_string summary))

let reltaint_intra =
  mk_intra "Perform intra-procedural analyses of functions defined in the wat file [file]. The functions analyzed correspond to the sequence of arguments [funs], for example intra foo.wat 1 2 1 analyzes function 1, followed by 2, and then re-analyzes 1 (which can produce different result, if 1 depends on 2)" Reltaint.analyze_intra
    (fun fid summary ->
       Printf.printf "function %d: %s, %s" fid (Relational.Summary.to_string (fst summary)) (Taint.Summary.to_string (snd summary)))

let int_comma_separated_list =
  Command.Arg_type.create (fun ids ->
      List.map (String.split ids ~on:',') ~f:int_of_string)

let mk_inter (desc : string) (analysis : Wasm_module.t -> int list list -> 'a IntMap.t) (print : int -> 'a -> unit) =
  Command.basic
    ~summary:desc
    Command.Let_syntax.(
      let%map_open filename = anon ("file" %: string)
      and sccs = anon (sequence ("funs" %: int_comma_separated_list)) in
      fun () ->
        let results = analysis (Wasm_module.of_file filename) sccs in
        IntMap.iteri results ~f:(fun ~key:id ~data: summary -> print id summary))

let taint_inter =
  mk_inter "Performs inter analysis of a set of functions in file [file]. [funs] is a list of comma-separated function ids, e.g., to analyze function 1, then analyze both function 2 and 3 as part of the same fixpoint computation, [funs] is 1 2,3. The full schedule for any file can be computed using the `schedule` target."
    Taint.analyze_inter
    (fun fid summary -> Printf.printf "function %d: %s\n" fid (Taint.Summary.to_string summary))

let slice_cfg =
  let report_time (msg : string) (t0 : Time.t) (t1 : Time.t) : unit =
    Printf.printf "Time for '%s': %s\n%!" msg (Time.Span.to_string (Time.diff t1 t0)) in
  Command.basic
    ~summary:"Slice a CFG at each call_indirect instruction"
    Command.Let_syntax.(
      let%map_open filename = anon ("file" %: string)
      and idx = anon ("fun" %: int)
      and criteria = anon ("criterias" %: int_comma_separated_list) in
      fun () ->
        Printf.printf "Reading module\n%!";
        Spec_inference.propagate_globals := false;
        Spec_inference.propagate_locals := false;
        Spec_inference.use_const := false;
        let module_ = Wasm_module.of_file filename in
        let t0 = Time.now () in
        let cfg = Spec_analysis.analyze_intra1 module_ idx in
        let t1 = Time.now () in
        report_time "Spec analysis" t0 t1;
        Printf.printf "outputting initial CFG in initial.dot\n%!";
        Out_channel.with_file "initial.dot"
          ~f:(fun ch ->
              Out_channel.output_string ch (Cfg.to_dot cfg (fun _ -> "")));
        (* TODO: hacky *)
        let actual_criteria = if true then List.take (Slicing.find_call_indirect_instructions cfg) 1 else criteria in
        List.iter actual_criteria ~f:(fun instr_idx ->
            (* instr_idx is the label of a call_indirect instruction, slice it *)
            Printf.printf "Slicing for instruction %d\n%!" instr_idx;
            let t0 = Time.now () in
            let _sliced_cfg = Slicing.slice cfg instr_idx in
            let t1 = Time.now () in
            report_time "Slicing" t0 t1;
            Printf.printf "outputting sliced cfg to sliced-%d.dot\n%!" instr_idx;
            Out_channel.with_file (Printf.sprintf "sliced-%d.dot" instr_idx)
                ~f:(fun ch ->
                    Out_channel.output_string ch (Cfg.to_dot _sliced_cfg (fun _ -> "")));
            (* let _annotated_slice_cfg = Spec.Intra.analyze module_ sliced_cfg in *)
            ()))

let () =
  Logging.add_callback (fun opt msg -> Printf.printf "[%s] %s" (Logging.option_to_string opt) msg);
  Command.run ~version:"0.0"
    (Command.group ~summary:"Static analysis of WebAssembly"
       ["cfg", cfg
       ; "cfgs", cfgs
       ; "callgraph", callgraph
       ; "schedule", schedule
       ; "spec-inference", spec_inference
       ; "count-vars", count_vars
       ; "taint-intra", taint_intra
       ; "taint-inter", taint_inter
       ; "reltaint-intra", reltaint_intra
       ; "relational-intra", relational_intra
       ; "slice", slice_cfg])
