open Core
open Wasm
open Wassail

let apply_to_textual (filename : string) (f : Ast.module_ -> unit) =
  let extract (l : (Script.var option * Script.definition) list) =
    List.iter l ~f:(fun (_var_opt, def) ->
        match def.it with
        | Script.Textual m -> f m
        | Script.Encoded _ -> failwith "unsupported"
        | Script.Quoted _ -> failwith "unsupported"
      ) in
  parse_file filename extract

let cfg =
  Command.basic
    ~summary:"Generate a DOT file representing the CFG of function [fid] from the wat file [in], in file [out]"
    Command.Let_syntax.(
      let%map_open file_in = anon ("in" %: string)
      and fid = anon ("fid" %: int)
      and file_out = anon ("out" %: string) in
      fun () ->
        apply_to_textual file_in (fun m ->
            let wasm_mod = Wasm_module.of_wasm m in
            let nimports = List.length wasm_mod.imported_funcs in
            if fid < nimports then
              Printf.printf "Can't build CFG for function %d: it is an imported function" fid
            else
              let cfg = Cfg_builder.build fid wasm_mod in
              Out_channel.with_file file_out
                ~f:(fun ch ->
                    Out_channel.output_string ch (Cfg.to_dot cfg (fun () -> "")))))

let cfgs =
  Command.basic
    ~summary:"Generate DOT files representing the CFG of each function defined in the wat file [in], and outputs them in the directory [out_dir]"
    Command.Let_syntax.(
      let%map_open file_in = anon ("in" %: string)
      and out_dir = anon ("out_dir" %: string) in
      fun () ->
        apply_to_textual file_in (fun m ->
            let wasm_mod = Wasm_module.of_wasm m in
            Core.Unix.mkdir_p out_dir;
            let nimports = List.length wasm_mod.imported_funcs in
            List.iteri wasm_mod.funcs
              ~f:(fun i _ ->
                  let faddr = nimports + i in
                  let cfg = Cfg_builder.build faddr wasm_mod in
                  Out_channel.with_file (Printf.sprintf "%s/%d.dot" out_dir faddr)
                    ~f:(fun ch ->
                        Out_channel.output_string ch (Cfg.to_dot cfg (fun () -> ""))))))

let callgraph =
  Command.basic
    ~summary:"Generate the call graph for the module from file [in], outputs as DOT to file [out]"
    Command.Let_syntax.(
      let%map_open file_in = anon ("in" %: string)
      and file_out = anon ("out" %: string) in
      fun () ->
        apply_to_textual file_in (fun m ->
            let wasm_mod = Wasm_module.of_wasm m in
            let cg = Call_graph.make wasm_mod in
            let nimports = List.length wasm_mod.imported_funcs in
            let schedule = Call_graph.analysis_schedule cg nimports in
            List.iter schedule ~f:(fun elems ->
                Printf.printf "%s " (String.concat ~sep:"," (List.map elems ~f:string_of_int)));
            Out_channel.with_file file_out
              ~f:(fun ch ->
                  Out_channel.output_string ch (Call_graph.to_dot cg))))

let schedule =
  Command.basic
    ~summary:"Generate the analysis schedule for the module from file [in]"
    Command.Let_syntax.(
      let%map_open file_in = anon ("in" %: string) in
      fun () ->
        apply_to_textual file_in (fun m ->
            let wasm_mod = Wasm_module.of_wasm m in
            let cg = Call_graph.make wasm_mod in
            let nimports = List.length wasm_mod.imported_funcs in
            let schedule = Call_graph.analysis_schedule cg nimports in
            List.iter schedule ~f:(fun elems ->
                Printf.printf "%s " (String.concat ~sep:"," (List.map elems ~f:string_of_int)));
            Printf.printf "\n"))

let mk_intra
    (desc : string)
    (init_summaries : unit Cfg.t IntMap.t -> Wasm_module.t -> 'a)
    (cb_imported : 'a -> int -> unit)
    (analysis : 'a -> Wasm_module.t -> Spec_inference.state Cfg.t -> 'a)
    (print_result : 'a -> int -> unit)
  =
  Command.basic
    ~summary:desc
    Command.Let_syntax.(
      let%map_open filename = anon ("file" %: string)
      and funs = anon (sequence ("funs" %: int)) in
      Logging.info (Printf.sprintf "Parsing program from file %s..." filename);
      fun () ->
        apply_to_textual filename (fun m ->
            let module SpecIntra = Intra.Make(Spec_inference) in
            Logging.info "Importing module...";
            let wasm_mod = Wasm_module.of_wasm m in
            Logging.info "Building CFGs";
            let cfgs = IntMap.of_alist_exn (List.mapi wasm_mod.funcs ~f:(fun i _ ->
                let faddr = wasm_mod.nimports + i in
                (faddr, Cfg_builder.build faddr wasm_mod))) in
            Logging.info "Initializing summaries";
            let summaries = List.fold_left funs
                ~init:(init_summaries cfgs wasm_mod)
                ~f:(fun summaries fid ->
                    if fid < wasm_mod.nimports then begin
                      Printf.printf "This is an imported function, it does not have to be analyzed.\n";
                      cb_imported summaries fid;
                      summaries
                    end else
                      let cfg = IntMap.find_exn cfgs fid in
                      Logging.info (Printf.sprintf "---------- Spec analysis of function %d ----------" cfg.idx);
                      let annotated_cfg = SpecIntra.analyze wasm_mod cfg in
                      analysis summaries wasm_mod annotated_cfg) in
            Logging.info "---------- Analysis done ----------";
            List.iter funs ~f:(fun fid ->
                print_result summaries fid)))

let spec_inference =
  mk_intra
    "Annotate the CFG with the inferred variables"
    (fun _cfgs _wasm_mod -> ())
    (fun _summaries _fid -> ())
    (fun () _wasm_mod annotated_cfg ->
       let file_out = Printf.sprintf "%d.dot" annotated_cfg.idx in
       Out_channel.with_file file_out
         ~f:(fun ch ->
             Out_channel.output_string ch (Cfg.to_dot annotated_cfg Spec_inference.state_to_dot_string)))
    (fun () _ -> ())

let count_vars =
  mk_intra
    "Count the number of program variables generated for a function"
    (fun _cfgs _wasm_mod -> IntMap.empty)
    (fun summaries fid ->
       let summary = IntMap.find_exn summaries fid in
       Printf.printf "Number of variables for %d: %d, max at the same time: %d\n" fid (Var.Set.length (fst summary)) (snd summary))
    (fun summaries wasm_mod annotated_cfg ->
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
           let extract_vars (st : Spec_inference.state) : Var.Set.t =
             Var.Set.filter ~f:(function
                 | Merge _ -> false
                 | MemoryKey _ | MemoryVal _ | MemoryValNew _ -> false
                 | _ -> true)
               (Var.Set.union (Var.Set.of_list st.vstack)
                  (Var.Set.union (Var.Set.of_list st.locals)
                     (Var.Set.union (Var.Set.of_list st.globals)
                        (Var.Set.of_list (List.concat_map (Var.Map.to_alist st.memory) ~f:(fun (a, b) -> [a; b]))))))
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
       IntMap.set summaries ~key:annotated_cfg.idx ~data:(vars, n))
    (fun summaries fid ->
       let summary = IntMap.find_exn summaries fid in
       Printf.printf "Vars %d: %d, max: %d\n" fid (Var.Set.length (fst summary)) (snd summary))

let taint_intra =
  mk_intra
    "Just like `intra`, but only performs the taint analysis"
    (fun cfgs wasm_mod -> Taint_summary.initial_summaries cfgs wasm_mod `Bottom)
    (fun summaries fid ->
       Printf.printf "Taint summary is:\n%s\n" (Taint_summary.to_string (IntMap.find_exn summaries fid));)
    (fun summaries wasm_mod cfg ->
       Logging.info (Printf.sprintf "---------- Taint analysis of function %d ----------" cfg.idx);
       let module RelSpec = Relational_spec.Spec(struct
           let instr_data = IntMap.empty
         end) in
       (* Run the taint analysis *)
       let module TaintTransfer = Taint_transfer.Make(RelSpec) in
       Taint_options.use_relational := false;
       let module TaintIntra = Intra.Make(TaintTransfer) in
       TaintIntra.init_summaries summaries;
       let result_cfg = TaintIntra.analyze wasm_mod cfg in
       let final_state = TaintIntra.final_state result_cfg in
       let taint_summary = TaintIntra.summary cfg final_state in
       IntMap.set summaries ~key:cfg.idx ~data:taint_summary)
    (fun summaries fid ->
       let summary = IntMap.find_exn summaries fid in
       Printf.printf "function %d: %s\n" fid (Taint_summary.to_string summary))

(*
let reltaint_intra =
  mk_intra
    "Perform intra-procedural analyses of functions defined in the wat file [file]. The functions analyzed correspond to the sequence of arguments [funs], for example intra foo.wat 1 2 1 analyzes function 1, followed by 2, and then re-analyzes 1 (which can produce different result, if 1 depends on 2)"
    (fun cfgs wasm_mod -> (Relational_summary.initial_summaries cfgs wasm_mod `Top,
                           Taint_summary.initial_summaries cfgs wasm_mod `Top))
    (fun summaries fid ->
       Printf.printf "Relational summary is:\n%s\n" (Relational_summary.to_string (IntMap.find_exn (fst summaries) fid));
       Printf.printf "Taint summary is:\n%s\n" (Taint_summary.to_string (IntMap.find_exn (snd summaries) fid));)
    (fun summaries instr_data block_data wasm_mod cfg ->
       let module Spec = Spec_inference.Spec(struct
           let instr_data () = instr_data
           let block_data () = block_data
         end) in
       let module RelationalIntra = Intra_fixpoint.Make(Relational_transfer.Make(Spec)) in
       RelationalIntra.init_summaries (fst summaries);
       Logging.info "---------- Relational analysis ----------";
       let results = RelationalIntra.analyze wasm_mod cfg in
       let out_state = RelationalIntra.out_state cfg results in
       (* Printf.printf "%d: %s\n" cfg.idx (RelationalIntra.state_to_string out_state); *)
       let relational_summary = RelationalIntra.summary cfg out_state in
       Printf.printf "Relational summary is:\n%s\n" (Relational_summary.to_string relational_summary);
       let module RelSpec = Relational_spec.Spec(struct
           let instr_data = RelationalIntra.extract_spec (snd results)
         end) in
       (* Run the taint analysis *)
       Logging.info "---------- Taint analysis ----------";
       let module TaintTransfer = Taint_transfer.Make(Spec)(RelSpec) in
       Taint_options.use_relational := true;
       let module TaintIntra = Intra_fixpoint.Make(TaintTransfer) in
       TaintIntra.init_summaries (snd summaries);
       let results = TaintIntra.analyze wasm_mod cfg in
       let out_state = TaintIntra.out_state cfg results in
       let taint_summary = TaintIntra.summary cfg out_state in
       (IntMap.set (fst summaries) ~key:cfg.idx ~data:relational_summary,
        IntMap.set (snd summaries) ~key:cfg.idx ~data:taint_summary))
    (fun summaries fid ->
       Printf.printf "function %d relational: %s\n" fid (Relational_summary.to_string (IntMap.find_exn (fst summaries) fid));
       Printf.printf "function %d taint: %s\n" fid (Taint_summary.to_string (IntMap.find_exn (snd summaries) fid)))

let relational_intra =
  mk_intra
    "Just like `intra`, but only performs relational analysis"
    (fun cfgs wasm_mod -> Relational_summary.initial_summaries cfgs wasm_mod `Top)
    (fun summaries fid ->
       Printf.printf "Relational summary is:\n%s\n" (Relational_summary.to_string (IntMap.find_exn summaries fid)))
    (fun summaries instr_data block_data wasm_mod cfg ->
       let module Spec = Spec_inference.Spec(struct
           let instr_data () = instr_data
           let block_data () = block_data
         end) in
       let module RelationalIntra = Intra_fixpoint.Make(Relational_transfer.Make(Spec)) in
       RelationalIntra.init_summaries summaries;
       Logging.info (Printf.sprintf "---------- Relational analysis of function %d ----------" cfg.idx);
       let results = RelationalIntra.analyze wasm_mod cfg in
       let out_state = RelationalIntra.out_state cfg results in
       let relational_summary = RelationalIntra.summary cfg out_state in
       let module RelSpec = Relational_spec.Spec(struct
           let instr_data = RelationalIntra.extract_spec (snd results)
         end) in
       IntMap.set summaries ~key:cfg.idx ~data:relational_summary)
    (fun summaries fid ->
       Printf.printf "function %d relational: %s\n" fid (Relational_summary.to_string (IntMap.find_exn summaries fid)))

let int_comma_separated_list =
  Command.Arg_type.create (fun ids ->
      List.map (String.split ids ~on:',') ~f:int_of_string)

let timer_start () = Unix.gettimeofday ()
let timer_value t0 =
  let t1 = Unix.gettimeofday () in
  t1 -. t0

let taint_inter =
  let desc = "Performs inter analysis of a set of functions in file [file]. [funs] is a list of comma-separated function ids, e.g., to analyze function 1, then analyze both function 2 and 3 as part of the same fixpoint computation, [funs] is 1 2,3. The full schedule for any file can be computed using the `schedule` target." in
  let init_summaries cfgs wasm_mod = Taint_summary.initial_summaries cfgs wasm_mod `Bottom in
  Command.basic
    ~summary:desc
    Command.Let_syntax.(
      let%map_open filename = anon ("file" %: string)
      and sccs = anon (sequence ("funs" %: int_comma_separated_list)) in
      fun () ->
        apply_to_textual filename (fun m ->
            let timer_init = timer_start () in
            let wasm_mod = Wasm_module.of_wasm m in
            let cfgs = IntMap.of_alist_exn (List.mapi wasm_mod.funcs ~f:(fun i _ ->
                let faddr = wasm_mod.nimports + i in
                (faddr, Cfg_builder.build faddr wasm_mod))) in
            let module SpecIntra = Intra_fixpoint.Make(Spec_inference) in
            let module SpecData = struct
              let i_data = ref IntMap.empty
              let b_data = ref IntMap.empty
              let instr_data () = !i_data
              let block_data () = !b_data
            end in
            let module Spec = Spec_inference.Spec(SpecData) in
            let module RelSpec = Relational_spec.Spec(struct
                let instr_data = IntMap.empty
              end) in
            let module TaintTransfer = Taint_transfer.Make(Spec)(RelSpec) in
            Taint_options.use_relational := false;
            let module TaintIntra = Intra_fixpoint.Make(TaintTransfer) in
            let module Intra = struct
              type full_results = TaintIntra.intra_results * TaintIntra.intra_results
              type state = TaintIntra.state
              type summary = TaintIntra.summary
              let equal_state = TaintIntra.equal_state
              let analyze wasm_mod (cfg : 'a Cfg.t) =
                (* TODO: this re-runs a spec analysis for every intra, not really useful... *)
                let (block_spec, instr_spec) = SpecIntra.analyze wasm_mod cfg in
                let timer_spec = timer_start () in
                SpecData.i_data := SpecIntra.extract_spec instr_spec;
                SpecData.b_data := SpecIntra.extract_spec block_spec;
                Printf.printf "Spec of %d: %.3f\n" cfg.idx (timer_value timer_spec);
                Gc.compact ();
                let timer_inter = timer_start () in
                Printf.printf "Performing taint intra of %d\n" cfg.idx;
                let res = TaintIntra.analyze wasm_mod cfg in
                Logging.info (Printf.sprintf "Inter-step time: %.3f" (timer_value timer_inter));
                res
              let out_state cfg res = TaintIntra.out_state cfg res
              let summary cfg state = TaintIntra.summary cfg state
            end in
            let module TaintInter = Inter_fixpoint.Make(Intra) in
            Logging.info (Printf.sprintf "Initialization time: %.3f" (timer_value timer_init));
            let summaries = List.fold_left sccs
                ~init:(init_summaries cfgs wasm_mod)
                ~f:(fun summaries funs ->
                    let scc_cfgs = IntMap.filter_keys cfgs ~f:(fun idx -> List.mem funs idx ~equal:Stdlib.(=)) in
                    TaintIntra.init_summaries summaries;
                    let sums = TaintInter.analyze scc_cfgs wasm_mod in
                    IntMap.fold sums
                      ~init:summaries
                      ~f:(fun ~key:idx ~data:sum acc ->
                          IntMap.set acc ~key:idx ~data:sum)) in
            IntMap.iteri summaries ~f:(fun ~key:idx ~data:sum ->
                Printf.printf "function %d taint: %s\n" idx (Taint_summary.to_string sum))))
*)
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
       (* ; "taint-inter", taint_inter
       ; "reltaint-intra", reltaint_intra
          ; "relational-intra", relational_intra *)])

