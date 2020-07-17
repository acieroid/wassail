open Core
open Wasm
open Wasmtaint

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
                    Out_channel.output_string ch (Cfg.to_dot cfg))))

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
                        Out_channel.output_string ch (Cfg.to_dot cfg)))))

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
                Printf.printf "%s" (String.concat ~sep:"," (List.map elems ~f:string_of_int)));
            Out_channel.with_file file_out
              ~f:(fun ch ->
                  Out_channel.output_string ch (Call_graph.to_dot cg))))

let mk_intra
    (desc : string)
    (init_summaries : Cfg.t IntMap.t -> Wasm_module.t -> 'a)
    (cb_imported : 'a -> int -> unit)
    (analysis : 'a -> (Spec_inference.state * Spec_inference.state) IntMap.t -> (Spec_inference.state * Spec_inference.state) IntMap.t -> Wasm_module.t -> Cfg.t -> 'a)
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
            let module SpecIntra = Intra_fixpoint.Make(Spec_inference) in
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
                      let (block_spec, instr_spec) = SpecIntra.analyze wasm_mod cfg in
                      analysis summaries (SpecIntra.extract_spec instr_spec) (SpecIntra.extract_spec block_spec) wasm_mod cfg) in
            Logging.info "---------- Analysis done ----------";
            List.iter funs ~f:(fun fid ->
                print_result summaries fid)))
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
       TaintTransfer.use_relational := true;
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

let taint_intra =
  mk_intra
    "Just like `intra`, but only performs the taint analysis"
    (fun cfgs wasm_mod -> Taint_summary.initial_summaries cfgs wasm_mod `Top)
    (fun summaries fid ->
       Printf.printf "Taint summary is:\n%s\n" (Taint_summary.to_string (IntMap.find_exn summaries fid));)
    (fun summaries instr_data block_data wasm_mod cfg ->
       let module Spec = Spec_inference.Spec(struct
           let instr_data () = instr_data
           let block_data () = block_data
         end) in
       Logging.info (Printf.sprintf "---------- Taint analysis of function %d ----------" cfg.idx);
       let module RelSpec = Relational_spec.Spec(struct
           let instr_data = IntMap.empty
         end) in
       (* Run the taint analysis *)
       let module TaintTransfer = Taint_transfer.Make(Spec)(RelSpec) in
       TaintTransfer.use_relational := false;
       let module TaintIntra = Intra_fixpoint.Make(TaintTransfer) in
       TaintIntra.init_summaries summaries;
       let results = TaintIntra.analyze wasm_mod cfg in
       let out_state = TaintIntra.out_state cfg results in
       let taint_summary = TaintIntra.summary cfg out_state in
       IntMap.set summaries ~key:cfg.idx ~data:taint_summary)
    (fun summaries fid ->
       let summary = IntMap.find_exn summaries fid in
       Printf.printf "function %d: %s\n" fid (Taint_summary.to_string summary))

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

let check =
  Command.basic
    ~summary:"Check functions supported by the analysis"
    Command.Let_syntax.(
      let%map_open filename = anon ("file" %: string) in
      fun () ->
        apply_to_textual filename (fun m ->
            let wasm_mod = Wasm_module.of_wasm m in
            let nimports = List.length wasm_mod.imported_funcs in
            let cfgs = IntMap.of_alist_exn (List.mapi wasm_mod.funcs ~f:(fun i _ ->
                let faddr = nimports + i in
                (faddr, Cfg_builder.build faddr wasm_mod))) in
            let supported = IntMap.filter cfgs ~f:Check_support.cfg_is_supported in
            let nsupported = List.length (IntMap.keys supported) in
            let nfuns = List.length (IntMap.keys cfgs) in
            Printf.printf "Supported [%d/%d]: %s\n" nsupported nfuns (String.concat ~sep:" " (List.map (IntMap.data supported) ~f:(fun cfg -> Printf.sprintf "%d" cfg.idx)))))

let int_comma_separated_list =
  Command.Arg_type.create (fun ids ->
      List.map (String.split ids ~on:',') ~f:int_of_string)

let taint_inter =
  let desc = "Performs inter analysis of a set of functions in file [file]. [funs] is a list of comma-separated function ids, e.g., to analyze function 1, then analyze both function 2 and 3 as part of the same fixpoint computation, [funs] is 1 2,3. The full schedule for any file can be computed using the `callgraph` target." in
  let init_summaries cfgs wasm_mod = Taint_summary.initial_summaries cfgs wasm_mod `Bottom in
  Command.basic
    ~summary:desc
    Command.Let_syntax.(
      let%map_open filename = anon ("file" %: string)
      and sccs = anon (sequence ("funs" %: int_comma_separated_list)) in
      fun () ->
        apply_to_textual filename (fun m ->
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
            TaintTransfer.use_relational := false;
            let module TaintIntra = Intra_fixpoint.Make(TaintTransfer) in
            let module Intra = struct
              type full_results = TaintIntra.intra_results * TaintIntra.intra_results
              type state = TaintIntra.state
              type summary = TaintIntra.summary
              let equal_state = TaintIntra.equal_state
              let analyze wasm_mod (cfg : Cfg.t) =
                (* TODO: this re-runs a spec analysis for every intra, not really useful... *)
                let (block_spec, instr_spec) = SpecIntra.analyze wasm_mod cfg in
                SpecData.i_data := SpecIntra.extract_spec instr_spec;
                SpecData.b_data := SpecIntra.extract_spec block_spec;
                TaintIntra.analyze wasm_mod cfg
              let out_state cfg res = TaintIntra.out_state cfg res
              let summary cfg state = TaintIntra.summary cfg state
            end in
            let module TaintInter = Inter_fixpoint.Make(Intra) in
            let summaries = List.fold_left sccs
                ~init:(init_summaries cfgs wasm_mod)
                ~f:(fun summaries funs ->
                    let scc_cfgs = IntMap.filter_keys cfgs ~f:(fun idx -> List.mem funs idx ~equal:Stdlib.(=)) in
                    let sums = TaintInter.analyze scc_cfgs wasm_mod in
                    IntMap.fold sums
                      ~init:summaries
                      ~f:(fun ~key:idx ~data:sum acc ->
                          IntMap.set acc ~key:idx ~data:sum)) in
            IntMap.iteri summaries ~f:(fun ~key:idx ~data:sum ->
                Printf.printf "function %d taint: %s\n" idx (Taint_summary.to_string sum))))

let () =
  Logging.add_callback (fun opt msg -> Printf.printf "[%s] %s" (Logging.option_to_string opt) msg);
  Command.run ~version:"0.0"
    (Command.group ~summary:"Static analysis of WebAssembly"
       ["cfg", cfg
       ; "cfgs", cfgs
       ; "callgraph", callgraph
       ; "taint-inter", taint_inter
       ; "reltaint-intra", reltaint_intra
       ; "taint-intra", taint_intra
       ; "relational-intra", relational_intra
       ; "check", check])
