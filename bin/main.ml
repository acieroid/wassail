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
let intra =
  Command.basic
    ~summary:"Perform intra-procedural analyses of functions defined in the wat file [file]. The functions analyzed correspond to the sequence of arguments [funs], for example intra foo.wat 1 2 1 analyzes function 1, followed by 2, and then re-analyzes 1 (which can produce different result, if 1 depends on 2)"
    Command.Let_syntax.(
      let%map_open filename = anon ("file" %: string)
      and funs = anon (sequence ("funs" %: int)) in
      fun () ->
        apply_to_textual filename (fun m ->
            let module SpecIntra = Intra_fixpoint.Make(Spec_inference) in
            let wasm_mod = Wasm_module.of_wasm m in
            let nimports = List.length wasm_mod.imported_funcs in
            let cfgs = IntMap.of_alist_exn (List.mapi wasm_mod.funcs ~f:(fun i _ ->
                let faddr = nimports + i in
                (faddr, Cfg_builder.build faddr wasm_mod))) in
            let summaries = List.fold_left funs
                ~init:(Relational_summary.initial_summaries cfgs wasm_mod `Top,
                       Taint_summary.initial_summaries cfgs wasm_mod `Top)
              ~f:(fun summaries fid ->
                  Printf.printf "Analyzing function %d\n" fid;
                  if fid < nimports then begin
                    Printf.printf "This is an imported function, it does not have to be analyzed.\n";
                    Printf.printf "Relational summary is:\n%s\n" (Relational_summary.to_string (IntMap.find_exn (fst summaries) fid));
                    Printf.printf "Taint summary is:\n%s\n" (Taint_summary.to_string (IntMap.find_exn (snd summaries) fid));
                    summaries
                  end else
                    let cfg = IntMap.find_exn cfgs fid in
                    let (block_spec, instr_spec) = SpecIntra.analyze wasm_mod cfg in
                    let extract_spec (m : SpecIntra.intra_results) = IntMap.filter_map m ~f:(function
                        | Uninitialized, _ -> None
                        | Simple s, Simple s' -> Some (s, s')
                        | Simple s, Branch (s1, s2) ->
                          assert (Spec_inference.compare_state s1 s2 = 0); (* both states should be equal *)
                          Some (s, s1)
                        | _ -> failwith "invalid spec") in
                    let module Spec = Spec_inference.Spec(struct
                      let instr_data = extract_spec instr_spec
                      let block_data = extract_spec block_spec
                    end) in
                    (* Plug in the results of the spec analysis *)
                    let module RelationalIntra = Intra_fixpoint.Make(Relational_transfer.Make(Spec)) in
                    RelationalIntra.init_summaries (fst summaries);
                    Printf.printf "-------------------------\nMain analysis\n-------------------\n";
                    let results = RelationalIntra.analyze wasm_mod cfg in
                    let out_state = RelationalIntra.out_state cfg results in
                    (* Printf.printf "%d: %s\n" cfg.idx (RelationalIntra.state_to_string out_state); *)
                    let relational_summary = RelationalIntra.summary cfg out_state in
                    Printf.printf "Relational summary is:\n%s\n" (Relational_summary.to_string relational_summary);

                    (* Run the taint analysis *)
                    let module TaintIntra = Intra_fixpoint.Make(Taint_transfer.Make(Spec)) in
                    TaintIntra.init_summaries (snd summaries);
                    let results = TaintIntra.analyze wasm_mod cfg in
                    let out_state = TaintIntra.out_state cfg results in
                    Printf.printf "%d: %s\n" cfg.idx (TaintIntra.state_to_string out_state);
                    let taint_summary = TaintIntra.summary cfg out_state in
                    Printf.printf "Taint summary is:\n%s\n" (Taint_summary.to_string taint_summary);
                    (IntMap.set (fst summaries) ~key:fid ~data:relational_summary,
                     IntMap.set (snd summaries) ~key:fid ~data:taint_summary)


                ) in
            Printf.printf "---------------\nAnalysis done, resulting summaries are:\n";
            List.iter funs ~f:(fun fid ->
                let summary = IntMap.find_exn (fst summaries) fid in
                Printf.printf "function %d: %s\n" fid (Relational_summary.to_string summary))))

let check =
  Command.basic
    ~summary:"Check functions supported by the analysis"
    Command.Let_syntax.(
      let%map_open filename = anon ("file" %: string) in
      fun () ->
        apply_to_textual filename (fun m ->
            let module SpecIntra = Intra_fixpoint.Make(Spec_inference) in
            let nimports = List.length (List.filter m.it.imports ~f:(fun import -> match import.it.idesc.it with
                | Ast.FuncImport _ -> true
                | _ -> false)) in
            let funcs = List.mapi m.it.funcs ~f:(fun i f -> (i+nimports, Check_support.func_is_supported f)) in
            let supported = List.filter funcs ~f:snd in
            Printf.printf "Supported [%d/%d]: %s\n" (List.length supported) (List.length funcs) (String.concat ~sep:" " (List.map supported ~f:(fun (idx, _) -> Printf.sprintf "%d" idx)))))

let inter =
  Command.basic
    ~summary:"Fully analyzes the wat file [file]"
    Command.Let_syntax.(
      let%map_open filename = anon ("file" %: string) in
      fun () ->
        apply_to_textual filename (fun _m ->
            failwith "TODO" (*
            let wasm_mod = Wasm_module.of_wasm m in
            let nimports = List.length wasm_mod.imported_funcs in
            let cfgs = IntMap.of_alist_exn (List.mapi wasm_mod.funcs ~f:(fun i _ ->
                let faddr = nimports + i in
                (faddr, Cfg_builder.build faddr wasm_mod))) in
            Inter_fixpoint.analyze cfgs wasm_mod;
            Printf.printf "--------- Results ---------\n";
            IntMap.iteri !(Inter_fixpoint.summaries)
              ~f:(fun ~key:fid ~data:summary ->
                  Printf.printf "Summary of function %d:\n%s" fid (Summary.to_string summary))*)))

let () =
  Logging.add_callback (fun opt msg -> Printf.printf "[%s] %s" (Logging.option_to_string opt) msg);
  Command.run ~version:"0.0"
    (Command.group ~summary:"Static analysis of WebAssembly"
       ["cfg", cfg
       ; "cfgs", cfgs
       ; "inter", inter
       ; "intra", intra
       ; "check", check])
