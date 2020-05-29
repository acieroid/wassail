open Core
open Wasm
open Wasmtaint

(* TODO: multiple targets:
   - cfg to build the CFGs and output them into dot files:
     ./main.exe cfg 1 1.dot # creates CFG for function 1 in 1.dot
     ./main.exe cfg all out/ # creates all CFGs in out/1.dot, etc.
   - intra to run an intra analysis on a given function. Can also be used to run multiple analyses
     ./main.exe intra 1 # runs intra of function 1
     ./main.exe intra 1,2,1 # runs intra of 1, followed by 2, followed by 1 again
   - inter to run the full analysis
     ./main.exe inter
*)

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
            List.iteri wasm_mod.funcs
              ~f:(fun i _ ->
                  let faddr = wasm_mod.nimports + i in
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
            let wasm_mod = Wasm_module.of_wasm m in
            let cfgs = IntMap.of_alist_exn (List.mapi wasm_mod.funcs ~f:(fun i _ ->
                let faddr = wasm_mod.nimports + i in
                (faddr, Cfg_builder.build faddr wasm_mod))) in
            let summaries = List.fold_left funs
              (* All summaries are initially bottom *)
              ~init:(IntMap.map cfgs ~f:(fun cfg -> Summary.bottom cfg wasm_mod))
              ~f:(fun summaries fid ->
                  Printf.printf "Analyzing function %d\n" fid;
                  let cfg = IntMap.find_exn cfgs fid in
                  let results = Intra_fixpoint.analyze cfg summaries wasm_mod in
                  let out_state = Intra_fixpoint.out_state cfg results in
                  let summary = Summary.make cfg out_state in
                  Printf.printf "Summary is:\n%s\n" (Summary.to_string summary);
                  IntMap.set summaries ~key:fid ~data:summary) in
            Printf.printf "---------------\nAnalysis done, resulting summaries are:\n";
            IntMap.iteri summaries ~f:(fun ~key:fid ~data:summary ->
                                        Printf.printf "function %d:\n%s\n" fid (Summary.to_string summary))))


let inter =
  Command.basic
    ~summary:"Fully analyzes the wat file [file]"
    Command.Let_syntax.(
      let%map_open filename = anon ("file" %: string) in
      fun () ->
        apply_to_textual filename (fun m ->
            let wasm_mod = Wasm_module.of_wasm m in
            let cfgs = IntMap.of_alist_exn (List.mapi wasm_mod.funcs ~f:(fun i _ ->
                let faddr = wasm_mod.nimports + i in
                (faddr, Cfg_builder.build faddr wasm_mod))) in
            Inter_fixpoint.analyze cfgs wasm_mod;
            Printf.printf "--------- Results ---------\n";
            IntMap.iteri !(Inter_fixpoint.summaries)
              ~f:(fun ~key:fid ~data:summary ->
                  Printf.printf "Summary of function %d:\n%s" fid (Summary.to_string summary))))


let () =
  Logging.add_callback (fun opt msg -> Printf.printf "[%s] %s" (Logging.option_to_string opt) msg);
  Command.run ~version:"0.0"
    (Command.group ~summary:"Static analysis of WebAssembly"
       ["cfg", cfg
       ; "cfgs", cfgs
       ; "inter", inter
       ; "intra", intra])
