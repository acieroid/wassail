open Core
open Wassail
open Utils

let cfg =
  Command.basic
    ~summary:"Generate a DOT file representing the CFG of function [fidx] from the wasm file [in], in file [out]"
    Command.Let_syntax.(
      let%map_open file_in = anon ("in" %: string)
      and fidx = anon ("fidx" %: int32)
      and file_out = anon ("out" %: string) in
      fun () ->
        on_cfg file_in fidx (fun cfg -> output_to_file file_out (Cfg.to_dot cfg)))

let cfg_adjlist =
  Command.basic
    ~summary:"Generate the CFG of function [fidx] from the wasm file [in], in two text files: [out].adjlist and [out].nodes"
    Command.Let_syntax.(
      let%map_open file_in = anon ("in" %: string)
      and fidx = anon ("fidx" %: int32)
      and file_out = anon ("out" %: string) in
      fun () ->
        on_cfg file_in fidx (fun cfg ->
            let (nodes, adjacency) = Cfg.to_adjlist cfg in
            output_to_file (file_out ^ ".nodes") nodes;
            output_to_file (file_out ^ ".adjlist") adjacency))

let cfgs =
  Command.basic
    ~summary:"Generate DOT files representing the CFG of each function defined in the wasm file [in], and outputs them in the directory [out_dir]"
    Command.Let_syntax.(
      let%map_open file_in = anon ("in" %: string)
      and out_dir = anon ("out_dir" %: string) in
      fun () ->
        let wasm_mod = Wasm_module.of_file file_in in
        Core_unix.mkdir_p out_dir;
        List.iteri wasm_mod.funcs
          ~f:(fun i _ ->
              let fidx = Int32.(wasm_mod.nfuncimports + (Int32.of_int_exn i)) in
              let cfg = Cfg_builder.build wasm_mod fidx in
              Out_channel.with_file (Printf.sprintf "%s/%ld.dot" out_dir fidx)
                ~f:(fun ch ->
                    Out_channel.output_string ch (Cfg.to_dot cfg))))

let icfg =
  Command.basic
    ~summary:"Generate a DOT file representing an interprocedural CFG (ICFG), starting at function [fidx] from the wasm file [in]"
    Command.Let_syntax.(
      let%map_open file_in = anon ("in" %: string)
      and fidx = anon ("fidx" %: int32)
      and file_out = anon ("out_file" %: string) in
      fun () ->
        let module_ = Wasm_module.of_file file_in in
        if Int32.(fidx < module_.nfuncimports) then
          Printf.printf "Can't build ICFG from function %ld: it is an imported function" fidx
        else
          let icfg = ICFG.make module_ fidx in
          Out_channel.with_file file_out
            ~f:(fun ch ->
                Out_channel.output_string ch (ICFG.to_dot icfg)))
