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
      and out_dir = anon ("out_dir" %: string)
      and timings = flag "-timings" no_arg ~doc:" Report time spent parsing, building CFGs, rendering DOT, and writing DOT files"
      and no_dot = flag "-no-dot" no_arg ~doc:" Build CFGs without rendering or writing DOT files" in
      fun () ->
        let (wasm_mod, parse_stats), parse_time =
          timed (fun () ->
              if timings then
                Wasm_module.of_file_with_stats file_in
              else
                Wasm_module.of_file file_in, { read = 0.0; decode = 0.0; convert = 0.0 })
        in
        if not no_dot then Core_unix.mkdir_p out_dir;
        let cfg_time = ref 0.0 in
        let cfg_stats = Cfg_builder.empty_build_stats () in
        let dot_time = ref 0.0 in
        let write_time = ref 0.0 in
        Wasm_module.iter_defined_funcs wasm_mod
          ~f:(fun f ->
              let fidx = f.idx in
              let cfg, elapsed =
                timed (fun () ->
                    if timings then
                      let cfg, stats = Cfg_builder.build_with_stats wasm_mod fidx in
                      Cfg_builder.add_build_stats cfg_stats stats;
                      cfg
                    else
                      Cfg_builder.build wasm_mod fidx)
              in
              cfg_time := !cfg_time +. elapsed;
              if not no_dot then begin
                let dot, elapsed = timed (fun () -> Cfg.to_dot cfg) in
                dot_time := !dot_time +. elapsed;
                let (), elapsed = timed (fun () ->
                    Out_channel.with_file (Printf.sprintf "%s/%ld.dot" out_dir fidx)
                      ~f:(fun ch ->
                          Out_channel.output_string ch dot)) in
                write_time := !write_time +. elapsed
              end);
        if timings then
          let total = parse_time +. !cfg_time +. !dot_time +. !write_time in
          eprintf
            "cfgs timings: functions=%d parse=%.6fs cfg=%.6fs dot=%.6fs write=%.6fs measured_total=%.6fs\nparse timings: read=%.6fs decode=%.6fs convert=%.6fs\ncfg_builder timings: raw_cfg=%.6fs return_edges=%.6fs filter_empty_blocks=%.6fs redirect_empty_edges=%.6fs entry_block=%.6fs edge_maps=%.6fs block_map=%.6fs metadata=%.6fs enclosing_block_id=%.6fs\n%!"
            (Wasm_module.num_defined_funcs wasm_mod)
            parse_time
            !cfg_time
            !dot_time
            !write_time
            total
            parse_stats.read
            parse_stats.decode
            parse_stats.convert
            cfg_stats.raw_cfg
            cfg_stats.return_edges
            cfg_stats.filter_empty_blocks
            cfg_stats.redirect_empty_edges
            cfg_stats.entry_block
            cfg_stats.edge_maps
            cfg_stats.block_map
            cfg_stats.metadata
            cfg_stats.enclosing_block_id)

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
