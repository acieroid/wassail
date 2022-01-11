open Core
open Wassail
open Utils

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
            if List.mem funs f.idx ~equal:Int32.(=) then
              let cfg = Cfg_builder.build module_ f.idx in
              Wasm_module.replace_func m f.idx (Codegen.cfg_to_func_inst cfg)
            else
              Wasm_module.remove_func m f.idx) in
        Out_channel.with_file outfile
          ~f:(fun ch ->
              Out_channel.output_string ch (Wasm_module.to_string module_)))

let generate_all =
  Command.basic
    ~summary:"Outputs the entire WebAssembly file without modification"
    Command.Let_syntax.(
      let%map_open filename = anon ("file" %: string)
      and outfile = anon ("out" %: string) in
      fun () ->
        let module_ = Wasm_module.of_file filename in
        Out_channel.with_file outfile
          ~f:(fun ch ->
              Out_channel.output_string ch (Wasm_module.to_string module_)))
