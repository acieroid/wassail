open Core
open Wassail

let cpg_json =
  Command.basic
    ~summary:"Export a whole-module Code Property Graph as JSON"
    Command.Let_syntax.(
      let%map_open file_in = anon ("in" %: string)
      and file_out = anon ("out" %: string) in
      fun () ->
        Cpg_json.of_file file_in
        |> Cpg_json.write_file file_out)
