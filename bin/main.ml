open Core
open Wasm
open Wasmtaint

let main file =
  Logging.add_callback (fun opt msg -> Printf.printf "[%s] %s" (Logging.option_to_string opt) msg);
  let run (l : (Script.var option * Script.definition) list) =
    List.iter l ~f:(fun (_var_opt, def) ->
        match def.it with
        | Script.Textual m ->
          let store = Store.init m in
          let nglobals = List.length store.globals in
          let cfgs = IntMap.of_alist_exn (List.mapi store.funcs ~f:(fun faddr _ -> (faddr, Cfg_builder.build faddr store))) in
          IntMap.iter cfgs ~f:(fun cfg ->
              Printf.printf "CFG for function %d\n" cfg.idx;
              Printf.printf "---------------\n%s\n---------------\n" (Cfg.to_dot cfg)
            );
          let results = Inter_fixpoint.analyze cfgs nglobals in
          Printf.printf "--------- Results ---------\n";
          IntMap.iteri results ~f:(fun ~key:cfg_idx ~data:res ->
              Printf.printf "Results for function %d: %s\n" cfg_idx (Domain.to_string res))
        | Script.Encoded _ -> failwith "unsupported"
        | Script.Quoted _ -> failwith "unsupported"
      ) in
  Printf.printf "Success? %b" (parse_file file run)


let spec =
  let open Command.Param in
  anon ("filename" %: string)

let command =
  Command.basic
    ~summary:"WasmTaint"
    (Command.Param.map spec ~f:(fun filename ->
         (fun () -> main filename)))
let () =
  Command.run ~version:"0.0" command
