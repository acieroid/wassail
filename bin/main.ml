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
  Printf.printf "Success? %b" (parse_file filename extract)

let main filename =
  Logging.add_callback (fun opt msg -> Printf.printf "[%s] %s" (Logging.option_to_string opt) msg);
  apply_to_textual filename (fun m ->
      let wasm_mod = Wasm_module.init m in
      let nglobals = List.length wasm_mod.globals in
      let cfgs = IntMap.of_alist_exn (List.mapi wasm_mod.funcs ~f:(fun faddr _ -> (faddr, Cfg_builder.build faddr wasm_mod))) in
      IntMap.iter cfgs ~f:(fun cfg ->
          Printf.printf "CFG for function %d\n" cfg.idx;
          Printf.printf "---------------\n%s\n---------------\n" (Cfg.to_dot cfg)
        );
      let results = Inter_fixpoint.analyze cfgs nglobals in
      Printf.printf "--------- Results ---------\n";
      IntMap.iteri results ~f:(fun ~key:cfg_idx ~data:res ->
          Printf.printf "Results for function %d: %s\n" cfg_idx (Domain.to_string res)))

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
