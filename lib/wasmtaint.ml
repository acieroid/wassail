open Core_kernel
open Wasm

include Helpers
module Store = Store
module Cfg_builder = Cfg_builder
module Cfg = Cfg
module Domain = Domain
module Instr = Instr
module Inter_fixpoint = Inter_fixpoint
module Basic_block = Basic_block

let trace name = print_endline ("-- " ^ name)

let error at category msg =
  trace ("Error: ");
  prerr_endline (Source.string_of_region at ^ ": " ^ category ^ ": " ^ msg);
  false

let input_from get_script run =
  try
    let script = get_script () in
    trace "Running...";
    run script;
    true
  with
  | Decode.Code (at, msg) -> error at "decoding error" msg
  | Parse.Syntax (at, msg) -> error at "syntax error" msg
  | Valid.Invalid (at, msg) -> error at "invalid module" msg
  | Import.Unknown (at, msg) -> error at "link failure" msg
  | Eval.Link (at, msg) -> error at "link failure" msg
  | Eval.Trap (at, msg) -> error at "runtime trap" msg
  | Eval.Exhaustion (at, msg) -> error at "resource exhaustion" msg
  | Eval.Crash (at, msg) -> error at "runtime crash" msg
  | Encode.Code (at, msg) -> error at "encoding error" msg

let parse_file name run =
  let ic = In_channel.create name in
  try
    let lexbuf = Lexing.from_channel ic in
    let success = input_from (fun _ ->
        let var_opt, def = Parse.parse name lexbuf Parse.Module in
        [(var_opt, def)]) run in
    In_channel.close ic;
    success
  with exn -> In_channel.close ic; raise exn

let parse_string str run =
  let lexbuf = Lexing.from_string str in
    let success = input_from (fun _ ->
        let var_opt, def = Parse.parse "foo.wat" lexbuf Parse.Module in
        [(var_opt, def)]) run in
  success

let cfgs : (Cfg.t IntMap.t) ref = ref IntMap.empty
let nglobals : int ref = ref (-1)
let initialize (program : string) : unit =
  let run (l : (Script.var option * Script.definition) list) =
    List.iter l ~f:(fun (_, def) ->
        match def.it with
        | Script.Textual m ->
          let store = Store.init m in
          trace (Printf.sprintf "nglobals: %d\n" (List.length store.globals));
          nglobals := List.length store.globals;
          cfgs := IntMap.of_alist_exn (List.mapi store.funcs ~f:(fun faddr _ -> (faddr, Cfg_builder.build faddr store)))
        | Script.Encoded _ -> failwith "unsupported"
        | Script.Quoted _ -> failwith "unsupported"
      ) in
  trace (Printf.sprintf "Success? %b" (parse_string program run))

