open Core_kernel
open Wasm

include Helpers
module Wasm_module = Wasm_module
module Cfg_builder = Cfg_builder
module Cfg = Cfg
module Domain = Domain
module Instr = Instr
module Inter_fixpoint = Inter_fixpoint
module Intra_fixpoint = Intra_fixpoint
module Basic_block = Basic_block
module Logging = Logging
module Transfer = Transfer
module Summary = Summary

let trace name = print_endline ("-- " ^ name)

let error at category msg =
  failwith (Printf.sprintf "Error: %s" (Source.string_of_region at ^ ": " ^ category ^ ": " ^ msg))

let input_from get_script run =
  try
    let script = get_script () in
    trace "Running...";
    run script;
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
  input_from (fun _ ->
      let var_opt, def = Parse.parse "foo.wat" lexbuf Parse.Module in
      [(var_opt, def)])
    run

let cfgs : (Cfg.t IntMap.t) ref = ref IntMap.empty
let module_ : (Wasm_module.t option) ref = ref None

(** A map of CFG dependencies. Each entry is of the form idx -> [idx1, idx2, ...] meaning that CFG idx depends on the analysis of CFGs idx1, idx2, etc. *)
let cfg_deps : (int list IntMap.t) ref = ref IntMap.empty
let nglobals : int ref = ref (-1)
let initialize (program : string) : unit =
  let run (l : (Script.var option * Script.definition) list) =
    List.iter l ~f:(fun (_, def) ->
        match def.it with
        | Script.Textual m ->
          let wasm_mod = Wasm_module.of_wasm m in
          module_ := Some wasm_mod;
          trace (Printf.sprintf "nglobals: %d\n" (List.length wasm_mod.globals));
          nglobals := List.length wasm_mod.globals;
          cfgs := IntMap.of_alist_exn (List.mapi wasm_mod.funcs ~f:(fun faddr _ -> (faddr, Cfg_builder.build faddr wasm_mod)));
          cfg_deps := IntMap.mapi !cfgs ~f:(fun ~key:_ ~data:cfg -> Cfg.dependencies cfg)
        | Script.Encoded _ -> failwith "unsupported"
        | Script.Quoted _ -> failwith "unsupported"
      ) in
  (parse_string program run)
