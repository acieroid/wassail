open Core_kernel
open Wasm

include Helpers

module Analysis_helpers = Analysis_helpers

module Logging = Logging

module Wasm_module = Wasm_module
module Instr = Instr
module Var = Var

module Basic_block = Basic_block
module Cfg = Cfg
module Cfg_builder = Cfg_builder

module Call_graph = Call_graph

module Inter = Inter
module Intra = Intra

module Spec_inference = Spec_inference

module Relational = Relational
module Taint = Taint
module Reltaint = Reltaint

let cfgs : ('a Cfg.t IntMap.t) ref = ref IntMap.empty
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
          nglobals := wasm_mod.nglobals;
          cfgs := IntMap.of_alist_exn (List.mapi wasm_mod.funcs ~f:(fun faddr _ -> (faddr, Cfg_builder.build faddr wasm_mod)));
          cfg_deps := IntMap.mapi !cfgs ~f:(fun ~key:_ ~data:cfg -> Cfg.dependencies cfg)
        | Script.Encoded _ -> failwith "unsupported"
        | Script.Quoted _ -> failwith "unsupported"
      ) in
  (parse_string program run)
