open Core_kernel
open Helpers
    
type t = Taint_domain.t

let to_string (s : t) : string = Taint_domain.to_string s

let initial_summaries (_cfg : Cfg.t IntMap.t) (_module_ : Wasm_module.t) (_typ : [`Bottom | `Top]) : t IntMap.t =
  failwith "TODO: taint_summary's initial summaries"
