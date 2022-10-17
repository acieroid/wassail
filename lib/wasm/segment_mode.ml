open Core

type t =
  | Passive
  | Active of { table_index: Int32.t; offset: unit Instr.t list }
  | Declarative
[@@deriving sexp, compare, equal]

let of_wasm (m: Wasm.Ast.module_) (label_maker : unit -> Instr.Label.t) (t : Wasm.Ast.segment_mode) : t =
  match t.it with
  | Passive -> Passive
  | Active { index; offset } ->
     Active {
         table_index = index.it;
         offset = Instr.seq_of_wasm m label_maker offset.it;
       }
  | Declarative -> Declarative

let offset (mode : t) : unit Instr.t list =
  match mode with
  | Passive -> []
  | Active { offset; _ } -> offset
  | Declarative -> []
