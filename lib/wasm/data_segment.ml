open Core

type t = {
    idx: Int32.t; (* The index of this data segment *)
    dinit: string; (* The initial data *)
    dmode: Segment_mode.t (* The mode of the segment *)
}
[@@deriving sexp, compare, equal]

let of_wasm (m : Wasm.Ast.module_) (idx : Int32.t) (d : Wasm.Ast.data_segment) : t =
  let label_maker = Instr.Label.maker (Instr.Label.Data idx) in
  {
    idx;
    dinit = d.it.dinit;
    dmode = Segment_mode.of_wasm m label_maker d.it.dmode;
  }
