open Core

type t = {
    idx: Int32.t; (* The index of this elem segment *)
    etype: Ref_type.t; (* The types contained in this segment *)
    einit: unit Instr.t list list; (* The initial elements *)
    emode: Segment_mode.t; (* The mode of the segment *)
}
[@@deriving sexp, compare, equal]

let of_wasm (m : Wasm.Ast.module_) (idx : Int32.t) (e : Wasm.Ast.elem_segment) : t =
  let label_maker = Instr.Label.maker (Instr.Label.Elem idx) in
  {
    idx;
    etype = Ref_type.of_wasm e.it.etype;
    einit = List.map ~f:(fun instrs -> Instr.seq_of_wasm m label_maker instrs.it) e.it.einit;
    emode = Segment_mode.of_wasm m label_maker e.it.emode;
  }
