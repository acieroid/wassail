open Core

module type SegmentType = sig
  type t
  [@@deriving sexp, compare, equal]

  type wasm_t

  val of_wasm : wasm_t -> t
end

module Make(ST: SegmentType) = struct
  type t = {
    index : Int32.t;
    offset : unit Instr.t list;
    init : ST.t;
  }
  [@@deriving sexp, compare, equal]

  let of_wasm (module_ : Wasm.Ast.module_) (idx : Int32.t) (d : ST.wasm_t Wasm.Ast.segment) : t =
    { index = d.it.index.it;
      offset = Instr.seq_of_wasm module_ (Instr.Label.maker (Instr.Label.Function idx)) d.it.offset.it;
      init = ST.of_wasm d.it.init}
end

module Data = struct
  type t = string
  [@@deriving sexp, compare, equal]

  type wasm_t = string
  let of_wasm (d : wasm_t) : t = d
end

module DataSegment = struct
  include Make(Data)
end

module Elem = struct
  type t = Int32.t list
  [@@deriving sexp, compare, equal]

  type wasm_t = Wasm.Ast.var list
  let of_wasm (e : wasm_t) : t = List.map e ~f:(fun v -> v.it)
end

module ElemSegment = struct
  include Make(Elem)
end
