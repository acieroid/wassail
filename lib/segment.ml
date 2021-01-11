open Core_kernel

module type SegmentType = sig
  type t
  [@@deriving sexp, compare, equal]
end

module Make(ST: SegmentType) = struct
  type t = {
    index : int;
    offset : unit Instr.t list;
    init : ST.t;
  }
  [@@deriving sexp, compare, equal]
end

module Data = struct
  type t = string
  [@@deriving sexp, compare, equal]
end

module DataSegment = struct
  include Make(Data)
  let of_wasm (module_ : Wasm.Ast.module_) (d : string Wasm.Ast.segment) : t =
    { index = Int32.to_int_exn d.it.index.it;
      offset = List.map d.it.offset.it ~f:(Instr.of_wasm module_);
      init = d.it.init }
end
                
