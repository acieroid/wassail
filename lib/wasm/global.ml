open Core

type mutability = Mutable | Immutable
[@@deriving sexp, compare, equal]

type global_type = {
  typ : Type.t;
  mutability : mutability
}
[@@deriving sexp, compare, equal]

let global_type_of_wasm (gtype : Wasm.Types.global_type) : global_type = match gtype with
  | GlobalType (t, mut) -> {
      typ = Type.of_wasm t;
      mutability = match mut with
        | Mutable -> Mutable
        | Immutable -> Immutable
    }

type t = {
  gtype : global_type;
  value : unit Instr.t list;
}
[@@deriving sexp, compare, equal]

let of_wasm (module_ : Wasm.Ast.module_) (idx : Int32.t) (g : Wasm.Ast.global) : t = {
  gtype = global_type_of_wasm g.it.gtype;
  value = Instr.seq_of_wasm module_ (Instr.Label.maker (Instr.Label.Function idx)) g.it.ginit.it;
}

let of_wasm_import (t : Wasm.Types.global_type) : t = {
  gtype = global_type_of_wasm t;
  value = []
};
