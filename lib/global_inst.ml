(*open Core_kernel
open Wasm

module T = struct
  type t = {
    value : Value.t;
    mut : bool;
    typ : Type.t
  }
  [@@deriving sexp, compare]
end
include T

let of_wasm (g : Ast.global) : t =
  { value = Value.of_wasm (match g.it.value.it with
        | [] -> failwith "Undefined global"
        | [v] -> begin match v.it with
            | Ast.Const l -> l.it
            | _ -> failwith "Unsupported non-const global instaciation"
          end
        | _ -> failwith "Unsupported global instanciation with multiple instructions");
    mut = (match g.it.gtype with
      | Types.GlobalType (_, Types.Immutable) -> false
      | Types.GlobalType (_, Types.Mutable) -> true);
    typ = (match g.it.gtype with
        | Types.GlobalType (t, _) -> Type.of_wasm t)
  }
let join (g1 : t) (g2 : t) : t =
  assert Stdlib.(g1.mut = g2.mut && g1.typ = g2.typ);
  { value = Value.join g1.value g2.value; mut = g1.mut; typ = g1.typ }
*)
