open Core_kernel

type t = {
  locals : Type.t list;
  body : unit Instr.t list;
}
[@@deriving sexp, compare, equal]

let of_wasm (m : Wasm.Ast.module_) (idx : Int32.t) (f : Wasm.Ast.func) : t = {
  locals = List.map f.it.locals ~f:Type.of_wasm;
  body = Instr.seq_of_wasm m (Instr.Label.maker (Instr.Label.Function idx)) f.it.body;
}

let to_string (f : t) : string =
  Printf.sprintf "locals: %s\ncode:\n%s" (Type.list_to_string f.locals) (Instr.list_to_string f.body (fun _ -> "") ~sep:"\n")
