open Core_kernel

module T = struct
  type t = {
    idx : int;
    name : string option;
    typ : (Type.t list * Type.t list);
    module_: Module_inst.t;
    code: Func.t;
  }
  [@@deriving sexp, compare]
end
include T
let of_wasm (m : Wasm.Ast.module_) (minst : Module_inst.t) (index : int) (f : Wasm.Ast.func) : t =
  let name = (List.find_map m.it.exports ~f:(fun x ->
      match x.it.edesc.it with
      | FuncExport v when (Int32.to_int_exn v.it) = index -> Some (Wasm.Ast.string_of_name x.it.name)
      | _ -> None
    ))
  in
  match Wasm.Ast.func_type_for m f.it.ftype with
  | FuncType (input, output) -> {
      idx = index;
      name = name;
      typ = (List.map input ~f:Type.of_wasm, List.map output ~f:Type.of_wasm);
      module_ = minst;
      code = Func.of_wasm m f
    }
let to_string (f : t) : string =
  Printf.sprintf "Function %s (%s -> %s):\nCode: %s"
    (match f.name with
     | Some n -> n
     | None -> "<noname>")
    (String.concat ~sep:", " (List.map (fst f.typ) ~f:Type.to_string))
    (String.concat ~sep:", " (List.map (snd f.typ) ~f:Type.to_string))
    (Func.to_string f.code)
