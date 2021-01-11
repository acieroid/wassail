open Core_kernel

module T = struct
  type t = {
    idx : int;
    name : string option;
    type_idx : Int32.t;
    typ : (Type.t list * Type.t list);
    module_: Module_inst.t;
    code: Func.t;
  }
  [@@deriving sexp, compare, equal]
end
include T
let of_wasm (m : Wasm.Ast.module_) (minst : Module_inst.t) (fid : int) (f : Wasm.Ast.func) (nglobals : int) : t =
  let name = (List.find_map m.it.exports ~f:(fun x ->
      match x.it.edesc.it with
      | FuncExport v when (Int32.to_int_exn v.it) = fid -> Some (Wasm.Ast.string_of_name x.it.name)
      | _ -> None
    ))
  in
  match Wasm.Ast.func_type_for m f.it.ftype with
  | FuncType (input, output) -> {
      idx = fid;
      name = name;
      type_idx = f.it.ftype.it;
      typ = (List.map input ~f:Type.of_wasm, List.map output ~f:Type.of_wasm);
      module_ = minst;
      code = Func.of_wasm m fid f (List.length input) nglobals (List.length output)
    }
let to_string (f : t) : string =
  Printf.sprintf "Function %s (%s -> %s):\nCode: %s"
    (match f.name with
     | Some n -> n
     | None -> "<noname>")
    (String.concat ~sep:", " (List.map (fst f.typ) ~f:Type.to_string))
    (String.concat ~sep:", " (List.map (snd f.typ) ~f:Type.to_string))
    (Func.to_string f.code)

let nargs (f : t) : int =
  List.length (fst f.typ)

let nlocals (f : t) : int =
  List.length f.code.locals

let nreturns (f : t) : int =
  List.length (snd f.typ)
