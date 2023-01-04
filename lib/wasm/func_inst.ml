open Core

module T = struct
  type t = {
    idx : Int32.t;
    name : string option;
    type_idx : Int32.t;
    typ : (Type.t list * Type.t list);
    code: Func.t;
  }
  [@@deriving sexp, compare, equal]
end
include T
let of_wasm (m : Wasm.Ast.module_) (fid : Int32.t) (f : Wasm.Ast.func) : t =
  let name = (List.find_map m.it.exports ~f:(fun x ->
      match x.it.edesc.it with
      | FuncExport v when Int32.(v.it = fid) -> Some (Wasm.Ast.string_of_name x.it.name)
      | _ -> None
    ))
  in
  match Wasm.Ast.func_type_for m f.it.ftype with
  | FuncType (input, output) -> {
      idx = fid;
      name = name;
      type_idx = f.it.ftype.it;
      typ = (List.map input ~f:Type.of_wasm, List.map output ~f:Type.of_wasm);
      code = Func.of_wasm m fid f
    }

let to_string (f : t) : string =
  Printf.sprintf "Function %s (%s -> %s):\nCode: %s"
    (match f.name with
     | Some n -> n
     | None -> "<noname>")
    (String.concat ~sep:", " (List.map (fst f.typ) ~f:Type.to_string))
    (String.concat ~sep:", " (List.map (snd f.typ) ~f:Type.to_string))
    (Func.to_string f.code)

let to_wasm_string (f : t) =
  let buf = Buffer.create 8192 in
  let put (s : string) = Buffer.add_string buf s in
  put (Printf.sprintf "  (func (;%ld;) (type %ld)" f.idx f.type_idx);
  if not (List.is_empty (fst f.typ)) then begin
      put (Printf.sprintf " (param %s)"
             (String.concat ~sep:" " (List.map (fst f.typ) ~f:Type.to_string)))
    end;
  if not (List.is_empty (snd f.typ)) then begin
      put (Printf.sprintf " (result %s)"
             (String.concat ~sep:" " (List.map (snd f.typ) ~f:Type.to_string)))
    end;
  put "\n";
  if not (List.is_empty f.code.locals) then begin
      put (Printf.sprintf "    (local %s)\n" (String.concat ~sep:" " (List.map f.code.locals ~f:Type.to_string)))
    end;
  put (Instr.list_to_string f.code.body ?indent:(Some 4) ?sep:(Some "\n") (fun () -> ""));
  put ")\n";
  Buffer.contents buf

let nargs (f : t) : int =
  List.length (fst f.typ)

let nlocals (f : t) : int =
  List.length f.code.locals

let nreturns (f : t) : int =
  List.length (snd f.typ)
