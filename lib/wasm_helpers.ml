open Core_kernel
open Wasm

(** Returns the (optional) type of a block *)
let type_of_block (bt : Ast.block_type) : Type.t option = match bt with
  | Ast.VarBlockType v -> failwith (Printf.sprintf "TODO: arity_of_block: var %s" (Int32.to_string v.it))
  | Ast.ValBlockType None -> None
  | Ast.ValBlockType (Some t) -> Some (Type.of_wasm t)

(** Returns the arity of a block *)
let arity_of_block (bt : Ast.block_type) : int * int = match type_of_block bt with
  | Some _ -> (0, 1)
  | None -> (0, 0)

(** Returns the arity of a function *)
let arity_of_fun_type (m : Ast.module_) (ft : Ast.var) : int * int =
    match Ast.func_type_for m ft with
      | FuncType (i, o) ->
        List.length i, List.length o

(** Returns the number of imports of a module *)
let nimports (m : Ast.module_) : int =
  List.count m.it.imports ~f:(fun import -> match import.it.idesc.it with
      | FuncImport _ -> true
      | _ -> false)

(** Returns the arity of a function *)
let arity_of_fun (m : Ast.module_) (f : Ast.var) : int * int =
  let n = Int32.of_int_exn (nimports m) in
  if Int32.(f.it < n) then
    (* imported function, arity is in import desc *)
    match (Lib.List32.nth m.it.imports f.it).it.idesc.it with
    | FuncImport v -> arity_of_fun_type m v
    | TableImport _ -> failwith "table import not supported"
    | MemoryImport _ -> failwith "memory import not supported"
    | GlobalImport _ -> failwith "global import not supported"
  else
    (* defined function, get arity from function list *)
    arity_of_fun_type m (Lib.List32.nth m.it.funcs Int32.(f.it - n)).it.ftype
