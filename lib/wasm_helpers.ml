open Core_kernel
open Wasm

(** Returns the (optional) type of a block *)
let type_of_block (m : Ast.module_) (bt : Ast.block_type) : Type.t option = match bt with
  | Ast.VarBlockType v -> begin match Ast.func_type_for m v with
      | Wasm.Types.FuncType ([], []) -> None
      | Wasm.Types.FuncType ([], [t]) -> Some (Type.of_wasm t)
      | Wasm.Types.FuncType _ -> failwith "Unsupported block type with parameters or more than one return value"
    end
  | Ast.ValBlockType None -> None
  | Ast.ValBlockType (Some t) -> Some (Type.of_wasm t)

(** Returns the arity of a block *)
let arity_of_block (m : Ast.module_) (bt : Ast.block_type) : int * int = match type_of_block m bt with
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
    let rec iter (count : int32) (l : Ast.import list) : int * int = match l with
      | [] -> failwith (Printf.sprintf "Cannot find function %ld" f.it)
      | { it = { idesc = { it = FuncImport v; _ }; _}; _} :: _ when Int32.(count = f.it) -> arity_of_fun_type m v
      | { it = { idesc = { it = FuncImport _; _}; _}; _} :: rest -> iter Int32.(count + 1l) rest
      | _ :: rest -> iter count rest (* other imports don't count towards increasing function indice *)in
    iter 0l m.it.imports
  else
    (* defined function, get arity from function list *)
    arity_of_fun_type m (Lib.List32.nth m.it.funcs Int32.(f.it - n)).it.ftype
