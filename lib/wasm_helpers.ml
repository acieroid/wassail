open Core
open Wasm

(** Returns the (optional) type of a block *)
let type_of_block (m : Ast.module_) (bt : Ast.block_type) : (Type.t list * Type.t list) = match bt with
  | Ast.VarBlockType v -> begin match Ast.func_type_for m v with
      | Wasm.Types.FuncType (in_type, out_type) -> (List.map ~f:Type.of_wasm in_type, List.map ~f:Type.of_wasm out_type)
    end
  | Ast.ValBlockType None -> [], []
  | Ast.ValBlockType (Some t) -> [], [Type.of_wasm t]

(** Returns the arity of a block *)
let arity_of_block (m : Ast.module_) (bt : Ast.block_type) : int * int = match type_of_block m bt with
  | (in_types, out_types) -> (List.length in_types, List.length out_types)

(** Returns the arity of a function *)
let arity_and_type_of_fun_type (m : Ast.module_) (ft : Ast.var) : ((int * int) * (Type.t list * Type.t list)) =
    match Ast.func_type_for m ft with
      | FuncType (i, o) ->
        ((List.length i, List.length o),
           (List.map i ~f:Type.of_wasm, List.map o ~f:Type.of_wasm))

(** Returns the number of imports of a module *)
let nimports (m : Ast.module_) : int =
  List.count m.it.imports ~f:(fun import -> match import.it.idesc.it with
      | FuncImport _ -> true
      | _ -> false)

(** Returns the arity of a function *)
let arity_and_type_of_fun (m : Ast.module_) (f : Ast.var) : ((int * int) * (Type.t list * Type.t list)) =
  let n = Int32.of_int_exn (nimports m) in
  if Int32.(f.it < n) then
    (* imported function, arity is in import desc *)
    let rec iter (count : int32) (l : Ast.import list) = match l with
      | [] -> failwith (Printf.sprintf "Cannot find function %ld" f.it)
      | { it = { idesc = { it = FuncImport v; _ }; _}; _} :: _ when Int32.(count = f.it) -> arity_and_type_of_fun_type m v
      | { it = { idesc = { it = FuncImport _; _}; _}; _} :: rest -> iter Int32.(count + 1l) rest
      | _ :: rest -> iter count rest (* other imports don't count towards increasing function indices *)in
    iter 0l m.it.imports
  else
    (* defined function, get arity from function list *)
    arity_and_type_of_fun_type m (Lib.List32.nth m.it.funcs Int32.(f.it - n)).it.ftype
