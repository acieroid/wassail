open Core_kernel
open Wasm

module T = struct
  (** Vstack specification, it is attached to all instructions and corresponds
     to the expected vstack after executing the instruction *)
  type vstack_spec = string list
  [@@deriving sexp, compare]

  (** A variable that will be used by the abstract domain *)
  type var = string
  [@@deriving sexp, compare]
  type vars = var list
  [@@deriving sexp, compare]
  type block_vars = var list * var list * var option
  [@@deriving sexp, compare]

  (** Data instructions *)
  type data =
    | Nop of vstack_spec
    | Drop of vstack_spec
    | Select of vstack_spec * var
    | MemorySize of vstack_spec * var
    | Const of Prim_value.t * vstack_spec * var
    | Binary of Binop.t * vstack_spec * var
    | Compare of Relop.t * vstack_spec * var
    | Test of Testop.t * vstack_spec * var
    | LocalGet of Var.t * vstack_spec * var
    | LocalSet of Var.t * vstack_spec * var
    | LocalTee of Var.t * vstack_spec * var
    | GlobalGet of Var.t * vstack_spec * var
    | GlobalSet of Var.t * vstack_spec * var
    | Load of Memoryop.t * vstack_spec * vars
    | Store of Memoryop.t * vstack_spec * vars
  (** Control instructions *)
  and control =
    | Block of t list * vstack_spec * vstack_spec * block_vars
    | Loop of t list * vstack_spec * vstack_spec * block_vars
    | If of t list * t list * vstack_spec * vstack_spec * block_vars
    | Call of Var.t * vstack_spec * var option
    | CallIndirect of Var.t * vstack_spec * var option
    | Br of Var.t * vstack_spec
    | BrIf of Var.t * vstack_spec
    | Return
    | Unreachable
  (** All instructions *)
  and t =
    | Data of data
    | Control of control
  [@@deriving sexp, compare]
end
include T

exception UnsupportedInstruction of t
let vstack_spec (instr : t) : vstack_spec = match instr with
  | Data d -> begin match d with
      | Nop v
      | Drop v
      | Select (v, _)
      | MemorySize (v, _)
      | Const (_, v, _)
      | Binary (_, v, _)
      | Compare (_, v, _)
      | Test (_, v, _)
      | LocalGet (_, v, _)
      | LocalSet (_, v, _)
      | LocalTee (_, v, _)
      | GlobalGet (_, v, _)
      | GlobalSet (_, v, _)
      | Load (_, v, _)
      | Store (_, v, _) -> v
    end
  | Control c -> begin match c with
      | Block (_, v, _, _)
      | Loop (_, v, _, _)
      | If (_, _, v, _, _)
      | Call (_, v, _)
      | CallIndirect (_, v, _)
      | Br (_, v)
      | BrIf (_, v) -> v
      | Return | Unreachable -> []
    end

let vstack_block_spec (instr : t) : vstack_spec = match instr with
  | Data d -> vstack_spec (Data d)
  | Control c -> begin match c with
      | Block (_, _, v, _)
      | Loop (_, _, v, _)
      | If (_, _, _, v, _) -> v
      | _ -> vstack_spec (Control c)
    end

let vars (instr : t) : vars = match instr with
  | Data d -> begin match d with
      | Nop _ | Drop _ -> []
      | Select (_, v)
      | MemorySize (_, v)
      | Const (_, _, v)
      | Binary (_, _, v)
      | Compare (_, _, v)
      | Test (_, _, v)
      | LocalGet (_, _, v)
      | LocalSet (_, _, v)
      | LocalTee (_, _, v)
      | GlobalGet (_, _, v)
      | GlobalSet (_, _, v) -> [v]
      | Load (_, _, vs)
      | Store (_, _, vs) -> vs
    end
  | Control c -> begin match c with
      | Block (_, _, _, _)
      | Loop (_, _, _, _)
      | If (_, _, _, _, _) -> []
      | Call (_, _, v) -> Option.to_list v
      | CallIndirect (_, _, _)
      | Br (_, _) | BrIf (_, _)
      | Return | Unreachable -> []
    end

let data_to_string (instr : data) : string =
  match instr with
     | Nop _ -> "nop"
     | Drop _ -> "drop"
     | Select _ -> "select"
     | MemorySize _ -> "memory_size"
     | Const (v, _, _) -> Printf.sprintf "const %s" (Prim_value.to_string v)
     | Binary (b, _, _) -> Printf.sprintf "binary %s" (Binop.to_string b)
     | Compare (r, _, _) -> Printf.sprintf "compare %s" (Relop.to_string r)
     | Test (t, _, _) -> Printf.sprintf "test %s" (Testop.to_string t)
     | LocalGet (v, _, _) -> Printf.sprintf "local.get %d" v
     | LocalSet (v, _, _) -> Printf.sprintf "local.set %d" v
     | LocalTee (v, _, _) -> Printf.sprintf "local.tee %d" v
     | GlobalGet (v, _, _) -> Printf.sprintf "global.get %d" v
     | GlobalSet (v, _, _) -> Printf.sprintf "global.set %d" v
     | Load (op, _, _) -> Printf.sprintf "load %s" (Memoryop.to_string op)
     | Store (op, _, _) -> Printf.sprintf "store %s" (Memoryop.to_string op)
let rec control_to_string ?sep:(sep : string = "\n") ?indent:(i : int = 0) (instr : control) : string =
  match instr with
  | Call (v, _, _) -> Printf.sprintf "call %d" v
  | CallIndirect (v, _, _) -> Printf.sprintf "call_indirect %d" v
  | Br (b, _) -> Printf.sprintf "br %d" b
  | BrIf (b, _) -> Printf.sprintf "brif %d" b
  | Return -> "return"
  | Unreachable -> "unreachable"
  | Block (instrs, _, _, _) -> Printf.sprintf "block%s%s" sep (list_to_string instrs ~indent:(i+2) ~sep:sep)
  | Loop (instrs, _, _, _) -> Printf.sprintf "loop%s%s" sep (list_to_string instrs ~indent:(i+2) ~sep:sep)
  | If (instrs1, instrs2, _, _, _) -> Printf.sprintf "if%s%s%selse%s%s" sep
                               (list_to_string instrs1 ~indent:(i+2) ~sep:sep) sep sep
                               (list_to_string instrs2 ~indent:(i+2) ~sep:sep)
and to_string ?sep:(sep : string = "\n") ?indent:(i : int = 0) (instr : t) : string =
  Printf.sprintf "%s%s" (String.make i ' ')
    (match instr with
     | Data instr -> data_to_string instr
     | Control instr -> control_to_string instr ~sep:sep ~indent:i)
and list_to_string ?indent:(i : int = 0) ?sep:(sep : string = ", ") (l : t list) : string =
  String.concat ~sep:sep (List.map l ~f:(to_string ?sep:(Some sep) ?indent:(Some i)))

let control_to_short_string (instr : control) : string =
  match instr with
  | Call (v, _, _) -> Printf.sprintf "call %d" v
  | CallIndirect (v, _, _) -> Printf.sprintf "call_indirect %d" v
  | Br (b, _) -> Printf.sprintf "br %d" b
  | BrIf (b, _) -> Printf.sprintf "brif %d" b
  | Return -> "return"
  | Unreachable -> "unreachable"
  | Block _ -> "block"
  | Loop _ -> "loop"
  | If _ -> "if"

let vstack_pop (vstack : string list) : string list = match vstack with
  | [] -> failwith "incorrect vstack manipulation when parsing instructions"
  | _ :: rest -> rest

let arity_of_block (bt : Ast.block_type) : int * int = match bt with
  | Ast.VarBlockType v -> failwith (Printf.sprintf "TODO: arity_of_block: var %s" (Int32.to_string v.it))
  | Ast.ValBlockType None -> (0, 0)
  | Ast.ValBlockType (Some _t) -> (0, 1) (* TODO: double check that this is what is intended *)

let arity_of_fun_type (m : Ast.module_) (ft : Ast.var) : int * int =
    match Ast.func_type_for m ft with
      | FuncType (i, o) ->
        List.length i, List.length o

let nimports (m : Ast.module_) : int =
  List.count m.it.imports ~f:(fun import -> match import.it.idesc.it with
      | FuncImport _ -> true
      | _ -> false)

let arity_of_fun (m : Ast.module_) (f : Ast.var) : int * int =
  let n = Int32.of_int_exn (nimports m) in
  if Int32.(f.it < n) then
    (* imported function, arity is in import desc *)
    match (Lib.List32.nth m.it.imports f.it).it.idesc.it with
    | FuncImport v -> arity_of_fun_type m v
    | _ -> failwith "not supported"
  else
    (* defined function, get arity from function list *)
    arity_of_fun_type m (Lib.List32.nth m.it.funcs Int32.(f.it - n)).it.ftype

let counter : int ref = ref 0
let alloc_var (_i : Ast.instr) (name : string) : string =
  let v = Printf.sprintf "%s_%d" name !counter in
  Printf.printf "alloc var %s\n" name;
  counter := !counter + 1;
  v

let rec of_wasm (m : Ast.module_) (fid : int) (i : Ast.instr) (vstack : string list) (nlocals : int) (nglobals : int) : t =
  let block_new_vars (name : string) (arity_out : int) : block_vars =
    (List.init nlocals ~f:(fun n -> alloc_var i (Printf.sprintf "%s_l%d" name n)),
     List.init nglobals ~f:(fun n -> alloc_var i (Printf.sprintf "%s_g%d" name n)),
     (if arity_out = 0 then None else Some (alloc_var i (Printf.sprintf "%s_ret" name)))) in
  match i.it with
  | Ast.Nop -> Data (Nop vstack)
  | Ast.Drop -> Data (Drop (List.drop vstack 1))
  | Ast.Block (st, instrs) ->
    let (arity_in, arity_out) = arity_of_block st in
    assert (arity_in = 0); (* what does it mean to have arity_in > 0? *)
    assert (arity_out <= 1);
    Printf.printf "[%d] block arity: %d, %d\n" fid arity_in arity_out;
    (* Create one var per local and global, and one extra var if arity_out is 1 *)
    let (body, _) = seq_of_wasm m fid instrs vstack nlocals nglobals in
    let (_, _, ret) as vars = block_new_vars "block" arity_out in
    Control (Block (body, vstack, (Option.to_list ret) @ vstack, vars))
  | Ast.Const lit ->
    let var = alloc_var i "const" in
    Data (Const (Prim_value.of_wasm lit.it, var :: vstack, var))
  | Ast.Binary bin ->
    let var = alloc_var i "bin" in
    Data (Binary (Binop.of_wasm bin, var :: (List.drop vstack 2), var))
  | Ast.Compare rel ->
    let var = alloc_var i "cmp" in
    Data (Compare (Relop.of_wasm rel, var :: (List.drop vstack 2), var))
  | Ast.LocalGet l ->
    let var = alloc_var i "local.get" in
    Data (LocalGet (Var.of_wasm l, var :: vstack, var))
  | Ast.LocalSet l ->
    (* The new variable will be used for the new value of the local *)
    let var = alloc_var i "local.set" in
    Data (LocalSet (Var.of_wasm l, List.drop vstack 1, var))
  | Ast.LocalTee l ->
    let var = alloc_var i "local.tee" in
    Data (LocalTee (Var.of_wasm l, vstack, var))
  | Ast.BrIf label ->
    Control (BrIf (Var.of_wasm label, List.drop vstack 1))
  | Ast.Br label ->
    Control (Br (Var.of_wasm label, vstack))
  | Ast.Call f ->
    let (arity_in, arity_out) = arity_of_fun m f in
    assert (arity_out <= 1);
    if arity_out = 0 then
      Control (Call (Var.of_wasm f, List.drop vstack arity_in, None))
    else
      let var = alloc_var i "call" in
      Control (Call (Var.of_wasm f, var :: (List.drop vstack arity_in), Some var))
  | Ast.Return ->
    Control Return (* TODO: in practice, return only keeps the necessary number of values on the vstack *)
  | Ast.Unreachable ->
    Control Unreachable
  | Ast.Select ->
    let var = alloc_var i "select" in
    Data (Select (var :: (List.drop vstack 3), var))
  | Ast.Loop (st, instrs) ->
    let (arity_in, arity_out) = arity_of_block st in
    Printf.printf "[%d] loop arity: %d, %d\n" fid arity_in arity_out;
    assert (arity_in = 0); (* TODO *)
    assert (arity_out <= 1); (* TODO: support any arity out? *)
    let (body, _) = seq_of_wasm m fid instrs vstack nlocals nglobals in
    let (_, _, ret) as vars = block_new_vars "loop" arity_out in
    Control (Loop (body, vstack, (Option.to_list ret) @ vstack, vars))
  | Ast.If (st, instrs1, instrs2) ->
    (* drop the condition *)
    let _, vstack' = Vstack.pop vstack in
    let (arity_in, arity_out) = arity_of_block st in
    assert (arity_in = 0);
    assert (arity_out <= 1);
    Printf.printf "[%d] if arity: %d, %d\n" fid arity_in arity_out;
    let (body1, _vstack1) = seq_of_wasm m fid instrs1 vstack' nlocals nglobals in
    let (body2, _vstack2) = seq_of_wasm m fid instrs2 vstack' nlocals nglobals in
    let (_, _, ret) as vars = block_new_vars "if" arity_out in
    Control (If (body1, body2, vstack', (Option.to_list ret) @ vstack', vars))
  | Ast.BrTable (_vs, _v) -> failwith "br_table unsupported"
  | Ast.CallIndirect f ->
    let (arity_in, arity_out) = arity_of_fun_type m f in
    assert (arity_out <= 1);
    if arity_out = 0 then
      Control (CallIndirect (Var.of_wasm f, List.drop vstack arity_in, None))
    else
      let var = alloc_var i "call_indirect" in
      Control (CallIndirect (Var.of_wasm f, var :: List.drop vstack arity_in, Some var))
  | Ast.GlobalGet g ->
    let var = alloc_var i "global.get" in
    Data (GlobalGet (Var.of_wasm g, var :: vstack, var))
  | Ast.GlobalSet g ->
    (* The new variable will be used for the new value of the global *)
    let var = alloc_var i "global.set" in
    Data (GlobalSet (Var.of_wasm g, List.drop vstack 1, var))
  | Ast.Load op ->
    let var_ret = alloc_var i "load" in
    let vars = [alloc_var i "load0"; alloc_var i "load1"; alloc_var i "load2"; alloc_var i "load3"] in
    Data (Load (Memoryop.of_wasm_load op, var_ret :: (List.drop vstack 1), var_ret :: vars))
  | Ast.Store op ->
    (* Allocate 4 variables to represent 4 addresses where the i32 value is stored *)
    (* TODO: also support i64, and support load8 which only requires one value *)
    let vars = [alloc_var i "store0"; alloc_var i "store1"; alloc_var i "store2"; alloc_var i "store3"] in
    Data (Store (Memoryop.of_wasm_store op, List.drop vstack 2, vars))
  | Ast.MemorySize ->
    let var = alloc_var i "memory.size" in
    Data (MemorySize (var :: vstack, var))
  | Ast.MemoryGrow -> failwith "memory_grow unsupported"
  | Ast.Test op ->
    let var = alloc_var i "test" in
    Data (Test (Testop.of_wasm op, var :: (List.drop vstack 1), var))
  | Ast.Convert _op -> failwith "convert unsupported"
  | Ast.Unary _op -> failwith "unary unsupported"
and seq_of_wasm (m : Ast.module_) (fid : int) (is : Ast.instr list) (vstack : string list) (nlocals : int) (nglobals : int) : t list * string list =
  let (instrs, vstack) = List.fold_left is
    ~init:([], vstack)
    ~f:(fun (instrs, vstack) instr ->
        let i = of_wasm m fid instr vstack nlocals nglobals in
        let vstack' = vstack_block_spec i in
        (i :: instrs, vstack')) in
    List.rev instrs, vstack
