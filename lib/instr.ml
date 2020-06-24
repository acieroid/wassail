open Core_kernel
open Wasm

module T = struct
  type arity = int * int
  [@@deriving sexp, compare]

  (** Data instructions *)
  type data =
    | Nop
    | Drop
    | Select
    | MemorySize
    | Const of Prim_value.t
    | Binary of Binop.t
    | Compare of Relop.t
    | Test of Testop.t
    | Convert of Convertop.t
    | LocalGet of Var.t
    | LocalSet of Var.t
    | LocalTee of Var.t
    | GlobalGet of Var.t
    | GlobalSet of Var.t
    | Load of Memoryop.t
    | Store of Memoryop.t
  (** Control instructions *)
  and control =
    | Block of arity * t list
    | Loop of arity * t list
    | If of arity * t list * t list
    | Call of arity * Var.t
    | CallIndirect of arity * Var.t
    | Br of Var.t
    | BrIf of Var.t
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

let data_to_string (instr : data) : string =
  match instr with
     | Nop -> "nop"
     | Drop -> "drop"
     | Select -> "select"
     | MemorySize -> "memory_size"
     | Const v -> Printf.sprintf "const %s" (Prim_value.to_string v)
     | Binary b -> Printf.sprintf "binary %s" (Binop.to_string b)
     | Compare r -> Printf.sprintf "compare %s" (Relop.to_string r)
     | Test t -> Printf.sprintf "test %s" (Testop.to_string t)
     | Convert t -> Printf.sprintf "cvt %s" (Convertop.to_string t)
     | LocalGet v -> Printf.sprintf "local.get %d" v
     | LocalSet v -> Printf.sprintf "local.set %d" v
     | LocalTee v -> Printf.sprintf "local.tee %d" v
     | GlobalGet v -> Printf.sprintf "global.get %d" v
     | GlobalSet v -> Printf.sprintf "global.set %d" v
     | Load op -> Printf.sprintf "load %s" (Memoryop.to_string op)
     | Store op -> Printf.sprintf "store %s" (Memoryop.to_string op)
let rec control_to_string ?sep:(sep : string = "\n") ?indent:(i : int = 0) (instr : control) : string =
  match instr with
  | Call (_, v) -> Printf.sprintf "call %d" v
  | CallIndirect (_, v) -> Printf.sprintf "call_indirect %d" v
  | Br b -> Printf.sprintf "br %d" b
  | BrIf b -> Printf.sprintf "brif %d" b
  | Return -> "return"
  | Unreachable -> "unreachable"
  | Block (_, instrs) -> Printf.sprintf "block%s%s" sep (list_to_string instrs ~indent:(i+2) ~sep:sep)
  | Loop (_, instrs) -> Printf.sprintf "loop%s%s" sep (list_to_string instrs ~indent:(i+2) ~sep:sep)
  | If (_, instrs1, instrs2) -> Printf.sprintf "if%s%s%selse%s%s" sep
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
  | Call (_, v) -> Printf.sprintf "call %d" v
  | CallIndirect (_, v) -> Printf.sprintf "call_indirect %d" v
  | Br b -> Printf.sprintf "br %d" b
  | BrIf b -> Printf.sprintf "brif %d" b
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
  counter := !counter + 1;
  v

(** Create an instruction from a WebAssembly instruction *)
let rec of_wasm (m : Ast.module_) (i : Ast.instr) : t =
  match i.it with
  | Ast.Nop -> Data Nop
  | Ast.Drop -> Data Drop
  | Ast.Block (st, instrs) ->
    let (arity_in, arity_out) = arity_of_block st in
    assert (arity_in = 0); (* what does it mean to have arity_in > 0? *)
    assert (arity_out <= 1);
    let body = seq_of_wasm m instrs in
    Control (Block ((arity_in, arity_out), body))
  | Ast.Const lit ->
    Data (Const (Prim_value.of_wasm lit.it))
  | Ast.Binary bin ->
    Data (Binary (Binop.of_wasm bin))
  | Ast.Compare rel ->
    Data (Compare (Relop.of_wasm rel))
  | Ast.LocalGet l ->
    Data (LocalGet (Var.of_wasm l))
  | Ast.LocalSet l ->
    Data (LocalSet (Var.of_wasm l))
  | Ast.LocalTee l ->
    Data (LocalTee (Var.of_wasm l))
  | Ast.BrIf label ->
    Control (BrIf (Var.of_wasm label))
  | Ast.Br label ->
    Control (Br (Var.of_wasm label))
  | Ast.Call f ->
    let (arity_in, arity_out) = arity_of_fun m f in
    assert (arity_out <= 1);
    Control (Call ((arity_in, arity_out), Var.of_wasm f))
  | Ast.Return ->
    Control Return
  | Ast.Unreachable ->
    Control Unreachable
  | Ast.Select ->
    Data Select
  | Ast.Loop (st, instrs) ->
    let (arity_in, arity_out) = arity_of_block st in
    assert (arity_in = 0); (* what does it mean to have arity_in > 0 for a loop? *)
    assert (arity_out <= 1); (* TODO: support any arity out? *)
    let body = seq_of_wasm m instrs in
    Control (Loop ((arity_in, arity_out), body))
  | Ast.If (st, instrs1, instrs2) ->
    let (arity_in, arity_out) = arity_of_block st in
    let body1 = seq_of_wasm m instrs1 in
    let body2 = seq_of_wasm m instrs2 in
    Control (If ((arity_in, arity_out), body1, body2))
  | Ast.BrTable (_vs, _v) -> failwith "unsupported: br_table"
  | Ast.CallIndirect f ->
    let (arity_in, arity_out) = arity_of_fun_type m f in
    assert (arity_out <= 1);
    Control (CallIndirect ((arity_in, arity_out), Var.of_wasm f))
  | Ast.GlobalGet g ->
    Data (GlobalGet (Var.of_wasm g))
  | Ast.GlobalSet g ->
    Data (GlobalSet (Var.of_wasm g))
  | Ast.Load op ->
    Data (Load (Memoryop.of_wasm_load op))
  | Ast.Store op ->
    Data (Store (Memoryop.of_wasm_store op))
  | Ast.MemorySize ->
    Data MemorySize
  | Ast.MemoryGrow -> failwith "memory_grow unsupported"
  | Ast.Test op ->
    Data (Test (Testop.of_wasm op))
  | Ast.Convert op ->
    Data (Convert (Convertop.of_wasm op))
  | Ast.Unary _op -> failwith "unary unsupported"
and seq_of_wasm (m : Ast.module_) (is : Ast.instr list) : t list =
  List.map is ~f:(of_wasm m)
