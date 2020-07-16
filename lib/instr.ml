open Core_kernel
open Wasm

module T = struct
  type label = int
  [@@deriving sexp, compare, equal]

  type 'a labelled = {
    instr : 'a;
    label : label
  }
  [@@deriving sexp, compare, equal]

  type arity = int * int
  [@@deriving sexp, compare, equal]

  (** Data instructions *)
  type data =
    | Nop
    | Drop
    | Select
    | MemorySize | MemoryGrow
    | Const of Prim_value.t
    | Unary of Unop.t
    | Binary of Binop.t
    | Compare of Relop.t
    | Test of Testop.t
    | Convert of Convertop.t
    | LocalGet of int
    | LocalSet of int
    | LocalTee of int
    | GlobalGet of int
    | GlobalSet of int
    | Load of Memoryop.t
    | Store of Memoryop.t
  (** Control instructions *)
  and control =
    | Block of arity * t list
    | Loop of arity * t list
    | If of arity * t list * t list
    | Call of arity * int
    | CallIndirect of arity * int
    | Br of int
    | BrIf of int
    | BrTable of int list * int
    | Return
    | Unreachable
  (** All instructions *)
  and t =
    | Data of data labelled
    | Control of control labelled
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
     | MemoryGrow -> "memory_grow"
     | Const v -> Printf.sprintf "const %s" (Prim_value.to_string v)
     | Binary b -> Printf.sprintf "binary %s" (Binop.to_string b)
     | Unary u -> Printf.sprintf "unary %s" (Unop.to_string u)
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
  | BrTable (t, b) -> Printf.sprintf "br_table %s %d" (String.concat ~sep:" " (List.map t ~f:(Printf.sprintf "%d"))) b
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
     | Data instr -> data_to_string instr.instr
     | Control instr -> control_to_string instr.instr ~sep:sep ~indent:i)
and list_to_string ?indent:(i : int = 0) ?sep:(sep : string = ", ") (l : t list) : string =
  String.concat ~sep:sep (List.map l ~f:(to_string ?sep:(Some sep) ?indent:(Some i)))

let control_to_short_string (instr : control) : string =
  match instr with
  | Block _ -> "block"
  | Loop _ -> "loop"
  | If _ -> "if"
  | _ -> control_to_string instr

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

let counter : label ref = ref 0
let new_label () : label =
  let v = !counter in
  counter := !counter + 1;
  v

let data_labelled (d : data) : t =
  Data { instr = d; label = new_label () }
let control_labelled (c : control) : t =
  Control { instr = c; label = new_label () }

(** Create an instruction from a WebAssembly instruction *)
let rec of_wasm (m : Ast.module_) (i : Ast.instr) : t =
  match i.it with
  | Ast.Nop -> data_labelled Nop
  | Ast.Drop -> data_labelled Drop
  | Ast.Block (st, instrs) ->
    let (arity_in, arity_out) = arity_of_block st in
    assert (arity_in = 0); (* what does it mean to have arity_in > 0? *)
    assert (arity_out <= 1);
    let body = seq_of_wasm m instrs in
    control_labelled (Block ((arity_in, arity_out), body))
  | Ast.Const lit ->
    data_labelled (Const (Prim_value.of_wasm lit.it))
  | Ast.Binary bin ->
    data_labelled (Binary (Binop.of_wasm bin))
  | Ast.Compare rel ->
    data_labelled (Compare (Relop.of_wasm rel))
  | Ast.LocalGet l ->
    data_labelled (LocalGet (Index.of_wasm l))
  | Ast.LocalSet l ->
    data_labelled (LocalSet (Index.of_wasm l))
  | Ast.LocalTee l ->
    data_labelled (LocalTee (Index.of_wasm l))
  | Ast.BrIf label ->
    control_labelled (BrIf (Index.of_wasm label))
  | Ast.Br label ->
    control_labelled (Br (Index.of_wasm label))
  | Ast.BrTable (table, label) ->
    control_labelled (BrTable (List.map table ~f:Index.of_wasm, Index.of_wasm label))
  | Ast.Call f ->
    let (arity_in, arity_out) = arity_of_fun m f in
    assert (arity_out <= 1);
    control_labelled (Call ((arity_in, arity_out), Index.of_wasm f))
  | Ast.Return ->
    control_labelled (Return)
  | Ast.Unreachable ->
    control_labelled (Unreachable)
  | Ast.Select ->
    data_labelled (Select)
  | Ast.Loop (st, instrs) ->
    let (arity_in, arity_out) = arity_of_block st in
    assert (arity_in = 0); (* what does it mean to have arity_in > 0 for a loop? *)
    assert (arity_out <= 1); (* TODO: support any arity out? *)
    let body = seq_of_wasm m instrs in
    control_labelled (Loop ((arity_in, arity_out), body))
  | Ast.If (st, instrs1, instrs2) ->
    let (arity_in, arity_out) = arity_of_block st in
    let body1 = seq_of_wasm m instrs1 in
    let body2 = seq_of_wasm m instrs2 in
    control_labelled (If ((arity_in, arity_out), body1, body2))
  | Ast.CallIndirect f ->
    let (arity_in, arity_out) = arity_of_fun_type m f in
    assert (arity_out <= 1);
    control_labelled (CallIndirect ((arity_in, arity_out), Index.of_wasm f))
  | Ast.GlobalGet g ->
    data_labelled (GlobalGet (Index.of_wasm g))
  | Ast.GlobalSet g ->
    data_labelled (GlobalSet (Index.of_wasm g))
  | Ast.Load op ->
    data_labelled (Load (Memoryop.of_wasm_load op))
  | Ast.Store op ->
    data_labelled (Store (Memoryop.of_wasm_store op))
  | Ast.MemorySize ->
    data_labelled (MemorySize)
  | Ast.MemoryGrow -> data_labelled MemoryGrow
  | Ast.Test op ->
    data_labelled (Test (Testop.of_wasm op))
  | Ast.Convert op ->
    data_labelled (Convert (Convertop.of_wasm op))
  | Ast.Unary op ->
    data_labelled (Unary (Unop.of_wasm op))
and seq_of_wasm (m : Ast.module_) (is : Ast.instr list) : t list =
  List.map is ~f:(of_wasm m)
