open Core_kernel
open Wasm

module T = struct
  (** Data instructions *)
  type data =
    | Nop
    | Drop
    | Select
    | Const of Value.t
    | Binary of Binop.t
    | Compare of Relop.t
    | Test of Testop.t
    | LocalGet of Var.t
    | LocalSet of Var.t
    | LocalTee of Var.t
    | GlobalGet of Var.t
    | GlobalSet of Var.t
    | Load of Memoryop.t
    | Store of Memoryop.t
  (** Control instructions *)
  and control =
    | Block of t list
    | Loop of t list
    | If of (t list * t list)
    | Call of Var.t
    | CallIndirect of Var.t
    | Br of Var.t
    | BrIf of Var.t
    | Return
    | Unreachable
  (** All instructions *)
  and  t =
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
     | Const v -> Printf.sprintf "const %s" (Value.to_string v)
     | Binary b -> Printf.sprintf "binary %s" (Binop.to_string b)
     | Compare r -> Printf.sprintf "compare %s" (Relop.to_string r)
     | Test t -> Printf.sprintf "test %s" (Testop.to_string t)
     | LocalGet v -> Printf.sprintf "local.get %d" v
     | LocalSet v -> Printf.sprintf "local.set %d" v
     | LocalTee v -> Printf.sprintf "local.tee %d" v
     | GlobalGet v -> Printf.sprintf "global.get %d" v
     | GlobalSet v -> Printf.sprintf "global.set %d" v
     | Load op -> Printf.sprintf "load %s" (Memoryop.to_string op)
     | Store op -> Printf.sprintf "store %s" (Memoryop.to_string op)
let rec control_to_string ?sep:(sep : string = "\n") ?indent:(i : int = 0) (instr : control) : string =
  match instr with
  | Call v -> Printf.sprintf "call %d" v
  | CallIndirect v -> Printf.sprintf "call_indirect %d" v
  | Br b -> Printf.sprintf "br %d" b
  | BrIf b -> Printf.sprintf "brif %d" b
  | Return -> "return"
  | Unreachable -> "unreachable"
  | Block instrs -> Printf.sprintf "block%s%s" sep (list_to_string instrs ~indent:(i+2) ~sep:sep)
  | Loop instrs -> Printf.sprintf "loop%s%s" sep (list_to_string instrs ~indent:(i+2) ~sep:sep)
  | If (instrs1, instrs2) -> Printf.sprintf "if%s%s%selse%s%s" sep
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
  | Call v -> Printf.sprintf "call %d" v
  | CallIndirect v -> Printf.sprintf "call_indirect %d" v
  | Br b -> Printf.sprintf "br %d" b
  | BrIf b -> Printf.sprintf "brif %d" b
  | Return -> "return"
  | Unreachable -> "unreachable"
  | Block _ -> "block"
  | Loop _ -> "loop"
  | If (_, _) -> "if"

let rec of_wasm (i : Ast.instr) : t =
  match i.it with
  | Ast.Nop -> Data Nop
  | Ast.Drop -> Data Drop
  | Ast.Block (_st, instrs) ->
    Control (Block (List.map instrs ~f:of_wasm))
  | Ast.Const lit -> Data (Const (Value.of_wasm lit.it))
  | Ast.Binary bin -> Data (Binary (Binop.of_wasm bin))
  | Ast.Compare rel -> Data (Compare (Relop.of_wasm rel))
  | Ast.LocalGet v -> Data (LocalGet (Var.of_wasm v))
  | Ast.LocalSet v -> Data (LocalSet (Var.of_wasm v))
  | Ast.LocalTee v -> Data (LocalTee (Var.of_wasm v))
  | Ast.BrIf v -> Control (BrIf (Var.of_wasm v))
  | Ast.Br v -> Control (Br (Var.of_wasm v))
  | Ast.Call v -> Control (Call (Var.of_wasm v))
  | Ast.Return -> Control Return
  | Ast.Unreachable -> Control Unreachable
  | Ast.Select -> Data Select
  | Ast.Loop (_st, instrs) -> Control (Loop (List.map instrs ~f:of_wasm))
  | Ast.If (_st, instrs1, instrs2) -> Control (If (List.map instrs1 ~f:of_wasm, List.map instrs2 ~f:of_wasm))
  | Ast.BrTable (_vs, _v) -> failwith "br_table unsupported"
  | Ast.CallIndirect v -> Control (CallIndirect (Var.of_wasm v))
  | Ast.GlobalGet v -> Data (GlobalGet (Var.of_wasm v))
  | Ast.GlobalSet v -> Data (GlobalSet (Var.of_wasm v))
  | Ast.Load op -> Data (Load (Memoryop.of_wasm_load op))
  | Ast.Store op -> Data (Store (Memoryop.of_wasm_store op))
  | Ast.MemorySize -> failwith "memory_size unsupported"
  | Ast.MemoryGrow -> failwith "memory_grow unsupported"
  | Ast.Test op -> Data (Test (Testop.of_wasm op))
  | Ast.Convert _op -> failwith "convert unsupported"
  | Ast.Unary _op -> failwith "unary unsupported"
