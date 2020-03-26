open Core_kernel
open Wasm

module T = struct
  (** Instructions *)
  type t =
    | Nop
    | Drop
    | Block of t list
    | Loop of t list
    | Const of Value.t
    | Binary of Binop.t
    | Compare of Relop.t
    | Test of Testop.t
    | LocalGet of Var.t
    | LocalSet of Var.t
    | LocalTee of Var.t
    | GlobalGet of Var.t
    | GlobalSet of Var.t
    | Call of Var.t
    | Br of Var.t
    | BrIf of Var.t
    | Return
    | Load of Memoryop.t
    | Store of Memoryop.t
  [@@deriving sexp, compare]
end
include T
let rec to_string ?sep:(sep : string = "\n") ?indent:(i : int = 0) (instr : t) : string =
  Printf.sprintf "%s%s" (String.make i ' ')
    (match instr with
     | Nop -> "nop"
     | Drop -> "drop"
     | Return -> "return"
     | Block instrs -> Printf.sprintf "block%s%s" sep (list_to_string instrs ~indent:(i+2) ~sep:sep)
     | Loop instrs -> Printf.sprintf "loop%s%s" sep (list_to_string instrs ~indent:(i+2) ~sep:sep)
     | Const v -> Printf.sprintf "const %s" (Value.to_string v)
     | Binary b -> Printf.sprintf "binary %s" (Binop.to_string b)
     | Compare r -> Printf.sprintf "compare %s" (Relop.to_string r)
     | Test t -> Printf.sprintf "test %s" (Testop.to_string t)
     | LocalGet v -> Printf.sprintf "local.get %d" v
     | LocalSet v -> Printf.sprintf "local.set %d" v
     | LocalTee v -> Printf.sprintf "local.tee %d" v
     | Br b -> Printf.sprintf "br %d" b
     | BrIf b -> Printf.sprintf "brif %d" b
     | GlobalGet v -> Printf.sprintf "global.get %d" v
     | GlobalSet v -> Printf.sprintf "global.set %d" v
     | Call v -> Printf.sprintf "call %d" v
     | Load op -> Printf.sprintf "load %s" (Memoryop.to_string op)
     | Store op -> Printf.sprintf "store %s" (Memoryop.to_string op)
    )
and list_to_string ?indent:(i : int = 0) ?sep:(sep : string = ", ") (l : t list) : string =
  String.concat ~sep:sep (List.map l ~f:(to_string ?sep:(Some sep) ?indent:(Some i)))

let rec of_wasm (i : Ast.instr) : t =
  match i.it with
  | Ast.Nop -> Nop
  | Ast.Drop -> Drop
  | Ast.Block (_st, instrs) ->
    Block (List.map instrs ~f:of_wasm)
  | Ast.Const lit -> Const (Value.of_wasm lit.it)
  | Ast.Binary bin -> Binary (Binop.of_wasm bin)
  | Ast.Compare rel -> Compare (Relop.of_wasm rel)
  | Ast.LocalGet v -> LocalGet (Var.of_wasm v)
  | Ast.LocalSet v -> LocalSet (Var.of_wasm v)
  | Ast.LocalTee v -> LocalTee (Var.of_wasm v)
  | Ast.BrIf v -> BrIf (Var.of_wasm v)
  | Ast.Br v -> Br (Var.of_wasm v)
  | Ast.Call v -> Call (Var.of_wasm v)
  | Ast.Return -> Return
  | Ast.Unreachable -> failwith "unsupported instruction: unreachable"
  | Ast.Select -> failwith "unsupported instruction: select"
  | Ast.Loop (_st, instrs) -> Loop (List.map instrs ~f:of_wasm)
  | Ast.If (_st, _instr, _instr') -> failwith "unsupported instruction: if"
  | Ast.BrTable (_vs, _v) -> failwith "unsupported instruction: brtable"
  | Ast.CallIndirect _v -> failwith "unsupported instruction: call indirect"
  | Ast.GlobalGet v -> GlobalGet (Var.of_wasm v)
  | Ast.GlobalSet v -> GlobalSet (Var.of_wasm v)
  | Ast.Load op -> Load (Memoryop.of_wasm_load op)
  | Ast.Store op -> Store (Memoryop.of_wasm_store op)
  | Ast.MemorySize -> failwith "unsupported instruction: current memory"
  | Ast.MemoryGrow -> failwith "unsupported instruction: memory grow"
  | Ast.Test op -> Test (Testop.of_wasm op)
  | Ast.Convert _op -> failwith "unsupported instruction: convert"
  | Ast.Unary _op -> failwith "unsupported instruction: unary"
