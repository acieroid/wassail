open Core
open Wasm

(* What we need for now:

Analyzing foo:

i32.const
i32.add
i32.le_s


Rest:
loop

i32.sub
i32.store
i32.load
i32.eqz
i32.mul
*)

module Value = struct
  module T = struct
    type t =
      | Const of int32
      | Int
      (* TODO: values are actually i32/i64/f32/f64 *)
    [@@deriving sexp, compare]
  end
  include T
  include Comparator.Make(T)

  (*
  let is_zero (v : t) =
    match v with
    | Const 0 -> true
    | Const _ -> false
    | Int -> true
  let is_not_zero (v : t) =
    match v with
    | Const 0 -> false
    | _ -> true *)
end

module Store = struct
  module T = struct
    type t = unit (* TODO *)
    [@@deriving sexp, compare]
  end
  include T
end

module Address = struct
  module T = struct
    type t = int (* TODO: abstract it, but how? *)
    [@@deriving sexp, compare]
  end
  include T
end

(* TODO: table instances *)
(* TODO: memory instances *)
(* TODO: global instances *)
(* TODO: export instances *)
(* TODO: external values *)

module Instr = struct
  module T = struct
    type binop =
      | I32Add
    [@@deriving sexp, compare]
    type relop =
      | I32LeS
    [@@deriving sexp, compare]
    type value_type =
      | I32Type
    [@@deriving sexp, compare]
    type stack_type = value_type list
    [@@deriving sexp, compare]
    type var = int
    [@@deriving sexp, compare]
    type value = Value.t
    [@@deriving sexp, compare]
    type t =
      | Nop
      | Drop
      | Block of stack_type * t list
      | Const of value
      | Binary of binop
      | Compare of relop
      | GetLocal of var
      | SetLocal of var
      | TeeLocal of var
      | Call of var
      | Return
    [@@deriving sexp, compare]
  end
  include T

  let convert_value_type (vt : Types.value_type) : value_type =
    match vt with
    | Types.I32Type -> I32Type
    | _ -> failwith "unsupported type"
  let convert_stack_type (st : Types.stack_type) : stack_type =
    List.map st ~f:convert_value_type
  let convert_value (v : Values.value) : value =
    match v with
    | I32 x -> Value.Const x
    | _ -> failwith "unsupported type"
  let convert_var (v : Ast.var) : var = Int32.to_int_exn v.it
  let convert_binop (b : Ast.binop) : binop =
    match b with
    | I32 Add -> I32Add
    | _ -> failwith "unsupported type"
  let convert_relop (r : Ast.relop) : relop =
    match r with
    | I32 LeS -> I32LeS
    | _ -> failwith "unsupported type"
  let rec of_wasm_instr (i : Ast.instr) : t =
    match i.it with
    | Ast.Nop -> Nop
    | Ast.Drop -> Drop
    | Ast.Block (st, instrs) ->
      Block (convert_stack_type st, List.map instrs ~f:of_wasm_instr)
    | Ast.Const lit -> Const (convert_value lit.it)
    | Ast.Binary bin -> Binary (convert_binop bin)
    | Ast.Compare rel -> Compare (convert_relop rel)
    | Ast.GetLocal v -> GetLocal (convert_var v)
    | Ast.SetLocal v -> SetLocal (convert_var v)
    | Ast.TeeLocal v -> TeeLocal (convert_var v)
    | Ast.Call v -> Call (convert_var v)
    | Ast.Return -> Return
    | _ -> failwith "unsupported instruction"
end


(*
    type t = Ast.instr
    let sexp_of_pos (p : Source.pos) : Sexp.t =
      Sexp.List [Sexp.Atom p.file; Sexp.Atom (string_of_int p.line); Sexp.Atom (string_of_int p.column)]
    let pos_of_sexp (s : Sexp.t) : Source.pos =
      match s with
      | Sexp.List [Sexp.Atom file; Sexp.Atom line; Sexp.Atom column] ->
        { file = file; line = int_of_string line; column = int_of_string column }
      | _ -> failwith "invalid position"
    let sexp_of_region (r : Source.region) : Sexp.t =
      Sexp.List [sexp_of_pos r.left; sexp_of_pos r.right]
    let region_of_sexp (s : Sexp.t) : Source.region =
      match s with
      | Sexp.List [left; right] ->
        { left = pos_of_sexp left; right = pos_of_sexp right }
      | _ -> failwith "invalid region"
    let sexp_of_value_type (vt : Types.value_type) : Sexp.t =
      Sexp.Atom (match vt with
          | I32Type -> "i32"
          | I64Type -> "i64"
          | F32Type -> "f32"
          | F64Type -> "f64")
    let value_type_of_sexp (s : Sexp.t) : Types.value_type =
      match s with
      | Sexp.Atom "i32" -> I32Type
      | Sexp.Atom "i64" -> I64Type
      | Sexp.Atom "f32" -> I32Type
      | Sexp.Atom "f64" -> F64Type
      | _ -> failwith "invalid value type"
    let sexp_of_stack_type (st : Types.stack_type) : Sexp.t =
      Sexp.List (List.map st ~f:sexp_of_value_type)
    let stack_type_of_sexp (s : Sexp.t) : Types.stack_type =
      match s with
      | Sexp.List l -> List.map l ~f:value_type_of_sexp
      | _ -> failwith "invalid value type"
    let sexp_of_var (v : Ast.var) : Sexp.t =
      Sexp.List [Int32.sexp_of_t v.it; Sexp.Atom "@@"; sexp_of_region v.at]
    let var_of_sexp (s : Sexp.t) : Ast.var =
      match s with
      | Sexp.List [v; Sexp.Atom "@@"; region] ->
        { it = Int32.t_of_sexp v ; at = region_of_sexp region }
      | _ -> failwith "invalid var"
    let sexp_of_value (v : Values.value) : Sexp.t =
      match v with
        | I32 v -> Sexp.List [Sexp.Atom "i32"; Int32.sexp_of_t v]
        | I64 v -> Sexp.List [Sexp.Atom "i64"; Int64.sexp_of_t v]
        | F32 _ -> failwith "unsupported: floats"
        | F64 _ -> failwith "unsupported: floats"
    let value_of_sexp (s : Sexp.t) : Values.value =
      match s with
      | Sexp.List [Sexp.Atom "i32"; v] -> I32 (Int32.t_of_sexp v)
      | Sexp.List [Sexp.Atom "i64"; v] -> I32 (Int32.t_of_sexp v)
      | _ -> failwith "invalid or unsupported value"
    let sexp_of_literal (lit : Ast.literal) : Sexp.t =
      Sexp.List [sexp_of_value lit.it; Sexp.Atom "@@"; sexp_of_region lit.at]
    let literal_of_sexp (s : Sexp.t) : Ast.literal =
      match s with
      | Sexp.List [lit; Sexp.Atom "@@"; region] ->
        { it = value_of_sexp lit; at = region_of_sexp region }
      | _ -> failwith "invalid literal"
    let sexp_of_intbinop (b : Ast.IntOp.binop) : Sexp.t =
      match b with
      | Add -> Sexp.Atom "add"
      | Sub -> Sexp.Atom "sub"
      | Mul -> Sexp.Atom "mul"
      | And -> Sexp.Atom "and"
      | Or -> Sexp.Atom "or"
      | Xor -> Sexp.Atom "xor"
      | _ -> failwith "unsupported intbinop"
    let intbinop_of_sexp (s : Sexp.t) : Ast.IntOp.binop =
      match s with
      | Sexp.Atom "add" -> Add
      | Sexp.Atom "sub" -> Sub
      | Sexp.Atom "mul" -> Mul
      | Sexp.Atom "and" -> And
      | Sexp.Atom "xor" -> Xor
      | _ -> failwith "unsupported intbinop"
    let sexp_of_intrelop (r : Ast.IntOp.relop) : Sexp.t =
      match r with
      | Eq -> Sexp.Atom "eq"
      | Ne -> Sexp.Atom "ne"
      | LtS -> Sexp.Atom "lt_s"
      | LtU -> Sexp.Atom "lt_u"
      | GtS -> Sexp.Atom "gt_s"
      | GtU -> Sexp.Atom "gt_u"
      | LeS -> Sexp.Atom "le_s"
      | LeU -> Sexp.Atom "le_u"
      | GeS -> Sexp.Atom "ge_s"
      | GeU -> Sexp.Atom "ge_u"
    let intrelop_of_sexp (s : Sexp.t) : Ast.IntOp.relop =
      match s with
      | Sexp.Atom "eq" -> Eq
      | Sexp.Atom "ne" -> Ne
      | Sexp.Atom "lt_s" -> LtS
      | Sexp.Atom "lt_u" -> LtU
      | Sexp.Atom "gt_s" -> GtS
      | Sexp.Atom "gt_u" -> GtU
      | Sexp.Atom "le_s" -> LeS
      | Sexp.Atom "le_u" -> LeU
      | Sexp.Atom "ge_s" -> GeS
      | Sexp.Atom "ge_u" -> GeU
      | _ -> failwith "invalid intrelop"
    let sexp_of_valueop (o : ('a, 'a, 'b, 'b) Values.op)
        (int : 'a -> Sexp.t) (float : 'b -> Sexp.t)
      : Sexp.t =
      match o with
      | I32 op -> Sexp.List [Sexp.Atom "i32"; int op]
      | I64 op -> Sexp.List [Sexp.Atom "i64"; int op]
      | F32 op -> Sexp.List [Sexp.Atom "f32"; float op]
      | F64 op -> Sexp.List [Sexp.Atom "f64"; float op]
    let valueop_of_sexp (s : Sexp.t) (int : Sexp.t -> 'a) (float : Sexp.t -> 'b) : ('a, 'a, 'b, 'b) Values.op =
      match s with
      | Sexp.List [Sexp.Atom "i32"; op] -> I32 (int op)
      | Sexp.List [Sexp.Atom "i64"; op] -> I64 (int op)
      | Sexp.List [Sexp.Atom "f32"; op] -> F32 (float op)
      | Sexp.List [Sexp.Atom "f64"; op] -> F64 (float op)
      | _ -> failwith "invalid valueop"
    let sexp_of_binop (b : Ast.binop) : Sexp.t = sexp_of_valueop b sexp_of_intbinop (fun _ -> failwith "unsupported: floats")
    let binop_of_sexp (s : Sexp.t) : Ast.binop = valueop_of_sexp s intbinop_of_sexp (fun _ -> failwith "unsupported: floats")
    let sexp_of_relop (r : Ast.relop) : Sexp.t = sexp_of_valueop r sexp_of_intrelop (fun _ -> failwith "unsupported: floats")
    let relop_of_sexp (s : Sexp.t) : Ast.relop = valueop_of_sexp s intrelop_of_sexp (fun _ -> failwith "unspported: floats")
    let rec sexp_of_t (i : t) : Sexp.t =
      Sexp.List [(match i.it with
          | Unreachable -> Sexp.Atom "unreachable"
          | Nop -> Sexp.Atom "nop"
          | Drop -> Sexp.Atom "drop"
          | Select -> Sexp.Atom "select"
          | Return -> Sexp.Atom "return"
          | GrowMemory -> Sexp.Atom "growmemory"
          | CurrentMemory -> Sexp.Atom "currentmemory"
          | Block (st, instrs) ->
            Sexp.List [Sexp.Atom "block";
                       sexp_of_stack_type st;
                       Sexp.List (List.map instrs ~f:sexp_of_t)]
          | Loop (st, instrs) ->
            Sexp.List [Sexp.Atom "loop";
                       sexp_of_stack_type st;
                       Sexp.List (List.map instrs ~f:sexp_of_t)]
          | GetLocal var -> Sexp.List [Sexp.Atom "getlocal"; sexp_of_var var]
          | SetLocal var -> Sexp.List [Sexp.Atom "setlocal"; sexp_of_var var]
          | TeeLocal var -> Sexp.List [Sexp.Atom "setlocal"; sexp_of_var var]
          | Br var -> Sexp.List [Sexp.Atom "br"; sexp_of_var var]
          | BrIf var -> Sexp.List [Sexp.Atom "brif"; sexp_of_var var]
          | Call var -> Sexp.List [Sexp.Atom "call"; sexp_of_var var]
          | CallIndirect var -> Sexp.List [Sexp.Atom "callindirect"; sexp_of_var var]
          | Const lit -> Sexp.List [Sexp.Atom "const"; sexp_of_literal lit]
          | Binary bin -> Sexp.List [Sexp.Atom "binary"; sexp_of_binop bin]
          | Compare rel -> Sexp.List [Sexp.Atom "compare"; sexp_of_relop rel]
          | _ -> failwith "unsupported instr"
        ); Sexp.Atom "@@"; sexp_of_region i.at]
    let rec t_of_sexp (s: Sexp.t) : t =
      match s with
      | Sexp.List [e; Sexp.Atom "@@"; region] ->
        {at = region_of_sexp region;
         it = match e with
           | Sexp.Atom "unreachable" -> Ast.Unreachable
           | Sexp.Atom "drop" -> Ast.Drop
           | Sexp.Atom "select" -> Ast.Select
           | Sexp.Atom "return" -> Ast.Return
           | Sexp.Atom "growmemory" -> Ast.GrowMemory
           | Sexp.Atom "currentmemory" -> Ast.CurrentMemory
           | Sexp.List [Sexp.Atom "block"; st; Sexp.List instrs] ->
             Ast.Block (stack_type_of_sexp st, List.map instrs ~f:(t_of_sexp))
           | Sexp.List [Sexp.Atom "loop"; st; Sexp.List instrs] ->
             Ast.Loop (stack_type_of_sexp st, List.map instrs ~f:(t_of_sexp))
           | Sexp.List [Sexp.Atom "getlocal"; v] -> Ast.GetLocal (var_of_sexp v)
           | Sexp.List [Sexp.Atom "setlocal"; v] -> Ast.SetLocal (var_of_sexp v)
           | Sexp.List [Sexp.Atom "teelocal"; v] -> Ast.SetLocal (var_of_sexp v)
           | Sexp.List [Sexp.Atom "br"; v] -> Ast.Br (var_of_sexp v)
           | Sexp.List [Sexp.Atom "brif"; v] -> Ast.BrIf (var_of_sexp v)
           | Sexp.List [Sexp.Atom "call"; v] -> Ast.Call (var_of_sexp v)
           | Sexp.List [Sexp.Atom "callindirect"; v] -> Ast.CallIndirect (var_of_sexp v)
           | Sexp.List [Sexp.Atom "const"; lit] -> Ast.Const (literal_of_sexp lit)
           | Sexp.List [Sexp.Atom "binary"; bin] -> Ast.Binary (binop_of_sexp bin)
           | Sexp.List [Sexp.Atom "compare"; rel] -> Ast.Compare (relop_of_sexp rel)
           | _ -> failwith "invalid sexp for instr"
        }
      | _ -> failwith "invadid sexpr for instr"
    let compare (i1 : t) (i2 : t) : int =
      match (i1.it, i2.it) with
      | _ -> failwith "TODO"
  end
  include T
end
module Label = struct
  module T = struct
    type t = int * Instr.t list
    [@@deriving sexp, compare]
  end
  include T
end
*)

module Frame = struct
  module T = struct
    type t =  {
      locals: Value.t list;
      (* TODO: module instance *)
    }
    [@@deriving sexp, compare]
  end
  include T
  let get_local (f : t) (l : Instr.var) : Value.t =
    List.nth_exn f.locals l
end

module Activation = struct
  module T = struct
    type t = int * Frame.t
    [@@deriving sexp, compare]
  end
  include T
end

module Configuration = struct
  module T = struct
    type t = {
      store : Store.t;
      frame : Frame.t;
      vstack : Value.t list;
      astack : Instr.t list;
    }
    [@@deriving sexp, compare]
  end
  include T
  module Set = Set.Make(T)
end

(*
module Stack = struct
  module T = struct
    type entry =
      | Val of Value.t
      | Lab of Label.t
      | Act of Activation.t
    [@@deriving sexp, compare]
    type t = entry list (* TODO: represent using an array/vector/stack *)
    [@@deriving sexp, compare]
  end
  include T
  include Comparator.Make(T)
  (* Pop a value from the stack, raise an exception if the stack is empty *)
  let pop_exn (s : t) : (entry * t) =
    match s with
    | [] -> failwith "popping from empty stack"
    | head :: tail -> (head, tail)
  let push_val (s : t) (v : Value.t) : t =
    v :: s
  module Set = Set.Make(T)
end *)

(* Structure of the execution:
Store ; Frame ; Instructions -> Store'; Frame'; Instructions'
   where Instructions is isomorphic to the stack *)
module FunctionAnalysis = struct
  let analyze (_f: Ast.func)  =
    let _eval (config : Configuration.t) : Configuration.Set.t =
      match config.stack with
      | [] -> Configuration.Set.empty
      | head :: rest -> match head with
        | Nop ->
          (* Do nothing. nop -> epsilon *)
          Configuration.Set.singleton config
        | Drop ->
          begin match rest with
          | [] -> failwith "Empty stack on drop (should not happen)"
          | _ :: rest -> Configuration.Set.singleton {config with stack = rest}
          end
        | GetLocal v ->
          let value = Frame.get_local config.frame v in
          Configuration.Set.singleton
            { config with stack = (Instr.Const value) :: rest }
        | SetLocal v -> failwith "TODO"
        | TeeLocal _ -> failwith "TODO"
        | Block _ -> failwith "TODO now"
        | Call _ -> failwith "TODO now"
        | Return -> failwith "TODO now"
        | Const _ -> failwith "TODO now"
        | Compare _ -> failwith "TODO now"
        | Binary _ -> failwith "TODO now"
    in
    ()
end
let trace name = print_endline ("-- " ^ name)

let error at category msg =
  trace ("Error: ");
  prerr_endline (Source.string_of_region at ^ ": " ^ category ^ ": " ^ msg);
  false

let input_from get_script run =
  try
    let script = get_script () in
    trace "Running...";
    run script;
    true
  with
  | Decode.Code (at, msg) -> error at "decoding error" msg
  | Parse.Syntax (at, msg) -> error at "syntax error" msg
  | Valid.Invalid (at, msg) -> error at "invalid module" msg
  | Import.Unknown (at, msg) -> error at "link failure" msg
  | Eval.Link (at, msg) -> error at "link failure" msg
  | Eval.Trap (at, msg) -> error at "runtime trap" msg
  | Eval.Exhaustion (at, msg) -> error at "resource exhaustion" msg
  | Eval.Crash (at, msg) -> error at "runtime crash" msg
  | Encode.Code (at, msg) -> error at "encoding error" msg

let parse_file name run =
  let ic = In_channel.create "foo.wat" in
  try
    let lexbuf = Lexing.from_channel ic in
    let success = input_from (fun _ ->
        let var_opt, def = Parse.parse name lexbuf Parse.Module in
        [(var_opt, def)]) run in
    In_channel.close ic;
    success
  with exn -> In_channel.close ic; raise exn

let () =
  let run (l : (Script.var option * Script.definition) list) =
    Printf.printf "I got %d elements\n" (List.length l);
    List.iter l ~f:(fun (var_opt, def) ->
        begin match var_opt with
        | Some {it = x; at = at} -> Printf.printf "var: %s (at %s)\n" x (Source.string_of_region at)
        | None -> Printf.printf "no var\n"
        end;
        begin match def.it with
          | Script.Textual m ->
            begin match m.it.start with
              | Some var -> Printf.printf "start: %s\n" (Int32.to_string var.it)
              | None -> Printf.printf "no start\n"
            end;
            List.iter m.it.exports ~f:(fun e ->
                Printf.printf "export: ";
                List.iter e.it.name ~f:(fun x -> Printf.printf "%c" (Char.of_int_exn x));
                Printf.printf "\n";
                match e.it.edesc.it with
                | FuncExport v -> Printf.printf "func %s\n" (Int32.to_string v.it)
                | TableExport v -> Printf.printf "table %s\n" (Int32.to_string v.it)
                | MemoryExport v -> Printf.printf "memory %s\n" (Int32.to_string v.it)
                | GlobalExport v -> Printf.printf "global %s\n" (Int32.to_string v.it));
            List.iter m.it.funcs ~f:(fun f ->
                Printf.printf "ftype: %s\n" (Int32.to_string f.it.ftype.it);
                Printf.printf "locals:\n";
                List.iter f.it.locals ~f:(fun t ->
                    Printf.printf "%s\n" (Types.string_of_value_type t));
                Printf.printf "instrs:\n";
                List.iter f.it.body ~f:(fun instr ->
                    Printf.printf "%s\n"
                      (match instr.it with
                       | Unreachable -> "unreachable"
                       | Nop -> "nop"
                       | Drop -> "drop"
                       | Select -> "select"
                       | Block _ -> "block"
                       | Loop _ -> "loop"
                       | If _ -> "if"
                       | Br _ -> "br"
                       | BrIf _ -> "brif"
                       | BrTable _ -> "brtable"
                       | Return -> "return"
                       | Call var -> Printf.sprintf "call %s" (Int32.to_string var.it)
                       | CallIndirect _ -> "callindirect"
                       | GetLocal _ -> "localget"
                       | SetLocal _ -> "localset"
                       | TeeLocal _ -> "localtee"
                       | GetGlobal _ -> "globalget"
                       | SetGlobal _ -> "globalset"
                       | Load _ -> "load"
                       | Store _ -> "store"
                       | CurrentMemory -> "memorysize"
                       | GrowMemory -> "memorygrow"
                       | Const _ -> "const"
                       | Test _ -> "test"
                       | Compare _ -> "compare"
                       | Unary _ -> "unary"
                       | Binary _ -> "binary"
                       | Convert _ -> "convert")))
          | Script.Encoded (x, y) -> Printf.printf "Encoded: %s\n------\n%s" x y
          | Script.Quoted (x, y) -> Printf.printf "Quoted: %s\n------\n%s" x y
        end
      ) in
  Printf.printf "Success? %b" (parse_file "foo.wat" run)
