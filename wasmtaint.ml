open Core
open Wasm

(* TODO: ([ ] = to start, [-] = started, [x] = finished)
  - [x] Support memory instructions (load, store)
  - [x] Display loaded program nicely
  - [ ] Tests
  - [ ] Use apron for abstraction of values
  - [ ] Display results of the analysis

For later:
  - Track taint!

For much later:
  - other types (i64, f32, f64)
  - other instructions
*)

(** These are types of values during the execution of wasm *)
module Type = struct
  module T = struct
    type t =
      | I32Type
      | I64Type
      | F32Type
      | F64Type
    [@@deriving sexp, compare]
  end
  include T

  let of_wasm (vt : Types.value_type) : t =
    match vt with
    | Types.I32Type -> I32Type
    | Types.I64Type -> I64Type
    | Types.F32Type -> F32Type
    | Types.F64Type -> F64Type
  let to_string (t : t) : string =
    match t with
    | I32Type -> "i32"
    | I64Type -> "i64"
    | F32Type -> "f32"
    | F64Type -> "f64"
  let list_to_string (l : t list) : string =
    String.concat ~sep:", " (List.map l ~f:to_string)
end

(** These are the values (and their abstractions) *)
module Value = struct
  module T = struct
    type t =
      | Const of int32
      | Int
      (* XXX: values are actually i32/i64/f32/f64 *)
    [@@deriving sexp, compare]
  end
  include T
  include Comparator.Make(T)

  let of_wasm (v : Values.value) : t =
    match v with
    | I32 x -> Const x
    | I64 _ -> failwith "unsupported type: I64"
    | F32 _ -> failwith "unsupported type: F32"
    | F64 _ -> failwith "unsupported type: F64"

  let to_string (v : t) : string =
    match v with
    | Const n -> Int32.to_string n
    | Int -> "int"

  (** Joins two values together *)
  let join (v1 : t) (v2 : t) : t =
    match (v1, v2) with
    | (Const n1, Const n2) when n1 = n2 -> Const n1
    | (Const _, Const _) -> Int
    | _ -> Int

  let is_zero (v : t) =
    match v with
    | Const 0l -> true
    | Const _ -> false
    | Int -> true
  let is_not_zero (v : t) =
    match v with
    | Const 0l -> false
    | _ -> true

  let zero (t : Type.t) : t =
    match t with
    | I32Type -> Const 0l
    | _ -> failwith "unsupported type"

  let top (t : Type.t) : t =
    match t with
    | I32Type -> Const 0l
    | _ -> failwith "unsupported type"

  let list_to_string (l : t list) : string =
    String.concat ~sep:", " (List.map l ~f:to_string)
end


(** These are the addresses. This needs to be further improved *)
module Address = struct
  module T = struct
    type t = int (* XXX: abstract it, but how? Also, depend on which address (function address are fine with int) *)
    [@@deriving sexp, compare]
  end
  include T
end

(** A variable in wasm is just an index *)
module Var = struct
  module T = struct
    type t = int
    [@@deriving sexp, compare]
  end
  include T
  let of_wasm (v : Ast.var) : t = Int32.to_int_exn v.it
end

(** Binary operations *)
module Binop = struct
  module T = struct
    type t =
      | I32Add
      | I32Sub
      | I32Mul
      (* XXX: there are many other operations *)
    [@@deriving sexp, compare]
  end
  include T
  let of_wasm (b : Ast.binop) : t =
    match b with
    | I32 Add -> I32Add
    | I32 Sub -> I32Sub
    | I32 Mul -> I32Mul
    | I32 _ -> failwith "unsupported operation"
    | _ -> failwith "unsupported type"

  let to_string (b : t) : string =
    match b with
    | I32Add -> "i32.add"
    | I32Sub -> "i32.sub"
    | I32Mul -> "i32.sub"

  (** Evaluates a binary operation on two values *)
  let eval (b : t) (v1 : Value.t) (v2 : Value.t) : Value.t =
    match (b, v1, v2) with
    | (I32Add, Const n1, Const n2) -> Const (Int32.(+) n1 n2)
    | (I32Add, _, _) -> Int
    | (I32Sub, Const n1, Const n2) -> Const (Int32.(-) n1 n2)
    | (I32Sub, _, _) -> Int
    | (I32Mul, Const n1, Const n2) -> Const (Int32.( * ) n1 n2)
    | (I32Mul, _, _) -> Int
end

(** Relational operation *)
module Relop = struct
  module T = struct
    type t =
      | I32Eq
      | I32Ne
      | I32LtS
      | I32GtS
      | I32LeS
      | I32GeS (* XXX: others *)
    [@@deriving sexp, compare]
  end
  include T
  let of_wasm (r : Ast.relop) : t =
    match r with
    | I32 Eq -> I32Eq
    | I32 Ne -> I32Ne
    | I32 LtS -> I32LtS
    | I32 GtS -> I32GtS
    | I32 LeS -> I32LeS
    | I32 GeS -> I32GeS
    | I32 _ -> failwith "unsupported relational operation"
    | _ -> failwith "unsupported type"
  let to_string (r : t) : string =
    match r with
    | I32Eq -> "i32.eq"
    | I32Ne -> "i32.ne"
    | I32LtS -> "i32.lt_s"
    | I32GtS -> "i32.gt_s"
    | I32LeS -> "i32.le_s"
    | I32GeS -> "i32.ge_s"
  let eval (r : t) (v1 : Value.t) (v2 : Value.t) : Value.t =
    match (r, v1, v2) with
    | (I32Eq, Const n1, Const n2) -> Const (if n1 = n2 then 1l else 0l)
    | (I32Eq, _, _) -> Int
    | (I32Ne, Const n1, Const n2) -> Const (if n1 <> n2 then 1l else 0l)
    | (I32Ne, _, _) -> Int
    | (I32LtS, Const n1, Const n2) -> Const (if n1 < n2 then 1l else 0l)
    | (I32LtS, _, _) -> Int
    | (I32GtS, Const n1, Const n2) -> Const (if n1 > n2 then 1l else 0l)
    | (I32GtS, _, _) -> Int
    | (I32LeS, Const n1, Const n2) -> Const (if n1 <= n2 then 1l else 0l)
    | (I32LeS, _, _) -> Int
    | (I32GeS, Const n1, Const n2) -> Const (if n1 >= n2 then 1l else 0l)
    | (I32GeS, _, _) -> Int
end

(** Test operations *)
module Testop = struct
  module T = struct
    type t =
      | I32Eqz
    [@@deriving sexp, compare]
  end
  include T
  let of_wasm (t : Ast.testop) : t =
    match t with
    | I32 Eqz -> I32Eqz
    | _ -> failwith "unsupported type"
  let to_string (t : t) : string =
    match t with
    | I32Eqz -> "i32.eqz"
  let eval (t : t) (v1 : Value.t) : Value.t =
    match (t, v1) with
    | (I32Eqz, Const 0l) -> Const 1l
    | (I32Eqz, Const _) -> Const 0l
    | (I32Eqz, Int) -> Value.join (Const 0l) (Const 1l)
end

(** Description of memory operations (load and store) *)
module Memoryop = struct
  module T = struct
    type extension = SX | ZX
    [@@deriving sexp, compare]
    type pack_size = Pack8 | Pack16 | Pack32
    [@@deriving sexp, compare]
    type t = {
      typ: Type.t;
      align: int;
      offset: int;
      (* The extension part is only use for load operation and should be ignored for store operations *)
      sz: (pack_size * extension) option;
    }
    [@@deriving sexp, compare]
  end
  include T
  let to_string (op : t) : string =
    Printf.sprintf "align=%d, offset=%d, sz=%s"
      op.align op.offset
      (match op.sz with
       | Some (pack, ext) ->
         Printf.sprintf "%s,%s"
           (match pack with
            | Pack8 -> "8"
            | Pack16 -> "16"
            | Pack32 -> "32")
           (match ext with
            | SX -> "sx"
            | ZX -> "zx")
       | None -> "none")
  let of_wasm_load (op : Ast.loadop) : t = {
    typ = Type.of_wasm op.ty;
    align = op.align;
    offset = Int32.to_int_exn op.offset;
    sz = Option.map op.sz ~f:(fun (pack, ext) ->
            (match pack with
            | Memory.Pack8 -> Pack8
            | Memory.Pack16 -> Pack16
            | Memory.Pack32 -> Pack32),
            match ext with
            | Memory.SX -> SX
            | Memory.ZX -> ZX);
  }
  let of_wasm_store (op : Ast.storeop) : t = {
    typ = Type.of_wasm op.ty;
    align = op.align;
    offset = Int32.to_int_exn op.offset;
    sz = Option.map op.sz ~f:(function
            | Memory.Pack8 -> (Pack8, SX)
            | Memory.Pack16 -> (Pack16, SX)
            | Memory.Pack32 -> (Pack32, SX));
  }
end

(** Instructions *)
module Instr = struct
  module T = struct
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
       | LocalTee v -> Printf.sprintf "tee.local %d" v
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
end

module Func = struct
  module T = struct
    type t = {
      locals : Type.t list;
      body : Instr.t list;
    }
    [@@deriving sexp, compare]
  end
  include T
  let of_wasm (f : Ast.func) : t = {
    body = List.map f.it.body ~f:Instr.of_wasm;
    locals = List.map f.it.locals ~f:Type.of_wasm;
  }
  let to_string (f : t) : string =
    Printf.sprintf "locals: %s\ncode:\n%s" (Type.list_to_string f.locals) (Instr.list_to_string f.body ~sep:"\n")
end

module ByteAbstr = struct
  module T = struct
    type t =
      | Const of char
      | Byte
    [@@deriving sexp, compare]
  end
  include T
  let zero = Const (Char.of_int_exn 0)
  let join (b1 : t) (b2 : t) : t =
    match (b1, b2) with
    | Const x, Const y when x = y -> Const x
    | _, _ -> Byte
end
module MemoryInst = struct
  module T = struct
    type t = {
      data: ByteAbstr.t; (* Abstraction: everything merged into the same value *)
      max_size: int option;
    }
    [@@deriving sexp, compare]
  end
  include T
  let page_size = 65536
  let of_wasm (m : Ast.memory) : t =
    match m.it.mtype with
    | MemoryType t ->
      {
        data = ByteAbstr.zero;
        max_size = Option.map t.max ~f:Int32.to_int_exn
      }
end

module ModuleInst = struct
  module T = struct
    type t = {
      funcaddrs: Address.t list;
      globaladdrs: Address.t list;
      memaddrs: Address.t list;
      (* Other fields are not represented because we don't need them. These are:
        - types: function types (this has already been validated so we can ignore it)
        - tableddrs: we don't support tables (yet)
        - exports: we don't take exports into account (yet) *)
    }
    [@@deriving sexp, compare]
  end
  include T

  let init (m : Ast.module_) : t =
    let funcaddrs = List.mapi m.it.funcs ~f:(fun i _ -> i) in
    let globaladdrs = List.mapi m.it.globals ~f:(fun i _ -> i) in
    let memaddrs = List.mapi m.it.memories ~f:(fun i _ -> i) in
    { funcaddrs; globaladdrs; memaddrs }
end

module GlobalInst = struct
  module T = struct
    type t = {
      value : Value.t;
      mut : bool;
    }
    [@@deriving sexp, compare]
  end
  include T

  let of_wasm (g : Ast.global) : t =
    { value = Value.of_wasm (match g.it.value.it with
          | [] -> failwith "Undefined global"
          | [v] -> begin match v.it with
              | Ast.Const l -> l.it
              | _ -> failwith "Unsupported non-const global instaciation"
            end
          | _ -> failwith "Unsupported global instanciation with multiple instructions");
      mut = match g.it.gtype with
        | Types.GlobalType (_, Types.Immutable) -> false
        | Types.GlobalType (_, Types.Mutable) -> true
    }
  let join (g1 : t) (g2 : t) : t =
    assert (g1.mut = g2.mut);
    { value = Value.join g1.value g2.value; mut = g1.mut }
end

module FuncInst = struct
  module T = struct
    type t = {
      name : string option;
      arity : (int * int);
      typ : (Type.t list * Type.t list);
      module_: ModuleInst.t;
      code: Func.t;
    }
    [@@deriving sexp, compare]
  end
  include T
  let of_wasm (m : Ast.module_) (minst : ModuleInst.t) (index : int) (f : Ast.func) : t =
    let name = (List.find_map m.it.exports ~f:(fun x ->
             match x.it.edesc.it with
             | FuncExport v when (Int32.to_int_exn v.it) = index -> Some (Ast.string_of_name x.it.name)
             | _ -> None
           ))
    in
    match Ast.func_type_for m f.it.ftype with
    | FuncType (input, output) -> {
        name = name;
        arity = (List.length input, List.length output);
        typ = (List.map input ~f:Type.of_wasm, List.map output ~f:Type.of_wasm);
        module_ = minst;
        code = Func.of_wasm f
      }
  let to_string (f : t) : string =
    Printf.sprintf "Function %s (%s -> %s):\nCode: %s"
      (match f.name with
       | Some n -> n
       | None -> "<noname>")
      (String.concat ~sep:", " (List.map (fst f.typ) ~f:Type.to_string))
      (String.concat ~sep:", " (List.map (snd f.typ) ~f:Type.to_string))
      (Func.to_string f.code)
end

module Store = struct
  module T = struct
    type t = {
      funcs : FuncInst.t list;
      globals : GlobalInst.t list;
      mems : MemoryInst.t list;
      (* XXX: other fields *)
    }
    [@@deriving sexp, compare]
  end
  include T
  let get_funcinst (s : t) (a : Address.t) : FuncInst.t =
    List.nth_exn s.funcs a
  let get_global (s : t) (a : Address.t) : GlobalInst.t =
    List.nth_exn s.globals a
  let set_global (s : t) (a : Address.t) (v : Value.t) : t =
    { s with globals = List.mapi s.globals ~f:(fun i g ->
          if i = a then { g with value = Value.join g.value v } else g) }
  let get_meminst (s : t) (a : Address.t) : MemoryInst.t =
    List.nth_exn s.mems a
  let join (s1 : t) (s2 : t) : t =
    assert (s1.funcs = s2.funcs);
    { s1 with
      globals = List.map2_exn s1.globals s2.globals ~f:GlobalInst.join
    }
  let init (m : Ast.module_) : t =
    let minst = ModuleInst.init m in
    ({
      funcs = List.mapi m.it.funcs ~f:(FuncInst.of_wasm m minst);
      globals = List.map m.it.globals ~f:GlobalInst.of_wasm;
      mems = List.map m.it.memories ~f:MemoryInst.of_wasm;
    })
end

module Frame = struct
  module T = struct
    type t = {
      arity: int;
      locals: Value.t list;
      module_: ModuleInst.t;
    }
    [@@deriving sexp, compare]
  end
  include T
  let funcaddr (f : t) (fn : Var.t) : Address.t =
    List.nth_exn f.module_.funcaddrs fn
  let get_local (f : t) (l : Var.t) : Value.t =
    List.nth_exn f.locals l
  let set_local (f : t) (l : Var.t) (v : Value.t) : t =
    { f with locals = List.mapi f.locals ~f:(fun i x -> if i = l then Value.join x v else x) }
  let get_global_addr (f : t) (g : Var.t) : Address.t =
    List.nth_exn f.module_.globaladdrs g
  let get_memory_addr (f : t) (n : int) : Address.t =
    List.nth_exn f.module_.memaddrs n
  let join (f1 : t) (f2 : t) =
    assert (f1.arity = f2.arity);
    assert (f1.module_ = f2.module_);
    { f1 with locals = List.map2_exn f1.locals f2.locals ~f:Value.join }
end

module Activation = struct
  module T = struct
    type t = int * Frame.t
    [@@deriving sexp, compare]
  end
  include T
end

module type CFGData = sig
  type t
  val to_string : t -> string
  val init : t
end

module CFGNoData : CFGData = struct
  type t = unit
  let to_string () = ""
  let init = ()
end

module BasicBlock = struct
  type block_sort = BlockEntry | BlockExit | LoopEntry | LoopExit | Normal | Function | Return
  type t = {
    idx: int;
    sort: block_sort;
    instrs: Instr.t list;
  }
  let to_string (b : t) : string = Printf.sprintf "block %d" b.idx
  let to_dot (b : t) : string =
    match b.sort with
    | Normal ->
      Printf.sprintf "block%d [shape=record, label=\"{Block %d:\\l\\l%s\\l}\"];"
        b.idx b.idx
        (String.concat ~sep:"\\l"
           (List.map b.instrs
              ~f:(fun instr ->
                  Printf.sprintf "%s" (Instr.to_string instr ~sep:"\\l"))))
    | BlockEntry ->
      Printf.sprintf "block%d [shape=ellipse, label = \"Block entry\"];" b.idx
    | BlockExit ->
      Printf.sprintf "block%d [shape=ellipse, label = \"Block exit\"];" b.idx
    | LoopEntry ->
      Printf.sprintf "block%d [shape=ellipse, label = \"Loop entry\"];" b.idx
    | LoopExit ->
      Printf.sprintf "block%d [shape=ellipse, label = \"Loop exit\"];" b.idx
    | Function ->
      Printf.sprintf "block%d [shape=star, label=\"Function call\"];" b.idx
    | Return ->
      Printf.sprintf "block%d [shape=point]" b.idx
end

module I = struct
  type t = int
  [@@deriving sexp, compare]
end

module IntMap = Map.Make(I)
module IntSet = Set.Make(I)

module CFG = struct
  type t = {
    (* The index of this CFG *)
    idx: int;
    (* The number of locals in that CFG *)
    nlocals: int;
    (* All basic blocks contained in this CFG, indexed in a map by their index *)
    basic_blocks: BasicBlock.t IntMap.t;
    (* The edges between basic blocks (forward direction) *)
    edges: (int list) IntMap.t;
    (* The edges between basic blocks (backward direction) *)
    back_edges: (int list) IntMap.t;
    (* The entry block *)
    entry_block: int;
    (* The exit block *)
    exit_block: int;
  }
  let to_string (cfg : t) : string = Printf.sprintf "CFG of function %d" cfg.idx
  let to_dot (cfg : t) : string =
    Printf.sprintf "digraph \"CFG of function %d\" {\n%s\n%s}\n"
      cfg.idx
      (String.concat ~sep:"\n" (List.map (IntMap.to_alist cfg.basic_blocks) ~f:(fun (_, b) -> BasicBlock.to_dot b)))
      (String.concat ~sep:"\n" (List.concat_map (IntMap.to_alist cfg.edges) ~f:(fun (left, right) ->
           List.map right ~f:(Printf.sprintf "block%d -> block%d;\n" left))))

  let find_block_exn (cfg : t) (idx : int) : BasicBlock.t =
    IntMap.find_exn cfg.basic_blocks idx

  let successors (cfg : t) (idx : int) : int list =
    IntMap.find_multi cfg.edges idx

  let predecessors (cfg : t) (idx : int) : int list =
    IntMap.find_multi cfg.back_edges idx
end

module Domain = struct
  (* The value stack is abstracted as a stack of values. It cannot grow unbounded so that is safe *)
  type vstack = Value.t list
  [@@deriving sexp, compare]
  let pop (vstack : vstack) : (Value.t * vstack) =
    match vstack with
    | hd :: tl -> (hd, tl)
    | _ -> failwith "Invalid empty vstack"
  (* Similarly, locals are finite for any function *)
  type locals = Value.t list
  [@@deriving sexp, compare]
  let get_local (l : locals) (x : int) : Value.t = List.nth_exn l x
  let set_local (l : locals) (x : int) (v' : Value.t) : locals = List.mapi l ~f:(fun i v -> if i = x then v' else v)

  (* There are also a finite number of globals *)
  type globals = Value.t list
  [@@deriving sexp, compare]
  let get_global (g : globals) (x : int) : Value.t = List.nth_exn g x
  let set_global (g : globals) (x : int) (v' : Value.t) : globals = List.mapi g ~f:(fun i v -> if i = x then v' else v)

  (* TODO *)
  type memory = TODO
  [@@deriving sexp, compare]

  type state = {
    vstack : vstack;
    locals : locals;
    globals : globals;
    memory : memory
  }
  [@@deriving sexp, compare]

  let to_string (s : state) : string =
    Printf.sprintf "{vstack: [%s], locals: [%s], globals: [%s]}"
      (String.concat ~sep:", " (List.map s.vstack ~f:Value.to_string))
      (String.concat ~sep:", " (List.mapi s.locals ~f:(fun i v -> Printf.sprintf "%d: %s" i (Value.to_string v))))
      (String.concat ~sep:", " (List.mapi s.locals ~f:(fun i v -> Printf.sprintf "%d: %s" i (Value.to_string v))))

  let bottom (nlocals : int) (globals : globals) (memory : memory) = {
    vstack = [];
    locals = List.init nlocals ~f:(fun _ -> Value.zero I32Type); (* TODO: should actually be bottom *)
    globals = globals;
    memory = memory
  }

  let join (s1 : state) (s2 : state) : state = {
    vstack =
      if List.length s1.vstack <> List.length s2.vstack then
        (* Different length, probably one has not been analyzed yet. Just take the maximal one *)
        if List.length s1.vstack > List.length s2.vstack then begin
          assert (s2.vstack = []);
          s1.vstack
        end else begin
          assert (s1.vstack = []);
          s2.vstack
        end
      else
        List.map2_exn s1.vstack s2.vstack ~f:Value.join;
    locals = List.map2_exn s1.locals s2.locals ~f:Value.join;
    globals = List.map2_exn s1.globals s2.globals ~f:Value.join;
    memory = TODO
  }
end

module Transfer = struct
  let rec instr_transfer (i : Instr.t) (state : Domain.state) : Domain.state =
    match i with
    | Nop ->
      state
    | Drop ->
      let (_, vstack') = Domain.pop state.vstack in
      { state with vstack = vstack' }
    | LocalGet x ->
      { state with vstack = (Domain.get_local state.locals x) :: state.vstack }
    | LocalSet x ->
      let (v, vstack') = Domain.pop state.vstack in
      { state with vstack = vstack';
                   locals = Domain.set_local state.locals x v }
    | LocalTee x ->
      let (v, vstack') = Domain.pop state.vstack in
      instr_transfer (LocalSet x) { state with vstack = v :: v :: vstack' }
    | GlobalGet x ->
      { state with vstack = (Domain.get_global state.globals x) :: state.vstack }
    | GlobalSet x ->
      let (v, vstack') = Domain.pop state.vstack in
      { state with vstack = vstack';
                   globals = Domain.set_global state.globals x v }
    | Br _ ->
      state
    | BrIf _ ->
      let (_, vstack') = Domain.pop state.vstack in
      { state with vstack = vstack' }
    | Return -> state
    | Const v ->
      { state with vstack = v :: state.vstack }
    | Compare rel ->
      let (v1, vstack') = Domain.pop state.vstack in
      let (v2, vstack'') = Domain.pop vstack' in
      let v = Relop.eval rel v1 v2 in
      { state with vstack = v :: vstack'' }
    | Binary bin ->
      let (v1, vstack') = Domain.pop state.vstack in
      let (v2, vstack'') = Domain.pop vstack' in
      let v = Binop.eval bin v1 v2 in
      { state with vstack = v :: vstack'' }
    | Test test ->
      let (v, vstack') = Domain.pop state.vstack in
      let v' = Testop.eval test v in
      { state with vstack = v' :: vstack' }
    | Load op ->
      (* TODO: for now, we just return the top value of the expected type *)
      let c = Value.top op.typ in (* value of the correct type *)
      { state with vstack = c :: state.vstack }
    | Store _op ->
      (* TODO: for now, we just ignore the store *)
      let (_, vstack') = Domain.pop state.vstack in
      { state with vstack = vstack' }
    | Block _ -> failwith "shouldn't happen"
    | Loop _ -> failwith "shouldn't happen"
    | Call _ -> failwith "shouldn't happen"

  let transfer (b : BasicBlock.t) (state : Domain.state) : Domain.state =
    match b.sort with
    | Normal ->
      Printf.printf "Block %d, instructions: %s\n" b.idx (String.concat ~sep:"," (List.map b.instrs ~f:Instr.to_string));
      List.fold_left b.instrs ~init:state ~f:(fun acc i ->
          let res = instr_transfer i acc in
          Printf.printf "%s -> %s -> %s\n" (Domain.to_string acc) (Instr.to_string i) (Domain.to_string res);
          res
        )
    | _ -> state
end

module Fixpoint = struct
  (* Analyzes a CFG. Returns a map where each basic blocks is mappped to its input state and output state *)
  let analyze (cfg : CFG.t) (globals : Domain.globals) (memory : Domain.memory) : (Domain.state * Domain.state) IntMap.t =
    let bottom = Domain.bottom cfg.nlocals globals memory in
    (* TODO: how to distinguish initial data from bottom? *)
    let data = ref (IntMap.of_alist_exn (List.map (IntMap.keys cfg.basic_blocks)
                                           ~f:(fun idx ->
                                               Printf.printf "creating data for block %d\n" idx;
                                               (idx, (bottom, bottom))))) in
    let rec fixpoint (worklist : IntSet.t) (iteration : int) : unit =
      if IntSet.is_empty worklist then
        () (* No more elements to consider. We can stop here *)
      else
        let block_idx = IntSet.min_elt_exn worklist in
        Printf.printf "Analyzing block %d\n" block_idx;
        let predecessors = CFG.predecessors cfg block_idx in
        (* in_state is the join of all the the out_state of the predecessors *)
        let in_state = List.fold_left (List.map predecessors ~f:(fun idx -> snd (IntMap.find_exn !data idx))) ~init:bottom ~f:Domain.join in
        (* The block to analyze *)
        let block = CFG.find_block_exn cfg block_idx in
        (* We analyze it *)
        let out_state = Transfer.transfer block in_state in
        (* Has out state changed? *)
        if out_state = snd (IntMap.find_exn !data block_idx) then
          (* Didn't change, we can safely ignore the successors *)
          (* TODO: make sure that this is true. If not, maybe we just have to put all blocks on the worklist for the first iteration(s) *)
          fixpoint (IntSet.remove worklist block_idx) (iteration+1)
        else
          (* Update the out state in the analysis results *)
          data := IntMap.set !data ~key:block_idx ~data:(in_state, out_state);
          (* And recurse by adding all successors *)
          let successors = CFG.successors cfg block_idx in
          fixpoint (IntSet.union (IntSet.remove worklist block_idx) (IntSet.of_list successors)) (iteration+1)
    in
    fixpoint (IntSet.singleton cfg.entry_block) 1;
    !data
end

module CFGBuilder = struct
  let build (faddr : Address.t) (store : Store.t) : CFG.t =
    let funcinst = Store.get_funcinst store faddr in
    let cur_idx : int ref = ref 0 in
    let new_idx () : int = let v = !cur_idx in cur_idx := v + 1; v in
    let mk_block (reverse_instrs : Instr.t list) : BasicBlock.t =
      let instrs = List.rev reverse_instrs in
      BasicBlock.{ idx = new_idx (); instrs = instrs; sort = BasicBlock.Normal; } in
    let mk_funblock () : BasicBlock.t =
      BasicBlock.{ idx = new_idx () ; instrs = []; sort = Function } in
    let mk_block_entry (is_loop : bool) : BasicBlock.t =
      BasicBlock.{ idx = new_idx () ; instrs = [];
                   sort = if is_loop then LoopEntry else BlockEntry } in
    let mk_block_exit (is_loop : bool) : BasicBlock.t =
      BasicBlock.{ idx = new_idx () ; instrs = [];
                   sort = if is_loop then LoopExit else BlockExit } in
    let mk_block_return () : BasicBlock.t =
      BasicBlock.{ idx = new_idx () ; instrs = []; sort = Return } in
    let rec helper (instrs : Instr.t list) (remaining : Instr.t list) : (
      (* The blocks created *)
      BasicBlock.t list *
      (* The edges within the blocks created *)
      (int * int) list *
      (* The break points as (block_idx, break_level *)
      (int * int) list *
      (* The blocks that have to be connected to the return *)
      int list *
      (* The entry and exit of the created blocks *)
      int * int) =
      match remaining with
      | [] ->
        (* If there's no instruction anymore, build the block and connect it to exit_id *)
        let block = mk_block instrs in
        ([block] (* only this block *), [] (* no edge *),
         [] (* no break point *), [] (* not connected to return *),
         block.idx, block.idx)
      | BrIf level :: rest ->
        (* This is a break up to level `level` *)
        (* First, construct the current block *)
        let block = mk_block (BrIf level :: instrs) in
        (* Then, construct the rest of the CFG *)
        let (blocks, edges, breaks, returns, entry', exit') = helper [] rest in
        (block :: blocks (* add the current block *),
         (block.idx, entry') :: edges (* add an edge between this block and the rest *),
         (block.idx, level) :: breaks (* add a break *),
         returns (* no return *),
         block.idx, exit')
      | Br level :: rest ->
        (* Similar to break, but because it is inconditional, there is no edge from this block to the next. In practice, rest should always be empty here *)
        assert (rest = []);
        let block = mk_block (Br level :: instrs) in
        let (blocks, edges, breaks, returns, _entry', exit') = helper [] rest in
        (block :: blocks (* add the current block *),
         edges (* no edge *),
         (block.idx, level) :: breaks (* add the break *),
         returns (* no return *),
         block.idx, exit')
      | Call f :: rest ->
        (* Also similar to br, but connects the edges differently. Moreover, we don't include the call in this block because it has to be treated differently. *)
        let block = mk_block (Call f :: instrs) in
        let fblock = mk_funblock () in
        let (blocks, edges, breaks, returns, entry', exit') = helper [] rest in
        (block :: fblock :: blocks (* add the current block and the function block *),
         (block.idx, fblock.idx) :: (fblock.idx, entry') :: edges (* connect current block to function block, and function block to the rest *),
         breaks (* no break *),
         returns (* no return *),
         block.idx, exit')
      | ((Block instrs') as b) :: rest
      | ((Loop instrs') as b) :: rest ->
        (* Create a new block with all instructions collected, without the last one *)
        let block = mk_block instrs in
        let is_loop = match b with
          | Loop _ -> true
          | _ -> false in
        let block_entry = mk_block_entry is_loop in
        (* Recurse inside the block *)
        let (blocks, edges, breaks, returns, entry', exit') = helper [] instrs' in
        (* Create a node for the exit of the block *)
        let block_exit = mk_block_exit is_loop in
        (* Recurse after the block *)
        let (blocks', edges', breaks', returns', entry'', exit'') = helper [] rest in
        (* Compute the new break levels:
           All breaks with level 0 break the current block
           All other breaks see their level decreased by one *)
        let new_breaks = List.map (List.filter (breaks @ breaks') ~f:(fun (_, level) -> level > 0)) ~f:(fun (idx, level) -> (idx, level -1)) in
        let break_edges = List.map (List.filter (breaks @ breaks') ~f:(fun (_, level) -> level = 0)) ~f:(fun (idx, _) -> (idx, block_exit.idx)) in
        (* Compute the new edges. This is different between a loop and a block, for the exit of the inside of the block *)
        let new_edges = if is_loop then
            [(block.idx, block_entry.idx); (block_entry.idx, entry'); (exit', block_entry.idx); (block_exit.idx, entry'')]
          else
            [(block.idx, block_entry.idx); (block_entry.idx, entry'); (exit', block_exit.idx); (block_exit.idx, entry'')]
        in
        (block :: block_entry :: block_exit :: (blocks @ blocks') (* add all blocks *),
         new_edges @ break_edges @ edges @ edges' (* add edges *),
         new_breaks (* filtered breaks *),
         returns @ returns' (* returns are propagated as is *),
         block.idx, exit'')
      | Return :: rest ->
        (* Return block. The rest of the instructions does not matter (it should be empty) *)
        assert (rest = []);
        (* We create a new block with all instructions collected, and return it *)
        let block = mk_block instrs in
        (* It is not connected to anything, but is marked as to be connected to a return block *)
        ([block], [], [], [block.idx], block.idx, block.idx)
      | i :: rest ->
        (* Instruction i is part of the block, but not the end of it so we continue *)
        helper (i :: instrs) rest
    in
    let (blocks, edges, breaks, returns, _entry_idx, exit_idx) = helper [] funcinst.code.body in
    let return_block = mk_block_return () in
    let blocks' = return_block :: blocks in
    let edges' = (exit_idx, return_block.idx) :: List.map returns ~f:(fun from -> (from, return_block.idx)) @ edges in
    assert (breaks = []); (* there shouldn't be any breaks outside the function *)
    (* We now filter empty normal blocks *)
    let (actual_blocks, filtered_blocks) = List.partition_tf blocks' ~f:(fun block -> match (block.sort, block.instrs) with
        | Normal, [] -> false
        | _ -> true) in
    let filtered_blocks_idx = List.map filtered_blocks ~f:(fun b -> b.idx) in
    (* And we have to redirect the edges: if there is an edge to a removed block, we make it point to its successors *)
    let actual_edges = List.fold_left filtered_blocks_idx ~init:edges' ~f:(fun edges idx ->
        (* idx is removed, so we find all edges pointing to idx (and we keep track of the other ones, as only these should be kept) *)
        let (pointing_to, edges') = List.partition_tf edges ~f:(fun (_, dst) -> dst = idx) in
        (* and we find all edges pointing from idx (again, keeping track of the other ones) *)
        let (pointing_from, edges'') = List.partition_tf edges' ~f:(fun (src, _) -> src = idx) in
        (* now we connect everything from both sets *)
        List.concat (List.map pointing_to ~f:(fun (src, _) -> List.map pointing_from ~f:(fun (_, dst) -> (src, dst)))) @ edges'') in
    CFG.{
      (* The index of this block is the integer that represent the address of this function *)
      idx = faddr;
      (* There are a+b locals, where a is the number of parameters of the function, and b is the number of local variables *)
      nlocals = fst funcinst.arity + List.length funcinst.code.locals;
      (*The basic blocks *)
      basic_blocks = IntMap.of_alist_exn (List.map actual_blocks ~f:(fun b -> (b.idx, b)));
      (* The forward edges *)
      edges = IntMap.of_alist_multi actual_edges;
      (* The backward edges *)
      back_edges = IntMap.of_alist_multi (List.rev actual_edges);
      (* The entry block *)
      (* TODO: probably not fully correct so we have to pay close attention to that: there should be a single entry block *)
      entry_block = Option.value_exn (List.min_elt (List.map actual_blocks ~f:(fun b -> b.idx)) ~compare:compare);
      (* The exit block is the return block *)
      exit_block = return_block.idx }
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
  let ic = In_channel.create name in
  try
    let lexbuf = Lexing.from_channel ic in
    let success = input_from (fun _ ->
        let var_opt, def = Parse.parse name lexbuf Parse.Module in
        [(var_opt, def)]) run in
    In_channel.close ic;
    success
  with exn -> In_channel.close ic; raise exn

let run_cfg () =
  let run (l : (Script.var option * Script.definition) list) =
    List.iter l ~f:(fun (_var_opt, def) ->
        match def.it with
        | Script.Textual m ->
          let store = Store.init m in
          let globals = ref (List.map store.globals ~f:(fun _ -> Value.zero Type.I32Type)) in
          List.iteri store.funcs ~f:(fun faddr _ ->
              Printf.printf "CFG for function %d\n" faddr;
              let cfg = CFGBuilder.build faddr store in
              Printf.printf "---------------\n%s\n---------------\n" (CFG.to_dot cfg);
              let results = Fixpoint.analyze cfg !globals TODO in
              Printf.printf "-------\nResults\n------\n";
              Map.iteri results ~f:(fun ~key:idx ~data:res ->
                  Printf.printf "block %d: %s -> %s\n" idx (Domain.to_string (fst res)) (Domain.to_string (snd res)));
              ()
            )
        | Script.Encoded _ -> failwith "unsupported"
        | Script.Quoted _ -> failwith "unsupported"
      ) in
  Printf.printf "Success? %b" (parse_file "examples/overflow/overflow.wat" run)

let () = run_cfg ()


