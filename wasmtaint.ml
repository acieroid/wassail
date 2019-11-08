open Core
open Wasm

(* TODO: ([ ] = to start, [-] = started, [x] = finished)
  - [x] Support memory instructions (load, store)
  - [x] Display loaded program nicely
  - [ ] Infinite loop due to CP lattice that doesn't join correctly when updating locals
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
  let rec to_string ?indent:(i : int = 0) (instr : t) : string =
    Printf.sprintf "%s%s" (String.make i ' ')
      (match instr with
       | Nop -> "nop"
       | Drop -> "drop"
       | Return -> "return"
       | Block instrs -> Printf.sprintf "block\n%s" (list_to_string instrs ~indent:(i+2) ~sep:"\n")
       | Loop instrs -> Printf.sprintf "loop\n%s" (list_to_string instrs ~indent:(i+2) ~sep:"\n")
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
    String.concat ~sep:sep (List.map l ~f:(to_string ?indent:(Some i)))

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

module Configuration = struct
  module T = struct
    type t = {
      (* XXX: we probably don't want the frame and the store as part of the config, or only as a reference *)
      store : Store.t;
      frame : Frame.t;
      vstack : Value.t list;
      astack : Instr.t list;
    }
    [@@deriving sexp, compare]
  end
  include T
  let to_string (c : t) : string =
    Printf.sprintf "Config{\n\tvals: [%s]\n\tinstr: [%s]\n\tlocals: [%s]\n}" (String.concat ~sep:", " (List.map c.vstack ~f:Value.to_string)) (String.concat ~sep:", " (List.map c.astack ~f:Instr.to_string)) (String.concat ~sep:", " (List.map c.frame.locals ~f:Value.to_string))
  module Set = Set.Make(T)
  module Map = Map.Make(T)
  let join (c1 : t) (c2 : t) : t =
    (* We're not supposed to join configurations with non-empty administrative stacks *)
    (* assert (c1.astack = [] && c2.astack = []); *)
    { store = Store.join c1.store c2.store;
      frame = Frame.join c1.frame c2.frame;
      vstack = List.map2_exn c1.vstack c2.vstack ~f:Value.join;
      astack = [] }
  let join_opt (c1 : t option) (c2 : t option) : t option =
    match (c1, c2) with
    | None, None -> None
    | Some c1, None -> Some c1
    | None, Some c2 -> Some c2
    | Some c1, Some c2 -> Some (join c1 c2)
end

module Break = struct
  module T = struct
    type t = int * Configuration.t
    [@@deriving sexp, compare]
  end
  include T
  include Comparator.Make(T)
  module Set = Set.Make(T)
  let to_string (b : t) : string =
    Printf.sprintf "(%d, %s)" (fst b) (Configuration.to_string (snd b))
end

module Deps = struct
  module T = struct
    (* Most of the time, a block either reaches its final state, a return statement, or a break. Due to joining, a block analysis could reach multiple of these *)
    type block_result = {
      configuration: Configuration.t option;
      breaks: Break.Set.t;
      returned: Configuration.t option;
    }
    [@@deriving sexp, compare]
    type t = {
      results: block_result Configuration.Map.t ref (* configuration -> configuration map *)
    }
    [@@deriving sexp, compare]
  end
  include T
  include Comparator.Make(T)
  let no_result : block_result =
    { configuration = None; breaks = Break.Set.empty; returned = None }
  let empty : t = {
    results = ref Configuration.Map.empty;
  }
  let join_block_results (b1 : block_result) (b2: block_result) : block_result =
    let join_conf_opt c1 c2 = match (c1, c2) with
      | (Some c1, Some c2) -> Some (Configuration.join c1 c2)
      | (Some c1, None) -> Some c1
      | (None, Some c2) -> Some c2
      | (None, None) -> None in
    { configuration = join_conf_opt b1.configuration b2.configuration;
      returned = join_conf_opt b1.returned b2.returned;
      breaks = Break.Set.union b1.breaks b2.breaks
    }
  let block_result_to_string (b : block_result) : string =
    Printf.sprintf "configuration %s, returned: %s, breaks: [%s]"
      (match b.configuration with
       | Some c -> Configuration.to_string c
       | None -> "none")
      (match b.returned with
       | Some c -> Configuration.to_string c
       | None -> "none")
      (String.concat ~sep:", " (List.map (Break.Set.elements b.breaks) ~f:Break.to_string))
end

(* Structure of the execution:
Store ; Frame ; Instructions -> Store'; Frame'; Instructions'
   where Instructions is isomorphic to the stack *)
module FunctionAnalysis = struct
  type step_result = {
    configs: Configuration.Set.t;
    breaks: Break.Set.t;
    returned: Configuration.t option;
    finished: Configuration.t option;
  }
  [@@deriving sexp, compare]

  let no_result : step_result = {
    configs = Configuration.Set.empty;
    breaks = Break.Set.empty;
    returned = None;
    finished = None;
  }
  let finished (c : Configuration.t) : step_result =
    { no_result with finished = Some c }
  let reached (c : Configuration.t) : step_result =
    { no_result with configs = Configuration.Set.singleton c }
  let returned (c : Configuration.t) : step_result =
    { no_result with returned = Some c }
  let break (b : Break.t) : step_result =
    { no_result with breaks = Break.Set.singleton b }
  let merge_results (r1 : step_result) (r2 : step_result) : step_result =
    {
      configs = Configuration.Set.union r1.configs r2.configs;
      breaks = Break.Set.union r1.breaks r2.breaks;
      returned = Configuration.join_opt r1.returned r2.returned;
      finished = Configuration.join_opt r2.finished r2.finished;
    }

  (* Analyzes a block. Return the configuration at the exit of a block. This resulting configuration is the join of all reachable configurations. The block could also have "returned" if it reaches a "Return" instruction *)
  let rec analyze_block (conf : Configuration.t) (deps : Deps.t) : Deps.block_result =
    Printf.printf "analyze_block %s\n" (Configuration.to_string conf);
    let rec run_analysis (todo: Configuration.t list) (current: Deps.block_result) : Deps.block_result =
        match todo with
        | [] -> current
        | c :: todo' ->
          Printf.printf "step conf %s from block %s\n" (Configuration.to_string c) (Configuration.to_string conf);
          let stepped = step c deps in
          run_analysis (Configuration.Set.fold stepped.configs ~init:todo' ~f:(fun acc c -> c :: acc))
            (Deps.join_block_results current
               { configuration = stepped.finished; returned = stepped.returned; breaks = stepped.breaks })
      in
    match Configuration.Map.find !(deps.results) conf  with
    | Some res ->
      (* results already computed, return it *)
      Printf.printf "result was cached: %s\n" (Deps.block_result_to_string res);
      res
    | None ->
      (* compute result and cache it. *)
      deps.results := Configuration.Map.update !(deps.results) conf ~f:(fun _ -> Deps.no_result);
      let r = run_analysis [conf] { configuration = None; returned = None; breaks = Break.Set.empty } in
      deps.results := Configuration.Map.update !(deps.results) conf ~f:(fun _ -> r);
      Printf.printf "result is: %s\n" (Deps.block_result_to_string r);
      r
  and invoke (funcaddr : Address.t) (vstack : Value.t list) (store : Store.t) (deps : Deps.t) : (Value.t list * int) =
    let f = Store.get_funcinst store funcaddr in
    let (in_arity, out_arity) = f.arity in
    assert (List.length vstack >= in_arity);
    let valn = List.take vstack in_arity in
    let zeros = List.map f.code.locals ~f:Value.zero in
    Printf.printf "fun %d there are (%d,%d) params and %d locals\n" funcaddr in_arity out_arity (List.length f.code.locals);
    let frame = Frame.{
      arity = in_arity + (List.length f.code.locals);
      module_ = f.module_;
      locals = valn @ zeros;
    } in
    (* let b = Block (snd f.typ) f.code.body in *)
    let in_conf = Configuration.{
        store ;
        frame ;
        vstack = [] ;
        astack = f.code.body
      } in
    Printf.printf "calling function, analyzing block\n";
    let analysis_result = analyze_block in_conf deps in
    let exit_conf = match (analysis_result.configuration, analysis_result.returned) with
      | (Some c1, Some c2) -> Configuration.join c1 c2
      | (Some c1, None) -> c1
      | (None, Some c2) -> c2
      | (None, None) -> failwith "no analysis result" in
    (List.take exit_conf.vstack in_arity, in_arity)

  and enter_block (config : Configuration.t) (instrs : Instr.t list) (deps : Deps.t) : step_result =
    Printf.printf "entering block %s\n" (Configuration.to_string config);
    let analysis_result = analyze_block { config with astack = instrs } deps in
    (* Decrease the index of the breaks, those that would reach 0 become finished configurations *)
    let (breaks, finished) = Break.Set.partition_tf analysis_result.breaks ~f:(fun (b, _) -> b = 1) in
    let breaks' = Break.Set.map breaks ~f:(fun (b, c) -> (b-1, c)) in
    let finished' = Break.Set.fold finished
        ~init:analysis_result.configuration
        ~f:(fun acc (_, c) ->
            match acc with
            | Some c' -> Some (Configuration.join c c')
            | None -> Some c) in
    { configs = begin match finished' with
          | Some c -> Configuration.Set.singleton { c with astack = List.tl_exn config.astack }
          | None -> Configuration.Set.empty (* next conf comes from successful exits of the analyzed block *)
        end;
      finished = None; (* there's no finished *)
      returned = analysis_result.returned (* Returns are propagated *);
      breaks = breaks' (* breaks are updated *)}
  (* Step a configuration by one instruction.
     Only recursive for specific rewrite case (e.g. TeeLocal is expressed in terms of SetLocal *)
  and step (config : Configuration.t) (deps : Deps.t) : step_result =
      match config.astack with
      | [] -> finished config
      | head :: astack' ->
        Printf.printf "head is %s\n, vstack is %s\n" (Instr.to_string head) (String.concat ~sep:"," (List.map ~f:Value.to_string config.vstack));
        match (head, config.vstack) with
        | Nop, _ ->
          (* [spec] Do nothing *)
          reached
            { config with astack = astack' }
        | Drop, _ :: vrest ->
          (* [spec] Assert: due to validation, a value is on the top of the stack.
             Pop the value val from the stack. *)
          reached
            { config with vstack = vrest; astack = astack' }
        | Drop, _ ->
          failwith "Invalid value stack for drop"
        | LocalGet x, vstack ->
          (* [spec] Let F be the current frame.
             Assert: due to validation, F.locals[x] exists.
             Let val be the value F.locals[x].
             Push the value val to the stack. *)
          let v = Frame.get_local config.frame x in
          reached
            { config with vstack = v :: vstack; astack = astack' }
        | LocalSet x, v :: vstack ->
          (* [spec] Let F be the current frame.
             Assert: due to validation, F.locals[x] exists.
             Assert: due to validation, a value is on the top of the stack.
             Pop the value val from the stack.
             Replace F.locals[x] with the value val. *)
          let frame' = Frame.set_local config.frame x v in
          reached
            { config with vstack = vstack; astack = astack'; frame = frame' }
        | LocalSet _, _ -> failwith "Invalid value stack for setlocal"
        | LocalTee x, v :: vstack ->
          (* [spec] Assert: due to validation, a value is on the top of the stack.
             Pop the value val from the stack.
             Push the value val to the stack.
             Push the value val to the stack.
             Execute the instruction (local.set x). *)
          step { config with vstack = v :: v :: vstack; astack = LocalSet x :: astack' } deps
        | LocalTee _, _ ->
          failwith "Invalid value stack for teelocal"
        | Block instrs, _ ->
          (* [spec] Let n be the arity |t?| of the result type t?.
             Let L be the label whose arity is n and whose continuation is the end of the block.
             Enter the block instr∗ with label L. *)
          (* We empty the administrative stack before analyzing the block, as we want the block to be analyzed up to its end *)
          Printf.printf "block\n";
          enter_block config instrs deps
        | Loop instrs, _ ->
          Printf.printf "loop\n";
          enter_block config (instrs @ [Loop instrs]) deps
        | Call x, vstack ->
          (* [spec] Let F be the current frame.
             Assert: due to validation, F.module.funcaddrs[x] exists.
             Let a be the function address F.module.funcaddrs[x].
             Invoke the function instance at address a. *)
          let funcaddr = Frame.funcaddr config.frame x in
          Printf.printf "call function %d with vstack: %s\n" funcaddr (String.concat ~sep:","  (List.map vstack ~f:Value.to_string));
          (* Invoke the function, get a list of return values *)
          let (return_vs, arity) = invoke funcaddr vstack config.store deps in
          reached { config with vstack = return_vs @ List.drop vstack arity }
        | Return, _vstack ->
          (* [spec] Let F be the current frame.
             Let n be the arity of F.
             Assert: due to validation, there are at least n values on the top of the stack.
             Pop the results valn from the stack.
             Assert: due to validation, the stack contains at least one frame.
             While the top of the stack is not a frame, do:
             Pop the top element from the stack.
             Assert: the top of the stack is the frame F.
             Pop the frame from the stack.
             Push valn to the stack.
             Jump to the instruction after the original call that pushed the frame. *)
          (* We just clear the administrative stack when returning *)
          returned { config with astack = [] }
        | Const v, vstack ->
          (* [spec] Push the value t.const c to the stack. *)
          reached { config with vstack = v :: vstack; astack = astack' }
        | Compare rel, v2 :: v1 :: vstack ->
          (* [spec] Assert: due to validation, two values of value type t are on the top of the stack.
             Pop the value t.const c2 from the stack.
             Pop the value t.const c1 from the stack.
             Let c be the result of computing relopt(c1,c2).
             Push the value i32.const c to the stack. *)
          let v = Relop.eval rel v1 v2 in
          reached
            { config with vstack = v :: vstack; astack = astack' }
        | Compare _, _ -> failwith "Invalid value stack for compare"
        | Binary bin, v2 :: v1 :: vstack ->
          (* [spec] Assert: due to validation, two values of value type t are on the top of the stack.
             Pop the value t.const c2 from the stack.
             Pop the value t.const c1 from the stack.
             If binopt(c1,c2) is defined, then:
               Let c be a possible result of computing binopt(c1,c2).
               Push the value t.const c to the stack.
             Else:
               Trap. *)
          let v = Binop.eval bin v1 v2 in (* TODO: trap *)
          reached
            { config with vstack = v :: vstack; astack = astack' }
        | Binary _, _ -> failwith "Invalid value stack for binary"
        | Test test, v :: vstack ->
          (* [spec] Assert: due to validation, a value of value type t is on the top of the stack.
             Pop the value t.const c1 from the stack.
             Let c be the result of computing testopt(c1).
             Push the value i32.const c to the stack. *)
          let v' = Testop.eval test v in
          reached
            { config with vstack = v' :: vstack; astack = astack' }
        | GlobalGet global, vstack ->
          (* [spec] Let F be the current frame.
             Assert: due to validation, F.module.globaladdrs[x] exists.
             Let a be the global address F.module.globaladdrs[x].
             Assert: due to validation, S.globals[a] exists.
             Let glob be the global instance S.globals[a].
             Let val be the value glob.value.
             Push the value val to the stack. *)
          let addr = Frame.get_global_addr config.frame global in
          let g = Store.get_global config.store addr in
          reached
            { config with vstack = g.value :: vstack; astack = astack' }
        | GlobalSet global, v :: vstack ->
          (* [spec] Let F be the current frame.
             Assert: due to validation, F.module.globaladdrs[x] exists.
             Let a be the global address F.module.globaladdrs[x].
             Assert: due to validation, S.globals[a] exists.
             Let glob be the global instance S.globals[a].
             Assert: due to validation, a value is on the top of the stack.
             Pop the value val from the stack.
             Replace glob.value with the value val. *)
          let addr = Frame.get_global_addr config.frame global in
          reached { config with vstack = vstack; astack = astack'; store = Store.set_global config.store addr v }
        | Test _, _ -> failwith "Invalid value stack for test"
        | Br n, _ ->
          (* [spec] Assert: due to validation, the stack contains at least l+1 labels.
             Let L be the l-th label appearing on the stack, starting from the top and counting from zero.
             Let n be the arity of L.
             Assert: due to validation, there are at least n values on the top of the stack.
             Pop the values valn from the stack.
             Repeat l+1 times:
             While the top of the stack is a value, do:
             Pop the value from the stack.
             Assert: due to validation, the top of the stack now is a label.
             Pop the label from the stack.
             Push the values valn to the stack.
             Jump to the continuation of L. *)
          (* We encode this as just returning Break with the value on n. This
             will be propagated back until n is 0 *)
          break (n, { config with astack = astack' })
        | BrIf n, v :: vstack ->
          (* [spec] Assert: due to validation, a value of value type i32 is on the top of the stack.
             Pop the value i32.const c from the stack.
             If c is non-zero, then:
               Execute the instruction (br l).
             Else:
               Do nothing. *)
          Printf.printf "Breaking, vstack is %s\n" (String.concat ~sep:"," (List.map ~f:Value.to_string vstack));
          let config' = { config with vstack = vstack; astack = astack' } in
          let r1 = if Value.is_zero v then reached config' else no_result in
          let r2 = if Value.is_zero v then break (n, config') else no_result in
          merge_results r1 r2
        | BrIf _, _ -> failwith "Invalid value stack for br_if"
        | Load op, _v :: vstack ->
          (* This is a pretty dumb memory abstraction: we just return the top value when loading something from the linea memory *)
          let c = Value.top op.typ in (* value of the correct type *)
          reached { config with astack = astack'; vstack = c :: vstack }
        | Store _op, _v :: _i :: vstack ->
          reached { config with astack = astack'; vstack = vstack }
        | _, _ -> failwith "Not implemented yet"

  let analyze_function (funcaddr : Address.t) (store : Store.t) (deps : Deps.t) : (Value.t list * int) =
    let f = Store.get_funcinst store funcaddr in
    Printf.printf "Analyzing function:\n%s\n" (FuncInst.to_string f);
    let (in_arity, _) = f.arity in
    let vstack = List.init in_arity ~f:(fun _ -> Value.Int) in
    invoke funcaddr vstack store deps
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

let () =
  let run (l : (Script.var option * Script.definition) list) =
    List.iter l ~f:(fun (_var_opt, def) ->
        match def.it with
        | Script.Textual m ->
          let store = Store.init m in
          let deps = Deps.empty in
          List.iteri store.funcs ~f:(fun faddr _ ->
              let (res, _arity) = FunctionAnalysis.analyze_function faddr store deps in
              Printf.printf "result: ";
              List.iter res ~f:(fun v -> Printf.printf "%s " (Value.to_string v));
              Printf.printf "\n");
          ()
        | Script.Encoded _ -> failwith "unsupported"
        | Script.Quoted _ -> failwith "unsupported"
      ) in
  Printf.printf "Success? %b" (parse_file "examples/overflow/overflow.wat" run)
