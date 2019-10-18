open Core
open Wasm

(* TODO: ([ ] = to start, [-] = started, [x] = finished)
  - [ ] Support memory instructions (load, store)
  - [ ] Display loaded program nicely
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
    Sexp.to_string [%sexp (v : t)]

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
  let eval (t : t) (v1 : Value.t) : Value.t =
    match (t, v1) with
    | (I32Eqz, Const 0l) -> Const 1l
    | (I32Eqz, Const _) -> Const 0l
    | (I32Eqz, Int) -> Value.join (Const 0l) (Const 1l)
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
    [@@deriving sexp, compare]
  end
  include T
  let to_string (i : t) : string =
    Sexp.to_string [%sexp (i : t)]

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
    | Ast.Load _op -> failwith "unsupported instruction: load"
    | Ast.Store _op -> failwith "unsupported instruction: store"
    | Ast.MemorySize -> failwith "unsupported instruction: current memory"
    | Ast.MemoryGrow -> failwith "unsupported instruction: memory grow"
    | Ast.Test op -> Test (Testop.of_wasm op)
    | Ast.Convert _op -> failwith "unsupported instruction: convert"
    | Ast.Unary _op -> failwith "unsupported instruction: unary"
end

module Store = struct
  module T = struct
    type func = {
      locals : Type.t list;
      body : Instr.t list;
    }
    [@@deriving sexp, compare]
    type moduleinst = {
      funcaddrs: Address.t list;
      globaladdrs: Address.t list;
      (* XXX: other fields *)
    }
    [@@deriving sexp, compare]
    type funcinst = {
      arity : (int * int);
      typ : (Type.t list * Type.t list);
      module_: moduleinst;
      code: func;
    }
    [@@deriving sexp, compare]
    type globalinst = {
      value : Value.t;
      mut : bool;
    }
    [@@deriving sexp, compare]
    type t = {
      funcs : funcinst list;
      globals : globalinst list;
      (* XXX: other fields *)
    }
    [@@deriving sexp, compare]
  end
  include T
  let get_funcinst (s : t) (a : Address.t) : funcinst =
    List.nth_exn s.funcs a
  let get_global (s : t) (a : Address.t) : globalinst =
    List.nth_exn s.globals a
  let join (s1 : t) (s2 : t) : t =
    assert (s1.funcs = s2.funcs);
    { s1 with
      globals = List.map2_exn s1.globals s2.globals ~f:(fun g1 g2 -> assert (g1.mut = g2.mut); { value = Value.join g1.value g2.value; mut = g1.mut })
    }
  let init (m : Ast.module_) : (t * Address.t list) =
    let mk_func (f : Ast.func) : func = {
      body = List.map f.it.body ~f:Instr.of_wasm;
      locals = List.map f.it.locals ~f:Type.of_wasm;
      }
    in
    let mk_funcinst (minst : moduleinst) (f : Ast.func) : funcinst =
      match Ast.func_type_for m f.it.ftype with
      | FuncType (input, output) -> {
          arity = (List.length input, List.length output);
          typ = (List.map input ~f:Type.of_wasm, List.map output ~f:Type.of_wasm);
          module_ = minst;
          code = mk_func f
        } in
    let mk_globalinst (g : Ast.global) : globalinst =
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
    in
    let funcaddrs = List.mapi m.it.funcs ~f:(fun i _ -> i) in
    let globaladdrs = List.mapi m.it.globals ~f:(fun i _ -> i) in
    let minst = { funcaddrs; globaladdrs } in
    ({
      funcs = List.map m.it.funcs ~f:(mk_funcinst minst);
      globals = List.map m.it.globals ~f:mk_globalinst;
    }, funcaddrs)
end

module Frame = struct
  module T = struct
    type t = {
      arity: int;
      locals: Value.t list;
      module_: Store.moduleinst;
    }
    [@@deriving sexp, compare]
  end
  include T
  let funcaddr (f : t) (fn : Var.t) : Address.t =
    List.nth_exn f.module_.funcaddrs fn
  let get_local (f : t) (l : Var.t) : Value.t =
    List.nth_exn f.locals l
  let set_local (f : t) (l : Var.t) (v : Value.t) : t =
    { f with locals = List.mapi f.locals ~f:(fun i x -> if i = l then v else x) }
  let get_global_addr (f : t) (g : Var.t) : Address.t =
    List.nth_exn f.module_.globaladdrs g
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
    let rec run_analysis (todo: Configuration.t list) (current: Deps.block_result) : Deps.block_result =
        match todo with
        | [] -> current
        | c :: todo' ->
          let stepped = step c deps in
          run_analysis (Configuration.Set.fold stepped.configs ~init:todo' ~f:(fun acc c -> c :: acc))
            (Deps.join_block_results current
               { configuration = stepped.finished; returned = stepped.returned; breaks = stepped.breaks })
      in
    match Configuration.Map.find !(deps.results) conf  with
    | Some res ->
      (* results already computed, return it *)
      res
    | None ->
      (* compute result and cache it. *)
      deps.results := Configuration.Map.update !(deps.results) conf ~f:(fun _ -> Deps.no_result);
      let r = run_analysis [conf] { configuration = None; returned = None; breaks = Break.Set.empty } in
      deps.results := Configuration.Map.update !(deps.results) conf ~f:(fun _ -> r);
      r
  and invoke (funcaddr : Address.t) (vstack : Value.t list) (store : Store.t) (deps : Deps.t) : (Value.t list * int) =
    let f = Store.get_funcinst store funcaddr in
    let (in_arity, _) = f.arity in
    let valn = List.take vstack in_arity in
    let zeros = List.map f.code.locals ~f:Value.zero in
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
    let analysis_result = analyze_block in_conf deps in
    let exit_conf = match (analysis_result.configuration, analysis_result.returned) with
      | (Some c1, Some c2) -> Configuration.join c1 c2
      | (Some c1, None) -> c1
      | (None, Some c2) -> c2
      | (None, None) -> failwith "no analysis result" in
    (List.take exit_conf.vstack in_arity, in_arity)

  and enter_block (config : Configuration.t) (instrs : Instr.t list) (deps : Deps.t) : step_result =
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
      | head :: astack' -> match (head, config.vstack) with
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
          enter_block config instrs deps
        | Loop instrs, _ ->
          enter_block config (instrs @ [Loop instrs]) deps
        | Call x, vstack ->
          (* [spec] Let F be the current frame.
             Assert: due to validation, F.module.funcaddrs[x] exists.
             Let a be the function address F.module.funcaddrs[x].
             Invoke the function instance at address a. *)
          let funcaddr = Frame.funcaddr config.frame x in
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
        | GlobalGet v, vstack ->
          (* [spec] Let F be the current frame.
             Assert: due to validation, F.module.globaladdrs[x] exists.
             Let a be the global address F.module.globaladdrs[x].
             Assert: due to validation, S.globals[a] exists.
             Let glob be the global instance S.globals[a].
             Let val be the value glob.value.
             Push the value val to the stack. *)
          let addr = Frame.get_global_addr config.frame v in
          let g = Store.get_global config.store addr in
          reached
            { config with vstack = g.value :: vstack; astack = astack' }
        | GlobalSet _, _ -> failwith "TODO: get global"
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
          let config' = { config with vstack = vstack; astack = astack' } in
          let r1 = if Value.is_zero v then reached config' else no_result in
          let r2 = if Value.is_zero v then break (n, config') else no_result in
          merge_results r1 r2
        | BrIf _, _ -> failwith "Invalid value stack for br_if"

  let analyze_function (funcaddr : Address.t) (store : Store.t) (deps : Deps.t) : (Value.t list * int) =
    let f = Store.get_funcinst store funcaddr in
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
    (* Printf.printf "I got %d elements\n" (List.length l); *)
    List.iter l ~f:(fun (_var_opt, def) ->
        (* begin match var_opt with
        | Some {it = x; at = at} -> Printf.printf "var: %s (at %s)\n" x (Source.string_of_region at)
        | None -> Printf.printf "no var\n"
           end; *)
        begin match def.it with
          | Script.Textual m ->
            (* begin match m.it.start with
              | Some var -> Printf.printf "start: %s\n" (Int32.to_string var.it)
              | None -> Printf.printf "no start\n"
               end; *)
            (* List.iter m.it.tables ~f:(fun table -> table.it.ttype) *)
            (* List.iter m.it.exports ~f:(fun e ->
                Printf.printf "export: ";
                List.iter e.it.name ~f:(fun x -> Printf.printf "%c" (Char.of_int_exn x));
                Printf.printf "\n";
                match e.it.edesc.it with
                | FuncExport v -> Printf.printf "func %s\n" (Int32.to_string v.it)
                | TableExport v -> Printf.printf "table %s\n" (Int32.to_string v.it)
                | MemoryExport v -> Printf.printf "memory %s\n" (Int32.to_string v.it)
                | GlobalExport v -> Printf.printf "global %s\n" (Int32.to_string v.it)); *)
            List.iter m.it.funcs ~f:(fun f ->
                Printf.printf "FUNCTION\n-------------------";
                Printf.printf "ftype: %s\n" (Int32.to_string f.it.ftype.it);
                Printf.printf "locals:\n";
                List.iter f.it.locals ~f:(fun t ->
                    Printf.printf "%s\n" (Types.string_of_value_type t));
                Printf.printf "instrs:\n";
                let rec print_instr (instr : Ast.instr) =
                      (match instr.it with
                       | Unreachable -> Printf.printf "unreachable\n"
                       | Nop -> Printf.printf "nop\n"
                       | Drop -> Printf.printf "drop\n"
                       | Select -> Printf.printf "select\n"
                       | Block (_st, instrs) -> Printf.printf "--[block\n";
                         List.iter instrs ~f:print_instr;
                         Printf.printf "--]"
                       | Loop (_st, instrs) -> Printf.printf "--[loop\n";
                         List.iter instrs ~f:print_instr;
                         Printf.printf "--]"
                       | If _ -> Printf.printf "if\n"
                       | Br v -> Printf.printf "br %s\n" (Int32.to_string v.it)
                       | BrIf v -> Printf.printf "brif %s\n" (Int32.to_string v.it)
                       | BrTable _ -> Printf.printf "brtable\n"
                       | Return -> Printf.printf "return\n"
                       | Call var -> Printf.printf "call %s\n" (Int32.to_string var.it)
                       | CallIndirect _ -> Printf.printf "callindirect\n"
                       | LocalGet _ -> Printf.printf "localget\n"
                       | LocalSet _ -> Printf.printf "localset\n"
                       | LocalTee _ -> Printf.printf "localtee\n"
                       | GlobalGet _ -> Printf.printf "globalget\n"
                       | GlobalSet _ -> Printf.printf "globalset\n"
                       | Load _ -> Printf.printf "load\n"
                       | Store _ -> Printf.printf "store\n"
                       | MemorySize -> Printf.printf "memorysize\n"
                       | MemoryGrow -> Printf.printf "memorygrow\n"
                       | Const _ -> Printf.printf "const\n"
                       | Test _ -> Printf.printf "test\n"
                       | Compare _ -> Printf.printf "compare\n"
                       | Unary _ -> Printf.printf "unary\n"
                       | Binary _ -> Printf.printf "binary\n"
                       | Convert _ -> Printf.printf "convert\n") in
                List.iter f.it.body ~f:print_instr);
            let (store, fs) = Store.init m in
            let deps = Deps.empty in
            List.iter fs ~f:(fun faddr ->
                let (res, _arity) = FunctionAnalysis.analyze_function faddr store deps in
                Printf.printf "result: ";
                List.iter res ~f:(fun v -> Printf.printf "%s " (Value.to_string v));
                Printf.printf "\n");
            ()
          | Script.Encoded (x, y) -> Printf.printf "Encoded: %s\n------\n%s" x y
          | Script.Quoted (x, y) -> Printf.printf "Quoted: %s\n------\n%s" x y
        end
      ) in
  Printf.printf "Success? %b" (parse_file "examples/overflow/overflow.wat" run)
