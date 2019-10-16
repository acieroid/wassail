open Core
open Wasm

module Type = struct
  module T = struct
    type t =
      | I32Type (* TODO: other types *)
    [@@deriving sexp, compare]
  end
  include T

  let of_wasm (vt : Types.value_type) : t =
    match vt with
    | Types.I32Type -> I32Type
    | Types.I64Type -> failwith "unsupported type: I64Type"
    | Types.F32Type -> failwith "unsupported type: F32Type"
    | Types.F64Type -> failwith "unsupported type: F64Type"
end

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

  let of_wasm (v : Values.value) : t =
    match v with
    | I32 x -> Const x
    | I64 _ -> failwith "unsupported type: I64"
    | F32 _ -> failwith "unsupported type: F32"
    | F64 _ -> failwith "unsupported type: F64"

  let to_string (v : t) : string =
    Sexp.to_string [%sexp (v : t)]

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
  let is_definitely_zero (v : t) =
    match v with
    | Const 0l -> true
    | _ -> false
  let is_not_zero (v : t) =
    match v with
    | Const 0l -> false
    | _ -> true
  let is_definitely_not_zero (v : t) =
    match v with
    | Const n -> n <> 0l
    | _ -> false
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

module Var = struct
  module T = struct
    type t = int
    [@@deriving sexp, compare]
  end
  include T
  let of_wasm (v : Ast.var) : t = Int32.to_int_exn v.it
end

module Binop = struct
  module T = struct
    type t =
      | I32Add
      | I32Sub
      | I32Mul
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

  let eval (b : t) (v1 : Value.t) (v2 : Value.t) : Value.t =
    match (b, v1, v2) with
    | (I32Add, Const n1, Const n2) -> Const (Int32.(+) n1 n2)
    | (I32Add, _, _) -> Int
    | (I32Sub, Const n1, Const n2) -> Const (Int32.(-) n1 n2)
    | (I32Sub, _, _) -> Int
    | (I32Mul, Const n1, Const n2) -> Const (Int32.( * ) n1 n2)
    | (I32Mul, _, _) -> Int
end

module Relop = struct
  module T = struct
    type t =
      | I32LeS
    [@@deriving sexp, compare]
  end
  include T
  let of_wasm (r : Ast.relop) : t =
    match r with
    | I32 LeS -> I32LeS
    | _ -> failwith "unsupported type"
  let eval (r : t) (v1 : Value.t) (v2 : Value.t) : Value.t =
    match (r, v1, v2) with
    | (I32LeS, Const n1, Const n2) -> Const (if n1 <= n2 then 1l else 0l)
    | (I32LeS, _, _) -> Int
end

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
      | GetLocal of Var.t
      | SetLocal of Var.t
      | TeeLocal of Var.t
      | GetGlobal of Var.t
      | SetGlobal of Var.t
      | Call of Var.t
      | Br of Var.t
      | BrIf of Var.t
      | Return
    [@@deriving sexp, compare]
  end
  include T

  let rec of_wasm (i : Ast.instr) : t =
    match i.it with
    | Ast.Nop -> Nop
    | Ast.Drop -> Drop
    | Ast.Block (_st, instrs) ->
      Block (List.map instrs ~f:of_wasm)
    | Ast.Const lit -> Const (Value.of_wasm lit.it)
    | Ast.Binary bin -> Binary (Binop.of_wasm bin)
    | Ast.Compare rel -> Compare (Relop.of_wasm rel)
    | Ast.GetLocal v -> GetLocal (Var.of_wasm v)
    | Ast.SetLocal v -> SetLocal (Var.of_wasm v)
    | Ast.TeeLocal v -> TeeLocal (Var.of_wasm v)
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
    | Ast.GetGlobal v -> GetGlobal (Var.of_wasm v)
    | Ast.SetGlobal v -> SetGlobal (Var.of_wasm v)
    | Ast.Load _op -> failwith "unsupported instruction: load"
    | Ast.Store _op -> failwith "unsupported instruction: store"
    | Ast.CurrentMemory -> failwith "unsupported instruction: current memory"
    | Ast.GrowMemory -> failwith "unsupported instruction: memory grow"
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
      (* TODO: other fields *)
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
      (* TODO: other fields *)
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
      (* TODO: we probably don't want the frame and the store as part of the config, or only as a reference *)
      store : Store.t;
      frame : Frame.t;
      vstack : Value.t list;
      astack : Instr.t list;
    }
    [@@deriving sexp, compare]
  end
  include T
  module Set = Set.Make(T)
  module Map = Map.Make(T)
  let join (c1 : t) (c2 : t) =
    (* We're not supposed to join configurations with non-empty administrative stacks *)
    assert (c1.astack = [] && c2.astack = []);
    { store = Store.join c1.store c2.store;
      frame = Frame.join c1.frame c2.frame;
      vstack = List.map2_exn c1.vstack c2.vstack ~f:Value.join;
      astack = [] }
end

module Deps = struct
  module T = struct
    (* Most of the time, a block either reaches its final state, a return statement, or a break. Due to joining, a block analysis could reach multiple of these *)
    type block_result = {
      configuration: Configuration.t option;
      breaks: (Var.t (* The block we break to *) * Configuration.t (* The configuration before the break *)) list; (* there could be different breaks reached *)
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
  let empty = {
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
      breaks = b1.breaks @ b2.breaks (* TODO: use sets to avoid duplicates *)
    }
end

(* Structure of the execution:
Store ; Frame ; Instructions -> Store'; Frame'; Instructions'
   where Instructions is isomorphic to the stack *)
module FunctionAnalysis = struct
  type step_result =
    | Configurations of Configuration.Set.t
    | Configuration of Configuration.t
    | Break of int * Configuration.t
    | BreakCond of (((* the break *) int * Configuration.t) * (* the next state *) Configuration.t)
    | Finished of Configuration.t
    | Returned of Configuration.t
  [@@deriving sexp, compare]

  (* Analyzes a block. Return the configuration at the exit of a block. This resulting configuration is the join of all reachable configurations. The block could also have "returned" if it reaches a "Return" instruction *)
  let rec analyze_block (conf : Configuration.t) (deps : Deps.t) : Deps.block_result =
    match Configuration.Map.find !(deps.results) conf  with
    | Some res ->
      (* results already computed, return it *)
      res
    | None ->
      (* compute result and cache it *)
      let rec run_analysis (todo: Configuration.t list) (visited : Configuration.Set.t) (current: Deps.block_result) : Deps.block_result =
        match todo with
        | [] -> current
        | c :: todo' ->
          if not (Configuration.Set.mem visited c) then
            let visited' = Configuration.Set.add visited c in
            match step c deps with
            | Configurations cs ->
              run_analysis (Configuration.Set.fold cs ~init:todo' ~f:(fun acc c -> c :: acc)) visited' current
            | Configuration c ->
              run_analysis (c :: todo') visited' current
            | Break (v, c) ->
              run_analysis todo' visited' (Deps.join_block_results current
                                             { configuration = None; returned = None; breaks = [(v, c)] })
            | BreakCond ((v, c1), c2) ->
              run_analysis (c2 :: todo) visited' (Deps.join_block_results current
                                                    { configuration = None; returned = None; breaks = [(v, c1)] })
            | Finished c ->
              run_analysis todo' visited' (Deps.join_block_results current
                                             { configuration = Some c; returned = None; breaks = [] })
            | Returned c ->
              run_analysis todo' visited' (Deps.join_block_results current
                                             { configuration = None; returned = Some c; breaks = [] })
          else
            run_analysis todo' visited current
      in
      run_analysis [conf] (Configuration.Set.empty) { configuration = None; returned = None; breaks = [] }
  and invoke (funcaddr : Address.t) (vstack : Value.t list) (store : Store.t) (deps : Deps.t) : (Value.t list * int) =
    let f = Store.get_funcinst store funcaddr in
    let (in_arity, _) = f.arity in
    let valn = List.take vstack in_arity in
    let zeros = List.map f.code.locals ~f:(function I32Type -> Value.Const 0l) in
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
    (* TODO: functions without result have an empty return stack *)
    (List.take exit_conf.vstack in_arity, in_arity)

  (* Step a configuration by one instruction.
     Only recursive for specific rewrite case (e.g. TeeLocal is expressed in terms of SetLocal *)
  and step (config : Configuration.t) (deps : Deps.t) : step_result =
      match config.astack with
      | [] -> Finished config
      | head :: astack' -> match (head, config.vstack) with
        | Nop, _ ->
          (* [spec] Do nothing *)
          Configuration
            { config with astack = astack' }
        | Drop, _ :: vrest ->
          (* [spec] Assert: due to validation, a value is on the top of the stack.
             Pop the value val from the stack. *)
          Configuration
            { config with vstack = vrest; astack = astack' }
        | Drop, _ ->
          failwith "Invalid value stack for drop"
        | GetLocal x, vstack ->
          (* [spec] Let F be the current frame.
             Assert: due to validation, F.locals[x] exists.
             Let val be the value F.locals[x].
             Push the value val to the stack. *)
          let v = Frame.get_local config.frame x in
          Configuration
            { config with vstack = v :: vstack; astack = astack' }
        | SetLocal x, v :: vstack ->
          (* [spec] Let F be the current frame.
             Assert: due to validation, F.locals[x] exists.
             Assert: due to validation, a value is on the top of the stack.
             Pop the value val from the stack.
             Replace F.locals[x] with the value val. *)
          let frame' = Frame.set_local config.frame x v in
          Configuration
            { config with vstack = vstack; astack = astack'; frame = frame' }
        | SetLocal _, _ -> failwith "Invalid value stack for setlocal"
        | TeeLocal x, v :: vstack ->
          (* [spec] Assert: due to validation, a value is on the top of the stack.
             Pop the value val from the stack.
             Push the value val to the stack.
             Push the value val to the stack.
             Execute the instruction (local.set x). *)
          step { config with vstack = v :: v :: vstack; astack = SetLocal x :: astack' } deps
        | TeeLocal _, _ ->
          failwith "Invalid value stack for teelocal"
        | Block instrs, _ ->
          (* [spec] Let n be the arity |t?| of the result type t?.
             Let L be the label whose arity is n and whose continuation is the end of the block.
             Enter the block instr∗ with label L. *)
          (* We empty the administrative stack before analyzing the block, as we want the block to be analyzed up to its end *)
          let analysis_result = analyze_block { config with astack = instrs } deps in
          (* TODO: we should change the return value of this function (similar to block_result) *)
          (* TODO: for now, we'll just assert that either you return, or you step *)
          assert (analysis_result.configuration = None || analysis_result.returned = None);
          begin match analysis_result.configuration with
          | Some c ->
                (* After analyzing the block, the astack should be empty *)
                assert (c.astack = []);
                (* And the vstack should have exactly the form of st *)
                (* But we don't check that (yet, TODO). *)
                (* We restore the administrative stack after analyzing the block *)
                Configuration { c with astack = astack' }
          | None -> match analysis_result.returned with
            | Some c -> Returned c
            | None -> failwith "no block result" (* TODO: break *)
          end
        | Loop instrs, _ ->
          let _analysis_result = analyze_block { config with astack = instrs @ [Loop instrs] } deps in
          failwith "TODO: same as block"
        | Call x, vstack ->
          (* [spec] Let F be the current frame.
             Assert: due to validation, F.module.funcaddrs[x] exists.
             Let a be the function address F.module.funcaddrs[x].
             Invoke the function instance at address a. *)
          let funcaddr = Frame.funcaddr config.frame x in
          (* Invoke the function, get a list of return values *)
          let (return_vs, arity) = invoke funcaddr vstack config.store deps in
          Configuration { config with vstack = return_vs @ List.drop vstack arity }
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
          Returned { config with astack = [] }
        | Const v, vstack ->
          (* [spec] Push the value t.const c to the stack. *)
          Configuration { config with vstack = v :: vstack; astack = astack' }
        | Compare rel, v2 :: v1 :: vstack ->
          (* [spec] Assert: due to validation, two values of value type t are on the top of the stack.
             Pop the value t.const c2 from the stack.
             Pop the value t.const c1 from the stack.
             Let c be the result of computing relopt(c1,c2).
             Push the value i32.const c to the stack. *)
          let v = Relop.eval rel v1 v2 in
          Configuration
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
          Configuration
            { config with vstack = v :: vstack; astack = astack' }
        | Binary _, _ -> failwith "Invalid value stack for binary"
        | Test test, v :: vstack ->
          (* [spec] Assert: due to validation, a value of value type t is on the top of the stack.
             Pop the value t.const c1 from the stack.
             Let c be the result of computing testopt(c1).
             Push the value i32.const c to the stack. *)
          let v' = Testop.eval test v in
          Configuration
            { config with vstack = v' :: vstack; astack = astack' }
        | GetGlobal v, vstack ->
          (* [spec] Let F be the current frame.
             Assert: due to validation, F.module.globaladdrs[x] exists.
             Let a be the global address F.module.globaladdrs[x].
             Assert: due to validation, S.globals[a] exists.
             Let glob be the global instance S.globals[a].
             Let val be the value glob.value.
             Push the value val to the stack. *)
          let addr = Frame.get_global_addr config.frame v in
          let g = Store.get_global config.store addr in
          Configuration
            { config with vstack = g.value :: vstack; astack = astack' }
        | SetGlobal _, _ -> failwith "TODO: get global"
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
          Break (n, { config with astack = astack' })
        | BrIf n, v :: vstack ->
          (* [spec] Assert: due to validation, a value of value type i32 is on the top of the stack.
             Pop the value i32.const c from the stack.
             If c is non-zero, then:
               Execute the instruction (br l).
             Else:
               Do nothing. *)
          let config' = { config with vstack = vstack; astack = astack' } in
          if Value.is_definitely_zero v then
            Configuration config'
          else if Value.is_definitely_not_zero v then
            Break (n, config')
          else
            BreakCond ((n, config'), config')
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
                       | GetLocal _ -> Printf.printf "localget\n"
                       | SetLocal _ -> Printf.printf "localset\n"
                       | TeeLocal _ -> Printf.printf "localtee\n"
                       | GetGlobal _ -> Printf.printf "globalget\n"
                       | SetGlobal _ -> Printf.printf "globalset\n"
                       | Load _ -> Printf.printf "load\n"
                       | Store _ -> Printf.printf "store\n"
                       | CurrentMemory -> Printf.printf "memorysize\n"
                       | GrowMemory -> Printf.printf "memorygrow\n"
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
  Printf.printf "Success? %b" (parse_file "foo.wat" run)
