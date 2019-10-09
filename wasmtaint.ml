open Core
open Wasm

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
    type value_type =
      | I32Type
    [@@deriving sexp, compare]
    type binop =
      | I32Add
    [@@deriving sexp, compare]
    type relop =
      | I32LeS
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

module Store = struct
  module T = struct
    type func = {
      typ : int; (* TODO: u32 *)
      locals : Instr.value_type list;
      body : Instr.t list;
    }
    [@@deriving sexp, compare]
    type funcaddr = Address.t
    [@@deriving sexp, compare]
    type moduleinst = {
      funcaddrs: funcaddr list;
      (* TODO: other fields *)
    }
    [@@deriving sexp, compare]
    type funcinst = {
      arity : (int * int);
      typ : (Instr.value_type list * Instr.value_type list);
      module_: moduleinst;
      code: func
    }
    [@@deriving sexp, compare]
    type t = {
      funcs : funcinst list;
      (* TODO: other fields *)
    }
    [@@deriving sexp, compare]
  end
  include T
  let get_funcinst (s : t) (a : Address.t) : funcinst =
    List.nth_exn s.funcs a
  let join (s1 : t) (s2 : t) : t =
    assert (s1.funcs = s2.funcs);
    s1
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
  let funcaddr (f : t) (fn : Instr.var) : Address.t =
    List.nth_exn f.module_.funcaddrs fn
  let get_local (f : t) (l : Instr.var) : Value.t =
    List.nth_exn f.locals l
  let set_local (f : t) (l : Instr.var) (v : Value.t) : t =
    { f with locals = List.mapi f.locals ~f:(fun i x -> if i = l then v else x) }
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
    (* Most of the time, a block either reaches its final state or a return statement. Due to joining, a block analysis could reach both *)
    type block_result = {
      configuration: Configuration.t option;
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
      returned = join_conf_opt b1.returned b2.returned }
end

(* Structure of the execution:
Store ; Frame ; Instructions -> Store'; Frame'; Instructions'
   where Instructions is isomorphic to the stack *)
module FunctionAnalysis = struct
  let eval_relop (r : Instr.relop) (v1 : Value.t) (v2 : Value.t) : Value.t =
    match (r, v1, v2) with
    | (I32LeS, Const n1, Const n2) -> Const (if n1 <= n2 then 1l else 0l)
    | (I32LeS, _, _) -> Int
  let eval_binop (b : Instr.binop) (v1 : Value.t) (v2 : Value.t) : Value.t =
    match (b, v1, v2) with
    | (I32Add, Const n1, Const n2) -> Const (Int32.(+) n1 n2)
    | (I32Add, _, _) -> Int

  type step_result =
    | Configurations of Configuration.Set.t
    | Configuration of Configuration.t
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
            | Finished c ->
              run_analysis todo' visited' (Deps.join_block_results current
                                             { configuration = Some c;
                                               returned = None })
            | Returned c ->
              run_analysis todo' visited' (Deps.join_block_results current
                                             { configuration = None;
                                               returned = Some c })
          else
            run_analysis todo' visited current
      in
      run_analysis [conf] (Configuration.Set.empty) { configuration = None; returned = None }
  and invoke (funcaddr : Address.t) (vstack : Value.t list) (store : Store.t) (deps : Deps.t) : (Value.t list * int) =
    let f = Store.get_funcinst store funcaddr in
    let (in_arity, out_arity) = f.arity in
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
    (List.take exit_conf.vstack out_arity, in_arity)

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
        | TeeLocal x, v :: vstack ->
          (* [spec] Assert: due to validation, a value is on the top of the stack.
             Pop the value val from the stack.
             Push the value val to the stack.
             Push the value val to the stack.
             Execute the instruction (local.set x). *)
          step { config with vstack = v :: v :: vstack; astack = SetLocal x :: astack' } deps
        | Block (_st, instrs), _ ->
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
            | None -> failwith "no block result"
          end
        | Call x, vstack ->
          (* [spec] Let F be the current frame.
             Assert: due to validation, F.module.funcaddrs[x] exists.
             Let a be the function address F.module.funcaddrs[x].
             Invoke the function instance at address a. *)
          let funcaddr = Frame.funcaddr config.frame x in
          (* Invoke the function, get a list of return values *)
          let (return_vs, in_arity) = invoke funcaddr vstack config.store deps in
          Configuration { config with vstack = return_vs @ List.drop vstack in_arity }
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
          let v = eval_relop rel v1 v2 in
          Configuration
            { config with vstack = v :: vstack; astack = astack' }
        | Binary bin, v2 :: v1 :: vstack ->
          (* [spec] Assert: due to validation, two values of value type t are on the top of the stack.
             Pop the value t.const c2 from the stack.
             Pop the value t.const c1 from the stack.
             If binopt(c1,c2) is defined, then:
               Let c be a possible result of computing binopt(c1,c2).
               Push the value t.const c to the stack.
             Else:
               Trap. *)
          let v = eval_binop bin v1 v2 in (* TODO: trap *)
          Configuration
            { config with vstack = v :: vstack; astack = astack' }
        | _ -> failwith "invalid configuration"
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
