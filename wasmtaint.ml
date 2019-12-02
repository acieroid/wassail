open Core
open Wasm
open Helpers

(* TODO: ([ ] = to start, [-] = started, [x] = finished)
  - [ ] Track taint
   - Tag values with their provenance (e.g., nth argument of function f)
   - Propagate taint values for data taint
   - Propagate taint values for control taint
  - [ ] Tests
  - [ ] Use apron for abstraction of values?

For later:
  - [ ] Improving analysis with a dependency graph
   -> This dependency graph can be dynamically discovered. Just start from exported functions, then analyze called functions.
   (Old text:
   What about having a graph of function dependencies.
   Exported functions are entry points.
   Direct calls are edges in that graph.
   Indirect calls will be dealth with later (if needed)
   Then we should go for solution a) below.
   1. Build a function call graph linearly: navigate through functions, each call is remembered as an edge (caller_idx, callee_idx)
     e.g., 0 calls 1, 1 calls 2, 2 calls 1, 3 calls 4.
     e.g., (overflow), 1 calls 0
   2. Filter functions that can't be reached from the exported functions.
     e.g., if only 0 is exported, remove (3, 4)
     e.g., (overflow) 1 is exported
   3. Compute a stratification of that graph
     e.g., Stratum 1: {0}, Stratum 2: {1,2}
     e.g., (overflow) Stratum 1: {1}, Stratum 2: {0}
   4. Finally, we can run the analysis on each stratum. Exported functions are analyzed with inputs set to Top. Other functions are called at some point and knowledge about their input has been refined in the previous stratum.
   5. We still need to fixpoint the entire analysis of the previous step in case the heap or global variables have been modified.)
  - [ ] Improve abstraction of the memory
  - [ ] Support for indirect function calls
  - [ ] Display results of the analysis nicely (how?)
  - [ ] Find benchmarks

For later:
  - other types (i64, f32, f64)
  - other instructions
*)

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
      Printf.sprintf "block%d [shape=ellipse, label = \"Block entry (%d)\"];" b.idx b.idx
    | BlockExit ->
      Printf.sprintf "block%d [shape=ellipse, label = \"Block exit (%d)\"];" b.idx b.idx
    | LoopEntry ->
      Printf.sprintf "block%d [shape=ellipse, label = \"Loop entry (%d)\"];" b.idx b.idx
    | LoopExit ->
      Printf.sprintf "block%d [shape=ellipse, label = \"Loop exit (%d)\"];" b.idx b.idx
    | Function ->
      Printf.sprintf "block%d [shape=star, label=\"Direct call(%d):\\n%s\"];"
        b.idx b.idx
        (String.concat ~sep:"\\l"
           (List.map b.instrs
              ~f:(fun instr ->
                  Printf.sprintf "%s" (Instr.to_string instr ~sep:"\\l"))))
    | Return ->
      Printf.sprintf "block%d [shape=point, label=\"%d\"]" b.idx b.idx
end

module CFG = struct
  type t = {
    (* Is this function exported or not? *)
    exported: bool;
    (* The index of this CFG *)
    idx: int;
    (* The number of parameters and return values of that CFG *)
    arity: (int * int);
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

  let callees (cfg : t) : IntSet.t =
    (* Loop through all the blocks of the cfg, collecting the targets of call instructions *)
    IntMap.fold cfg.basic_blocks ~init:IntSet.empty ~f:(fun ~key:_ ~data:block callees -> match block.sort with
        | Function -> begin match block.instrs with
            | Call n :: [] -> IntSet.union (IntSet.singleton n) callees
            | _ -> callees
          end
        | _ -> callees)

  let callers (cfgs : t IntMap.t) (cfg : t) : IntSet.t =
    IntMap.fold cfgs ~init:IntSet.empty ~f:(fun ~key:caller ~data:cfg' callers ->
        if IntSet.mem (callees cfg') cfg.idx then
          (* cfg' calls into cfg *)
          IntSet.union (IntSet.singleton caller) callers
        else
          callers)
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
    memory : memory;
    calls : (Value.t list) IntMap.t; (* A map of function called, from function index to parameters given *)
  }
  [@@deriving sexp, compare]

  let vstack_to_string (vstack : vstack) : string =
    String.concat ~sep:", " (List.map vstack ~f:Value.to_string)
  let locals_to_string (locals : locals) : string =
    String.concat ~sep:", " (List.mapi locals ~f:(fun i v -> Printf.sprintf "%d: %s" i (Value.to_string v)))
  let globals_to_string (globals : globals) : string =
    String.concat ~sep:", " (List.mapi globals ~f:(fun i v -> Printf.sprintf "%d: %s" i (Value.to_string v)))

  let to_string (s : state) : string =
    Printf.sprintf "{vstack: [%s], locals: [%s], globals: [%s]}"
      (vstack_to_string s.vstack)
      (locals_to_string s.locals)
      (globals_to_string s.globals)

  let init (args : Value.t list) (nlocals : int) (globals : globals) (memory : memory) = {
    vstack = [];
    locals = args @ (List.init nlocals ~f:(fun _ -> Value.zero I32Type));
    globals = globals;
    memory = memory;
    (* The list of calls is initially empty *)
    calls = IntMap.empty;
  }
  let join_globals (g1 : globals) (g2 : globals) : globals =
    Value.join_vlist_exn g1 g2
  let join_memory (_m1 : memory) (_m2 : memory) : memory =
    TODO

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
    globals = join_globals s1.globals s2.globals;
    memory = join_memory s1.memory s2.memory;
    calls = IntMap.merge s1.calls s2.calls ~f:(fun ~key:_ data -> match data with
        | `Both (a, b) -> Some (Value.join_vlist_exn a b)
        | `Left a -> Some a
        | `Right b -> Some b)
  }
  let join_opt (s1 : state option) (s2 : state option) : state option =
    match (s1, s2) with
    | Some s1, Some s2 -> Some (join s1 s2)
    | Some s1, None -> Some s1
    | None, Some s2 -> Some s2
    | None, None -> None
end

(* A function summary *)
module Summary = struct
  (* For now it just models the number of arguments taken by the function, and the result on the vstack (in practice, either 0 or 1 value) *)
  type t = {
    nargs : int;
    result : Value.t list;
  }
  let to_string (s : t) : string =
    Printf.sprintf "Sum(%d, %s)" s.nargs
      (String.concat ~sep:", " (List.map s.result ~f:Value.to_string))

  (* Constructs a summary given a CFG and a domain state resulting from that CFG *)
  let make (cfg : CFG.t) (state : Domain.state) : t = {
    nargs = fst cfg.arity;
    result = List.take state.vstack (snd cfg.arity);
  }

  (* Constructs an empty bottom summary given a CFG *)
  let bottom (cfg : CFG.t) : t = {
    nargs = fst cfg.arity;
    result = List.init (snd cfg.arity) ~f:(fun _ -> Value.bottom);
  }

  (* Apply the summary to a state, updating the vstack as if the function was
     called, AND updating the set of called functions *)
  let apply (sum : t) (fidx : Var.t) (st : Domain.state) : Domain.state =
    { st with
      vstack = sum.result @ (List.drop st.vstack sum.nargs );
      calls = IntMap.update st.calls fidx ~f:(function
          | None -> List.take st.vstack sum.nargs
          | Some vs -> Value.join_vlist_exn vs (List.take st.vstack sum.nargs))
    }
end

module Transfer = struct
  let rec instr_transfer (i : Instr.t) (state : Domain.state) : Domain.state =
    match i with
    | Nop ->
      state
    | Drop ->
      let (_, vstack') = Domain.pop state.vstack in
      assert (List.length vstack' = List.length state.vstack - 1);
      { state with vstack = vstack' }
    | LocalGet x ->
      let vstack' = (Domain.get_local state.locals x) :: state.vstack in
      assert (List.length vstack' = 1 + List.length state.vstack);
      { state with vstack = vstack' }
    | LocalSet x ->
      let (v, vstack') = Domain.pop state.vstack in
      assert (List.length vstack' = List.length state.vstack - 1);
      { state with vstack = vstack';
                   locals = Domain.set_local state.locals x v }
    | LocalTee x ->
      let (v, vstack') = Domain.pop state.vstack in
      let res = instr_transfer (LocalSet x) { state with vstack = v :: v :: vstack' } in
      assert (List.length res.vstack = List.length state.vstack);
      res
    | GlobalGet x ->
      let vstack' = (Domain.get_global state.globals x) :: state.vstack in
      assert (List.length vstack' = List.length state.vstack + 1);
      { state with vstack = vstack' }
    | GlobalSet x ->
      let (v, vstack') = Domain.pop state.vstack in
      assert (List.length vstack' = List.length state.vstack - 1);
      { state with vstack = vstack';
                   globals = Domain.set_global state.globals x v }
    | Br _ ->
      state
    | BrIf _ ->
      let (_, vstack') = Domain.pop state.vstack in
      assert (List.length vstack' = List.length state.vstack - 1);
      { state with vstack = vstack' }
    | Return -> state
    | Const v ->
      let vstack' = v :: state.vstack in
      assert (List.length vstack' = List.length state.vstack + 1);
      { state with vstack = vstack' }
    | Compare rel ->
      let (v1, vstack') = Domain.pop state.vstack in
      let (v2, vstack'') = Domain.pop vstack' in
      let v = Relop.eval rel v1 v2 in
      let vstack''' = v :: vstack'' in
      assert (List.length vstack''' = List.length state.vstack - 1);
      { state with vstack =  vstack''' }
    | Binary bin ->
      let (v1, vstack') = Domain.pop state.vstack in
      let (v2, vstack'') = Domain.pop vstack' in
      let v = Binop.eval bin v1 v2 in
      let vstack''' = v :: vstack'' in
      assert (List.length vstack''' = List.length state.vstack - 1);
      { state with vstack = vstack''' }
    | Test test ->
      let (v, vstack') = Domain.pop state.vstack in
      let v' = Testop.eval test v in
      let vstack'' = v' :: vstack' in
      assert (List.length vstack'' = List.length state.vstack);
      { state with vstack = vstack'' }
    | Load op ->
      (* TODO: for now, we just return the top value of the expected type *)
      let (_, vstack') = Domain.pop state.vstack in
      let c = Value.top_no_source op.typ in (* value of the correct type *)
      let vstack'' = c :: vstack' in
      assert (List.length vstack'' = List.length state.vstack);
      { state with vstack = vstack'' }
    | Store _op ->
      (* TODO: for now, we just ignore the store *)
      let (_, vstack') = Domain.pop state.vstack in
      let (_, vstack'') = Domain.pop vstack' in
      assert (List.length vstack'' = List.length state.vstack - 2);
      { state with vstack = vstack'' }
    | Block _ -> failwith "shouldn't happen"
    | Loop _ -> failwith "shouldn't happen"
    | Call _ -> failwith "shouldn't happen"

  let transfer (b : BasicBlock.t) (state : Domain.state) (summaries: Summary.t IntMap.t) : Domain.state =
    match b.sort with
    | Normal ->
      List.fold_left b.instrs ~init:state ~f:(fun acc i ->
          let res = instr_transfer i acc in
          res
        )
    | Function -> begin match b.instrs with
        | Call f :: [] ->
          (* We encounter a function call, retrieve its summary and apply it *)
          (* We assume all summaries are defined *)
          let summary = IntMap.find_exn summaries f in
          Summary.apply summary f state
        | _ -> failwith "Invalid function block"
      end
    | _ -> state
end

module IntraFixpoint = struct
  (* Analyzes a CFG. Returns a map where each basic blocks is mappped to its input state and output state *)
  let analyze (cfg : CFG.t) (args : Value.t list) (globals : Domain.globals) (memory : Domain.memory) (summaries : Summary.t IntMap.t) : (Domain.state * Domain.state) IntMap.t =
    let bottom = None in
    assert (List.length args = (fst cfg.arity)); (* Given number of arguments should match the in arity of the function *)
    let init = Domain.init args cfg.nlocals globals memory in
    let data = ref (IntMap.of_alist_exn (List.map (IntMap.keys cfg.basic_blocks)
                                           ~f:(fun idx ->
                                               (idx, (bottom, bottom))))) in
    let rec fixpoint (worklist : IntSet.t) (iteration : int) : unit =
      if IntSet.is_empty worklist then
        () (* No more elements to consider. We can stop here *)
      else
        let block_idx = IntSet.min_elt_exn worklist in
        let predecessors = CFG.predecessors cfg block_idx in
        (* in_state is the join of all the the out_state of the predecessors *)
        let in_state = Option.value (List.fold_left (List.map predecessors ~f:(fun idx -> snd (IntMap.find_exn !data idx))) ~init:bottom ~f:Domain.join_opt) ~default:init in
        (* The block to analyze *)
        let block = CFG.find_block_exn cfg block_idx in
        (* We analyze it *)
        let out_state = Transfer.transfer block in_state summaries in
        (* Has out state changed? *)
        let previous_out_state = snd (IntMap.find_exn !data block_idx) in
        match previous_out_state with
        | Some st when Domain.compare_state out_state st = 0 ->
          (* Didn't change, we can safely ignore the successors *)
          (* TODO: make sure that this is true. If not, maybe we just have to put all blocks on the worklist for the first iteration(s) *)
          fixpoint (IntSet.remove worklist block_idx) (iteration+1)
        | _ ->
          (* Update the out state in the analysis results, joining it with the previous one *)
          let new_out_state = Domain.join_opt (Some out_state) previous_out_state in
          data := IntMap.set !data ~key:block_idx ~data:(Some in_state, new_out_state);
          (* And recurse by adding all successors *)
          let successors = CFG.successors cfg block_idx in
          fixpoint (IntSet.union (IntSet.remove worklist block_idx) (IntSet.of_list successors)) (iteration+1)
    in
    fixpoint (IntSet.singleton cfg.entry_block) 1;
    IntMap.map !data ~f:(fun (in_state, out_state) -> (Option.value_exn in_state, Option.value_exn out_state))

  (* Similar to analyze, but only return the out state for a CFG *)
  let analyze_coarse (cfg : CFG.t) (args : Value.t list) (globals : Domain.globals) (memory : Domain.memory) (summaries : Summary.t IntMap.t) : Domain.state =
    let results = analyze cfg args globals memory summaries in
    snd (IntMap.find_exn results cfg.exit_block)
end

module InterFixpoint = struct
  (* Analyze multiple CFGS, returns a map from CFG id to out_state for each CFG *)
  let analyze (cfgs : CFG.t IntMap.t) (nglobals : int) : Domain.state IntMap.t =
    let data = ref (IntMap.of_alist_exn (List.map (IntMap.keys cfgs)
                                           ~f:(fun idx ->
                                               (idx, None)))) in
    let rec fixpoint (worklist : IntSet.t)
        (globals : Domain.globals) (memory : Domain.memory)
        (summaries : Summary.t IntMap.t) (calls : (Value.t list) IntMap.t) =
      if IntSet.is_empty worklist then
        () (* empty worklist, analysis finished *)
      else
        let cfg_idx = IntSet.min_elt_exn worklist in
        let cfg = IntMap.find_exn cfgs cfg_idx in
        let args = match IntMap.find calls cfg_idx with
          | Some _ when cfg.exported ->
            (* We have stored specific arguments, but this function is exported so it can be called with any argument *)
            List.init (fst cfg.arity) ~f:(fun i -> Value.top Type.I32Type (cfg.idx, i))
          | Some args ->
            (* Function is not exported, so it can only be called with what we discovered *)
            args
          | None when cfg.exported ->
            (* No call has been analyzed yet, and this function is exported, so we start from top *)
            List.init (fst cfg.arity) ~f:(fun i -> Value.top Type.I32Type (cfg.idx, i))
          | None ->
            (* No call analyzed, function is not called from anywhere, use bottom as arguments *)
            List.init (fst cfg.arity) ~f:(fun _ -> Value.bottom) in
        Printf.printf "Analyzing cfg %d with globals: [%s] and args: [%s]\n" cfg_idx (Domain.globals_to_string globals) (Value.list_to_string args);
        let out_state = IntraFixpoint.analyze_coarse cfg args globals memory summaries in
        let previous_out_state = IntMap.find_exn !data cfg_idx in
        match previous_out_state with
        | Some st when Domain.compare_state out_state st = 0 ->
          (* Same results as before, we can just recurse without having to recompute globals nor memory nor calls *)
          fixpoint (IntSet.remove worklist cfg_idx) globals memory summaries calls
        | _ ->
          (* Result differed, we have to add all callees and callers to the worklist.
             Callers because the analyzed function could have modified globals/memory that will be read by the caller.
             Callees for the same reason. *)
          let callees = CFG.callees cfg in
          let callers = CFG.callers cfgs cfg in
          let new_globals = Domain.join_globals globals out_state.globals in
          let new_memory = Domain.join_memory memory out_state.memory in
          let summary = Summary.make cfg out_state in
          let new_summaries = IntMap.set summaries ~key:cfg.idx ~data:summary in
          let new_calls = IntMap.merge calls out_state.calls ~f:(fun ~key:_ data -> match data with
              | `Both (a, b) -> Some (Value.join_vlist_exn a b)
              | `Left a -> Some a
              | `Right b -> Some b) in
          data := IntMap.set !data ~key:cfg_idx ~data:(Some out_state);
          fixpoint (IntSet.union (IntSet.remove worklist cfg_idx) (IntSet.union callees callers)) new_globals new_memory new_summaries new_calls
    in
    let summaries0 = IntMap.map cfgs ~f:(fun cfg -> Summary.bottom cfg) in
    let calls0 = IntMap.empty in
    fixpoint (IntSet.of_list (IntMap.keys cfgs)) (List.init nglobals ~f:(fun _ -> Value.zero Type.I32Type)) TODO summaries0 calls0 ;
    IntMap.map !data ~f:(fun v -> match v with
        | Some result -> result
        | None -> failwith "...")
end

module CFGBuilder = struct
  let build (faddr : Address.t) (store : Store.t) : CFG.t =
    let funcinst = Store.get_funcinst store faddr in
    let cur_idx : int ref = ref 0 in
    let new_idx () : int = let v = !cur_idx in cur_idx := v + 1; v in
    let mk_block (reverse_instrs : Instr.t list) : BasicBlock.t =
      let instrs = List.rev reverse_instrs in
      BasicBlock.{ idx = new_idx (); instrs = instrs; sort = BasicBlock.Normal; } in
    let mk_funblock (f : Address.t) : BasicBlock.t =
      BasicBlock.{ idx = new_idx () ; instrs = [Call f]; sort = Function } in
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
        let block = mk_block (instrs) in
        let fblock = mk_funblock f in
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
      (* Exported functions have names, non-exported don't *)
      exported = Option.is_some funcinst.name;
      (* The index of this block is the integer that represent the address of this function *)
      idx = faddr;
      (* Arity of the function *)
      arity = funcinst.arity;
      (* Number of locals in the function *)
      nlocals = List.length funcinst.code.locals;
      (*The basic blocks *)
      basic_blocks = IntMap.of_alist_exn (List.map actual_blocks ~f:(fun b -> (b.idx, b)));
      (* The forward edges *)
      edges = IntMap.of_alist_multi actual_edges;
      (* The backward edges *)
      back_edges = IntMap.of_alist_multi (List.map actual_edges ~f:(fun (left, right) -> (right, left)));
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
          let nglobals = List.length store.globals in
          let cfgs = IntMap.of_alist_exn (List.mapi store.funcs ~f:(fun faddr _ -> (faddr, CFGBuilder.build faddr store))) in
          IntMap.iter cfgs ~f:(fun cfg ->
              Printf.printf "CFG for function %d\n" cfg.idx;
              Printf.printf "---------------\n%s\n---------------\n" (CFG.to_dot cfg)
            );
          let results = InterFixpoint.analyze cfgs nglobals in
          Printf.printf "--------- Results ---------\n";
          IntMap.iteri results ~f:(fun ~key:cfg_idx ~data:res ->
              Printf.printf "Results for function %d: %s\n" cfg_idx (Domain.to_string res))
        | Script.Encoded _ -> failwith "unsupported"
        | Script.Quoted _ -> failwith "unsupported"
      ) in
  Printf.printf "Success? %b" (parse_file "examples/overflow/overflow.wat" run)

let () = run_cfg ()


