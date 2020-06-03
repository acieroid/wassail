open Core_kernel
open Helpers

(** The result of applying the transfer function. *)
type result =
  | Uninitialized (** Meaning it has not been computed yet *)
  | Simple of Domain.state (** A single successor *)
  | Branch of Domain.state * Domain.state (** Upon a brif, there are two successor states: one where the condition holds, and where where it does not hold. This is used to model that. *)
[@@deriving sexp, compare]

let result_to_string (r : result) : string = match r with
  | Uninitialized -> "uninitialized"
  | Simple st -> Domain.to_string st
  | Branch (st1, st2) -> Printf.sprintf "branch:\n%s\n%s" (Domain.to_string st1) (Domain.to_string st2)

let join_result (r1 : result) (r2 : result) =
  match (r1, r2) with
  | Uninitialized, _ -> r2
  | _, Uninitialized -> r1
  | Simple st1, Simple st2 -> Simple (Domain.join st1 st2)
  | Branch (st1, st2), Branch (st1', st2') -> Branch (Domain.join st1 st1', Domain.join st2 st2')
  | _ -> failwith "Cannot join results"

(* Transfer function for data instructions.
   Takes as argument the instruction `i`, and the state before the instruction (prestate).
   Returns the resulting state (poststate).
*)
let rec data_instr_transfer (i : Instr.data) (state : Domain.state) : Domain.state =
  match i with
  | Nop -> state
  | Drop ->
    let (_, vstack') = Vstack.pop state.vstack in
    assert (List.length vstack' = List.length state.vstack - 1);
    { state with vstack = vstack' }
  | Select ->
    let (c, vstack') = Vstack.pop state.vstack in
    let (v2, vstack'') = Vstack.pop vstack' in
    let (v1, vstack''') = Vstack.pop vstack'' in
    begin match (Value.is_zero c, Value.is_not_zero c) with
      | (true, false) -> (* definitely 0, keep v2 *)
        { state with vstack = v2 :: vstack''' }
      | (false, true) -> (* definitely not 0, keep v1 *)
        { state with vstack = v1 :: vstack''' }
      | (true, true) -> (* could be 0 or not 0, join v1 and v2 *)
        { state with vstack = (Value.join v1 v2) :: vstack''' }
      | (false, false) -> (* none, push bottom *)
        { state with vstack = Value.bottom v1.typ :: vstack''' }
    end
  | LocalGet x ->
    let vstack' = (Locals.get_local state.locals x) :: state.vstack in
    assert (List.length vstack' = 1 + List.length state.vstack);
    { state with vstack = vstack' }
  | LocalSet x ->
    let (v, vstack') = Vstack.pop state.vstack in
    assert (List.length vstack' = List.length state.vstack - 1);
    { state with vstack = vstack';
                 locals = Locals.set_local state.locals x v }
  | LocalTee x ->
    let (v, vstack') = Vstack.pop state.vstack in
    data_instr_transfer (LocalSet x) { state with vstack = v :: v :: vstack' }
  | GlobalGet x ->
    let vstack' = (Globals.get_global state.globals x) :: state.vstack in
    assert (List.length vstack' = List.length state.vstack + 1);
    { state with vstack = vstack' }
  | GlobalSet x ->
    let (v, vstack') = Vstack.pop state.vstack in
    assert (List.length vstack' = List.length state.vstack - 1);
    { state with vstack = vstack';
                 globals = Globals.set_global state.globals x v }
  | Const v ->
    let vstack' = v :: state.vstack in
    assert (List.length vstack' = List.length state.vstack + 1);
    { state with vstack = vstack' }
  | Compare rel ->
    let (v2, vstack') = Vstack.pop state.vstack in
    let (v1, vstack'') = Vstack.pop vstack' in
    let v = Relop.eval rel v1 v2 in
    let vstack''' = v :: vstack'' in
    assert (List.length vstack''' = List.length state.vstack - 1);
    { state with vstack =  vstack''' }
  | Binary bin ->
    let (v2, vstack') = Vstack.pop state.vstack in
    let (v1, vstack'') = Vstack.pop vstack' in
    let v = Binop.eval state.memory bin v1 v2 in
    let vstack''' = v :: vstack'' in
    assert (List.length vstack''' = List.length state.vstack - 1);
    { state with vstack = vstack''' }
  | Test test ->
    let (v, vstack') = Vstack.pop state.vstack in
    let v' = Testop.eval test v in
    let vstack'' = v' :: vstack' in
    assert (List.length vstack'' = List.length state.vstack);
    { state with vstack = vstack'' }
  | Load op ->
    let (i, vstack') = Vstack.pop state.vstack in
    let c = Memory.load state.memory i.value op in
    let state' = { state with vstack = c :: vstack' } in
    assert (List.length state'.vstack = List.length state.vstack);
    state'
  | Store op ->
    (* Pop the value t.const c from the stack *)
    let (c, vstack') = Vstack.pop state.vstack in
    (* Pop the value i32.const i from the stack *)
    let (i, vstack'') = Vstack.pop vstack' in
    (* let b be the byte sequence of c *)
    let memory' = Memory.store state.memory i.value c op in
    assert (List.length vstack'' = List.length state.vstack - 2);
    { state with vstack = vstack''; memory = memory' }

let control_instr_transfer
    (i : Instr.control) (* The instruction *)
    (state : Domain.state) (* The pre state *)
    (summaries : Summary.t IntMap.t) (* Summaries to apply function calls *)
    (module_ : Wasm_module.t) (* The wasm module (read-only) *)
    (cfg : Cfg.t) (* The CFG analyzed *)
  : result =
  match i with
  | Call f ->
      (* We encounter a function call, retrieve its summary and apply it *)
      (* We assume all summaries are defined *)
      let summary = IntMap.find_exn summaries f in
      Simple (Summary.apply summary f state module_)
  | CallIndirect typ ->
    let (v, vstack') = Vstack.pop state.vstack in
    let state' = { state with vstack = vstack' } in
    let table = List.nth_exn module_.tables 0 in
    let funs = Table_inst.get_subsumed_by_index table v in
    let resulting_state = List.fold_left funs ~init:None ~f:(fun acc f ->
        match f with
        | Some fa ->
          Printf.printf "fa:%d\n" fa;
          if Stdlib.(List.nth module_.types typ = (List.nth module_.funcs fa).typ) then
            (* Types match, apply the summary *)
            let summary = IntMap.find_exn summaries fa in
            Domain.join_opt (Summary.apply summary fa state' module_) acc
          else
            (* Types don't match, can't apply this function so we ignore it *)
            acc
        | None -> acc) in
    begin match resulting_state with
      | Some st -> Simple st
      | None -> failwith "call_indirect cannot resolve call"
    end
  | Br _ ->
    Simple state
  | BrIf _ ->
    let (cond, vstack') = Vstack.pop state.vstack in
    assert (List.length vstack' = List.length state.vstack - 1);
    Branch ({ state with vstack = vstack'; memory = Memory.refine state.memory cond true },
            { state with vstack = vstack'; memory = Memory.refine state.memory cond false })
  | If _ ->
    (* If is similar to br_if, and the control flow is handled by the CFG visitor.
       We only need to propagate the right state in the right branch, just like br_if *)
    let (cond, vstack') = Vstack.pop state.vstack in
    assert (List.length vstack' = List.length state.vstack - 1);
    Branch ({ state with vstack = vstack'; memory = Memory.refine state.memory cond true },
            { state with vstack = vstack'; memory = Memory.refine state.memory cond false })
  | Return ->
    (* return only keeps the necessary number of values from the stack *)
    let arity = List.length cfg.return_types in
    Simple { state with vstack = List.take state.vstack arity }
  | Unreachable ->
    (* Unreachable, so what we return does not really matter *)
    Simple { state with vstack = [] }
  | _ -> failwith (Printf.sprintf "Unsupported control instruction: %s" (Instr.control_to_string i))

let transfer (b : Basic_block.t) (state : Domain.state) (summaries : Summary.t IntMap.t) (module_ : Wasm_module.t) (cfg : Cfg.t) : result =
  match b.content with
  | Data instrs ->
    Simple (List.fold_left instrs ~init:state ~f:(fun prestate i ->
        (* Printf.printf "pre: %s\ninstr: %s\n" (Domain.to_string prestate) (Instr.data_to_string i); *)
        let poststate = data_instr_transfer i prestate in
        poststate))
  | Control instr ->
    (* Printf.printf "pre: %s\ncontrol: %s\n" (Domain.to_string state) (Instr.control_to_string instr); *)
    control_instr_transfer instr state summaries module_ cfg
  | Nothing -> Simple state
