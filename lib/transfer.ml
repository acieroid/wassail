open Core_kernel
open Helpers

(** The result of applying the transfer function. *)
type result =
  | Uninitialized (** Meaning it has not been computed yet *)
  | Simple of Domain.state (** A single successor *)
  | Branch of Domain.state * Domain.state (** Upon a brif, there are two successor states: one where the condition holds, and where where it does not hold. This is used to model that. *)
[@@deriving sexp, compare]

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
    assert (Option.is_empty op.sz); (* We only support N = 32 for now *)
    let memory' = Memory.store state.memory i.value c op in
    assert (List.length vstack'' = List.length state.vstack - 2);
    { state with vstack = vstack''; memory = memory' }

let control_instr_transfer (i : Instr.control) (state : Domain.state) (summaries : Summary.t IntMap.t) : result =
  match i with
  | Call f ->
      (* We encounter a function call, retrieve its summary and apply it *)
      (* We assume all summaries are defined *)
      let summary = IntMap.find_exn summaries f in
      Simple (Summary.apply summary f state)
  | Br _ ->
    Simple state
  | BrIf _ ->
    let (cond, vstack') = Vstack.pop state.vstack in
    assert (List.length vstack' = List.length state.vstack - 1);
    Branch ({ state with vstack = vstack'; memory = Memory.refine state.memory cond true },
            { state with vstack = vstack'; memory = Memory.refine state.memory cond false })
  | Return ->
    Simple state
  | _ -> failwith "TODO"

let transfer (b : Basic_block.t) (state : Domain.state) (summaries : Summary.t IntMap.t) : result =
  match b.content with
  | Data instrs ->
    Simple (List.fold_left instrs ~init:state ~f:(fun prestate i ->
         let poststate = data_instr_transfer i prestate in
         Printf.printf "pre: %s\ninstr: %s\n" (Domain.to_string prestate) (Instr.data_to_string i);
         poststate))
  | Control instr -> control_instr_transfer instr state summaries
  | Nothing -> Simple state

