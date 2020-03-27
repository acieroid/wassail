open Core_kernel
open Helpers

(* Transfer function for an instruction.
   Takes as argument the instruction `i`, and the state before the instruction (prestate).
   Returns the resulting state (poststate).
*)
let rec instr_transfer (i : Instr.t) (state : Domain.state) : Domain.state =
  match i with
  | Nop ->
    state
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
    let state' = instr_transfer (LocalSet x) { state with vstack = v :: v :: vstack' } in
    assert (List.length state'.vstack = List.length state.vstack);
    state'
  | GlobalGet x ->
    let vstack' = (Globals.get_global state.globals x) :: state.vstack in
    assert (List.length vstack' = List.length state.vstack + 1);
    { state with vstack = vstack' }
  | GlobalSet x ->
    let (v, vstack') = Vstack.pop state.vstack in
    assert (List.length vstack' = List.length state.vstack - 1);
    { state with vstack = vstack';
                 globals = Globals.set_global state.globals x v }
  | Br _ ->
    state
  | BrIf _ ->
    let (_, vstack') = Vstack.pop state.vstack in
    assert (List.length vstack' = List.length state.vstack - 1);
    { state with vstack = vstack' }
  | Return ->
    state
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
    let v = Binop.eval bin v1 v2 in
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
    let c = Memory.load state.memory i op in
    let state' = { state with vstack = c :: vstack' } in
    assert (List.length state'.vstack = List.length state.vstack);
    state'
  | Store op ->
    (* Pop the value t.const c from the stack *)
    let (c, vstack') = Vstack.pop state.vstack in
    (* Pop the value i32.const i from the stack *)
    let (i, vstack'') = Vstack.pop vstack' in
    (* let b be the byte sequence of c *)
    assert (op.sz = None); (* We only support N = 32 for now *)
    let memory' = Memory.store state.memory i c op in
    assert (List.length vstack'' = List.length state.vstack - 2);
    { state with vstack = vstack''; memory = memory' }
  | Block _ -> failwith "shouldn't happen"
  | Loop _ -> failwith "shouldn't happen"
  | Call _ -> failwith "shouldn't happen"

let transfer (b : Basic_block.t) (state : Domain.state) (summaries: Summary.t IntMap.t) : Domain.state =
  match b.sort with
  | Normal ->
    List.fold_left b.instrs ~init:state ~f:(fun prestate i ->
        let poststate = instr_transfer i prestate in
        Printf.printf "pre: %s\ninstr: %s\npost: %s\n" (Domain.to_string prestate) (Instr.to_string i) (Domain.to_string poststate);
        poststate
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
