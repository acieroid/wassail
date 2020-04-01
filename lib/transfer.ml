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

(* Transfer function for an instruction.
   Takes as argument the instruction `i`, and the state before the instruction (prestate).
   Returns the resulting state (poststate).
*)
let rec instr_transfer (i : Instr.t) (state : Domain.state) : result =
  match i with
  | Nop ->
    Simple state
  | Drop ->
    let (_, vstack') = Vstack.pop state.vstack in
    assert (List.length vstack' = List.length state.vstack - 1);
    Simple { state with vstack = vstack' }
  | LocalGet x ->
    let vstack' = (Locals.get_local state.locals x) :: state.vstack in
    assert (List.length vstack' = 1 + List.length state.vstack);
    Simple { state with vstack = vstack' }
  | LocalSet x ->
    let (v, vstack') = Vstack.pop state.vstack in
    assert (List.length vstack' = List.length state.vstack - 1);
    Simple { state with vstack = vstack';
                        locals = Locals.set_local state.locals x v }
  | LocalTee x ->
    let (v, vstack') = Vstack.pop state.vstack in
    instr_transfer (LocalSet x) { state with vstack = v :: v :: vstack' }
  | GlobalGet x ->
    let vstack' = (Globals.get_global state.globals x) :: state.vstack in
    assert (List.length vstack' = List.length state.vstack + 1);
    Simple { state with vstack = vstack' }
  | GlobalSet x ->
    let (v, vstack') = Vstack.pop state.vstack in
    assert (List.length vstack' = List.length state.vstack - 1);
    Simple { state with vstack = vstack';
                        globals = Globals.set_global state.globals x v }
  | Br _ ->
    Simple state
  | BrIf _ ->
    let (cond, vstack') = Vstack.pop state.vstack in
    assert (List.length vstack' = List.length state.vstack - 1);
    Branch ({ state with vstack = vstack'; memory = Memory.refine state.memory cond true },
            { state with vstack = vstack'; memory = Memory.refine state.memory cond false })
  | Return ->
    Simple state
  | Const v ->
    let vstack' = v :: state.vstack in
    assert (List.length vstack' = List.length state.vstack + 1);
    Simple { state with vstack = vstack' }
  | Compare rel ->
    let (v2, vstack') = Vstack.pop state.vstack in
    let (v1, vstack'') = Vstack.pop vstack' in
    let v = Relop.eval rel v1 v2 in (* We don't resolve addresses in value here to keep conditionals in the shape we want them *)
    let vstack''' = v :: vstack'' in
    assert (List.length vstack''' = List.length state.vstack - 1);
    Simple { state with vstack =  vstack''' }
  | Binary bin ->
    let (v2, vstack') = Vstack.pop state.vstack in
    let (v1, vstack'') = Vstack.pop vstack' in
    let v = Binop.eval bin (Memory.resolve state.memory v1) (Memory.resolve state.memory v2) in (* We roselv addresses here *)
    let vstack''' = v :: vstack'' in
    assert (List.length vstack''' = List.length state.vstack - 1);
    Simple { state with vstack = vstack''' }
  | Test test ->
    let (v, vstack') = Vstack.pop state.vstack in
    let v' = Testop.eval test v in
    let vstack'' = v' :: vstack' in
    assert (List.length vstack'' = List.length state.vstack);
    Simple { state with vstack = vstack'' }
  | Load op ->
    let (i, vstack') = Vstack.pop state.vstack in
    let c = Memory.load state.memory i op in
    let state' = { state with vstack = c :: vstack' } in
    assert (List.length state'.vstack = List.length state.vstack);
    Simple state'
  | Store op ->
    (* Pop the value t.const c from the stack *)
    let (c, vstack') = Vstack.pop state.vstack in
    (* Pop the value i32.const i from the stack *)
    let (i, vstack'') = Vstack.pop vstack' in
    (* let b be the byte sequence of c *)
    assert (op.sz = None); (* We only support N = 32 for now *)
    let memory' = Memory.store state.memory i c op in
    assert (List.length vstack'' = List.length state.vstack - 2);
    Simple { state with vstack = vstack''; memory = memory' }
  | Block _ -> failwith "shouldn't happen"
  | Loop _ -> failwith "shouldn't happen"
  | Call _ -> failwith "shouldn't happen"

let transfer (b : Basic_block.t) (state : Domain.state) (summaries: Summary.t IntMap.t) : result =
  match b.sort with
  | Normal ->
    List.fold_left b.instrs ~init:(Simple state) ~f:(fun prestate i ->
        match prestate with
        | Simple st -> let poststate = instr_transfer i st in
          Printf.printf "pre: %s\ninstr: %s\n" (Domain.to_string st) (Instr.to_string i);
          poststate
        | _ -> failwith "Unexpected")
  | Function -> begin match b.instrs with
      | Call f :: [] ->
        (* We encounter a function call, retrieve its summary and apply it *)
        (* We assume all summaries are defined *)
        let summary = IntMap.find_exn summaries f in
        Simple (Summary.apply summary f state)
      | _ -> failwith "Invalid function block" (* TODO: or rather unsupported? *)
    end
  | LoopEntry ->
    Printf.printf "Loop entry\n---------------------------\n";
    Simple state
  | LoopExit ->
    Printf.printf "Loop exit\n---------------------------\n";
    Simple state
  | _ ->
    (* All blocks represent control flow and are handled by the fixpoint, there is no need to special case them here *)
    Simple state
