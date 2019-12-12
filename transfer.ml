open Core_kernel
open Helpers

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

let transfer (b : Basic_block.t) (state : Domain.state) (summaries: Summary.t IntMap.t) : Domain.state =
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
