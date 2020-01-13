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
    assert (op.sz = None); (* We only support N = 32 for now *)
    let (i, vstack') = Domain.pop state.vstack in
    let state' = begin match Domain.formula_mapsto_4bytes state.memory i op.offset with
      | Some c ->
        (* If there is already a value in the heap formula, use it *)
        let vstack'' = c :: vstack' in
        { state with vstack = vstack'' }
      | None ->
        (* Otherwise, expand the heap formula *)
        (* TODO: this expansion should be on the pre formula, not the post! *)
        let c = Value.top op.typ (Value.Source.Heap i.value) in (* value of the correct type *)
        let vstack'' = c :: vstack' in
        let memory' = Domain.star state.memory
            (Domain.star (MapsTo (ByteInValue (i, op.offset), ByteInValue (c, 0)))
               (Domain.star (MapsTo (ByteInValue (i, op.offset + 1), ByteInValue (c, 1)))
                  (Domain.star (MapsTo (ByteInValue (i, op.offset + 2), ByteInValue (c, 2)))
                     (MapsTo (ByteInValue (i, op.offset + 3), ByteInValue (c, 3)))))) in
        { state with vstack = vstack''; memory = memory' }
    end in
    assert (List.length state'.vstack = List.length state.vstack);
    state'
  | Store op ->
    (* Pop the value t.const c from the stack *)
    let (c, vstack') = Domain.pop state.vstack in
    (* Pop the value i32.const i from the stack *)
    let (i, vstack'') = Domain.pop vstack' in
    (* let b be the byte sequence of c *)
    assert (op.sz = None); (* We only support N = 32 for now *)
    (* "Replace the bytes mem.data[ea:N/8] with b*" -> we remember that in the heap formula *)
    (* The formula is:
       heap * ea -> c0 * ea+1 -> c1 * ea+2 -> c2 * ea+3 -> c3
       where c is formed of the bytes c0, c1, c2, c3 *)
    let memory' = Domain.star state.memory
        (Domain.star (MapsTo (ByteInValue (i, op.offset), ByteInValue (c, 0)))
           (Domain.star (MapsTo (ByteInValue (i, op.offset + 1), ByteInValue (c, 1)))
              (Domain.star (MapsTo (ByteInValue (i, op.offset + 2), ByteInValue (c, 2)))
                 (MapsTo (ByteInValue (i, op.offset + 3), ByteInValue (c, 3)))))) in
    assert (List.length vstack'' = List.length state.vstack - 2);
    { state with vstack = vstack''; memory = memory' }
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
