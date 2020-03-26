open Core_kernel
open Helpers

(* Transfer function for an instruction.
   Takes as argument the instruction `i`, and the state before the instruction (prestate).
   Returns a formula and the resulting state (poststate). The formula indicates what has to be assumed in the prestate *)
let rec instr_transfer (i : Instr.t) (state : Domain.state) : (Memory.formula * Domain.state) =
  match i with
  | Nop ->
    [], state
  | Drop ->
    let (_, vstack') = Vstack.pop state.vstack in
    assert (List.length vstack' = List.length state.vstack - 1);
    [], { state with vstack = vstack' }
  | LocalGet x ->
    let vstack' = (Locals.get_local state.locals x) :: state.vstack in
    assert (List.length vstack' = 1 + List.length state.vstack);
    [], { state with vstack = vstack' }
  | LocalSet x ->
    let (v, vstack') = Vstack.pop state.vstack in
    assert (List.length vstack' = List.length state.vstack - 1);
    [], { state with vstack = vstack';
                 locals = Locals.set_local state.locals x v }
  | LocalTee x ->
    let (v, vstack') = Vstack.pop state.vstack in
    let (f, state') = instr_transfer (LocalSet x) { state with vstack = v :: v :: vstack' } in
    assert (List.length state'.vstack = List.length state.vstack);
    f, state'
  | GlobalGet x ->
    let vstack' = (Globals.get_global state.globals x) :: state.vstack in
    assert (List.length vstack' = List.length state.vstack + 1);
    [], { state with vstack = vstack' }
  | GlobalSet x ->
    let (v, vstack') = Vstack.pop state.vstack in
    assert (List.length vstack' = List.length state.vstack - 1);
    [], { state with vstack = vstack';
                     globals = Globals.set_global state.globals x v }
  | Br _ ->
    [], state
  | BrIf _ ->
    let (_, vstack') = Vstack.pop state.vstack in
    assert (List.length vstack' = List.length state.vstack - 1);
    [], { state with vstack = vstack' }
  | Return -> [], state
  | Const v ->
    let vstack' = v :: state.vstack in
    assert (List.length vstack' = List.length state.vstack + 1);
    [], { state with vstack = vstack' }
  | Compare rel ->
    let (v1, vstack') = Vstack.pop state.vstack in
    let (v2, vstack'') = Vstack.pop vstack' in
    let v = Relop.eval rel v1 v2 in
    let vstack''' = v :: vstack'' in
    assert (List.length vstack''' = List.length state.vstack - 1);
    [], { state with vstack =  vstack''' }
  | Binary bin ->
    let (v1, vstack') = Vstack.pop state.vstack in
    let (v2, vstack'') = Vstack.pop vstack' in
    let v = Binop.eval bin v1 v2 in
    let vstack''' = v :: vstack'' in
    assert (List.length vstack''' = List.length state.vstack - 1);
    [], { state with vstack = vstack''' }
  | Test test ->
    let (v, vstack') = Vstack.pop state.vstack in
    let v' = Testop.eval test v in
    let vstack'' = v' :: vstack' in
    assert (List.length vstack'' = List.length state.vstack);
    [], { state with vstack = vstack'' }
  | Load op ->
    assert (op.sz = None); (* We only support N = 32 for now *)
    let (i, vstack') = Vstack.pop state.vstack in
    let (f, state') = begin match Memory.formula_mapsto_4bytes state.memory i op.offset with
      | Some c ->
        (* If there is already a value in the heap formula, use it *)
        let vstack'' = c :: vstack' in
        [], { state with vstack = vstack'' }
      | None ->
        (* Otherwise, expand the heap formula *)
        (* TODO: this expansion should be on the pre formula, not the post! *)
        let c = Value.top op.typ (Value.Source.Heap i.value) in (* value of the correct type *)
        let vstack'' = c :: vstack' in
        let f = Memory.[(ByteInValue (i, op.offset), ByteInValue (c, 0));
                        (ByteInValue (i, op.offset + 1), ByteInValue (c, 1));
                        (ByteInValue (i, op.offset + 2), ByteInValue (c, 2));
                        (ByteInValue (i, op.offset + 3), ByteInValue (c, 3))] in
        let memory' = Memory.join state.memory f in
        f, { state with vstack = vstack''; memory = memory'}
    end in
    assert (List.length state'.vstack = List.length state.vstack);
    f, state'
  | Store op ->
    (* Pop the value t.const c from the stack *)
    let (c, vstack') = Vstack.pop state.vstack in
    (* Pop the value i32.const i from the stack *)
    let (i, vstack'') = Vstack.pop vstack' in
    (* let b be the byte sequence of c *)
    assert (op.sz = None); (* We only support N = 32 for now *)
    (* "Replace the bytes mem.data[ea:N/8] with b*" -> we remember that in the heap formula *)
    (* The formula is:
       heap * ea -> c0 * ea+1 -> c1 * ea+2 -> c2 * ea+3 -> c3
       where c is formed of the bytes c0, c1, c2, c3 *)
    (* TODO: expand pre-state with formula *)
    let memory' = Memory.join state.memory
        [(ByteInValue (i, op.offset), ByteInValue (c, 0));
         (ByteInValue (i, op.offset + 1), ByteInValue (c, 1));
         (ByteInValue (i, op.offset + 2), ByteInValue (c, 2));
         (ByteInValue (i, op.offset + 3), ByteInValue (c, 3))] in
    assert (List.length vstack'' = List.length state.vstack - 2);
    [], { state with vstack = vstack''; memory = memory' }
  | Block _ -> failwith "shouldn't happen"
  | Loop _ -> failwith "shouldn't happen"
  | Call _ -> failwith "shouldn't happen"

let transfer (b : Basic_block.t) (state : Domain.state) (summaries: Summary.t IntMap.t) : (Memory.formula * Domain.state) =
  match b.sort with
  | Normal ->
    List.fold_left b.instrs ~init:([], state) ~f:(fun (f, prestate) i ->
        let (f', poststate) = instr_transfer i prestate in
        Printf.printf "pre: %s\ninstr: %s\npost: %s\n" (Domain.to_string prestate) (Instr.to_string i) (Domain.to_string poststate);
        (f @ f', poststate)
      )
  | Function -> begin match b.instrs with
      | Call f :: [] ->
        (* We encounter a function call, retrieve its summary and apply it *)
        (* We assume all summaries are defined *)
        let summary = IntMap.find_exn summaries f in
        [], Summary.apply summary f state
      | _ -> failwith "Invalid function block"
    end
  | _ -> [], state
