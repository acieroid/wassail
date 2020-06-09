open Core_kernel
open Helpers

(** The result of applying the transfer function. *)
type result =
  | Uninitialized (** Meaning it has not been computed yet *)
  | Simple of Domain.state (** A single successor *)
  | Branch of Domain.state * Domain.state (** Upon a brif, there are two successor states: one where the condition holds, and where where it does not hold. This is used to model that. *)
[@@deriving compare]

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

(** Transfer function for data instructions.
   @param i the instruction for which to compute the transfer function
   @param state the state before the instruction (prestate).
   @param vstack_spec: the specification of what the vstack looks like after execution
   @return the resulting state (poststate).
*)
let data_instr_transfer (module_ : Wasm_module.t) (i : Instr.data) (state : Domain.state) (vstack_spec : Vstack.t) (new_vars : string list) : Domain.state =
  match i with
  | Nop -> state
  | MemorySize ->
    (* memory size is bounded by the initial memory size and the maximum memory size *)
    let ret, _ = Vstack.pop vstack_spec in
    let mem = List.nth_exn module_.mems 0 in
    (* add ret = [min,max] where min and max are the memory size bounds *)
    { (Domain.add_constraint state ret
         (Printf.sprintf "[%d;%d]"
            mem.min_size
            (match mem.max_size with
             | Some max -> max
             | None -> failwith "unsupported infinite max size" (* can easily supported, the constraint just becomes ret >= min *))))
      with vstack = ret :: state.vstack }
  | Drop ->
    let (_, vstack') = Vstack.pop state.vstack in
    assert (List.length vstack' = List.length state.vstack - 1);
    { state with vstack = vstack' }
  | Select ->
    let ret, _ = Vstack.pop vstack_spec in
    let c, vstack' = Vstack.pop state.vstack in
    let v2, vstack'' = Vstack.pop vstack' in
    let v1, vstack''' = Vstack.pop vstack'' in
    { (match Domain.is_zero state c with
          | (true, false) -> (* definitely 0, add ret = v2 *)
            Domain.add_constraint state ret v2
          | (false, true) -> (* definitely not 0, add ret = v2 *)
            Domain.add_constraint state ret v1
          | (true, true) -> (* could be 0 or not 0, don't add any constraint *)
            state (* TODO: with the right domain, we could encode ret = v1 join v2? *)
          | (false, false) -> (* none, add bottom constraint *)
            Domain.add_constraint state ret "[-1;1]"
        ) with vstack = ret :: vstack''' }
  | LocalGet l ->
    let ret, _ = Vstack.pop vstack_spec in
    (* add ret = ln where ln is the local accessed *)
    { (Domain.add_constraint state ret (Locals.get_local state.locals l))
      with vstack = ret :: state.vstack }
  | LocalSet l ->
    (* add ln' = v where ln' is the variable for the local set and v is the top of the vstack *)
    let (v, vstack') = Vstack.pop state.vstack in
    begin match new_vars with
      | local :: [] ->
        { (Domain.add_constraint state local v)
          with vstack = vstack'; locals = Locals.set_local state.locals l local }
      | _ -> failwith "local.set: invalid new vars"
    end
  | LocalTee l ->
    (* same as local.set x followed by local.get x *)
    let ret, _ = Vstack.pop vstack_spec in
    let v, vstack' = Vstack.pop state.vstack in
    begin match new_vars with
      | local :: [] ->
        { (Domain.add_constraint
             (Domain.add_constraint state ret v)
             local v)
          with vstack = vstack'; locals = Locals.set_local state.locals l local }
      | _ -> failwith "local.tee: invalid new vasr"
    end
  | GlobalGet g ->
    let ret, _ = Vstack.pop vstack_spec in
    (* add v = gn where gn is the local accessed *)
    { (Domain.add_constraint state ret (Globals.get_global state.globals g))
      with vstack = ret :: state.vstack }
  | GlobalSet g ->
    let (v, vstack') = Vstack.pop state.vstack in
    begin match new_vars with
      | global :: [] ->
        { (Domain.add_constraint state global v)
          with vstack = vstack'; globals = Globals.set_global state.globals g global }
      | _ -> failwith "global.set: invalid new vars"
    end
  | Const n ->
    let ret, _ = Vstack.pop vstack_spec in
    (* add ret = n *)
    { (Domain.add_constraint state ret (Prim_value.to_string n))
      with vstack = ret :: state.vstack }
  | Compare _rel ->
    let ret, _ = Vstack.pop vstack_spec in
    let (_v2, vstack') = Vstack.pop state.vstack in
    let (_v1, vstack'') = Vstack.pop vstack' in
    (* TODO: reflect "rel v1 v2" in the constraints, when possible *)
    (* add ret = [0;1] *)
    { (Domain.add_constraint state ret "[0;1]")
      with vstack = ret :: vstack'' }
  | Binary _bin ->
    let ret, _ = Vstack.pop vstack_spec in
    let (_v2, vstack') = Vstack.pop state.vstack in
    let (_v1, vstack'') = Vstack.pop vstack' in
    (* TODO: reflect "bin v1 v2" the operation in the constraints, when possible *)
    (* don't add any constraint (for now)  *)
    { state
      with vstack = ret :: vstack'' }
  | Test _test ->
    let ret, _ = Vstack.pop vstack_spec in
    let (_v, vstack') = Vstack.pop state.vstack in
    (* TODO: reflect "test v" in the constraints, when possible *)
    (* add ret = [0;1] *)
    { (Domain.add_constraint state ret "[0;1]")
      with vstack = ret :: vstack' }
  | Load { typ = I32; align = _; offset = _; sz = None } ->
    failwith "NYI: load" (*
    let (i, vstack') = Vstack.pop state.vstack in
    let c = Memory.load state.memory i.value op in
    let state' = { state with vstack = c :: vstack' } in
    assert (List.length state'.vstack = List.length state.vstack);
    state' *)
  | Load _ -> failwith "load not supported with such op argument"
  | Store { typ = I32; align = _; offset; sz = None } ->
    begin match new_vars with
      | [addr0; addr1; addr2; addr3] ->
        let vval, vstack' = Vstack.pop state.vstack in
        let vaddr, vstack'' = Vstack.pop vstack' in
        { (Domain.add_constraint
            (Domain.add_constraint
               (Domain.add_constraint
                  (Domain.add_constraint state addr3 (Printf.sprintf "%s+%d" vaddr (offset+3)))
                  addr2 (Printf.sprintf "%s+%d" vaddr (offset+2)))
               addr1 (Printf.sprintf "%s+%d" vaddr (offset+1)))
            addr0 (Printf.sprintf "%s+%d" vaddr offset))
          with vstack = vstack''; memory = Memory.store state.memory [(addr3, (vval, 3));
                                                                      (addr2, (vval, 2));
                                                                      (addr1, (vval, 1));
                                                                      (addr0, (vval, 0))]
        }
      | _ -> failwith "TODO"
    end
  | Store _ -> failwith "store not supported with such op argument"

let control_instr_transfer
    (i : Instr.control) (* The instruction *)
    (state : Domain.state) (* The pre state *)
    (summaries : Summary.t IntMap.t) (* Summaries to apply function calls *)
    (module_ : Wasm_module.t) (* The wasm module (read-only) *)
    (cfg : Cfg.t) (* The CFG analyzed *)
    (new_vars : string list)
  : result =
  match i with
  | Call f ->
      (* We encounter a function call, retrieve its summary and apply it *)
      (* We assume all summaries are defined *)
    let summary = IntMap.find_exn summaries f in
    (* We get the return name from the new_vars *)
    let ret = match new_vars with
      | v :: [] -> Some v (* we expect the summary to return a variable *)
      | _ -> None in
      Simple (Summary.apply summary f state ret module_)
  | CallIndirect _typ -> failwith "NYI: call_indirect" (*
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
    end *)
  | Br _ ->
    Simple state
  | BrIf _ ->
    let (cond, vstack') = Vstack.pop state.vstack in
    let state' = { state with vstack = vstack' } in
    (* Add cond = 1 to the constraints of the true branch, cond = 0 to the constrainst of the false branch *)
      Branch (Domain.add_constraint state' cond "1" (* true branch *),
              Domain.add_constraint state' cond "0" (* false branch *))
  | If _ ->
    (* If is similar to br_if, and the control flow is handled by the CFG visitor.
       We only need to propagate the right state in the right branch, just like br_if *)
    let (cond, vstack') = Vstack.pop state.vstack in
    let state' = { state with vstack = vstack' } in
    Branch (Domain.add_constraint state' cond "1",
            Domain.add_constraint state' cond "0")
  | Return ->
    (* return only keeps the necessary number of values from the stack *)
    let arity = List.length cfg.return_types in
    Simple { state with vstack = List.take state.vstack arity }
  | Unreachable ->
    (* Unreachable, so what we return does not really matter *)
    Simple { state with vstack = [] }
  | _ -> failwith (Printf.sprintf "Unsupported control instruction: %s" (Instr.control_to_string i))

let transfer (b : Basic_block.t) (state : Domain.state) (summaries : Summary.t IntMap.t) (module_ : Wasm_module.t) (cfg : Cfg.t) : result =
  Printf.printf "analyzing block %d\n" b.idx;
  match b.content with
  | Data instrs ->
    Simple (List.fold_left instrs ~init:state ~f:(fun prestate (instr, vstack, new_vars) ->
        Printf.printf "pre: %s\ninstr: %s\n" (Domain.to_string prestate) (Instr.data_to_string instr);
        let poststate = data_instr_transfer module_ instr prestate vstack new_vars in
        poststate))
  | Control (instr, _vstack, new_vars) ->
    (* Printf.printf "pre: %s\ncontrol: %s\n" (Domain.to_string state) (Instr.control_to_string instr); *)
    control_instr_transfer instr state summaries module_ cfg new_vars
  | Nothing -> Simple state
