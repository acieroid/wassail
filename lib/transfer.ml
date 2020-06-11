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
let data_instr_transfer (module_ : Wasm_module.t) (i : Instr.data) (state : Domain.state) : Domain.state =
  match i with
  | Nop _ -> state
  | MemorySize (_, ret) ->
    (* memory size is bounded by the initial memory size and the maximum memory size *)
    let mem = List.nth_exn module_.mems 0 in
    (* add ret = [min,max] where min and max are the memory size bounds *)
    { (Domain.add_constraint state ret
         (Printf.sprintf "[%d;%d]"
            mem.min_size
            (match mem.max_size with
             | Some max -> max
             | None -> failwith "unsupported infinite max size" (* can easily supported, the constraint just becomes ret >= min *))))
      with vstack = ret :: state.vstack }
  | Drop _ ->
    let _, vstack' = Vstack.pop state.vstack in
    { state with vstack = vstack' }
  | Select (_, ret)->
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
  | LocalGet (l, _, ret) ->
    (* add ret = ln where ln is the local accessed *)
    { (Domain.add_constraint state ret (Locals.get_local state.locals l))
      with vstack = ret :: state.vstack }
  | LocalSet (l, _, local) ->
    (* add ln' = v where ln' is the variable for the local set and v is the top of the vstack *)
    let v, vstack' = Vstack.pop state.vstack in
    { (Domain.add_constraint state local v)
      with vstack = vstack'; locals = Locals.set_local state.locals l local }
  | LocalTee (l, vstack_spec, local) ->
    (* same as local.set x followed by local.get x *)
    let ret, _ = Vstack.pop vstack_spec in
    let v, vstack' = Vstack.pop state.vstack in
    { (Domain.add_constraint
         (Domain.add_constraint state ret v)
         local v)
      with vstack = ret :: vstack'; locals = Locals.set_local state.locals l local }
  | GlobalGet (g, _, ret) ->
    (* add v = gn where gn is the local accessed *)
    { (Domain.add_constraint state ret (Globals.get_global state.globals g))
      with vstack = ret :: state.vstack }
  | GlobalSet (g, _, global) ->
     let v, vstack' = Vstack.pop state.vstack in
     { (Domain.add_constraint state global v)
       with vstack = vstack'; globals = Globals.set_global state.globals g global }
  | Const (n, _, ret) ->
    (* add ret = n *)
    { (Domain.add_constraint state ret (Prim_value.to_string n))
      with vstack = ret :: state.vstack }
  | Compare (_rel, _, ret) ->
    let _v2, vstack' = Vstack.pop state.vstack in
    let _v1, vstack'' = Vstack.pop vstack' in
    (* TODO: reflect "rel v1 v2" in the constraints, when possible *)
    (* add ret = [0;1] *)
    { (Domain.add_constraint state ret "[0;1]")
      with vstack = ret :: vstack'' }
  | Binary (_bin, _, ret) ->
    let _v2, vstack' = Vstack.pop state.vstack in
    let _v1, vstack'' = Vstack.pop vstack' in
    (* TODO: reflect "bin v1 v2" the operation in the constraints, when possible *)
    (* don't add any constraint (for now)  *)
    { state
      with vstack = ret :: vstack'' }
  | Test (_test, _, ret) ->
    let _v, vstack' = Vstack.pop state.vstack in
    (* TODO: reflect "test v" in the constraints, when possible *)
    (* add ret = [0;1] *)
    { (Domain.add_constraint state ret "[0;1]")
      with vstack = ret :: vstack' }
  | Load ({ typ = I32; offset; sz = None }, _, vars) ->
    let vaddr, vstack' = Vstack.pop state.vstack in
    begin match vars with
      | [ret; addr0; addr1; addr2; addr3] ->
        (* add the constraints on addresses, i.e., addri = vaddr+offset+i where vaddr is the variable from the stack, addri is the address from which we will load *)
        let state' =
          { (Domain.add_constraint
               (Domain.add_constraint
                  (Domain.add_constraint
                     (Domain.add_constraint state addr3 (Printf.sprintf "%s+%d" vaddr (offset+3)))
                     addr2 (Printf.sprintf "%s+%d" vaddr (offset+2)))
                  addr1 (Printf.sprintf "%s+%d" vaddr (offset+1)))
               addr0 (Printf.sprintf "%s+%d" vaddr offset))
            with vstack = ret :: vstack' } in
        (* Now loads all addris (which check for each addri whether it is precisely equal to one of the store address and returns the corresponding value).
           If this is the case for all addri, and if their respective values are compatible (i.e., 4 bytes of the same variable), then we can precisely add a constraint, otherwise we don't restrict the return value *)
        begin match Memory.load8 state'.memory addr3 (Domain.are_precisely_equal state'),
                    Memory.load8 state'.memory addr2 (Domain.are_precisely_equal state'),
                    Memory.load8 state'.memory addr1 (Domain.are_precisely_equal state'),
                    Memory.load8 state'.memory addr0 (Domain.are_precisely_equal state') with
        | Some (v, 3), Some (v', 2), Some (v'', 1), Some (v''', 0) when Stdlib.(v = v' && v' = v'' && v'' = v''') ->
          Domain.add_constraint state' ret v
        | _ ->
          state'
        end
      | _ -> failwith "load: invalid variables"
    end
  | Load ({ typ = I32; offset; sz = Some (Pack8, ZX) }, _, vars) ->
    Logging.warn "ImpreciseOperation" "load8 returns top";
    let vaddr, vstack' = Vstack.pop state.vstack in
    begin match vars with
      | [ret; addr0; _addr1; _addr2; _addr3] ->
        let state' = { (Domain.add_constraint state addr0 (Printf.sprintf "%s+%d" vaddr offset))
                       with vstack = ret :: vstack' } in
        begin match Memory.load8 state'.memory addr0 (Domain.are_precisely_equal state') with
          | Some (v, b) ->
            Logging.info (Printf.sprintf "load8: got byte %d of %s" b v);
            (* There's no way of encoding this constraint with apron *)
            state'
          | None -> state'
        end
      | _ -> failwith "load: invalid variables"
    end
  | Load (op, _, _) ->
    (* TODO: load with sz=8,zx (and others, but this is the most important now *)
    failwith (Printf.sprintf "load not supported with such op argument: %s" (Memoryop.to_string op))
  | Store ({ typ = I32; offset; sz = None }, _, vars) ->
    begin match vars with
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
  | Store (op, _, _) ->
    (* TODO: store with i64? *)
    failwith (Printf.sprintf "store not supported with such op argument: %s" (Memoryop.to_string op))

let control_instr_transfer
    (i : Instr.control) (* The instruction *)
    (state : Domain.state) (* The pre state *)
    (summaries : Summary.t IntMap.t) (* Summaries to apply function calls *)
    (module_ : Wasm_module.t) (* The wasm module (read-only) *)
    (cfg : Cfg.t) (* The CFG analyzed *)
  : result =
  match i with
  | Call (f, _, ret) ->
      (* We encounter a function call, retrieve its summary and apply it *)
      (* We assume all summaries are defined *)
    let summary = IntMap.find_exn summaries f in
    (* We get the return name from the new_vars *)
    Simple (Summary.apply summary f state ret module_)
  | CallIndirect (_typ, _, _ret) -> failwith "NYI: call_indirect" (*
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
    (* return drops everything from the stack, but adds constraints for the return value if necessary *)
    Simple
      (if List.length cfg.return_types = 1 then
         let v, _ = Vstack.pop state.vstack in
         Domain.add_constraint { state with vstack = [] } (Domain.return_name cfg.idx) v
       else
         { state with vstack = [] })
  | Unreachable ->
    (* Unreachable, so what we return does not really matter *)
    Simple { state with vstack = [] }
  | _ -> failwith (Printf.sprintf "Unsupported control instruction: %s" (Instr.control_to_string i))

let check_vstack (state : Domain.state) (spec : Vstack.t) : unit =
  if Stdlib.((List.length state.vstack) = (List.length spec)) then
    ()
  else
    failwith (Printf.sprintf "invalid vstack (expected [%s]) in state %s" (String.concat spec ~sep:",") (Domain.to_string state))

let transfer (b : Basic_block.t) (state : Domain.state) (summaries : Summary.t IntMap.t) (module_ : Wasm_module.t) (cfg : Cfg.t) : result =
  Printf.printf "analyzing block %d\n" b.idx;
  match b.content with
  | Data instrs ->
    Simple (List.fold_left instrs ~init:state ~f:(fun prestate instr ->
        Printf.printf "pre: %s\ninstr: %s\n" (Domain.to_string prestate) (Instr.data_to_string instr);
        let poststate = data_instr_transfer module_ instr prestate in
        check_vstack poststate (Instr.vstack_spec (Data instr));
        poststate))
  | Control instr ->
    Printf.printf "pre: %s\ninstr: %s\n" (Domain.to_string state) (Instr.control_to_short_string instr);
    let poststate = control_instr_transfer instr state summaries module_ cfg in
    let vstack_spec = Instr.vstack_spec (Control instr) in
    begin match poststate with
      | Uninitialized -> ()
      | Simple s -> check_vstack s vstack_spec
      | Branch (s1, s2) -> check_vstack s1 vstack_spec; check_vstack s2 vstack_spec
    end;
    poststate
  | Nothing -> Simple state
  | ControlMerge _ -> Simple state
