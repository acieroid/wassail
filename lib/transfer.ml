open Core_kernel
open Helpers

type state = Domain.state
[@@deriving compare]

type result =
  | Uninitialized
  | Simple of state
  | Branch of state * state
[@@deriving compare]

let init_state (cfg : Cfg.t) = Domain.init cfg (failwith "TODO") (failwith "TODO")

let state_to_string = Domain.to_string

let join_state = Domain.join

(** Merges the entry states before analyzing the given block *)
let merge_flows (_module_ : Wasm_module.t) (_cfg : Cfg.t) (block : Basic_block.t) (states : state list) : state =
    match states with
    | [] -> (* no in state, use init *)
      failwith "init"
    | s :: [] -> (* single state *)
      s
    | _ ->
      (* multiple states, block should be a control-flow merge *)
      begin match block.content with
        | ControlMerge ->
          let ret = failwith "TODO" in
          let locals = failwith "TODO" in
          let globals = failwith "TODO" in
          (* for each state in states *)
          let states' = List.map states ~f:(fun s ->
              (* replace the top of the stack if necessary *)
              let vstack = match ret with
                | Some v -> v :: (List.drop s.vstack 1)
                | None -> s.vstack in
              (* add constraints for locals and globals *)
              let constraints = List.mapi locals ~f:(fun i l -> (l, List.nth_exn s.locals i)) @
                                List.mapi globals ~f:(fun i g -> (g, List.nth_exn s.globals i)) @
                                List.map (Option.to_list ret) ~f:(fun r -> (r, List.hd_exn s.vstack)) in
              Domain.add_constraints
                  { s with locals = locals; globals = globals; vstack = vstack }
                  constraints) in
          (* now join all the states: their vstack, locals and globals should be
             the same, only their memory might differ, but joining memory is
             handled in memory.ml by computing the most general memory *)
          List.reduce_exn states' ~f:Domain.join
        | _ -> failwith (Printf.sprintf "Invalid block with multiple input states: %d" block.idx)
      end

(** Transfer function for data instructions.
   @param i the instruction for which to compute the transfer function
   @param state the state before the instruction (prestate).
   @param vstack_spec: the specification of what the vstack looks like after execution
   @return the resulting state (poststate).
*)
let data_instr_transfer (module_ : Wasm_module.t) (i : Instr.data Instr.labelled) (state : Domain.state) : Domain.state =
  match i.instr with
  | Nop -> state
  | MemorySize ->
    let ret = failwith "TODO" in
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
  | Drop ->
    let _, vstack' = Vstack.pop state.vstack in
    { state with vstack = vstack' }
  | Select ->
    let ret = failwith "TODO" in
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
    let ret = failwith "TODO" in
    (* add ret = ln where ln is the local accessed *)
    { (Domain.add_constraint state ret (Locals.get_local state.locals l))
      with vstack = ret :: state.vstack }
  | LocalSet l ->
    let local = failwith "TODO" in
    (* add ln' = v where ln' is the variable for the local set and v is the top of the vstack *)
    let v, vstack' = Vstack.pop state.vstack in
    { (Domain.add_constraint state local v)
      with vstack = vstack'; locals = Locals.set_local state.locals l local }
  | LocalTee l ->
    let vstack_spec = failwith "TODO" in
    let local = failwith "TODO" in
    (* same as local.set x followed by local.get x *)
    let ret, _ = Vstack.pop vstack_spec in
    let v, vstack' = Vstack.pop state.vstack in
    { (Domain.add_constraint
         (Domain.add_constraint state ret v)
         local v)
      with vstack = ret :: vstack'; locals = Locals.set_local state.locals l local }
  | GlobalGet g ->
    let ret = failwith "TODO" in
    (* add v = gn where gn is the local accessed *)
    { (Domain.add_constraint state ret (Globals.get_global state.globals g))
      with vstack = ret :: state.vstack }
  | GlobalSet g ->
    let global = failwith "TODO" in
     let v, vstack' = Vstack.pop state.vstack in
     { (Domain.add_constraint state global v)
       with vstack = vstack'; globals = Globals.set_global state.globals g global }
  | Const n ->
    let ret = failwith "TODO" in
    (* add ret = n *)
    { (Domain.add_constraint state ret (Prim_value.to_string n))
      with vstack = ret :: state.vstack }
  | Compare _ ->
    let ret = failwith "TODO" in
    let _v2, vstack' = Vstack.pop state.vstack in
    let _v1, vstack'' = Vstack.pop vstack' in
    (* TODO: reflect "rel v1 v2" in the constraints, when possible *)
    (* add ret = [0;1] *)
    { (Domain.add_constraint state ret "[0;1]")
      with vstack = ret :: vstack'' }
  | Binary _ ->
    let ret = failwith "TODO" in
    let _v2, vstack' = Vstack.pop state.vstack in
    let _v1, vstack'' = Vstack.pop vstack' in
    (* TODO: reflect "bin v1 v2" the operation in the constraints, when possible *)
    (* don't add any constraint (for now)  *)
    { state
      with vstack = ret :: vstack'' }
  | Test _ ->
    let ret = failwith "TODO" in
    let _v, vstack' = Vstack.pop state.vstack in
    (* TODO: reflect "test v" in the constraints, when possible *)
    (* add ret = [0;1] *)
    { (Domain.add_constraint state ret "[0;1]")
      with vstack = ret :: vstack' }
  | Convert _ ->
    let ret = failwith "TODO" in
    let _v, vstack' = Vstack.pop state.vstack in
    (* Don't add any constraint *)
    { state with vstack = ret :: vstack' }
  | Load ({ typ = I32; offset; sz = None }) ->
    let vars = failwith "TODO" in
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
          (* TODO: this may not be correct: v could be an i64 value of which we only take 4 bytes *)
          Domain.add_constraint state' ret v
        | _ ->
          state'
        end
      | _ -> failwith "load: invalid variables"
    end
  | Load ({ typ = I32; offset; sz = Some (Pack8, _) }) ->
    let vars = failwith "TODO" in
    Logging.warn "ImpreciseOperation" "load8 returns top, and ignores sx/zx";
    let vaddr, vstack' = Vstack.pop state.vstack in
    begin match vars with
      | [ret; addr0; _addr1; _addr2; _addr3] ->
        let state' = { (Domain.add_constraint state addr0 (Printf.sprintf "%s+%d" vaddr offset))
                       with vstack = ret :: vstack' } in
        begin match Memory.load8 state'.memory addr0 (Domain.are_precisely_equal state') with
          | Some (v, b) ->
            Logging.warn "ImpreciseOperation" (Printf.sprintf "load8: got byte %d of %s" b v);
            (* There's no way of encoding this constraint with apron *)
            (* TODO: add ret = v if b = -1 *)
            state'
          | None -> state'
        end
      | _ -> failwith "load: invalid variables"
    end
  | Load op ->
    (* TODO: load with sz=8,zx (and others, but this is the most important now *)
    failwith (Printf.sprintf "load not supported with such op argument: %s" (Memoryop.to_string op))
  | Store { typ = I32; offset; sz = None } ->
    let vars = failwith "TODO" in
    begin match vars with
      | [addr0; addr1; addr2; addr3] ->
        let vval, vstack' = Vstack.pop state.vstack in
        let vaddr, vstack'' = Vstack.pop vstack' in
        { (Domain.add_constraints state
             [(addr3, (Printf.sprintf "%s+%d" vaddr (offset+3)));
              (addr2, (Printf.sprintf "%s+%d" vaddr (offset+2)));
              (addr1, (Printf.sprintf "%s+%d" vaddr (offset+1)));
              (addr0, (Printf.sprintf "%s+%d" vaddr offset))])
          with vstack = vstack''; memory = Memory.store state.memory [(addr3, (vval, 3));
                                                                      (addr2, (vval, 2));
                                                                      (addr1, (vval, 1));
                                                                      (addr0, (vval, 0))]
        }
      | _ -> failwith "store: invalid vars"
    end
  | Store { typ = I64; offset; sz = None } ->
    let vars = failwith "TODO" in
    begin match vars with
      | [addr0; addr1; addr2; addr3; addr4; addr5; addr6; addr7] ->
        let vval, vstack' = Vstack.pop state.vstack in
        let vaddr, vstack'' = Vstack.pop vstack' in
        { (Domain.add_constraints state
             [(addr7, (Printf.sprintf "%s+%d" vaddr (offset+7)));
              (addr6, (Printf.sprintf "%s+%d" vaddr (offset+6)));
              (addr5, (Printf.sprintf "%s+%d" vaddr (offset+5)));
              (addr4, (Printf.sprintf "%s+%d" vaddr (offset+4)));
              (addr3, (Printf.sprintf "%s+%d" vaddr (offset+3)));
              (addr2, (Printf.sprintf "%s+%d" vaddr (offset+2)));
              (addr1, (Printf.sprintf "%s+%d" vaddr (offset+1)));
              (addr0, (Printf.sprintf "%s+%d" vaddr offset))])
          with vstack = vstack''; memory = Memory.store state.memory [(addr6, (vval, 7));
                                                                      (addr5, (vval, 6));
                                                                      (addr4, (vval, 4));
                                                                      (addr3, (vval, 3));
                                                                      (addr2, (vval, 2));
                                                                      (addr1, (vval, 1));
                                                                      (addr0, (vval, 0))]
        }
      | _ -> failwith "store: invalid vars"
    end
  | Store { typ = I32; offset; sz = Some (Pack8, SX)} ->
    let vars = failwith "TODO" in
    Logging.warn "ImpreciseOperation" "store8 ignores sx/zx";
    begin match vars with
      | [addr0; _addr1; _addr2; _addr3] ->
        let vval, vstack' = Vstack.pop state.vstack in
        let vaddr, vstack'' = Vstack.pop vstack' in
        { (Domain.add_constraint state addr0 (Printf.sprintf "%s+%d" vaddr offset))
          with vstack = vstack'';
               (* -1 means we store the entire value? *)
               memory = Memory.store state.memory [(addr0, (vval, -1))] }
      | _ -> failwith "store: invalid vars"
    end
  | Store op ->
    (* TODO: store with i64? *)
    failwith (Printf.sprintf "store not supported with such op argument: %s" (Memoryop.to_string op))

let control_instr_transfer
    (i : Instr.control Instr.labelled) (* The instruction *)
    (state : Domain.state) (* The pre state *)
    (summaries : Summary.t IntMap.t) (* Summaries to apply function calls *)
    (module_ : Wasm_module.t) (* The wasm module (read-only) *)
    (cfg : Cfg.t) (* The CFG analyzed *)
  : result =
  match i.instr with
  | Call (_arity, f) ->
    let ret  = failwith "TODO" in
      (* We encounter a function call, retrieve its summary and apply it *)
      (* We assume all summaries are defined *)
    let summary = IntMap.find_exn summaries f in
    (* We get the return name from the new_vars *)
    Simple (Summary.apply summary f state ret module_)
  | CallIndirect (_arity, typ) ->
    let ret = failwith "TODO" in
    (* v is the index in the table that points to the called functiion *)
    let (v, vstack') = Vstack.pop state.vstack in
    let state' = { state with vstack = vstack' } in
    (* Get table 0 *)
    let table = List.nth_exn module_.tables 0 in
    (* Get all indices that v could be equal to *)
    let funids = List.filter (Table_inst.indices table) ~f:(fun idx ->
        fst (Domain.is_equal state' v idx)) in
    let funs = List.map funids ~f:(fun idx -> Table_inst.get table idx) in
    (* The type of the function that should be applied *)
    let ftype = Wasm_module.get_type module_ typ in
    let arity_in = List.length (fst ftype) in
    let arity_out = List.length (snd ftype) in
    assert (arity_out <= 1);
    (* Apply the summaries *)
    let resulting_state = List.fold_left funs ~init:None ~f:(fun acc f ->
        match f with
        | Some fa ->
          if Stdlib.(ftype = (Wasm_module.get_func_type module_ fa)) then begin
            Logging.info (Printf.sprintf "call_indirect applies function %d (type: %s)" fa (Type.funtype_to_string ftype));

            (* Types match, apply the summary *)
            let summary = IntMap.find_exn summaries fa in
            Some (Domain.join_opt (Summary.apply summary fa state' ret module_) acc)
          end else
            (* Types don't match, can't apply this function so we ignore it *)
            acc
        | None -> acc) in
    begin match resulting_state with
      | Some st -> Simple st
      | None ->
        (* If the call can't be resolved, pop the right number of arguments, and push ret if necessary *)
        Simple { state with vstack = Option.to_list ret @ List.drop vstack' arity_in }
    end
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
         let ret = Domain.return_name cfg.idx in
         let v, _ = Vstack.pop state.vstack in
         Domain.add_constraint { state with vstack = [ret] } ret v
       else
         { state with vstack = [] })
  | Unreachable ->
    (* Unreachable, so what we return does not really matter *)
    Simple { state with vstack = [] }
  | _ -> failwith (Printf.sprintf "Unsupported control instruction: %s" (Instr.control_to_string i.instr))

(* let check_vstack (state : Domain.state) (spec : Vstack.t) : unit =
  if Stdlib.((List.length state.vstack) = (List.length spec)) then
    ()
  else
    failwith (Printf.sprintf "invalid vstack (expected [%s]) in state %s" (String.concat spec ~sep:",") (Domain.to_string state)) *)

let transfer (module_ : Wasm_module.t) (cfg : Cfg.t) (b : Basic_block.t) (state : state) : result =
  (* Printf.printf "analyzing block %d\n" b.idx; *)
  match b.content with
  | Data instrs ->
    Simple (List.fold_left instrs ~init:state ~f:(fun prestate instr ->
        (* Printf.printf "pre: %s\ninstr: %s\n" (Domain.to_string prestate) (Instr.data_to_string instr); *)
        let poststate = data_instr_transfer module_ instr prestate in
        (* check_vstack poststate (Instr.vstack_spec (Data instr)); TODO *)
        poststate))
  | Control instr ->
    (* Printf.printf "pre: %s\ninstr: %s\n" (Domain.to_string state) (Instr.control_to_short_string instr); *)
    let poststate = control_instr_transfer instr state (failwith "summaries") module_ cfg in
    (* TODO let vstack_spec = Instr.vstack_spec (Control instr) in
    begin match poststate with
      | Uninitialized -> ()
      | Simple s -> check_vstack s vstack_spec
      | Branch (s1, s2) -> check_vstack s1 vstack_spec; check_vstack s2 vstack_spec
       end; *)
    poststate
  | Nothing -> Simple state
  | ControlMerge -> Simple state
