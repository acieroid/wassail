open Core
open Helpers

(** Remove vars in certain cases, but this is probably wrong so it is disabled *)
let remove_vars : bool ref = ref false

type annot_expected = Spec.t

type state = Relational_domain.t
[@@deriving compare, equal]

module Domain = Relational_domain

type summary = Relational_summary.t

let extract_from_bottom (annot : Spec.t) : Spec.SpecWithoutBottom.t = match annot with
  | Bottom -> failwith "can't extract annot from bottom"
  | NotBottom s -> s

let annot_vars (annot : annot_expected) : Var.Set.t =
  Spec.vars_of annot

let vars (cfg : annot_expected Cfg.t) : Var.Set.t =
  List.fold_left (Cfg.all_annots cfg)
    ~init:Var.Set.empty
    ~f:(fun acc annot ->
        Var.Set.union acc (annot_vars annot))

let entry_vars (cfg : annot_expected Cfg.t) : Var.Set.t =
  annot_vars (Cfg.state_before_block cfg cfg.entry_block (Spec_inference.init_state cfg))

let exit_vars (cfg : annot_expected Cfg.t) : Var.Set.t =
  annot_vars (Cfg.state_after_block cfg cfg.exit_block (Spec_inference.init_state cfg))

let reachable_vars (instr : annot_expected Instr.t) : Var.Set.t =
  Var.Set.union (annot_vars (Instr.annotation_before instr)) (annot_vars (Instr.annotation_after instr))

let init_state (cfg : annot_expected Cfg.t) = Domain.init cfg (vars cfg)

let bottom_state (cfg : annot_expected Cfg.t) = Domain.bottom (vars cfg)

let state_to_string = Domain.to_string

let join_state = Domain.join

let widen_state = Domain.widen

(** Merges the entry states before analyzing the given block *)
let merge_flows (_module_ : Wasm_module.t) (cfg : annot_expected Cfg.t) (block : annot_expected Basic_block.t) (states : (int * state) list) : state =
    match states with
    | [] -> (* no in state, use init *)
      init_state cfg
    | _ ->
      (* one or multiple states *)
      begin match block.content with
        | Control { instr = Merge; _ } ->
          (* block is a control-flow merge *)
          let spec = Cfg.state_after_block cfg block.idx (Spec_inference.init_state cfg) in
          let states' = List.map states ~f:(fun (idx, s) ->
              (* Similar to what is done in data_instr_transfer: restrict the vars to only the important ones *)
              let s = if !remove_vars then
                  Domain.change_vars s (Var.Set.union_list [annot_vars spec; entry_vars cfg; exit_vars cfg])
                else s in
              (* get the spec after that state *)
              let spec' = Cfg.state_after_block cfg idx (Spec_inference.init_state cfg)in
              (* equate all different variables in the post-state with the ones in the pre-state *)
              Domain.add_equality_constraints s (Spec_inference.extract_different_vars spec spec')) in
          (* And finally joins all the states *)
          List.reduce_exn states' ~f:join_state
        | _ ->
          (* Not a control-flow merge, there should be a single predecessor *)
          begin match states with
            | (_, s) :: [] -> s
            | _ -> failwith (Printf.sprintf "Invalid block with multiple input states: %d" block.idx)
          end
      end

(** Transfer function for data instructions.
      @param i the instruction for which to compute the transfer function
      @param state the state before the instruction (prestate).
      @param vstack_spec: the specification of what the vstack looks like after execution
      @return the resulting state (poststate).
*)
let data_instr_transfer (module_ : Wasm_module.t) (cfg : annot_expected Cfg.t) (i : annot_expected Instr.labelled_data) (state : state) : state =
  let ret (i : annot_expected Instr.labelled_data) : Var.t = match List.hd (extract_from_bottom i.annotation_after).vstack with
    | Some r -> r
    | None -> failwith "Relational data_instr_transfer: no return value" in
  let state = if !remove_vars then Domain.change_vars state (Var.Set.union_list [reachable_vars (Data i); entry_vars cfg; exit_vars cfg]) else state in
  match i.instr with
  | Nop -> state
  | MemorySize ->
    (* memory size is bounded by the initial memory size and the maximum memory size *)
    let mem = Wasm_module.get_meminst module_ 0 in
    begin match mem.max_size with
      | Some max ->
        (* add ret = [min,max] where min and max are the memory size bounds *)
        Domain.add_interval_constraint state (ret i) (mem.min_size, max)
      | None -> (* TODO: add the constraint ret >= min *) state
    end
  | MemoryGrow ->
    (* not modeled precisely: returns the size of the memory before it has grown *)
    state
  | Drop -> state
  | Select ->
    let ret = ret i in
    let vstack = (extract_from_bottom i.annotation_before).vstack in
    let (c, v2, v1) = pop3 vstack in
    begin
      match Domain.is_zero state c with
      | (true, false) -> (* definitely 0, add ret = v2 *)
        Domain.add_equality_constraint state ret v2
      | (false, true) -> (* definitely not 0, add ret = v2 *)
        Domain.add_equality_constraint state ret v1
      | (true, true) -> (* could be 0 or not 0, join both constraints *)
        Domain.join
          (Domain.add_equality_constraint state ret v1)
          (Domain.add_equality_constraint state ret v2)
      | (false, false) -> (* none, add bottom constraint *)
        Domain.add_interval_constraint state ret (-1, 1)
    end
  | LocalGet l ->
    (* add ret = ln where ln is the local accessed *)
    Domain.add_equality_constraint state (ret i) (get_nth (extract_from_bottom i.annotation_before).locals l)
  | LocalSet l ->
    let local = get_nth (extract_from_bottom i.annotation_after).locals l in
    (* add ln' = v where ln' is the variable for the local set and v is the top of the vstack *)
    let v = pop (extract_from_bottom i.annotation_before).vstack in
    Domain.add_equality_constraint state local v
  | LocalTee l ->
    let local = get_nth (extract_from_bottom i.annotation_after).locals l in
    (* same as local.set x followed by local.get x *)
    let v = pop (extract_from_bottom i.annotation_before).vstack in
    Domain.add_equality_constraints state [(ret i, v); (local, v)]
  | GlobalGet g ->
    (* add v = gn where gn is the local accessed *)
    Domain.add_equality_constraint state (ret i) (get_nth (extract_from_bottom i.annotation_before).globals g)
  | GlobalSet g ->
    let global = get_nth (extract_from_bottom i.annotation_before).globals g in
    let v = pop (extract_from_bottom i.annotation_before).vstack in
    Domain.add_equality_constraint state global v
  | Const n ->
    (* add ret = n *)
    Domain.add_constraint state (ret i) (Prim_value.to_string n)
  | Compare _ ->
    (* TODO: reflect "rel v1 v2" in the constraints, when possible *)
    (* add ret = [0;1] *)
    Domain.add_interval_constraint state (ret i) (0, 1)
  | Binary { op = Binop.Add; _ } ->
    (* TODO: this is not sound, as + in the constraints is not a binary + *)
    let v2, v1 = pop2 (extract_from_bottom i.annotation_before).vstack in
    Domain.add_constraint state (ret i) (Domain.add v1 v2)
  | Binary { op = Binop.Sub; _ } ->
    let v2, v1 = pop2 (extract_from_bottom i.annotation_before).vstack in
    Domain.add_constraint state (ret i) (Domain.sub v1 v2)
  | Binary _ ->
    (* TODO: reflect "bin v1 v2" the operation in the constraints, when possible *)
    (* don't add any constraint (for now)  *)
    state
  | Unary _ ->
    state
  | Test _ ->
    (* TODO: reflect "test v" in the constraints, when possible *)
    (* add ret = [0;1] *)
    Domain.add_interval_constraint state (ret i) (0, 1)
  | Convert _ ->
    (*    let _v, vstack' = Vstack.Spec.pop state.vstack in *)
    (* Don't add any constraint *)
    state
  | Load {offset; _ } ->
    if !Relational_options.ignore_memory then state else
      let ret = ret i in
      let vaddr = pop (extract_from_bottom i.annotation_before).vstack in
      (* We are loading from address vaddr (also mk_i.label_0).
         The resulting value is ret (also mv_i.label_0).
         a. If there are addresses that are equal to vaddr, we join all the values they map to and assign ret to it. In case they differ, ret will be top.
         b. If there are no such addresses, then ret is not constrained and remains Top.
         We can safely ignore the case where addresses "may be equal to" vaddr, because this is correctly handled by both previous cases:
           a. there is at least one address that is definitely equal to vaddr, then these addresses are definitely loaded
           b. the resulting value is top, hence it is soundly over-approximative *)
      (* We assume load/stores are symmetric, i.e., when a load/store operation is made on an address for a specific type and size, all other operations on the same address are made with the same type and size *)
      (* First, find all addresses that are equal to vaddr *)
      let mem = (extract_from_bottom i.annotation_before).memory in
      let addrs = List.filter (Var.OffsetMap.keys mem)
          ~f:(fun a -> match Domain.are_equal_with_offset state a (vaddr, offset) with
              | (true, false) -> true (* definitely equal *)
              | _ -> false) in
      if List.is_empty addrs then
        (* Case b: no such addresses, then ret is unconstrained *)
        state
      else
        (* Case a: at least one address: join their value into the state after adding the constraint *)
        (* TODO: first meet their value?
           Basically, if we have [mv = 0, mv' = top], and we know that their address (mk and mk') are both definitely equal to the loaded address, then we should have in the post state: [mv = 0, mv' = 0]
 *)
        let states = List.map addrs ~f:(fun a ->
            (* Get the value *)
            let v = Var.OffsetMap.find_exn mem a in
            (* ret = value *)
            Domain.add_equality_constraint state ret v) in
        (* Meet the domains! This is because they should all be equivalent.
           TODO: carefully check that *)
        List.reduce_exn states ~f:Domain.meet
  | Store { offset; _ } ->
    let res = if !Relational_options.ignore_memory then state else
      let vval, vaddr = pop2 (extract_from_bottom i.annotation_before).vstack in
      (* Find all memory keys that are definitely equal to the address *)
      let mem = (extract_from_bottom i.annotation_after).memory in
      let equal_addrs = List.filter (Var.OffsetMap.keys mem)
          ~f:(fun a -> match Domain.are_equal_with_offset state a (vaddr, offset) with
              | (true, false) -> true (* definitely equal *)
              | _ -> false) in
      if not (List.is_empty equal_addrs) then begin
        (* If there are addresses that are definitely equal, update the corresponding value *)
        let states = List.map equal_addrs
            ~f:(fun a ->
                let v' = Var.OffsetMap.find_exn mem a in
                Domain.add_equality_constraint state vval v') in
        List.reduce_exn states ~f:Domain.meet
      end else begin
        (* Otherwise, do similar for all addresses that may be equal *)
        let maybe_equal_addrs = List.filter (Var.OffsetMap.keys mem)
            ~f:(fun a -> match Domain.are_equal_with_offset state a (vaddr, offset) with
                | (true, true) -> true (* maybe equal *)
                | _ -> false) in
        let states = List.map maybe_equal_addrs
            ~f:(fun a ->
                let v' = Var.OffsetMap.find_exn mem a in
                (* We need to join with the unmodified state because the addres *may* be equal *)
                Domain.join state
                  (Domain.add_equality_constraint state (failwith "which value here? (was v)") v')) in
        List.reduce_exn states ~f:Domain.join
      end in
    res

let control_instr_transfer
    (module_ : Wasm_module.t) (* The wasm module (read-only) *)
    (summaries : summary Int32Map.t) (* The summaries of other functions *)
    (cfg : annot_expected Cfg.t) (* The CFG analyzed *)
    (i : annot_expected Instr.labelled_control) (* The instruction *)
    (state : Domain.t) (* The pre state *)
  : [`Simple of state | `Branch of state * state ] =
  (* This restricts the variables to only those used in the current instruction and those defined at the entry/exit of the CFG. This can be safely disabled. *)
  let state = if !remove_vars then Domain.change_vars state (Var.Set.union_list [reachable_vars (Control i); entry_vars cfg; exit_vars cfg]) else state in
  let apply_summary (f : Int32.t) (arity : int * int) (state : state) : state =
    let summary = Int32Map.find_exn summaries f in
    let args = List.take (extract_from_bottom i.annotation_before).vstack (fst arity) in
    let ret = if snd arity = 1 then List.hd (extract_from_bottom i.annotation_after).vstack else None in
    Relational_summary.apply summary state (List.map ~f:Var.to_string args) (Option.map ~f:Var.to_string ret)
  in
  match i.instr with
  | Call (arity, _, f) ->
    (* We encounter a function call, retrieve its summary and apply it *)
    (* We assume all summaries are defined *)
    `Simple (apply_summary f arity state)
  | CallIndirect (arity, _, typ) ->
    (* v is the index in the table that points to the called functiion *)
    let v = pop (extract_from_bottom i.annotation_before).vstack in
    (* Get table 0 *)
    let table = List.nth_exn module_.table_insts 0 in
    (* Get all indices that v could be equal to *)
    let funids = List.filter (Table_inst.indices table) ~f:(fun idx ->
        fst (Domain.is_equal state v idx)) in
    let funs = List.map funids ~f:(fun idx -> Table_inst.get table idx) in
    (* The type of the function that should be applied *)
    let ftype = Wasm_module.get_type module_ typ in
    assert (snd arity <= 1);
    (* Apply the summaries *)
    let resulting_state = List.fold_left funs ~init:None ~f:(fun acc f ->
        match f with
        | Some fa ->
          if Stdlib.(ftype = (Wasm_module.get_func_type module_ fa)) then begin
            Log.info
              (Printf.sprintf "call_indirect applies function %s (type: %s)" (Int32.to_string fa) (Type.funtype_to_string ftype));
            (* Types match, apply the summary *)
            Some (Domain.join_opt (apply_summary fa arity state) acc)
          end else
            (* Types don't match, can't apply this function so we ignore it *)
            acc
        | None -> acc) in
    begin match resulting_state with
      | Some st -> `Simple st
      | None ->
        (* The call can't be resolved, no constraints to add *)
        `Simple state
    end
  | Br _ ->
    `Simple state
  | BrIf _
  | If _ ->
    let cond = Var.to_string (pop (extract_from_bottom i.annotation_before).vstack) in
    (* restrict cond = 1 to the constraints of the true branch, cond = 0 to the constrainst of the false branch *)
    `Branch (state (* true branch, cond is non-zero. Can't refine it (would have to meet with ]-inf,-1] union [1, +inf[, which is ]-inf,+inf[ in the current domains *),
             Domain.meet_interval state cond (0, 0) (* false branch, cond is zero *))
  | Return ->
    (* return does not change anything *)
    `Simple state
  | Unreachable ->
    (* Unreachable, so bottom is returned *)
    `Simple (bottom_state cfg)
  | Merge -> (* Not handled here, but in merge_flows *) `Simple state
  | _ -> failwith (Printf.sprintf "Unsupported control instruction: %s" (Instr.control_to_short_string i.instr))

let memvars (annot : annot_expected) : Var.t list =
  Spec.memvars annot

let extract_summary (cfg : annot_expected Cfg.t) (analyzed_cfg : state Cfg.t) : summary =
  let out_state = Cfg.state_after_block analyzed_cfg cfg.exit_block (init_state cfg) in
  let spec_entry = Cfg.state_before_block cfg cfg.entry_block (Spec_inference.init_state cfg) in
  let spec_exit = Cfg.state_after_block cfg cfg.exit_block (Spec_inference.init_state cfg) in
  Relational_summary.make cfg out_state
    (if List.length cfg.return_types = 1 then (List.hd (extract_from_bottom spec_exit).vstack) else None)
    (memvars spec_entry)
    (memvars spec_exit)
    (extract_from_bottom spec_exit).globals

let dummy_annotate (cfg : 'a Cfg.t) : ('a * state) Cfg.t =
  let bot = Relational_domain.bottom Var.Set.empty in
  Cfg.map_annotations cfg ~f:(fun i -> ((Instr.annotation_before i, bot), (Instr.annotation_after i, bot)))
