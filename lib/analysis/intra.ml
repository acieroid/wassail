open Core
open Helpers

(** A simple intra-procedural analysis *)
module type INTRA_ONLY = sig

  module Transfer : Transfer.TRANSFER_BASE

  type extra = unit

  (** Analyze a function (represented by its CFG) from the module *)
  val analyze
    : Wasm_module.t
    -> Transfer.annot_expected Cfg.t
    -> extra
    -> Transfer.State.t Cfg.t

end

module type INTRA_FOR_SUMMARY = sig
  module Transfer : Transfer.SUMMARY_TRANSFER
  type extra = Transfer.summary Int32Map.t

  val analyze
    : Wasm_module.t
    -> Transfer.annot_expected Cfg.t
    -> extra
    -> Transfer.State.t Cfg.t
end

module type CALL_ADAPTER = sig
  module Transfer : Transfer.TRANSFER_BASE

  type extra

  val analyze_call
    : Wasm_module.t
    -> Transfer.annot_expected Cfg.t
    -> Transfer.annot_expected Instr.labelled_call
    -> Transfer.State.t
    -> extra
    -> Transfer.State.t
end

module Result (Transfer : Transfer.TRANSFER_BASE) = struct

  (** The result of applying the transfer function. *)
  type t =
    | Uninitialized (** Meaning it has not been computed yet *)
    | Simple of Transfer.State.t (** A single successor *)
    | Branch of Transfer.State.t * Transfer.State.t (** Upon a `br_if`, there are two successor states: one where the condition holds, and where where it does not hold. This is used to model that. *)
  [@@deriving compare]

  (** The results of an intra analysis are a mapping from instruction labels to their in and out values *)
  type intra_results = (Transfer.State.t * t) Instr.Label.Map.t

  (** Converts a result to a state. May require joining output states in case of branching *)
  let to_state (r : t) : Transfer.State.t = match r with
    | Uninitialized -> Transfer.bottom
    | Simple s -> s
    | Branch (s1, s2) -> Transfer.State.join s1 s2

  let to_string (r : t) : string = match r with
    | Uninitialized -> "uninit"
    | Simple s -> Printf.sprintf "simple: %s" (Transfer.State.to_string s)
    | Branch (s1, s2) -> Printf.sprintf "branch: %s\nand: %s" (Transfer.State.to_string s1) (Transfer.State.to_string s2)

  let join  (r1 : t) (r2 : t) : t =
    match (r1, r2) with
    | Simple s1, _ when Transfer.State.equal s1 Transfer.bottom -> r2
    | _, Simple s2 when Transfer.State.equal s2 Transfer.bottom -> r1
    | Uninitialized, _ -> r2
    | _, Uninitialized -> r1
    | Simple st1, Simple st2 ->
      let joined = Transfer.State.join st1 st2 in
      Simple joined
    | Branch (st1, st2), Branch (st1', st2') ->
      Branch (Transfer.State.join st1 st1',
              Transfer.State.join st2 st2')
    | _ -> failwith (Printf.sprintf "Cannot join results: %s and %s" (to_string r1) (to_string r2))

  let widen (r1 : t) (r2 : t) : t =
      match (r1, r2) with
      | Uninitialized, _ -> r2
      | _, Uninitialized -> r1
      | Simple st1, Simple st2 ->
        Simple (Transfer.State.widen st1 st2)
      | Branch (st1, st2), Branch (st1', st2') ->
        Branch (Transfer.State.widen st1 st1',
                Transfer.State.widen st2 st2')
      | _ -> failwith "Cannot widen results"
end


module Make
    (Transfer : Transfer.TRANSFER_BASE)
    (CallAdapter : CALL_ADAPTER with module Transfer = Transfer) = struct
  (* Include transfer to get a definition for state, equal_state, and annot_expected *)
  module Transfer = Transfer
  module Result = Result(Transfer)
  type extra = CallAdapter.extra

  (** Analyzes a CFG. Returns the final state after computing the transfer of the entire function. That final state is a pair where the first element are the results per block, and the second element are the results per instructions.
      @param module_ is the overall WebAssembly module, needed to access type information, tables, etc.
      @param cfg is the CFG to analyze *)
  let analyze_ (module_ : Wasm_module.t) (cfg : Transfer.annot_expected Cfg.t) (extra : CallAdapter.extra) : Result.intra_results =
    let bottom = Result.Uninitialized in
    (* Data of the analysis, per block *)
    let block_out : Result.t Cfg.BlockIdx.Map.t ref = ref Cfg.BlockIdx.Map.empty in
    let after_block (block_idx : Cfg.BlockIdx.t) : Result.t = match Map.find !block_out block_idx with
      | Some r -> r
      | None -> bottom in
    (* Data of the analysis, per instruction *)
    let instr_data : Result.intra_results ref = ref Instr.Label.Map.empty in
    (* Applies the transfer function to an entire block *)
    let transfer (b : 'a Basic_block.t) (state : Transfer.State.t) : Result.t =
      match b.content with
      | Data instrs ->
        Simple (List.fold_left instrs ~init:state ~f:(fun prestate instr ->
            let poststate = Transfer.data module_ cfg instr prestate in
            instr_data := Instr.Label.Map.set !instr_data ~key:instr.label ~data:(prestate, Simple poststate);
            poststate))
      | Call instr ->
        let poststate = Result.Simple (CallAdapter.analyze_call module_ cfg instr state extra) in
        instr_data := Instr.Label.Map.set !instr_data ~key:instr.label ~data:(state, poststate);
        poststate
      | Entry | Return _ -> failwith "Should not have a entry/return in an intra analysis"
      | Control instr ->
        let poststate = match Transfer.control module_ cfg instr state with
          | `Simple s -> Result.Simple s
          | `Branch (s1, s2) -> Result.Branch (s1, s2)
        in
        instr_data := Instr.Label.Map.set !instr_data ~key:instr.label ~data:(state, poststate);
        poststate in

    (* Analyzes one block, returning the state after this block *)
    let analyze_block (block_idx : Cfg.BlockIdx.t) : Result.t =
      (* The block to analyze *)
      let block = Cfg.find_block_exn cfg block_idx in
      let incoming = Cfg.predecessors cfg block_idx in
      (* in_state is the join of all the the out_state of the predecessors.
         Special case: if the out_state of a predecessor is not a simple one, that means we are the target of a break.
         If this is the case, we pick the right branch, according to the edge data *)
      let pred_states = (List.map incoming ~f:(fun (idx, d) -> match (after_block idx, d) with
          | Simple s, _ -> (idx, s)
          | Branch (t, _), Some true -> (idx, t)
          | Branch (_, f), Some false -> (idx, f)
          | Branch _, None -> failwith (Printf.sprintf "invalid branch state at block %s, from block %s"
                                          (Cfg.BlockIdx.to_string block_idx)
                                          (Cfg.BlockIdx.to_string idx))
          | Uninitialized, _ -> (idx, Transfer.bottom))) in
      let is_entry = cfg.entry_block = block_idx in
      let in_state =
        (* If it's the entry state, take the init value (there should only be one way to reach the entry), otherwise merge the flows *)
        if is_entry then
          Transfer.init module_ (Wasm_module.get_funcinst module_ block.fidx)
        else
          Transfer.merge_flows module_ cfg block pred_states in
      (* We analyze it *)
      transfer block in_state
    in
    let rec fixpoint (worklist : Cfg.BlockIdx.Set.t) (iteration : int) : unit =
      if IntSet.is_empty worklist then
        () (* No more elements to consider. We can stop here *)
      else
        let block_idx = Set.min_elt_exn worklist in
        Log.debug (Printf.sprintf "-----------------------\n Analyzing block %s\n" (Cfg.BlockIdx.to_string block_idx));
        let out_state = analyze_block block_idx in
        Log.debug (Printf.sprintf "out_state is: %s\n" (Result.to_string out_state));
        (* Has out state changed? *)
        let previous_out_state = after_block block_idx in
        match previous_out_state with
        | st when Result.compare out_state st = 0 ->
          (* Didn't change, we can safely ignore the successors *)
          fixpoint (IntSet.remove worklist block_idx) (iteration+1)
        | _ ->
          (* Update the out state in the analysis results.
             We join with the previous results *)
          let new_out_state =
            (* XXX: Join may not be necessary here, as long as out_state is greater than previous_out_state *)
            if Cfg.is_loop_head cfg block_idx then
              Result.widen previous_out_state (Result.join previous_out_state out_state)
            else

              Result.join previous_out_state out_state
          in
          block_out := IntMap.set !block_out ~key:block_idx ~data:new_out_state;
          (* And recurse by adding all successors *)
          let successors = Cfg.successors cfg block_idx in
          fixpoint (IntSet.union (IntSet.remove worklist block_idx) (Cfg.BlockIdx.Set.of_list successors)) (iteration+1)
    in
    (* Performs narrowing by re-analyzing once each block *)
    let rec narrow (blocks : Cfg.BlockIdx.t list) : unit = match blocks with
      | [] -> ()
      | block_idx :: blocks ->
        let out_state = analyze_block block_idx in
        block_out := IntMap.set !block_out ~key:block_idx ~data:out_state;
        narrow blocks
    in
    fixpoint (Cfg.BlockIdx.Set.singleton (Cfg.entry_block cfg)) 1;
    narrow (IntMap.keys cfg.basic_blocks);
    !instr_data

  let analyze
      (module_ : Wasm_module.t)
      (cfg : Transfer.annot_expected Cfg.t)
      (extra : CallAdapter.extra)
    : Transfer.State.t Cfg.t =
    let to_state (results_pair : Result.t * Result.t) : (Transfer.State.t * Transfer.State.t) =
      (Result.to_state (fst results_pair), Result.to_state (snd results_pair)) in
    let instr_data = analyze_ module_ cfg extra in
    let analyzed_cfg = Cfg.map_annotations cfg
        ~f:(fun i -> to_state (match Instr.Label.Map.find instr_data (Instr.label i) with
            | Some (before, after) -> (Simple before, after)
            | None -> (Uninitialized, Uninitialized))) in
    analyzed_cfg

end

module IntraOnlyCallAdapter (Transfer : Transfer.INTRA_ONLY_TRANSFER)
  : CALL_ADAPTER with module Transfer = Transfer and type extra = unit = struct
  module Transfer = Transfer
  type extra = unit

  let analyze_call
      (module_ : Wasm_module.t)
      (cfg : Transfer.annot_expected Cfg.t)
      (instr : Transfer.annot_expected Instr.labelled_call)
      (state : Transfer.State.t)
      (() : unit)
    : Transfer.State.t =
    Transfer.call module_ cfg instr state
end

(** TODO: this is an exact duplicate of Make, for typing purpose. It should be possible to avoid this *)
module MakeSumm
    (Transfer : Transfer.SUMMARY_TRANSFER)
    (CallAdapter : CALL_ADAPTER with module Transfer = Transfer) = struct
  (* Include transfer to get a definition for state, equal_state, and annot_expected *)
  module Transfer = Transfer
  module Result = Result(Transfer)
  type extra = CallAdapter.extra

  (** Analyzes a CFG. Returns the final state after computing the transfer of the entire function. That final state is a pair where the first element are the results per block, and the second element are the results per instructions.
      @param module_ is the overall WebAssembly module, needed to access type information, tables, etc.
      @param cfg is the CFG to analyze *)
  let analyze_ (module_ : Wasm_module.t) (cfg : Transfer.annot_expected Cfg.t) (extra : CallAdapter.extra) : Result.intra_results =
    let bottom = Result.Uninitialized in
    (* Data of the analysis, per block *)
    let block_out : Result.t Cfg.BlockIdx.Map.t ref = ref Cfg.BlockIdx.Map.empty in
    let after_block (block_idx : Cfg.BlockIdx.t) : Result.t = match Map.find !block_out block_idx with
      | Some r -> r
      | None -> bottom in
    (* Data of the analysis, per instruction *)
    let instr_data : Result.intra_results ref = ref Instr.Label.Map.empty in
    (* Applies the transfer function to an entire block *)
    let transfer (b : 'a Basic_block.t) (state : Transfer.State.t) : Result.t =
      match b.content with
      | Data instrs ->
        Simple (List.fold_left instrs ~init:state ~f:(fun prestate instr ->
            let poststate = Transfer.data module_ cfg instr prestate in
            instr_data := Instr.Label.Map.set !instr_data ~key:instr.label ~data:(prestate, Simple poststate);
            poststate))
      | Call instr ->
        let poststate = Result.Simple (CallAdapter.analyze_call module_ cfg instr state extra) in
        instr_data := Instr.Label.Map.set !instr_data ~key:instr.label ~data:(state, poststate);
        poststate
      | Entry | Return _ -> failwith "Should not have a return in an intra analysis"
      | Control instr ->
        let poststate = match Transfer.control module_ cfg instr state with
          | `Simple s -> Result.Simple s
          | `Branch (s1, s2) -> Result.Branch (s1, s2)
        in
        instr_data := Instr.Label.Map.set !instr_data ~key:instr.label ~data:(state, poststate);
        poststate in

    (* Analyzes one block, returning the state after this block *)
    let analyze_block (block_idx : Cfg.BlockIdx.t) : Result.t =
      (* The block to analyze *)
      let block = Cfg.find_block_exn cfg block_idx in
      let incoming = Cfg.predecessors cfg block_idx in
      (* in_state is the join of all the the out_state of the predecessors.
         Special case: if the out_state of a predecessor is not a simple one, that means we are the target of a break.
         If this is the case, we pick the right branch, according to the edge data *)
      let pred_states = (List.map incoming ~f:(fun (idx, d) -> match (after_block idx, d) with
          | Simple s, _ -> (idx, s)
          | Branch (t, _), Some true -> (idx, t)
          | Branch (_, f), Some false -> (idx, f)
          | Branch _, None -> failwith (Printf.sprintf "invalid branch state at block %s, from block %s"
                                          (Cfg.BlockIdx.to_string block_idx)
                                          (Cfg.BlockIdx.to_string idx))
          | Uninitialized, _ -> (idx, Transfer.bottom))) in
      let is_entry = cfg.entry_block = block_idx in
      let in_state =
        (* If it's the entry state, take the init value (there should only be one way to reach the entry), otherwise merge the flows *)
        if is_entry then
          Transfer.init module_ (Wasm_module.get_funcinst module_ block.fidx)
        else
          Transfer.merge_flows module_ cfg block pred_states in
      (* We analyze it *)
      transfer block in_state
    in

    let rec fixpoint (worklist : Cfg.BlockIdx.Set.t) (iteration : int) : unit =
      if IntSet.is_empty worklist then
        () (* No more elements to consider. We can stop here *)
      else
        let block_idx = Set.min_elt_exn worklist in
        Log.debug (Printf.sprintf "-----------------------\n Analyzing block %s\n" (Cfg.BlockIdx.to_string block_idx));
        let out_state = analyze_block block_idx in
        Log.debug (Printf.sprintf "out_state is: %s\n" (Result.to_string out_state));
        (* Has out state changed? *)
        let previous_out_state = after_block block_idx in
        match previous_out_state with
        | st when Result.compare out_state st = 0 ->
          (* Didn't change, we can safely ignore the successors *)
          fixpoint (IntSet.remove worklist block_idx) (iteration+1)
        | _ ->
          (* Update the out state in the analysis results.
             We join with the previous results *)
          let new_out_state =
            (* XXX: Join may not be necessary here, as long as out_state is greater than previous_out_state. But it is safe and should not change precision (as x \sqsubseteq y => x \join y = y) *)
            if Cfg.is_loop_head cfg block_idx then
              Result.widen previous_out_state (Result.join previous_out_state out_state)
            else
              Result.join previous_out_state out_state
          in
          block_out := IntMap.set !block_out ~key:block_idx ~data:new_out_state;
          (* And recurse by adding all successors *)
          let successors = Cfg.successors cfg block_idx in
          fixpoint (IntSet.union (IntSet.remove worklist block_idx) (Cfg.BlockIdx.Set.of_list successors)) (iteration+1)
    in
    (* Performs narrowing by re-analyzing once each block *)
    let rec _narrow (blocks : Cfg.BlockIdx.t list) : unit = match blocks with
      | [] -> ()
      | block_idx :: blocks ->
        let out_state = analyze_block block_idx in
        block_out := IntMap.set !block_out ~key:block_idx ~data:out_state;
        _narrow blocks
    in
    fixpoint (Cfg.BlockIdx.Set.singleton (Cfg.entry_block cfg)) 1;
    (* _narrow (IntMap.keys cfg.basic_blocks); *)
    !instr_data

  let analyze
      (module_ : Wasm_module.t)
      (cfg : Transfer.annot_expected Cfg.t)
      (extra : CallAdapter.extra)
    : Transfer.State.t Cfg.t =
    let to_state (results_pair : Result.t * Result.t) : (Transfer.State.t * Transfer.State.t) =
      (Result.to_state (fst results_pair), Result.to_state (snd results_pair)) in
    let instr_data = analyze_ module_ cfg extra in
    let analyzed_cfg = Cfg.map_annotations cfg
        ~f:(fun i -> to_state (match Instr.Label.Map.find instr_data (Instr.label i) with
            | Some (before, after) -> (Simple before, after)
            | None -> (Uninitialized, Uninitialized))) in
    analyzed_cfg
end

module SummaryCallAdapter (Transfer : Transfer.SUMMARY_TRANSFER)
  : CALL_ADAPTER with module Transfer = Transfer and type extra = Transfer.summary Int32Map.t = struct
  module Transfer = Transfer
  type extra = Transfer.summary Int32Map.t

  let analyze_call
      (module_ : Wasm_module.t)
      (_cfg : Transfer.annot_expected Cfg.t)
      (instr : Transfer.annot_expected Instr.labelled_call)
      (state : Transfer.State.t)
      (summaries : extra)
    : Transfer.State.t =
    let apply_summary f arity state =
      match Int32Map.find summaries f with
      | None ->
        if Int32.(f < module_.nfuncimports) then
          Transfer.apply_imported module_ f arity instr state
        else
          (* This function depend on another function that has not been analyzed yet, so it is part of some recursive loop. It will eventually stabilize *)
          state
      | Some summary ->
        Transfer.apply_summary module_ f arity instr state summary in
    match instr.instr with
    | CallDirect (arity, _, f) -> apply_summary f arity state
    | CallIndirect (_, arity, _, typ) ->
      let targets = Call_graph.indirect_call_targets module_ typ in
      (* Apply the summaries and joins them *)
      List.fold_left targets
        ~init:state
        ~f:(fun acc idx -> Transfer.State.join (apply_summary idx arity state) acc)
end

module MakeClassicalInter (Transfer : Transfer.CLASSICAL_INTER_TRANSFER) = struct
  module Transfer = Transfer
  module Result = Result(Transfer)

  module ResultKey = struct
    module T = struct
      type kind = Entry | Return | None
      [@@deriving sexp, compare]

      type t = {
        label: Instr.Label.t;
        kind: kind;
      }
      [@@deriving sexp, compare]
    end
    include T
    module Map = Map.Make(T)
  end
  type results = (Transfer.State.t * Result.t) ResultKey.Map.t

  (** Analyzes a CFG. Returns the final state after computing the transfer of the entire function. That final state is a pair where the first element are the results per block, and the second element are the results per instructions.
      @param module_ is the overall WebAssembly module, needed to access type information, tables, etc.
      @param cfg is the CFG to analyze *)
  let analyze_ (module_ : Wasm_module.t) (icfg : Transfer.annot_expected Icfg.t) : Result.intra_results =
    let bottom = Result.Uninitialized in
    (* Data of the analysis, per block *)
    let block_out : Result.t Icfg.BlockIdx.Map.t ref = ref Icfg.BlockIdx.Map.empty in
    let after_block (block_idx : Icfg.BlockIdx.t) : Result.t = match Map.find !block_out block_idx with
      | Some r -> r
      | None -> bottom in
    (* Data of the analysis, per instruction *)
    let instr_data : results ref = ref ResultKey.Map.empty in
    (* Applies the transfer function to an entire block *)
    let transfer (b : 'a Basic_block.t) (state : Transfer.State.t) : Result.t =
       Printf.printf "Analysis of block %ld_%d from state %s\n" b.fidx b.idx (Transfer.State.to_string state);
      let cfg = Map.find_exn icfg.cfgs b.fidx in
      let result = match b.content with
        | Data instrs ->
          Result.Simple (List.fold_left instrs ~init:state ~f:(fun prestate instr ->
              let poststate = Transfer.data module_ cfg instr prestate in
              instr_data := Instr.Label.Map.set !instr_data ~key:{ label = instr.label; kind = None } ~data:(prestate, Simple poststate);
              poststate))
        | Call instr ->
          let poststate = Transfer.call_inter module_ cfg instr state in
          instr_data := Instr.Label.Map.set !instr_data ~key:{ label = instr.label; kind = None } ~data:(state, Simple poststate);
          Simple poststate
        | Entry ->
          let poststate = Transfer.entry module_ cfg state in
          Simple poststate
        | Return instr ->
          let state_before_entry = match Map.find !instr_data { label = instr.label; kind = None } with
            | None ->
              (* This happens when we return to a function we have not yet analyzed. This can happen with a call graph such as:
                 1 calls 2 and 3.
                 2 calls 4.
                 3 calls 4.
                 Then, 1 will be analyzed, followed by 2, followed by 4. Upon return, we will go to the return site of 3 as well, even though we haven't analyzed the call in 3. *)
              Printf.printf "Return block has no previous state %ld_%d\n" b.fidx b.idx;
              Transfer.bottom
            | Some (state, _) -> state in
          let poststate = Transfer.return module_ cfg instr state_before_entry state in
          instr_data := Instr.Label.Map.set !instr_data ~key:{ label = instr.label; kind = Return } ~data:(state, Simple poststate);
          Simple poststate
        | Control instr ->
          let poststate = match Transfer.control module_ cfg instr state with
            | `Simple s -> Result.Simple s
            | `Branch (s1, s2) -> Result.Branch (s1, s2)
          in
          instr_data := Instr.Label.Map.set !instr_data ~key:{ label = instr.label; kind = None } ~data:(state, poststate);
          poststate in
       Printf.printf "Analysis of block %ld_%d results in state %s\n" b.fidx b.idx (Result.to_string result);
      result in

    (* Analyzes one block, returning the state after this block *)
    let analyze_block (block_idx : Icfg.BlockIdx.t) (block : 'a Basic_block.t) : Result.t =
      let incoming = Icfg.predecessors icfg block_idx in
      (* in_state is the join of all the the out_state of the predecessors.
         Special case: if the out_state of a predecessor is not a simple one, that means we are the target of a break.
         If this is the case, we pick the right branch, according to the edge data *)
      let pred_states = (List.map incoming ~f:(fun (idx, d) -> match (after_block idx, d) with
          | Simple s, _ -> (idx, s)
          | Branch (t, _), Some true -> (idx, t)
          | Branch (_, f), Some false -> (idx, f)
          | Branch _, None -> failwith (Printf.sprintf "invalid branch state at block %s, from block %s"
                                          (Icfg.BlockIdx.to_string block_idx)
                                          (Icfg.BlockIdx.to_string idx))
          | Uninitialized, _ -> (idx, Transfer.bottom))) in
      let cfg = Map.find_exn icfg.cfgs block_idx.fidx in
      let is_entry = Int32.(icfg.entry = block_idx.fidx) && cfg.entry_block = block_idx.block_idx &&
                     match block_idx.kind with Regular -> true | _ -> false in
      let in_state =
        (* If it's the entry state, take the init value (there should only be one way to reach the entry), otherwise merge the flows *)
        if is_entry then
          Transfer.init module_ (Wasm_module.get_funcinst module_ block.fidx)
        else
          (* TODO: if it's a return state, we need to take the imported functions into account here. *)
          (* TODO: The problem: we need a block index, which we can't have. The
             block index is only used to extract the previous annot_expected. We
             could compute it, but we need an "init annot_expected", which is
             annoying. BUT maybe we can just replicate what state_after_|lock
             does, and ignore the entry case. After all, if there's a merge, we
             don't expect to come from the entry state? This would be because
             entry_state is only used if the block is Data[], Etnry, or Return,
             and there's no predecessor or only empty ones *)
          (* TODO: merge_flows can correctly deal with defined functions. But for imported ones, we likely need something different. *)
          (* TODO: it would be much easier to have a dummy function for imported with a specific block. Then one can just define the transfer on that kind of block *)
          Transfer.merge_flows module_ cfg block (List.map ~f:(fun (b, s) -> (b.block_idx, s)) pred_states) in
      (* We analyze it *)
      transfer block in_state
    in

    let rec fixpoint (worklist : Icfg.BlockIdx.Set.t) (iteration : int) : unit =
      if IntSet.is_empty worklist then
        () (* No more elements to consider. We can stop here *)
      else
        let block_idx = Set.min_elt_exn worklist in
        let block = Icfg.find_block_exn icfg block_idx in
        Printf.printf "-----------------------\n Analyzing block %s\n" (Icfg.BlockIdx.to_string block_idx);
        let out_state = analyze_block block_idx block in
        Log.debug (Printf.sprintf "out_state is: %s\n" (Result.to_string out_state));
        (* Has out state changed? *)
        let previous_out_state = after_block block_idx in
        Printf.printf "out_state is %s and previous is %s, are they equal? %d\n"
          (Result.to_string out_state)
          (Result.to_string previous_out_state)
          (Result.compare out_state previous_out_state);
        match out_state with
        | st when Result.compare previous_out_state st = 0 ->
          (* Didn't change (or stayed bottom), we can safely ignore the successors *)
          fixpoint (IntSet.remove worklist block_idx) (iteration+1)
        | Simple s when Transfer.State.equal s Transfer.bottom ->
          Printf.printf "stayed bottom\n";
          (* Didn't change (or stayed bottom), we can safely ignore the successors *)
          fixpoint (IntSet.remove worklist block_idx) (iteration+1)
        | _ ->
          Printf.printf "fall through\n";
          (* Update the out state in the analysis results.
             We join with the previous results *)
          let new_out_state =
            (* XXX: Join may not be necessary here, as long as out_state is greater than previous_out_state *)
            if Icfg.is_loop_head icfg block_idx then
              Result.widen previous_out_state (Result.join previous_out_state out_state)
            else begin
              Printf.printf "joining %s and %s at %s\n"
                (Result.to_string previous_out_state)
                (Result.to_string out_state)
                (Icfg.BlockIdx.to_string block_idx);
              Result.join previous_out_state out_state
            end
          in
          block_out := IntMap.set !block_out ~key:block_idx ~data:new_out_state;
          (* And recurse by adding all successors *)
          let successors = Icfg.successors icfg block_idx in
          let successors = match block.content with
            | Call _ ->
              (* If it's a call, we need to add the corresponding return as a
                 successor. This is because the analysis of the return depends
                 both on the state after the called function and on the state
                 after the call but before entering the function *)
              successors @ [{block_idx with kind = Return}] (* add it at the end because usually we want to analyze the callee before *)
            | _ -> successors in
          Printf.printf "adding successors: %s\n" (String.concat ~sep:"," (List.map ~f:Icfg.BlockIdx.to_string successors));
          fixpoint (IntSet.union (IntSet.remove worklist block_idx) (Icfg.BlockIdx.Set.of_list successors)) (iteration+1)
    in
    (* Performs narrowing by re-analyzing once each block *)
    let rec _narrow (blocks : Icfg.BlockIdx.t list) : unit = match blocks with
      | [] -> ()
      | block_idx :: blocks ->
        let block = Icfg.find_block_exn icfg block_idx in
        let out_state = analyze_block block_idx block in
        block_out := IntMap.set !block_out ~key:block_idx ~data:out_state;
        _narrow blocks
    in
    fixpoint (Icfg.BlockIdx.Set.singleton (Icfg.entry_block icfg)) 1;
    (* _narrow (IntMap.keys cfg.basic_blocks); *)
    !instr_data
    |> Map.to_alist
    |> List.filter_map ~f:(fun (k, v) -> match k.kind with
        | None -> Some (k.label, v)
        | _ -> None)
    |> Instr.Label.Map.of_alist_exn

  let analyze
      (module_ : Wasm_module.t)
      (icfg : Transfer.annot_expected Icfg.t)
    : Transfer.State.t Icfg.t =
    let to_state (results_pair : Result.t * Result.t) : (Transfer.State.t * Transfer.State.t) =
      (Result.to_state (fst results_pair), Result.to_state (snd results_pair)) in
    let instr_data = analyze_ module_ icfg in
    let analyzed_cfg = Icfg.map_annotations icfg
        ~f:(fun i -> to_state (match Instr.Label.Map.find instr_data (Instr.label i) with
            | Some (before, after) -> (Simple before, after)
            | None -> (Uninitialized, Uninitialized))) in
    analyzed_cfg
end


module MakeIntraOnly (Transfer : Transfer.INTRA_ONLY_TRANSFER) = Make(Transfer)(IntraOnlyCallAdapter(Transfer))
module MakeSummaryBased (Transfer : Transfer.SUMMARY_TRANSFER)
    : INTRA_FOR_SUMMARY with module Transfer = Transfer
  = MakeSumm(Transfer)(SummaryCallAdapter(Transfer))
