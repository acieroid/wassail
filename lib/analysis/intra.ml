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
    -> Transfer.annot_expected Transfer.Cfg.t
    -> Transfer.annot_expected Instr.labelled_call
    -> Transfer.State.t
    -> extra
    -> [ `Simple of Transfer.State.t | `Multiple of Transfer.State.t list ]
end

module Make
    (Transfer : Transfer.TRANSFER_BASE)
    (CallAdapter : CALL_ADAPTER with module Transfer = Transfer) = struct
  (* Include transfer to get a definition for state, equal_state, and annot_expected *)
  module Transfer = Transfer
  module Cfg = Transfer.Cfg
  type extra = CallAdapter.extra

  (** The result of applying the transfer function. *)
  type result =
    | Uninitialized (** Meaning it has not been computed yet *)
    | Simple of Transfer.State.t (** A single successor *)
    | Branch of Transfer.State.t * Transfer.State.t (** Upon a `br_if`, there are two successor states: one where the condition holds, and where where it does not hold. This is used to model that. *)
    | Multiple of Transfer.State.t list (** Upon a call_indirect (or br_table), there could be more than two successors *)
  [@@deriving compare]

  (** The results of an intra analysis are a mapping from instruction labels to their in and out values *)
  type intra_results = (result * result) Instr.Label.Map.t

  (** Converts a result to a state. May require joining output states in case of branching *)
  let result_to_state (cfg : Transfer.annot_expected Cfg.t) (r : result) : Transfer.State.t = match r with
    | Uninitialized -> Transfer.bottom cfg
    | Simple s -> s
    | Branch (s1, s2) -> Transfer.State.join s1 s2
    | Multiple states -> List.fold_left states ~init:(Transfer.bottom cfg) ~f:Transfer.State.join

  let result_to_string (r : result) : string = match r with
    | Uninitialized -> "uninit"
    | Simple s -> Printf.sprintf "simple: %s" (Transfer.State.to_string s)
    | Branch (s1, s2) -> Printf.sprintf "branch: %s\nand: %s" (Transfer.State.to_string s1) (Transfer.State.to_string s2)
    | Multiple states -> Printf.sprintf "multiple: [%s]" (String.concat ~sep:"," (List.map ~f:Transfer.State.to_string states))

  (** Analyzes a CFG. Returns the final state after computing the transfer of the entire function. That final state is a pair where the first element are the results per block, and the second element are the results per instructions.
      @param module_ is the overall WebAssembly module, needed to access type information, tables, etc.
      @param cfg is the CFG to analyze *)
  let analyze_ (module_ : Wasm_module.t) (cfg : Transfer.annot_expected Cfg.t) (extra : CallAdapter.extra) : intra_results =
    let bottom = Uninitialized in
    (* Data of the analysis, per block *)
    let block_out : result Cfg.BlockIdx.Map.t ref = ref Cfg.BlockIdx.Map.empty in
    let after_block (block_idx : Cfg.BlockIdx.t) : result = match Map.find !block_out block_idx with
      | Some r -> r
      | None -> bottom in
    (* Data of the analysis, per instruction *)
    let instr_data : intra_results ref = ref Instr.Label.Map.empty in
    (* Applies the transfer function to an entire block *)
    let transfer (b : 'a Basic_block.t) (state : Transfer.State.t) : result =
      match b.content with
      | Data instrs ->
        Simple (List.fold_left instrs ~init:state ~f:(fun prestate instr ->
            let poststate = Transfer.data module_ cfg instr prestate in
            instr_data := Instr.Label.Map.set !instr_data ~key:instr.label ~data:(Simple prestate, Simple poststate);
            poststate))
      | Call instr ->
        let poststate = match CallAdapter.analyze_call module_ cfg instr state extra with
          | `Simple s -> Simple s
          | `Multiple states -> Multiple states in
        instr_data := Instr.Label.Map.set !instr_data ~key:instr.label ~data:(Simple state, poststate);
        poststate
      | Control instr ->
        let poststate = match Transfer.control module_ cfg instr state with
          | `Simple s -> Simple s
          | `Branch (s1, s2) -> Branch (s1, s2)
          | `Multiple states -> Multiple states
        in
        instr_data := Instr.Label.Map.set !instr_data ~key:instr.label ~data:(Simple state, poststate);
        poststate in

    (* Analyzes one block, returning the state after this block *)
    let analyze_block (block_idx : Cfg.BlockIdx.t) : result =
      (* The block to analyze *)
      let block = Cfg.find_block_exn cfg block_idx in
      let incoming = Cfg.incoming_edges cfg block_idx in
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
          | Multiple states, _ -> (idx, Transfer.merge_flows module_ cfg block (List.map ~f:(fun s -> (idx, s)) states))
          | Uninitialized, _ -> (idx, Transfer.bottom cfg))) in
      let in_state = Transfer.merge_flows module_ cfg block pred_states in
      (* We analyze it *)
      transfer block in_state
    in
    let join_result  (r1 : result) (r2 : result) =
    match (r1, r2) with
    | Uninitialized, _ -> r2
    | _, Uninitialized -> r1
    | Simple st1, Simple st2 ->
      let joined = Transfer.State.join st1 st2 in
      Simple joined
    | Branch (st1, st2), Branch (st1', st2') ->
      Branch (Transfer.State.join st1 st1',
              Transfer.State.join st2 st2')
    | _ -> failwith "Cannot join results" in
    let widen_result (r1 : result) (r2 : result) =
      match (r1, r2) with
      | Uninitialized, _ -> r2
      | _, Uninitialized -> r1
      | Simple st1, Simple st2 ->
        Simple (Transfer.State.widen st1 st2)
      | Branch (st1, st2), Branch (st1', st2') ->
        Branch (Transfer.State.widen st1 st1',
                Transfer.State.widen st2 st2')
      | _ -> failwith "Cannot widen results" in
    let rec fixpoint (worklist : Cfg.BlockIdx.Set.t) (iteration : int) : unit =
      if IntSet.is_empty worklist then
        () (* No more elements to consider. We can stop here *)
      else
        let block_idx = Set.min_elt_exn worklist in
        Log.debug (Printf.sprintf "-----------------------\n Analyzing block %s\n" (Cfg.BlockIdx.to_string block_idx));
        let out_state = analyze_block block_idx in
        Log.debug (Printf.sprintf "out_state is: %s\n" (result_to_string out_state));
        (* Has out state changed? *)
        let previous_out_state = after_block block_idx in
        match previous_out_state with
        | st when compare_result out_state st = 0 ->
          (* Didn't change, we can safely ignore the successors *)
          fixpoint (IntSet.remove worklist block_idx) (iteration+1)
        | _ ->
          (* Update the out state in the analysis results.
             We join with the previous results *)
          let new_out_state =
            (* TODO: Join may not be necessary here, as long as out_state is greater than previous_out_state *)
            if Cfg.is_loop_head cfg block_idx then
              widen_result previous_out_state (join_result previous_out_state out_state)
            else
              join_result previous_out_state out_state
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
    let to_state (results_pair : result * result) : (Transfer.State.t * Transfer.State.t) =
      (result_to_state cfg (fst results_pair), result_to_state cfg (snd results_pair)) in
    let instr_data = analyze_ module_ cfg extra in
    let analyzed_cfg = Cfg.map_annotations cfg
        ~f:(fun i -> to_state (match Instr.Label.Map.find instr_data (Instr.label i) with
            | Some v -> v
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
    : [ `Simple of Transfer.State.t | `Multiple of Transfer.State.t list ] =
    Transfer.call module_ cfg instr state
end

module MakeSumm
    (Transfer : Transfer.SUMMARY_TRANSFER)
    (CallAdapter : CALL_ADAPTER with module Transfer = Transfer) = struct
  (* Include transfer to get a definition for state, equal_state, and annot_expected *)
  module Transfer = Transfer
  module Cfg = Transfer.Cfg
  type extra = CallAdapter.extra

  (** The result of applying the transfer function. *)
  type result =
    | Uninitialized (** Meaning it has not been computed yet *)
    | Simple of Transfer.State.t (** A single successor *)
    | Branch of Transfer.State.t * Transfer.State.t (** Upon a `br_if`, there are two successor states: one where the condition holds, and where where it does not hold. This is used to model that. *)
    | Multiple of Transfer.State.t list (** Upon a call_indirect (or br_table), there could be more than two successors *)
  [@@deriving compare]

  (** The results of an intra analysis are a mapping from instruction labels to their in and out values *)
  type intra_results = (result * result) Instr.Label.Map.t

  (** Converts a result to a state. May require joining output states in case of branching *)
  let result_to_state (cfg : Transfer.annot_expected Cfg.t) (r : result) : Transfer.State.t = match r with
    | Uninitialized -> Transfer.bottom cfg
    | Simple s -> s
    | Branch (s1, s2) -> Transfer.State.join s1 s2
    | Multiple states -> List.fold_left states ~init:(Transfer.bottom cfg) ~f:Transfer.State.join

  let result_to_string (r : result) : string = match r with
    | Uninitialized -> "uninit"
    | Simple s -> Printf.sprintf "simple: %s" (Transfer.State.to_string s)
    | Branch (s1, s2) -> Printf.sprintf "branch: %s\nand: %s" (Transfer.State.to_string s1) (Transfer.State.to_string s2)
    | Multiple states -> Printf.sprintf "multiple: [%s]" (String.concat ~sep:"," (List.map ~f:Transfer.State.to_string states))

  (** Analyzes a CFG. Returns the final state after computing the transfer of the entire function. That final state is a pair where the first element are the results per block, and the second element are the results per instructions.
      @param module_ is the overall WebAssembly module, needed to access type information, tables, etc.
      @param cfg is the CFG to analyze *)
  let analyze_ (module_ : Wasm_module.t) (cfg : Transfer.annot_expected Cfg.t) (extra : CallAdapter.extra) : intra_results =
    let bottom = Uninitialized in
    (* Data of the analysis, per block *)
    let block_out : result Cfg.BlockIdx.Map.t ref = ref Cfg.BlockIdx.Map.empty in
    let after_block (block_idx : Cfg.BlockIdx.t) : result = match Map.find !block_out block_idx with
      | Some r -> r
      | None -> bottom in
    (* Data of the analysis, per instruction *)
    let instr_data : intra_results ref = ref Instr.Label.Map.empty in
    (* Applies the transfer function to an entire block *)
    let transfer (b : 'a Basic_block.t) (state : Transfer.State.t) : result =
      match b.content with
      | Data instrs ->
        Simple (List.fold_left instrs ~init:state ~f:(fun prestate instr ->
            let poststate = Transfer.data module_ cfg instr prestate in
            instr_data := Instr.Label.Map.set !instr_data ~key:instr.label ~data:(Simple prestate, Simple poststate);
            poststate))
      | Call instr ->
        let poststate = match CallAdapter.analyze_call module_ cfg instr state extra with
          | `Simple s -> Simple s
          | `Multiple states -> Multiple states in
        instr_data := Instr.Label.Map.set !instr_data ~key:instr.label ~data:(Simple state, poststate);
        poststate
      | Control instr ->
        let poststate = match Transfer.control module_ cfg instr state with
          | `Simple s -> Simple s
          | `Branch (s1, s2) -> Branch (s1, s2)
          | `Multiple states -> Multiple states
        in
        instr_data := Instr.Label.Map.set !instr_data ~key:instr.label ~data:(Simple state, poststate);
        poststate in

    (* Analyzes one block, returning the state after this block *)
    let analyze_block (block_idx : Cfg.BlockIdx.t) : result =
      (* The block to analyze *)
      let block = Cfg.find_block_exn cfg block_idx in
      let incoming = Cfg.incoming_edges cfg block_idx in
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
          | Multiple states, _ -> (idx, Transfer.merge_flows module_ cfg block (List.map ~f:(fun s -> (idx, s)) states))
          | Uninitialized, _ -> (idx, Transfer.bottom cfg))) in
      let in_state = Transfer.merge_flows module_ cfg block pred_states in
      (* We analyze it *)
      transfer block in_state
    in
    let join_result  (r1 : result) (r2 : result) =
    match (r1, r2) with
    | Uninitialized, _ -> r2
    | _, Uninitialized -> r1
    | Simple st1, Simple st2 ->
      let joined = Transfer.State.join st1 st2 in
      Simple joined
    | Branch (st1, st2), Branch (st1', st2') ->
      Branch (Transfer.State.join st1 st1',
              Transfer.State.join st2 st2')
    | _ -> failwith "Cannot join results" in
    let widen_result (r1 : result) (r2 : result) =
      match (r1, r2) with
      | Uninitialized, _ -> r2
      | _, Uninitialized -> r1
      | Simple st1, Simple st2 ->
        Simple (Transfer.State.widen st1 st2)
      | Branch (st1, st2), Branch (st1', st2') ->
        Branch (Transfer.State.widen st1 st1',
                Transfer.State.widen st2 st2')
      | _ -> failwith "Cannot widen results" in
    let rec fixpoint (worklist : Cfg.BlockIdx.Set.t) (iteration : int) : unit =
      if IntSet.is_empty worklist then
        () (* No more elements to consider. We can stop here *)
      else
        let block_idx = Set.min_elt_exn worklist in
        Log.debug (Printf.sprintf "-----------------------\n Analyzing block %s\n" (Cfg.BlockIdx.to_string block_idx));
        let out_state = analyze_block block_idx in
        Log.debug (Printf.sprintf "out_state is: %s\n" (result_to_string out_state));
        (* Has out state changed? *)
        let previous_out_state = after_block block_idx in
        match previous_out_state with
        | st when compare_result out_state st = 0 ->
          (* Didn't change, we can safely ignore the successors *)
          fixpoint (IntSet.remove worklist block_idx) (iteration+1)
        | _ ->
          (* Update the out state in the analysis results.
             We join with the previous results *)
          let new_out_state =
            (* TODO: Join may not be necessary here, as long as out_state is greater than previous_out_state *)
            if Cfg.is_loop_head cfg block_idx then
              widen_result previous_out_state (join_result previous_out_state out_state)
            else
              join_result previous_out_state out_state
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

  let analyze (module_ : Wasm_module.t) (cfg : Transfer.annot_expected Cfg.t) (extra : CallAdapter.extra) : Transfer.State.t Cfg.t =
    let to_state (results_pair : result * result) : (Transfer.State.t * Transfer.State.t) =
      (result_to_state cfg (fst results_pair), result_to_state cfg (snd results_pair)) in
    let instr_data = analyze_ module_ cfg extra in
    let analyzed_cfg = Cfg.map_annotations cfg
        ~f:(fun i -> to_state (match Instr.Label.Map.find instr_data (Instr.label i) with
            | Some v -> v
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
    : [ `Simple of Transfer.State.t | `Multiple of Transfer.State.t list ] =
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
    | CallDirect (arity, _, f) -> `Simple (apply_summary f arity state)
    | CallIndirect (_, arity, _, typ) ->
      let targets = Call_graph.indirect_call_targets module_ typ in
      (* Apply the summaries and joins them *)
      `Simple (List.fold_left targets
        ~init:state
        ~f:(fun acc idx -> Transfer.State.join (apply_summary idx arity state) acc))
end

module MakeIntraOnly (Transfer : Transfer.INTRA_ONLY_TRANSFER) = Make(Transfer)(IntraOnlyCallAdapter(Transfer))
module MakeSummaryBased (Transfer : Transfer.SUMMARY_TRANSFER)
    : INTRA_FOR_SUMMARY with module Transfer = Transfer
  = MakeSumm(Transfer)(SummaryCallAdapter(Transfer))
