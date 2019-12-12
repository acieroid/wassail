open Core_kernel
open Helpers

(* TODO: change to Domain.state IntMap.t, adapt find_exn to find in fixpoint *)
let data : ((Domain.state option) IntMap.t) ref = ref IntMap.empty

(* Analyze multiple CFGS, returns a map from CFG id to out_state for each CFG *)
let analyze (cfgs : Cfg.t IntMap.t) (nglobals : int) : Domain.state IntMap.t =
  data := (IntMap.of_alist_exn (List.map (IntMap.keys cfgs)
                                  ~f:(fun idx ->
                                      (idx, None))));
  let rec fixpoint (worklist : IntSet.t)
      (globals : Domain.globals) (memory : Domain.memory)
      (summaries : Summary.t IntMap.t) (calls : (Value.t list) IntMap.t) =
    if IntSet.is_empty worklist then
      () (* empty worklist, analysis finished *)
    else
      let cfg_idx = IntSet.min_elt_exn worklist in
      let cfg = IntMap.find_exn cfgs cfg_idx in
      let args = List.init (fst cfg.arity) ~f:(fun i -> Value.top Type.I32Type (Parameter i))
      (* We could use the following definition for having more precise args, but this will defeat the purpose of compositional analysis *)
          (* match IntMap.find calls cfg_idx with
        | Some _ when cfg.exported ->
          (* We have stored specific arguments, but this function is exported so it can be called with any argument *)
          List.init (fst cfg.arity) ~f:(fun i -> Value.top Type.I32Type (Parameter i))
        | Some args ->
          (* Function is not exported, so it can only be called with what we discovered *)
          args
        | None when cfg.exported ->
          (* No call has been analyzed yet, and this function is exported, so we start from top *)
          List.init (fst cfg.arity) ~f:(fun i -> Value.top Type.I32Type (Parameter i))
        | None ->
          (* No call analyzed, function is not called from anywhere, use bottom as arguments *)
             List.init (fst cfg.arity) ~f:(fun _ -> Value.bottom)  *) in
      Printf.printf "Analyzing cfg %d with globals: [%s] and args: [%s]\n" cfg_idx (Domain.globals_to_string globals) (Value.list_to_string args);
      let out_state = Intra_fixpoint.analyze_coarse cfg args globals memory summaries in
      let previous_out_state = IntMap.find_exn !data cfg_idx in
      match previous_out_state with
      | Some st when Domain.compare_state out_state st = 0 ->
        (* Same results as before, we can just recurse without having to recompute globals nor memory nor calls *)
        fixpoint (IntSet.remove worklist cfg_idx) globals memory summaries calls
      | _ ->
        (* Result differed, we have to add all callees and callers to the worklist.
           Callers because the analyzed function could have modified globals/memory that will be read by the caller.
           Callees for the same reason. *)
        let callees = Cfg.callees cfg in
        let callers = Cfg.callers cfgs cfg in
        let new_globals = globals
        (* Globals don't change, we are compositional now *)
          (* Domain.join_globals globals out_state.globals *) in
        let new_memory = Domain.join_memory memory out_state.memory in
        let summary = Summary.make cfg out_state in
        let new_summaries = IntMap.set summaries ~key:cfg.idx ~data:summary in
        let new_calls = IntMap.merge calls out_state.calls ~f:(fun ~key:_ data -> match data with
            | `Both (a, b) -> Some (Value.join_vlist_exn a b)
            | `Left a -> Some a
            | `Right b -> Some b) in
        data := IntMap.set !data ~key:cfg_idx ~data:(Some out_state);
        fixpoint (IntSet.union (IntSet.remove worklist cfg_idx) (IntSet.union callees callers)) new_globals new_memory new_summaries new_calls
  in
  let summaries0 = IntMap.map cfgs ~f:(fun cfg -> Summary.bottom cfg) in
  let calls0 = IntMap.empty in
  let globals = (List.init nglobals ~f:(fun i -> Value.top Type.I32Type (Global i))) in
  fixpoint (IntSet.of_list (IntMap.keys cfgs)) globals TODO summaries0 calls0 ;
  IntMap.map !data ~f:(fun v -> match v with
      | Some result -> result
      | None -> failwith "...")
