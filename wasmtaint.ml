open Core_kernel
open Wasm

include Helpers
module Store = Store
module Cfg_builder = Cfg_builder
module Cfg = Cfg
module Domain = Domain
module Instr = Instr


module IntraFixpoint = struct
  (* Analyzes a CFG. Returns a map where each basic blocks is mappped to its input state and output state *)
  let analyze (cfg : Cfg.t) (args : Value.t list) (globals : Domain.globals) (memory : Domain.memory) (summaries : Summary.t IntMap.t) : (Domain.state * Domain.state) IntMap.t =
    let bottom = None in
    assert (List.length args = (fst cfg.arity)); (* Given number of arguments should match the in arity of the function *)
    let init = Domain.init args cfg.nlocals globals memory in
    let data = ref (IntMap.of_alist_exn (List.map (IntMap.keys cfg.basic_blocks)
                                           ~f:(fun idx ->
                                               (idx, (bottom, bottom))))) in
    let rec fixpoint (worklist : IntSet.t) (iteration : int) : unit =
      if IntSet.is_empty worklist then
        () (* No more elements to consider. We can stop here *)
      else
        let block_idx = IntSet.min_elt_exn worklist in
        let predecessors = Cfg.predecessors cfg block_idx in
        (* in_state is the join of all the the out_state of the predecessors *)
        let in_state = Option.value (List.fold_left (List.map predecessors ~f:(fun idx -> snd (IntMap.find_exn !data idx))) ~init:bottom ~f:Domain.join_opt) ~default:init in
        (* The block to analyze *)
        let block = Cfg.find_block_exn cfg block_idx in
        (* We analyze it *)
        let out_state = Transfer.transfer block in_state summaries in
        (* Has out state changed? *)
        let previous_out_state = snd (IntMap.find_exn !data block_idx) in
        match previous_out_state with
        | Some st when Domain.compare_state out_state st = 0 ->
          (* Didn't change, we can safely ignore the successors *)
          (* TODO: make sure that this is true. If not, maybe we just have to put all blocks on the worklist for the first iteration(s) *)
          fixpoint (IntSet.remove worklist block_idx) (iteration+1)
        | _ ->
          (* Update the out state in the analysis results, joining it with the previous one *)
          let new_out_state = Domain.join_opt (Some out_state) previous_out_state in
          data := IntMap.set !data ~key:block_idx ~data:(Some in_state, new_out_state);
          (* And recurse by adding all successors *)
          let successors = Cfg.successors cfg block_idx in
          fixpoint (IntSet.union (IntSet.remove worklist block_idx) (IntSet.of_list successors)) (iteration+1)
    in
    fixpoint (IntSet.singleton cfg.entry_block) 1;
    IntMap.map !data ~f:(fun (in_state, out_state) -> (Option.value_exn in_state, Option.value_exn out_state))

  (* Similar to analyze, but only return the out state for a CFG *)
  let analyze_coarse (cfg : Cfg.t) (args : Value.t list) (globals : Domain.globals) (memory : Domain.memory) (summaries : Summary.t IntMap.t) : Domain.state =
    let results = analyze cfg args globals memory summaries in
    snd (IntMap.find_exn results cfg.exit_block)
end

module InterFixpoint = struct
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
        let args = match IntMap.find calls cfg_idx with
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
            List.init (fst cfg.arity) ~f:(fun _ -> Value.bottom) in
        Printf.printf "Analyzing cfg %d with globals: [%s] and args: [%s]\n" cfg_idx (Domain.globals_to_string globals) (Value.list_to_string args);
        let out_state = IntraFixpoint.analyze_coarse cfg args globals memory summaries in
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
          let new_globals = Domain.join_globals globals out_state.globals in
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
    fixpoint (IntSet.of_list (IntMap.keys cfgs)) (List.init nglobals ~f:(fun _ -> Value.zero Type.I32Type)) TODO summaries0 calls0 ;
    IntMap.map !data ~f:(fun v -> match v with
        | Some result -> result
        | None -> failwith "...")
end


let trace name = print_endline ("-- " ^ name)

let error at category msg =
  trace ("Error: ");
  prerr_endline (Source.string_of_region at ^ ": " ^ category ^ ": " ^ msg);
  false

let input_from get_script run =
  try
    let script = get_script () in
    trace "Running...";
    run script;
    true
  with
  | Decode.Code (at, msg) -> error at "decoding error" msg
  | Parse.Syntax (at, msg) -> error at "syntax error" msg
  | Valid.Invalid (at, msg) -> error at "invalid module" msg
  | Import.Unknown (at, msg) -> error at "link failure" msg
  | Eval.Link (at, msg) -> error at "link failure" msg
  | Eval.Trap (at, msg) -> error at "runtime trap" msg
  | Eval.Exhaustion (at, msg) -> error at "resource exhaustion" msg
  | Eval.Crash (at, msg) -> error at "runtime crash" msg
  | Encode.Code (at, msg) -> error at "encoding error" msg

let parse_file name run =
  let ic = In_channel.create name in
  try
    let lexbuf = Lexing.from_channel ic in
    let success = input_from (fun _ ->
        let var_opt, def = Parse.parse name lexbuf Parse.Module in
        [(var_opt, def)]) run in
    In_channel.close ic;
    success
  with exn -> In_channel.close ic; raise exn

let parse_string str run =
  let lexbuf = Lexing.from_string str in
    let success = input_from (fun _ ->
        let var_opt, def = Parse.parse "foo.wat" lexbuf Parse.Module in
        [(var_opt, def)]) run in
  success

let cfgs : (Cfg.t IntMap.t) ref = ref IntMap.empty
let nglobals : int ref = ref (-1)
let initialize (program : string) : unit =
  let run (l : (Script.var option * Script.definition) list) =
    List.iter l ~f:(fun (_, def) ->
        match def.it with
        | Script.Textual m ->
          let store = Store.init m in
          trace (Printf.sprintf "nglobals: %d\n" (List.length store.globals));
          nglobals := List.length store.globals;
          cfgs := IntMap.of_alist_exn (List.mapi store.funcs ~f:(fun faddr _ -> (faddr, Cfg_builder.build faddr store)))
        | Script.Encoded _ -> failwith "unsupported"
        | Script.Quoted _ -> failwith "unsupported"
      ) in
  trace (Printf.sprintf "Success? %b" (parse_string program run))

