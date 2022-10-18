open Core
open Helpers

module Spec_inference = struct
  include Spec

  (** Allows the use of variables encoding constants *)
  let use_const : bool ref = ref true

  let propagate_locals : bool ref = ref true

  let propagate_globals : bool ref = ref true

  (*---- Types ----*)
  (** Spec inference does not require any annotation *)
  type annot_expected = unit

  (** No summaries for this analysis *)
  type summary = unit

  let extract_summary _ _ = ()

  (*---- Helper functions ----*)
  (** Like List.drop, but raises an exception if the list does not contain enough element *)
  let drop (n : int) (vstack : Var.t list) =
    if (List.length vstack < n) then begin
      failwith "Spec_inference.drop: not enough elements in stack"
    end else
      List.drop vstack n

  let top (l : Var.t list) = match List.hd l with
    | Some v -> v
    | None -> failwith "Spec_inference.top: var list is empty"

  let take (l : Var.t list) (n : int) =
    if List.length l < n then
      failwith "Spec_inference.take: not enough element in var list"
    else
      List.take l n

  let get (n : Int32.t) (l : Var.t list) =
    match List.nth l (Int32.to_int_exn n) with
    | Some v -> v
    | _ -> failwith "Spec_inference.get: nth exception"

  let set (n : Int32.t) (l : Var.t list) (v : Var.t) = List.mapi l ~f:(fun i v' -> if i = (Int32.to_int_exn n) then v else v')

  (*---- State ----*)
  type state = Spec.t
  [@@deriving compare, equal]

  let state_to_string = Spec.to_string

  let init_state (cfg : 'a Cfg.t) : state = Spec.NotBottom {
    vstack = []; (* the vstack is initially empty *)
    locals =
      if !use_const then
        (List.mapi cfg.arg_types ~f:(fun i _ -> Var.Local i)) @ (List.map cfg.local_types ~f:(fun _ -> Var.Const (Prim_value.I32 0l)))
      else
        List.mapi (cfg.arg_types @ cfg.local_types) ~f:(fun i _ -> Var.Local i);
    globals = List.mapi cfg.global_types ~f:(fun i _ -> Var.Global i);
    memory = Var.OffsetMap.empty;
    stack_size_at_entry = Instr.Label.Map.empty;
  }

  let bottom : state = Spec.Bottom

  let bottom_state _ = bottom

  let join_state (_s1 : state) (s2 : state) : state =
    s2 (* only keep the "most recent" state, this is safe for this analysis *)

  (* No widening *)
  let widen_state _ s2 = s2

  let rec compute_stack_size_at_entry (cfg : annot_expected Cfg.t) (label : Instr.Label.t) (state : SpecWithoutBottom.t) : SpecWithoutBottom.t =
    match Cfg.find_enclosing_block cfg label with
    | None -> (* no enclosing block, the stack is initally empty at the beginning of the function, we ignore that *)
      state
    | Some block_label ->
      begin match Instr.Label.Map.find state.stack_size_at_entry block_label with
        | None ->
          (* We are at the first instruction, the current stack size is the stack size at entry *)
          let size = List.length state.vstack in
          (* It could be that this is e.g., a block contained in an if. The block and loop instructions are not reified in the CFG, and we therefore have to manually map them to their parent for stack size computation. This is done by a recursive call: if block 1 is an if, containing block 2, a block, containing an instruction with `label`, we compute the stack size at etnry of both block 1 and 2 from the current state. *)
          let state = compute_stack_size_at_entry cfg block_label state in
          { state with
            stack_size_at_entry = Instr.Label.Map.set state.stack_size_at_entry ~key:block_label ~data:size }
        | Some _ ->
          (* we already computed it *)
          state
      end
  (*---- Transfer functions ----*)

  let data_instr_transfer
      (_module_ : Wasm_module.t)
      (cfg : annot_expected Cfg.t)
      (i : annot_expected Instr.labelled_data)
    : state -> state = Spec.lift (function state ->
      let ret = Var.Var i.label in
      let state = compute_stack_size_at_entry cfg i.label state in
      match i.instr with
      | Nop -> state
      | MemorySize -> { state with vstack = ret :: state.vstack }
      | MemoryGrow -> { state with vstack = ret :: drop 1 state.vstack }
      | Drop -> { state with vstack = drop 1 state.vstack }
      | Select _ -> { state with vstack = ret :: (drop 3 state.vstack) }
      | LocalGet l -> { state with vstack = (if !propagate_locals then get l state.locals else ret) :: state.vstack }
      | LocalSet l -> { state with vstack = drop 1 state.vstack; locals = set l state.locals (if !propagate_locals then top state.vstack else ret) }
      | LocalTee l -> { state with locals = set l state.locals (if !propagate_locals then top state.vstack else ret) }
      | GlobalGet g -> { state with vstack = (if !propagate_globals then get g state.globals else ret) :: state.vstack }
      | GlobalSet g -> { state with vstack = drop 1 state.vstack; globals = set g state.globals (if !propagate_globals then top state.vstack else ret) }
      | Const n -> { state with vstack = (if !use_const then (Var.Const n) else ret) :: state.vstack }
      | Compare _ -> { state with vstack = ret :: (drop 2 state.vstack) }
      | Binary _ -> { state with vstack = ret :: (drop 2 state.vstack) }
      | Unary _ -> { state with vstack = ret :: (drop 1 state.vstack) }
      | Test _ -> { state with vstack = ret :: (drop 1 state.vstack) }
      | Convert _ -> { state with vstack = ret :: (drop 1 state.vstack) }
      | Load _ -> { state with vstack = ret :: (drop 1 state.vstack) }
      | Store { offset; _} ->
        let (value, addr) = pop2 state.vstack in
        { state with vstack = drop 2 state.vstack;
                     memory = Var.OffsetMap.update state.memory (addr, offset) ~f:(fun _ -> value) }
      | RefIsNull -> { state with vstack = ret :: (drop 1 state.vstack) }
      | RefFunc _ -> { state with vstack = ret :: state.vstack }
      | RefNull _ -> { state with vstack = ret :: state.vstack })

  let control_instr_transfer (_module_ : Wasm_module.t) _summaries (cfg : 'a Cfg.t) (i : ('a Instr.control, 'a) Instr.labelled) : state -> [`Simple of state | `Branch of state * state ] = Spec.wrap ~default:(`Simple bottom) (function state ->
      let ret = Var.Var i.label in
      let state = compute_stack_size_at_entry cfg i.label state in
      let get_block_return_stack_size n =
        let stack_size_at_entry = match Cfg.find_enclosing_block cfg i.label with
          | None -> 0 (* the stack is initially empty *)
          | Some block_label -> begin match Instr.Label.Map.find state.stack_size_at_entry block_label with
              | Some size -> size
              | None -> failwith "Spec inference: no stack size computed at entry of block"
            end in
        let out_arity = match Cfg.find_nth_parent_block cfg i.label n with
          | Some block_label ->
            let arity = Cfg.block_arity cfg block_label in
            if Cfg.is_loop_exn cfg block_label then
              (* If we break out of a loop, we're actually looping back, so we need to look at the param and not the result here *)
              fst arity
            else
              snd arity
          | None -> List.length cfg.return_types in
        stack_size_at_entry + out_arity in
      match i.instr with
      | Call ((arity_in, arity_out), _, _) ->
        `Simple (NotBottom { state with vstack = (if arity_out = 1 then [ret] else []) @ (drop arity_in state.vstack) })
      | CallIndirect (_, (arity_in, arity_out), _, _) ->
        (* Like call, but reads the function index from the vstack *)
        `Simple (NotBottom { state with vstack = (if arity_out = 1 then [ret] else []) @ (drop (arity_in+1) state.vstack) })
      | Br n ->
        `Simple (NotBottom ({ state with vstack = take state.vstack (get_block_return_stack_size n) }))
      | BrIf n ->
        let state' = NotBottom ({ state with vstack = take (drop 1 state.vstack) (get_block_return_stack_size n) }) in
        `Branch (state', state')
      | If _ ->
        let state' = NotBottom ({ state with vstack = drop 1 state.vstack }) in
        `Branch (state', state')
      | BrTable (ns, n) ->
        let arities = List.map (n :: ns) ~f:get_block_return_stack_size in
         (* LIMITATION: we assume all block arities are equal. Not sure this is always the case, but this is a limitation we have *)
        assert (IntSet.length (IntSet.of_list arities) = 1);
        let arity = List.hd_exn arities in
        `Simple (NotBottom { state with vstack = take (drop 1 state.vstack) arity })
      | Return -> `Simple (NotBottom ({ state with vstack = take state.vstack (List.length cfg.return_types) }))
      | Unreachable ->
        (* failwith (Printf.sprintf "unsupported: unreachable") (*  in function %ld cfg.idx *) *)
        `Simple bottom
      | Merge -> `Simple (NotBottom state)
      | _ -> failwith (Printf.sprintf "Unsupported control instruction: %s" (Instr.control_to_short_string i.instr)))

  let merge
      (_module_ : Wasm_module.t)
      (cfg : annot_expected Cfg.t)
      (block : annot_expected Basic_block.t)
      (states : state list) : state =
    let counter = ref 0 in
    let new_var () : Var.t =
      let res = Var.Merge (block.idx, !counter) in
      counter := !counter + 1;
      res in
    let _ : Var.t = new_var() in
    let rename_exit (s : SpecWithoutBottom.t) =
      (* If this is the exit block, rename the top of the stack to a new variable *)
      if cfg.exit_block = block.idx then
        { s with vstack = List.mapi s.vstack ~f:(fun i v -> if i = 0 then Var.Return else v) }
      else
        s
    in
    match block.content with
    | Control { instr = Merge; _ } ->
      (* Multiple cases: either we have no predecessor, we have unanalyzed predecessors, or we have only analyzed predecessors *)
      (Spec.lift rename_exit) (begin match states with
        | [] ->
          (* entry node *)
          init_state cfg
        | s :: [] ->
          (* single predecessor node?! *)
          s
        | _ ->
          (* multiple predecessors *)
          let bot = bottom_state cfg in
          begin match List.filter states ~f:(fun s -> not (equal_state s bot)) with
            | [] ->
              (* No predecessors have been analyzed. Return bottom. In practice, this should only happen if one of the predecessors is the empty entry block. TODO: it should be cleaned so that this does not arise and we can throw an exception here *)
              init_state cfg
            | s :: [] -> (* only one non-bottom predecessor *) s
            | state :: states ->
              (* multiple predecessors to merge *)
              (* First compute the state with holes where we will need to put merge variables *)
              let with_holes = List.fold_left states
                  ~init:state
                  ~f:(fun acc s ->
                      match acc, s with
                      | Bottom, Bottom -> Bottom
                      | NotBottom s, Bottom | Bottom, NotBottom s -> NotBottom s
                      | NotBottom acc, NotBottom s ->
                        let f opt v = match opt with
                          | v' when Var.equal v v' -> v'
                          | _ -> Var.Hole (* this is a hole *)
                        in
                        if (List.length acc.vstack <> List.length s.vstack) then
                          failwith "unsupported in spec_inference: incompatible stack lengths (probably due to mismatches in br_table branches)";
                        assert (List.length acc.locals = List.length s.locals);
                        assert (List.length acc.globals = List.length s.globals);
                        NotBottom ({ vstack = List.map2_exn acc.vstack s.vstack ~f:f;
                                     locals = List.map2_exn acc.locals s.locals ~f:f;
                                     globals = List.map2_exn acc.globals s.globals ~f:f;
                                     memory = Var.OffsetMap.merge acc.memory s.memory ~f:(fun ~key:_ v -> match v with
                                         | `Both (v1, v2) -> Some (f v1 v2)
                                         | `Left _v | `Right _v ->
                                           (* If a binding is only present in one branch, then it is lost upon join.
                                              This is necessary to preserve soundness.
                                              For example, if one branch has memory [m: v], the other has memory [],
                                              then after the join point of these branches, only [] is a valid memory
                                              (otherwise we could derive information assuming that m is bound to v,
                                              which is not always the case)*)
                                           None);
                                     stack_size_at_entry = Instr.Label.Map.merge acc.stack_size_at_entry s.stack_size_at_entry ~f:(fun ~key:_ presence -> match presence with
                                         | `Both (a, b) ->
                                           if a <> b then failwith "Cannot merge due to different stack sizes" else Some a
                                         | `Left a | `Right a -> Some a)
                                   })) in
              (* Then, add merge variables *)
              let plug_holes = (function
                  | Var.Hole -> (* add a merge variable *) new_var ()
                  | v -> (* no hole, keep the variable *) v) in
              lift (fun s -> { vstack = List.map s.vstack ~f:plug_holes;
                               locals = List.map s.locals ~f:plug_holes;
                               globals = List.map s.globals ~f:plug_holes;
                               memory = Var.OffsetMap.map s.memory ~f:plug_holes;
                               stack_size_at_entry = s.stack_size_at_entry;
                             }) with_holes
          end
      end)
    | _ ->
      (* not a control-flow merge, should only be one predecessor (or none if it is the entry point) *)
      begin match states with
        | [] -> init_state cfg
        | s :: [] -> s
        | _ ->  failwith (Printf.sprintf "Invalid block with multiple input states: %d" block.idx)
      end

  let merge_flows
      (module_ : Wasm_module.t)
      (cfg : annot_expected Cfg.t)
      (block : annot_expected Basic_block.t)
      (states : (int * state) list)
    : state =
    (* Checks the validity of the merge and dispatches to `merge` *)
    begin match states with
      | _ :: _ :: _ -> begin match block.content with
          | Control { instr = Merge; _ } -> ()
          | _ -> failwith (Printf.sprintf "Invalid block with multiple input states: %d" block.idx)
        end
      | _ -> ()
    end;
    merge module_ cfg block (List.map ~f:snd states)

end

module Intra = Intra.Make(Spec_inference)
include Spec_inference

(** Extract vars that have been redefined in a merge block *)
let new_merge_variables (cfg : t Cfg.t) (merge_block : t Basic_block.t) : (Var.t * Var.t) list =
  (* The predecessors of merge_block *)
  let preds = Cfg.predecessors cfg merge_block.idx in
  let state_after = Cfg.state_after_block cfg merge_block.idx (init_state cfg) in
  List.fold_left preds ~init:[] ~f:(fun acc pred_idx ->
      let state_before = Cfg.state_after_block cfg pred_idx (init_state cfg) in
      if Spec_inference.equal state_before Spec_inference.bottom then
        (* Ignore bottom state *)
        acc
      else
        (extract_different_vars state_before state_after) @ acc)

(** Return the list of variables defined by an instruction *)
let instr_def (cfg : t Cfg.t) (instr : t Instr.t) : Var.t list =
  let defs = match instr with
    | Instr.Data i ->
      let state_after = match i.annotation_after with
        | Bottom -> failwith "bottom annotation, this an unreachable instruction"
        | NotBottom s -> s in
      let top_n n = take state_after.vstack n in
      begin match i.instr with
        | Nop | Drop -> []
        | Select _ | MemorySize
        | Unary _ | Binary _ | Compare _ | Test _ | Convert _
        | Const _ | MemoryGrow
        | RefIsNull | RefNull _ | RefFunc _ -> top_n 1
        | LocalGet _ ->
          if !propagate_locals then
            []
          else
            top_n 1
        | GlobalGet _ ->
          if !propagate_globals then
            []
          else
            top_n 1
        | LocalSet l | LocalTee l ->
          if !propagate_locals then
            []
          else
            [get_nth state_after.locals l]
        | GlobalSet g ->
          if !propagate_globals then
            []
          else
            [get_nth state_after.globals g]
        | Load _ -> top_n 1
        | Store _ ->
          []
      end
    | Instr.Control i ->
      let top_n n = match i.annotation_after with
        | Bottom -> failwith "bottom annotation"
        | NotBottom s -> take s.vstack n in
        begin match i.instr with
        | Block _ | Loop _ -> [] (* we handle instruction individually rather than through their block *)
        | If _ -> [] (* We could say that if defines its "resulting" value, but that will be handled by the merge node *)
        | Call ((_, arity_out), _, _) -> top_n arity_out
        | CallIndirect (_, (_, arity_out), _, _) -> top_n arity_out
        | Merge ->
          (* Merge instruction defines new variabes *)
          let block = Cfg.find_enclosing_block_exn cfg (Instr.label instr) in
          let vars = List.map (new_merge_variables cfg block) ~f:snd in
          (* There might be duplicates. For example, i3 becomes m0 from one branch, and i4 becomes m0 from another branch.
             If that is the case, we have two definitions of m0.
             Hence we eliminate duplicates *)
          Var.Set.to_list (Var.Set.of_list vars)
        | Br _ | BrIf _ | BrTable _ | Return | Unreachable -> []
      end
  in
  List.filter defs ~f:(function
      | Var.Const _ -> false (* constants are not variables that can be defined (otherwise definitions are not unique anymore) *)
      | Var.Local _ -> false (* locals don't have a definition point (they are part of the entry state) *)
      | Var.Global _ -> false (* same for globals *)
      | _ -> true)

(** Return the list of variables used by an instruction.
    If var is provided, only the use related the computation of this specific var will be return (This is only relevant for mere blocks)
 *)
let instr_use (cfg : t Cfg.t) ?var:(var : Var.t option) (instr : t Instr.t) : Var.t list = match instr with
  | Instr.Data i ->
    let top_n n = match i.annotation_before with
      | Bottom -> failwith "bottom annotation"
      | NotBottom s -> take s.vstack n in
    begin match i.instr with
      | Nop -> []
      | Drop -> top_n 1
      | Select _ -> top_n 3
      | MemorySize -> []
      | MemoryGrow -> top_n 1
      | Const _ -> []
      | Unary _ | Test _ | Convert _ -> top_n 1
      | Binary _ | Compare _ -> top_n 2
      | LocalGet l ->
        let locals = match i.annotation_before with
        | Bottom -> failwith "bottom annotation"
        | NotBottom s -> s.locals in
        [get_nth locals l] (* use local l *)
      | LocalSet _ | LocalTee _ -> top_n 1 (* use top of the stack to define local *)
      | GlobalGet g ->
        let globals = match i.annotation_before with
        | Bottom -> failwith "bottom annotation"
        | NotBottom s -> s.globals in
        [get_nth globals g] (* use global g *)
      | GlobalSet _ -> top_n 1
      | Load _ -> top_n 1 (* use memory address from the top of the stack *)
      | Store _ -> top_n 2 (* use address and valu from the top of the stack *)
      | RefIsNull  -> top_n 1
      | RefNull _ | RefFunc _ -> []
    end
  | Instr.Control i ->
    let top_n n = match i.annotation_before with
        | Bottom -> failwith "bottom annotation"
        | NotBottom s -> take s.vstack n in
    begin match i.instr with
      | Block _ | Loop _ -> [] (* we handle instruction individually rather than through their block *)
      | If _ -> top_n 1 (* relies on top value to decide the branch taken *)
      | Call ((arity_in, _), _, _) -> top_n arity_in (* uses the n arguments from the stack *)
      | CallIndirect (_, (arity_in, _), _, _) -> top_n (arity_in + 1) (* + 1 because we need to pop the index that will refer to the called function, on top of the arguments *)
      | BrIf _ | BrTable _ -> top_n 1
      | Merge ->
        (* Merge instruction uses the variables it redefines *)
        let block = Cfg.find_enclosing_block_exn cfg (Instr.label instr) in
        begin match var with
          | None ->
            List.map (new_merge_variables cfg block) ~f:fst
          | Some v ->
            List.filter_map (new_merge_variables cfg block) ~f:(fun (old, new_) ->
                if Var.equal new_ v then
                  Some old
                else
                  None)
        end
      | Return ->
        top_n (List.length cfg.return_types)
      | Br _ | Unreachable -> []
    end

module Spec_inference_opaque : Transfer.TRANSFER = Spec_inference
