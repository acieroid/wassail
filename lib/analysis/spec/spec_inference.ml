open Core_kernel
open Helpers

module Spec_inference (* : Transfer.TRANSFER TODO *) = struct
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
  let summary _ _ = ()
  let init_summaries _ = ()

  (*---- Helper functions ----*)
  (** Like List.drop, but raises an exception if the list does not contain enough element *)
  let drop (n : int) (vstack : Var.t list) =
    if (List.length vstack < n) then begin
      failwith "Spec_inference.drop: not enough elements in stack"
    end else
      List.drop vstack n

  let top (l : Var.t list) = List.hd_exn l

  let get (n : Int32.t) (l : Var.t list) = List.nth_exn l (Int32.to_int_exn n)

  let set (n : Int32.t) (l : Var.t list) (v : Var.t) = List.mapi l ~f:(fun i v' -> if i = (Int32.to_int_exn n) then v else v')

  (*---- State ----*)
  type state = Spec.t
  [@@deriving compare, equal]

  let state_to_string = Spec.to_string

  let init_state (cfg : 'a Cfg.t) : state = {
    vstack = []; (* the vstack is initially empty *)
    locals =
      if !use_const then
        (List.mapi cfg.arg_types ~f:(fun i _ -> Var.Local i)) @ (List.map cfg.local_types ~f:(fun _ -> Var.Const (Prim_value.I32 0l)))
      else
        List.mapi (cfg.arg_types @ cfg.local_types) ~f:(fun i _ -> Var.Local i);
    globals = List.mapi cfg.global_types ~f:(fun i _ -> Var.Global i);
    memory = Var.OffsetMap.empty;
  }

  let bottom : state = {
    vstack = [];
    locals = [];
    globals = [];
    memory = Var.OffsetMap.empty;
  }

  let bottom_state _ = bottom

  let join_state (_s1 : state) (s2 : state) : state =
    s2 (* only keep the "most recent" state, this is safe for this analysis *)

  (* No widening *)
  let widen_state _ s2 = s2

  (*---- Transfer functions ----*)

  let data_instr_transfer
      (_module_ : Wasm_module.t)
      (_cfg : annot_expected Cfg.t)
      (i : annot_expected Instr.labelled_data)
      (state : state)
    : state =
    let ret = Var.Var i.label in
    match i.instr with
    | Nop -> state
    | MemorySize -> { state with vstack = ret :: state.vstack }
    | MemoryGrow -> { state with vstack = ret :: drop 1 state.vstack }
    | Drop -> { state with vstack = drop 1 state.vstack }
    | Select -> { state with vstack = ret :: (drop 3 state.vstack) }
    | LocalGet l -> { state with vstack = (if !propagate_locals then get l state.locals else ret) :: state.vstack }
    | LocalSet l -> { state with vstack = drop 1 state.vstack; locals = set l state.locals (if !propagate_locals then (List.hd_exn state.vstack) else ret) }
    | LocalTee l -> { state with locals = set l state.locals (if !propagate_locals then (List.hd_exn state.vstack) else ret) }
    | GlobalGet g -> { state with vstack = (if !propagate_globals then get g state.globals else ret) :: state.vstack }
    | GlobalSet g -> { state with globals = set g state.globals (if !propagate_globals then (List.hd_exn state.vstack) else ret) }
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

  let control_instr_transfer (_module_ : Wasm_module.t) (cfg : 'a Cfg.t) (i : ('a Instr.control, 'a) Instr.labelled) (state : state) : [`Simple of state | `Branch of state * state] =
    let ret = Var.Var i.label in
    match i.instr with
    | Call ((arity_in, arity_out), _) ->
      `Simple { state with vstack = (if arity_out = 1 then [ret] else []) @ (drop arity_in state.vstack) }
    | CallIndirect ((arity_in, arity_out), _) ->
      (* Like call, but reads the function index from the vstack *)
      `Simple { state with vstack = (if arity_out = 1 then [ret] else []) @ (drop (arity_in+1) state.vstack) }
    | Br _ -> `Simple state
    | BrIf _ | If _ ->
      `Branch ({ state with vstack = drop 1 state.vstack },
               { state with vstack = drop 1 state.vstack })
    | BrTable _ -> `Simple { state with vstack = drop 1 state.vstack }
    | Return -> `Simple (if List.length cfg.return_types = 1 then
                           { state with vstack = [List.hd_exn state.vstack] }
                         else
                           { state with vstack = [] })
    | Unreachable -> `Simple { state with vstack = [] }
    | Merge -> `Simple state
    | _ -> failwith (Printf.sprintf "Unsupported control instruction: %s" (Instr.control_to_short_string i.instr))

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
    let rename_exit (s : state) =
      (* If this is the exit block, rename the top of the stack to a new variable *)
      if cfg.exit_block = block.idx then
        { s with vstack = List.mapi s.vstack ~f:(fun i v -> if i = 0 then Var.Return else v) }
      else
        s
    in
    match block.content with
    | Control { instr = Merge; _ } ->
      (* Multiple cases: either we have no predecessor, we have unanalyzed predecessors, or we have only analyzed predecessors *)
      rename_exit (begin match states with
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
            | [] -> failwith "No predecessor of a merge node have been analyzed, should not happen"
            | s :: [] -> (* only one non-bottom predecessor *) s
            | states ->
              (* multiple predecessors to merge *)
              (* First compute the state with holes where we will need to put merge variables *)
              let with_holes = List.fold_left states
                  ~init:(List.hd_exn states)
                  ~f:(fun acc s ->
                      let f opt v = match opt with
                        | v' when Var.equal v v' -> v'
                        | _ -> Var.Hole (* this is a hole *)
                      in
                      assert (List.length acc.vstack = List.length s.vstack);
                      assert (List.length acc.locals = List.length s.locals);
                      assert (List.length acc.globals = List.length s.globals);
                      { vstack = List.map2_exn acc.vstack s.vstack ~f:f;
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
                              None); }) in
              (* Then, add merge variables *)
              let plug_holes = (function
                  | Var.Hole -> (* add a merge variable *) new_var ()
                  | v -> (* no hole, keep the variable *) v) in
              { vstack = List.map with_holes.vstack ~f:plug_holes;
                locals = List.map with_holes.locals ~f:plug_holes;
                globals = List.map with_holes.globals ~f:plug_holes;
                memory = Var.OffsetMap.map with_holes.memory ~f:plug_holes }
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

(** Return the list of variables defined by an instruction *)
let instr_def (cfg : Spec.t Cfg.t) (instr : Spec.t Instr.t) : Var.t list =
  let defs = match instr with
    | Instr.Data i ->
      let top_n n = List.take i.annotation_after.vstack n in
      begin match i.instr with
        | Nop | Drop | MemoryGrow -> []
        | Select | MemorySize
        | Unary _ | Binary _ | Compare _ | Test _ | Convert _
        | Const _ -> top_n 1
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
            [get_nth i.annotation_after.locals l]
        | GlobalSet g ->
          if !propagate_globals then
            []
          else
            [get_nth i.annotation_after.globals g]
        | Load _ -> top_n 1
        | Store _ ->
          []
          (*let addr = List.nth_exn i.annotation_before.vstack 1 (* address is not the top of the stack but the element after *) in
            [match Var.OffsetMap.find i.annotation_after.memory (addr, offset) with
             | Some v -> v
             | None -> failwith (Printf.sprintf "Wrong memory annotation while looking for %s+%d in memory (instr: %s), annot after: %s" (Var.to_string addr) offset (Instr.to_string instr Spec_inference.state_to_string) (Spec_inference.state_to_string i.annotation_after))] *)
      end
    | Instr.Control i ->
      let top_n n = List.take i.annotation_after.vstack n in
      begin match i.instr with
        | Block _ | Loop _ -> [] (* we handle instruction individually rather than through their block *)
        | If _ -> [] (* We could say that if defines its "resulting" value, but that will be handled by the merge node *)
        | Call ((_, arity_out), _) -> top_n arity_out
        | CallIndirect ((_, arity_out), _) -> top_n arity_out
        | Merge ->
          (* Merge instruction defines new variabes *)
          let block = Cfg.find_enclosing_block cfg instr in
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

(** Return the list of variables used by an instruction *)
let instr_use (cfg : Spec.t Cfg.t) (instr : Spec.t Instr.t) : Var.t list = match instr with
  | Instr.Data i ->
    let top_n n = List.take i.annotation_before.vstack n in
    begin match i.instr with
      | Nop -> []
      | Drop -> top_n 1
      | Select -> top_n 3
      | MemorySize -> []
      | MemoryGrow -> top_n 1
      | Const _ -> []
      | Unary _ | Test _ | Convert _ -> top_n 1
      | Binary _ | Compare _ -> top_n 2
      | LocalGet l -> [get_nth i.annotation_before.locals l] (* use local l *)
      | LocalSet _ | LocalTee _ -> top_n 1 (* use top of the stack to define local *)
      | GlobalGet g -> [get_nth i.annotation_before.globals g] (* use global g *)
      | GlobalSet _ -> top_n 1
      | Load _ -> top_n 1 (* use memory address from the top of the stack *)
      | Store _ -> top_n 2 (* use address and valu from the top of the stack *)
    end
  | Instr.Control i ->
    let top_n n = List.take i.annotation_before.vstack n in
    begin match i.instr with
      | Block _ | Loop _ -> [] (* we handle instruction individually rather than through their block *)
      | If _ -> top_n 1 (* relies on top value to decide the branch taken *)
      | Call ((arity_in, _), _) -> top_n arity_in (* uses the n arguments from the stack *)
      | CallIndirect ((arity_in, __), _) -> top_n (arity_in + 1) (* + 1 because we need to pop the index that will refer to the called function, on top of the arguments *)
      | BrIf _ | BrTable _ -> top_n 1
      | Merge ->
        (* Merge instruction uses the variables it redefines *)
        let block = Cfg.find_enclosing_block cfg instr in
        List.map (new_merge_variables cfg block) ~f:fst
      | Br _ | Return | Unreachable -> []
    end
