open Core
open Helpers


(* module Time = Time_float (* Time is deprecated in Core, we should use Time_float instead *) *)

module InSlice = struct
  module T = struct
    (** Intermediary data structure used as part of the slicing to track which
        instruction should be part of the slice, and for what reason *)
    type t = {
      label: Instr.Label.t; (** The label of the instruction that should be added to the slice *)
      reason: Var.t option; (** The corresponding var that make this instruction part of the slice, if there is one *)
    }
    [@@deriving sexp, compare, equal]

    let to_string (t : t) : string = match t.reason with
      | None -> Instr.Label.to_string t.label
      | Some var -> Printf.sprintf "%s(%s)" (Instr.Label.to_string t.label) (Var.to_string var)

    let make (label : Instr.Label.t) (var : Var.t option) (instructions : 'a Instr.t Instr.Label.Map.t) =
      { label ;
        reason = match Cfg.find_instr instructions label with
          | Some (Instr.Control { instr = Merge; _ }) -> var
          | Some _ -> None
          | None -> failwith "Did not find the instruction! Should not happen"
      }
  end
  module Set = struct
    include Set
    include Set.Make(T)
  end
  include T
end

type preanalysis_results = {
  control_dependencies : Instr.Label.Set.t Instr.Label.Map.t;
  control_time : Time_float.Span.t;
  data_dependencies : Use_def.UseDefChains.t;
  data_time : Time_float.Span.t;
  mem_dependencies : Memory_deps.t;
  mem_time : Time_float.Span.t;
  global_dependencies : Instr.Label.Set.t Instr.Label.Map.t; (* global.get instructions depend on fct calls, and calls depend on global.set instructions *)
  global_time : Time_float.Span.t;
}

(** Performs the pre-analysis phase in order to slice a function, according to any slicing criterion *)
let preanalysis 
    (module_ : Wasm_module.t) 
    (cfg : Spec_domain.t Cfg.t) 
    (cfg_instructions : Spec_domain.t Instr.t Instr.Label.Map.t) 
    (pointer_analysis : Value_set.pointer_analysis option)
  : preanalysis_results =
  let t0 = Time_float.now () in
  let control_dependencies = Control_deps.control_deps_exact_instrs cfg in
  let t1 = Time_float.now () in
  let (_, _, data_dependencies) = Use_def.make module_ cfg in
  let t2 = Time_float.now () in
  let global_deps = Global_read.function_global_deps module_ in
  let mem_dependencies = Memory_deps.make module_ global_deps pointer_analysis cfg in
  (
  let t3 = Time_float.now () in
  let global_dependencies = Global_deps.global_dependencies ~module_ ~global_deps ~cfg ~cfg_instructions ~pointer_analysis in
  let t4 = Time_float.now () in
  let control_time = Time_float.diff t1 t0 in
  let data_time = Time_float.diff t2 t1 in
  let mem_time = Time_float.diff t3 t2 in
  let global_time = Time_float.diff t4 t3 in
  { control_dependencies; control_time;
    data_dependencies; data_time;
    mem_dependencies; mem_time;
    (* global_set_instructions;  *)
    global_time;
    global_dependencies}
  )


(** Identify instructions to keep in a backwards slice on `cfg`, using the
    slicing criterion `criterion`, encoded as an instruction index. Returns the
    set of instructions that are part of the slice, as a set of instruction
    labels. *)
let instructions_to_keep 
    (module_ : Wasm_module.t) 
    (cfg : Spec_domain.t Cfg.t) 
    (cfg_instructions : Spec_domain.t Instr.t Instr.Label.Map.t) 
    (preanalysis : preanalysis_results) 
    (criteria : Instr.Label.Set.t) 
  : (Instr.Label.Set.t * (Time_float.Span.t * Time_float.Span.t * Time_float.Span.t * Time_float.Span.t * Time_float.Span.t)) =
  Log.info (fun () -> Printf.sprintf "Slicing with criteria %s" (Instr.Label.Set.to_string criteria));
  let t0 = Time_float.now () in
  let rec loop (worklist : InSlice.Set.t) (slice : Instr.Label.Set.t) (visited : InSlice.Set.t) : Instr.Label.Set.t =
    (* Perform backward slicing as follows:
       Given an instruction as the slicing criterion (we can derive variable uses from instructions),
       perform the following fixpoint algorithm, starting with W = instr
         let instr = pop(W)
         add instr to the current slice
         for use in instr_uses(instr):
           for def in usedef(use):
             if def contains an istruction, add def.instr to W
           for _, instr' in cdeps(use.var):
             add instr to W
         for instr' in mem_deps(instr):
           add instr to W *)
    match InSlice.Set.choose worklist with
    | None -> (* worklist is empty *)
      slice
    | Some slicepart when InSlice.Set.mem visited slicepart ->
      (* Already seen this slice part, no need to process it again *)
      loop (InSlice.Set.remove worklist slicepart) slice visited
    | Some slicepart ->
      Log.info (fun () -> Printf.sprintf "Looking at instruction %s" (InSlice.to_string slicepart));
      (* Add instr to the current slice *)
      let slice' = Instr.Label.Set.add slice slicepart.label in
      let visited' = InSlice.Set.add visited slicepart in
      let uses =
        match Cfg.find_instr cfg_instructions slicepart.label with
        | None -> failwith "Unsupported in slicing: cannot find an instruction. It probably is part of unreachable code."
        | Some instr -> Spec_inference.instr_use module_ cfg ?var:slicepart.reason instr in
      (* For use in instr_uses(instr) *)
      let worklist' = List.fold_left uses ~init:worklist
          ~f:(fun w use ->
              (* Get the definition corresponding to the current use *)
              let def = Use_def.UseDefChains.get preanalysis.data_dependencies (Use_def.Use.make slicepart.label use) in
              (* For def in usedef(use): if def contains an instruction, add def.instr to W *)
              let data_deps : InSlice.Set.t = match def with
                | Use_def.Def.Instruction (instr', var) ->
                  Log.info
                    (fun () -> Printf.sprintf "Instruction %s (%s) is part of the slice due to its data dependence on %s"
                       (Instr.Label.to_string instr') (Instr.to_string (Instr.Label.Map.find_exn cfg_instructions instr')) (Var.to_string var));
                  InSlice.Set.singleton (InSlice.make instr' (Some var) cfg_instructions)
                | Use_def.Def.Entry _ -> InSlice.Set.empty
                | Use_def.Def.Constant _ -> InSlice.Set.empty in
              InSlice.Set.union w data_deps) in
      (* Add all control dependencies of instr to W *)
      let control_deps : InSlice.Set.t = match Instr.Label.Map.find preanalysis.control_dependencies slicepart.label with
        | None -> InSlice.Set.empty
        | Some deps -> InSlice.Set.of_list (List.map (Instr.Label.Set.to_list deps)
                                              ~f:(fun label ->
                                                  Log.info
                                                    (fun () -> Printf.sprintf "Instruction %s (%s) is part of the slice due to control dependences"
                                                       (Instr.Label.to_string label) (Instr.to_string (Instr.Label.Map.find_exn cfg_instructions label)));
                                                  InSlice.make label None cfg_instructions)) in
      let worklist'' = InSlice.Set.union worklist' control_deps in
      (* For instr' in mem_deps(instr): add instr to W *)
      let memory_deps = (InSlice.Set.of_list
             (List.map ~f:(fun label ->
                  Log.info
                    (fun () -> Printf.sprintf "Instruction %s (%s) is part of the slice due to memory dependences"
                       (Instr.Label.to_string label) (Instr.to_string (Instr.Label.Map.find_exn cfg_instructions label)));
                  InSlice.make label None cfg_instructions)
                (Instr.Label.Set.to_list (Memory_deps.deps_for preanalysis.mem_dependencies slicepart.label)))) in
      let worklist''' = InSlice.Set.union worklist'' memory_deps in
      (* Add all global side-effect dependencies of instr to W.
         For example, a global.get may depend on a call instruction if the
         called function may execute a global.set on the same global. *)
      let global_deps : InSlice.Set.t = match Instr.Label.Map.find preanalysis.global_dependencies slicepart.label with
        | None -> InSlice.Set.empty
        | Some deps -> InSlice.Set.of_list (List.map (Instr.Label.Set.to_list deps)
                                              ~f:(fun label ->
                                                  Log.info (fun () -> Printf.sprintf "Instruction %s (%s) is part of the slice due to global dependences"
                                                    (Instr.Label.to_string label) (Instr.to_string (Instr.Label.Map.find_exn cfg_instructions label)));
                                                  InSlice.make label None cfg_instructions)) in
      let worklist'''' = InSlice.Set.union worklist''' global_deps in
      loop (InSlice.Set.remove worklist'''' slicepart) slice' visited' in
  let agrawal (slice : Instr.Label.Set.t) : Instr.Label.Set.t =
    (* For each br instruction of the function, we add them to the slice if they are control-dependent on an instruction in the slice *)
    (* For each instruction in the slice, we add all br instructions that are control-dependent on it *)
    Instr.Label.Set.fold (Instr.Label.Set.of_list (Instr.Label.Map.keys cfg_instructions))
      ~init:slice
      ~f:(fun slice label ->
          let instr = Instr.Label.Map.find_exn cfg_instructions label in
          match instr with
          | Control { instr = Br _; _ } -> begin match Instr.Label.Map.find preanalysis.control_dependencies label with
              | Some instrs -> begin match Instr.Label.Set.find_map instrs
                                             ~f:(fun i -> if Instr.Label.Set.mem slice i then Some i else None) with
                | Some label' ->
                    Log.info (fun () -> Printf.sprintf "Agrawal tells us to add %s to the slice because there is a dependency to %s\n" (Instr.Label.to_string label) (Instr.Label.to_string label'));
                    Instr.Label.Set.add slice label
                  | None -> slice
                end
              | None -> slice
            end
          | _ -> slice) in
  let initial_worklist = (InSlice.Set.of_list (List.map (Instr.Label.Set.to_list criteria) ~f:(fun criterion -> InSlice.{ label = criterion; reason = None }))) in
  let initial_slice = Instr.Label.Set.empty in
  let slice = Instr.Label.Set.filter (agrawal (loop initial_worklist initial_slice InSlice.Set.empty))
    ~f:(fun lab -> match lab.section with
        | MergeInFunction _ ->
          (* Merge instructions do not need to be marked as part of the slice once slicing has been performed *)
          false
        | _ -> true) in
  let t1 = Time_float.now () in
  (slice, (preanalysis.control_time, preanalysis.data_time, preanalysis.mem_time, preanalysis.global_time, Time_float.diff t1 t0))

type instr_type_element =
  | T of Type.t
  | Any of string
[@@deriving equal]

let instr_type_element_to_string t = match t with
  | T t -> Type.to_string t
  | Any _ -> "any"

let type_of_data
    (i : (Instr.data, 'a) Instr.labelled)
    (cfg : 'a Cfg.t)
    (instructions_map : Spec_domain.t Instr.t Instr.Label.Map.t)
  : instr_type_element list * instr_type_element list =
  match Cfg.find_instr instructions_map i.label with
  | None ->
    ([], [])
  | Some _ ->
    match i.instr with
    | Nop -> ([], [])
    | Drop -> ([Any "any"], [])
    | Select _ -> ([Any "a"; Any "a"; T Type.I32], [Any "a"])
    | MemorySize -> ([], [T Type.I32])
    | MemoryGrow -> ([T Type.I32], [T Type.I32])
    | MemoryCopy | MemoryInit _ -> ([T Type.I32; T Type.I32; T Type.I32], [])
    | MemoryFill -> ([T Type.I32; Any "a"; T Type.I32], [])
    | Const (I32 _) -> ([], [T Type.I32])
    | Const (I64 _) -> ([], [T Type.I64])
    | Const (F32 _) -> ([], [T Type.F32])
    | Const (F64 _) -> ([], [T Type.F64])
    | Unary op -> ([T op.typ], [T op.typ])
    | Binary op -> ([T op.typ; T op.typ], [T op.typ])
    | Compare op -> ([T op.typ; T op.typ], [T Type.I32])
    | Test I32Eqz -> ([T Type.I32], [T Type.I32])
    | Test I64Eqz -> ([T Type.I64], [T Type.I32])
    | Convert op -> ([Any "a"], [T op.typ]) (* XXX: it should really be a specific type rather than any *)
    | LocalGet l -> ([], [T (Cfg.local_type cfg l)])
    | LocalSet l -> ([T (Cfg.local_type cfg l)], [])
    | LocalTee l ->
      let t = Cfg.local_type cfg l in
      ([T t], [T t])
    | GlobalGet g -> ([], [T (List32.nth_exn cfg.global_types g)])
    | GlobalSet g -> ([T (List32.nth_exn cfg.global_types g)], [])
    | Load op -> ([T Type.I32], [T op.typ])
    | Store op -> ([T Type.I32; T op.typ], [])
    | RefIsNull -> ([Any "any"], [T Type.I32])
    | RefNull _ -> ([], [Any "any"])
    | RefFunc _ -> ([], [Any "any"])

let type_of_call
    (i : (Instr.call, 'a) Instr.labelled)
    (_cfg : unit Cfg.t)
    (instructions_map : Spec_domain.t Instr.t Instr.Label.Map.t)
  : instr_type_element list * instr_type_element list =
  let vstack_before = match Cfg.find_instr instructions_map i.label with
    | None -> None
    | Some reachable_instruction ->
      match Instr.annotation_before reachable_instruction with
      | Bottom -> None (* unreachable because it hasn't been spec-analyzed! *)
      | NotBottom s -> Some s.vstack in
  match i.instr, vstack_before with
  | (_, None) ->
    Log.warn (fun () -> Printf.sprintf "instruction is unreachable: %s" (Instr.Label.to_string i.label));
    (* instruction is unreachable, treating it as having no effect *)
    ([], [])
  | (CallDirect (_, (in_type, out_type), _), _) -> (List.map in_type ~f:(fun t -> T t), List.map out_type ~f:(fun t -> T t))
  | (CallIndirect (_, _, (in_type, out_type), _), _) ->
      ((List.map in_type ~f:(fun t -> T t)) @ [T Type.I32], (List.map out_type ~f:(fun t -> T t)))

let type_of_control
    (i : ('a Instr.control, 'a) Instr.labelled)
    (_cfg : unit Cfg.t)
    (instructions_map : Spec_domain.t Instr.t Instr.Label.Map.t)
  : instr_type_element list * instr_type_element list =
  let vstack_before = match Cfg.find_instr instructions_map i.label with
    | None -> None
    | Some reachable_instruction ->
      match Instr.annotation_before reachable_instruction with
      | Bottom -> None (* unreachable because it hasn't been spec-analyzed! *)
      | NotBottom s -> Some s.vstack in
  match i.instr, vstack_before with
  | (Block (bt, _, _), _)
  | (Loop (bt, _, _), _) ->
    (* Blocks and loops are not reified in the CFG, so we don't want to check their reachability *)
    (List.map ~f:(fun t -> T t) (fst bt),
     List.map ~f:(fun t -> T t) (snd bt))
  | (_, None) ->
    Log.warn (fun () -> Printf.sprintf "instruction is unreachable: %s" (Instr.Label.to_string i.label));
    (* instruction is unreachable, treating it as having no effect *)
    ([], [])
  | (_, Some vstack_before) ->
    (* instruction is reachable *)
    match i.instr with
    | If (bt, _, _, _) ->
      (* the net effect of the head, which drops the first element of the stack *)
      ((T Type.I32) :: (List.map ~f:(fun t -> T t) (fst bt)),
       List.map ~f:(fun t -> T t) (snd bt))
    | Br _ ->
      (* The code after a br is not reachable, so we can assume that br drops everything from the stack *)
      let vstack = vstack_before in
      (List.mapi vstack ~f:(fun i _ -> Any (string_of_int i)), [])
    | BrIf _ ->
      let rest = List.drop vstack_before 1 in
      ([T Type.I32], List.mapi rest ~f:(fun i _ -> Any (string_of_int i)))
    | BrTable (_, _) ->
      let vstack = List.drop vstack_before 1 in
      ([T Type.I32] @ (List.mapi vstack ~f:(fun i _ -> Any (string_of_int i))), [])
    | Return ->
      (List.mapi vstack_before ~f:(fun i _ -> Any (string_of_int i)),
       (* was this, but it actually doesn't leave anything on the stack:
         (List.map cfg.return_types ~f:(fun t -> T t)) *)
      [])
    | Unreachable -> ([], [])
    | Merge -> ([], [])
    | Block _ | Loop _ -> failwith "should not happen" (* because we have the block/loop instructions handled in the first match *)

(** Construct a dummy list of instruction that has the given type *)
let dummy_instrs (t : instr_type_element list * instr_type_element list) (next_label : unit -> int) : (Instr.data, unit) Instr.labelled list =
  let dummy_label () : Instr.Label.t = { section = Instr.Label.Dummy; id = next_label () } in
  (* before anything, we remove parts of types that won't be needed, e.g., [i32] -> [i32] can be replaced by [] -> [] *)
  let rec loop (l1 : instr_type_element list) (l2 : instr_type_element list) (n : int) : int = match (l1, l2) with
    | (Any _) :: t1, _ :: t2
    | _ :: t1, (Any _) :: t2 -> loop t1 t2 (n+1)
    | T h1 :: t1, T h2 :: t2 when Type.equal h1 h2 -> loop t1 t2 (n+1)
    | _ -> n in
  let prefix = loop (fst t) (snd t) 0 in
  let input = List.drop (fst t) prefix in
  let output = List.drop (snd t) prefix in
  (* we pop everything off the stack, then we push *)
  let input = List.map input ~f:(fun _ -> { Instr.instr = Instr.Drop; label = dummy_label (); line_number = -1; annotation_before = (); annotation_after = (); }) in
  let push (v : Prim_value.t) = { Instr.instr = Instr.Const v; label = dummy_label (); line_number = -1; annotation_before = (); annotation_after = () } in
  let output = List.map output ~f:(function
      | Any _ -> push (Prim_value.I32 0l)
      | T Type.I32 -> push (Prim_value.I32 0l)
      | T Type.I64 -> push (Prim_value.I64 0L)
      | T Type.F32 -> push (Prim_value.F32 (Wasm.F32.of_float 0.))
      | T Type.F64 -> push (Prim_value.F64 (Wasm.F64.of_float 0.))) in
  input @ output

(** The type of an instruction on the stack: positive if it expects value on the stack, negative otherwise *)
let type_of (i : 'a Instr.t) (cfg : 'a Cfg.t) (instructions_map : Spec_domain.t Instr.t Instr.Label.Map.t) : (instr_type_element list * instr_type_element list) =
  match i with
  | Data d -> type_of_data d cfg instructions_map
  | Control c -> type_of_control c cfg instructions_map
  | Call c -> type_of_call c cfg instructions_map

let instrs_type (instrs : unit Instr.t list) (cfg : 'a Cfg.t) (instructions_map : Spec_domain.t Instr.t Instr.Label.Map.t) : (instr_type_element list * instr_type_element list) =
  let input, output = List.fold_left instrs ~init:([], []) ~f:(fun (initial_stack, current_stack) instr ->
      let (i, o) = type_of instr cfg instructions_map in
      let (initial_stack, current_stack) =
        List.fold_left i ~init:(initial_stack, current_stack) ~f:(fun (initial_stack, current_stack) t ->
          match current_stack with
          | _ :: rest ->
            (* We don't check that the types match, as we assume they will *)
            (initial_stack, rest)
          | [] ->
            (t :: initial_stack, current_stack)) in
      (initial_stack, (List.rev o) @ current_stack)) in
  List.rev input, output

let counter : int ref = ref 0
let reset_counter () : unit = counter := 0
let next_label : unit -> int =
    fun () ->
      let v = !counter in
      counter := v+1;
      v

let replace_with_equivalent_instructions (instrs : unit Instr.t list) (cfg : 'a Cfg.t) (instructions_map : Spec_domain.t Instr.t Instr.Label.Map.t) : unit Instr.t list =
  if List.is_empty instrs then instrs else
    let t = instrs_type instrs cfg instructions_map in
    let replaced = List.map (dummy_instrs t next_label) ~f:(fun i -> Instr.Data i) in
   Log.info (fun () -> Printf.sprintf "Replacing instructions %s of type %s -> %s with %s"
                (String.concat ~sep:"," (List.map ~f:Instr.to_string instrs))
                (String.concat ~sep:"," (List.map ~f:instr_type_element_to_string (fst t)))
                (String.concat ~sep:"," (List.map ~f:instr_type_element_to_string (snd t)))
                (String.concat ~sep:"," (List.map ~f:Instr.to_string replaced)));
    replaced

(* Check if the body is empty or only consist only of dummy instructions *)
let body_can_be_removed (body : unit Instr.t list) : bool =
  (* A safer alternative is simply: List.is_empty body *)
  List.for_all body ~f:(fun instr -> Instr.Label.equal_section (Instr.label instr).section Instr.Label.Dummy)

let rec slice (cfg : 'a Cfg.t) (cfg_instructions : Spec_domain.t Instr.t Instr.Label.Map.t) (original_instructions : unit Instr.t list) (instructions_to_keep : Instr.Label.Set.t): unit Instr.t list =
  let rec loop (instrs : unit Instr.t list) (to_remove_rev : unit Instr.t list) : unit Instr.t list =
    match instrs with
    | [] -> replace_with_equivalent_instructions (List.rev to_remove_rev) cfg cfg_instructions
    | (Control ({ instr = Block (bt, arity, body); _ } as instr)) as entire_instr :: rest ->
      (* if (fst arity) > 0 || (snd arity) > 0 then failwith "Unsupported: block with arity greater than 0"; *)
      let sliced_body = slice cfg cfg_instructions body instructions_to_keep in
      (* XXX: we could also drop the block if it is not empty but only contains instructions that are not part of the slice (basically, only dummy instructions) *)
      if body_can_be_removed sliced_body then
        (* Block body is empty, drop the block entirely *)
        loop rest (entire_instr :: to_remove_rev)
      else
        (replace_with_equivalent_instructions (List.rev to_remove_rev) cfg cfg_instructions) @ [Instr.Control { instr with instr = Block (bt, arity, sliced_body) }] @ loop rest []
    | (Control ({ instr = Loop (bt, arity, body); _ } as instr)) as entire_instr :: rest ->
      (* if (fst arity) > 0 || (snd arity) > 0 then failwith "Unsupported: loop with arity greater than 0"; *)
      let sliced_body = slice cfg cfg_instructions body instructions_to_keep in
      if body_can_be_removed sliced_body then
        loop rest (entire_instr :: to_remove_rev)
      else
        (replace_with_equivalent_instructions (List.rev to_remove_rev) cfg cfg_instructions) @ [Instr.Control { instr with instr = Loop (bt, arity, sliced_body) }] @ loop rest []
    | (Control ({ instr = If (bt, arity, then_, else_); _ } as instr)) as entire_instr :: rest ->
      (* if (fst arity) > 0 || (snd arity) > 0 then failwith "Unsupported: if with arity greater than 0"; *)
      let sliced_then = slice cfg cfg_instructions then_ instructions_to_keep in
      let sliced_else = slice cfg cfg_instructions else_ instructions_to_keep in
      if body_can_be_removed sliced_then && body_can_be_removed sliced_else then
        loop rest (entire_instr :: to_remove_rev)
      else
        (replace_with_equivalent_instructions (List.rev to_remove_rev) cfg cfg_instructions) @
        [Instr.Control { instr with instr = If (bt, arity,
                                                sliced_then,
                                                sliced_else) }] @ loop rest []
    | instr :: rest when Instr.Label.Set.mem instructions_to_keep (Instr.label instr) ->
    (replace_with_equivalent_instructions (List.rev to_remove_rev) cfg cfg_instructions) @ [instr] @ loop rest []
    | instr :: rest ->
      loop rest (instr :: to_remove_rev) in
  loop original_instructions []

let slice_to_funcinst 
    (module_ : Wasm_module.t) 
    (cfg : Spec_domain.t Cfg.t) 
    (cfg_instructions : Spec_domain.t Instr.t Instr.Label.Map.t) 
    ?instrs:(instructions_in_slice : Instr.Label.Set.t option = None) 
    (slicing_criteria : Instr.Label.Set.t) 
    (pointer_analysis : Value_set.pointer_analysis option) 
  : Func_inst.t =
  let instructions_in_slice = match instructions_in_slice with
    | Some instrs -> instrs
    | None ->
      Log.info (fun () -> "Computing instructions part of the slice");
      let instrs, _ = instructions_to_keep module_ cfg cfg_instructions (preanalysis module_ cfg cfg_instructions pointer_analysis) slicing_criteria in
      instrs in
  Log.info (fun () -> "Clearing annotations");
  let unit_cfg = Cfg.clear_annotations cfg in
  Log.info (fun () -> "Constructing a valid slice");
  let instructions = slice unit_cfg cfg_instructions (Cfg.body unit_cfg) instructions_in_slice  in
  { idx = cfg.idx;
    name = Some cfg.name;
    type_idx = cfg.type_idx;
    typ = (cfg.arg_types, cfg.return_types);
    code = { locals = cfg.local_types; body = instructions } }

(** Return the indices of each call_indirect instructions *)
let find_call_indirect_instructions (cfg : Spec_domain.t Cfg.t) : Instr.Label.t list =
  List.filter_map (Cfg.all_instructions_list cfg) ~f:(fun instr -> match instr with
      | Call {label; instr = CallIndirect _; _} -> Some label
      | _ -> None)

module Test = struct
  open Instr.Label.Test
  let build_cfg ?fidx:(fidx : int32 = 0l) (program : string) 
    : Wasm_module.t * unit Cfg.t * Spec_domain.t Cfg.t =
    let module_ = Wasm_module.of_string program in
    let cfg_raw = 
      Cfg_builder.build module_ fidx
      |> Cfg.without_empty_nodes_with_no_predecessors in
    let cfg = 
      Spec_inference.Intra.analyze module_ cfg_raw ()
    in
      (* Spec_analysis.analyze_intra1 module_ fidx in *)
    (module_, cfg_raw, cfg)

  let%test "simple slicing - first slicing criterion, only const" =
    Spec_inference.propagate_globals := false;
    Spec_inference.propagate_locals := false;
    Spec_inference.use_const := false;
    let module_, _, cfg = build_cfg "(module
  (type (;0;) (func (param i32) (result i32)))
  (func (;test;) (type 0) (param i32) (result i32)
    memory.size ;; Instr 0
    memory.size ;; Instr 1
    i32.add     ;; Instr 2 -- slicing criterion
    drop        ;; Instr 3
    memory.size ;; Instr 4
    memory.size ;; Instr 5
    i32.add)    ;; Instr 6
   (memory (;0;) 2))" in
    let all_instrs = Cfg.all_instructions cfg in
    let actual, _ = instructions_to_keep module_ cfg all_instrs (preanalysis module_ cfg all_instrs None) (Instr.Label.Set.singleton (lab 2)) in
    let expected = Instr.Label.Set.of_list [lab 0; lab 1; lab 2] in
    Instr.Label.Set.check_equality ~actual:actual ~expected:expected

  let%test "simple slicing - second slicing criterion, with locals" =
    Spec_inference.propagate_globals := false;
    Spec_inference.propagate_locals := false;
    Spec_inference.use_const := false;
    let module_, _, cfg = build_cfg "(module
  (type (;0;) (func (param i32) (result i32)))
  (func (;test;) (type 0) (param i32) (result i32)
    memory.size ;; Instr 0
    memory.size ;; Instr 1
    i32.add     ;; Instr 2
    drop        ;; Instr 3
    local.get 0 ;; Instr 4
    memory.size ;; Instr 5
    i32.add)    ;; Instr 6 -- slicing criterion
  (memory (;0;) 2))" in
    let all_instrs = Cfg.all_instructions cfg in
    let actual, _ = instructions_to_keep module_ cfg all_instrs (preanalysis module_ cfg all_instrs None) (Instr.Label.Set.singleton (lab 6)) in
    let expected = Instr.Label.Set.of_list [lab 4; lab 5; lab 6] in
    Instr.Label.Set.check_equality ~actual:actual ~expected:expected

  let%test "slicing with block and br_if" =
    Spec_inference.propagate_globals := false;
    Spec_inference.propagate_locals := false;
    Spec_inference.use_const := false;
    let module_, _, cfg = build_cfg "(module
  (type (;0;) (func (param i32) (result i32)))
  (func (;test;) (type 0) (param i32) (result i32)
    block         ;; Instr 0
      memory.size ;; Instr 1 -- data dependency of instruction 2
      br_if 0     ;; Instr 2 -- control dependency of the slicing criterion
      memory.size ;; Instr 3 -- slicing criterion
      drop        ;; Instr 4
    end
    local.get 0)  ;; Instr 5
  (memory (;0;) 2))" in
    let all_instrs = Cfg.all_instructions cfg in
    let actual, _ = instructions_to_keep module_ cfg all_instrs (preanalysis module_ cfg all_instrs None) (Instr.Label.Set.singleton (lab 3)) in
    let expected = Instr.Label.Set.of_list [lab 1; lab 2; lab 3] in
    Instr.Label.Set.check_equality ~actual:actual ~expected:expected

  let%test "slicing with block and br_if -- second slicing criterion" =
     Spec_inference.propagate_globals := false;
     Spec_inference.propagate_locals := false;
     Spec_inference.use_const := false;
     let module_, _, cfg = build_cfg "(module
  (type (;0;) (func (param i32) (result i32)))
  (func (;test;) (type 0) (param i32) (result i32)
    block         ;; Instr 0
      memory.size ;; Instr 1
      br_if 0     ;; Instr 2 -- has a data dep on 1
      memory.size ;; Instr 3 -- has a control dep on 2
      drop        ;; Instr 4 -- slicing criterion, has a data dep on instr 3
    end
    local.get 0)  ;; Instr 5
  (memory (;0;) 2))" in
    let all_instrs = Cfg.all_instructions cfg in
    let actual, _ = instructions_to_keep module_ cfg all_instrs (preanalysis module_ cfg all_instrs None) (Instr.Label.Set.singleton (lab 4)) in
    let expected = Instr.Label.Set.of_list [lab 1; lab 2; lab 3; lab 4] in
    Instr.Label.Set.check_equality ~actual:actual ~expected:expected

  let%test "slicing with merge blocks" =
     Spec_inference.propagate_globals := false;
     Spec_inference.propagate_locals := false;
     Spec_inference.use_const := false;
    let module_, _, cfg = build_cfg "(module
  (type (;0;) (func (param i32) (result i32)))
  (func (;test;) (type 0) (param i32) (result i32)
    memory.size     ;; Instr 0
    if (result i32) ;; Instr 1
      memory.size   ;; Instr 2
    else
      memory.size   ;; Instr 3
    end
    ;; Merge block 4 here
    ;; ----
    memory.size     ;; Instr 4
    memory.size     ;; Instr 5
    i32.add         ;; Instr 6
    drop            ;; Instr 7
    ;; ---- this previous part should not be part of the slice
    memory.size     ;; Instr 8
    i32.add)        ;; Instr 9 -- slicing criterion
  (memory (;0;) 2))" in
    let all_instrs = Cfg.all_instructions cfg in
    let actual, _ = instructions_to_keep module_ cfg all_instrs (preanalysis module_ cfg all_instrs None) (Instr.Label.Set.singleton (lab 9)) in
    (* Merge blocks do not need to be in the slice *)
    let expected = Instr.Label.Set.of_list [lab 0; lab 1; lab 2; lab 3; lab 8; lab 9] in
    Instr.Label.Set.check_equality ~actual:actual ~expected:expected

   let%test_unit "slicing with a block containing a single drop should produce a valid slice" =
     Spec_inference.propagate_globals := false;
     Spec_inference.propagate_locals := false;
     Spec_inference.use_const := false;
     let module_, _, cfg = build_cfg "(module
   (type (;0;) (func (param i32) (result i32)))
   (func (;test;) (type 0) (param i32) (result i32)
    block (result i32)            ;; Instr 0
      local.get 0                 ;; Instr 1 [i0]
      local.get 0                 ;; Instr 2 [i1, i0]
      if (param i32) (result i32) ;; Instr 3 [i0]
        drop                      ;; Instr 4 []
        i32.const 0               ;; Instr 5 [i4]
      else
        nop                       ;; Instr 6 [i0]
      end
                                  ;; [i0] and [i4] merged into [m1]
      i32.const 32                ;; Instr 7 ;; [i6, m1]
      i32.add                     ;; Instr 8 ;; [i7]
    end))" in
     let _funcinst = slice_to_funcinst module_ cfg (Cfg.all_instructions cfg) (Instr.Label.Set.singleton (lab 8)) in
     ()

   let%test_unit "slicing intra-block block containing a single drop - variant" =
     Spec_inference.propagate_globals := false;
     Spec_inference.propagate_locals := false;
     Spec_inference.use_const := false;
     let module_, _, cfg = build_cfg "(module
   (type (;0;) (func (param i32) (result i32)))
   (func (;test;) (type 0) (param i32) (result i32)
    block (result i32)            ;; Instr 0
      local.get 0                 ;; Instr 1
      local.get 0                 ;; Instr 2
      if (param i32) (result i32) ;; Instr 3
        drop                      ;; Instr 4
        i32.const 0               ;; Instr 5
      else
        i32.const 1               ;; Instr 6
        drop                      ;; Instr 7
      end
      i32.const 32                ;; Instr 8
      i32.add                     ;; Instr 9
    end))" in
     let _funcinst = slice_to_funcinst module_ cfg (Cfg.all_instructions cfg) (Instr.Label.Set.singleton (lab 9)) in
     ()

   let%test_unit "slicing with a block containing a single drop - variant" =
     Spec_inference.propagate_globals := false;
     Spec_inference.propagate_locals := false;
     Spec_inference.use_const := false;
     let module_, _, cfg = build_cfg "(module
   (type (;0;) (func (param i32) (result i32)))
   (func (;test;) (type 0) (param i32) (result i32)
    block (result i32)            ;; Instr 0
      local.get 0                 ;; Instr 1
      local.get 0                 ;; Instr 2
      if (param i32) (result i32) ;; Instr 3
        drop                      ;; Instr 4
        i32.const 0               ;; Instr 5
      else
        i32.const 1               ;; Instr 6
        drop                      ;; Instr 7
      end
      i32.const 32                ;; Instr 8
      i32.add                     ;; Instr 9
    end)
   )" in
     let _funcinst = slice_to_funcinst module_ cfg (Cfg.all_instructions cfg) (Instr.Label.Set.singleton (lab 9)) in
     ()

   let check_slice ?(test_name : string = "slicing test") ?(with_pointer_analysis : bool = false) original sliced fidx criterion =
     Spec_inference.propagate_globals := false;
     Spec_inference.propagate_locals := false;
     Spec_inference.use_const := false;
     let module_, cfg_raw, cfg = build_cfg ~fidx original in
     let pointer_analysis : Value_set.pointer_analysis option =
       if with_pointer_analysis then
         Some (Value_set.run_pointer_analysis module_ cfg_raw fidx)
       else
         None
     in
     let instruction = 
       Map.find_exn
       (cfg |> Cfg.all_instructions)
       (lab ~fidx criterion) in
     Printf.printf "[%s]\n\tslicing criterion: %s\n" test_name (Instr.to_string instruction);
     let actual = (slice_to_funcinst module_ cfg (Cfg.all_instructions cfg) (Instr.Label.Set.singleton (lab ~fidx criterion)) pointer_analysis).code.body in
     let _, _, expected_cfg = build_cfg ~fidx sliced in
     let expected = Cfg.body expected_cfg in
     if List.length expected <> List.length actual then begin
       Log.error (fun () -> Printf.sprintf "slices are different:\n---expected:---\n%s\n---\nand\n---actual:---\n%s\n---\n"
                    (Instr.list_to_string expected (fun () -> ""))
                    (Instr.list_to_string actual (fun () -> "")));
       false
     end else
       List.equal (fun x y ->
           if Instr.equal (fun () () -> true) (Instr.drop_labels x) (Instr.drop_labels y) then
             true
           else begin
             Log.error (fun () -> Printf.sprintf "instruction not equal: %s != %s\n" (Instr.to_string x) (Instr.to_string y));
             false
           end) expected actual


   let%test "slicing intra-block should only include the relevant instructions" =
     let original = "(module
   (type (;0;) (func (param i32) (result i32)))
   (func (;test;) (type 0) (param i32) (result i32)
    i32.const 0     ;; Instr 0
    i32.const 1     ;; Instr 1
    i32.add         ;; Instr 2
    drop            ;; Instr 3
    i32.const 2     ;; Instr 4
    i32.const 3     ;; Instr 5
    i32.add         ;; Instr 6
   )
   (table (;0;) 1 1 funcref)
   (memory (;0;) 2)
   (global (;0;) (mut i32) (i32.const 66560)))" in
     let sliced = "(module
   (type (;0;) (func (param i32) (result i32)))
   (func (;test;) (type 0) (param i32) (result i32)
    i32.const 0     ;; Instr 0
    i32.const 1     ;; Instr 1
    i32.add         ;; Instr 2
   )
   (table (;0;) 1 1 funcref)
   (memory (;0;) 2)
   (global (;0;) (mut i32) (i32.const 66560)))" in
     check_slice original sliced 0l 2

   let%test "slicing should replace type-varying instructions correctly" =
     let original = "(module
   (type (;0;) (func (param i32) (result)))
   (type (;1;) (func (param i32 i32) (result i32)))
   (func (;test;) (type 0) (param i32) (result)
    i32.const 1024  ;; instr 0
    local.get 0     ;; instr 1
    call 1          ;; instr 2, slicing criterion
    drop            ;; instr 3
    i32.const 4992  ;; instr 4
    i64.const 0     ;; instr 5
    i64.store       ;; instr 6
    i32.const 1024  ;; instr 7
    i32.const 0     ;; instr 8
    call 1          ;; instr 9
    drop            ;; instr 10
    i32.const 4992  ;; instr 11
    i64.const 0     ;; instr 12
    i64.store       ;; instr 13
    local.get 0     ;; instr 14
    i32.const 16    ;; instr 15
    i32.add         ;; instr 16
    global.set 0    ;; instr 17
   )
   (func (;1;) (type 1) (param i32 i32) (result i32)
     local.get 0)
   (table (;0;) 1 1 funcref)
   (memory (;0;) 2)
   (global (;0;) (mut i32) (i32.const 66560)))" in
     let sliced = "(module
  (type (;0;) (func (param i32) (result)))
  (type (;1;) (func (param i32 i32) (result i32)))
  (func (;0;) (type 0) (param i32) (result)
    i32.const 1024
    local.get 0
    call 1
    drop)
  (func (;1;) (type 1) (param i32 i32) (result i32)
    local.get 0)
  (table (;0;) 1 1 funcref)
  (memory (;0;) 2)
  (global (;0;) (mut i32) (i32.const 66560))
)" in
     check_slice original sliced 0l 2

   let%test "slicing with memory does not fail" =
     let original =  "(module
   (type (;0;) (func (param i32) (result)))
   (func (;test;) (type 0) (param i32) (result)
    memory.size     ;; Instr 0
    memory.size     ;; Instr 1
    i32.store       ;; Instr 2
    memory.size     ;; Instr 3
    memory.size     ;; Instr 4
    i32.store       ;; Instr 5
   )
   (table (;0;) 1 1 funcref)
   (memory (;0;) 2)
   (global (;0;) (mut i32) (i32.const 66560)))" in
     let sliced = "(module
   (type (;0;) (func (param i32) (result)))
   (func (;test;) (type 0) (param i32) (result)
    memory.size     ;; Instr 3
    memory.size     ;; Instr 4
    i32.store       ;; Instr 5
   )
   (table (;0;) 1 1 funcref)
   (memory (;0;) 2)
   (global (;0;) (mut i32) (i32.const 66560)))" in
     check_slice original sliced 0l 5

   let%test "slicing with memory contains the relevant store instruction" =
     let original = "(module
  (type (;0;) (func (param i32) (result i32)))
  (func (;test;) (type 0) (param i32) (result i32)
    memory.size     ;; Instr 0
    memory.size     ;; Instr 1
    i32.store       ;; Instr 2
    memory.size     ;; Instr 3
    i32.load)       ;; Instr 4
  (memory (;0;) 2))" in
     check_slice original original (* all instructions are kept *) 0l 4

   let%test "slice with merge block should not contain irrelevant instructions" =
     let original = "(module
  (type (;0;) (func (param i32 i32) (result i32)))
  (func (;test;) (type 0) (param i32 i32) (result i32)
    local.get 0 ;; Instr 0
    if ;; Instr 1
      i32.const 42 ;; Instr 2
      local.set 0 ;; Instr 3
    end
    local.get 1) ;; Instr 4
  )" in
     (* The slice should only contain instruction 4 among the original instructions.
     It can contain an empty block/if though. *)
     let sliced = "(module
  (type (;0;) (func (param i32 i32) (result i32)))
  (func (;test;) (type 0) (param i32 i32) (result i32)
    local.get 1) ;; Instr 4
  )" in
     check_slice original sliced 0l 4

   let%test "slice with merge block should not contain non-relevant instructions, variation" =
     let original = "(module
  (type (;0;) (func (param i32 i32) (result i32)))
  (func (;test;) (type 0) (param i32 i32) (result i32)
    local.get 0 ;; Instr 0
    if ;; Instr 1
      i32.const 42 ;; Instr 2
      local.set 0 ;; Instr 3
    else
      i32.const 42 ;; Instr 4
      local.set 1 ;; Instr 5
    end
    local.get 1) ;; Instr 6
  )" in
     (* The slice should not contain instructions 2 and 3 *)
     let sliced =
       "(module
  (type (;0;) (func (param i32 i32) (result i32)))
  (func (;test;) (type 0) (param i32 i32) (result i32)
    local.get 0 ;; Instr 0
    if ;; Instr 1
    else
      i32.const 42 ;; Instr 4
      local.set 1 ;; Instr 5
    end
    local.get 1) ;; Instr 6
  )" in
     check_slice original sliced 0l 6

  let%test "slice with i32.add" =
    let original = "(module
  (type (;0;) (func (param i32) (result i32)))
  (func (;test;) (type 0) (param i32) (result i32)
    global.get 0 ;; Instr 0
    i32.const 16 ;; Instr 1
    i32.sub      ;; Instr 2
    local.tee 0  ;; Instr 3
    local.get 0  ;; Instr 4
    i32.const 16 ;; Instr 5
    i32.add      ;; Instr 6
    global.set 0) ;; Instr 7, slicing criterion
  (global (;0;) (mut i32) (i32.const 69232)))
" in
    check_slice original original 0l 7

   let%test "slice with an empty if" =
     let original = "(module
(type (;0;) (func (param i32) (result i32)))
(func (;0;) (type 0)
  local.get 0
  i32.const -1
  i32.eq
  if
  end
  local.get 0
)
)" in
     let sliced = "(module
(type (;0;) (func (param i32) (result i32)))
(func (;0;) (type 0)
  local.get 0
))" in
     check_slice original sliced 0l 4

   let%test "slice with a if that only contains br" =
     let original = "(module
(type (;0;) (func (param i32) (result i32)))
(func (;0;) (type 0)
  block (result i32)
    local.get 0
    block (param i32)
      if
        br 0
      else
        br 1
      end
    end
    local.get 0
  end
))" in
     let sliced = "(module
(type (;0;) (func (param i32) (result i32)))
(func (;0;) (type 0)
  block (result i32)
    local.get 0
  end
))" in
     check_slice original sliced 0l 6

   let%test "slice on a simple infinite loop example" =
     let original = "(module
  (type (;0;) (func))
  (func (;0;) (type 0)
    loop
      i32.const 1 ;; slicing criterion
      if
        br 0
      end
    end))" in
     let sliced = "(module
  (type (;0;) (func))
  (func (;0;) (type 0)
    loop
      i32.const 1
      if
        br 0
      end
    end))" in
    check_slice original sliced 0l 2


   let%test "slice on the example from Agrawal 1994 (Fig. 3) should be correct" =
     let original = "(module
  (type (;0;) (func))
  (type (;1;) (func (result i32)))
  (type (;1;) (func (param i32) (result i32)))
  (func (;eof;) (type 1) (result i32)
    i32.const 0)
  (func (;read;) (type 1) (result i32)
    i32.const 0)
  (func (;f;) (type 2) (param i32) (result i32)
    local.get 0)
  (func (;test;) (type 0)
    (local i32 i32 i32)
    ;; Local 0: sum
    ;; Local 1: positive
    ;; Local 2: x
    block ;; block 0
      loop ;; loop 0 (L3)
        call 0 ;; eof() --> Instr 2 should be part of the slice
        br_if 1 ;; goto end of block 0 if eof() --> Instr 3 should be part of the slice
        block ;; block 1
          block ;; block 2
            call 1 ;; read() --> Inst r6 should be part of the slice
            local.tee 2 ;; x = read()
            br_if 0 ;; jump to end of block 2 (L8) if x != 0 (was: x > 0) --> Instr 8 should be part of the slice
            local.get 2
            call 2 ;; f(x)
            local.get 0
            i32.add ;; sum + f(x)
            local.set 0 ;; sum = sum + f(x)
            br 1 ;; jump to end of block 1 (L13)
          end ;; end of block 2 (L8)
          block ;; block 3
            local.get 1 ;; --> Instr 16 should be part of the slice
            i32.const 1 ;; --> Instr 17 should be part of the slice
            i32.add     ;; --> Instr 18 should be part of the slice
            local.set 1 ;; positives = positives + 1 --> should be part of the slice
            local.get 2
            br_if 0 ;; jump to end of block 3 (L12) if x != 0 (was: x%2 != 0)
            local.get 2
            call 2 ;; f(x) (was: f2(x))
            local.get 0
            i32.add
            local.set 0 ;; sum = sum + f2(x) (was + f2(x))
            br 1 ;; jump to end of block 1 (L13)
          end ;; end of block 3 (L12)
          local.get 2
          call 2
          local.get 0
          i32.add
          local.set 0 ;; sum = sum + f(x) (was + f3(x))
        end ;; end of block 1 (L13)
        br 0 ;; jump to beginning of loop 0 (L3) ;; --> Instr 33 should be part of the slice (with Agrawal's algorithm, not the conventional one!)
      end ;; end of loop 0
    end ;; end of block 0 (L14)
    local.get 0
    call 2 ;; f(sum) (was: write(sum))
    drop
    local.get 1 ;; --> Instr 37 should be part of the slice
    ;; The following instruction is the slicing criterion, i.e., instruction number 38
    call 2 ;; f(positives) (was: write(positives)) --> Instr 38 should be part of the slice
    drop
    ))" in
     let sliced = "(module
  (type (;0;) (func))
  (type (;1;) (func (result i32)))
  (type (;2;) (func (param i32) (result i32)))
  (func (;0;) (type 1) (result i32)
    i32.const 0)
  (func (;1;) (type 1) (result i32)
    i32.const 0)
  (func (;2;) (type 2) (param i32) (result i32)
    local.get 0)
  (func (;3;) (type 0)
    (local i32 i32 i32)
    ;; If a line is annotated with Agrawal, it means it is part of Agrawal's original solution
    ;; If a line is annotated with superfluous, it means it is part of our slice because we don't assume functions to be pure, while Agrawal does. Hence, our solution is an over-approximation of Agrawal's, but it is still correct.
    block
      loop
        call 0 ;; eof(), Agrawal
        br_if 1 ;; Agrawal
        block
          block
            call 1 ;; read(), Agrawal
            local.tee 2 ;; x = read(), Agrawal
            br_if 0 ;; Agrawal
            local.get 2 ;; superfluous due to call
            call 2 ;; superfluous due to missing purity assumption
            local.get 0 ;; superflous
            i32.add ;; superfluous
            local.set 0 ;; superfluous
            br 1 ;; Agrawal
          end
          block
            local.get 1 ;; Agrawal
            i32.const 1 ;; Agrawal
            i32.add ;; Agrawal
            local.set 1 ;; Agrawal
            local.get 2 ;; superfluous
            br_if 0 ;; superfluous
            local.get 2 ;; superfluous
            call 2 ;; superflous due to missing purity assumption
            local.get 0 ;; superfluous
            i32.add ;; superfluous
            local.set 0 ;; superfluous due to call 2 at the end
            br 1 ;; superfluous
          end
          local.get 2 ;; superfluous
          call 2 ;; superfluous
          local.get 0 ;; superfluous
          i32.add ;; superfluous
          local.set 0 ;; superfluous due to call 2 at the end
        end
        br 0 ;; Agrawal
      end
    end
    local.get 0 ;; superfluous due to call
    call 2 ;; superfluous due to missing purity assumption
    drop
    local.get 1 ;; Agrawal
    call 2 ;; Agrawal
    drop))" in
     check_slice original sliced 3l 38

   let%test "slice on the example from Agrawal 1994 (Fig. 5) should be correct" =
     let original = "(module
  (type (;0;) (func))
  (type (;1;) (func (result i32)))
  (type (;1;) (func (param i32) (result i32)))
  (func (;eof;) (type 1) (result i32)
    i32.const 0)
  (func (;read;) (type 1) (result i32)
    i32.const 0)
  (func (;f;) (type 2) (param i32) (result i32)
    local.get 0)
  (func (;test;) (type 0)
    (local i32 i32 i32)
    ;; Local 0: sum
    ;; Local 1: positive
    ;; Local 2: x
    block ;; block 0
      loop ;; loop 0 (L3)
        call 0 ;; eof() ;; --> Instr 2, part of the slice
        i32.const 0 ;; --> Instr 3, part of the slice
        i32.ne ;; --> Instr 4, part of the slice
        br_if 1 ;; goto end of block 0 if !eof() --> Instr 5, part of the slice
        call 1 ;; read() ;; --> Instr 6, part of the slice
        local.tee 2 ;; x = read()
        if ;; if (x != 0) (was if (x <= 0)) --> part of the slice
          local.get 2
          call 2 ;; f(x)
          local.get 0
          i32.add ;; sum + f(x)
          local.set 0 ;; sum = sum + f(x)
          br 1 ;; jump to the beggining of loop 0 (L3) ;; --> Instr 14, part of the slice (with Agrawal's additions)
        end
        local.get 1 ;; --> part of the slice
        i32.const 1 ;; --> part of the slice
        i32.add     ;; --> part of the slice
        local.set 1 ;; positives = positives + 1 --> Instr 18,  part of the slice
        local.get 2
        if ;; if (x != 0) (was: x%2 != 0)
          local.get 2
          call 2 ;; f(x) (was: f2(x))
          local.get 0
          i32.add
          local.set 0 ;; sum = sum + f2(x) (was + f2(x))
          br 1 ;; jump to beginning of loop (L3)
        end
        local.get 2
        call 2
        local.get 0
        i32.add
        local.set 0 ;; sum = sum + f(x) (was + f3(x))
        br 0 ;; jump to beginning of loop 0 (L3)
      end ;; end of loop 0
    end ;; end of block 0 (L14)
    local.get 0
    call 2 ;; f(sum) (was: write(sum))
    drop
    local.get 1 ;; --> Instr 36, part of the slice
    ;; The following instruction is the slicing criterion, i.e., instruction number 37
    call 2 ;; f(positives) (was: write(positives)) --> Instr 37, part of the slice
    drop
    ))" in
     let sliced = "(module
  (type (;0;) (func))
  (type (;1;) (func (result i32)))
  (type (;2;) (func (param i32) (result i32)))
  (func (;0;) (type 1) (result i32)
    i32.const 0)
  (func (;1;) (type 1) (result i32)
    i32.const 0)
  (func (;2;) (type 2) (param i32) (result i32)
    local.get 0)
  (func (;3;) (type 0)
    (local i32 i32 i32)
    block
      loop
        call 0 ;; Agrawal
        i32.const 0 ;; Agrawal
        i32.ne ;; Agrawal
        br_if 1 ;; Agrawal
        call 1 ;; Agrawal
        local.tee 2 ;; superfluous due to call
        if ;; Agrawal
          local.get 2 ;; superfluous due to call
          call 2 ;; superfluous due to missing purity assumption
          local.get 0 ;; superfluous due to local 0
          i32.add ;; superfluous due to local 0
          local.set 0 ;; superfluous due to call
          br 1 ;; Agrawal
        end
        local.get 1 ;; Agrawal
        i32.const 1 ;; Agrawal
        i32.add ;; Agrawal
        local.set 1 ;; Agrawal
        local.get 2 ;; superfluous
        if ;; superfluous
          local.get 2 ;; superfluous due to call 0
          call 2 ;; superfluous due to missing purity assumption
          local.get 0 ;; superfluous due to local 0
          i32.add ;; superfluous due to local 0
          local.set 0 ;; superfluous due to call
          br 1 ;; superfluous
        end
        local.get 2 ;; superfluous due to call
        call 2 ;; superfluous due to missing purity assumption
        local.get 0 ;; superfluous due to call 0
        i32.add ;; superfluous due to local 0
        local.set 0 ;; superfluous due to call
        br 0 ;; superfluous due to local 0
      end
    end
    local.get 0 ;; superfluous due to call
    call 2 ;; superfluous due to missing purity assumption
    drop ;; superfluous due to call
    local.get 1 ;; Agrawal
    call 2 ;; Agrawal
    drop)
)" in
     check_slice original sliced 3l 37

   let%test_unit "slicing function 14 of trmm" =
     let module_ = Wasm_module.of_file "../../../benchmarks/polybench-clang/trmm.wat" in
     Spec_inference.propagate_globals := false;
     Spec_inference.propagate_locals := false;
     Spec_inference.use_const := false;
     let cfg = Spec_analysis.analyze_intra1 module_ 14l in
     List.iter (find_call_indirect_instructions cfg) ~f:(fun instr_idx ->
        (* instr_idx is the label of a call_indirect instruction, slice it *)
        Spec_inference.propagate_locals := false;
        Spec_inference.propagate_globals := false;
        Spec_inference.use_const := false;
        let funcinst = slice_to_funcinst module_ cfg (Cfg.all_instructions cfg) (Instr.Label.Set.singleton instr_idx) None in
        let module_ = Wasm_module.replace_func module_ 14l funcinst in
        (* We should be able to re-annotate the graph *)
        let _new_cfg = Spec_analysis.analyze_intra1 module_ 14l in
        ())

   let%test_unit "slicing function 22 of trmm" =
     let module_ = Wasm_module.of_file "../../../benchmarks/polybench-clang/trmm.wat" in
     Spec_inference.propagate_globals := false;
     Spec_inference.propagate_locals := false;
     Spec_inference.use_const := false;
     let cfg = Spec_analysis.analyze_intra1 module_ 22l in
     List.iter (find_call_indirect_instructions cfg) ~f:(fun instr_idx ->
         (* instr_idx is the label of a call_indirect instruction, slice it *)
         Spec_inference.propagate_locals := false;
         Spec_inference.propagate_globals := false;
         Spec_inference.use_const := false;
         let funcinst = slice_to_funcinst module_ cfg (Cfg.all_instructions cfg) (Instr.Label.Set.singleton instr_idx) None in
         let module_ = Wasm_module.replace_func module_ 22l funcinst in
         (* We should be able to re-annotate the graph *)
         Spec_inference.propagate_locals := true;
         Spec_inference.propagate_globals := true;
         Spec_inference.use_const := true;
         let _new_cfg = Spec_analysis.analyze_intra1 module_ 22l in
         ())

   let%test "slicing of SCAM mug example (variant 1) should produce the full program as the slice" =
     let original = "(module
  (type (;0;) (func))
  (type (;1;) (func (result i32)))
  (type (;2;) (func (param i32) (result i32)))
  (func (;0;) (type 2) ;; int p(int i)
    i32.const 0)
  (func (;1;) (type 2) ;; int q(int c)
    i32.const 0)
  (func (;2;) (type 1) ;; int f()
    i32.const 0)
  (func (;3;) (type 1) ;; int g()
    i32.const 0)
  (func (;4;) (type 2) ;; int h(int i)
    i32.const 0)
  (func (;5;) (type 0) ;; int main()
    (local i32 i32 i32)
    ;; Local 0: i
    ;; Local 1: x
    ;; Local 2: c
    local.get 0
    call 0 ;; p(i)
    if ;; label = @1
      loop ;; label = @2
        local.get 2
        call 1 ;; q(c)
        if  ;; label = @3
          call 2 ;; f()
          ;; The following instruction is the slicing criterion
          local.set 1 ;; x = result of f()
          call 3 ;; g()
          local.set 2 ;; c = result of g()
        end
        local.get 0
        call 4 ;; ;; h(i)
        local.set 0 ;; i = result of h(i)
        local.get 0
        call 0 ;; p(i)
        br_if 0 (;@2;)
      end
    end)
   )" in
     check_slice original original 5l 8

   let%test "slicing of SCAM mug example (variant 2) should produce the full program as the slice" =
     let original = "(module
  (type (;0;) (func))
  (type (;1;) (func (result i32)))
  (type (;2;) (func (param i32) (result i32)))
  (func (;0;) (type 2) ;; int p(int i)
    i32.const 0)
  (func (;1;) (type 2) ;; int q(int c)
    i32.const 0)
  (func (;2;) (type 1) ;; int f()
    i32.const 0)
  (func (;3;) (type 1) ;; int g()
    i32.const 0)
  (func (;4;) (type 2) ;; int h(int i)
    i32.const 0)
  (func (;5;) (type 0) ;; int main()
    (local i32 i32 i32)
    ;; Local 0: i
    ;; Local 1: x
    ;; Local 2: c
    block
      loop
        local.get 0
        call 0 ;; p(i)
        i32.const 0
        i32.ne
        br_if 1
        local.get 2
        call 1 ;; q(c)
        if  ;; label = @3
          call 2 ;; f()
          ;; The following instruction is the slicing criterion
          local.set 1 ;; x = result of f()
          call 3 ;; g()
          local.set 2 ;; c = result of g()
        end
        local.get 0
        call 4 ;; ;; h(i)
        local.set 0 ;; i = result of h(i)
        br 0
      end
    end)
  )" in
     check_slice original original 5l 11

   let%test "slicing of Montréal boat example should produce the full program as the slice" =
     let original = "(module
  (type (;0;) (func))
  (type (;1;) (func (result i32)))
  (type (;2;) (func (param i32) (result i32)))
  (func (;0;) (type 2) ;; p(j)
    i32.const 0)
  (func (;1;) (type 2) ;; int q(k)
    i32.const 0)
  (func (;2;) (type 2) ;; int f1(k)
    i32.const 0)
  (func (;3;) (type 2) ;; int f2(k)
    i32.const 0)
  (func (;4;) (type 2) ;; int f3(k)
    i32.const 0)
  (func (;5;) (type 0) ;; void main()
    (local i32 i32 i32)
    ;; Local 0: j
    ;; Local 1: k
    local.get 0
    call 0 ;; p(j)
    if ;; label = @1
      loop ;; label = @2
        local.get 1
        call 1 ;; q(k)
        if  ;; label = @3
          local.get 1
          call 2 ;; f1(k)
          local.set 1 ;; k = result of f1(k)
        else
          local.get 1
          call 3 ;; f2(k)
          local.set 1 ;; k = result of f2(k)
          local.get 0
          call 4 ;; f3(j)
          local.set 0 ;; j = result of f3(j)
        end
        local.get 0
        call 0 ;; p(j)
        br_if 0 (;@2;)
      end
      local.get 0 ;; Slicing criterion
      drop
    end))" in
     check_slice original original 5l 19

   let word_count ="(module
  (type (;0;) (func))
  (type (;1;) (func (result i32)))
  (func (;0;) (type 1) ;; char getchar()
    i32.const 0)
  (func (;1;) (type 0) ;; void main()
    (local i32 i32 i32 i32 i32)
    ;; Local 0: c
    ;; Local 1: nl
    ;; Local 2: nw
    ;; Local 3: nc
    ;; Local 4: inword
    ;; EOF = -1
    ;; '\\n' = 10
    ;; ' ' = 32
    ;; '\\t' = 9
    call 0 ;; getchar();
    local.tee 0 ;; c = result of getchar();
    i32.const 0 ;; EOF
    i32.ne ;; c != EOF
    if ;; label = @1
      loop ;; label = @2
        local.get 3
        i32.const 1
        i32.add
        local.set 3 ;; nc = nc + 1
        local.get 0
        i32.const 10
        i32.eq ;; c = '\\n'
        if
          local.get 1
          i32.const 1
          i32.add
          local.set 1 ;; nl = nl + 1
        end
        local.get 0
        i32.const 32
        i32.eq ;; c == ' '
        ;; In the original program, the condition is c == ' ' || c == '\\n' || c = '\\t'
        if
          i32.const 0
          local.set 4 ;; inword = NO
        else
          local.get 4
          if ;; inword == NO
            i32.const 1
            local.set 4 ;; inword = YES
            local.get 2
            i32.const 1
            i32.add
            local.set 2 ;; nw = nw + 1
          end
        end
        call 0
        local.tee 0
        i32.const 0 ;; EOF
        i32.ne ;; c != EOF
        br_if 0
      end
    end
    local.get 0 ;; c
    drop
    local.get 1 ;; nl
    drop
    local.get 2 ;; nw
    drop
    local.get 3 ;; nc
    drop
    local.get 4 ;; inword
    drop))"

   let%test "slicing of word count example (slicing criterion 1) should produce the expected slice" =
     let slice = "(module
(type (;0;) (func))
(type (;1;) (func (result i32)))
(func (;0;) (type 1)
  i32.const 0
)
(func (;1;) (type 0)
(local i32 i32 i32 i32 i32)
  call 0 ;; getchar();
  local.tee 0 ;; c = getchar();
  i32.const 0 ;; EOF
  i32.ne ;; c != EOF
  if
    loop
      local.get 0 ;; c
      i32.const 32 ;; ' '
      i32.eq ;; c == ' '
      if
        i32.const 0 ;; NO
        local.set 4 ;; inword = NO
      else
        local.get 4 ;; inword
        if ;; inword == NO
          i32.const 1 ;; YES
          local.set 4 ;; inword = YES
          local.get 2 ;; nw
          i32.const 1
          i32.add
          local.set 2 ;; nw = nw + 1
        end
      end
      call 0
      local.tee 0
      i32.const 0
      i32.ne ;; c != EOF
      br_if 0
    end
  end
  local.get 2 ;; c
  drop
))" in
     check_slice word_count slice 1l 41
   let%test "slicing of word count example (slicing criterion 2) should produce the expected slice" =
     let slice = "(module
(type (;0;) (func))
(type (;1;) (func (result i32)))
(func (;0;) (type 1)
  i32.const 0
)
(func (;1;) (type 0)
(local i32 i32 i32 i32 i32)
  call 0
  i32.const 0
  i32.ne
  if
    loop
      local.get 3
      i32.const 1
      i32.add
      local.set 3  ;; nc = nc + 1
      call 0 ;; getchar()
      i32.const 0
      i32.ne ;; c != EOF
      br_if 0
    end
  end
  local.get 3
  drop
))" in
     check_slice word_count slice 1l 43

  let%test "slicing of word count example (slicing criterion 3) should produce the expected slice" =
     let slice = "(module
(type (;0;) (func))
(type (;1;) (func (result i32)))
(func (;0;) (type 1)
  i32.const 0
)
(func (;1;) (type 0)
(local i32 i32 i32 i32 i32)
  call 0
  local.tee 0
  i32.const 0
  i32.ne ;; c != EOF
  if
    loop
      local.get 0
      i32.const 10
      i32.eq ;; c = '\\n'
      if
        local.get 1
        i32.const 1
        i32.add
        local.set 1 ;; nl = nl + 1
      end
      call 0
      local.tee 0 ;; c = getchar()
      i32.const 0
      i32.ne
      br_if 0 ;; c != EOF
    end
  end
  local.get 1
  drop
))" in
     check_slice word_count slice 1l 39

    let%test "slicing of word count example (slicing criterion 4) should produce the expected slice" =
     let slice = "(module
(type (;0;) (func))
(type (;1;) (func (result i32)))
(func (;0;) (type 1)
  i32.const 0
)
(func (;1;) (type 0)
(local i32 i32 i32 i32 i32)
  call 0 ;; c = getchar()
  local.tee 0
  i32.const 0
  i32.ne ;; c != EOF
  if
    loop
      local.get 0
      i32.const 32
      i32.eq ;; c == ' '
      if
        i32.const 0
        local.set 4 ;; inword = NO
      else
        local.get 4 ;; inword == YES
        if
          i32.const 1
          local.set 4 ;; inword = NO
        end
      end
      call 0
      local.tee 0 ;; c = getchar()
      i32.const 0
      i32.ne
      br_if 0 ;; c == EOF
    end
  end
  local.get 4
  drop
))" in
     check_slice word_count slice 1l 45

    let%test "slicing of word count example (slicing criterion 5) should produce the expected slice" =
     let slice = "(module
(type (;0;) (func))
(type (;1;) (func (result i32)))
(func (;0;) (type 1)
  i32.const 0
)
(func (;1;) (type 0)
(local i32 i32 i32 i32 i32)
  call 0  ;; getchar()
  local.tee 0  ;; c = getchar();
  i32.const 0
  i32.ne ;; c != EOF
  if
    loop
      call 0 ;; getchar()
      local.tee 0 ;; c = getchar()
      i32.const 0
      i32.ne ;; c != EOF
      br_if 0
    end
  end
  local.get 0 ;; c
  drop
))" in
     check_slice word_count slice 1l 37

    let%test "slicing with return" =
      let original = "(module
(type (;0;) (func))
(type (;1;) (func (result i32)))
(func (;0;) (type 1)
  i32.const 0
  i32.const 1
  i32.const 2
  i32.const 3
  return))" in
      let slice = "(module
(type (;0;) (func (result i32)))
(func (;0;) (type 0)
  i32.const 0 ;; dummy instr
  i32.const 0 ;; dummy instr
  i32.const 0 ;; dummy instr
  i32.const 3
  return))" in
      check_slice original slice 0l 4

    let%test "slicing blocks with results and br" =
      let original = "(module
(type (;0;) (func))
(type (;1;) (func (param i32) (result i32)))
(func (;0;) (type 1) (param i32) (result i32)
  loop (result i32)
    block (result i32)
      local.get 0 ;; This is the slicing criterion
      drop
      i32.const 1
      if
        i32.const 1
        ;; The difficulty lies in removing this instruction, which is not considered part of the slice.
        ;; However, its effect on the stack (-1) depends on the current block arity (0) and the target block arity (1)
        br 1
      end
      i32.const 2
    end
  end))" in
     let slice = "(module
(type (;0;) (func))
(type (;1;) (func (param i32) (result i32)))
(func (;0;) (type 1) (param i32) (result i32)
  loop (result i32)
    block (result i32)
      local.get 0
      drop
      i32.const 0
    end
  end)
)" in
       check_slice original slice 0l 3

  let%test "slicing blocks with result" =
    let original = "(module
   (type (;0;) (func (param i32) (result i32)))
   (func (;0;) (type 0) (param i32) (result i32)
    i32.const 0     ;; Instr 0
    if (result i32) ;; Instr 1
      i32.const 1   ;; Instr 2
    else
      i32.const 2   ;; Instr 3
    end
    ;; ---- This part should not be part of the slice (until ----)
    i32.const 3     ;; Instr 4
    i32.const 4     ;; Instr 5
    i32.add         ;; Instr 6
    drop            ;; Instr 7
    ;; ----
    i32.const 5     ;; Instr 8
    i32.add)        ;; Instr 9
   )" in
    let slice = "(module
   (type (;0;) (func (param i32) (result i32)))
   (func (;test;) (type 0) (param i32) (result i32)
    i32.const 0     ;; Instr 0
    if (result i32) ;; Instr 1
      i32.const 1   ;; Instr 2
    else
      i32.const 2   ;; Instr 3
    end
    i32.const 5     ;; Instr 8
    i32.add)        ;; Instr 9
   )" in
    check_slice original slice 0l 9

  let%test "slicing with unreachable code" =
    let original = "(module
(type (;0;) (func (param i32) (result i32)))
(func (;0;) (type 0) (param i32) (result i32)
    block
      local.get 0
      br_if 0
      unreachable
    end
    local.get 0
))" in
    let slice = "(module
(type (;0;) (func (param i32) (result i32)))
(func (;0;) (type 0) (param i32) (result i32)
    block
      local.get 0
      drop
    end
    i32.const 0
))" in
    check_slice original slice 0l 1

  (* XXX: fix this one eventually *)
  (*
  let%test "slicing with unreachable return" =
    Log.warn (fun () -> "STARTING TEST\n----------\n--------\n%!");
    let original = "(module
(type (;0;) (func (result i32)))
(func (;0;) (type 0) (result i32)
    (local i32 i32)
    local.get 0 ;; [_]
    return ;; []
    i32.const -1 ;; [_] not reachable
    return) ;; [] not reachable
)" in
    let slice = "(module
(type (;0;) (func (result i32)))
(func (;0;) (type 0) (result i32)
    (local i32 i32)
    local.get 0 ;; [_]
))" in
    check_slice original slice 0l 0 *)

    let%test "slicing with extra breaks" =
      let original = "(module
(type (;0;) (func))
(func (;0;) (type 0)
    block
      br 0
      br 0
    end))
" in
      let slice = "(module
(type (;0;) (func))
(func (;0;) (type 0)
    block
      br 0
    end))
" in
      check_slice original slice 0l 1

    let%test "load depends on relevant store operations" =
      let original = 
        "(module
          (memory (export \"mem\") 1)
          (func 
            i32.const 14    ;; instr 0
            i32.const 99    ;; instr 1
            i32.store       ;; instr 2

            i32.const 15    ;; instr 3
            i32.const 99    ;; instr 4
            i32.store       ;; instr 5

            i32.const 16    ;; instr 6
            i32.const 99    ;; instr 7
            i32.store       ;; instr 8

            i32.const 17    ;; instr 9
            i32.const 99    ;; instr 10
            i32.store       ;; instr 11

            i32.const 18    ;; instr 12
            i32.const 99    ;; instr 13
            i32.store       ;; instr 14

            i32.const 19    ;; instr 15
            i32.const 99    ;; instr 16
            i32.store       ;; instr 17

            i32.const 20    ;; instr 18
            i32.const 99    ;; instr 19
            i32.store       ;; instr 20

            i32.const 21    ;; instr 21
            i32.const 99    ;; instr 22
            i32.store       ;; instr 23

            i32.const 22    ;; instr 24
            i32.const 99    ;; instr 25
            i32.store       ;; instr 26

            i32.const 18    ;; instr 27
            i32.load        ;; instr 28,   slicing criterion

            i32.const 14    ;; instr 29
            i32.const 99    ;; instr 30
            i32.store       ;; instr 31

            i32.const 15    ;; instr 32
            i32.const 99    ;; instr 33
            i32.store       ;; instr 34

            i32.const 16    ;; instr 35
            i32.const 99    ;; instr 36
            i32.store       ;; instr 37

            i32.const 17    ;; instr 38
            i32.const 99    ;; instr 39
            i32.store       ;; instr 40

            i32.const 18    ;; instr 41
            i32.const 99    ;; instr 42
            i32.store       ;; instr 43

            i32.const 19    ;; instr 44
            i32.const 99    ;; instr 45
            i32.store       ;; instr 46

            i32.const 20    ;; instr 47
            i32.const 99    ;; instr 48
            i32.store       ;; instr 49

            i32.const 21    ;; instr 50
            i32.const 99    ;; instr 51
            i32.store       ;; instr 52

            i32.const 22    ;; instr 53
            i32.const 99    ;; instr 54
            i32.store       ;; instr 55
            drop
          ))"
      in
      let slice =
        "(module
          (memory (export \"mem\") 1)
          (func 
            i32.const 15    ;; instr 3
            i32.const 99    ;; instr 4
            i32.store       ;; instr 5

            i32.const 16    ;; instr 6
            i32.const 99    ;; instr 7
            i32.store       ;; instr 8

            i32.const 17    ;; instr 9
            i32.const 99    ;; instr 10
            i32.store       ;; instr 11

            i32.const 18    ;; instr 12
            i32.const 99    ;; instr 13
            i32.store       ;; instr 14

            i32.const 19    ;; instr 15
            i32.const 99    ;; instr 16
            i32.store       ;; instr 17

            i32.const 20    ;; instr 18
            i32.const 99    ;; instr 19
            i32.store       ;; instr 20

            i32.const 21    ;; instr 21
            i32.const 99    ;; instr 22
            i32.store       ;; instr 23

            i32.const 18    ;; instr 27
            i32.load        ;; instr 28,   slicing criterion
            drop
          ))"
      in
      check_slice ~test_name:"load depends on relevant store operations" ~with_pointer_analysis:true original slice 0l 28

    let%test "load with non-singleton address depends on all possibly relevant stores" =
      let original =
        "(module
          (memory (export \"mem\") 1)
          (func (param $p i32)
            ;; Irrelevant: writes bytes [0, 3]
            i32.const 0
            i32.const 10
            i32.store

            ;; Relevant if $p selects address 4: writes bytes [4, 7]
            i32.const 4
            i32.const 20
            i32.store

            ;; Relevant if $p selects address 8: writes bytes [8, 11]
            i32.const 8
            i32.const 30
            i32.store

            ;; Irrelevant: writes bytes [12, 15]
            i32.const 12
            i32.const 40
            i32.store

            ;; load address is {4, 8}
            local.get $p
            if (result i32)
              i32.const 4
            else
              i32.const 8
            end
            i32.load        ;; slicing criterion
            drop
          ))"
      in
      let slice =
        "(module
          (memory (export \"mem\") 1)
          (func (param $p i32)
            ;; Relevant if $p selects address 4: writes bytes [4, 7]
            i32.const 4
            i32.const 20
            i32.store

            ;; Relevant if $p selects address 8: writes bytes [8, 11]
            i32.const 8
            i32.const 30
            i32.store

            ;; load address is {4, 8}
            local.get $p
            if (result i32)
              i32.const 4
            else
              i32.const 8
            end
            i32.load
            drop
          ))"
      in
      check_slice ~test_name:"load with non-singleton address depends on all possibly relevant stores" ~with_pointer_analysis:true original slice 0l 16

  let%test "load from relative address depends on overlapping store with same relative offset" =
    let original =
      "(module
        (memory (export \"mem\") 1)
        (func (param $p i32)
          ;; Irrelevant: writes bytes [$p, $p + 3]
          local.get $p
          i32.const 0
          i32.add
          i32.const 10
          i32.store

          ;; Relevant: writes bytes [$p + 3, $p + 6]
          local.get $p
          i32.const 3
          i32.add
          i32.const 20
          i32.store

          ;; Irrelevant: writes bytes [$p + 8, $p + 11]
          local.get $p
          i32.const 8
          i32.add
          i32.const 30
          i32.store

          ;; Reads bytes [$p + 4, $p + 7]
          local.get $p
          i32.const 4
          i32.add
          i32.load
          drop
        ))"
    in
    let slice =
      "(module
        (memory (export \"mem\") 1)
        (func (param $p i32)
          local.get $p
          i32.const 3
          i32.add
          i32.const 20
          i32.store

          local.get $p
          i32.const 4
          i32.add
          i32.load
          drop
        ))"
    in
    check_slice ~test_name:"load from relative address depends on overlapping store with same relative offset" ~with_pointer_analysis:true original slice 0l 18

  let%test "load keeps store with incompatible relative offset" =
    let original =
      "(module
        (memory (export \"mem\") 1)
        (func
          i32.const 0
          i32.const 10
          i32.store8
          i32.const 4
          i32.load ;; slicing criterion
          drop
        ))"
    in
    let slice =
      "(module
        (memory (export \"mem\") 1)
        (func
          i32.const 4
          i32.load
          drop
        ))"
    in
    check_slice ~test_name:"load keeps store with incompatible relative offset" ~with_pointer_analysis:true original slice 0l 4

  let%test "i32.load depends on store8 overlapping one byte" =
    let original =
      "(module
        (memory (export \"mem\") 1)
        (func
          ;; Irrelevant: writes byte 3, just before the load range [4, 7]
          i32.const 3
          i32.const 10
          i32.store8

          ;; Relevant: writes byte 7, the last byte read by the load
          i32.const 7
          i32.const 20
          i32.store8

          ;; Irrelevant: writes byte 8, just after the load range [4, 7]
          i32.const 8
          i32.const 30
          i32.store8

          i32.const 4
          i32.load        ;; slicing criterion
          drop
        ))"
    in
    let slice =
      "(module
        (memory (export \"mem\") 1)
        (func
          ;; Relevant: writes byte 7, the last byte read by the load
          i32.const 7
          i32.const 20
          i32.store8

          i32.const 4
          i32.load
          drop
        ))"
    in
    check_slice ~test_name:"i32.load depends on store8 overlapping one byte" ~with_pointer_analysis:true original slice 0l 10

  let%test "i32.load depends on store16 overlapping boundary bytes" =
    let original =
      "(module
        (memory (export \"mem\") 1)
        (func
          i32.const 2
          i32.const 10
          i32.store16

          i32.const 6
          i32.const 20
          i32.store16

          i32.const 8
          i32.const 30
          i32.store16

          i32.const 4
          i32.load
          drop
        ))"
    in
    let slice =
      "(module
        (memory (export \"mem\") 1)
        (func
          i32.const 6
          i32.const 20
          i32.store16

          i32.const 4
          i32.load
          drop
        ))"
    in
    check_slice ~test_name:"i32.load depends on store16 overlapping boundary bytes" ~with_pointer_analysis:true original slice 0l 10

  let%test "i32.load depends on overlapping i64.store" =
    let original =
      "(module
        (memory (export \"mem\") 1)
        (func
          ;; Irrelevant: writes bytes [0,7]
          i32.const 0
          i64.const 10
          i64.store

          ;; Relevant: writes bytes [4,11]
          i32.const 4
          i64.const 20
          i64.store

          ;; Irrelevant: writes bytes [12,19]
          i32.const 12
          i64.const 30
          i64.store

          ;; Reads bytes [8,11]
          i32.const 8
          i32.load
          drop
        ))"
    in
    let slice =
      "(module
        (memory (export \"mem\") 1)
        (func
          i32.const 4
          i64.const 20
          i64.store

          i32.const 8
          i32.load
          drop
        ))"
    in
    check_slice
      ~test_name:"i32.load depends on overlapping i64.store"
      ~with_pointer_analysis:true
      original slice 0l 10

      
  let%test "i32.load with offset depends on store with overlapping offset-adjusted address" =
    let original =
      "(module
        (memory (export \"mem\") 1)
        (func
          i32.const 0
          i32.const 10
          i32.store

          i32.const 0
          i32.const 20
          i32.store offset=4

          i32.const 0
          i32.const 30
          i32.store offset=8

          i32.const 4
          i32.load
          drop
        ))"
    in
    let slice =
      "(module
        (memory (export \"mem\") 1)
        (func
          i32.const 0
          i32.const 20
          i32.store offset=4

          i32.const 4
          i32.load
          drop
        ))"
    in
    check_slice
      ~test_name:"i32.load with offset depends on store with overlapping offset-adjusted address"
      ~with_pointer_analysis:true
      original slice 0l 10

  let%test "i32.load with relative base and offset depends on overlapping store offset" =
    let original =
      "(module
        (memory (export \"mem\") 1)
        (func (param $p i32)
          local.get $p
          i32.const 10
          i32.store offset=0

          local.get $p
          i32.const 20
          i32.store offset=4

          local.get $p
          i32.const 30
          i32.store offset=10

          local.get $p
          i32.load offset=6
          drop
        ))"
    in
    let slice =
      "(module
        (memory (export \"mem\") 1)
        (func (param $p i32)
          local.get $p
          i32.const 20
          i32.store offset=4

          local.get $p
          i32.load offset=6
          drop
        ))"
    in
    check_slice
      ~test_name:"i32.load with relative base and offset depends on overlapping store offset"
      ~with_pointer_analysis:true
      original slice 0l 10

  let%test "i32.load with relative non-singleton address keeps possibly overlapping store" =
    let original =
      "(module
        (memory (export \"mem\") 1)
        (func (param $p i32) (param $c i32)
          local.get $p
          i32.const 10
          i32.store offset=0

          local.get $p
          i32.const 20
          i32.store offset=6

          local.get $p
          i32.const 30
          i32.store offset=12

          local.get $p
          local.get $c
          if (result i32)
            i32.const 4
          else
            i32.const 8
          end
          i32.add
          i32.load
          drop
        ))"
    in
    let slice =
      "(module
        (memory (export \"mem\") 1)
        (func (param $p i32) (param $c i32)
          local.get $p
          i32.const 20
          i32.store offset=6

          local.get $p
          local.get $c
          if (result i32)
            i32.const 4
          else
            i32.const 8
          end
          i32.add
          i32.load
          drop
        ))"
    in
    check_slice
      ~test_name:"i32.load with relative non-singleton address keeps possibly overlapping store"
      ~with_pointer_analysis:true
      original slice 0l 15


  let%test "load keeps all overlapping stores" =
    let original =
      "(module
        (memory (export \"mem\") 1)
        (func
          ;; Writes [4,7]
          i32.const 4
          i32.const 10
          i32.store

          ;; Writes [6,9]
          i32.const 6
          i32.const 20
          i32.store

          ;; Irrelevant: writes [12,15]
          i32.const 12
          i32.const 30
          i32.store

          ;; Reads [7,10]
          i32.const 7
          i32.load
          drop
        ))"
    in
    let slice =
      "(module
        (memory (export \"mem\") 1)
        (func
          ;; Writes [4,7]
          i32.const 4
          i32.const 10
          i32.store

          ;; Writes [6,9]
          i32.const 6
          i32.const 20
          i32.store

          ;; Reads [7,10]
          i32.const 7
          i32.load
          drop
        ))"
    in
    check_slice
      ~test_name:"load keeps all overlapping stores"
      ~with_pointer_analysis:true
      original slice 0l 10

  let%test "load fully contained in store is a dependency" =
    let original =
      "(module
        (memory (export \"mem\") 1)
        (func
          ;; Writes [4,11]
          i32.const 4
          i64.const 10
          i64.store

          ;; Irrelevant: writes [12,15]
          i32.const 12
          i32.const 20
          i32.store

          ;; Reads [6,9]
          i32.const 6
          i32.load
          drop
        ))"
    in
    let slice =
      "(module
        (memory (export \"mem\") 1)
        (func
          ;; Writes [4,11]
          i32.const 4
          i64.const 10
          i64.store

          ;; Reads [6,9]
          i32.const 6
          i32.load
          drop
        ))"
    in
    check_slice
      ~test_name:"load fully contained in store is a dependency"
      ~with_pointer_analysis:true
      original slice 0l 7


  let%test "i32.load16_u depends only on stores overlapping two loaded bytes" =
    let original =
      "(module
        (memory (export \"mem\") 1)
        (func
          i32.const 3
          i32.const 10
          i32.store8       ;; writes one byte at addresse [3]

          i32.const 5
          i32.const 20
          i32.store8       ;; writes one byte at addresse [5]

          i32.const 6
          i32.const 30
          i32.store8       ;; writes one byte at addresse [6]

          i32.const 4
          i32.load16_u     ;; reads content of addresses [4, 5]
          drop
        ))"
    in
    let slice =
      "(module
        (memory (export \"mem\") 1)
        (func
          i32.const 5
          i32.const 20
          i32.store8

          i32.const 4
          i32.load16_u
          drop
        ))"
    in
    check_slice
      ~test_name:"i32.load16_u depends only on stores overlapping two loaded bytes"
      ~with_pointer_analysis:true
      original slice 0l 10

  let%test "i64.load32_u depends only on stores overlapping four loaded bytes" =
    let original =
      "(module
        (memory (export \"mem\") 1)
        (func
          i32.const 3
          i32.const 10
          i32.store8

          i32.const 7
          i32.const 20
          i32.store8

          i32.const 8
          i32.const 30
          i32.store8

          i32.const 4
          i64.load32_u
          drop
        ))"
    in
    let slice =
      "(module
        (memory (export \"mem\") 1)
        (func
          i32.const 7
          i32.const 20
          i32.store8

          i32.const 4
          i64.load32_u
          drop
        ))"
    in
    check_slice
      ~test_name:"i64.load32_u depends only on stores overlapping four loaded bytes"
      ~with_pointer_analysis:true
      original slice 0l 10

  let%test "i32.load8_u depends only on stores overlapping one loaded byte" =
    let original =
      "(module
        (memory (export \"mem\") 1)
        (func
          i32.const 2
          i32.const 10
          i32.store8

          i32.const 3
          i32.const 20
          i32.store8

          i32.const 4
          i32.const 30
          i32.store8

          i32.const 3
          i32.load8_u
          drop
        ))"
    in
    let slice =
      "(module
        (memory (export \"mem\") 1)
        (func
          i32.const 3
          i32.const 20
          i32.store8

          i32.const 3
          i32.load8_u
          drop
        ))"
    in
    check_slice
      ~test_name:"i32.load8_u depends only on stores overlapping one loaded byte"
      ~with_pointer_analysis:true
      original slice 0l 10


  let%test "i32.load fully contained in i64.store is a dependency" =
    let original =
      "(module
        (memory (export \"mem\") 1)
        (func
          i32.const 4
          i64.const 10
          i64.store

          i32.const 12
          i32.const 20
          i32.store

          i32.const 6
          i32.load
          drop
        ))"
    in
    let slice =
      "(module
        (memory (export \"mem\") 1)
        (func
          i32.const 4
          i64.const 10
          i64.store

          i32.const 6
          i32.load
          drop
        ))"
    in
    check_slice
      ~test_name:"i32.load fully contained in i64.store is a dependency"
      ~with_pointer_analysis:true
      original slice 0l 7

  let%test "i32.load8_s depends only on stores overlapping one loaded byte" =
    let original =
      "(module
        (memory (export \"mem\") 1)
        (func
          i32.const 9
          i32.const 10
          i32.store8

          i32.const 10
          i32.const 20
          i32.store8

          i32.const 11
          i32.const 30
          i32.store8

          i32.const 10
          i32.load8_s
          drop
        ))"
    in
    let slice =
      "(module
        (memory (export \"mem\") 1)
        (func
          i32.const 10
          i32.const 20
          i32.store8

          i32.const 10
          i32.load8_s
          drop
        ))"
    in
    check_slice
      ~test_name:"i32.load8_s depends only on stores overlapping one loaded byte"
      ~with_pointer_analysis:true
      original slice 0l 10

  let%test "i32.load depends on store16 fully contained in loaded bytes" =
    let original =
      "(module
        (memory (export \"mem\") 1)
        (func
          i32.const 2
          i32.const 10
          i32.store16

          i32.const 5
          i32.const 20
          i32.store16

          i32.const 8
          i32.const 30
          i32.store16

          i32.const 4
          i32.load
          drop
        ))"
    in
    let slice =
      "(module
        (memory (export \"mem\") 1)
        (func
          i32.const 5
          i32.const 20
          i32.store16

          i32.const 4
          i32.load
          drop
        ))"
    in
    check_slice
      ~test_name:"i32.load depends on store16 fully contained in loaded bytes"
      ~with_pointer_analysis:true
      original slice 0l 10

  let%test "load in loop depends on later overlapping store in same block" =
    let original =
      "(module
        (memory (export \"mem\") 1)
        (func
          loop
            i32.const 4
            i32.load
            drop

            i32.const 4
            i32.const 10
            i32.store

            i32.const 99
            i32.const 99
            i32.store

            br 0
          end
        ))"
    in
    let slice =
      "(module
        (memory (export \"mem\") 1)
        (func
          loop
            i32.const 4
            i32.load
            drop

            i32.const 4
            i32.const 10
            i32.store

            br 0
          end
        ))"
    in
    check_slice
      ~test_name:"load in loop depends on later overlapping store in same block"
      ~with_pointer_analysis:true
      original slice 0l 2


  let%test "load does not depend on later store in same non-loop block" =
    let original =
      "(module
        (memory (export \"mem\") 1)
        (func
          i32.const 4
          i32.load        ;; slicing criterion
          drop

          ;; Same address, but executed after the load in a non-loop block.
          i32.const 4
          i32.const 10
          i32.store
        ))"
    in
    let slice =
      "(module
        (memory (export \"mem\") 1)
        (func
          i32.const 4
          i32.load
          drop
        ))"
    in
    check_slice
      ~test_name:"load does not depend on later store in same non-loop block"
      ~with_pointer_analysis:true
      original slice 0l 1

  let%test "load depends on preceding memory.fill and memory.copy but not following ones" =
    let original =
      "(module
        (memory (export \"mem\") 1)
        (func
          i32.const 0
          i32.const 0
          i32.const 4
          memory.fill

          i32.const 8
          i32.const 0
          i32.const 4
          memory.copy

          i32.const 8
          i32.load
          drop

          i32.const 16
          i32.const 0
          i32.const 4
          memory.fill

          i32.const 24
          i32.const 16
          i32.const 4
          memory.copy
        ))"
    in
    let slice =
      "(module
        (memory (export \"mem\") 1)
        (func
          i32.const 0
          i32.const 0
          i32.const 4
          memory.fill

          i32.const 8
          i32.const 0
          i32.const 4
          memory.copy

          i32.const 8
          i32.load
          drop
        ))"
    in
    check_slice
      ~test_name:"load depends on preceding memory.fill and memory.copy but not following ones"
      ~with_pointer_analysis:true
      original slice 0l 9

  let%test "global.get depends only on calls that modify same global" =
    let original =
      "(module
        (type (;0;) (func (result i32)))
        (type (;1;) (func))

        (func $set_global_0 (type 1)
          i32.const 42
          global.set 0)

        (func $set_global_1 (type 1)
          i32.const 99
          global.set 1)

        (func $test (type 0) (result i32)
          call $set_global_1
          call $set_global_0
          global.get 0)

        (global (mut i32) (i32.const 0))
        (global (mut i32) (i32.const 0)))"
    in
    let slice =
      "(module
        (type (;0;) (func (result i32)))
        (type (;1;) (func))

        (func $set_global_0 (type 1)
          i32.const 42
          global.set 0)

        (func $set_global_1 (type 1))

        (func $test (type 0) (result i32)
          call $set_global_0
          global.get 0)

        (global (mut i32) (i32.const 0))
        (global (mut i32) (i32.const 0)))"
    in
    check_slice
      ~test_name:"global.get depends only on calls that modify same global"
      ~with_pointer_analysis:true
      original slice 2l 2

  let%test "global.get depends on direct call that transitively modifies same global" =
    let original =
      "(module
        (type (;0;) (func (result i32)))
        (type (;1;) (func))

        (func $set_global_0 (type 1)
          i32.const 42
          global.set 0)

        (func $set_global_1 (type 1)
          i32.const 99
          global.set 1)

        (func $call_set_global_0 (type 1)
          call $set_global_0)

        (func $call_set_global_1 (type 1)
          call $set_global_1)

        (func $test (type 0) (result i32)
          call $call_set_global_1
          call $call_set_global_0
          global.get 0)

        (global (mut i32) (i32.const 0))
        (global (mut i32) (i32.const 0)))"
    in
    let slice =
      "(module
        (type (;0;) (func (result i32)))
        (type (;1;) (func))

        (func $set_global_0 (type 1)
          i32.const 42
          global.set 0)

        (func $set_global_1 (type 1))

        (func $call_set_global_0 (type 1)
          call $set_global_0)

        (func $call_set_global_1 (type 1))

        (func $test (type 0) (result i32)
          call $call_set_global_0
          global.get 0)

        (global (mut i32) (i32.const 0))
        (global (mut i32) (i32.const 0)))"
    in
    check_slice
      ~test_name:"global.get depends on direct call that transitively modifies same global"
      ~with_pointer_analysis:true
      original slice 4l 2


  let%test "global.get does not depend on later direct call in same non-loop block" =
    let original =
      "(module
        (type (;0;) (func (result i32)))
        (type (;1;) (func))

        (func $set_global_0 (type 1)
          i32.const 42
          global.set 0)

        (func $test (type 0) (result i32)
          global.get 0
          call $set_global_0)

        (global (mut i32) (i32.const 0)))"
    in
    let slice =
      "(module
        (type (;0;) (func (result i32)))
        (type (;1;) (func))

        (func $set_global_0 (type 1))

        (func $test (type 0) (result i32)
          global.get 0)

        (global (mut i32) (i32.const 0)))"
    in
    check_slice
      ~test_name:"global.get does not depend on later direct call in same non-loop block"
      ~with_pointer_analysis:true
      original slice 1l 0

  let%test "global.get in loop depends on later direct call in same block" =
    let original =
      "(module
        (type (;0;) (func))
        (type (;1;) (func))

        (func $set_global_0 (type 1)
          i32.const 42
          global.set 0)

        (func $test (type 0)
          loop
            global.get 0
            drop
            call $set_global_0
            br 0
          end)

        (global (mut i32) (i32.const 0)))"
    in
    let slice =
      "(module
        (type (;0;) (func))
        (type (;1;) (func))

        (func $set_global_0 (type 1)
          i32.const 42
          global.set 0)

        (func $test (type 0)
          loop
            global.get 0
            drop
            call $set_global_0
            br 0
          end)

        (global (mut i32) (i32.const 0)))"
    in
    check_slice
      ~test_name:"global.get in loop depends on later direct call in same block"
      ~with_pointer_analysis:true
      original slice 1l 1


  let%test "global.get depends on all previous direct calls that modify same global" =
    let original =
      "(module
        (type (;0;) (func (result i32)))
        (type (;1;) (func))

        (func $set_global_0_to_42 (type 1)
          i32.const 42
          global.set 0)

        (func $set_global_0_to_99 (type 1)
          i32.const 99
          global.set 0)

        (func $set_global_1 (type 1)
          i32.const 123
          global.set 1)

        (func $test (type 0) (result i32)
          call $set_global_1
          call $set_global_0_to_42
          call $set_global_0_to_99
          global.get 0)

        (global (mut i32) (i32.const 0))
        (global (mut i32) (i32.const 0)))"
    in
    let slice =
      "(module
        (type (;0;) (func (result i32)))
        (type (;1;) (func))

        (func $set_global_0_to_42 (type 1)
          i32.const 42
          global.set 0)

        (func $set_global_0_to_99 (type 1)
          i32.const 99
          global.set 0)

        (func $set_global_1 (type 1))

        (func $test (type 0) (result i32)
          call $set_global_0_to_42
          call $set_global_0_to_99
          global.get 0)

        (global (mut i32) (i32.const 0))
        (global (mut i32) (i32.const 0)))"
    in
    check_slice
      ~test_name:"global.get depends on all previous direct calls that modify same global"
      ~with_pointer_analysis:true
      original slice 3l 3

  let%test "global.get after if depends only on branch call modifying same global" =
    let original =
      "(module
        (type (;0;) (func (param i32) (result i32)))
        (type (;1;) (func))

        (func $set_global_0 (type 1)
          i32.const 42
          global.set 0)

        (func $set_global_1 (type 1)
          i32.const 99
          global.set 1)

        (func $test (type 0) (param $c i32) (result i32)
          local.get $c
          if
            call $set_global_0
          else
            call $set_global_1
          end
          global.get 0)

        (global (mut i32) (i32.const 0))
        (global (mut i32) (i32.const 0)))"
    in
    let slice =
      "(module
        (type (;0;) (func (param i32) (result i32)))
        (type (;1;) (func))

        (func $set_global_0 (type 1)
          i32.const 42
          global.set 0)

        (func $set_global_1 (type 1))

        (func $test (type 0) (param $c i32) (result i32)
          local.get $c
          if
            call $set_global_0
          else
          end
          global.get 0)

        (global (mut i32) (i32.const 0))
        (global (mut i32) (i32.const 0)))"
    in
    check_slice
      ~test_name:"global.get after if depends only on branch call modifying same global"
      ~with_pointer_analysis:true
      original slice 2l 4

  let%test "global.get depends on nested direct call that modifies same global" =
    let original =
      "(module
        (type (;0;) (func (result i32)))
        (type (;1;) (func))

        (func $set_global_0 (type 1)
          i32.const 42
          global.set 0)

        (func $middle (type 1)
          call $set_global_0)

        (func $outer (type 1)
          call $middle)

        (func $test (type 0) (result i32)
          call $outer
          global.get 0)

        (global (mut i32) (i32.const 0)))"
    in
    let slice =
      "(module
        (type (;0;) (func (result i32)))
        (type (;1;) (func))

        (func $set_global_0 (type 1)
          i32.const 42
          global.set 0)

        (func $middle (type 1)
          call $set_global_0)

        (func $outer (type 1)
          call $middle)

        (func $test (type 0) (result i32)
          call $outer
          global.get 0)

        (global (mut i32) (i32.const 0)))"
    in
    check_slice
      ~test_name:"global.get depends on nested direct call that modifies same global"
      ~with_pointer_analysis:true
      original slice 3l 1

  let%test "global.get ignores nested direct call that modifies different global" =
    let original =
      "(module
        (type (;0;) (func (result i32)))
        (type (;1;) (func))

        (func $set_global_1 (type 1)
          i32.const 42
          global.set 1)

        (func $middle (type 1)
          call $set_global_1)

        (func $outer (type 1)
          call $middle)

        (func $test (type 0) (result i32)
          call $outer
          global.get 0)

        (global (mut i32) (i32.const 0))
        (global (mut i32) (i32.const 0)))"
    in
    let slice =
      "(module
        (type (;0;) (func (result i32)))
        (type (;1;) (func))

        (func $set_global_1 (type 1))

        (func $middle (type 1))

        (func $outer (type 1))

        (func $test (type 0) (result i32)
          global.get 0)

        (global (mut i32) (i32.const 0))
        (global (mut i32) (i32.const 0)))"
    in
    check_slice
      ~test_name:"global.get ignores nested direct call that modifies different global"
      ~with_pointer_analysis:true
      original slice 3l 1

  let%test "global.get does not depend on direct call that only reads same global" =
    let original =
      "(module
        (type (;0;) (func (result i32)))
        (type (;1;) (func))

        (func $read_global_0 (type 1)
          global.get 0
          drop)

        (func $test (type 0) (result i32)
          call $read_global_0
          global.get 0)

        (global (mut i32) (i32.const 0)))"
    in
    let slice =
      "(module
        (type (;0;) (func (result i32)))
        (type (;1;) (func))

        (func $read_global_0 (type 1))

        (func $test (type 0) (result i32)
          global.get 0)

        (global (mut i32) (i32.const 0)))"
    in
    check_slice
      ~test_name:"global.get does not depend on direct call that only reads same global"
      ~with_pointer_analysis:true
      original slice 1l 1


  let%test "global.get does not depend on later direct call even if it modifies same global" =
    let original =
      "(module
        (type (;0;) (func (result i32)))
        (type (;1;) (func))

        (func $set_global_0 (type 1)
          i32.const 42
          global.set 0)

        (func $test (type 0) (result i32)
          global.get 0
          call $set_global_0)

        (global (mut i32) (i32.const 0)))"
    in
    let slice =
      "(module
        (type (;0;) (func (result i32)))
        (type (;1;) (func))

        (func $set_global_0 (type 1))

        (func $test (type 0) (result i32)
          global.get 0)

        (global (mut i32) (i32.const 0)))"
    in
    check_slice
      ~test_name:"global.get does not depend on later direct call even if it modifies same global"
      ~with_pointer_analysis:true
      original slice 1l 0


  let%test "global.get depends only on relevant indirect call target" =
    let original =
      "(module
        (type (;0;) (func (result i32)))
        (type (;1;) (func))

        (func $set_global_0 (type 1)
          i32.const 42
          global.set 0)

        (func $set_global_1 (type 1)
          i32.const 99
          global.set 1)

        (func $test (type 0) (result i32)
          i32.const 1
          call_indirect (type 1)

          i32.const 0
          call_indirect (type 1)

          global.get 0)

        (table 2 funcref)
        (elem (i32.const 0) $set_global_0 $set_global_1)
        (global (mut i32) (i32.const 0))
        (global (mut i32) (i32.const 0)))"
    in
    let slice =
      "(module
        (type (;0;) (func (result i32)))
        (type (;1;) (func))

        (func $set_global_0 (type 1)
          i32.const 42
          global.set 0)

        (func $set_global_1 (type 1))

        (func $test (type 0) (result i32)
          i32.const 0
          call_indirect (type 1)

          global.get 0)

        (table 2 funcref)
        (elem (i32.const 0) $set_global_0 $set_global_1)
        (global (mut i32) (i32.const 0))
        (global (mut i32) (i32.const 0)))"
    in
    check_slice
      ~test_name:"global.get depends only on relevant indirect call target"
      ~with_pointer_analysis:true
      original slice 2l 4


  let%test "global.get depends on indirect call with unknown relevant target" =
    let original =
      "(module
        (type (;0;) (func (result i32)))
        (type (;1;) (func (param i32) (result i32)))
        (type (;2;) (func))

        (func $set_global_0 (type 2)
          i32.const 42
          global.set 0)

        (func $set_global_1 (type 2)
          i32.const 99
          global.set 1)

        (func $test (type 1) (param $idx i32) (result i32)
          local.get $idx
          call_indirect (type 2)

          global.get 0)

        (table 2 funcref)
        (elem (i32.const 0) $set_global_0 $set_global_1)
        (global (mut i32) (i32.const 0))
        (global (mut i32) (i32.const 0)))"
    in
    let slice =
      "(module
        (type (;0;) (func (result i32)))
        (type (;1;) (func (param i32) (result i32)))
        (type (;2;) (func))

        (func $set_global_0 (type 2)
          i32.const 42
          global.set 0)

        (func $set_global_1 (type 2))

        (func $test (type 1) (param $idx i32) (result i32)
          local.get $idx
          call_indirect (type 2)

          global.get 0)

        (table 2 funcref)
        (elem (i32.const 0) $set_global_0 $set_global_1)
        (global (mut i32) (i32.const 0))
        (global (mut i32) (i32.const 0)))"
    in
    check_slice
      ~test_name:"global.get depends on indirect call with unknown relevant target"
      ~with_pointer_analysis:true
      original slice 2l 2


let%test "global.get depends on indirect call with unknown relevant target 2" =
    let original =
      "(module
        (type (;0;) (func (result i32)))
        (type (;1;) (func (param i32) (result i32)))
        (type (;2;) (func))

        (func $set_global_0 (type 2)
          i32.const 42
          global.set 0)

        (func $set_global_1 (type 2)
          i32.const 99
          global.set 1)

        (func $test (type 1) (param $x i32) (result i32)
          local.get $x
          if (result i32)
            i32.const 0
          else
            i32.const 1
          end
          call_indirect (type 2)

          global.get 0)

        (table 2 funcref)
        (elem (i32.const 0) $set_global_0 $set_global_1)
        (global (mut i32) (i32.const 0))
        (global (mut i32) (i32.const 0)))"
    in
    let slice =
      "(module
        (type (;0;) (func (result i32)))
        (type (;1;) (func (param i32) (result i32)))
        (type (;2;) (func))

        (func $set_global_0 (type 2)
          i32.const 42
          global.set 0)

        (func $set_global_1 (type 2))

        (func $test (type 1) (param $x i32) (result i32)
          local.get $x
          if (result i32)
            i32.const 0
          else
            i32.const 1
          end
          call_indirect (type 2)

          global.get 0)

        (table 2 funcref)
        (elem (i32.const 0) $set_global_0 $set_global_1)
        (global (mut i32) (i32.const 0))
        (global (mut i32) (i32.const 0)))"
    in
    check_slice
      ~test_name:"global.get depends on indirect call with unknown relevant target 2"
      ~with_pointer_analysis:true
      original slice 2l 5


  let%test "global.get ignores unknown indirect call when no table target modifies same global" =
    let original =
      "(module
        (type (;0;) (func (param i32) (result i32)))
        (type (;1;) (func))

        (func $set_global_0_not_in_table (type 1)
          i32.const 42
          global.set 0)

        (func $set_global_1 (type 1)
          i32.const 99
          global.set 1)

        (func $do_nothing (type 1))

        (func $test (type 0) (param $p i32) (result i32)
          local.get $p
          call_indirect (type 1)
          global.get 0)

        (table 2 funcref)
        (elem (i32.const 0) $set_global_1 $do_nothing)
        (global (mut i32) (i32.const 0))
        (global (mut i32) (i32.const 0)))"
    in
    let slice =
      "(module
        (type (;0;) (func (param i32) (result i32)))
        (type (;1;) (func))

        (func $set_global_0_not_in_table (type 1))

        (func $set_global_1 (type 1))

        (func $do_nothing (type 1))

        (func $test (type 0) (param $p i32) (result i32)
          global.get 0)

        (table 2 funcref)
        (elem (i32.const 0) $set_global_1 $do_nothing)
        (global (mut i32) (i32.const 0))
        (global (mut i32) (i32.const 0)))"
    in
    check_slice
      ~test_name:"global.get ignores unknown indirect call when no table target modifies same global"
      ~with_pointer_analysis:true
      original slice 3l 2


  let%test "global.get ignores indirect call to table target that does not modify same global" =
    let original =
      "(module
        (type (;0;) (func (result i32)))
        (type (;1;) (func))

        (func $f0 (type 1)
          i32.const 11
          global.set 1)

        (func $f1 (type 1)
          i32.const 42
          global.set 0)

        (func $f2 (type 1)
          i32.const 22
          global.set 1)

        (func $test (type 0) (result i32)
          i32.const 1
          call_indirect (type 1)
          global.get 0)

        (table 2 funcref)
        (elem (i32.const 0) $f0 $f2)

        (global (mut i32) (i32.const 0))
        (global (mut i32) (i32.const 0)))"
    in
    let slice =
      "(module
        (type (;0;) (func (result i32)))
        (type (;1;) (func))

        (func $f0 (type 1))
        (func $f1 (type 1))
        (func $f2 (type 1))

        (func $test (type 0) (result i32)
          global.get 0)

        (table 2 funcref)
        (elem (i32.const 0) $f0 $f2)

        (global (mut i32) (i32.const 0))
        (global (mut i32) (i32.const 0)))"
    in
    check_slice
      ~test_name:"global.get ignores indirect call to table target that does not modify same global"
      ~with_pointer_analysis:true
      original slice 3l 2


let%test "global.get depends on indirect call to table target that modifies same global" =
  let original =
    "(module
      (type (;0;) (func (result i32)))
      (type (;1;) (func))

      (func $f0 (type 1)
        i32.const 11
        global.set 1)

      (func $f1_not_in_table (type 1)
        i32.const 99
        global.set 1)

      (func $f2 (type 1)
        i32.const 42
        global.set 0)

      (func $test (type 0) (result i32)
        i32.const 1
        call_indirect (type 1)
        global.get 0)

      (table 2 funcref)
      (elem (i32.const 0) $f0 $f2)

      (global (mut i32) (i32.const 0))
      (global (mut i32) (i32.const 0)))"
  in
  let slice =
    "(module
      (type (;0;) (func (result i32)))
      (type (;1;) (func))

      (func $f0 (type 1))

      (func $f1_not_in_table (type 1))

      (func $f2 (type 1)
        i32.const 42
        global.set 0)

      (func $test (type 0) (result i32)
        i32.const 1
        call_indirect (type 1)
        global.get 0)

      (table 2 funcref)
      (elem (i32.const 0) $f0 $f2)

      (global (mut i32) (i32.const 0))
      (global (mut i32) (i32.const 0)))"
  in
  check_slice
    ~test_name:"global.get depends on indirect call to table target that modifies same global"
    ~with_pointer_analysis:true
    original slice 3l 2


  let%test "global.get depends on unknown indirect call with relevant table target" =
    let original =
      "(module
        (type (;0;) (func (param i32) (result i32)))
        (type (;1;) (func))

        (func $f0 (type 1)
          i32.const 11
          global.set 1)

        (func $f1 (type 1)
          i32.const 42
          global.set 0)

        (func $f2 (type 1)
          i32.const 22
          global.set 1)

        (func $test (type 0) (param $p i32) (result i32)
          local.get $p
          call_indirect (type 1)
          global.get 0)

        (table 3 funcref)
        (elem (i32.const 0) $f0 $f1 $f2)

        (global (mut i32) (i32.const 0))
        (global (mut i32) (i32.const 0)))"
    in
    let slice =
      "(module
        (type (;0;) (func (param i32) (result i32)))
        (type (;1;) (func))

        (func $f0 (type 1))

        (func $f1 (type 1)
          i32.const 42
          global.set 0)

        (func $f2 (type 1))

        (func $test (type 0) (param $p i32) (result i32)
          local.get $p
          call_indirect (type 1)
          global.get 0)

        (table 3 funcref)
        (elem (i32.const 0) $f0 $f1 $f2)

        (global (mut i32) (i32.const 0))
        (global (mut i32) (i32.const 0)))"
    in
    check_slice
      ~test_name:"global.get depends on unknown indirect call with relevant table target"
      ~with_pointer_analysis:true
      original slice 3l 2


  let%test "global.get ignores call that restores modified global before returning" =
    let original = "(module
                      (type (;0;) (func))
                      (type (;1;) (func (result i32)))

                      (func (;0;) (type 0)
                        global.get 0
                        i32.const 1
                        i32.add
                        global.set 0

                        global.get 0
                        i32.const 1
                        i32.sub
                        global.set 0)

                      (func (;1;) (type 1) (result i32)
                        call 0       ;; Instr 0: temporarily modifies global 0, then restores it
                        global.get 0 ;; Instr 1: slicing criterion
                      )

                      (global (;0;) (mut i32) (i32.const 42)))"
    in
    let slice = "(module
                  (type (;0;) (func))
                  (type (;1;) (func (result i32)))

                  (func (;0;) (type 0)
                    global.get 0
                    i32.const 1
                    i32.add
                    global.set 0

                    global.get 0
                    i32.const 1
                    i32.sub
                    global.set 0)

                  (func (;1;) (type 1) (result i32)
                    global.get 0
                  )

                  (global (;0;) (mut i32) (i32.const 42)))"
    in
    check_slice 
      ~test_name:"global.get ignores call that restores modified global before returning"
      ~with_pointer_analysis:true
      original slice 1l 1

  let%test "global.get ignores call that saves and restores global through local" =
    let original = "(module
                      (type (;0;) (func))
                      (type (;1;) (func (result i32)))

                      (func (;0;) (type 0) (local i32)
                        global.get 0
                        local.set 0
                        i32.const 123
                        global.set 0
                        local.get 0
                        global.set 0)

                      (func (;1;) (type 1) (result i32)
                        call 0
                        global.get 0)

                      (global (;0;) (mut i32) (i32.const 42)))"
    in
    let slice = "(module
                      (type (;0;) (func))
                      (type (;1;) (func (result i32)))

                      (func (;0;) (type 0) (local i32)
                        global.get 0
                        local.set 0
                        i32.const 123
                        global.set 0
                        local.get 0
                        global.set 0)

                      (func (;1;) (type 1) (result i32)
                        global.get 0)

                      (global (;0;) (mut i32) (i32.const 42)))"
    in
    check_slice
      ~test_name:"global.get ignores call that saves and restores global through local"
      ~with_pointer_analysis:true
      original slice 1l 1

  let%test "global.get keeps call that may leave global modified on one branch" =
    let original = "(module
                      (type (;0;) (func))
                      (type (;1;) (func (result i32)))

                      (func (;0;) (type 0)
                        global.get 1
                        if
                          i32.const 123
                          global.set 0
                        else
                          global.get 0
                          global.set 0
                        end)

                      (func (;1;) (type 1) (result i32)
                        call 0
                        global.get 0)

                      (global (;0;) (mut i32) (i32.const 42))
                      (global (;1;) (mut i32) (i32.const 0)))"
    in
    let slice = original in
    check_slice
      ~test_name:"global.get keeps call that may leave global modified on one branch"
      ~with_pointer_analysis:true
      original slice 1l 1

  let%test "global.get keeps only call that actually changes same global" =
    let original = "(module
                      (type (;0;) (func))
                      (type (;1;) (func (result i32)))

                      (func (;0;) (type 0)
                        (local i32)
                        global.get 0
                        local.set 0
                        i32.const 123
                        global.set 0
                        local.get 0
                        global.set 0)

                      (func (;1;) (type 0)
                        i32.const 999
                        global.set 0)

                      (func (;2;) (type 1) (result i32)
                        call 0
                        call 1
                        global.get 0)

                      (global (;0;) (mut i32) (i32.const 42)))"
    in
    let slice = "(module
                    (type (;0;) (func))
                    (type (;1;) (func (result i32)))

                    (func (;0;) (type 0)
                      (local i32)
                      global.get 0
                      local.set 0
                      i32.const 123
                      global.set 0
                      local.get 0
                      global.set 0)

                    (func (;1;) (type 0)
                      i32.const 999
                      global.set 0)

                    (func (;2;) (type 1) (result i32)
                      call 1
                      global.get 0)

                    (global (;0;) (mut i32) (i32.const 42)))"
    in
    check_slice
      ~test_name:"global.get keeps only call that actually changes same global"
      ~with_pointer_analysis:true
      original slice 2l 2


  let%test "global.get ignores call that changes different global" =
    let original = "(module
                      (type (;0;) (func))
                      (type (;1;) (func (result i32)))

                      (func (;0;) (type 0)
                        i32.const 999
                        global.set 1)

                      (func (;1;) (type 1) (result i32)
                        call 0
                        global.get 0)

                      (global (;0;) (mut i32) (i32.const 42))
                      (global (;1;) (mut i32) (i32.const 0)))"
    in
    let slice = "(module
                    (type (;0;) (func))
                    (type (;1;) (func (result i32)))

                    (func (;0;) (type 0)
                      i32.const 999
                      global.set 1)

                    (func (;1;) (type 1) (result i32)
                      global.get 0)

                    (global (;0;) (mut i32) (i32.const 42))
                    (global (;1;) (mut i32) (i32.const 0)))"
    in
    check_slice
      ~test_name:"global.get ignores call that changes different global"
      ~with_pointer_analysis:true
      original slice 1l 1

  let%test "global.get ignores restored call and call changing different global" =
    let original =
      "(module
        (type (;0;) (func))
        (type (;1;) (func (result i32)))

        (func (;0;) (type 0)
          (local i32)
          global.get 0
          local.set 0
          i32.const 123
          global.set 0
          local.get 0
          global.set 0)

        (func (;1;) (type 0)
          i32.const 999
          global.set 1)

        (func (;2;) (type 1) (result i32)
          call 0
          call 1
          global.get 0)

        (global (;0;) (mut i32) (i32.const 42))
        (global (;1;) (mut i32) (i32.const 0)))"
    in
    let slice =
      "(module
        (type (;0;) (func))
        (type (;1;) (func (result i32)))

        (func (;0;) (type 0)
          (local i32)
          global.get 0
          local.set 0
          i32.const 123
          global.set 0
          local.get 0
          global.set 0)

        (func (;1;) (type 0)
          i32.const 999
          global.set 1)

        (func (;2;) (type 1) (result i32)
          global.get 0)

        (global (;0;) (mut i32) (i32.const 42))
        (global (;1;) (mut i32) (i32.const 0)))"
    in
    check_slice
      ~test_name:"global.get ignores restored call and call changing different global"
      ~with_pointer_analysis:true
      original slice 2l 2


  let%test "global.get keeps call_indirect that changes same global" =
    let original =
      "(module
        (type (;0;) (func))
        (type (;1;) (func (result i32)))

        (func (;0;) (type 0)
          i32.const 999
          global.set 0)

        (func (;1;) (type 1) (result i32)
          i32.const 0
          call_indirect (type 0)
          global.get 0)

        (table (;0;) 1 funcref)
        (elem (;0;) (i32.const 0) func 0)
        (global (;0;) (mut i32) (i32.const 42)))"
    in
    check_slice
      ~test_name:"global.get keeps call_indirect that changes same global"
      ~with_pointer_analysis:true
      original original 1l 2


  let%test "global.get ignores call_indirect that restores same global" =
    let original =
      "(module
        (type (;0;) (func))
        (type (;1;) (func (result i32)))

        (func (;0;) (type 0)
          (local i32)
          global.get 0
          local.set 0
          i32.const 999
          global.set 0
          local.get 0
          global.set 0)

        (func (;1;) (type 1) (result i32)
          i32.const 0
          call_indirect (type 0)
          global.get 0)

        (table (;0;) 1 funcref)
        (elem (;0;) (i32.const 0) func 0)
        (global (;0;) (mut i32) (i32.const 42)))"
    in
    let slice =
      "(module
        (type (;0;) (func))
        (type (;1;) (func (result i32)))

        (func (;0;) (type 0)
          (local i32)
          global.get 0
          local.set 0
          i32.const 999
          global.set 0
          local.get 0
          global.set 0)

        (func (;1;) (type 1) (result i32)
          global.get 0)

        (table (;0;) 1 funcref)
        (elem (;0;) (i32.const 0) func 0)
        (global (;0;) (mut i32) (i32.const 42)))"
    in
    check_slice
      ~test_name:"global.get ignores call_indirect that restores same global"
      ~with_pointer_analysis:true
      original slice 1l 2

  let%test "global.get keeps call_indirect to selected target that changes same global" =
    let original =
      "(module
        (type (;0;) (func))
        (type (;1;) (func (result i32)))

        (func (;0;) (type 0)
          i32.const 999
          global.set 0)

        (func (;1;) (type 0)
          i32.const 123
          global.set 1)

        (func (;2;) (type 1) (result i32)
          i32.const 0
          call_indirect (type 0)
          global.get 0)

        (table (;0;) 2 funcref)
        (elem (;0;) (i32.const 0) func 0 1)

        (global (;0;) (mut i32) (i32.const 42))
        (global (;1;) (mut i32) (i32.const 0)))"
    in
    check_slice
      ~test_name:"global.get keeps call_indirect to selected target that changes same global"
      ~with_pointer_analysis:true
      original original 2l 2


  let%test "global.get ignores call_indirect to selected target that changes different global" =
    let original =
      "(module
        (type (;0;) (func))
        (type (;1;) (func (result i32)))

        (func (;0;) (type 0)
          i32.const 123
          global.set 1)

        (func (;1;) (type 0)
          i32.const 999
          global.set 0)

        (func (;2;) (type 1) (result i32)
          i32.const 0
          call_indirect (type 0)
          global.get 0)

        (table (;0;) 2 funcref)
        (elem (;0;) (i32.const 0) func 0 1)

        (global (;0;) (mut i32) (i32.const 42))
        (global (;1;) (mut i32) (i32.const 0)))"
    in
    let slice =
      "(module
        (type (;0;) (func))
        (type (;1;) (func (result i32)))

        (func (;0;) (type 0)
          i32.const 123
          global.set 1)

        (func (;1;) (type 0)
          i32.const 999
          global.set 0)

        (func (;2;) (type 1) (result i32)
          global.get 0)

        (table (;0;) 2 funcref)
        (elem (;0;) (i32.const 0) func 0 1)

        (global (;0;) (mut i32) (i32.const 42))
        (global (;1;) (mut i32) (i32.const 0)))"
    in
    check_slice
      ~test_name:"global.get ignores call_indirect to selected target that changes different global"
      ~with_pointer_analysis:true
      original slice 2l 2

  let%test "global.get keeps call_indirect with unknown target that may change same global" =
    let original =
      "(module
        (type (;0;) (func))
        (type (;1;) (func (param i32) (result i32)))

        (func (;0;) (type 0)
          i32.const 123
          global.set 1)

        (func (;1;) (type 0)
          i32.const 999
          global.set 0)

        (func (;2;) (type 1) (param i32) (result i32)
          local.get 0
          call_indirect (type 0)
          global.get 0)

        (table (;0;) 2 funcref)
        (elem (;0;) (i32.const 0) func 0 1)

        (global (;0;) (mut i32) (i32.const 42))
        (global (;1;) (mut i32) (i32.const 0)))"
    in
    check_slice
      ~test_name:"global.get keeps call_indirect with unknown target that may change same global"
      ~with_pointer_analysis:true
      original original 2l 2


  let%test "load keeps preceding call that may write overlapping memory" =
    let original =
      "(module
        (type (;0;) (func))

        (func (;0;) (type 0)
          i32.const 4
          i32.const 999
          i32.store)

        (func (;1;) (type 0)
          i32.const 100
          i32.const 888
          i32.store)

        (func (;2;) (type 0)
          call 0
          call 1

          i32.const 4
          i32.load

          call 0
          call 1
          drop)

        (memory (;0;) 1))"
    in
    let slice =
      "(module
        (type (;0;) (func))

        (func (;0;) (type 0)
          i32.const 4
          i32.const 999
          i32.store)

        (func (;1;) (type 0)
          i32.const 100
          i32.const 888
          i32.store)

        (func (;2;) (type 0)
          call 0

          i32.const 4
          i32.load

          drop)

        (memory (;0;) 1))"
    in
    check_slice
      ~test_name:"load keeps preceding call that may write overlapping memory"
      ~with_pointer_analysis:true
      original slice 2l 3


  let%test "load ignores preceding call that writes disjoint memory" =
    let original =
      "(module
        (type (;0;) (func))
        (type (;1;) (func))

        (func (;0;) (type 0)
          ;; Writes bytes [100,103]
          i32.const 100
          i32.const 999
          i32.store)

        (func (;1;) (type 1)
          call 0

          i32.const 4
          i32.load

          call 0
          call 0
          drop)

        (memory (;0;) 1))"
    in
    let slice =
      "(module
        (type (;0;) (func))
        (type (;1;) (func))

        (func (;0;) (type 0)
          ;; Writes bytes [100,103]
          i32.const 100
          i32.const 999
          i32.store)

        (func (;1;) (type 1)
          i32.const 4
          i32.load
          drop)

        (memory (;0;) 1))"
    in
    check_slice
      ~test_name:"load ignores preceding call that writes disjoint memory"
      ~with_pointer_analysis:true
      original slice 1l 2


  let%test "load keeps only preceding call that writes overlapping memory" =
    let original =
      "(module
        (type (;0;) (func))

        (func (;0;) (type 0)
          i32.const 100
          i32.const 999
          i32.store)

        (func (;1;) (type 0)
          i32.const 4
          i32.const 888
          i32.store)

        (func (;2;) (type 0)
          call 0
          call 1

          i32.const 4
          i32.load

          call 0
          call 1
          drop)

        (memory (;0;) 1))"
    in
    let slice =
      "(module
        (type (;0;) (func))

        (func (;0;) (type 0)
          i32.const 100
          i32.const 999
          i32.store)

        (func (;1;) (type 0)
          i32.const 4
          i32.const 888
          i32.store)

        (func (;2;) (type 0)
          call 1

          i32.const 4
          i32.load

          drop)

        (memory (;0;) 1))"
    in
    check_slice
      ~test_name:"load keeps only preceding call that writes overlapping memory"
      ~with_pointer_analysis:true
      original slice 2l 3


  let%test "load keeps preceding call that writes overlapping boundary memory" =
    let original =
      "(module
        (type (;0;) (func))

        (func (;0;) (type 0)
          ;; Writes bytes [3, 6], overlapping the load range [4, 7]
          i32.const 3
          i32.const 999
          i32.store)

        (func (;1;) (type 0)
          ;; Writes bytes [100, 103], irrelevant
          i32.const 0
          i32.const 888
          i32.store)

        (func (;2;) (type 0)
          call 0
          call 1

          i32.const 4
          i32.load

          call 0
          call 1
          drop)

        (memory (;0;) 1))"
    in
    let slice =
      "(module
        (type (;0;) (func))

        (func (;0;) (type 0)
          ;; Writes bytes [3, 6], overlapping the load range [4, 7]
          i32.const 3
          i32.const 999
          i32.store)

        (func (;1;) (type 0)
          ;; Writes bytes [100, 103], irrelevant
          i32.const 100
          i32.const 888
          i32.store)

        (func (;2;) (type 0)
          call 0

          i32.const 4
          i32.load
          drop)

        (memory (;0;) 1))"
    in
    check_slice
      ~test_name:"load keeps preceding call that writes overlapping boundary memory"
      ~with_pointer_analysis:true
      original slice 2l 3


  let%test "load keeps preceding call that may write one of several possible addresses" =
    let original =
      "(module
        (type (;0;) (func (param i32)))
        (type (;1;) (func (param i32)))

        (func (;0;) (type 0) (param $c i32)
          local.get $c
          if (result i32)
            i32.const 4
          else
            i32.const 100
          end
          i32.const 999
          i32.store)

        (func (;1;) (type 1) (param $c i32)
          local.get $c
          call 0

          i32.const 4
          i32.load

          local.get $c
          call 0
          drop)

        (memory (;0;) 1))"
    in
    let slice =
      "(module
        (type (;0;) (func (param i32)))
        (type (;1;) (func (param i32)))

        (func (;0;) (type 0) (param $c i32)
          local.get $c
          if (result i32)
            i32.const 4
          else
            i32.const 100
          end
          i32.const 999
          i32.store)

        (func (;1;) (type 1) (param $c i32)
          local.get $c
          call 0

          i32.const 4
          i32.load
          drop)

        (memory (;0;) 1))"
    in
    check_slice
      ~test_name:"load keeps preceding call that may write one of several possible addresses"
      ~with_pointer_analysis:true
      original slice 1l 3


  let%test "load keeps preceding call that writes at parameter address" =
    let original =
      "(module
        (type (;0;) (func (param i32)))
        (type (;1;) (func (param i32)))

        (func (;0;) (type 0) (param $p i32)
          local.get $p
          i32.const 999
          i32.store)

        (func (;1;) (type 1) (param $p i32)
          local.get $p
          call 0

          i32.const 4
          i32.load

          local.get $p
          call 0
          drop)

        (memory (;0;) 1))"
    in
    let slice =
      "(module
        (type (;0;) (func (param i32)))
        (type (;1;) (func (param i32)))

        (func (;0;) (type 0) (param $p i32)
          local.get $p
          i32.const 999
          i32.store)

        (func (;1;) (type 1) (param $p i32)
          local.get $p
          call 0

          i32.const 4
          i32.load

          drop)

        (memory (;0;) 1))"
    in
    check_slice
      ~test_name:"load keeps preceding call that writes at parameter address"
      ~with_pointer_analysis:true
      original slice 1l 3


  let%test "load keeps preceding call when load may read one of several possible addresses" =
    let original =
      "(module
        (type (;0;) (func))
        (type (;1;) (func (param i32)))

        (func (;0;) (type 0)
          i32.const 4
          i32.const 999
          i32.store)

        (func (;1;) (type 1) (param $c i32)
          call 0

          local.get $c
          if (result i32)
            i32.const 4
          else
            i32.const 100
          end
          i32.load

          call 0
          drop)

        (memory (;0;) 1))"
    in
    let slice =
      "(module
        (type (;0;) (func))
        (type (;1;) (func (param i32)))

        (func (;0;) (type 0)
          i32.const 4
          i32.const 999
          i32.store)

        (func (;1;) (type 1) (param $c i32)
          call 0

          local.get $c
          if (result i32)
            i32.const 4
          else
            i32.const 100
          end
          i32.load
          drop)

        (memory (;0;) 1))"
    in
    check_slice
      ~test_name:"load keeps preceding call when load may read one of several possible addresses"
      ~with_pointer_analysis:true
      original slice 1l 5



  let%test "load keeps preceding call when load reads at parameter address" =
    let original =
      "(module
        (type (;0;) (func))
        (type (;1;) (func (param i32)))

        (func (;0;) (type 0)
          i32.const 4
          i32.const 999
          i32.store)

        (func (;1;) (type 1) (param $p i32)
          call 0

          local.get $p
          i32.load

          call 0
          drop)

        (memory (;0;) 1))"
    in
    let slice =
      "(module
        (type (;0;) (func))
        (type (;1;) (func (param i32)))

        (func (;0;) (type 0)
          i32.const 4
          i32.const 999
          i32.store)

        (func (;1;) (type 1) (param $p i32)
          call 0

          local.get $p
          i32.load
          drop)

        (memory (;0;) 1))"
    in
    check_slice
      ~test_name:"load keeps preceding call when load reads at parameter address"
      ~with_pointer_analysis:true
      original slice 1l 2



  let%test "load keeps preceding call that writes at difference of parameter addresses" =
    let original =
      "(module
        (type (;0;) (func (param i32) (param i32)))
        (type (;1;) (func (param i32) (param i32)))

        (func (;0;) (type 0) (param $p i32) (param $q i32)
          local.get $p
          local.get $q
          i32.sub
          i32.const 999
          i32.store)

        (func (;1;) (type 1) (param $p i32) (param $q i32)
          local.get $p
          local.get $q
          call 0

          i32.const 4
          i32.load

          local.get $p
          local.get $q
          call 0
          drop)

        (memory (;0;) 1))"
    in
    let slice =
      "(module
        (type (;0;) (func (param i32) (param i32)))
        (type (;1;) (func (param i32) (param i32)))

        (func (;0;) (type 0) (param $p i32) (param $q i32)
          local.get $p
          local.get $q
          i32.sub
          i32.const 999
          i32.store)

        (func (;1;) (type 1) (param $p i32) (param $q i32)
          local.get $p
          local.get $q
          call 0

          i32.const 4
          i32.load
          drop)

        (memory (;0;) 1))"
    in
    check_slice
      ~test_name:"load keeps preceding call that writes at difference of parameter addresses"
      ~with_pointer_analysis:true
      original slice 1l 4


  let%test "load keeps preceding call whose known arguments write at loaded address" =
    let original =
      "(module
        (type (;0;) (func (param i32) (param i32)))
        (type (;1;) (func))

        (func (;0;) (type 0) (param $p i32) (param $q i32)
          local.get $p
          local.get $q
          i32.sub
          i32.const 999
          i32.store)

        (func (;1;) (type 1)
          i32.const 10
          i32.const 6
          call 0

          i32.const 4
          i32.load

          i32.const 20
          i32.const 16
          call 0
          drop)

        (memory (;0;) 1))"
    in
    let slice =
      "(module
        (type (;0;) (func (param i32) (param i32)))
        (type (;1;) (func))

        (func (;0;) (type 0) (param $p i32) (param $q i32)
          local.get $p
          local.get $q
          i32.sub
          i32.const 999
          i32.store)

        (func (;1;) (type 1)
          i32.const 10
          i32.const 6
          call 0

          i32.const 4
          i32.load
          drop)

        (memory (;0;) 1))"
    in
    check_slice
      ~test_name:"load keeps preceding call whose known arguments write at loaded address"
      ~with_pointer_analysis:true
      original slice 1l 4


  let%test "load keeps preceding call whose known arguments may write near loaded address" =
    let original =
      "(module
        (type (;0;) (func (param i32) (param i32)))
        (type (;1;) (func))

        (func (;0;) (type 0) (param $p i32) (param $q i32)
          local.get $p
          local.get $q
          i32.sub
          i32.const 999
          i32.store)

        (func (;1;) (type 1)
          i32.const 13
          i32.const 3
          call 0

          i32.const 4
          i32.load

          i32.const 20
          i32.const 16
          call 0
          drop)

        (memory (;0;) 1))"
    in
    let slice =
      "(module
        (type (;0;) (func (param i32) (param i32)))
        (type (;1;) (func))

        (func (;0;) (type 0) (param $p i32) (param $q i32)
          local.get $p
          local.get $q
          i32.sub
          i32.const 999
          i32.store)

        (func (;1;) (type 1)
          i32.const 13
          i32.const 3
          call 0

          i32.const 4
          i32.load
          drop)

        (memory (;0;) 1))"
    in
    check_slice
      ~test_name:"load keeps preceding call whose known arguments may write near loaded address"
      ~with_pointer_analysis:true
      original slice 1l 4


  let%test "load ignores preceding call whose known arguments write far from loaded address" =
    let original =
      "(module
        (type (;0;) (func (param i32) (param i32)))
        (type (;1;) (func))

        (func (;0;) (type 0) (param $p i32) (param $q i32)
          local.get $p
          local.get $q
          i32.sub
          i32.const 999
          i32.store)

        (func (;1;) (type 1)
          i32.const 13
          i32.const 2
          call 0

          i32.const 4
          i32.load

          i32.const 20
          i32.const 16
          call 0
          drop)

        (memory (;0;) 1))"
    in
    let slice =
      "(module
        (type (;0;) (func (param i32) (param i32)))
        (type (;1;) (func))

        (func (;0;) (type 0) (param $p i32) (param $q i32)
          local.get $p
          local.get $q
          i32.sub
          i32.const 999
          i32.store)

        (func (;1;) (type 1)
          i32.const 4
          i32.load
          drop)

        (memory (;0;) 1))"
    in
    check_slice
      ~test_name:"load ignores preceding call whose known arguments write far from loaded address"
      ~with_pointer_analysis:true
      original slice 1l 4



  let%test "load keeps preceding call that writes overlapping relative memory" =
    let original =
      "(module
        (type (;0;) (func (param i32)))
        (type (;1;) (func (param i32)))

        (func (;0;) (type 0) (param $p i32)
          local.get $p
          i32.const 3
          i32.add
          i32.const 999
          i32.store)

        (func (;1;) (type 1) (param $p i32)
          local.get $p
          call 0

          local.get $p
          i32.const 4
          i32.add
          i32.load

          local.get $p
          call 0
          drop)

        (memory (;0;) 1))"
    in
    let slice =
      "(module
        (type (;0;) (func (param i32)))
        (type (;1;) (func (param i32)))

        (func (;0;) (type 0) (param $p i32)
          local.get $p
          i32.const 3
          i32.add
          i32.const 999
          i32.store)

        (func (;1;) (type 1) (param $p i32)
          local.get $p
          call 0

          local.get $p
          i32.const 4
          i32.add
          i32.load
          drop)

        (memory (;0;) 1))"
    in
    check_slice
      ~test_name:"load keeps preceding call that writes overlapping relative memory"
      ~with_pointer_analysis:true
      original slice 1l 5


  let%test "load ignores preceding call that writes disjoint relative memory" =
    let original =
      "(module
        (type (;0;) (func (param i32)))
        (type (;1;) (func (param i32)))

        (func (;0;) (type 0) (param $p i32)
          local.get $p
          i32.const 20
          i32.add
          i32.const 999
          i32.store)

        (func (;1;) (type 1) (param $p i32)
          local.get $p
          call 0

          local.get $p
          i32.const 4
          i32.add
          i32.load

          local.get $p
          call 0
          drop)

        (memory (;0;) 1))"
    in
    let slice =
      "(module
        (type (;0;) (func (param i32)))
        (type (;1;) (func (param i32)))

        (func (;0;) (type 0) (param $p i32)
          local.get $p
          i32.const 20
          i32.add
          i32.const 999
          i32.store)

        (func (;1;) (type 1) (param $p i32)
          local.get $p
          i32.const 4
          i32.add
          i32.load
          drop)

        (memory (;0;) 1))"
    in
    check_slice
      ~test_name:"load ignores preceding call that writes disjoint relative memory"
      ~with_pointer_analysis:true
      original slice 1l 5


  let%test "load keeps only preceding call that writes overlapping relative memory" =
    let original =
      "(module
        (type (;0;) (func (param i32)))

        (func (;0;) (type 0) (param $p i32)
          local.get $p
          i32.const 20
          i32.add
          i32.const 111
          i32.store)

        (func (;1;) (type 0) (param $p i32)
          local.get $p
          i32.const 3
          i32.add
          i32.const 222
          i32.store)

        (func (;2;) (type 0) (param $p i32)
          local.get $p
          call 0

          local.get $p
          call 1

          local.get $p
          i32.const 4
          i32.add
          i32.load

          local.get $p
          call 0

          local.get $p
          call 1
          drop)

        (memory (;0;) 1))"
    in
    let slice =
      "(module
        (type (;0;) (func (param i32)))

        (func (;0;) (type 0) (param $p i32)
          local.get $p
          i32.const 20
          i32.add
          i32.const 111
          i32.store)

        (func (;1;) (type 0) (param $p i32)
          local.get $p
          i32.const 3
          i32.add
          i32.const 222
          i32.store)

        (func (;2;) (type 0) (param $p i32)
          local.get $p
          call 1

          local.get $p
          i32.const 4
          i32.add
          i32.load
          drop)

        (memory (;0;) 1))"
    in
    check_slice
      ~test_name:"load keeps only preceding call that writes overlapping relative memory"
      ~with_pointer_analysis:true
      original slice 2l 7


  let%test "load keeps only preceding call that writes overlapping relative memory" =
    let original =
      "(module
        (type (;0;) (func (param i32)))

        (func (;0;) (type 0) (param $p i32)
          local.get $p
          i32.const 20
          i32.add
          i32.const 111
          i32.store)

        (func (;1;) (type 0) (param $p i32)
          local.get $p
          i32.const 3
          i32.add
          i32.const 222
          i32.store)

        (func (;2;) (type 0) (param $p i32)
          local.get $p
          call 0

          local.get $p
          call 1

          local.get $p
          i32.const 4
          i32.add
          i32.load

          local.get $p
          call 0

          local.get $p
          call 1
          drop)

        (memory (;0;) 1))"
    in
    let slice =
      "(module
        (type (;0;) (func (param i32)))

        (func (;0;) (type 0) (param $p i32)
          local.get $p
          i32.const 20
          i32.add
          i32.const 111
          i32.store)

        (func (;1;) (type 0) (param $p i32)
          local.get $p
          i32.const 3
          i32.add
          i32.const 222
          i32.store)

        (func (;2;) (type 0) (param $p i32)
          local.get $p
          call 1

          local.get $p
          i32.const 4
          i32.add
          i32.load
          drop)

        (memory (;0;) 1))"
    in
    check_slice
      ~test_name:"load keeps only preceding call that writes overlapping relative memory"
      ~with_pointer_analysis:true
      original slice 2l 7


  let%test "load keeps preceding call that transitively writes overlapping memory" =
    let original =
      "(module
        (type (;0;) (func))

        (func (;0;) (type 0)
          i32.const 4
          i32.const 999
          i32.store)

        (func (;1;) (type 0)
          call 0)

        (func (;2;) (type 0)
          call 1

          i32.const 4
          i32.load

          call 1
          drop)

        (memory (;0;) 1))"
    in
    let slice =
      "(module
        (type (;0;) (func))

        (func (;0;) (type 0)
          i32.const 4
          i32.const 999
          i32.store)

        (func (;1;) (type 0)
          call 0)

        (func (;2;) (type 0)
          call 1

          i32.const 4
          i32.load
          drop)

        (memory (;0;) 1))"
    in
    check_slice
      ~test_name:"load keeps preceding call that transitively writes overlapping memory"
      ~with_pointer_analysis:true
      original slice 2l 2


  let%test "load ignores preceding call that transitively writes disjoint memory" =
    let original =
      "(module
        (type (;0;) (func))

        (func (;0;) (type 0)
          i32.const 100
          i32.const 999
          i32.store)

        (func (;1;) (type 0)
          call 0)

        (func (;2;) (type 0)
          call 1

          i32.const 4
          i32.load

          call 1
          drop)

        (memory (;0;) 1))"
    in
    let slice =
      "(module
        (type (;0;) (func))

        (func (;0;) (type 0)
          i32.const 100
          i32.const 999
          i32.store)

        (func (;1;) (type 0))

        (func (;2;) (type 0)
          i32.const 4
          i32.load
          drop)

        (memory (;0;) 1))"
    in
    check_slice
      ~test_name:"load ignores preceding call that transitively writes disjoint memory"
      ~with_pointer_analysis:true
      original slice 2l 2


  let%test "load ignores preceding call that transitively writes disjoint memory" =
    let original =
      "(module
        (type (;0;) (func))

        (func (;0;) (type 0)
          i32.const 100
          i32.const 999
          i32.store)

        (func (;1;) (type 0)
          call 0)

        (func (;2;) (type 0)
          call 1

          i32.const 4
          i32.load

          call 1
          drop)

        (memory (;0;) 1))"
    in
    let slice =
      "(module
        (type (;0;) (func))

        (func (;0;) (type 0)
          i32.const 100
          i32.const 999
          i32.store)

        (func (;1;) (type 0))

        (func (;2;) (type 0)
          i32.const 4
          i32.load
          drop)

        (memory (;0;) 1))"
    in
    check_slice
      ~test_name:"load ignores preceding call that transitively writes disjoint memory"
      ~with_pointer_analysis:true
      original slice 2l 2


  let%test "load ignores preceding call that only reads overlapping memory" =
    let original =
      "(module
        (type (;0;) (func))

        (func (;0;) (type 0)
          i32.const 4
          i32.load
          drop)

        (func (;1;) (type 0)
          call 0

          i32.const 4
          i32.load

          call 0
          drop)

        (memory (;0;) 1))"
    in
    let slice =
      "(module
        (type (;0;) (func))

        (func (;0;) (type 0)
          i32.const 4
          i32.load
          drop)

        (func (;1;) (type 0)
          i32.const 4
          i32.load
          drop)

        (memory (;0;) 1))"
    in
    check_slice
      ~test_name:"load ignores preceding call that only reads overlapping memory"
      ~with_pointer_analysis:true
      original slice 1l 2


  let%test "load keeps preceding call that fills overlapping memory" =
    let original =
      "(module
        (type (;0;) (func))

        (func (;0;) (type 0)
          i32.const 4
          i32.const 0
          i32.const 4
          memory.fill)

        (func (;1;) (type 0)
          call 0

          i32.const 4
          i32.load

          call 0
          drop)

        (memory (;0;) 1))"
    in
    let slice =
      "(module
        (type (;0;) (func))

        (func (;0;) (type 0)
          i32.const 4
          i32.const 0
          i32.const 4
          memory.fill)

        (func (;1;) (type 0)
          call 0

          i32.const 4
          i32.load
          drop)

        (memory (;0;) 1))"
    in
    check_slice
      ~test_name:"load keeps preceding call that fills overlapping memory"
      ~with_pointer_analysis:true
      original slice 1l 2


  let%test "i32.load keeps only call whose i64.store overlaps loaded bytes" =
    let original =
      "(module
        (type (;0;) (func))

        (func (;0;) (type 0)
          i32.const 0
          i64.const 111
          i64.store)

        (func (;1;) (type 0)
          i32.const 1
          i64.const 222
          i64.store)

        (func (;2;) (type 0)
          call 0
          call 1

          i32.const 8
          i32.load

          call 0
          call 1
          drop)

        (memory (;0;) 1))"
    in
    let slice =
      "(module
        (type (;0;) (func))

        (func (;0;) (type 0)
          i32.const 0
          i64.const 111
          i64.store)

        (func (;1;) (type 0)
          i32.const 1
          i64.const 222
          i64.store)

        (func (;2;) (type 0)
          call 1

          i32.const 8
          i32.load
          drop)

        (memory (;0;) 1))"
    in
    check_slice
      ~test_name:"i32.load keeps only call whose i64.store overlaps loaded bytes"
      ~with_pointer_analysis:true
      original slice 2l 3


  let%test "load keeps call_indirect to selected target that writes overlapping memory" =
    let original =
      "(module
        (type (;0;) (func))
        (type (;1;) (func))

        (func (;0;) (type 0)
          i32.const 4
          i32.const 999
          i32.store)

        (func (;1;) (type 0)
          i32.const 100
          i32.const 888
          i32.store)

        (func (;2;) (type 1)
          i32.const 0
          call_indirect (type 0)

          i32.const 4
          i32.load

          i32.const 0
          call_indirect (type 0)
          drop)

        (table (;0;) 2 funcref)
        (elem (;0;) (i32.const 0) func 0 1)
        (memory (;0;) 1))"
    in
    let slice =
      "(module
        (type (;0;) (func))
        (type (;1;) (func))

        (func (;0;) (type 0)
          i32.const 4
          i32.const 999
          i32.store)

        (func (;1;) (type 0)
          i32.const 100
          i32.const 888
          i32.store)

        (func (;2;) (type 1)
          i32.const 0
          call_indirect (type 0)

          i32.const 4
          i32.load
          drop)

        (table (;0;) 2 funcref)
        (elem (;0;) (i32.const 0) func 0 1)
        (memory (;0;) 1))"
    in
    check_slice
      ~test_name:"load keeps call_indirect to selected target that writes overlapping memory"
      ~with_pointer_analysis:true
      original slice 2l 3


  let%test "load drops call_indirect to selected target that writes disjoint memory" =
    let original =
      "(module
        (type (;0;) (func))
        (type (;1;) (func))

        (func (;0;) (type 0)
          i32.const 100
          i32.const 999
          i32.store)

        (func (;1;) (type 0)
          i32.const 4
          i32.const 888
          i32.store)

        (func (;2;) (type 1)
          i32.const 0
          call_indirect (type 0)

          i32.const 4
          i32.load
          drop)

        (table (;0;) 2 funcref)
        (elem (;0;) (i32.const 0) func 0 1)
        (memory (;0;) 1))"
    in
    let slice =
      "(module
        (type (;0;) (func))
        (type (;1;) (func))

        (func (;0;) (type 0)
          i32.const 100
          i32.const 999
          i32.store)

        (func (;1;) (type 0)
          i32.const 4
          i32.const 888
          i32.store)

        (func (;2;) (type 1)
          i32.const 4
          i32.load
          drop)

        (table (;0;) 2 funcref)
        (elem (;0;) (i32.const 0) func 0 1)
        (memory (;0;) 1))"
    in
    check_slice
      ~test_name:"load drops call_indirect to selected target that writes disjoint memory"
      ~with_pointer_analysis:true
      original slice 2l 3


  let%test "load keeps call_indirect with unknown target when one possible target writes overlapping memory" =
    let original =
      "(module
        (type (;0;) (func))
        (type (;1;) (func (param i32)))

        (func (;0;) (type 0)
          i32.const 100
          i32.const 111
          i32.store)

        (func (;1;) (type 0)
          i32.const 4
          i32.const 222
          i32.store)

        (func (;2;) (type 1) (param $target i32)
          local.get $target
          call_indirect (type 0)

          i32.const 4
          i32.load
          drop)

        (table (;0;) 2 funcref)
        (elem (;0;) (i32.const 0) func 0 1)
        (memory (;0;) 1))"
    in
    let slice =
      original
    in
    check_slice
      ~test_name:"load keeps call_indirect with unknown target when one possible target writes overlapping memory"
      ~with_pointer_analysis:true
      original slice 2l 3




  let%test "load drops call_indirect with unknown target when no possible target writes overlapping memory" =
    let original =
      "(module
        (type (;0;) (func))
        (type (;1;) (func (param i32)))

        (func (;0;) (type 0)
          i32.const 100
          i32.const 111
          i32.store)

        (func (;1;) (type 0)
          i32.const 200
          i32.const 222
          i32.store)

        (func (;2;) (type 1) (param $target i32)
          local.get $target
          call_indirect (type 0)

          i32.const 4
          i32.load
          drop)

        (table (;0;) 2 funcref)
        (elem (;0;) (i32.const 0) func 0 1)
        (memory (;0;) 1))"
    in
    let slice =
      "(module
        (type (;0;) (func))
        (type (;1;) (func (param i32)))

        (func (;0;) (type 0)
          i32.const 100
          i32.const 111
          i32.store)

        (func (;1;) (type 0)
          i32.const 200
          i32.const 222
          i32.store)

        (func (;2;) (type 1) (param $target i32)
          i32.const 4
          i32.load
          drop)

        (table (;0;) 2 funcref)
        (elem (;0;) (i32.const 0) func 0 1)
        (memory (;0;) 1))"
    in
    check_slice
      ~test_name:"load drops call_indirect with unknown target when no possible target writes overlapping memory"
      ~with_pointer_analysis:true
      original slice 2l 3


  let%test "load keeps call_indirect to selected target that transitively writes overlapping memory" =
    let original =
      "(module
        (type (;0;) (func))
        (type (;1;) (func))

        (func (;0;) (type 0)
          i32.const 4
          i32.const 999
          i32.store)

        (func (;1;) (type 0)
          call 0)

        (func (;2;) (type 1)
          i32.const 0
          call_indirect (type 0)

          i32.const 4
          i32.load

          i32.const 0
          call_indirect (type 0)
          drop)

        (table (;0;) 1 funcref)
        (elem (;0;) (i32.const 0) func 1)
        (memory (;0;) 1))"
    in
    let slice =
      "(module
        (type (;0;) (func))
        (type (;1;) (func))

        (func (;0;) (type 0)
          i32.const 4
          i32.const 999
          i32.store)

        (func (;1;) (type 0)
          call 0)

        (func (;2;) (type 1)
          i32.const 0
          call_indirect (type 0)

          i32.const 4
          i32.load
          drop)

        (table (;0;) 1 funcref)
        (elem (;0;) (i32.const 0) func 1)
        (memory (;0;) 1))"
    in
    check_slice
      ~test_name:"load keeps call_indirect to selected target that transitively writes overlapping memory"
      ~with_pointer_analysis:true
      original slice 2l 3


  let%test "load ignores later call_indirect in non-loop block" =
    let original =
      "(module
        (type (;0;) (func))
        (type (;1;) (func))

        (func (;0;) (type 0)
          i32.const 4
          i32.const 999
          i32.store)

        (func (;1;) (type 1)
          i32.const 4
          i32.load

          i32.const 0
          call_indirect (type 0)
          drop)

        (table (;0;) 1 funcref)
        (elem (;0;) (i32.const 0) func 0)
        (memory (;0;) 1))"
    in
    let slice =
      "(module
        (type (;0;) (func))
        (type (;1;) (func))

        (func (;0;) (type 0))

        (func (;1;) (type 0)
          i32.const 4
          i32.load
          drop)

        (table (;0;) 1 funcref)
        (elem (;0;) (i32.const 0) func 0)
        (memory (;0;) 1))"
    in
    check_slice
      ~test_name:"load ignores later call_indirect in non-loop block"
      ~with_pointer_analysis:true
      original slice 1l 1


  let%test "call depends on previous global.set when callee reads same global" =
    let original =
      "(module
        (type (;0;) (func))
        (type (;1;) (func (result i32)))

        (func (;0;) (type 1)
          global.get 0)

        (func (;1;) (type 0)
          i32.const 42
          global.set 0

          call 0
          drop)

        (global (;0;) (mut i32) (i32.const 0)))"
    in
    let slice = original in
    check_slice
      ~test_name:"call depends on previous global.set when callee reads same global"
      ~with_pointer_analysis:true
      original slice 1l 2


  let%test "call does not depend on previous global.set when callee reads no global" =
    let original =
      "(module
        (type (;0;) (func))

        (func (;0;) (type 0))

        (func (;1;) (type 0)
          i32.const 42
          global.set 0

          call 0)

        (global (;0;) (mut i32) (i32.const 0)))"
    in
    let slice =
      "(module
        (type (;0;) (func))

        (func (;0;) (type 0))

        (func (;1;) (type 0)
          call 0)

        (global (;0;) (mut i32) (i32.const 0)))"
    in
    check_slice
      ~test_name:"call does not depend on previous global.set when callee reads no global"
      ~with_pointer_analysis:true
      original slice 1l 2


  let%test "call does not depend on previous global.set when callee reads different global" =
    let original =
      "(module
        (type (;0;) (func))
        (type (;1;) (func (result i32)))

        (func (;0;) (type 1)
          global.get 1)

        (func (;1;) (type 0)
          i32.const 42
          global.set 0

          call 0
          drop)

        (global (;0;) (mut i32) (i32.const 0))
        (global (;1;) (mut i32) (i32.const 0)))"
    in
    let slice =
      "(module
        (type (;0;) (func))
        (type (;1;) (func (result i32)))

        (func (;0;) (type 1)
          global.get 1)

        (func (;1;) (type 0)
          call 0
          drop)

        (global (;0;) (mut i32) (i32.const 0))
        (global (;1;) (mut i32) (i32.const 0)))"
    in
    check_slice
      ~test_name:"call does not depend on previous global.set when callee reads different global"
      ~with_pointer_analysis:true
      original slice 1l 2


  let%test "call does not depend on previous global.set of unused global" =
    let original =
      "(module
        (type (;0;) (func))
        (type (;1;) (func (result i32)))

        (func (;0;) (type 1)
          global.get 0)

        (func (;1;) (type 0)
          i32.const 42
          global.set 1

          call 0
          drop)

        (global (;0;) (mut i32) (i32.const 0))
        (global (;1;) (mut i32) (i32.const 0)))"
    in
    let slice =
      "(module
        (type (;0;) (func))
        (type (;1;) (func (result i32)))

        (func (;0;) (type 1)
          global.get 0)

        (func (;1;) (type 0)
          call 0
          drop)

        (global (;0;) (mut i32) (i32.const 0))
        (global (;1;) (mut i32) (i32.const 0)))"
    in
    check_slice
      ~test_name:"call does not depend on previous global.set of unused global"
      ~with_pointer_analysis:true
      original slice 1l 2


  let%test "call depends on previous global.set when callee transitively reads same global" =
    let original =
      "(module
        (type (;0;) (func))
        (type (;1;) (func (result i32)))

        (func (;0;) (type 1)
          global.get 0)

        (func (;1;) (type 1)
          call 0)

        (func (;2;) (type 0)
          i32.const 42
          global.set 0

          call 1
          drop)

        (global (;0;) (mut i32) (i32.const 0)))"
    in
    let slice = original in
    check_slice
      ~test_name:"call depends on previous global.set when callee transitively reads same global"
      ~with_pointer_analysis:true
      original slice 2l 2
  

  let%test "call does not depend on later global.set even when callee reads same global" =
    let original =
      "(module
        (type (;0;) (func))
        (type (;1;) (func (result i32)))

        (func (;0;) (type 1)
          global.get 0)

        (func (;1;) (type 0)
          call 0
          drop

          i32.const 42
          global.set 0

          i32.const 13
          global.set 1)

        (global (;0;) (mut i32) (i32.const 0))
        (global (;1;) (mut i32) (i32.const 0)))"
    in
    let slice =
      "(module
        (type (;0;) (func))
        (type (;1;) (func (result i32)))

        (func (;0;) (type 1)
          global.get 0)

        (func (;1;) (type 0)
          call 0
          drop)

        (global (;0;) (mut i32) (i32.const 0))
        (global (;1;) (mut i32) (i32.const 0)))"
    in
    check_slice
      ~test_name:"call does not depend on later global.set even when callee reads same global"
      ~with_pointer_analysis:true
      original slice 1l 0


  let%test "call_indirect depends on previous global.set when selected target reads same global" =
    let original =
      "(module
        (type (;0;) (func))
        (type (;1;) (func (result i32)))

        (func (;0;) (type 1)
          global.get 0)

        (func (;1;) (type 0)
          i32.const 42
          global.set 0

          i32.const 0
          call_indirect (type 1)
          drop)

        (table (;0;) 1 funcref)
        (elem (;0;) (i32.const 0) func 0)

        (global (;0;) (mut i32) (i32.const 0)))"
    in
    let slice = original in
    check_slice
      ~test_name:"call_indirect depends on previous global.set when selected target reads same global"
      ~with_pointer_analysis:true
      original slice 1l 3


  let%test "call_indirect does not depend on previous global.set when selected target reads no global" =
    let original =
      "(module
        (type (;0;) (func))

        (func (;0;) (type 0))

        (func (;1;) (type 0)
          i32.const 42
          global.set 0

          i32.const 0
          call_indirect (type 0))

        (table (;0;) 1 funcref)
        (elem (;0;) (i32.const 0) func 0)

        (global (;0;) (mut i32) (i32.const 0)))"
    in
    let slice =
      "(module
        (type (;0;) (func))

        (func (;0;) (type 0))

        (func (;1;) (type 0)
          i32.const 0
          call_indirect (type 0))

        (table (;0;) 1 funcref)
        (elem (;0;) (i32.const 0) func 0)

        (global (;0;) (mut i32) (i32.const 0)))"
    in
    check_slice
      ~test_name:"call_indirect does not depend on previous global.set when selected target reads no global"
      ~with_pointer_analysis:true
      original slice 1l 3



  let%test "call_indirect does not depend on previous global.set when selected target reads different global" =
    let original =
      "(module
        (type (;0;) (func))
        (type (;1;) (func (result i32)))

        (func (;0;) (type 1)
          global.get 1)

        (func (;1;) (type 0)
          i32.const 42
          global.set 0

          i32.const 0
          call_indirect (type 1)
          drop)

        (table (;0;) 1 funcref)
        (elem (;0;) (i32.const 0) func 0)

        (global (;0;) (mut i32) (i32.const 0))
        (global (;1;) (mut i32) (i32.const 0)))"
    in
    let slice =
      "(module
        (type (;0;) (func))
        (type (;1;) (func (result i32)))

        (func (;0;) (type 1)
          global.get 1)

        (func (;1;) (type 0)
          i32.const 0
          call_indirect (type 1)
          drop)

        (table (;0;) 1 funcref)
        (elem (;0;) (i32.const 0) func 0)

        (global (;0;) (mut i32) (i32.const 0))
        (global (;1;) (mut i32) (i32.const 0)))"
    in
    check_slice
      ~test_name:"call_indirect does not depend on previous global.set when selected target reads different global"
      ~with_pointer_analysis:true
      original slice 1l 3



  let%test "call_indirect does not depend on previous global.set of unused global" =
    let original =
      "(module
        (type (;0;) (func))
        (type (;1;) (func (result i32)))

        (func (;0;) (type 1)
          global.get 0)

        (func (;1;) (type 0)
          i32.const 42
          global.set 1

          i32.const 0
          call_indirect (type 1)
          drop)

        (table (;0;) 1 funcref)
        (elem (;0;) (i32.const 0) func 0)

        (global (;0;) (mut i32) (i32.const 0))
        (global (;1;) (mut i32) (i32.const 0)))"
    in
    let slice =
      "(module
        (type (;0;) (func))
        (type (;1;) (func (result i32)))

        (func (;0;) (type 1)
          global.get 0)

        (func (;1;) (type 0)
          i32.const 0
          call_indirect (type 1)
          drop)

        (table (;0;) 1 funcref)
        (elem (;0;) (i32.const 0) func 0)

        (global (;0;) (mut i32) (i32.const 0))
        (global (;1;) (mut i32) (i32.const 0)))"
    in
    check_slice
      ~test_name:"call_indirect does not depend on previous global.set of unused global"
      ~with_pointer_analysis:true
      original slice 1l 3


  let%test "call_indirect depends on previous global.set when selected target transitively reads same global" =
    let original =
      "(module
        (type (;0;) (func))
        (type (;1;) (func (result i32)))

        (func (;0;) (type 1)
          global.get 0)

        (func (;1;) (type 1)
          call 0)

        (func (;2;) (type 0)
          i32.const 42
          global.set 0

          i32.const 0
          call_indirect (type 1)
          drop)

        (table (;0;) 1 funcref)
        (elem (;0;) (i32.const 0) func 1)

        (global (;0;) (mut i32) (i32.const 0)))"
    in
    let slice = original in
    check_slice
      ~test_name:"call_indirect depends on previous global.set when selected target transitively reads same global"
      ~with_pointer_analysis:true
      original slice 2l 3


  let%test "call_indirect depends on previous global.set when parameter target may read same global" =
    let original =
      "(module
        (type (;0;) (func))
        (type (;1;) (func (result i32)))
        (type (;2;) (func (param i32)))

        (func (;0;) (type 1)
          global.get 0)

        (func (;1;) (type 1)
          i32.const 0)

        (func (;2;) (type 2) (param $target i32)
          i32.const 42
          global.set 0

          local.get $target
          call_indirect (type 1)
          drop)

        (table (;0;) 2 funcref)
        (elem (;0;) (i32.const 0) func 0 1)

        (global (;0;) (mut i32) (i32.const 0)))"
    in
    let slice = original in
    check_slice
      ~test_name:"call_indirect depends on previous global.set when parameter target may read same global"
      ~with_pointer_analysis:true
      original slice 2l 3



  let%test "call_indirect depends on previous global.set when value-set target may read same global" =
    let original =
      "(module
        (type (;0;) (func))
        (type (;1;) (func (result i32)))
        (type (;2;) (func (param i32)))

        (func (;0;) (type 1)
          global.get 0)

        (func (;1;) (type 1)
          i32.const 0)

        (func (;2;) (type 2) (param $c i32)
          i32.const 42
          global.set 0

          local.get $c
          if (result i32)
            i32.const 0
          else
            i32.const 1
          end
          call_indirect (type 1)
          drop)

        (table (;0;) 2 funcref)
        (elem (;0;) (i32.const 0) func 0 1)

        (global (;0;) (mut i32) (i32.const 0)))"
    in
    let slice = original in
    check_slice
      ~test_name:"call_indirect depends on previous global.set when value-set target may read same global"
      ~with_pointer_analysis:true
      original slice 2l 6


  let%test "call_indirect depends on previous global.set when selected table target reads same global" =
    let original =
      "(module
        (type (;0;) (func))
        (type (;1;) (func (result i32)))

        (func (;0;) (type 1)
          global.get 0)

        (func (;1;) (type 1)
          global.get 1)

        (func (;2;) (type 0)
          i32.const 42
          global.set 0

          i32.const 0
          call_indirect (type 1)
          drop)

        (table (;0;) 2 funcref)
        (elem (;0;) (i32.const 0) func 0 1)

        (global (;0;) (mut i32) (i32.const 0))
        (global (;1;) (mut i32) (i32.const 0)))"
    in
    let slice = original in
    check_slice
      ~test_name:"call_indirect depends on previous global.set when selected table target reads same global"
      ~with_pointer_analysis:true
      original slice 2l 3


  let%test "call_indirect does not depend on previous global.set when selected table target reads different global" =
    let original =
      "(module
        (type (;0;) (func))
        (type (;1;) (func (result i32)))

        (func (;0;) (type 1)
          global.get 0)

        (func (;1;) (type 1)
          global.get 1)

        (func (;2;) (type 0)
          i32.const 42
          global.set 0

          i32.const 1
          call_indirect (type 1)
          drop)

        (table (;0;) 2 funcref)
        (elem (;0;) (i32.const 0) func 0 1)

        (global (;0;) (mut i32) (i32.const 0))
        (global (;1;) (mut i32) (i32.const 0)))"
    in
    let slice =
      "(module
        (type (;0;) (func))
        (type (;1;) (func (result i32)))

        (func (;0;) (type 1)
          global.get 0)

        (func (;1;) (type 1)
          global.get 1)

        (func (;2;) (type 0)
          i32.const 1
          call_indirect (type 1)
          drop)

        (table (;0;) 2 funcref)
        (elem (;0;) (i32.const 0) func 0 1)

        (global (;0;) (mut i32) (i32.const 0))
        (global (;1;) (mut i32) (i32.const 0)))"
    in
    check_slice
      ~test_name:"call_indirect does not depend on previous global.set when selected table target reads different global"
      ~with_pointer_analysis:true
      original slice 2l 3


  let%test "call depends on previous store when callee loads same address" =
    let original =
      "(module
        (type (;0;) (func))
        (type (;1;) (func (result i32)))

        (func (;0;) (type 1)
          i32.const 4
          i32.load)

        (func (;1;) (type 0)
          i32.const 4
          i32.const 42
          i32.store

          call 0
          drop)

        (memory (;0;) 1))"
    in
    let slice = original in
    check_slice
      ~test_name:"call depends on previous store when callee loads same address"
      ~with_pointer_analysis:true
      original slice 1l 3


  let%test "call depends on previous store when callee load overlaps one byte" =
    let original =
      "(module
        (type (;0;) (func))
        (type (;1;) (func (result i32)))

        (func (;0;) (type 1)
          i32.const 3
          i32.load)

        (func (;1;) (type 0)
          i32.const 0
          i32.const 42
          i32.store

          call 0
          drop)

        (memory (;0;) 1))"
    in
    let slice = original in
    check_slice
      ~test_name:"call depends on previous store when callee load overlaps one byte"
      ~with_pointer_analysis:true
      original slice 1l 3


  let%test "call does not depend on previous store when callee load is disjoint" =
    let original =
      "(module
        (type (;0;) (func))
        (type (;1;) (func (result i32)))

        (func (;0;) (type 1)
          i32.const 4
          i32.load)

        (func (;1;) (type 0)
          i32.const 0
          i32.const 42
          i32.store

          call 0
          drop)

        (memory (;0;) 1))"
    in
    let slice =
      "(module
        (type (;0;) (func))
        (type (;1;) (func (result i32)))

        (func (;0;) (type 1)
          i32.const 8
          i32.load)

        (func (;1;) (type 0)
          call 0
          drop)

        (memory (;0;) 1))"
    in
    check_slice
      ~test_name:"call does not depend on previous store when callee load is disjoint"
      ~with_pointer_analysis:true
      original slice 1l 3


  let%test "call depends on previous store when callee transitively loads affected address" =
    let original =
      "(module
        (type (;0;) (func))
        (type (;1;) (func (result i32)))

        (func (;0;) (type 1)
          i32.const 4
          i32.load)

        (func (;1;) (type 1)
          call 0)

        (func (;2;) (type 0)
          i32.const 4
          i32.const 42
          i32.store

          call 1
          drop)

        (memory (;0;) 1))"
    in
    let slice = original in
    check_slice
      ~test_name:"call depends on previous store when callee transitively loads affected address"
      ~with_pointer_analysis:true
      original slice 2l 3


  let%test "call keeps previous store with incompatible relative offset" =
    let original =
      "(module
        (type (;0;) (func (result i32)))
        (type (;1;) (func (param i32)))

        (func (;0;) (type 0)
          i32.const 4
          i32.load)

        (func (;1;) (type 1) (param $p i32)
          local.get $p
          i32.const 42
          i32.store

          call 0
          drop)

        (memory (;0;) 1))"
    in
    let slice = original in
    check_slice
      ~test_name:"call keeps previous store with incompatible relative offset"
      ~with_pointer_analysis:true
      original slice 1l 3


  let%test "call drops previous store with compatible disjoint relative offset" =
    let original =
      "(module
        (func (;0;) (param $p i32) (result i32)
          local.get $p
          i32.const 8
          i32.add
          i32.load)

        (func (;1;) (param $p i32)
          local.get $p
          i32.const 42
          i32.store

          local.get $p
          call 0
          drop)

        (memory (;0;) 1))"
    in
    let slice =
      "(module
        (func (;0;) (param $p i32) (result i32)
          local.get $p
          i32.const 8
          i32.add
          i32.load)

        (func (;1;) (param $p i32)
          local.get $p
          call 0
          drop)

        (memory (;0;) 1))"
    in
    check_slice
      ~test_name:"call drops previous store with compatible disjoint relative offset"
      ~with_pointer_analysis:true
      original slice 1l 4



  let%test "call does not depend on later store even when callee load overlaps" =
    let original =
      "(module
        (type (;0;) (func))
        (type (;1;) (func (result i32)))

        (func (;0;) (type 1)
          i32.const 4
          i32.load)

        (func (;1;) (type 0)
          call 0
          drop

          i32.const 4
          i32.const 42
          i32.store)

        (memory (;0;) 1))"
    in
    let slice =
      "(module
        (type (;0;) (func))
        (type (;1;) (func (result i32)))

        (func (;0;) (type 1)
          i32.const 4
          i32.load)

        (func (;1;) (type 0)
          call 0
          drop)

        (memory (;0;) 1))"
    in
    check_slice
      ~test_name:"call does not depend on later store even when callee load overlaps"
      ~with_pointer_analysis:true
      original slice 1l 0


  let%test "call_indirect depends on previous store when selected target loads same address" =
    let original =
      "(module
        (type (;0;) (func))
        (type (;1;) (func (result i32)))

        (func (;0;) (type 1)
          i32.const 4
          i32.load)

        (func (;1;) (type 1)
          i32.const 0)

        (func (;2;) (type 0)
          i32.const 4
          i32.const 42
          i32.store

          i32.const 0
          call_indirect (type 1)
          drop)

        (table (;0;) 2 funcref)
        (elem (;0;) (i32.const 0) func 0 1)
        (memory (;0;) 1))"
    in
    let slice = original in
    check_slice
      ~test_name:"call_indirect depends on previous store when selected target loads same address"
      ~with_pointer_analysis:true
      original slice 2l 4



  let%test "call_indirect depends on previous store when selected target load overlaps one byte" =
    let original =
      "(module
        (type (;0;) (func))
        (type (;1;) (func (result i32)))

        (func (;0;) (type 1)
          i32.const 3
          i32.load)

        (func (;1;) (type 1)
          i32.const 0)

        (func (;2;) (type 0)
          i32.const 0
          i32.const 42
          i32.store

          i32.const 0
          call_indirect (type 1)
          drop)

        (table (;0;) 2 funcref)
        (elem (;0;) (i32.const 0) func 0 1)
        (memory (;0;) 1))"
    in
    let slice = original in
    check_slice
      ~test_name:"call_indirect depends on previous store when selected target load overlaps one byte"
      ~with_pointer_analysis:true
      original slice 2l 4


  let%test "call_indirect does not depend on previous store when selected target load is disjoint" =
    let original =
      "(module
        (type (;0;) (func))
        (type (;1;) (func (result i32)))

        (func (;0;) (type 1)
          i32.const 8
          i32.load)

        (func (;1;) (type 1)
          i32.const 0)

        (func (;2;) (type 0)
          i32.const 0
          i32.const 42
          i32.store

          i32.const 0
          call_indirect (type 1)
          drop)

        (table (;0;) 2 funcref)
        (elem (;0;) (i32.const 0) func 0 1)
        (memory (;0;) 1))"
    in
    let slice =
      "(module
        (type (;0;) (func))
        (type (;1;) (func (result i32)))

        (func (;0;) (type 1)
          i32.const 8
          i32.load)

        (func (;1;) (type 1)
          i32.const 0)

        (func (;2;) (type 0)
          i32.const 0
          call_indirect (type 1)
          drop)

        (table (;0;) 2 funcref)
        (elem (;0;) (i32.const 0) func 0 1)
        (memory (;0;) 1))"
    in
    check_slice
      ~test_name:"call_indirect does not depend on previous store when selected target load is disjoint"
      ~with_pointer_analysis:true
      original slice 2l 4


  let%test "call_indirect depends on previous store when selected target transitively loads affected address" =
    let original =
      "(module
        (type (;0;) (func))
        (type (;1;) (func (result i32)))

        (func (;0;) (type 1)
          i32.const 4
          i32.load)

        (func (;1;) (type 1)
          call 0)

        (func (;2;) (type 1)
          i32.const 0)

        (func (;3;) (type 0)
          i32.const 4
          i32.const 42
          i32.store

          i32.const 0
          call_indirect (type 1)
          drop)

        (table (;0;) 2 funcref)
        (elem (;0;) (i32.const 0) func 1 2)
        (memory (;0;) 1))"
    in
    let slice = original in
    check_slice
      ~test_name:"call_indirect depends on previous store when selected target transitively loads affected address"
      ~with_pointer_analysis:true
      original slice 3l 4


  let%test "call_indirect keeps previous store with incompatible relative offset" =
    let original =
      "(module
        (type (;0;) (func (result i32)))
        (type (;1;) (func (param i32)))
        (type (;2;) (func))

        (func (;0;) (type 0)
          i32.const 4
          i32.load)

        (func (;1;) (type 0)
          i32.const 0)

        (func (;2;) (type 1) (param $p i32)
          local.get $p
          i32.const 42
          i32.store

          i32.const 0
          call_indirect (type 0)
          drop)

        (table (;0;) 2 funcref)
        (elem (;0;) (i32.const 0) func 0 1)
        (memory (;0;) 1))"
    in
    let slice = original in
    check_slice
      ~test_name:"call_indirect keeps previous store with incompatible relative offset"
      ~with_pointer_analysis:true
      original slice 2l 4


  let%test "call_indirect depends on previous store when selected same-typed target loads same address" =
    let original =
      "(module
        (type (;0;) (func (result i32)))
        (type (;1;) (func))

        (func (;0;) (type 0)
          i32.const 4
          i32.load)

        (func (;1;) (type 0)
          i32.const 0)

        (func (;2;) (type 1)
          i32.const 4
          i32.const 42
          i32.store

          i32.const 0
          call_indirect (type 0)
          drop)

        (table (;0;) 2 funcref)
        (elem (;0;) (i32.const 0) func 0 1)
        (memory (;0;) 1))"
    in
    let slice = original in
    check_slice
      ~test_name:"call_indirect depends on previous store when selected same-typed target loads same address"
      ~with_pointer_analysis:true
      original slice 2l 4


  let%test "call_indirect depends on previous store when selected same-typed target load overlaps one byte" =
    let original =
      "(module
        (type (;0;) (func (result i32)))
        (type (;1;) (func))

        (func (;0;) (type 0)
          i32.const 3
          i32.load)

        (func (;1;) (type 0)
          i32.const 0)

        (func (;2;) (type 1)
          i32.const 0
          i32.const 42
          i32.store

          i32.const 0
          call_indirect (type 0)
          drop)

        (table (;0;) 2 funcref)
        (elem (;0;) (i32.const 0) func 0 1)
        (memory (;0;) 1))"
    in
    let slice = original in
    check_slice
      ~test_name:"call_indirect depends on previous store when selected same-typed target load overlaps one byte"
      ~with_pointer_analysis:true
      original slice 2l 4


  let%test "call_indirect does not depend on previous store when selected same-typed target load is disjoint" =
    let original =
      "(module
        (type (;0;) (func (result i32)))
        (type (;1;) (func))

        (func (;0;) (type 0)
          i32.const 8
          i32.load)

        (func (;1;) (type 0)
          i32.const 0)

        (func (;2;) (type 1)
          i32.const 0
          i32.const 42
          i32.store

          i32.const 0
          call_indirect (type 0)
          drop)

        (table (;0;) 2 funcref)
        (elem (;0;) (i32.const 0) func 0 1)
        (memory (;0;) 1))"
    in
    let slice =
      "(module
        (type (;0;) (func (result i32)))
        (type (;1;) (func))

        (func (;0;) (type 0)
          i32.const 8
          i32.load)

        (func (;1;) (type 0)
          i32.const 0)

        (func (;2;) (type 1)
          i32.const 0
          call_indirect (type 0)
          drop)

        (table (;0;) 2 funcref)
        (elem (;0;) (i32.const 0) func 0 1)
        (memory (;0;) 1))"
    in
    check_slice
      ~test_name:"call_indirect does not depend on previous store when selected same-typed target load is disjoint"
      ~with_pointer_analysis:true
      original slice 2l 4


  let%test "call_indirect depends on previous store when selected same-typed target transitively loads affected address" =
    let original =
      "(module
        (type (;0;) (func (result i32)))
        (type (;1;) (func))

        (func (;0;) (type 0)
          i32.const 4
          i32.load)

        (func (;1;) (type 0)
          call 0)

        (func (;2;) (type 0)
          i32.const 0)

        (func (;3;) (type 1)
          i32.const 4
          i32.const 42
          i32.store

          i32.const 0
          call_indirect (type 0)
          drop)

        (table (;0;) 2 funcref)
        (elem (;0;) (i32.const 0) func 1 2)
        (memory (;0;) 1))"
    in
    let slice = original in
    check_slice
      ~test_name:"call_indirect depends on previous store when selected same-typed target transitively loads affected address"
      ~with_pointer_analysis:true
      original slice 3l 4


  let%test "call_indirect keeps previous store with incompatible relative offset and same-typed targets" =
    let original =
      "(module
        (type (;0;) (func (result i32)))
        (type (;1;) (func (param i32)))

        (func (;0;) (type 0)
          i32.const 4
          i32.load)

        (func (;1;) (type 0)
          i32.const 0)

        (func (;2;) (type 1) (param $p i32)
          local.get $p
          i32.const 42
          i32.store

          i32.const 0
          call_indirect (type 0)
          drop)

        (table (;0;) 2 funcref)
        (elem (;0;) (i32.const 0) func 0 1)
        (memory (;0;) 1))"
    in
    let slice = original in
    check_slice
      ~test_name:"call_indirect keeps previous store with incompatible relative offset and same-typed targets"
      ~with_pointer_analysis:true
      original slice 2l 4


  let%test "call_indirect drops previous store with compatible disjoint relative offset and same-typed targets" =
    let original =
      "(module
        (type (;0;) (func (param i32) (result i32)))
        (type (;1;) (func (param i32)))

        (func (;0;) (param $p i32) (result i32)
          local.get $p
          i32.const 8
          i32.add
          i32.load)

        (func (;1;) (param $p i32) (result i32)
          i32.const 0)

        (func (;2;) (param $p i32)
          local.get $p
          i32.const 42
          i32.store

          local.get $p
          i32.const 0
          call_indirect (type 0)
          drop)

        (table (;0;) 2 funcref)
        (elem (;0;) (i32.const 0) func 0 1)
        (memory (;0;) 1))"
    in
    let slice =
      "(module
        (type (;0;) (func (param i32) (result i32)))
        (type (;1;) (func (param i32)))

        (func (;0;) (param $p i32) (result i32)
          local.get $p
          i32.const 8
          i32.add
          i32.load)

        (func (;1;) (param $p i32) (result i32)
          i32.const 0)

        (func (;2;) (param $p i32)
          local.get $p
          i32.const 0
          call_indirect (type 0)
          drop)

        (table (;0;) 2 funcref)
        (elem (;0;) (i32.const 0) func 0 1)
        (memory (;0;) 1))"
    in
    check_slice
      ~test_name:"call_indirect drops previous store with compatible disjoint relative offset and same-typed targets"
      ~with_pointer_analysis:true
      original slice 2l 5


  let%test "call_indirect does not depend on later store even when selected same-typed target load overlaps" =
    let original =
      "(module
        (type (;0;) (func (result i32)))
        (type (;1;) (func))

        (func (;0;) (type 0)
          i32.const 4
          i32.load)

        (func (;1;) (type 0)
          i32.const 0)

        (func (;2;) (type 1)
          i32.const 0
          call_indirect (type 0)
          drop

          i32.const 4
          i32.const 42
          i32.store)

        (table (;0;) 2 funcref)
        (elem (;0;) (i32.const 0) func 0 1)
        (memory (;0;) 1))"
    in
    let slice =
      "(module
        (type (;0;) (func (result i32)))
        (type (;1;) (func))

        (func (;0;) (type 0)
          i32.const 4
          i32.load)

        (func (;1;) (type 0)
          i32.const 0)

        (func (;2;) (type 1)
          i32.const 0
          call_indirect (type 0)
          drop)

        (table (;0;) 2 funcref)
        (elem (;0;) (i32.const 0) func 0 1)
        (memory (;0;) 1))"
    in
    check_slice
      ~test_name:"call_indirect does not depend on later store even when selected same-typed target load overlaps"
      ~with_pointer_analysis:true
      original slice 2l 1
end
