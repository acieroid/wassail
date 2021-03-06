open Core_kernel
open Helpers

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
      Log.info (Printf.sprintf "instr %s is part of the slice due to %s\n"
        (Instr.Label.to_string label)
        (match var with
         | Some v -> Var.to_string v
         | None -> "no var"));
      { label ;
        reason = match Cfg.find_instr instructions label with
          | Some (Instr.Control { instr = Merge; _ }) -> var
          | Some _ -> None
          | None -> failwith "Did not find the instruction! Should not happen"
      }
  end
  module Set = Set.Make(T)
  include T
end

(** Identify instructions to keep in a backwards slice on `cfg`, using the
    slicing criterion `criterion`, encoded as an instruction index. Returns the
    set of instructions that are part of the slice, as a set of instruction
    labels. *)
let instructions_to_keep (cfg : Spec.t Cfg.t) (cfg_instructions : Spec.t Instr.t Instr.Label.Map.t) (criterion : Instr.Label.t) : (Instr.Label.Set.t * Time.Span.t * Time.Span.t) =
  let t0 = Time.now () in
  let control_dependencies = Control_deps.control_deps_exact_instrs cfg in
  let (_, _, data_dependencies) = Use_def.make cfg in
  let mem_dependencies = Memory_deps.make cfg in
  let global_set_instructions = InSlice.Set.of_list (List.map ~f:(fun label -> InSlice.{ label; reason = None })
                                                       (Instr.Label.Map.keys (Instr.Label.Map.filter cfg_instructions ~f:(function
                                                           | Data { instr = GlobalSet _; _ } -> true
                                                           | _ -> false)))) in
  let t1 = Time.now () in
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
      (* Add instr to the current slice *)
      let slice' = Instr.Label.Set.add slice slicepart.label in
      let visited' = InSlice.Set.add visited slicepart in
      let uses =
        match Cfg.find_instr cfg_instructions slicepart.label with
        | None -> failwith "Unsupported in slicing: cannot find an instruction. It probably is part of unreachable code."
        | Some instr -> Spec_inference.instr_use cfg ?var:slicepart.reason instr in
      (* For use in instr_uses(instr) *)
      let worklist' = List.fold_left uses ~init:worklist
          ~f:(fun w use ->
              (* Get the definition corresponding to the current use *)
              let def = Use_def.UseDefChains.get data_dependencies (Use_def.Use.make slicepart.label use) in
              (* For def in usedef(use): if def contains an instruction, add def.instr to W *)
              let data_deps : InSlice.Set.t = match def with
                | Use_def.Def.Instruction (instr', var) ->
                  InSlice.Set.singleton (InSlice.make instr' (Some var) cfg_instructions)
                | Use_def.Def.Entry _ -> InSlice.Set.empty
                | Use_def.Def.Constant _ -> InSlice.Set.empty in
              InSlice.Set.union w data_deps) in
      (* Add all control dependencies of instr to W *)
      let control_deps : InSlice.Set.t = match Instr.Label.Map.find control_dependencies slicepart.label with
        | None -> InSlice.Set.empty
        | Some deps -> InSlice.Set.of_list (List.map (Instr.Label.Set.to_list deps)
                                              ~f:(fun label -> InSlice.make label None cfg_instructions)) in
      let worklist'' = InSlice.Set.union worklist' control_deps in
      (* For instr' in mem_deps(instr): add instr to W *)
      let worklist''' = InSlice.Set.union worklist''
          (InSlice.Set.of_list
             (List.map ~f:(fun label -> InSlice.make label None cfg_instructions)
                (Instr.Label.Set.to_list (Memory_deps.deps_for mem_dependencies slicepart.label)))) in
      loop (InSlice.Set.remove worklist''' slicepart) slice' visited' in
  let agrawal (slice : Instr.Label.Set.t) : Instr.Label.Set.t =
    (* For each instruction in the slice, we add all br instructions that are control-dependent on it *)
    Instr.Label.Set.fold (Instr.Label.Set.of_list (Instr.Label.Map.keys cfg_instructions))
      ~init:slice
      ~f:(fun slice label ->
          let instr = Instr.Label.Map.find_exn cfg_instructions label in
          match instr with
          | Control { instr = Br _; _ } -> begin match Instr.Label.Map.find control_dependencies label with
              | Some instrs -> begin match Instr.Label.Set.find_map instrs
                                             ~f:(fun i -> if Instr.Label.Set.mem slice i then Some i else None) with
                  | Some _ ->
                    Log.info (Printf.sprintf "Agrawal tells us to add %s to the slice\n" (Instr.Label.to_string label));
                    Instr.Label.Set.add slice label
                  | None -> slice
                end
              | None -> slice
            end
          | _ -> slice) in
  let initial_worklist = InSlice.Set.union global_set_instructions (InSlice.Set.singleton { label = criterion; reason = None }) in
  let initial_slice = Instr.Label.Set.empty in
  let slice = Instr.Label.Set.filter (agrawal (loop initial_worklist initial_slice InSlice.Set.empty))
    ~f:(fun lab -> match lab.section with
        | MergeInFunction _ ->
          (* Merge instructions do not need to be marked as part of the slice once slicing has been performed *)
          false
        | _ -> true) in
  let t2 = Time.now () in
  (slice, Time.diff t1 t0, Time.diff t2 t1)

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
  : instr_type_element list * instr_type_element list =
  match i.instr with
  | Nop -> ([], [])
  | Drop -> ([Any "any"], [])
  | Select -> ([Any "a"; Any "a"; T Type.I32], [Any "a"])
  | MemorySize -> ([], [T Type.I32])
  | MemoryGrow -> ([T Type.I32], [T Type.I32])
  | Const (I32 _) -> ([], [T Type.I32])
  | Const (I64 _) -> ([], [T Type.I64])
  | Const (F32 _) -> ([], [T Type.F32])
  | Const (F64 _) -> ([], [T Type.F32])
  | Unary op -> ([T op.typ], [T op.typ])
  | Binary op -> ([T op.typ; T op.typ], [T op.typ])
  | Compare op -> ([T op.typ; T op.typ], [T Type.I32])
  | Test I32Eqz -> ([T Type.I32], [T Type.I32])
  | Test I64Eqz -> ([T Type.I64], [T Type.I32])
  | Convert op -> ([Any "a"], [T op.typ]) (* TODO: it should really be a specific type rather than any *)
  | LocalGet l -> ([], [T (Cfg.local_type cfg l)])
  | LocalSet l -> ([T (Cfg.local_type cfg l)], [])
  | LocalTee l ->
    let t = Cfg.local_type cfg l in
    ([T t], [T t])
  | GlobalGet g -> ([], [T (List32.nth_exn cfg.global_types g)])
  | GlobalSet g -> ([T (List32.nth_exn cfg.global_types g)], [])
  | Load op -> ([T Type.I32], [T op.typ])
  | Store op -> ([T Type.I32; T op.typ], [])

let type_of_control
    (i : ('a Instr.control, 'a) Instr.labelled)
    (cfg : unit Cfg.t)
    (instructions_map : Spec.t Instr.t Instr.Label.Map.t)
  : instr_type_element list * instr_type_element list =
  match i.instr with
  | Call (_, (in_type, out_type), _) -> (List.map in_type ~f:(fun t -> T t), List.map out_type ~f:(fun t -> T t))
  | CallIndirect (_, (in_type, out_type), _) ->
    ((List.map in_type ~f:(fun t -> T t)) @ [T Type.I32], (List.map out_type ~f:(fun t -> T t)))
  | If (bt, _, _, _) ->
    (* the net effect of the head, which drops the first element of the stack *)
    ([T Type.I32], match bt with
      | Some t -> [T t]
      | None -> [])
  | Block (bt, _, _)
  | Loop (bt, _, _) ->
    ([], match bt with
      | Some t -> [T t]
      | None -> [])
  | Br _ -> ([], [])
  | BrIf _ -> ([T Type.I32], [])
  | BrTable (_, _) -> ([T Type.I32], [])
  | Return ->
    begin match Cfg.find_instr instructions_map i.label with
    | Some i ->
      let annot_before = Instr.annotation_before i in
      let vstack = annot_before.vstack in
      (List.mapi vstack ~f:(fun i _ -> Any (string_of_int i)), (List.map cfg.return_types ~f:(fun t -> T t)))
    | None -> failwith "Unsupported in slicing: return instruction is part of unreachable code"
    end
  | Unreachable -> ([], [])
  | Merge -> ([], [])

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
  let input = List.map input ~f:(fun _ -> { Instr.instr = Instr.Drop; label = dummy_label (); annotation_before = (); annotation_after = (); }) in
  let push (v : Prim_value.t) = { Instr.instr = Instr.Const v; label = dummy_label (); annotation_before = (); annotation_after = () } in
  let output = List.map output ~f:(function
      | Any _ -> push (Prim_value.I32 0l)
      | T Type.I32 -> push (Prim_value.I32 0l)
      | T Type.I64 -> push (Prim_value.I64 0L)
      | T Type.F32 -> push (Prim_value.F32 (Wasm.F32.of_float 0.))
      | T Type.F64 -> push (Prim_value.F64 (Wasm.F64.of_float 0.))) in
  input @ output

(** The type of an instruction on the stack: positive if it expects value on the stack, negative otherwise *)
let type_of (i : 'a Instr.t) (cfg : 'a Cfg.t) (instructions_map : Spec.t Instr.t Instr.Label.Map.t) : (instr_type_element list * instr_type_element list) =
  match i with
  | Data d -> type_of_data d cfg
  | Control c -> type_of_control c cfg instructions_map

let instrs_type (instrs : unit Instr.t list) (cfg : 'a Cfg.t) (instructions_map : Spec.t Instr.t Instr.Label.Map.t) : (instr_type_element list * instr_type_element list) =
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

let replace_with_equivalent_instructions (instrs : unit Instr.t list) (cfg : 'a Cfg.t) (instructions_map : Spec.t Instr.t Instr.Label.Map.t) : unit Instr.t list =
  let t = instrs_type instrs cfg instructions_map in
  List.map (dummy_instrs t next_label) ~f:(fun i -> Instr.Data i)

let rec slice_alternative (cfg : 'a Cfg.t) (cfg_instructions : Spec.t Instr.t Instr.Label.Map.t) (original_instructions : unit Instr.t list) (instructions_to_keep : Instr.Label.Set.t): unit Instr.t list =
  let rec loop (instrs : unit Instr.t list) (to_remove_rev : unit Instr.t list) : unit Instr.t list =
    match instrs with
    | [] -> replace_with_equivalent_instructions (List.rev to_remove_rev) cfg cfg_instructions
    | (Control ({ instr = Block (bt, arity, body); _ } as instr)) as entire_instr :: rest ->
      let sliced_body = slice_alternative cfg cfg_instructions body instructions_to_keep in
      (* TODO: we could also drop the block if it is not empty but only contains instructions that are not part of the slice (basically, only dummy instructions) *)
      if List.is_empty sliced_body then
        (* Block body is empty, drop the block entirely *)
        loop rest (entire_instr :: to_remove_rev)
      else
        (replace_with_equivalent_instructions to_remove_rev cfg cfg_instructions) @ [Instr.Control { instr with instr = Block (bt, arity, sliced_body) }] @ loop rest []
    | (Control ({ instr = Loop (bt, arity, body); _ } as instr)) as entire_instr :: rest ->
      let sliced_body = slice_alternative cfg cfg_instructions body instructions_to_keep in
      if List.is_empty sliced_body then
        loop rest (entire_instr :: to_remove_rev)
      else
        (replace_with_equivalent_instructions to_remove_rev cfg cfg_instructions) @ [Instr.Control { instr with instr = Loop (bt, arity, sliced_body) }] @ loop rest []
    | (Control ({ instr = If (bt, arity, then_, else_); _ } as instr)) as entire_instr :: rest ->
      let sliced_then = slice_alternative cfg cfg_instructions then_ instructions_to_keep in
      let sliced_else = slice_alternative cfg cfg_instructions else_ instructions_to_keep in
      if List.is_empty sliced_then && List.is_empty sliced_else then
        loop rest (entire_instr :: to_remove_rev)
      else
        (replace_with_equivalent_instructions to_remove_rev cfg cfg_instructions) @
        [Instr.Control { instr with instr = If (bt, arity,
                                                sliced_then,
                                                sliced_else) }] @ loop rest []
    | instr :: rest when Instr.Label.Set.mem instructions_to_keep (Instr.label instr) ->
    (replace_with_equivalent_instructions to_remove_rev cfg cfg_instructions) @ [instr] @ loop rest []
    | instr :: rest ->
      loop rest (instr :: to_remove_rev) in
  loop original_instructions []

let slice_alternative_to_funcinst (cfg : Spec.t Cfg.t) (cfg_instructions : Spec.t Instr.t Instr.Label.Map.t) ?instrs:(instructions_in_slice : Instr.Label.Set.t option = None) (slicing_criterion : Instr.Label.t) : Func_inst.t =
  let instructions_in_slice = match instructions_in_slice with
    | Some instrs -> instrs
    | None ->
      Log.info "Computing instructions part of the slice";
      let instrs, _, _ = instructions_to_keep cfg cfg_instructions slicing_criterion in
      instrs in
  Log.info "Clearing annotations";
  let unit_cfg = Cfg.clear_annotations cfg in
  Log.info "Constructing a valid slice";
  let instructions = slice_alternative unit_cfg cfg_instructions (Cfg.body unit_cfg) instructions_in_slice  in
  { idx = cfg.idx;
    name = Some cfg.name;
    type_idx = cfg.type_idx;
    typ = (cfg.arg_types, cfg.return_types);
    code = { locals = cfg.local_types; body = instructions } }

(** Return the indices of each call_indirect instructions *)
let find_call_indirect_instructions (cfg : Spec.t Cfg.t) : Instr.Label.t list =
  List.filter_map (Cfg.all_instructions_list cfg) ~f:(fun instr -> match instr with
      | Control {label; instr = CallIndirect _; _} -> Some label
      | _ -> None)

module Test = struct
  open Instr.Label.Test
  let build_cfg ?fidx:(fidx : int32 = 0l) (program : string) : Wasm_module.t * Spec.t Cfg.t =
    let module_ = Wasm_module.of_string program in
    let cfg = Spec_analysis.analyze_intra1 module_ fidx in
    (module_, Cfg.without_empty_nodes_with_no_predecessors cfg)

  let%test "simple slicing - first slicing criterion, only const" =
    Spec_inference.propagate_globals := false;
    Spec_inference.propagate_locals := false;
    Spec_inference.use_const := false;
    let _, cfg = build_cfg "(module
  (type (;0;) (func (param i32) (result i32)))
  (func (;test;) (type 0) (param i32) (result i32)
    memory.size ;; Instr 0
    memory.size ;; Instr 1
    i32.add     ;; Instr 2 -- slicing criterion
    drop        ;; Instr 3
    memory.size ;; Instr 4
    memory.size ;; Instr 5
    i32.add)    ;; Instr 6
  )" in
    let actual, _, _ = instructions_to_keep cfg (Cfg.all_instructions cfg) (lab 2) in
    let expected = Instr.Label.Set.of_list [lab 0; lab 1; lab 2] in
    Instr.Label.Set.check_equality ~actual:actual ~expected:expected

  let%test "simple slicing - second slicing criterion, with locals" =
    Spec_inference.propagate_globals := false;
    Spec_inference.propagate_locals := false;
    Spec_inference.use_const := false;
    let _, cfg = build_cfg "(module
  (type (;0;) (func (param i32) (result i32)))
  (func (;test;) (type 0) (param i32) (result i32)
    memory.size ;; Instr 0
    memory.size ;; Instr 1
    i32.add     ;; Instr 2
    drop        ;; Instr 3
    local.get 0 ;; Instr 4
    memory.size ;; Instr 5
    i32.add)    ;; Instr 6 -- slicing criterion
  )" in
    let actual, _, _ = instructions_to_keep cfg (Cfg.all_instructions cfg) (lab 6) in
    let expected = Instr.Label.Set.of_list [lab 4; lab 5; lab 6] in
    Instr.Label.Set.check_equality ~actual:actual ~expected:expected

  let%test "slicing with block and br_if" =
    Spec_inference.propagate_globals := false;
    Spec_inference.propagate_locals := false;
    Spec_inference.use_const := false;
    let _, cfg = build_cfg "(module
  (type (;0;) (func (param i32) (result i32)))
  (func (;test;) (type 0) (param i32) (result i32)
    block         ;; Instr 0
      memory.size ;; Instr 1 -- data dependency of instruction 2
      br_if 0     ;; Instr 2 -- control dependency of the slicing criterion
      memory.size ;; Instr 3 -- slicing criterion
      drop        ;; Instr 4
    end
    local.get 0)  ;; Instr 5
  )" in
    let actual, _, _ = instructions_to_keep cfg (Cfg.all_instructions cfg) (lab 3) in
    let expected = Instr.Label.Set.of_list [lab 1; lab 2; lab 3] in
    Instr.Label.Set.check_equality ~actual:actual ~expected:expected

  let%test "slicing with block and br_if -- second slicing criterion" =
     Spec_inference.propagate_globals := false;
     Spec_inference.propagate_locals := false;
     Spec_inference.use_const := false;
    let _, cfg = build_cfg "(module
  (type (;0;) (func (param i32) (result i32)))
  (func (;test;) (type 0) (param i32) (result i32)
    block         ;; Instr 0
      memory.size ;; Instr 1
      br_if 0     ;; Instr 2 -- has a data dep on 1
      memory.size ;; Instr 3 -- has a control dep on 2
      drop        ;; Instr 4 -- slicing criterion, has a data dep on instr 3
    end
    local.get 0)  ;; Instr 5
  )" in
    let actual, _, _ = instructions_to_keep cfg (Cfg.all_instructions cfg) (lab 4) in
    let expected = Instr.Label.Set.of_list [lab 1; lab 2; lab 3; lab 4] in
    Instr.Label.Set.check_equality ~actual:actual ~expected:expected

  let%test "slicing with merge blocks" =
     Spec_inference.propagate_globals := false;
     Spec_inference.propagate_locals := false;
     Spec_inference.use_const := false;
    let _, cfg = build_cfg "(module
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
  )" in
    let actual, _, _ = instructions_to_keep cfg (Cfg.all_instructions cfg) (lab 9) in
    (* Merge blocks do not need to be in the slice *)
    let expected = Instr.Label.Set.of_list [lab 0; lab 1; lab 2; lab 3; lab 8; lab 9] in
    Instr.Label.Set.check_equality ~actual:actual ~expected:expected

  let%test_unit "slicing with merge blocks using slice" =
     Spec_inference.propagate_globals := false;
     Spec_inference.propagate_locals := false;
     Spec_inference.use_const := false;
    let _, cfg = build_cfg "(module
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
    i32.add)        ;; Instr 9
   (table (;0;) 1 1 funcref)
   (memory (;0;) 2)
   (global (;0;) (mut i32) (i32.const 66560)))" in
    let _funcinst = slice_alternative_to_funcinst cfg (Cfg.all_instructions cfg) (lab 9) in
    (* Nothing is really tested here, besides the fact that we don't want any exceptions to be thrown *)
    ()

   let%test_unit "slicing with a block containing a single drop should produce a valid slice" =
     Spec_inference.propagate_globals := false;
     Spec_inference.propagate_locals := false;
     Spec_inference.use_const := false;
     let _, cfg = build_cfg "(module
   (type (;0;) (func (param i32) (result i32)))
   (func (;test;) (type 0) (param i32) (result i32)
    block           ;; Instr 0
      local.get 0   ;; Instr 1 [i0]
      local.get 0   ;; Instr 2 [i1, i0]
      if            ;; Instr 3 [i0]
        drop        ;; Instr 4 []
        i32.const 0 ;; Instr 5 [i4]
      else
        nop         ;; Instr 6 [i0]
      end
                    ;; [i0] and [i4] merged into [m1]
      i32.const 32  ;; Instr 7 ;; [i6, m1]
      i32.add       ;; Instr 8 ;; [i7]
    end)
   )" in
     let _funcinst = slice_alternative_to_funcinst cfg (Cfg.all_instructions cfg) (lab 8) in
     ()

   let%test_unit "slicing intra-block block containing a single drop - variant" =
     Spec_inference.propagate_globals := false;
     Spec_inference.propagate_locals := false;
     Spec_inference.use_const := false;
     let _, cfg = build_cfg "(module
   (type (;0;) (func (param i32) (result i32)))
   (func (;test;) (type 0) (param i32) (result i32)
    block           ;; Instr 0
      local.get 0   ;; Instr 1
      local.get 0   ;; Instr 2
      if            ;; Instr 3
        drop        ;; Instr 4
        i32.const 0 ;; Instr 5
      else
        i32.const 1 ;; Instr 6
        drop        ;; Instr 7
      end
      i32.const 32  ;; Instr 8
      i32.add       ;; Instr 9
    end)
   )" in
     let _funcinst = slice_alternative_to_funcinst cfg (Cfg.all_instructions cfg) (lab 9) in
     ()

   let%test_unit "slicing with a block containing a single drop - variant" =
     Spec_inference.propagate_globals := false;
     Spec_inference.propagate_locals := false;
     Spec_inference.use_const := false;
     let _, cfg = build_cfg "(module
   (type (;0;) (func (param i32) (result i32)))
   (func (;test;) (type 0) (param i32) (result i32)
    block           ;; Instr 0
      local.get 0   ;; Instr 1
      local.get 0   ;; Instr 2
      if            ;; Instr 3
        drop        ;; Instr 4
        i32.const 0 ;; Instr 5
      else
        i32.const 1 ;; Instr 6
        drop        ;; Instr 7
      end
      i32.const 32  ;; Instr 8
      i32.add       ;; Instr 9
    end)
   )" in
     let _funcinst = slice_alternative_to_funcinst cfg (Cfg.all_instructions cfg) (lab 9) in
     ()

   let check_slice original sliced fidx criterion =
     Spec_inference.propagate_globals := false;
     Spec_inference.propagate_locals := false;
     Spec_inference.use_const := false;
     let _, cfg = build_cfg ~fidx original in
     let expected = (slice_alternative_to_funcinst cfg (Cfg.all_instructions cfg) (lab ~fidx criterion)).code.body in
     let _, expected_cfg = build_cfg ~fidx sliced in
     let actual = Cfg.body expected_cfg in
     List.equal (fun x y ->
         if Instr.equal (fun () () -> true) (Instr.drop_labels x) (Instr.drop_labels y) then
           true
         else begin
           Printf.printf "instruction not equal: %s != %s\n" (Instr.to_string x) (Instr.to_string y);
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

   let%test "slicing with memory does not fail" =
     let original =  "(module
   (type (;0;) (func (param i32) (result i32)))
   (func (;test;) (type 0) (param i32) (result i32)
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
   (type (;0;) (func (param i32) (result i32)))
   (func (;test;) (type 0) (param i32) (result i32)
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
  )" in
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
  block
    local.get 0
    block
      if
        br 0
      else
        br 1
      end
    end
    local.get 0
  end
))" in
     (* This is not the ideal slice, but this is good enough *)
     let sliced = "(module
(type (;0;) (func (param i32) (result i32)))
(func (;0;) (type 0)
  block
    i32.const 0
    block
      drop
    end
    local.get 0
  end
)
)" in
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
  (type (;1;) (func (param i32) (result i32)))
  (func (;eof;) (type 1) (result i32)
    i32.const 0)
  (func (;read;) (type 1) (result i32)
    i32.const 0)
  (func (;f;) (type 2) (param i32) (result i32)
    local.get 0)
  (func (;test;) (type 0)
    (local i32 i32 i32)
    block
      loop
        call 0
        br_if 1
        block
          block
            call 1
            br_if 0
            br 1
          end ;; end of block 2 (L8)
          block
            local.get 1
            i32.const 1
            i32.add
            local.set 1
          end
        end
        br 0
      end
    end
    local.get 1
    call 2
    drop
    ))" in
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
  (type (;1;) (func (param i32) (result i32)))
  (func (;eof;) (type 1) (result i32)
    i32.const 0)
  (func (;read;) (type 1) (result i32)
    i32.const 0)
  (func (;f;) (type 2) (param i32) (result i32)
    local.get 0)
  (func (;test;) (type 0)
    (local i32 i32 i32)
  block
    loop
      call 0
      i32.const 0
      i32.ne
      br_if 1
      call 1
      if
        br 1
      end
      local.get 1
      i32.const 1
      i32.add
      local.set 1
    end
  end
  local.get 1
  call 2
  drop))" in
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
        let funcinst = slice_alternative_to_funcinst cfg (Cfg.all_instructions cfg) instr_idx in
        let module_ = Wasm_module.replace_func module_ 14l funcinst in
        (* We should be able to re-annotate the graph *)
        Spec_inference.propagate_locals := true;
        Spec_inference.propagate_globals := true;
        Spec_inference.use_const := true;
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
         let funcinst = slice_alternative_to_funcinst cfg (Cfg.all_instructions cfg) instr_idx in
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

 end
