open Core_kernel

module SlicePart = struct
  module T = struct
    type t =
      | Instruction of Instr.label
      | Merge of int
    [@@deriving sexp, compare, equal]
    let to_string (t : t) : string = match t with
      | Instruction l -> Printf.sprintf "instr(%d)" l
      | Merge idx -> Printf.sprintf "merge(%d)" idx
  end
  include T
  module Set = struct
    include Set.Make(T)
    let to_string (s : t) : string = String.concat ~sep:"," (List.map (to_list s) ~f:T.to_string)
  end
end

(** Performs backwards slicing on `cfg`, using the slicing criterion
   `criterion`, encoded as an instruction index. Returns the set
   of instructions that are part of the slice, as a list of instruction
    indices. *)
let slicing (cfg : Spec_inference.state Cfg.t) (criterion : Instr.label) : SlicePart.Set.t =
  let control_dependencies = Control_deps.make cfg in
  let (_, _, data_dependencies) = Use_def.make cfg in
  let rec loop (worklist : SlicePart.Set.t) (slice : SlicePart.Set.t) : SlicePart.Set.t =
    (* Perform backward slicing as follows:
       Given an instruction as the slicing criterion (we can derive variable uses from instructions),
       perform the following fixpoint algorithm, starting with W = instr
         let instr = pop(W)
         add instr to the current slice
         for use in instr_uses(instr):
           for def in usedef(use):
             if def contains an istruction, add def.instr to W
           for _, instr' in cdeps(use.var):
             add instr to W *)
    match SlicePart.Set.choose worklist with
    | None -> (* worklist is empty *)
      slice
    | Some slicepart ->
      let uses = match slicepart with
        | Instruction instr -> Use_def.instr_use (Cfg.find_instr_exn cfg instr)
        | Merge block_idx ->
          (* to find uses of a merge block, we look at variables that are
             redefined: all such initial variables are then considered to be
             used *)
          let vars = Spec_inference.new_merge_variables cfg (Cfg.find_block_exn cfg block_idx) in
          List.map vars ~f:fst in
      let worklist' = List.fold_left uses ~init:worklist
          ~f:(fun w use ->
              let def = Use_def.UseDefChains.get data_dependencies (match slicepart with
                  | Instruction instr -> Use_def.Use.Instruction (instr, use)
                  | Merge blockidx -> Use_def.Use.Merge (blockidx, use)) in
              let to_add_from_def = match def with
                | Use_def.Def.Instruction (instr', _) -> SlicePart.Set.singleton (Instruction instr')
                | Use_def.Def.Merge (blockidx, _) -> SlicePart.Set.singleton (Merge blockidx)
                | Use_def.Def.Entry _ -> SlicePart.Set.empty in
              let preds = Control_deps.find control_dependencies use in (* the control dependencies for the current use *)
              Control_deps.Pred.Set.fold preds
                ~init:(SlicePart.Set.union w to_add_from_def)
                ~f:(fun w (_, instr') ->
                    (* TODO: can't merge block also have control dependencies? Maybe not relevant, as they will have data dependencies on what they redefine *)
                    SlicePart.Set.add w (Instruction instr'))) in
      loop (SlicePart.Set.remove worklist' slicepart) (SlicePart.Set.add slice slicepart) in
  loop (SlicePart.Set.singleton (Instruction criterion)) SlicePart.Set.empty

(** Performs backwards slicing on `cfg`, relying on `slicing` defined above.
    Returns the slice as a modified CFG *)
let slice (_cfg : Spec_inference.state Cfg.t) (_criterion : Instr.label) : Spec_inference.state Cfg.t =
  failwith "TODO"

let%test "simple slicing - first slicing criterion, only const" =
  Instr.reset_counter ();
  let module_ = Wasm_module.of_string "(module
  (type (;0;) (func (param i32) (result i32)))
  (func (;test;) (type 0) (param i32) (result i32)
    i32.const 1 ;; Instr 0
    i32.const 2 ;; Instr 1
    i32.add     ;; Instr 2
    drop        ;; Instr 3
    local.get 0 ;; Instr 4
    i32.const 3 ;; Instr 5
    i32.add)    ;; Instr 6
  (table (;0;) 1 1 funcref)
  (memory (;0;) 2)
  (global (;0;) (mut i32) (i32.const 66560)))" in
  let cfg = Spec_analysis.analyze_intra1 module_ 0 in
  let actual = slicing cfg 2 in
  let expected = SlicePart.Set.of_list [Instruction 0; Instruction 1; Instruction 2] in
  SlicePart.Set.equal actual expected

let%test "simple slicing - second slicing criterion, with locals" =
  Instr.reset_counter ();
  let module_ = Wasm_module.of_string "(module
  (type (;0;) (func (param i32) (result i32)))
  (func (;test;) (type 0) (param i32) (result i32)
    i32.const 1 ;; Instr 0
    i32.const 2 ;; Instr 1
    i32.add     ;; Instr 2
    drop        ;; Instr 3
    local.get 0 ;; Instr 4
    i32.const 3 ;; Instr 5
    i32.add)    ;; Instr 6
  (table (;0;) 1 1 funcref)
  (memory (;0;) 2)
  (global (;0;) (mut i32) (i32.const 66560)))" in
  let cfg = Spec_analysis.analyze_intra1 module_ 0 in
  let actual = slicing cfg 6 in
  (* Why is Instruction 4 not part of the slice?
     Because this is not the instruction that *defines* l0, l0 is defined at the entry point of the function.
     Instead, what will happen is that when we see that we need l0 in the slice, we can easily add a local.get 0 instruction to the slice, not matter what the input program was *)
  let expected = SlicePart.Set.of_list [Instruction 5; Instruction 6] in
  SlicePart.Set.equal actual expected

let%test "slicing with block and br_if" =
  Instr.reset_counter ();
  let module_ = Wasm_module.of_string "(module
  (type (;0;) (func (param i32) (result i32)))
  (func (;test;) (type 0) (param i32) (result i32)
    block         ;; Instr 0
      i32.const 1 ;; Instr 1
      br_if 0     ;; Instr 2
      i32.const 2 ;; Instr 3
      drop        ;; Instr 4
    end
    local.get 0)   ;; Instr 5
  (table (;0;) 1 1 funcref)

  (memory (;0;) 2)
  (global (;0;) (mut i32) (i32.const 66560)))" in
  let cfg = Spec_analysis.analyze_intra1 module_ 0 in
  let actual = slicing cfg 3 in
  let expected = SlicePart.Set.of_list [Instruction 0; Instruction 1; Instruction 2; Instruction 3] in
  SlicePart.Set.equal actual expected

let%test "slicing with merge blocks" =
  Instr.reset_counter ();
  let module_ = Wasm_module.of_string "(module
  (type (;0;) (func (param i32) (result i32)))
  (func (;test;) (type 0) (param i32) (result i32)
    i32.const 0     ;; Instr 0
    if (result i32) ;; Instr 1
      i32.const 1   ;; Instr 2
    else
      i32.const 2   ;; Instr 3
    end
    ;; Merge block 4 here
    ;; ----
    i32.const 4     ;; Instr 4
    i32.const 5     ;; Instr 5
    i32.add         ;; Instr 6
    drop            ;; Instr 7
    ;; ---- this previous part should not be part of the slice
    i32.const 3     ;; Instr 8
    i32.add)        ;; Instr 9
  (table (;0;) 1 1 funcref)
  (memory (;0;) 2)
  (global (;0;) (mut i32) (i32.const 66560)))" in
  let cfg = Spec_analysis.analyze_intra1 module_ 0 in
  let actual = slicing cfg 9 in
  let expected = SlicePart.Set.of_list [Instruction 0; Instruction 1; Instruction 2; Instruction 3; Merge 4; Instruction 8; Instruction 9] in
  SlicePart.Set.equal actual expected

(* 
Challenges:
  - should we include breaks as part of the slice? It seems so -> need control deps that contain instructions
  - how to deal with blocks as part of the slice? -> work on a block-per-block basis?
  - how to construct slices that adhere to the stack convention? -> should be doable to add dummy drop or pushes to fill in the blank

*)
