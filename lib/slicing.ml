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
*)
open Core_kernel
open Helpers

module VarInstr = struct
  module T = struct
    type t = Var.t * Instr.label
    [@@deriving sexp, compare, equal]
  end
  include T
  module Set = Set.Make(T)
end


(** Performs backwards slicing on `cfg`, using the slicing criterion
   `criterion`, encoded as an instruction index. Returns the set
   of instructions that are part of the slice, as a list of instruction
    indices. *)
let slicing (cfg : Spec_inference.state Cfg.t) (criterion : Instr.label) : IntSet.t =
  let control_dependencies = Control_deps.make cfg in
  let (_, _, data_dependencies) = Use_def.make cfg in
  let rec loop (worklist : IntSet.t) (slice : IntSet.t) =
    match IntSet.choose worklist with
    | None -> (* worklist is empty *)
      slice
    | Some instr ->
      let uses_of_instr = Use_def.instr_use (Cfg.find_instr_exn cfg instr) in
      let worklist' = List.fold_left uses_of_instr ~init:worklist
          ~f:(fun w use ->
              let def = Use_def.UseDefChains.get data_dependencies (Use_def.Use.Instruction (instr, use)) in
              let to_add_from_def = match def with
                | Use_def.Def.Instruction (instr', _) -> IntSet.singleton instr'
                | _ -> IntSet.empty in
              let preds = Control_deps.find control_dependencies use in
              Control_deps.Pred.Set.fold preds
                ~init:(IntSet.union w to_add_from_def)
                ~f:(fun w (_, instr') -> IntSet.add w instr')) in
      loop (IntSet.remove worklist' instr) (IntSet.add slice instr) in
  loop (IntSet.singleton criterion) IntSet.empty

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
  let expected = IntSet.of_list [0; 1; 2] in
  IntSet.equal actual expected

(* This test is failing because we need access to local l0.
    It makes sense because it is not local.get 0 that defines l0 (it is defined at the entry point of the code.
    But maybe it would make more sense if use-defs would connect uses of locals to the most recent local.get? 
    Or that would be part of the slicing algorithm: 
      - if the definition is a local l, then add the most recent instruction (walk backwards in the CFG to find it) that is a local.get l
*)
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
  let expected = IntSet.of_list [4; 5; 6] in
  Printf.printf "slice: %s\n" (IntSet.to_string actual);
  IntSet.equal actual expected

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
  let expected = IntSet.of_list [0; 1; 2; 3] in
  Printf.printf "slice: %s\n" (IntSet.to_string actual);
  IntSet.equal actual expected

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
    i32.const 3     ;; Instr 4
    i32.add)        ;; Instr 5
  (table (;0;) 1 1 funcref)

  (memory (;0;) 2)
  (global (;0;) (mut i32) (i32.const 66560)))" in
  let cfg = Spec_analysis.analyze_intra1 module_ 0 in
  let actual = slicing cfg 5 in
  let expected = IntSet.of_list [0; 1; 2; 3; 4; 5] in
  Printf.printf "slice: %s\n" (IntSet.to_string actual);
  IntSet.equal actual expected

(* 
Challenges:
  - should we include breaks as part of the slice? It seems so -> need control deps that contain instructions
  - how to deal with blocks as part of the slice? -> work on a block-per-block basis?
  - how to construct slices that adhere to the stack convention? -> should be doable to add dummy drop or pushes to fill in the blank

*)
