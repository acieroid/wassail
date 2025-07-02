open Core

type t = Instr.Label.Set.t Instr.Label.Map.t (* Map from instruction to its memory dependencies *)

let make (cfg : 'a Cfg.t) : t =
  let instrs = Cfg.all_instructions cfg in
  (* Instructions that are load or call depend on all stores/calls that may have been executed before, hence on all stores contained in a predecessor of the current node in the CFG *)
  let loads_and_calls = Instr.Label.Map.keys
      (Instr.Label.Map.filter instrs ~f:(fun i -> match i with
           | Control { instr = Call _ ; _ } -> true
           | Control { instr = CallIndirect _ ; _ } -> true
           | Data { instr = Load _ ; _ } -> true
           | _ -> false)) in
  let deps = Instr.Label.Map.of_alist_exn (List.map loads_and_calls ~f:(fun label ->
      let block = Cfg.find_enclosing_block_exn cfg label in
      let predecessors = Cfg.all_predecessors cfg block in
      (label, List.fold_left predecessors ~init:Instr.Label.Set.empty ~f:(fun acc block ->
           Instr.Label.Set.union acc
             (match block.content with
             | Control { instr = Call _; label; _ } -> Instr.Label.Set.singleton label
             | Control { instr = CallIndirect _; label; _ } -> Instr.Label.Set.singleton label
             | Control _ -> Instr.Label.Set.empty
             | Data instrs ->
               Instr.Label.Set.of_list (List.filter_map instrs ~f:(function
                   | { instr = Store _; label; _ } -> Some label
                   | _ -> None))))))) in
  deps

let deps_for (deps : t) (instr : Instr.Label.t) : Instr.Label.Set.t =
  match Instr.Label.Map.find deps instr with
  | Some instrs -> instrs
  | None -> Instr.Label.Set.empty

module Test = struct
  let%test "mem-dep with memory" =
    let open Instr.Label.Test in
    let module_ = Wasm_module.of_string "(module
  (type (;0;) (func (param i32) (result i32)))
  (func (;test;) (type 0) (param i32) (result i32)
    memory.size     ;; Instr 0
    memory.size     ;; Instr 1
    i32.store       ;; Instr 2
    memory.size     ;; Instr 3
    i32.load)       ;; Instr 4
  (memory (;0;) 2))" in
    let cfg = Spec_analysis.analyze_intra1 module_ 0l in
    let deps = make cfg in
    let actual = deps_for deps (lab 4) in
    let expected = Instr.Label.Set.singleton (lab 2) in
    Instr.Label.Set.check_equality ~actual ~expected
  let%test "mem-dep with call" =
    let open Instr.Label.Test in
    let module_ = Wasm_module.of_string "(module
  (type (;0;) (func (param i32) (result i32 i32 i32)))
  (type (;1;) (func))
  (func (;test;) (type 0) (param i32) (result i32 i32 i32)
    memory.size     ;; Instr 0
    memory.size     ;; Instr 1
    call 1       ;; Instr 2
    memory.size     ;; Instr 3
    i32.load)       ;; Instr 4
  (func (;1;) (type 1))
  (memory (;0;) 2))" in
    let cfg = Spec_analysis.analyze_intra1 module_ 0l in
    let deps = make cfg in
    let actual = deps_for deps (lab 4) in
    let expected = Instr.Label.Set.singleton (lab 2) in
    Instr.Label.Set.check_equality ~actual ~expected

end
