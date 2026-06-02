open Core

type t = Instr.Label.Set.t Instr.Label.Map.t (* Map from instruction to its memory dependencies *)

let make (cfg : 'a Cfg.t) : t =
  let instrs = Cfg.all_instructions cfg in
  let predecessor_cache = Hashtbl.Poly.create () in
  let predecessors_for (block : 'a Basic_block.t) =
    Hashtbl.find_or_add predecessor_cache block.idx
      ~default:(fun () -> Cfg.all_predecessors cfg block)
  in
  let loop_cache = Hashtbl.Poly.create () in
  let is_in_loop (block : 'a Basic_block.t) (predecessors : 'a Basic_block.t list) =
    Hashtbl.find_or_add loop_cache block.idx ~default:(fun () ->
        List.exists predecessors ~f:(fun p ->
            List.exists (Cfg.Edges.from cfg.edges p.idx) ~f:(fun (pred_of_p, _) ->
                Int.equal pred_of_p block.idx)))
  in
  (* Instructions that are load or call depend on all stores/calls that may have been executed before, hence on all stores contained in a predecessor of the current node in the CFG *)
  let loads_and_calls = Instr.Label.Map.keys
      (Instr.Label.Map.filter instrs ~f:(fun i -> match i with
           | Call { instr = CallDirect _ ; _ } -> true
           | Call { instr = CallIndirect _ ; _ } -> true
           | Data { instr = Load _ ; _ } -> true
           | Data { instr = MemoryCopy; _ } -> true
           | _ -> false)) in
  let deps = Instr.Label.Map.of_alist_exn (List.map loads_and_calls ~f:(fun label ->
      let enclosing_block = Cfg.find_enclosing_block_exn cfg label in
      let predecessors = predecessors_for enclosing_block in
      let is_in_loop = is_in_loop enclosing_block predecessors in
      let get_store_label : 'a Instr.labelled_data -> Instr.Label.t option  = (function
          | { instr = Store _ | MemoryCopy | MemoryFill | MemoryInit _ ; label = sl; _ } -> Some sl
          | _ -> None) in
      (label, List.fold_left predecessors ~init:Instr.Label.Set.empty ~f:(fun acc pred_block ->
           Instr.Label.Set.union acc
             (match pred_block.content with
             | Call { instr = CallDirect _; label = cl; _ } -> Instr.Label.Set.singleton cl
             | Call { instr = CallIndirect _; label = cl; _ } -> Instr.Label.Set.singleton cl
             | Control _ | Entry | Return _ | Imported _ -> Instr.Label.Set.empty
             | Data block_instrs ->
               if Int.equal pred_block.idx enclosing_block.idx && not is_in_loop then
                 (* Same block: only include stores that appear before the load/call, unless there is a loop (the block can reach itself) *)
                 let preceding = List.take_while block_instrs
                     ~f:(fun i -> not (Instr.Label.equal i.label label)) in
                 Instr.Label.Set.of_list (List.filter_map preceding ~f:get_store_label)
               else
                 Instr.Label.Set.of_list (List.filter_map block_instrs ~f:get_store_label)))))) in
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
  let%test "non dep with memory with store after load in the same block" =
    let open Instr.Label.Test in
    let module_ = Wasm_module.of_string "(module
  (type (;0;) (func (param i32) (result)))
  (func (;test;) (type 0) (param i32) (result)
    memory.size     ;; Instr 0
    i32.load        ;; Instr 1
    memory.size     ;; Instr 2
    i32.store)      ;; Instr 3 (should not be a dependency of the preceding load in the same block)
  (memory (;0;) 2))" in
    let cfg = Spec_analysis.analyze_intra1 module_ 0l in
    let deps = make cfg in
    let actual = deps_for deps (lab 1) in
    let expected = Instr.Label.Set.empty in
    Instr.Label.Set.check_equality ~actual ~expected
  let%test "dep with memory with store after load in the same block, in a loop" =
    let open Instr.Label.Test in
    let module_ = Wasm_module.of_string "(module
  (type (;0;) (func (param i32) (result)))
  (func (;test;) (type 0) (param i32) (result)
    loop              ;; Instr 0
      memory.size     ;; Instr 1
      i32.load        ;; Instr 2
      memory.size     ;; Instr 3
      i32.store       ;; Instr 4 (should be a dependency of the preceding load in the same block)
      br 0
    end)
  (memory (;0;) 2))" in
    let cfg = Spec_analysis.analyze_intra1 module_ 0l in
    let deps = make cfg in
    let actual = deps_for deps (lab 2) in
    let expected = Instr.Label.Set.singleton (lab 4) in
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

  let%test "memory.fill is a memory writer" =
    let open Instr.Label.Test in
    let module_ = Wasm_module.of_string "(module
  (type (;0;) (func (result i32)))
  (func (;test;) (type 0) (result i32)
    i32.const 0   ;; Instr 0: dst
    i32.const 0   ;; Instr 1: val
    i32.const 0   ;; Instr 2: len
    memory.fill   ;; Instr 3: writes memory
    i32.const 0   ;; Instr 4: load address
    i32.load)     ;; Instr 5: must depend on Instr 3
  (memory (;0;) 2))" in
    let cfg = Spec_analysis.analyze_intra1 module_ 0l in
    let deps = make cfg in
    let actual = deps_for deps (lab 5) in
    let expected = Instr.Label.Set.singleton (lab 3) in
    Instr.Label.Set.check_equality ~actual ~expected

  let%test "memory.init is a memory writer" =
    let open Instr.Label.Test in
    let module_ = Wasm_module.of_string "(module
  (type (;0;) (func (result i32)))
  (func (;test;) (type 0) (result i32)
    i32.const 0     ;; Instr 0: dst
    i32.const 0     ;; Instr 1: src offset in data segment
    i32.const 0     ;; Instr 2: len
    memory.init 0   ;; Instr 3: writes memory
    i32.const 0     ;; Instr 4: load address
    i32.load)       ;; Instr 5: must depend on Instr 3
  (memory (;0;) 2)
  (data \"xxxx\"))" in
    let cfg = Spec_analysis.analyze_intra1 module_ 0l in
    let deps = make cfg in
    let actual = deps_for deps (lab 5) in
    let expected = Instr.Label.Set.singleton (lab 3) in
    Instr.Label.Set.check_equality ~actual ~expected

  let%test "memory.copy is a memory writer" =
    let open Instr.Label.Test in
    let module_ = Wasm_module.of_string "(module
  (type (;0;) (func (result i32)))
  (func (;test;) (type 0) (result i32)
    i32.const 0   ;; Instr 0: dst
    i32.const 0   ;; Instr 1: src
    i32.const 0   ;; Instr 2: len
    memory.copy   ;; Instr 3: writes memory
    i32.const 0   ;; Instr 4: load address
    i32.load)     ;; Instr 5: must depend on Instr 3
  (memory (;0;) 2))" in
    let cfg = Spec_analysis.analyze_intra1 module_ 0l in
    let deps = make cfg in
    let actual = deps_for deps (lab 5) in
    let expected = Instr.Label.Set.singleton (lab 3) in
    Instr.Label.Set.check_equality ~actual ~expected

  let%test "memory.copy is a memory reader" =
    let open Instr.Label.Test in
    let module_ = Wasm_module.of_string "(module
  (type (;0;) (func))
  (func (;test;) (type 0)
    i32.const 0   ;; Instr 0: store address
    i32.const 42  ;; Instr 1: store value
    i32.store     ;; Instr 2: writes memory
    i32.const 0   ;; Instr 3: dst
    i32.const 0   ;; Instr 4: src
    i32.const 0   ;; Instr 5: len
    memory.copy)  ;; Instr 6: reads memory — must depend on Instr 2
  (memory (;0;) 2))" in
    let cfg = Spec_analysis.analyze_intra1 module_ 0l in
    let deps = make cfg in
    let actual = deps_for deps (lab 6) in
    let expected = Instr.Label.Set.singleton (lab 2) in
    Instr.Label.Set.check_equality ~actual ~expected

end
