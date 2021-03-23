type t = {
  loads : Instr.Label.Set.t;
  stores : Instr.Label.Set.t;
}

let make (cfg : 'a Cfg.t) : t =
  let instrs = Cfg.all_instructions cfg in
  let loads = Instr.Label.Set.of_list (Instr.Label.Map.keys
                                         (Instr.Label.Map.filter instrs ~f:(fun i -> match i with
                                              | Data { instr = Load _ ; _ } -> true
                                              | _ -> false))) in
  let stores = Instr.Label.Set.of_list (Instr.Label.Map.keys
                                          (Instr.Label.Map.filter instrs ~f:(fun i -> match i with
                                               | Data { instr = Store _ ; _ } -> true
                                               | _ -> false))) in
  { loads; stores }

let deps_for (deps : t) (instr : Instr.Label.t) : Instr.Label.Set.t =
  if Instr.Label.Set.mem deps.loads instr then
    (* This is a load operation, it depends on ALL store operations (because any of these store could write to the loaded address *)
    deps.stores
  else
    (* This is not a load operation, there is no memory dependency for it *)
    Instr.Label.Set.empty

module Test = struct
  let%test "mem-dep with memory2" =
    let open Instr.Label.Test in
    let module_ = Wasm_module.of_string "(module
  (type (;0;) (func (param i32) (result i32)))
  (func (;test;) (type 0) (param i32) (result i32)
    memory.size     ;; Instr 0
    memory.size     ;; Instr 1
    i32.store       ;; Instr 2
    memory.size     ;; Instr 3
    i32.load)       ;; Instr 4
  )" in
    let cfg = Spec_analysis.analyze_intra1 module_ 0l in
    let deps = make cfg in
    let actual = deps_for deps (lab 4) in
    let expected = Instr.Label.Set.singleton (lab 2) in
    Instr.Label.Set.check_equality ~actual ~expected
end
