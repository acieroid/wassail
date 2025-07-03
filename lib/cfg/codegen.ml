open Core

let codegen (cfg : unit Cfg.t) : unit Instr.t list =
  List.map cfg.instructions ~f:(fun label -> match Instr.Label.Map.find cfg.label_to_instr label with
      | Some instr -> instr
      | None -> failwith (Printf.sprintf "codegen: instruction not found: %s" (Instr.Label.to_string label)))
  (* fst (codegen_until cfg cfg.entry_block (-1) IntSet.empty) *)

let cfg_to_func_inst (cfg : unit Cfg.t) : Func_inst.t =
  let body: unit Instr.t list = codegen cfg in
    { idx = cfg.idx;
      name = Some cfg.name;
      type_idx = cfg.type_idx;
      typ = (cfg.arg_types, cfg.return_types);
      code = { locals = cfg.local_types; body }
    }

module Test = struct
  let output_exactly_matches_input (module_str : string) : bool =
    let module_ = Wasm_module.of_string module_str in
    let cfg = Cfg_builder.build module_ 0l in
    let module_ = Wasm_module.replace_func module_ 0l (cfg_to_func_inst cfg) in
    let printed = Wasm_module.to_string module_ in
    if String.equal printed module_str then
      true
    else begin
      Printf.printf "Output does not match, got:\n%s\n" printed;
      false
    end

  let%test "codegen for trivial module should generate all the code" =
    output_exactly_matches_input "(module
  (type (;0;) (func (param i32)))
  (func (;0;) (type 0) (param i32)
    memory.size
    memory.size
    i32.store
    memory.size
    memory.size
    i32.store)
  (table (;0;) 1 1 funcref)
  (memory (;0;) 2)
  (global (;0;) (mut i32) (i32.const 66560))
)"

  let%test "codegen for nested blocks should generate all the code" =
    output_exactly_matches_input "(module
  (type (;0;) (func (param i32) (result i32)))
  (func (;0;) (type 0) (param i32) (result i32)
    block (result i32)
      i32.const 0
      drop
      block
        i32.const 1
        br_if 0
        i32.const 2
        br 1
      end
      i32.const 3
    end)
  (table (;0;) 1 1 funcref)
  (memory (;0;) 2)
  (global (;0;) (mut i32) (i32.const 66560))
)"

  let%test "codegen for memory operations should produce the correct offset and alignment when printed" =
    output_exactly_matches_input "(module
  (type (;0;) (func (param i32) (result i32)))
  (func (;0;) (type 0) (param i32) (result i32)
    i32.const 1812
    i32.const -1
    i32.store offset=12
    i32.const 1812
    i64.const -1
    i64.store offset=8 align=4
    i32.const 1812
    i32.const -1
    i32.store16 align=1
    i32.const 1812
    i32.load16_u align=1)
  (table (;0;) 1 1 funcref)
  (memory (;0;) 2)
  (global (;0;) (mut i32) (i32.const 66560))
)"

  let%test "codegen for consecutive blocks should produce the same result" =
    output_exactly_matches_input "(module
  (type (;0;) (func (param i32)))
  (func (;0;) (type 0) (param i32)
    block
      loop
        i32.const 0
        br_if 1
        i32.const 0
        br_if 0
        block

        end
        block
          i32.const 0
          i32.const 1
          i32.add
          drop
          i32.const 0
          br_if 0
        end
      end
    end
    i32.const 0
    drop
    i32.const 0
    drop)
  (table (;0;) 1 1 funcref)
  (memory (;0;) 2)
  (global (;0;) (mut i32) (i32.const 66560))
)"

  let%test "codegen should support infinite loops" =
    output_exactly_matches_input "(module
  (type (;0;) (func (param i32) (result i32)))
  (func (;0;) (type 0) (param i32) (result i32)
    (local i32)
    i32.const 1
    local.set 1
    block
      loop
        local.get 0
        i32.eqz
        br_if 1
        local.get 1
        local.get 0
        i32.mul
        local.set 1
        local.get 0
        i32.const 1
        i32.sub
        local.set 0
        br 0
      end
    end
    local.get 1)
)"

  let%test "codegen should support empty ifs" =
    output_exactly_matches_input "(module
  (type (;0;) (func (param i32) (result i32)))
  (func (;0;) (type 0) (param i32) (result i32)
    local.get 0
    i32.const -1
    i32.eq
    if

    end
    local.get 0)
)"
  let%test "codegen should support if branches that jump to different places" =
    output_exactly_matches_input "(module
  (type (;0;) (func (param i32) (result i32)))
  (type (;1;) (func (param i32)))
  (func (;0;) (type 0) (param i32) (result i32)
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
    end)
)"
end
