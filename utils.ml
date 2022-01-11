open Core
open Wassail

let int32 = Command.Arg_type.create Int32.of_string

let int32_comma_separated_list =
  Command.Arg_type.create (fun ids ->
      List.map (String.split ids ~on:',') ~f:Int32.of_string)

let on_cfg (file_in : string) (fid : Int32.t) (f : unit Wassail.Cfg.t -> unit) : unit =
  let wasm_mod = Wasm_module.of_file file_in in
  if Int32.(fid < wasm_mod.nfuncimports) then
    Printf.printf "Can't build CFG for function %ld: it is an imported function\n" fid
  else
    f (Wassail.Cfg.without_empty_nodes_with_no_predecessors (Cfg_builder.build wasm_mod fid))

let output_to_file (file_out : string) (content : string) : unit =
  Out_channel.with_file file_out ~f:(fun ch -> Out_channel.output_string ch content)

let load =
  Command.basic
    ~summary:"Load a module and quits"
    Command.Let_syntax.(
      let%map_open file_in = anon ("in" %: string) in
      fun () ->
        try
          let _module : Wasm_module.t = Wasm_module.of_file file_in in
          Printf.printf "Successfully loaded %s\n" file_in
        with e -> Printf.printf "Error when loading %s: %s\n" file_in (Exn.to_string e); exit 1)

let imports =
  Command.basic
    ~summary:"List functions imported by a WebAssembly module"
    Command.Let_syntax.(
      let%map_open file_in = anon ("in" %: string) in
      fun () ->
        let wasm_mod = Wasm_module.of_file file_in in
        List.iter wasm_mod.imported_funcs ~f:(fun (idx, name, ftype) ->
            Printf.printf "%ld\t%s\t%s\n"
              idx name (Type.funtype_to_string ftype)))

let exports =
  Command.basic
    ~summary:"List functions exported by a WebAssembly module"
    Command.Let_syntax.(
      let%map_open file_in = anon ("in" %: string) in
      fun () ->
        let wasm_mod = Wasm_module.of_file file_in in
        List.iter wasm_mod.exported_funcs ~f:(fun (idx, name, ftype) ->
            Printf.printf "%ld\t%s\t%s\n"
              idx name (Type.funtype_to_string ftype)))

let instructions =
  Command.basic
    ~summary:"List instructions used by a WebAssembly module, and how many time each instruction appears"
    Command.Let_syntax.(
      let%map_open file_in = anon ("in" %: string) in
      fun () ->
        let wasm_mod = Wasm_module.of_file file_in in
        StringMap.iteri (Instruction_counter.count wasm_mod) ~f:(fun ~key:instr ~data:count ->
            Printf.printf "%d\t%s\n" count instr))

let sizes =
  Command.basic
    ~summary:"Output the size (in bytes) of each section of a WebAssembly module"
    Command.Let_syntax.(
      let%map_open file_in = anon ("in" %: string) in
      fun () ->
        let wasm_mod = Wasm_module.of_file file_in in
        let sizes = Sizes.generate_binary wasm_mod None in
        Printf.printf "%d\ttype\n" sizes.type_section;
        Printf.printf "%d\timport\n" sizes.import_section;
        Printf.printf "%d\tfunc\n" sizes.func_section;
        Printf.printf "%d\ttable\n" sizes.table_section;
        Printf.printf "%d\tmemory\n" sizes.memory_section;
        Printf.printf "%d\tglobal\n" sizes.global_section;
        Printf.printf "%d\texport\n" sizes.export_section;
        Printf.printf "%d\tstart\n" sizes.start_section;
        Printf.printf "%d\telem\n" sizes.elem_section;
        Printf.printf "%d\tcode\n" sizes.code_section;
        Printf.printf "%d\tdata\n" sizes.data_section)

let mem_imports =
  Command.basic
    ~summary:"Outputs the number of memories exported by this module"
    Command.Let_syntax.(
      let%map_open file_in = anon ("in" %: string) in
      fun () ->
        let wasm_mod = Wasm_module.of_file file_in in
        let count = List.count wasm_mod.imports ~f:(fun import -> match import.idesc with
            | MemoryImport _ -> true
            | _ -> false) in
        Printf.printf "%d\n" count)

let mem_exports =
  Command.basic
    ~summary:"Outputs the number of memories exported by this module"
    Command.Let_syntax.(
      let%map_open file_in = anon ("in" %: string) in
      fun () ->
        let wasm_mod = Wasm_module.of_file file_in in
        let count = List.count wasm_mod.exports ~f:(fun export -> match export.edesc with
            | MemoryExport _ -> true
            | _ -> false) in
        Printf.printf "%d\n" count)

let function_instructions =
  Command.basic
    ~summary:"Returns the labels of instructions of a given function"
    Command.Let_syntax.(
      let%map_open file_in = anon ("in" %: string)
      and fidx = anon ("fidx" %: int32) in
      fun () ->
        let wasm_mod = Wasm_module.of_file file_in in
        let labels = List.fold_left
            (List.find_exn wasm_mod.funcs ~f:(fun f -> Int32.(f.idx = fidx))).code.body
            ~init:Instr.Label.Set.empty
            ~f:(fun acc instr ->
                Instr.Label.Set.union acc (Instr.all_labels_no_blocks_no_merge instr)) in
        Instr.Label.Set.iter labels ~f:(fun label ->
            Printf.printf "%s\n" (Instr.Label.to_string label)))

let functions =
  Command.basic
    ~summary:"Returns the indices of functions of a WebAssembly modules"
    Command.Let_syntax.(
      let%map_open file_in = anon ("in" %: string) in
      fun () ->
        let wasm_mod = Wasm_module.of_file file_in in
        List.iter wasm_mod.funcs
          ~f:(fun f -> Printf.printf "%ld\n" f.idx))

