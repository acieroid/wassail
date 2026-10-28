open Core
open Helpers
open Instr

type node = {
  id : int;
  node_type : string;
  name : string;
  code : string;
  canonical_code : string option;
  line : int;
  filename : string;
  order : int;
  full_name : string option;
  signature : string option;
  is_external : bool option;
  type_full_name : string option;
  method_full_name : string option;
  dispatch_type : string option;
  control_structure_type : string option;
  index : int option;
  language : string option;
  version : string option;
  source_label : string option;
  branch_target_label : string option;
}

type edge = {
  source : int;
  target : int;
  edge_type : string;
}

type t = {
  file : string;
  nodes : node list;
  edges : edge list;
}

let node_to_json (n : node) : Yojson.Safe.t =
  let optional_string name value =
    Option.map value ~f:(fun value -> (name, `String value))
  in
  let optional_bool name value =
    Option.map value ~f:(fun value -> (name, `Bool value))
  in
  let optional_int name value =
    Option.map value ~f:(fun value -> (name, `Int value))
  in
  `Assoc (List.filter_opt [
      Some ("id", `Int n.id);
      Some ("label", `String n.node_type);
      Some ("nodeType", `String n.node_type);
      Some ("name", `String n.name);
      Some ("code", `String n.code);
      optional_string "canonicalCode" n.canonical_code;
      Some ("line", `Int n.line);
      Some ("lineNumber", if n.line < 0 then `Null else `Int n.line);
      Some ("filename", `String n.filename);
      Some ("order", `Int n.order);
      optional_string "fullName" n.full_name;
      optional_string "signature" n.signature;
      optional_bool "isExternal" n.is_external;
      optional_string "typeFullName" n.type_full_name;
      optional_string "methodFullName" n.method_full_name;
      optional_string "dispatchType" n.dispatch_type;
      optional_string "controlStructureType" n.control_structure_type;
      optional_int "index" n.index;
      optional_string "language" n.language;
      optional_string "version" n.version;
      optional_string "sourceLabel" n.source_label;
      optional_string "branchTargetLabel" n.branch_target_label;
    ])

let edge_to_json (e : edge) : Yojson.Safe.t =
  `Assoc [
    ("source", `Int e.source);
    ("target", `Int e.target);
    ("label", `String e.edge_type);
    ("edgeType", `String e.edge_type);
  ]

let to_json (cpg : t) : Yojson.Safe.t =
  `Assoc [
    ("file", `String cpg.file);
    ("nodes", `List (List.map cpg.nodes ~f:node_to_json));
    ("edges", `List (List.map cpg.edges ~f:edge_to_json));
  ]

let to_string (cpg : t) : string =
  Yojson.Safe.pretty_to_string (to_json cpg)

let write_file (filename : string) (cpg : t) : unit =
  Out_channel.with_file filename ~f:(fun ch ->
      Out_channel.output_string ch (to_string cpg);
      Out_channel.output_char ch '\n')

let label_line (instr : 'a Instr.t) : int =
  Instr.line_number instr

let type_list_to_signature_part (types : Type.t list) : string =
  types
  |> List.map ~f:Type.to_string
  |> String.concat ~sep:","

let function_signature (args, returns : Type.t list * Type.t list) : string =
  Printf.sprintf "(%s)->(%s)" (type_list_to_signature_part args) (type_list_to_signature_part returns)

let function_full_name (idx : Int32.t) : string =
  Printf.sprintf "wasm.func_%ld" idx

let type_full_name_of_returns (returns : Type.t list) : string =
  match returns with
  | [] -> "void"
  | [typ] -> Type.to_string typ
  | types -> Printf.sprintf "(%s)" (type_list_to_signature_part types)

let node_type_of_instr (instr : 'a Instr.t) : string =
  match instr with
  | Control { instr = Block _; _ } -> "BLOCK"
  | Control { instr = Loop _ | If _ | Br _ | BrIf _ | BrTable _ | Return | Unreachable; _ } ->
    "CONTROL_STRUCTURE"
  | Control { instr = Merge; _ } -> "CONTROL_STRUCTURE"
  | Data { instr = Const _; _ } -> "LITERAL"
  | Data _ | Call _ -> "CALL"

let import_name (module_ : Wasm_module.t) (idx : Int32.t) : string option =
  List.find_map module_.imported_funcs ~f:(fun desc ->
      if Int32.equal desc.idx idx then Some desc.name else None)

let function_name (func : Func_inst.t) : string =
  Option.value func.name ~default:(Printf.sprintf "func_%ld" func.idx)

let instr_name (module_ : Wasm_module.t) (instr : 'a Instr.t) : string =
  match instr with
  | Call { instr = CallDirect (_, _, idx); _ } ->
    Option.value (import_name module_ idx) ~default:(Instr.to_mnemonic instr)
  | _ -> Instr.to_mnemonic instr

let instr_method_full_name (_module_ : Wasm_module.t) (instr : 'a Instr.t) : string option =
  match instr with
  | Call { instr = CallDirect (_, _, idx); _ } -> Some (function_full_name idx)
  | Call { instr = CallIndirect _; _ } -> Some "<operator>.call_indirect"
  | Data _ | Control _ -> None

let instr_signature (module_ : Wasm_module.t) (instr : 'a Instr.t) : string option =
  match instr with
  | Call { instr = CallDirect (_, typ, _); _ } -> Some (function_signature typ)
  | Call { instr = CallIndirect (_, _, typ, _); _ } -> Some (function_signature typ)
  | Data _ | Control _ ->
    let arity_in, arity_out =
      match instr with
      | Data d ->
        begin match d.instr with
        | Const v -> ([], [Prim_value.typ v])
        | _ -> ([], [])
        end
      | Control _ | Call _ -> ([], [])
    in
    ignore module_;
    ignore arity_in;
    ignore arity_out;
    None

let control_structure_type (instr : 'a Instr.t) : string option =
  match instr with
  | Control { instr = If _; _ } -> Some "IF"
  | Control { instr = Loop _; _ } -> Some "WHILE"
  | Control { instr = Br _; _ } -> Some "BREAK"
  | Control { instr = BrIf _; _ } -> Some "BREAK"
  | Control { instr = BrTable _; _ } -> Some "SWITCH"
  | Control { instr = Return; _ } -> Some "RETURN"
  | Control { instr = Unreachable; _ } -> Some "THROW"
  | Control { instr = Block _ | Merge; _ } | Data _ | Call _ -> None

let instr_code (instr : 'a Instr.t) : string =
  match instr with
  | Control c -> Instr.control_to_short_string c.instr
  | _ -> Instr.to_string instr ~sep:" " ~indent:0 ~annot_str:(fun _ -> "")

type source_instr = {
  source_mnemonic : string;
  source_label : string option;
  branch_target_labels : string list;
}

let strip_source_label (label : string) : string =
  String.drop_prefix label 1

let source_label_token (token : string) : string option =
  if String.is_prefix token ~prefix:"$" && String.length token > 1
  then Some (strip_source_label token)
  else None

let is_source_instr_mnemonic = function
  | "func" | "block" | "loop" | "if" | "br" | "br_if" | "br_table" -> true
  | _ -> false

let normalize_source_token (token : string) : string =
  String.strip token ~drop:(function
      | '(' | ')' -> true
      | _ -> false)

let source_tokens_of_line (line : string) : string list =
  let line =
    match String.substr_index line ~pattern:";;" with
    | Some idx -> String.prefix line idx
    | None -> line
  in
  String.split_on_chars line ~on:[' '; '\t'; '\r'; '\n']
  |> List.map ~f:normalize_source_token
  |> List.filter ~f:(fun token -> not (String.is_empty token))

let source_instrs_of_line (line : string) : source_instr list =
  let tokens = source_tokens_of_line line in
  let rec collect_targets acc = function
    | token :: rest when is_source_instr_mnemonic token -> List.rev acc, token :: rest
    | token :: rest ->
      let acc =
        match source_label_token token with
        | Some label -> label :: acc
        | None -> acc
      in
      collect_targets acc rest
    | [] -> List.rev acc, []
  in
  let rec loop acc = function
    | [] -> List.rev acc
    | mnemonic :: rest when is_source_instr_mnemonic mnemonic ->
      let source_label, rest =
        match rest with
        | token :: rest ->
          begin match source_label_token token with
          | Some label when String.equal mnemonic "func"
                         || String.equal mnemonic "block"
                         || String.equal mnemonic "loop"
                         || String.equal mnemonic "if" ->
            Some label, rest
          | _ -> None, token :: rest
          end
        | [] -> None, []
      in
      let branch_target_labels, rest =
        match mnemonic with
        | "br" | "br_if" | "br_table" -> collect_targets [] rest
        | _ -> [], rest
      in
      loop ({ source_mnemonic = mnemonic; source_label; branch_target_labels } :: acc) rest
    | _ :: rest -> loop acc rest
  in
  loop [] tokens

let source_instrs_of_lines (lines : string list) : source_instr list IntMap.t =
  lines
  |> List.mapi ~f:(fun idx line -> idx + 1, source_instrs_of_line line)
  |> List.filter ~f:(fun (_, instrs) -> not (List.is_empty instrs))
  |> IntMap.of_alist_exn

let source_instrs_of_file (file : string) : source_instr list IntMap.t =
  match Stdlib.Filename.extension file with
  | ".wat" ->
    begin try source_instrs_of_lines (In_channel.read_lines file) with _ -> IntMap.empty end
  | _ -> IntMap.empty

let source_instrs_of_string (source : string) : source_instr list IntMap.t =
  source_instrs_of_lines (String.split_lines source)

let source_mnemonic_of_instr (instr : 'a Instr.t) : string =
  match instr with
  | Control { instr = Block _; _ } -> "block"
  | Control { instr = Loop _; _ } -> "loop"
  | Control { instr = If _; _ } -> "if"
  | Control { instr = Br _; _ } -> "br"
  | Control { instr = BrIf _; _ } -> "br_if"
  | Control { instr = BrTable _; _ } -> "br_table"
  | _ -> Instr.to_mnemonic instr

let display_fields_of_instr (module_ : Wasm_module.t) (source : source_instr option)
    (instr : 'a Instr.t) : string * string * string option * string option * string option =
  let canonical_code = instr_code instr in
  let canonical_name = instr_name module_ instr in
  match source, instr with
  | Some { source_label = Some label; _ }, Control { instr = Block _ | Loop _ | If _; _ } ->
    let mnemonic = source_mnemonic_of_instr instr in
    label, Printf.sprintf "%s $%s" mnemonic label, Some canonical_code, Some label, None
  | Some { branch_target_labels = labels; _ }, Control { instr = Br _ | BrIf _ | BrTable _; _ }
    when not (List.is_empty labels) ->
    let mnemonic = source_mnemonic_of_instr instr in
    let source_targets = List.map labels ~f:(Printf.sprintf "$%s") in
    canonical_name,
    String.concat ~sep:" " (mnemonic :: source_targets),
    Some canonical_code,
    None,
    Some (String.concat ~sep:" " labels)
  | _ ->
    canonical_name, canonical_code, Some canonical_code, None, None

let arity (instr : 'a Instr.t) : int * int =
  match instr with
  | Data d ->
    begin match d.instr with
    | Nop -> (0, 0)
    | Drop -> (1, 0)
    | Select _ -> (3, 1)
    | MemorySize -> (0, 1)
    | MemoryGrow -> (1, 1)
    | MemoryFill | MemoryCopy | MemoryInit _ -> (3, 0)
    | Const _ -> (0, 1)
    | Unary _ | Test _ | Convert _ | Load _ | RefIsNull -> (1, 1)
    | Binary _ | Compare _ -> (2, 1)
    | LocalGet _ | GlobalGet _ | RefNull _ | RefFunc _ -> (0, 1)
    | LocalSet _ | GlobalSet _ -> (1, 0)
    | LocalTee _ -> (1, 1)
    | Store _ -> (2, 0)
    end
  | Call c ->
    begin match c.instr with
    | CallDirect ((arity_in, arity_out), _, _) -> (arity_in, arity_out)
    | CallIndirect (_, (arity_in, arity_out), _, _) -> (arity_in + 1, arity_out)
    end
  | Control c ->
    begin match c.instr with
    | Block (_, (arity_in, arity_out), _) -> (arity_in, arity_out)
    | Loop (_, (arity_in, arity_out), _) -> (arity_in, arity_out)
    | If (_, (arity_in, arity_out), _, _) -> (arity_in + 1, arity_out)
    | BrIf _ | BrTable _ -> (1, 0)
    | Return | Br _ | Unreachable | Merge -> (0, 0)
    end

let pop_operands (stack : 'a list) (n : int) : 'a list * 'a list =
  let operands = List.take stack n |> List.rev in
  let stack = List.drop stack n in
  operands, stack

module Build = struct
  type state = {
    next_id : int;
    nodes_rev : node list;
    edges_rev : edge list;
    label_to_node : int Instr.Label.Map.t;
    filename : string;
    source_instrs_by_line : source_instr list IntMap.t;
  }

  let empty ?(source_instrs_by_line = IntMap.empty) (filename : string) : state = {
    next_id = 1;
    nodes_rev = [];
    edges_rev = [];
    label_to_node = Instr.Label.Map.empty;
    filename;
    source_instrs_by_line;
  }

  let add_node ?full_name ?signature ?is_external ?type_full_name ?method_full_name ?dispatch_type
      ?control_structure_type ?index ?language ?version ?canonical_code ?source_label ?branch_target_label
      (state : state) (node_type : string) (name : string)
      (code : string) (line : int) : state * int =
    let id = state.next_id in
    { state with
      next_id = state.next_id + 1;
      nodes_rev = {
        id;
        node_type;
        name;
        code;
        canonical_code;
        line;
        filename = state.filename;
        order = id - 1;
        full_name;
        signature;
        is_external;
        type_full_name;
        method_full_name;
        dispatch_type;
        control_structure_type;
        index;
        language;
        version;
        source_label;
        branch_target_label;
      } :: state.nodes_rev;
    }, id

  let consume_source_instr (state : state) (instr : unit Instr.t) : state * source_instr option =
    let line = label_line instr in
    let mnemonic = source_mnemonic_of_instr instr in
    let rec pick skipped = function
      | [] -> None, List.rev skipped
      | source :: rest when String.equal source.source_mnemonic mnemonic ->
        Some source, List.rev_append skipped rest
      | source :: rest -> pick (source :: skipped) rest
    in
    match IntMap.find state.source_instrs_by_line line with
    | None -> state, None
    | Some sources ->
      let source, remaining = pick [] sources in
      let source_instrs_by_line =
        if List.is_empty remaining
        then IntMap.remove state.source_instrs_by_line line
        else IntMap.set state.source_instrs_by_line ~key:line ~data:remaining
      in
      { state with source_instrs_by_line }, source

  let add_edge (state : state) (source : int) (target : int) (edge_type : string) : state =
    { state with edges_rev = { source; target; edge_type } :: state.edges_rev }

  let add_label (state : state) (label : Instr.Label.t) (node_id : int) : state =
    { state with label_to_node = Instr.Label.Map.set state.label_to_node ~key:label ~data:node_id }
end

let add_ast_edges (state : Build.state) (source : int) (targets : int list) : Build.state =
  List.fold_left targets ~init:state ~f:(fun state target ->
      Build.add_edge state source target "AST")

let rec add_instr (module_ : Wasm_module.t) (parent : int) (state : Build.state) (stack : int list)
    (instr : unit Instr.t) : Build.state * int list =
  let state, source = Build.consume_source_instr state instr in
  let name, code, canonical_code, source_label, branch_target_label =
    display_fields_of_instr module_ source instr
  in
  let state, node_id =
    Build.add_node
      ?signature:(instr_signature module_ instr)
      ?method_full_name:(instr_method_full_name module_ instr)
      ?dispatch_type:(Option.map (instr_method_full_name module_ instr) ~f:(fun _ -> "STATIC_DISPATCH"))
      ?control_structure_type:(control_structure_type instr)
      ?canonical_code
      ?source_label
      ?branch_target_label
      state
      (node_type_of_instr instr)
      name
      code
      (label_line instr)
  in
  let state = Build.add_label state (Instr.label instr) node_id in
  match instr with
  | Control c ->
    begin match c.instr with
    | Block (_, (_, arity_out), body)
    | Loop (_, (_, arity_out), body) ->
      let operands, stack = pop_operands stack (fst (arity instr)) in
      let state = add_ast_edges state node_id operands in
      let state, _ = add_instrs module_ node_id state stack body in
      let state = if arity_out = 0 then Build.add_edge state parent node_id "AST" else state in
      let stack = if arity_out = 0 then stack else List.init arity_out ~f:(fun _ -> node_id) @ stack in
      state, stack
    | If (_, (_, arity_out), then_, else_) ->
      let operands, stack = pop_operands stack (fst (arity instr)) in
      let state = add_ast_edges state node_id operands in
      let state, _ = add_instrs module_ node_id state stack then_ in
      let state, _ = add_instrs module_ node_id state stack else_ in
      let state = if arity_out = 0 then Build.add_edge state parent node_id "AST" else state in
      let stack = if arity_out = 0 then stack else List.init arity_out ~f:(fun _ -> node_id) @ stack in
      state, stack
    | BrIf _ | BrTable _ ->
      let operands, stack = pop_operands stack (fst (arity instr)) in
      let state = add_ast_edges state node_id operands in
      let state = Build.add_edge state parent node_id "AST" in
      state, stack
    | Br _ | Return | Unreachable | Merge ->
      let state = Build.add_edge state parent node_id "AST" in
      state, stack
    end
  | Data _ | Call _ ->
    let arity_in, arity_out = arity instr in
    let operands, stack = pop_operands stack arity_in in
    let state = add_ast_edges state node_id operands in
    let state = if arity_out = 0 then Build.add_edge state parent node_id "AST" else state in
    let stack = if arity_out = 0 then stack else List.init arity_out ~f:(fun _ -> node_id) @ stack in
    state, stack

and add_instrs (module_ : Wasm_module.t) (parent : int) (state : Build.state) (stack : int list)
    (instrs : unit Instr.t list) : Build.state * int list =
  let initial_stack_size = List.length stack in
  let state, stack =
    List.fold_left instrs ~init:(state, stack) ~f:(fun (state, stack) instr ->
        add_instr module_ parent state stack instr)
  in
  let produced = Int.max 0 (List.length stack - initial_stack_size) in
  let roots = List.take stack produced |> List.rev in
  add_ast_edges state parent roots, stack

let block_labels (block : 'a Basic_block.t) : Instr.Label.t list =
  match block.content with
  | Data instrs -> List.map instrs ~f:(fun i -> i.label)
  | Control { instr = Merge; _ } | Entry | Return _ | Imported _ -> []
  | Control i -> [i.label]
  | Call i -> [i.label]

let block_first_label (block : 'a Basic_block.t) : Instr.Label.t option =
  List.hd (block_labels block)

let block_last_label (block : 'a Basic_block.t) : Instr.Label.t option =
  List.last (block_labels block)

let find_first_concrete_label (cfg : 'a Cfg.t) (start : int) : Instr.Label.t option =
  let rec loop visited work =
    match work with
    | [] -> None
    | idx :: rest when IntSet.mem visited idx -> loop visited rest
    | idx :: rest ->
      let visited = IntSet.add visited idx in
      let block = Cfg.find_block_exn cfg idx in
      match block_first_label block with
      | Some label -> Some label
      | None ->
        let succs = Cfg.successors cfg idx in
        loop visited (rest @ succs)
  in
  loop IntSet.empty [start]

let add_dedup_edge (seen : StringSet.t ref) (edges : edge list ref) (edge : edge) : unit =
  let key = Int.to_string edge.source ^ "\000" ^ Int.to_string edge.target ^ "\000" ^ edge.edge_type in
  if not (StringSet.mem !seen key) then begin
    seen := StringSet.add !seen key;
    edges := edge :: !edges
  end

let add_cfg_edges (cfg : Spec_domain.t Cfg.t) (label_to_node : int Instr.Label.Map.t)
    (seen : StringSet.t ref) (edges : edge list ref) : unit =
  let rec adjacent_pairs labels =
    match labels with
    | left :: (right :: _ as rest) -> (left, right) :: adjacent_pairs rest
    | [] | [_] -> []
  in
  IntMap.iteri cfg.basic_blocks ~f:(fun ~key:_ ~data:block ->
      match block_labels block with
      | [] | [_] -> ()
      | labels ->
        List.iter (adjacent_pairs labels) ~f:(fun (src_label, dst_label) ->
            match Instr.Label.Map.find label_to_node src_label, Instr.Label.Map.find label_to_node dst_label with
            | Some source, Some target -> add_dedup_edge seen edges { source; target; edge_type = "CFG" }
            | _ -> ()));
  IntMap.iteri cfg.edges ~f:(fun ~key:src ~data:dsts ->
      let src_label = block_last_label (Cfg.find_block_exn cfg src) in
      Cfg.Edge.Set.iter dsts ~f:(fun (dst, _) ->
          match src_label, find_first_concrete_label cfg dst with
          | Some src_label, Some dst_label ->
            begin match Instr.Label.Map.find label_to_node src_label, Instr.Label.Map.find label_to_node dst_label with
            | Some source, Some target -> add_dedup_edge seen edges { source; target; edge_type = "CFG" }
            | _ -> ()
            end
          | _ -> ()))

let add_reaching_defs (module_ : Wasm_module.t) (cfg : Spec_domain.t Cfg.t)
    (label_to_node : int Instr.Label.Map.t) (seen : StringSet.t ref) (edges : edge list ref) : unit =
  begin match try Some (Use_def.make module_ cfg) with _ -> None with
  | Some (_, _, chains) ->
    Use_def.Use.Map.iteri chains ~f:(fun ~key:use ~data:def ->
        match def with
        | Use_def.Def.Instruction (def_label, _) ->
          begin match Instr.Label.Map.find label_to_node def_label, Instr.Label.Map.find label_to_node use.label with
          | Some source, Some target -> add_dedup_edge seen edges { source; target; edge_type = "REACHING_DEF" }
          | _ -> ()
          end
        | Entry _ | Constant _ -> ())
  | None ->
    Log.warn (fun () ->
        Printf.sprintf "Skipping SSA reaching-def edges for function %ld because use-def analysis failed" cfg.idx)
  end;
  let mem_deps = Memory_deps.make cfg in
  Instr.Label.Map.iteri mem_deps ~f:(fun ~key:use_label ~data:defs ->
      Instr.Label.Set.iter defs ~f:(fun def_label ->
          match Instr.Label.Map.find label_to_node def_label, Instr.Label.Map.find label_to_node use_label with
          | Some source, Some target -> add_dedup_edge seen edges { source; target; edge_type = "REACHING_DEF" }
          | _ -> ()))

let add_lexical_local_reaching_defs (instrs : unit Instr.t list) (label_to_node : int Instr.Label.Map.t)
    (seen : StringSet.t ref) (edges : edge list ref) : unit =
  let add_edge defs use_label local =
    match Int32Map.find defs local, Instr.Label.Map.find label_to_node use_label with
    | Some def_label, Some target ->
      begin match Instr.Label.Map.find label_to_node def_label with
      | Some source -> add_dedup_edge seen edges { source; target; edge_type = "REACHING_DEF" }
      | None -> ()
      end
    | _ -> ()
  in
  let rec loop defs instrs =
    List.fold_left instrs ~init:defs ~f:(fun defs instr ->
        match instr with
        | Data { instr = LocalGet local; label; _ } ->
          add_edge defs label local;
          defs
        | Data { instr = LocalSet local | LocalTee local; label; _ } ->
          Int32Map.set defs ~key:local ~data:label
        | Control { instr = Block (_, _, body) | Loop (_, _, body); _ } ->
          loop defs body
        | Control { instr = If (_, _, then_, else_); _ } ->
          let defs_then = loop defs then_ in
          let defs_else = loop defs else_ in
          Int32Map.merge defs_then defs_else ~f:(fun ~key:_ -> function
              | `Both (left, right) when Instr.Label.equal left right -> Some left
              | `Left label | `Right label -> Some label
              | `Both (_, right) -> Some right)
        | Data _ | Call _ | Control _ -> defs)
  in
  ignore (loop Int32Map.empty instrs : Instr.Label.t Int32Map.t)

let add_cdg_edges (cfg : Spec_domain.t Cfg.t) (label_to_node : int Instr.Label.Map.t)
    (seen : StringSet.t ref) (edges : edge list ref) : unit =
  let deps = Control_deps.control_deps_exact_instrs cfg in
  Instr.Label.Map.iteri deps ~f:(fun ~key:dependent ~data:controllers ->
      Instr.Label.Set.iter controllers ~f:(fun controller ->
          match Instr.Label.Map.find label_to_node controller, Instr.Label.Map.find label_to_node dependent with
          | Some source, Some target -> add_dedup_edge seen edges { source; target; edge_type = "CDG" }
          | _ -> ()))

let analyzed_cfg (module_ : Wasm_module.t) (idx : Int32.t) : Spec_domain.t Cfg.t =
  Spec_analysis.analyze_intra1 module_ idx
  |> Cfg.without_empty_nodes_with_no_predecessors

let add_import_method (state : Build.state) (desc : Wasm_module.func_desc) : Build.state =
  let state, _ =
    Build.add_node
      ~full_name:(function_full_name desc.idx)
      ~signature:(function_signature (desc.arguments, desc.returns))
      ~is_external:true
      state
      "METHOD"
      desc.name
      (Printf.sprintf "func %ld" desc.idx)
      (-1)
  in
  state

let add_method_parameters (state : Build.state) (method_id : int) (args : Type.t list) : Build.state =
  List.foldi args ~init:state ~f:(fun index state typ ->
      let state, param_id =
        Build.add_node
          ~type_full_name:(Type.to_string typ)
          ~index:(index + 1)
          state
          "METHOD_PARAMETER_IN"
          (Printf.sprintf "param_%d" index)
          (Printf.sprintf "param %d" index)
          (-1)
      in
      Build.add_edge state method_id param_id "AST")

let add_method_return (state : Build.state) (method_id : int) (returns : Type.t list) : Build.state =
  let state, return_id =
    Build.add_node
      ~type_full_name:(type_full_name_of_returns returns)
      state
      "METHOD_RETURN"
      "RET"
      "RET"
      (-1)
  in
  Build.add_edge state method_id return_id "AST"

let of_module ?(source_instrs_by_line = IntMap.empty) (file : string) (module_ : Wasm_module.t) : t =
  let filename = Filename.basename file in
  let state = Build.empty ~source_instrs_by_line filename in
  let state, _ =
    Build.add_node
      ~language:"WASM"
      ~version:"1.1"
      state
      "META_DATA"
      "WASM"
      "WASM"
      (-1)
  in
  let state, _ =
    Build.add_node state "FILE" filename filename (-1)
  in
  let state =
    List.fold_left module_.imported_funcs ~init:state ~f:add_import_method
  in
  let state, cfgs =
    List.fold_left module_.funcs
      ~init:(state, [])
      ~f:(fun (state, cfgs) func ->
          let cfg = analyzed_cfg module_ func.idx in
          let method_name = function_name func in
          let state, method_id =
            Build.add_node
              ~full_name:(function_full_name func.idx)
              ~signature:(function_signature func.typ)
              ~is_external:false
              state
              "METHOD"
              method_name
              (Printf.sprintf "func %ld" func.idx)
              (-1)
          in
          let state = add_method_parameters state method_id (fst func.typ) in
          let state = add_method_return state method_id (snd func.typ) in
          let state, _ = add_instrs module_ method_id state [] func.code.body in
          state, (cfg, state.label_to_node, func.code.body) :: cfgs)
  in
  let seen = ref StringSet.empty in
  let edges = ref (List.rev state.edges_rev) in
  List.iter cfgs ~f:(fun (cfg, label_to_node, body) ->
      add_cfg_edges cfg label_to_node seen edges;
      add_reaching_defs module_ cfg label_to_node seen edges;
      add_lexical_local_reaching_defs body label_to_node seen edges;
      add_cdg_edges cfg label_to_node seen edges);
  let edges =
    (!edges @ [])
    |> List.rev
    |> List.dedup_and_sort ~compare:(fun a b ->
        [%compare: int * int * string] (a.source, a.target, a.edge_type) (b.source, b.target, b.edge_type))
  in
  { file = filename; nodes = List.rev state.nodes_rev; edges }

let of_file (file : string) : t =
  of_module ~source_instrs_by_line:(source_instrs_of_file file) file (Wasm_module.of_file file)

module Test = struct
  let json_of_string (source : string) : t =
    of_module ~source_instrs_by_line:(source_instrs_of_string source) "test.wat" (Wasm_module.of_string source)

  let edges_of_type cpg edge_type =
    List.filter cpg.edges ~f:(fun e -> String.equal e.edge_type edge_type)

  let node_by_code cpg code =
    List.find_exn cpg.nodes ~f:(fun n -> String.equal n.code code)

  let node_by_type_and_name cpg node_type name =
    List.find_exn cpg.nodes ~f:(fun n ->
        String.equal n.node_type node_type && String.equal n.name name)

  let edge_triples cpg =
    cpg.edges
    |> List.map ~f:(fun e -> e.source, e.target, e.edge_type)
    |> List.sort ~compare:[%compare: int * int * string]

  let has_edge cpg edge_type source_code target_code =
    let source = (node_by_code cpg source_code).id in
    let target = (node_by_code cpg target_code).id in
    List.exists cpg.edges ~f:(fun e ->
        String.equal e.edge_type edge_type &&
        Int.equal e.source source &&
        Int.equal e.target target)

  let has_edge_between_any cpg edge_type source_code target_code =
    let sources =
      cpg.nodes
      |> List.filter ~f:(fun n -> String.equal n.code source_code)
      |> List.map ~f:(fun n -> n.id)
    in
    let targets =
      cpg.nodes
      |> List.filter ~f:(fun n -> String.equal n.code target_code)
      |> List.map ~f:(fun n -> n.id)
    in
    List.exists cpg.edges ~f:(fun e ->
        String.equal e.edge_type edge_type &&
        List.mem sources e.source ~equal:Int.equal &&
        List.mem targets e.target ~equal:Int.equal)

  let%test_unit "simple stack expression AST" =
    let cpg = json_of_string "(module
      (type (func (param i32) (result i32)))
      (func (type 0) (param i32) (result i32)
        local.get 0
        i32.const 1
        i32.add))" in
    [%test_result: bool] (has_edge cpg "AST" "i32.add" "local.get 0") ~expect:true;
    [%test_result: bool] (has_edge cpg "AST" "i32.add" "i32.const 1") ~expect:true

  let%test_unit "br_if condition AST and CDG" =
    let cpg = json_of_string "(module
      (type (func (param i32) (result i32)))
      (func (type 0) (param i32) (result i32)
        block
          i32.const 1
          br_if 0
          i32.const 2
          drop
        end
        local.get 0))" in
    [%test_result: bool] (has_edge cpg "AST" "br_if 0" "i32.const 1") ~expect:true;
    [%test_result: bool] (not (List.is_empty (edges_of_type cpg "CDG"))) ~expect:true

  let%test_unit "wat source block labels are restored in presentation fields" =
    let labeled = json_of_string "(module
      (type (func))
      (func (type 0)
        loop $L4
          block $B5
            i32.const 1
            br_if $B5
          end
        end))" in
    let canonical = json_of_string "(module
      (type (func))
      (func (type 0)
        loop
          block
            i32.const 1
            br_if 0
          end
        end))" in
    let block = node_by_type_and_name labeled "BLOCK" "B5" in
    let branch = node_by_code labeled "br_if $B5" in
    [%test_result: string] block.code ~expect:"block $B5";
    [%test_result: string option] block.source_label ~expect:(Some "B5");
    [%test_result: string option] block.canonical_code ~expect:(Some "block");
    [%test_result: string] branch.code ~expect:"br_if $B5";
    [%test_result: string option] branch.branch_target_label ~expect:(Some "B5");
    [%test_result: string option] branch.canonical_code ~expect:(Some "br_if 0");
    [%test_result: (int * int * string) list] (edge_triples labeled) ~expect:(edge_triples canonical)

  let%test_unit "unlabeled wat keeps canonical branch text" =
    let cpg = json_of_string "(module
      (type (func))
      (func (type 0)
        block
          i32.const 1
          br_if 0
        end))" in
    let block = node_by_code cpg "block" in
    let branch = node_by_code cpg "br_if 0" in
    [%test_result: string] block.name ~expect:"block";
    [%test_result: string option] block.source_label ~expect:None;
    [%test_result: string option] block.canonical_code ~expect:(Some "block");
    [%test_result: string option] branch.branch_target_label ~expect:None;
    [%test_result: string option] branch.canonical_code ~expect:(Some "br_if 0")

  let%test_unit "local reaching definition" =
    let cpg = json_of_string "(module
      (type (func (param i32) (result i32)))
      (func (type 0) (param i32) (result i32)
        (local i32)
        i32.const 7
        local.tee 1
        drop
        local.get 1))" in
    [%test_result: bool] (has_edge cpg "REACHING_DEF" "local.tee 1" "local.get 1") ~expect:true

  let%test_unit "memory reaching definition" =
    let cpg = json_of_string "(module
      (type (func (result i32)))
      (func (type 0) (result i32)
        i32.const 0
        i32.const 42
        i32.store
        i32.const 0
        i32.load)
      (memory 1))" in
    [%test_result: bool] (has_edge cpg "REACHING_DEF" "i32.store" "i32.load") ~expect:true

  let%test_unit "whole module with imported call" =
    let cpg = json_of_string "(module
      (type (func (result i32)))
      (import \"env\" \"getchar\" (func (type 0)))
      (func (type 0) (result i32) call 0)
      (func (type 0) (result i32) i32.const 0))" in
    [%test_result: int] (List.length (List.filter cpg.nodes ~f:(fun n ->
        String.equal n.node_type "METHOD" && Option.equal Bool.equal n.is_external (Some false)))) ~expect:2;
    [%test_result: int] (List.length (List.filter cpg.nodes ~f:(fun n ->
        String.equal n.node_type "METHOD" && Option.equal Bool.equal n.is_external (Some true)))) ~expect:1;
    [%test_result: bool] (List.exists cpg.nodes ~f:(fun n ->
        String.equal n.code "call 0" && String.equal n.name "getchar")) ~expect:true

  let%test_unit "deterministic output" =
    let source = "(module
      (type (func (result i32)))
      (func (type 0) (result i32) i32.const 0))" in
    [%test_result: string] (to_string (json_of_string source)) ~expect:(to_string (json_of_string source))

  let%test_unit "joern-like schema fields are emitted" =
    let cpg = json_of_string "(module
      (type (func (param i32) (result i32)))
      (func (export \"main\") (type 0) (param i32) (result i32)
        local.get 0))" in
    [%test_result: bool] (List.exists cpg.nodes ~f:(fun n ->
        String.equal n.node_type "META_DATA" &&
        Option.equal String.equal n.language (Some "WASM") &&
        Option.equal String.equal n.version (Some "1.1"))) ~expect:true;
    [%test_result: bool] (List.exists cpg.nodes ~f:(fun n ->
        String.equal n.node_type "FILE" && String.equal n.filename "test.wat")) ~expect:true;
    [%test_result: bool] (List.exists cpg.nodes ~f:(fun n ->
        String.equal n.node_type "METHOD" &&
        String.equal n.name "main" &&
        Option.equal String.equal n.full_name (Some "wasm.func_0") &&
        Option.equal String.equal n.signature (Some "(i32)->(i32)") &&
        Option.equal Bool.equal n.is_external (Some false))) ~expect:true;
    [%test_result: bool] (List.exists cpg.nodes ~f:(fun n ->
        String.equal n.node_type "METHOD_PARAMETER_IN" &&
        Option.equal Int.equal n.index (Some 1) &&
        Option.equal String.equal n.type_full_name (Some "i32"))) ~expect:true;
    [%test_result: bool] (List.exists cpg.nodes ~f:(fun n ->
        String.equal n.node_type "METHOD_RETURN" &&
        Option.equal String.equal n.type_full_name (Some "i32"))) ~expect:true

  let%test_unit "loop fixture with unreachable analysis still exports" =
    let cpg = of_file "../../../test/loop.wat" in
    [%test_result: bool] (not (List.is_empty cpg.nodes)) ~expect:true;
    [%test_result: bool] (not (List.is_empty (edges_of_type cpg "AST"))) ~expect:true;
    [%test_result: bool] (not (List.is_empty (edges_of_type cpg "CFG"))) ~expect:true

  let%test_unit "get_token-shaped fixture has deterministic CPG edges" =
    let cpg = of_file "../../../test/cpg_get_token.wat" in
    let json = to_string cpg in
    let reparsed = Yojson.Safe.from_string json in
    [%test_result: bool] (Yojson.Safe.equal reparsed (to_json cpg)) ~expect:true;
    List.iter ["AST"; "CFG"; "REACHING_DEF"; "CDG"] ~f:(fun edge_type ->
        [%test_result: bool] (not (List.is_empty (edges_of_type cpg edge_type))) ~expect:true);
    [%test_result: bool] (has_edge_between_any cpg "REACHING_DEF" "local.tee 3" "local.get 3") ~expect:true;
    [%test_result: string] json ~expect:(to_string (of_file "../../../test/cpg_get_token.wat"))
end
