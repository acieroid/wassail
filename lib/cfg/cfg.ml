open Core
open Helpers

module Edge = struct
  module T = struct
    (* An edge has an id and can be annotated with a boolean (to indicate a then/else) *)
    type t = int * bool option
    [@@deriving sexp, compare, equal]
  end
  include T
  let to_string (e : t) : string = string_of_int (fst e)
  module Set = struct
    include Set
    include Set.Make(T)
  end
end

module Edges = struct
  module T = struct
    type t = Edge.Set.t IntMap.t
    [@@deriving sexp, compare, equal]
  end
  include T
  let from (edges : t) (idx : int) : Edge.t list =
    match IntMap.find edges idx with
    | Some es -> Edge.Set.to_list es
    | None -> []

  let find_exn (edges : t) (src : int) (dst : int) : Edge.t =
    List.find_exn (from edges src) ~f:(fun (idx, _) -> idx = dst)

  let add (edges : t) (from : int) (edge : Edge.t) : t =
    IntMap.update edges from ~f:(function
        | None -> Edge.Set.singleton edge
        | Some es -> Edge.Set.add es edge)

  let remove (edges : t) (from : int) (to_ : int) : t =
    IntMap.update edges from ~f:(function
        | None -> Edge.Set.empty
        | Some es -> Edge.Set.filter es ~f:(fun (dst, _) -> not (dst = to_)))

  let remove_from (edges : t) (from : int) : t =
    IntMap.remove edges from

end

module Cfg = struct

  module BlockIdx = struct
    type t = int
    [@@deriving sexp, compare, equal]
    let to_string = string_of_int
    module Set = IntSet
    module Map = IntMap
  end

  type 'a t = {
    (** Is this an exported function? *)
    exported: bool;
    (** The name of the function *)
    name: string;
    (** The index of the function (each function has a unique index in
        WebAssembly) *)
    idx: Int32.t;
    (** The index of the type of the function. For simplicity, one can directly
        access the type of the function using fields [arg_types] and
        [return_types] *)
    type_idx: Int32.t;
    (** The types of the globals (for the entire program) *)
    global_types: Type.t list;
    (** The types of this function's locals *)
    local_types: Type.t list;
    (** The type of this function's arguments *)
    arg_types: Type.t list;
    (** The return types of this function. Usually zero or one type. *)
    return_types: Type.t list;
    (** The basic blocks, indexed by their internal id *)
    basic_blocks: 'a Basic_block.t IntMap.t;
    (** The (forward) edges, mapping from basic blocks to outgoing edges *)
    edges: Edges.t;
    (** Like [edges], but in reverse. *)
    back_edges: Edges.t;
    (** The id of the entry block of this function *)
    entry_block: int;
    (** The id of the exit block of this function *)
    exit_block: int;
    (** The blocks that are considered loop heads. Used for widening *)
    loop_heads: IntSet.t;
    (** All the labels of instructions in the function. A label is an unique identifier for instructions *)
    instructions: Instr.Label.t list;
    (** Maps labels to their actual instructions *)
    label_to_instr: unit Instr.t Instr.Label.Map.t;
    (** The arity of each block (WebAssembly block, so loop and block
        instructions, not basic block), indexed by the label of the block. The
        arity is the number of values expected on the stack before the block,
        and the number of values remaining on the stack after the execution of
        block *)
    block_arities: (int * int) Instr.Label.Map.t;
    (** Maps labels to the label of the block that contains the corresponding instruction *)
    label_to_enclosing_block: Instr.Label.t Instr.Label.Map.t;
    (** Maps labels to the id of the block that contains them *)
    label_to_enclosing_block_id: int Instr.Label.Map.t;
  }
  [@@deriving compare, equal]

  let name (cfg : 'a t) : string =
    cfg.name

  let global_types (cfg : 'a t) : Type.t list =
    cfg.global_types

  let local_types (cfg : 'a t) : Type.t list =
    cfg.local_types

  let arg_types (cfg : 'a t) : Type.t list =
    cfg.arg_types

  let return_types (cfg : 'a t) : Type.t list =
    cfg.return_types

  let local_type (cfg : 'a t) (l : int32) : Type.t =
    let nargs = List32.length cfg.arg_types in
    if Int32.(l < nargs) then
      List32.nth_exn cfg.arg_types l
    else
      List32.nth_exn cfg.local_types Int32.(l-nargs)

  let is_loop_head (cfg : 'a t) (block_idx : int) : bool =
    IntSet.mem cfg.loop_heads block_idx

  let exit_block (cfg : 'a t) : int =
    cfg.exit_block

  let entry_block (cfg : 'a t) : int =
    cfg.entry_block

  let to_string (cfg : 'a t) : string = Printf.sprintf "CFG of function %s" (Int32.to_string cfg.idx)

  let to_dot
      ?annot_str:(annot_str : ('a -> string) = fun _ -> "")
      ?extra_data:(extra_data : string = "")
      ?include_edges:(include_edges : bool = true)
      (cfg : 'a t) : string =
    Printf.sprintf "digraph \"CFG of function %s\" {\n%s\n%s\n%s}\n"
      (Int32.to_string cfg.idx)
      (String.concat ~sep:"\n" (List.map (IntMap.to_alist cfg.basic_blocks) ~f:(fun (_, b) -> Basic_block.to_dot ~annot_str b)))
      begin if include_edges then
          (String.concat ~sep:"\n" (List.concat_map (IntMap.to_alist cfg.edges) ~f:(fun (src, dsts) ->
               List.map (Edge.Set.to_list dsts) ~f:(fun (dst, br) -> Printf.sprintf "block%d -> block%d [label=\"%s\"];\n" src dst (match br with
                   | Some true -> "t"
                   | Some false -> "f"
                   | None -> "")))))
        else
          ""
      end
      extra_data

  (** Converts the graph to a textual representation of nodes and its adjacency matrix. *)
  let to_adjlist (cfg : 'a t) : string * string =
    let nodes = cfg.basic_blocks
                |> IntMap.to_alist
                |> List.map ~f:(fun (idx, block) ->
                    let (t, content) = match block.content with
                      | Call instr -> ("c", Instr.call_to_string instr.instr)
                      | Entry -> ("e", "")
                      | Return instr -> ("r", Instr.call_to_string instr.instr)
                      | Imported desc -> ("i", Printf.sprintf "import %ld" desc.idx)
                      | Control instr -> ("c", Instr.control_to_short_string instr.instr)
                      | Data instrs -> ("d", List.map instrs ~f:(fun i -> Instr.data_to_string i.instr) |> String.concat ~sep:":" )
                    in Printf.sprintf "%d:%s:%s" idx t content)
                |> String.concat ~sep:"\n" in
    let adj = cfg.edges
              |> IntMap.to_alist
              |> List.map ~f:(fun (source, edges) ->
                  edges
                  |> Edge.Set.to_list
                  |> List.map ~f:(fun (dest, branch) ->
                      Printf.sprintf "%d %d %s"
                        source dest
                        (match branch with
                         | Some true -> "t"
                         | Some false -> "f"
                         | None -> "x"))
                  |> String.concat ~sep:"\n")
              |> String.concat ~sep:"\n" in
    (nodes, adj)

  let find_block (cfg : 'a t) (idx : int) : 'a Basic_block.t option =
    IntMap.find cfg.basic_blocks idx

  let find_block_exn (cfg : 'a t) (idx : int) : 'a Basic_block.t =
    match find_block cfg idx with
    | Some b -> b
    | None -> failwith (Printf.sprintf "Cfg.find_block_exn did not find block %d" idx)

  let find_enclosing_block (cfg : 'a t) (label : Instr.Label.t) : Instr.Label.t option =
    Instr.Label.Map.find cfg.label_to_enclosing_block label

  let find_enclosing_block_exn (cfg : 'a t) (label : Instr.Label.t) : 'a Basic_block.t  =
    match List.find (IntMap.to_alist cfg.basic_blocks) ~f:(fun (_, block) ->
        let labels = Basic_block.all_direct_instruction_labels block in
        Instr.Label.Set.mem labels label) with
    | Some (_, b) -> b
    | None -> failwith "find_enclosing_block did not find a block"

  let rec find_nth_parent_block (cfg : 'a t) (instruction : Instr.Label.t) (n : int32) : Instr.Label.t option =
    match find_enclosing_block cfg instruction with
    | None ->
      if Int32.(n = 0l) then
        None (* break out of the function *)
      else
        failwith "Cfg.find_nth_parent_block_exn: break level is too high"
    | Some parent ->
      if Int32.(n = 0l) then
        Some parent
      else
        find_nth_parent_block cfg parent Int32.(n-1l)

  let block_arity (cfg : 'a t) (block_label : Instr.Label.t) : int * int =
    match Instr.Label.Map.find cfg.block_arities block_label with
    | Some arity -> arity
    | None -> failwith "Cfg.block_arity: cannot find block"

  let outgoing_edges (cfg : 'a t) (idx : int) : Edge.t list =
    Edges.from cfg.edges idx

  let successors (cfg : 'a t) (idx : int) : int list =
    List.map (outgoing_edges cfg idx) ~f:fst

  let predecessors (cfg : 'a t) (idx : int) : Edge.t list =
    Edges.from cfg.back_edges idx

  let rec non_empty_predecessors (cfg :'a t) (idx : int) : int list =
    let preds = predecessors cfg idx in
    let non_empty = List.filter preds ~f:(fun (pred, _) ->
        let block = find_block_exn cfg pred in
        match block.content with
        | Data [] -> false
        | _ -> true) in
    if List.is_empty non_empty then
      List.concat_map preds ~f:(fun (pred, _) -> non_empty_predecessors cfg pred)
    else
      List.map ~f:fst non_empty

  let callees (cfg : 'a t) : Int32Set.t =
    (* Loop through all the blocks of the cfg, collecting the targets of call instructions *)
    IntMap.fold cfg.basic_blocks
      ~init:Int32Set.empty
      ~f:(fun ~key:_ ~data:block callees -> match block.content with
          | Call { instr = CallDirect (_, _, n); _} -> Int32Set.union (Int32Set.singleton n) callees
          | _ -> callees)

  let callers (cfgs : 'a t Int32Map.t) (cfg : 'a t) : Int32Set.t =
    Int32Map.fold cfgs
      ~init:Int32Set.empty ~f:(fun ~key:caller ~data:cfg' callers ->
          if Int32Set.mem (callees cfg') cfg.idx then
            (* cfg' calls into cfg *)
            Int32Set.add callers caller
          else
            callers)

  let all_instructions (cfg : 'a t) : 'a Instr.t Instr.Label.Map.t =
    IntMap.fold cfg.basic_blocks ~init:Instr.Label.Map.empty ~f:(fun ~key:_ ~data:block acc ->
        match block.content with
        | Control i -> Instr.Label.Map.add_exn acc ~key:i.label ~data:(Instr.Control i)
        | Call i -> Instr.Label.Map.add_exn acc ~key:i.label ~data:(Instr.Call i)
        | Entry | Return _ | Imported _ -> acc
        | Data d -> List.fold_left d ~init:acc ~f:(fun acc i ->
            Instr.Label.Map.add_exn acc ~key:i.label ~data:(Instr.Data i)))

  let all_instructions_list (cfg : 'a t) : 'a Instr.t list =
    List.map ~f:snd (Instr.Label.Map.to_alist (all_instructions cfg))

  let find_instr (instructions : 'a Instr.t Instr.Label.Map.t) (label : Instr.Label.t) : 'a Instr.t option =
    Instr.Label.Map.find instructions label

  let find_instr_exn (instructions : 'a Instr.t Instr.Label.Map.t) (label : Instr.Label.t) : 'a Instr.t =
    match find_instr instructions label with
    | Some i -> i
    | None ->
      failwith (Printf.sprintf "Cfg.find_instr_exn did not find instruction with label %s" (Instr.Label.to_string label))

  let all_blocks (cfg : 'a t) : 'a Basic_block.t list =
    IntMap.data cfg.basic_blocks

  let all_direct_calls_blocks (cfg : 'a t) : 'a Basic_block.t list =
    List.filter (all_blocks cfg) ~f:Basic_block.is_direct_call

  let all_edges (cfg : 'a t) : (int * int) list =
    List.concat_map (IntMap.to_alist cfg.edges) ~f:(fun (src, edges) ->
        List.map (Edge.Set.to_list edges) ~f:(fun (dst, _) -> (src, dst)))

  let all_merge_blocks (cfg : 'a t) : 'a Basic_block.t list =
    IntMap.fold cfg.basic_blocks ~init:[] ~f:(fun ~key:_ ~data:block l ->
        match block.content with
        | Control { instr = Merge; _ } -> block :: l
        | _ -> l)

  let all_block_indices (cfg : 'a t) : IntSet.t =
    IntSet.of_list (IntMap.keys cfg.basic_blocks)

  let all_annots (cfg : 'a t) : 'a list =
    IntMap.fold cfg.basic_blocks ~init:[] ~f:(fun ~key:_ ~data:block l -> (Basic_block.all_annots block) @ l)

  let map_annotations
      (cfg : 'a t)
      ~(instrs : 'a Instr.t -> 'b * 'b)
      ~(blocks : BlockIdx.t -> 'b * 'b)
    : 'b t =
    { cfg with
      basic_blocks = IntMap.mapi ~f:(fun ~key:bidx ~data:b ->
          let before, after = blocks bidx in
          Basic_block.map_annotations b ~f:instrs before after) cfg.basic_blocks;
    }

  let clear_annotations (cfg : 'a t) : unit t =
    map_annotations cfg ~instrs:(fun _ -> (), ()) ~blocks:(fun _ -> (), ())

  let body (cfg : 'a t) : unit Instr.t list =
    List.map cfg.instructions ~f:(fun l -> Instr.Label.Map.find_exn cfg.label_to_instr l)

  let replace_block (cfg : 'a t) (block : 'a Basic_block.t) : 'a t =
    { cfg with
      basic_blocks = IntMap.update cfg.basic_blocks block.idx ~f:(function
          | None -> failwith "Cfg.replace_block called with a block that did not exist in the previous CFG"
          | Some _ -> block) }

  let remove_block_rewrite_edges (cfg : 'a t) (block_idx : int) : 'a t =
    assert (block_idx <> cfg.entry_block);
    assert (block_idx <> cfg.exit_block);
    (* A block is removed, and edges for that block need to be rewritten:
       - for each incoming edge, merge it with each outgoing edge
       - in case there is a branching edge, keep the branching information
    *)
    let without_edges =
      let edges' = Edges.remove_from cfg.edges block_idx in (* Remove all edges starting from the current block *)
      let srcs = Edges.from cfg.back_edges block_idx in (* Find all edges that go to this node *)
      List.fold_left srcs ~init:edges' ~f:(fun edges (src, _) ->
          (* and remove them *)
          Edges.remove edges src block_idx) in
    let outgoing_edges = Edges.from cfg.edges block_idx in
    let incoming_edges = Edges.from cfg.back_edges block_idx in
    (* Connect each incoming edge to each outgoing edge *)
    let new_edges = List.fold_left incoming_edges ~init:without_edges ~f:(fun edges (src, cond) ->
        List.fold_left outgoing_edges ~init:edges ~f:(fun edges (dst, _) ->
            if src <> dst then begin
              Edges.add edges src (dst, cond (* Keep the first condition as the second block can't be executed anymore *))
            end else
              (* No self-cycle. *)
              edges)) in
    let without_back_edges =
      let back_edges' = Edges.remove_from cfg.back_edges block_idx in
      let dsts = Edges.from cfg.edges block_idx in
      List.fold_left dsts ~init:back_edges' ~f:(fun back_edges (dst, _) ->
          let after = Edges.remove back_edges dst block_idx in
          after
        ) in
    let outgoing_edges = Edges.from cfg.edges block_idx in
    let incoming_edges = Edges.from cfg.back_edges block_idx in
    let new_back_edges = List.fold_left incoming_edges ~init:without_back_edges ~f:(fun back_edges (src, cond) ->
        List.fold_left outgoing_edges ~init:back_edges ~f:(fun back_edges (dst, _) ->
            if src <> dst then
              Edges.add back_edges dst (src, cond)
            else
              back_edges)) in
    { cfg with
      edges = new_edges;
      back_edges = new_back_edges;
      basic_blocks = IntMap.remove cfg.basic_blocks block_idx }

  let insert_block_between (cfg : 'a t) (src : int) (dst : int) (new_block : 'a Basic_block.t) : 'a t =
    let (_, forward_annot) = Edges.find_exn cfg.edges src dst in
    let edges =
      Edges.add (Edges.add (Edges.remove cfg.edges src dst)
                   src (new_block.idx, forward_annot))
        new_block.idx (dst, None) in
    let (_, backward_annot) = Edges.find_exn cfg.back_edges dst src in
    let back_edges =
      Edges.add (Edges.add (Edges.remove cfg.back_edges dst src)
                   dst (new_block.idx, None))
        new_block.idx (src, backward_annot) in
    let basic_blocks = match find_block cfg new_block.idx with
      | Some _ -> (* Block already present, do not add it *)
        cfg.basic_blocks
      | None ->
        IntMap.add_exn cfg.basic_blocks ~key:new_block.idx ~data:new_block
    in
    { cfg with edges; back_edges; basic_blocks }

  let has_edge (cfg : 'a t) (src : int) (dst : int) : bool =
    List.exists (Edges.from cfg.edges src) ~f:(fun (x, _) -> x = dst)

  (* We need to sometimes keep nodes that are empty and have no predecessors, in
     order to remember block/loop exits. This is to remove them when needed *)
  let without_empty_nodes_with_no_predecessors (cfg : 'a t) : 'a t =
    let to_remove = IntMap.keys (IntMap.filter cfg.basic_blocks ~f:(fun block ->
        block.idx <> cfg.entry_block &&
        List.is_empty (predecessors cfg block.idx) &&
        Basic_block.is_empty block)) in
    List.fold_left to_remove ~init:cfg ~f:(fun cfg idx -> remove_block_rewrite_edges cfg idx)

  let all_predecessors (cfg : 'a t) (block : 'a Basic_block.t) : 'a Basic_block.t list =
    let rec loop (worklist : IntSet.t) (visited : IntSet.t) (acc : 'a Basic_block.t list) : 'a Basic_block.t list =
      match IntSet.choose worklist with
      | None -> acc
      | Some block_idx when IntSet.mem visited block_idx -> loop (IntSet.remove worklist block_idx) visited acc
      | Some block_idx ->
        let preds = List.map ~f:fst (Edges.from cfg.back_edges block_idx) in
        let worklist' = IntSet.union (IntSet.of_list preds) (IntSet.remove worklist block_idx) in
        loop worklist' (IntSet.add visited block_idx) (find_block_exn cfg block_idx :: acc)
    in
    loop (IntSet.singleton block.idx) IntSet.empty []

  let is_loop_exn (cfg : 'a t) (label : Instr.Label.t) : bool =
    match Instr.Label.Map.find cfg.label_to_instr label with
    | None -> failwith "Cfg.is_loop_exn: did not find the instruction"
    | Some (Control i) -> begin match i.instr with
        | Loop _ -> true
        | Block _ | If _ -> false
        | _ -> failwith "Cfg.is_loop_exn: this is not a block"
      end
    | Some _  -> failwith "Cfg.is_loop_exn: this is not a control instruction"

end

include Cfg
