open Core_kernel
open Helpers

module Edge = struct
  module T = struct
    type t = int * bool option
    [@@deriving sexp, compare, equal]
  end
  include T
  let to_string (e : t) : string = string_of_int (fst e)
  module Set = Set.Make(T)
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

type 'a t = {
  exported: bool;
  name: string;
  idx: Int32.t;
  global_types: Type.t list;
  arg_types: Type.t list;
  local_types: Type.t list;
  return_types: Type.t list;
  basic_blocks: 'a Basic_block.t IntMap.t;
  edges: Edges.t;
  back_edges: Edges.t;
  entry_block: int;
  exit_block: int;
  loop_heads: IntSet.t;
}
[@@deriving compare, equal]

let to_string (cfg : 'a t) : string = Printf.sprintf "CFG of function %s" (Int32.to_string cfg.idx)

let to_dot
    ?annot_str:(annot_str : ('a -> string) = fun _ -> "")
    ?extra_data:(extra_data : string = "")
    (cfg : 'a t) : string =
  Printf.sprintf "digraph \"CFG of function %s\" {\n%s\n%s\n%s}\n"
    (Int32.to_string cfg.idx)
    (String.concat ~sep:"\n" (List.map (IntMap.to_alist cfg.basic_blocks) ~f:(fun (_, b) -> Basic_block.to_dot ~annot_str b)))
    (String.concat ~sep:"\n" (List.concat_map (IntMap.to_alist cfg.edges) ~f:(fun (src, dsts) ->
         List.map (Edge.Set.to_list dsts) ~f:(fun (dst, br) -> Printf.sprintf "block%d -> block%d [label=\"%s\"];\n" src dst (match br with
             | Some true -> "t"
             | Some false -> "f"
             | None -> "")))))
    extra_data

let find_block (cfg : 'a t) (idx : int) : 'a Basic_block.t option =
  IntMap.find cfg.basic_blocks idx

let find_block_exn (cfg : 'a t) (idx : int) : 'a Basic_block.t =
  match find_block cfg idx with
  | Some b -> b
  | None -> failwith (Printf.sprintf "Cfg.find_block_exn did not find block %d" idx)

let outgoing_edges (cfg : 'a t) (idx : int) : Edge.t list =
  Edges.from cfg.edges idx

let successors (cfg : 'a t) (idx : int) : int list =
  List.map (outgoing_edges cfg idx) ~f:fst

let rec non_empty_successors (cfg :'a t) (idx : int) : int list =
  let succs = successors cfg idx in
  let non_empty = List.filter succs ~f:(fun succ ->
      let block = find_block_exn cfg succ in
      match block.content with
      | Data [] -> false
      | _ -> true) in
  if List.is_empty non_empty then
    List.concat_map succs ~f:(non_empty_successors cfg)
  else
    non_empty

let incoming_edges (cfg : 'a t) (idx : int) : Edge.t list =
  Edges.from cfg.back_edges idx

let predecessors (cfg : 'a t) (idx : int) : int list =
  List.map (incoming_edges cfg idx) ~f:fst

let rec non_empty_predecessors (cfg :'a t) (idx : int) : int list =
  let preds = predecessors cfg idx in
  let non_empty = List.filter preds ~f:(fun pred ->
      let block = find_block_exn cfg pred in
      match block.content with
      | Data [] -> false
      | _ -> true) in
  if List.is_empty non_empty then
    List.concat_map preds ~f:(non_empty_predecessors cfg)
  else
    non_empty

let callees (cfg : 'a t) : Int32Set.t =
  (* Loop through all the blocks of the cfg, collecting the targets of call instructions *)
  IntMap.fold cfg.basic_blocks
    ~init:Int32Set.empty
    ~f:(fun ~key:_ ~data:block callees -> match block.content with
        | Control { instr = Call (_, n); _} -> Int32Set.union (Int32Set.singleton n) callees
        | _ -> callees)

let callers (cfgs : 'a t Int32Map.t) (cfg : 'a t) : Int32Set.t =
  Int32Map.fold cfgs
    ~init:Int32Set.empty ~f:(fun ~key:caller ~data:cfg' callers ->
      if Int32Set.mem (callees cfg') cfg.idx then
        (* cfg' calls into cfg *)
        Int32Set.add callers caller
      else
        callers)

let find_enclosing_block_exn (cfg : 'a t) (label : Instr.Label.t) : 'a Basic_block.t =
  match List.find (IntMap.to_alist cfg.basic_blocks) ~f:(fun (_, block) ->
      let labels = Basic_block.all_direct_instruction_labels block in
      Instr.Label.Set.mem labels label) with
  | Some (_, b) -> b
  | None -> failwith "find_enclosing_block did not find a block"

let all_instructions (cfg : 'a t) : 'a Instr.t Instr.Label.Map.t =
  IntMap.fold cfg.basic_blocks ~init:Instr.Label.Map.empty ~f:(fun ~key:_ ~data:block acc ->
      match block.content with
      | Control i -> Instr.Label.Map.add_exn acc ~key:i.label ~data:(Instr.Control i)
      | Data d -> List.fold_left d ~init:acc ~f:(fun acc i ->
          Instr.Label.Map.add_exn acc ~key:i.label ~data:(Instr.Data i)))

let all_instructions_list (cfg : 'a t) : 'a Instr.t list =
  List.map ~f:snd (Instr.Label.Map.to_alist (all_instructions cfg))

let find_instr_exn (instructions : 'a Instr.t Instr.Label.Map.t) (label : Instr.Label.t) : 'a Instr.t =
  match Instr.Label.Map.find instructions label with
  | Some i -> i
  | None -> failwith "Cfg.find_instr_exn did not find instruction"

let all_blocks (cfg : 'a t) : 'a Basic_block.t list =
  IntMap.data cfg.basic_blocks

let all_merge_blocks (cfg : 'a t) : 'a Basic_block.t list =
  IntMap.fold cfg.basic_blocks ~init:[] ~f:(fun ~key:_ ~data:block l ->
      match block.content with
      | Control { instr = Merge; _ } -> block :: l
      | _ -> l)

let all_block_indices (cfg : 'a t) : IntSet.t =
  IntSet.of_list (IntMap.keys cfg.basic_blocks)

let all_instruction_labels (cfg : 'a t) : Instr.Label.Set.t =
  IntMap.fold cfg.basic_blocks ~init:Instr.Label.Set.empty ~f:(fun ~key:_ ~data:block l ->
      Instr.Label.Set.union (Basic_block.all_instruction_labels block) l)

let all_annots (cfg : 'a t) : 'a list =
  IntMap.fold cfg.basic_blocks ~init:[] ~f:(fun ~key:_ ~data:block l -> (Basic_block.all_annots block) @ l)

let map_annotations (cfg : 'a t) ~(f : 'a Instr.t -> 'b * 'b) : 'b t =
  { cfg with
    basic_blocks = IntMap.map ~f:(fun b -> Basic_block.map_annotations b ~f) cfg.basic_blocks }

let rec state_before_block (cfg : 'a t) (block_idx : int) : 'a =
  let block = find_block_exn cfg block_idx in
  match block.content with
  | Control i -> Instr.annotation_before (Control i)
  | Data [] -> begin match non_empty_predecessors cfg block_idx with
      | [] -> failwith "state_before_block: no predecessor of an empty block"
      | pred :: [] ->
        (* The state before this block is the state after its non-empty predecessor *)
        state_after_block cfg pred
      | _ -> failwith "state_before_block: multiple predecessors for an empty block"
    end
  | Data (i :: _) -> Instr.annotation_before (Data i)
and state_after_block (cfg : 'a t) (block_idx : int) : 'a =
  (* This implementation is the complement of state_before_block *)
  let block = find_block_exn cfg block_idx in
  match block.content with
  | Control i -> Instr.annotation_after (Control i)
  | Data [] -> begin match non_empty_successors cfg block_idx with
      | [] -> failwith "state_after_block: no successor of an empty block"
      | succ :: [] ->
        state_before_block cfg succ
      | _ -> failwith "state_after_bloc: multiple successors for an empty block"
    end
  | Data l -> Instr.annotation_after (Data (List.last_exn l))

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

let to_func_inst (cfg :'a t) : Func_inst.t =
  let generate_code _ _ = failwith "TODO" in
  let body: unit Instr.t list = generate_code [cfg.entry_block] IntSet.empty in
    { idx = cfg.idx;
      name = Some cfg.name;
      type_idx = failwith "TODO";
      typ = (cfg.arg_types, cfg.return_types);
      code = { locals = cfg.local_types; body }
    }
