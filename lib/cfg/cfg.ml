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
  instructions : 'a Instr.t Instr.Label.Map.t;
  edges: Edges.t;
  back_edges: Edges.t;
  entry_block: int;
  exit_block: int;
  loop_heads: IntSet.t;
}
[@@deriving compare, equal]

let to_string (cfg : 'a t) : string = Printf.sprintf "CFG of function %s" (Int32.to_string cfg.idx)

let to_dot ?annot_str:(annot_str : ('a -> string) = fun _ -> "") (cfg : 'a t) : string =
  Printf.sprintf "digraph \"CFG of function %s\" {\n%s\n%s}\n"
    (Int32.to_string cfg.idx)
    (String.concat ~sep:"\n" (List.map (IntMap.to_alist cfg.basic_blocks) ~f:(fun (_, b) -> Basic_block.to_dot ~annot_str b)))
    (String.concat ~sep:"\n" (List.concat_map (IntMap.to_alist cfg.edges) ~f:(fun (src, dsts) ->
         List.map (Edge.Set.to_list dsts) ~f:(fun (dst, br) -> Printf.sprintf "block%d -> block%d [label=\"%s\"];\n" src dst (match br with
             | Some true -> "t"
             | Some false -> "f"
             | None -> "")))))

let find_block_exn (cfg : 'a t) (idx : int) : 'a Basic_block.t =
  match IntMap.find cfg.basic_blocks idx with
  | Some b -> b
  | None -> failwith "Cfg.find_block_exn did not find block"

let find_instr_exn (cfg : 'a t) (label : Instr.Label.t) : 'a Instr.t =
  match Instr.Label.Map.find cfg.instructions label with
  | Some i -> i
  | None -> failwith "Cfg.find_instr_exn did not find instruction"

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

let find_enclosing_block (cfg : 'a t) (instr : 'a Instr.t) : 'a Basic_block.t =
  let label = Instr.label instr in
  match List.find (IntMap.to_alist cfg.basic_blocks) ~f:(fun (_, block) ->
      let labels = Basic_block.all_instruction_labels block in
      Instr.Label.Set.mem labels label) with
  | Some (_, b) -> b
  | None -> failwith "find_enclosing_block did not find a block"

let all_instructions (cfg : 'a t) : 'a Instr.t list =
  List.map ~f:snd (Instr.Label.Map.to_alist cfg.instructions)

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
    basic_blocks = IntMap.map ~f:(fun b -> Basic_block.map_annotations b ~f) cfg.basic_blocks;
    instructions = Instr.Label.Map.map ~f:(fun i -> Instr.map_annotation i ~f) cfg.instructions; }

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
