open Core_kernel
open Helpers

type edges = (int * bool option) list
[@@deriving sexp, compare, equal]

type 'a t = {
  exported: bool;
  name: string;
  idx: int;
  global_types: Type.t list;
  arg_types: Type.t list;
  local_types: Type.t list;
  return_types: Type.t list;
  basic_blocks: 'a Basic_block.t IntMap.t;
  instructions : 'a Instr.t IntMap.t;
  edges: edges IntMap.t;
  back_edges: edges IntMap.t;
  entry_block: int;
  exit_block: int;
  loop_heads: IntSet.t;
}
[@@deriving compare, equal]

(* TODO: subsumed by callees and callers *)
(* let dependencies (cfg : 'a t) : int list =
  List.filter_map (IntMap.to_alist cfg.basic_blocks) ~f:(fun (_idx, block) -> match block.content with
      | Control { instr = Call (_, n); _ } -> Some n
      | _ -> None) *)

let to_string (cfg : 'a t) : string = Printf.sprintf "CFG of function %d" cfg.idx

let to_dot (cfg : 'a t) (annot_to_string : 'a -> string) : string =
  Printf.sprintf "digraph \"CFG of function %d\" {\n%s\n%s}\n"
    cfg.idx
    (String.concat ~sep:"\n" (List.map (IntMap.to_alist cfg.basic_blocks) ~f:(fun (_, b) -> Basic_block.to_dot b annot_to_string)))
    (String.concat ~sep:"\n" (List.concat_map (IntMap.to_alist cfg.edges) ~f:(fun (src, dsts) ->
         List.map dsts ~f:(fun (dst, br) -> Printf.sprintf "block%d -> block%d [label=\"%s\"];\n" src dst (match br with
             | Some true -> "t"
             | Some false -> "f"
             | None -> "")))))

let find_block_exn (cfg : 'a t) (idx : int) : 'a Basic_block.t =
  match IntMap.find cfg.basic_blocks idx with
  | Some b -> b
  | None -> failwith "Cfg.find_block_exn did not find block"

let find_instr_exn (cfg : 'a t) (label : Instr.label) : 'a Instr.t =
  match IntMap.find cfg.instructions label with
  | Some i -> i
  | None -> failwith "Cfg.find_instr_exn did not find instruction"

let outgoing_edges (cfg : 'a t) (idx : int) : edges =
  IntMap.find_multi cfg.edges idx

let successors (cfg : 'a t) (idx : int) : int list =
  List.map (outgoing_edges cfg idx) ~f:fst

let incoming_edges (cfg : 'a t) (idx : int) : edges =
  IntMap.find_multi cfg.back_edges idx

let predecessors (cfg : 'a t) (idx : int) : int list =
  List.map (incoming_edges cfg idx) ~f:fst

let callees (cfg : 'a t) : IntSet.t =
  (* Loop through all the blocks of the cfg, collecting the targets of call instructions *)
  IntMap.fold cfg.basic_blocks
    ~init:IntSet.empty
    ~f:(fun ~key:_ ~data:block callees -> match block.content with
        | Control { instr = Call (_, n); _} -> IntSet.union (IntSet.singleton n) callees
        | _ -> callees)

let callers (cfgs : 'a t IntMap.t) (cfg : 'a t) : IntSet.t =
  IntMap.fold cfgs
    ~init:IntSet.empty ~f:(fun ~key:caller ~data:cfg' callers ->
      if IntSet.mem (callees cfg') cfg.idx then
        (* cfg' calls into cfg *)
        IntSet.union (IntSet.singleton caller) callers
      else
        callers)

let all_instructions (cfg : 'a t) : 'a Instr.t list =
  List.map ~f:snd (IntMap.to_alist cfg.instructions)

let all_merge_blocks (cfg : 'a t) : 'a Basic_block.t list =
  IntMap.fold cfg.basic_blocks ~init:[] ~f:(fun ~key:_ ~data:block l ->
      match block.content with
      | ControlMerge -> block :: l
      | _ -> l)

let all_block_indices (cfg : 'a t) : IntSet.t =
  IntSet.of_list (IntMap.keys cfg.basic_blocks)

let all_instruction_labels (cfg : 'a t) : IntSet.t =
  IntMap.fold cfg.basic_blocks ~init:IntSet.empty ~f:(fun ~key:_ ~data:block l ->
      IntSet.union (Basic_block.all_instruction_labels block) l)

let all_annots (cfg : 'a t) : 'a list =
  IntMap.fold cfg.basic_blocks ~init:[] ~f:(fun ~key:_ ~data:block l -> (Basic_block.all_annots block) @ l)

let annotate (cfg : 'a t) (block_data : ('b * 'b) IntMap.t) (instr_data : ('b * 'b) IntMap.t) : 'b t =
  { cfg with
    basic_blocks = IntMap.map ~f:(fun b -> Basic_block.annotate b block_data instr_data) cfg.basic_blocks;
    instructions = IntMap.map ~f:(fun i -> Instr.annotate i instr_data) cfg.instructions;
  }

let add_annotation (cfg : 'a t) (block_data : ('b * 'b) IntMap.t) (instr_data : ('b * 'b) IntMap.t) : ('a * 'b) t =
  { cfg with
    basic_blocks = IntMap.map ~f:(fun b -> Basic_block.add_annotation b block_data instr_data) cfg.basic_blocks;
    instructions = IntMap.map ~f:(fun i -> Instr.add_annotation i instr_data) cfg.instructions;
  }

let forward_edges_from_node (cfg : 'a t) (node_idx : int) : (int * bool option) list =
  match IntMap.find cfg.edges node_idx with
  | Some e -> e
  | None -> []

let backward_edges_from_node (cfg : 'a t) (node_idx : int) : (int * bool option) list =
  match IntMap.find cfg.back_edges node_idx with
  | Some e -> e
  | None -> []

