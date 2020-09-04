open Core_kernel
open Helpers

type edge = (int * bool option) list
[@@deriving sexp, compare, equal]

type 'a t = {
  (* Is this function exported or not? *)
  exported: bool;
  (* The name of the function *)
  name: string;
  (* The index of this CFG *)
  idx: int;
  (* Types of globals (they are not specific to this CFG, but useful to have here) *)
  global_types: Type.t list;
  (* Types of arguments *)
  arg_types: Type.t list;
  (* Types of locals *)
  local_types: Type.t list;
  (* Types of return values *)
  return_types: Type.t list;
  (* All basic blocks contained in this CFG, indexed in a map by their index *)
  basic_blocks: 'a Basic_block.t IntMap.t;
  (* The edges between basic blocks (forward direction) *)
  edges: edge IntMap.t;
  (* The edges between basic blocks (backward direction) *)
  back_edges: edge IntMap.t;
  (* The edges data *)
  (*  edges_data: EdgeDataIntMap.t; *)
  (* The entry block *)
  entry_block: int;
  (* The exit block *)
  exit_block: int;
  (* The loop heads *)
  loop_heads: IntSet.t;
}
[@@deriving compare, equal]

let dependencies (cfg : 'a t) : int list =
  List.filter_map (IntMap.to_alist cfg.basic_blocks) ~f:(fun (_idx, block) -> match block.content with
      | Control { instr = Call (_, n); _ } -> Some n
      | _ -> None)

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
  IntMap.find_exn cfg.basic_blocks idx

let successors (cfg : 'a t) (idx : int) : int list =
  List.map (IntMap.find_multi cfg.edges idx) ~f:fst

let predecessors (cfg : 'a t) (idx : int) : (int * bool option) list =
  IntMap.find_multi cfg.back_edges idx

(** Finds the functions called in this CFG *)
let callees (cfg : 'a t) : IntSet.t =
  (* Loop through all the blocks of the cfg, collecting the targets of call instructions *)
  IntMap.fold cfg.basic_blocks
    ~init:IntSet.empty
    ~f:(fun ~key:_ ~data:block callees -> match block.content with
        | Control { instr = Call (_, n); _} -> IntSet.union (IntSet.singleton n) callees
        | _ -> callees)

(** Finds the callers of this function *)
let callers (cfgs : 'a t IntMap.t) (cfg : 'a t) : IntSet.t =
  IntMap.fold cfgs
    ~init:IntSet.empty ~f:(fun ~key:caller ~data:cfg' callers ->
      if IntSet.mem (callees cfg') cfg.idx then
        (* cfg' calls into cfg *)
        IntSet.union (IntSet.singleton caller) callers
      else
        callers)

(** Change the annotations of a CFG *)
let annotate (cfg : 'a t) (data : ('b * 'b) IntMap.t) : 'b t =
  { cfg with basic_blocks = IntMap.map ~f:(fun b -> Basic_block.annotate b data) cfg.basic_blocks }
