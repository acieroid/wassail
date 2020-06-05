open Core_kernel
open Helpers

module IntListIntMap = struct
  type t = (int list) IntMap.t
  [@@deriving sexp, compare]
end

module EdgesIntMap = struct
  type t = ((int * bool option) list) IntMap.t
  [@@deriving sexp, compare]
end

module BasicBlocks = struct
  type t = Basic_block.t IntMap.t
  [@@deriving sexp, compare]
end

type t = {
  (* Is this function exported or not? *)
  exported: bool;
  (* The name of the function *)
  name: string;
  (* The index of this CFG *)
  idx: int;
  (* Types of arguments *)
  arg_types: Type.t list;
  (* Types of locals *)
  local_types: Type.t list;
  (* Typpes of return values *)
  return_types: Type.t list;
  (* All basic blocks contained in this CFG, indexed in a map by their index *)
  basic_blocks: BasicBlocks.t;
  (* The edges between basic blocks (forward direction) *)
  edges: EdgesIntMap.t;
  (* The edges between basic blocks (backward direction) *)
  back_edges: EdgesIntMap.t;
  (* The edges data *)
  (*  edges_data: EdgeDataIntMap.t; *)
  (* The entry block *)
  entry_block: int;
  (* The exit block *)
  exit_block: int;
}

let dependencies (cfg : t) : int list =
  List.filter_map (IntMap.to_alist cfg.basic_blocks) ~f:(fun (_idx, block) -> match block.content with
      | Control (Call n, _) -> Some n
      | _ -> None)
[@@deriving sexp, compare, yojson]
let to_string (cfg : t) : string = Printf.sprintf "CFG of function %d" cfg.idx

let to_dot (cfg : t) : string =
  Printf.sprintf "digraph \"CFG of function %d\" {\n%s\n%s}\n"
    cfg.idx
    (String.concat ~sep:"\n" (List.map (IntMap.to_alist cfg.basic_blocks) ~f:(fun (_, b) -> Basic_block.to_dot b)))
    (String.concat ~sep:"\n" (List.concat_map (IntMap.to_alist cfg.edges) ~f:(fun (src, dsts) ->
         List.map dsts ~f:(fun (dst, br) -> Printf.sprintf "block%d -> block%d [label=\"%s\"];\n" src dst (match br with
             | Some true -> "t"
             | Some false -> "f"
             | None -> "")))))

let find_block_exn (cfg : t) (idx : int) : Basic_block.t =
  IntMap.find_exn cfg.basic_blocks idx

let successors (cfg : t) (idx : int) : int list =
  List.map (IntMap.find_multi cfg.edges idx) ~f:fst

let predecessors (cfg : t) (idx : int) : (int * bool option) list =
  IntMap.find_multi cfg.back_edges idx

let callees (cfg : t) : IntSet.t =
  (* Loop through all the blocks of the cfg, collecting the targets of call instructions *)
  IntMap.fold cfg.basic_blocks
    ~init:IntSet.empty
    ~f:(fun ~key:_ ~data:block callees -> match block.content with
        | Control (Call n, _) -> IntSet.union (IntSet.singleton n) callees
        | _ -> callees)

let callers (cfgs : t IntMap.t) (cfg : t) : IntSet.t =
  IntMap.fold cfgs
    ~init:IntSet.empty ~f:(fun ~key:caller ~data:cfg' callers ->
      if IntSet.mem (callees cfg') cfg.idx then
        (* cfg' calls into cfg *)
        IntSet.union (IntSet.singleton caller) callers
      else
        callers)
