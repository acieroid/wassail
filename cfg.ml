open Core_kernel
open Helpers

type t = {
  (* Is this function exported or not? *)
  exported: bool;
  (* The index of this CFG *)
  idx: int;
  (* The number of parameters and return values of that CFG *)
  arity: (int * int);
  (* The number of locals in that CFG *)
  nlocals: int;
  (* All basic blocks contained in this CFG, indexed in a map by their index *)
  basic_blocks: Basic_block.t IntMap.t;
  (* The edges between basic blocks (forward direction) *)
  edges: (int list) IntMap.t;
  (* The edges between basic blocks (backward direction) *)
  back_edges: (int list) IntMap.t;
  (* The entry block *)
  entry_block: int;
  (* The exit block *)
  exit_block: int;
}
let to_string (cfg : t) : string = Printf.sprintf "CFG of function %d" cfg.idx
let to_dot (cfg : t) : string =
  Printf.sprintf "digraph \"CFG of function %d\" {\n%s\n%s}\n"
    cfg.idx
    (String.concat ~sep:"\n" (List.map (IntMap.to_alist cfg.basic_blocks) ~f:(fun (_, b) -> Basic_block.to_dot b)))
    (String.concat ~sep:"\n" (List.concat_map (IntMap.to_alist cfg.edges) ~f:(fun (left, right) ->
         List.map right ~f:(Printf.sprintf "block%d -> block%d;\n" left))))

let find_block_exn (cfg : t) (idx : int) : Basic_block.t =
  IntMap.find_exn cfg.basic_blocks idx

let successors (cfg : t) (idx : int) : int list =
  IntMap.find_multi cfg.edges idx

let predecessors (cfg : t) (idx : int) : int list =
  IntMap.find_multi cfg.back_edges idx

let callees (cfg : t) : IntSet.t =
  (* Loop through all the blocks of the cfg, collecting the targets of call instructions *)
  IntMap.fold cfg.basic_blocks ~init:IntSet.empty ~f:(fun ~key:_ ~data:block callees -> match block.sort with
      | Function -> begin match block.instrs with
          | Call n :: [] -> IntSet.union (IntSet.singleton n) callees
          | _ -> callees
        end
      | _ -> callees)

let callers (cfgs : t IntMap.t) (cfg : t) : IntSet.t =
  IntMap.fold cfgs ~init:IntSet.empty ~f:(fun ~key:caller ~data:cfg' callers ->
      if IntSet.mem (callees cfg') cfg.idx then
        (* cfg' calls into cfg *)
        IntSet.union (IntSet.singleton caller) callers
      else
        callers)
