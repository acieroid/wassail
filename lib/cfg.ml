open Core_kernel
open Helpers

module IntListIntMap = struct
  module IntMap = Map.Make(I)

  type t = (int list) IntMap.t
  [@@deriving sexp, compare]
  let to_yojson m = IntMap.to_alist m
                    |> [%to_yojson: (int * int list) list]
  let of_yojson json = match [%of_yojson: (int * int list) list] json with
    | Ok a -> begin match IntMap.of_alist a with
        | `Duplicate_key n -> Error (Printf.sprintf "IntListIntMap.of_yojson: duplicate key %d" n)
        | `Ok v -> Ok v
      end
    | Error err -> Error err
end

module BasicBlocks = struct
  module IntMap = Map.Make(I)

  type t = Basic_block.t IntMap.t
  [@@deriving sexp, compare]
  let to_yojson (m : t) = IntMap.to_alist m
                    |> [%to_yojson: (int * Basic_block.t) list]
  let of_yojson json = match [%of_yojson: (int * Basic_block.t) list] json with
    | Ok a -> begin match IntMap.of_alist a with
        | `Duplicate_key n -> Error (Printf.sprintf "BasicBlockIntMap.of_yojson: duplicate key %d" n)
        | `Ok v -> Ok v
      end
    | Error err -> Error err
end



type t = {
  (* Is this function exported or not? *)
  exported: bool;
  (* The name of the function *)
  name: string;
  (* The index of this CFG *)
  idx: int;
  (* The number of parameters and return values of that CFG *)
  arity: (int * int);
  (* The number of locals in that CFG *)
  nlocals: int;
  (* All basic blocks contained in this CFG, indexed in a map by their index *)
  basic_blocks: BasicBlocks.t;
  (* The edges between basic blocks (forward direction) *)
  edges: IntListIntMap.t;
  (* The edges between basic blocks (backward direction) *)
  back_edges: IntListIntMap.t;
  (* The entry block *)
  entry_block: int;
  (* The exit block *)
  exit_block: int;
}

let dependencies (cfg : t) : int list =
  List.filter_map (BasicBlocks.IntMap.to_alist cfg.basic_blocks) ~f:(fun (_idx, block) -> match block.sort with
      | Function -> begin match List.nth block.instrs 0 with
          | Some (Instr.Call n) -> Some n
          | _ -> None
        end
      | _ -> None)
[@@deriving sexp, compare, yojson]
let to_string (cfg : t) : string = Printf.sprintf "CFG of function %d" cfg.idx
let to_dot (cfg : t) : string =
  Printf.sprintf "digraph \"CFG of function %d\" {\n%s\n%s}\n"
    cfg.idx
    (String.concat ~sep:"\n" (List.map (BasicBlocks.IntMap.to_alist cfg.basic_blocks) ~f:(fun (_, b) -> Basic_block.to_dot b)))
    (String.concat ~sep:"\n" (List.concat_map (IntListIntMap.IntMap.to_alist cfg.edges) ~f:(fun (left, right) ->
         List.map right ~f:(Printf.sprintf "block%d -> block%d;\n" left))))

let find_block_exn (cfg : t) (idx : int) : Basic_block.t =
  BasicBlocks.IntMap.find_exn cfg.basic_blocks idx

let successors (cfg : t) (idx : int) : int list =
  BasicBlocks.IntMap.find_multi cfg.edges idx

let predecessors (cfg : t) (idx : int) : int list =
  BasicBlocks.IntMap.find_multi cfg.back_edges idx

let callees (cfg : t) : IntSet.t =
  (* Loop through all the blocks of the cfg, collecting the targets of call instructions *)
  BasicBlocks.IntMap.fold cfg.basic_blocks
    ~init:IntSet.empty
    ~f:(fun ~key:_ ~data:block callees -> match block.sort with
        | Function -> begin match block.instrs with
            | Call n :: [] -> IntSet.union (IntSet.singleton n) callees
            | _ -> callees
          end
        | _ -> callees)

let callers (cfgs : t IntMap.t) (cfg : t) : IntSet.t =
  IntMap.fold cfgs
    ~init:IntSet.empty ~f:(fun ~key:caller ~data:cfg' callers ->
      if IntSet.mem (callees cfg') cfg.idx then
        (* cfg' calls into cfg *)
        IntSet.union (IntSet.singleton caller) callers
      else
        callers)
