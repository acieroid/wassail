open Core_kernel

type block_sort = BlockEntry | BlockExit | LoopEntry | LoopExit | Normal | Function | Return
[@@deriving sexp, compare]

type block_content =
  | Control of Instr.control
  | Data of (Instr.data) list
  | ControlMerge of Instr.vstack_spec * Instr.block_vars
  | Nothing
[@@deriving sexp, compare]

type t = {
  idx: int;
  content: block_content;
} [@@deriving sexp, compare]

let to_string (b : t) : string = Printf.sprintf "block %d, %s" b.idx (match b.content with
    | Control instr -> Printf.sprintf "control block: %s" (Instr.control_to_string instr)
    | Data instrs -> Printf.sprintf "data block: %s" (String.concat ~sep:"\\l"
         (List.map instrs
            ~f:(fun instr ->
                Printf.sprintf "%s" (Instr.data_to_string instr))))
    | ControlMerge _ -> Printf.sprintf "control merge"
    | Nothing -> "empty")

let to_dot (b : t) : string =
  match b.content with
  | Data instrs ->
    Printf.sprintf "block%d [shape=record, label=\"{Data block %d:\\l\\l%s\\l}\"];"
      b.idx b.idx
      (String.concat ~sep:"\\l"
         (List.map instrs
            ~f:(fun instr ->
                Printf.sprintf "%s" (Instr.data_to_string instr))))
  | Control instr ->
    Printf.sprintf "block%d [shape=ellipse, label = \"Control block %d: %s\"];" b.idx b.idx (Instr.control_to_short_string instr)
  | ControlMerge _ ->
    Printf.sprintf "block%d [shape=point, label=\"%d\"]" b.idx b.idx
  | Nothing ->
    Printf.sprintf "block%d [shape=point, label=\"%d\"]" b.idx b.idx
