open Core_kernel

type block_sort = BlockEntry | BlockExit | LoopEntry | LoopExit | Normal | Function | Return
                  [@@deriving sexp, compare, yojson]
type t = {
  idx: int;
  sort: block_sort;
  instrs: Instr.t list;
} [@@deriving sexp, compare, yojson]
let to_string (b : t) : string = Printf.sprintf "block %d" b.idx
let to_dot (b : t) : string =
  match b.sort with
  | Normal ->
    Printf.sprintf "block%d [shape=record, label=\"{Block %d:\\l\\l%s\\l}\"];"
      b.idx b.idx
      (String.concat ~sep:"\\l"
         (List.map b.instrs
            ~f:(fun instr ->
                Printf.sprintf "%s" (Instr.to_string instr ~sep:"\\l"))))
  | BlockEntry ->
    Printf.sprintf "block%d [shape=ellipse, label = \"Block entry (%d)\"];" b.idx b.idx
  | BlockExit ->
    Printf.sprintf "block%d [shape=ellipse, label = \"Block exit (%d)\"];" b.idx b.idx
  | LoopEntry ->
    Printf.sprintf "block%d [shape=ellipse, label = \"Loop entry (%d)\"];" b.idx b.idx
  | LoopExit ->
    Printf.sprintf "block%d [shape=ellipse, label = \"Loop exit (%d)\"];" b.idx b.idx
  | Function ->
    Printf.sprintf "block%d [shape=star, label=\"Direct call(%d):\\n%s\"];"
      b.idx b.idx
      (String.concat ~sep:"\\l"
         (List.map b.instrs
            ~f:(fun instr ->
                Printf.sprintf "%s" (Instr.to_string instr ~sep:"\\l"))))
  | Return ->
    Printf.sprintf "block%d [shape=point, label=\"%d\"]" b.idx b.idx
