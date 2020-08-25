open Core_kernel

(** A basic block can either be a control block, a data block, or a merge block *)
type block_content =
  | Control of Instr.control Instr.labelled
  | Data of Instr.data Instr.labelled list
  | ControlMerge
[@@deriving sexp, compare]

(** A basic block *)
type t = {
  idx: int; (** Its index *)
  content: block_content; (** Its content *)
} [@@deriving sexp, compare]

let to_string (b : t) : string = Printf.sprintf "block %d, %s" b.idx (match b.content with
    | Control instr -> Printf.sprintf "control block: %s" (Instr.control_to_string instr.instr)
    | Data instrs -> Printf.sprintf "data block: %s" (String.concat ~sep:"\\l"
         (List.map instrs
            ~f:(fun instr ->
                Printf.sprintf "%s" (Instr.data_to_string instr.instr))))
    | ControlMerge -> Printf.sprintf "control merge")

let to_dot (b : t) : string =
  match b.content with
  | Data instrs ->
    Printf.sprintf "block%d [shape=record, label=\"{Data block %d:\\l\\l%s\\l}\"];"
      b.idx b.idx
      (String.concat ~sep:"\\l"
         (List.map instrs
            ~f:(fun instr ->
                Printf.sprintf "%s" (Instr.data_to_string instr.instr))))
  | Control instr ->
    Printf.sprintf "block%d [shape=ellipse, label = \"Control block %d:\\l\\l%s\"];" b.idx b.idx (Instr.control_to_short_string instr.instr)
  | ControlMerge ->
    Printf.sprintf "block%d [shape=point, label=\"%d\"]" b.idx b.idx
