open Core_kernel
open Helpers

(** A basic block can either be a control block, a data block, or a merge block *)
type 'a block_content =
  | Control of ('a Instr.control, 'a) Instr.labelled
  | Data of (Instr.data, 'a) Instr.labelled list
  | ControlMerge
[@@deriving sexp, compare, equal]

(** A basic block *)
type 'a t = {
  idx: int; (** Its index *)
  content: 'a block_content; (** Its content *)
}
[@@deriving sexp, compare, equal]

(** Convert a block to its string representation *)
let to_string (b : 'a t) (annot_to_string : 'a -> string) : string = Printf.sprintf "block %d, %s" b.idx (match b.content with
    | Control instr -> Printf.sprintf "control block: %s" (Instr.control_to_string instr.instr annot_to_string)
    | Data instrs -> Printf.sprintf "data block: %s" (String.concat ~sep:"\\l"
         (List.map instrs
            ~f:(fun instr ->
                Printf.sprintf "%s" (Instr.data_to_string instr.instr))))
    | ControlMerge -> Printf.sprintf "control merge")

(** Convert a basic block to its dot representation *)
let to_dot (b : 'a t) (annot_to_string : 'a -> string) : string =
  match b.content with
  | Data instrs ->
    Printf.sprintf "block%d [shape=record, label=\"{Data block %d:\\l\\l%s\\l}\"];"
      b.idx b.idx
      (String.concat ~sep:"\\l"
         (List.map instrs
            ~f:(fun instr ->
                Printf.sprintf "%s ;; %s → %s" (Instr.data_to_string instr.instr) (annot_to_string instr.annotation_before) (annot_to_string instr.annotation_after))))
  | Control instr ->
    Printf.sprintf "block%d [shape=ellipse, label = \"Control block %d:\\l\\l%s ;; %s → %s\"];" b.idx b.idx (Instr.control_to_short_string instr.instr) (annot_to_string instr.annotation_before) (annot_to_string instr.annotation_after)
  | ControlMerge ->
    Printf.sprintf "block%d [shape=diamond, label=\"%d\"]" b.idx b.idx

(** Change the annotations of a basic block *)
let annotate (b : 'a t) (data : ('b * 'b) IntMap.t) : 'b t =
  { b with content = match b.content with
        | Control c -> Control (Instr.annotate_control c data)
        | Data instrs -> Data (List.map instrs ~f:(fun i -> Instr.annotate_data i data))
        | ControlMerge -> ControlMerge }
