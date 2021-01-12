open Core_kernel
open Helpers

module T = struct
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
    annotation_before : 'a;
    annotation_after : 'a;
  }
  [@@deriving sexp, compare, equal]
end
include T

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
                Printf.sprintf "%d:%s ;; %s → %s" instr.label (Instr.data_to_string instr.instr) (annot_to_string instr.annotation_before) (annot_to_string instr.annotation_after))))
  | Control instr ->
    Printf.sprintf "block%d [shape=ellipse, label = \"Control block %d:\\l\\l%d:%s ;; %s → %s\"];" b.idx b.idx instr.label (Instr.control_to_short_string instr.instr) (annot_to_string instr.annotation_before) (annot_to_string instr.annotation_after)
  | ControlMerge ->
    Printf.sprintf "block%d [shape=diamond, label=\"%d\"]" b.idx b.idx

(** Returns all the instructions contained within this block (in no particular order) *)
let all_instructions (b : 'a t) : 'a Instr.t list =
  match b.content with
  | Control i -> [Instr.Control i]
  | Data d -> List.map d ~f:(fun i -> Instr.Data i)
  | ControlMerge -> []

(** Returns all the labels of the instructions contained within this block *)
let all_instruction_labels (b : 'a t) : IntSet.t =
  match b.content with
  | Control i -> Instr.all_labels (Control i)
  | Data d -> List.fold_left d ~init:IntSet.empty ~f:(fun acc i ->
      IntSet.union acc (Instr.all_labels (Data i)))
  | ControlMerge -> IntSet.empty

let all_annots (b : 'a t) : 'a list =
  match b.content with
  | Control i -> [i.annotation_before; i.annotation_after]
  | Data d -> List.fold_left d ~init:[] ~f:(fun acc i -> [i.annotation_before; i.annotation_after] @ acc)
  | ControlMerge -> [b.annotation_before; b.annotation_after]

(** Change the annotations of a basic block *)
let annotate (b : 'a t) (block_data : ('b * 'b) IntMap.t) (instr_data : ('b * 'b) IntMap.t) : 'b t =
  let (annotation_before, annotation_after) = match IntMap.find block_data b.idx with
    | Some r -> r
    | None -> failwith "Basic_block.annotate: can't find data" in
  { b with content = begin match b.content with
        | Control c -> Control (Instr.annotate_control c instr_data)
        | Data instrs -> Data (List.map instrs ~f:(fun i -> Instr.annotate_data i instr_data))
        | ControlMerge -> ControlMerge
      end;
           annotation_before;
           annotation_after }

(** Add more annotations to an already-annotated CFG *)
let add_annotation (b : 'a t) (block_data : ('b * 'b) IntMap.t) (instr_data : ('b * 'b) IntMap.t) : ('a * 'b) t =
  let (annotation_before, annotation_after) = match IntMap.find block_data b.idx with
    | Some r -> r
    | None -> failwith "Basic_block.add_annotation: can't find data" in
  { b with content = begin match b.content with
        | Control c -> Control (Instr.add_annotation_control c instr_data)
        | Data instrs -> Data (List.map instrs ~f:(fun i -> Instr.add_annotation_data i instr_data))
        | ControlMerge -> ControlMerge
      end;
           annotation_before = (b.annotation_before, annotation_before);
           annotation_after = (b.annotation_after, annotation_after) }

(** Maps a function over annotations of the block *)
let map_annotations (b : 'a t) ~(f : 'a -> 'b) : 'b t =
  { b with content = begin match b.content with
        | Control c -> Control (Instr.map_annotation_control c ~f:f)
        | Data instrs -> Data (List.map instrs ~f:(Instr.map_annotation_data ~f:f))
        | ControlMerge -> ControlMerge
      end;
           annotation_before = f b.annotation_before;
           annotation_after = f b.annotation_after }

(** Clear the annotation of the block *)
let clear_annotation (b : 'a t) : unit t =
  map_annotations b ~f:(fun _ -> ())
                                 
