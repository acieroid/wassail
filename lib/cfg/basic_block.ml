open Core_kernel

module T = struct
  (** A basic block can either be a control block, a data block, or a merge block *)
  type 'a block_content =
    | Control of ('a Instr.control, 'a) Instr.labelled
    | Data of (Instr.data, 'a) Instr.labelled list
  [@@deriving sexp, compare, equal]

  (** A basic block *)
  type 'a t = {
    idx: int; (** Its index *)
    content: 'a block_content; (** Its content *)
  }
  [@@deriving sexp, compare, equal]
end
include T

(** Convert a block to its string representation *)
let to_string ?annot_str:(annot_str : 'a -> string = fun _ -> "") (b : 'a t) : string = Printf.sprintf "block %d, %s" b.idx (match b.content with
    | Control instr -> Printf.sprintf "control block: %s" (Instr.control_to_string instr.instr ~annot_str)
    | Data instrs -> Printf.sprintf "data block: %s" (String.concat ~sep:"\\l"
         (List.map instrs
            ~f:(fun instr ->
                Printf.sprintf "%s" (Instr.data_to_string instr.instr)))))

(** Convert a basic block to its dot representation *)
let to_dot ?annot_str:(annot_str : 'a -> string = fun _ -> "") (b : 'a t) : string =
  match b.content with
  | Data instrs ->
    Printf.sprintf "block%d [shape=record, label=\"{Data block %d:\\l\\l%s\\l}\"];"
      b.idx b.idx
      (String.concat ~sep:"\\l"
         (List.map instrs
            ~f:(fun instr ->
                Printf.sprintf "%s:%s ;; %s → %s" (Instr.Label.to_string instr.label) (Instr.data_to_string instr.instr) (annot_str instr.annotation_before) (annot_str instr.annotation_after))))
  | Control instr ->
    Printf.sprintf "block%d [shape=ellipse, label = \"Control block %d:\\l\\l%s:%s ;; %s → %s\"];" b.idx b.idx (Instr.Label.to_string instr.label) (Instr.control_to_short_string instr.instr) (annot_str instr.annotation_before) (annot_str instr.annotation_after)

(** Return all the instructions contained within this block (in no particular order) *)
let all_instructions (b : 'a t) : 'a Instr.t list =
  match b.content with
  | Control i -> [Instr.Control i]
  | Data d -> List.map d ~f:(fun i -> Instr.Data i)

(** Return all the labels of the instructions contained within this block *)
let all_instruction_labels (b : 'a t) : Instr.Label.Set.t =
  match b.content with
  | Control i -> Instr.all_labels (Control i)
  | Data d -> List.fold_left d ~init:Instr.Label.Set.empty ~f:(fun acc i ->
      Instr.Label.Set.union acc (Instr.all_labels (Data i)))

(** Return all annotations *)
let all_annots (b : 'a t) : 'a list =
  match b.content with
  | Control i -> [i.annotation_before; i.annotation_after]
  | Data d -> List.fold_left d ~init:[] ~f:(fun acc i -> [i.annotation_before; i.annotation_after] @ acc)

(** Map a function over annotations of the block *)
let map_annotations (b : 'a t) ~(f : 'a Instr.t -> 'b * 'b) : 'b t =
  { b with content = begin match b.content with
        | Control c -> Control (Instr.map_annotation_control c ~f)
        | Data instrs -> Data (List.map instrs ~f:(Instr.map_annotation_data ~f))
      end }

(** Clear the annotation of the block *)
let clear_annotation (b : 'a t) : unit t =
  map_annotations b ~f:(fun _ -> (), ())