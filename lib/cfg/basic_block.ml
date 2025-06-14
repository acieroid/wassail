open Core

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
  (* TODO: also add block annotations *)
  match b.content with
  | Data instrs ->
    let first_annot = match List.hd instrs with
      | Some instr ->
        begin match annot_str instr.annotation_before with
          | "" -> ""
          | s -> Printf.sprintf "{%s}|" s
        end
      | None -> "" in
    Printf.sprintf "block%d [shape=record, label=\"{Data block %d|%s%s}\"];"
      b.idx b.idx
      first_annot
      (String.concat ~sep:"|"
         (List.map instrs
            ~f:(fun instr ->
                let annot_after = match annot_str instr.annotation_after with
                  | "" -> ""
                  | s -> Printf.sprintf "|{%s}" s in
                Printf.sprintf "{<instr%s>%s:%s\\l}%s"
                  (Instr.Label.to_string instr.label)
                  (Instr.Label.to_string instr.label)
                  (Instr.data_to_string instr.instr)
                  annot_after)))

  | Control instr ->
    Printf.sprintf "block%d [shape=Mrecord, label=\"{Control block %d|%s<instr%s>%s:%s%s}\"];"
      b.idx b.idx
      (match annot_str instr.annotation_before with
       | "" -> ""
       | s -> Printf.sprintf "{%s}|" s)
      (Instr.Label.to_string instr.label)
      (Instr.Label.to_string instr.label)
      (Instr.control_to_short_string instr.instr)
      (match annot_str instr.annotation_after with
       | "" -> ""
       | s -> Printf.sprintf "|{%s}" s)

(** Return all the labels of the instructions directly contained in this block *)
let all_direct_instruction_labels (b : 'a t) : Instr.Label.Set.t =
  match b.content with
  | Control i -> Instr.Label.Set.singleton i.label
  | Data d -> Instr.Label.Set.of_list (List.map d ~f:(fun i -> i.label))

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

(** Check if the block is a merge block *)
let is_merge (b : 'a t) : bool =
  match b.content with
  | Control { instr = Merge; _ } -> true
  | _ -> false

(** Check if the block is a call or call_indirect block *)
let is_call (b : 'a t) : bool =
  match b.content with
  | Control { instr = Instr.Call (_, _, _); _ } -> true
  | Control { instr = Instr.CallIndirect (_, _, _, _); _ } -> true
  | _ -> false

(** Check if the block is a direct call *)
let is_direct_call (b : 'a t) : bool =
  match b.content with
  | Control { instr = Instr.Call (_, _, _); _ } -> true
  | _ -> false

(** Check if the block is a data block *)
let is_data (b : 'a t) : bool =
  match b.content with
  | Data _ -> true
  | _ -> false

(** Check if the block is a control block *)
let is_control (b : 'a t) : bool =
  match b.content with
  | Control _ -> true
  | _ -> false

(** Check if the block is an empty block *)
let is_empty (b : 'a t) : bool =
  match b.content with
  | Data [] -> true
  | _ -> false
