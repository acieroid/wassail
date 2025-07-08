open Core

module T = struct
  (** A basic block can either be a control block, a data block, or a merge block *)
  type 'a block_content =
    | Control of 'a Instr.labelled_control
    | Data of 'a Instr.labelled_data list
    | Call of 'a Instr.labelled_call
    | Entry
    | Return of 'a Instr.labelled_call (* no-op, annotated with the corresponding call *)
  [@@deriving sexp, compare, equal]

  (** A basic block *)
  type 'a t = {
    idx: int; (** Its index *)
    fidx: Int32.t; (** The function index containing this block *)
    content: 'a block_content; (** Its content *)
  }
  [@@deriving sexp, compare, equal]
end
include T

(** Convert a block to its string representation *)
let to_string ?annot_str:(annot_str : 'a -> string = fun _ -> "") (b : 'a t) : string =
  Printf.sprintf "block %d, %s" b.idx (match b.content with
    | Control instr -> Printf.sprintf "control block: %s" (Instr.control_to_string instr.instr ~annot_str)
    | Call instr -> Printf.sprintf "call block: %s" (Instr.call_to_string instr.instr)
    | Entry -> Printf.sprintf "return block"
    | Return _ -> Printf.sprintf "return block"
    | Data instrs -> Printf.sprintf "data block: %s" (String.concat ~sep:"\\l"
         (List.map instrs
            ~f:(fun instr ->
                Printf.sprintf "%s" (Instr.data_to_string instr.instr)))))

(** Convert a basic block to its dot representation *)
let to_dot ?prefix:(prefix : string = "") ?color:(color : string = "") ?annot_str:(annot_str : 'a -> string = fun _ -> "") (b : 'a t) : string =
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
    Printf.sprintf "block%s%d [shape=record, color=%s, label=\"{Data block %s%d|%s%s}\"];"
      prefix b.idx
      color
      prefix b.idx
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
  | Call instr ->
    Printf.sprintf "block%s%d [shape=Mrecord, color=%s, label=\"{Call block %s%d|%s<instr%s>%s:%s%s}\"];"
      prefix b.idx
      color
      prefix b.idx
      (match annot_str instr.annotation_before with
       | "" -> ""
       | s -> Printf.sprintf "{%s}|" s)
      (Instr.Label.to_string instr.label)
      (Instr.Label.to_string instr.label)
      (Instr.call_to_string instr.instr)
      (match annot_str instr.annotation_after with
       | "" -> ""
       | s -> Printf.sprintf "|{%s}" s)
  | Entry | Return _ -> "" (* not represented *)
  | Control instr ->
    Printf.sprintf "block%s%d [shape=Mrecord, color=%s, label=\"{Control block %s%d|%s<instr%s>%s:%s%s}\"];"
      prefix b.idx
      color
      prefix b.idx
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
  | Control { label; _ } | Call { label; _ } -> Instr.Label.Set.singleton label
  | Data d -> Instr.Label.Set.of_list (List.map d ~f:(fun i -> i.label))
  | Entry | Return _ -> Instr.Label.Set.empty

(** Return all annotations *)
let all_annots (b : 'a t) : 'a list =
  match b.content with
  | Control { annotation_before; annotation_after; _ }
  | Call { annotation_before; annotation_after; _} -> [annotation_before; annotation_after]
  | Data d -> List.fold_left d ~init:[] ~f:(fun acc i -> [i.annotation_before; i.annotation_after] @ acc)
  | Entry | Return _ -> []

(** Map a function over annotations of the block *)
let map_annotations (b : 'a t) ~(f : 'a Instr.t -> 'b * 'b) : 'b t =
  { b with content = begin match b.content with
        | Control c -> Control (Instr.map_annotation_control c ~f)
        | Call c -> Call (Instr.map_annotation_call c ~f)
        | Data instrs -> Data (List.map instrs ~f:(Instr.map_annotation_data ~f))
        | Return c -> Return (Instr.map_annotation_call c ~f)
        | Entry -> Entry
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
  | Call { instr = Instr.CallDirect (_, _, _); _ } -> true
  | Call { instr = Instr.CallIndirect (_, _, _, _); _ } -> true
  | _ -> false

(** Check if the block is a direct call *)
let is_direct_call (b : 'a t) : bool =
  match b.content with
  | Call { instr = Instr.CallDirect (_, _, _); _ } -> true
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
