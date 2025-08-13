open Core

module T = struct
  (** A basic block can either be a control block, a data block, or a merge block *)
  type 'a block_content =
    | Control of 'a Instr.labelled_control
    | Data of 'a Instr.labelled_data list
    | Call of 'a Instr.labelled_call
    | Entry
    | Return of 'a Instr.labelled_call (* no-op, annotated with the corresponding call *)
    | Imported of Wasm_module.func_desc (* Dummy block for imported functions *)
  [@@deriving sexp, compare, equal]

  (** A basic block *)
  type 'a t = {
    idx: int; (** Its index *)
    fidx: Int32.t; (** The function index containing this block *)
    content: 'a block_content; (** Its content *)
    annotation_before: 'a;
    annotation_after: 'a;
  }
  [@@deriving sexp, compare, equal]
end
include T

(** Convert a block to its string representation *)
let to_string ?annot_str:(annot_str : 'a -> string = fun _ -> "") (b : 'a t) : string =
  Printf.sprintf "block %d, %s" b.idx (match b.content with
    | Control instr -> Printf.sprintf "control block: %s" (Instr.control_to_string instr.instr ~annot_str)
    | Call instr -> Printf.sprintf "call block: %s" (Instr.call_to_string instr.instr)
    | Entry -> Printf.sprintf "entry block"
    | Return _ -> Printf.sprintf "return block"
    | Imported desc -> Printf.sprintf "import %ld" desc.idx
    | Data instrs -> Printf.sprintf "data block: %s" (String.concat ~sep:"\\l"
         (List.map instrs
            ~f:(fun instr ->
                Printf.sprintf "%s" (Instr.data_to_string instr.instr)))))

(** Convert a basic block to its dot representation *)
let to_dot
    ?prefix:(prefix : string = "")
    ?color:(color : string = "black")
    ?annot_str:(annot_str : 'a -> string = fun _ -> "")
    (b : 'a t) : string =
  match b.content with
  | Data instrs ->
    let first_annot = match List.hd instrs with
      | Some instr ->
        annot_str instr.annotation_before
      | None -> "" in
    Printf.sprintf "block%s%d [shape=none, color=%s, label=<<table><tr><td colspan=\"2\">Data block %s%d</td></tr>%s%s</table>>];"
      prefix b.idx
      color
      prefix b.idx
      first_annot
      (String.concat ~sep:"|"
         (List.map instrs
            ~f:(fun instr ->
                Printf.sprintf "<tr><td>%s</td><td>%s</td></tr>%s"
                  (Instr.Label.to_string instr.label)
                  (Instr.data_to_string instr.instr)
                  (annot_str instr.annotation_after))))
  | Call instr ->
    Printf.sprintf "block%s%d [shape=none, color=%s, label=<<table><tr><td colspan=\"2\">Call block %s%d</td></tr>%s<tr><td>%s</td><td>%s</td></tr>%s</table>>];"
      prefix b.idx
      color
      prefix b.idx
      (annot_str instr.annotation_before)
      (Instr.Label.to_string instr.label)
      (Instr.call_to_string instr.instr)
      (annot_str instr.annotation_after)
  | Imported desc ->
    Printf.sprintf "block%s%d [shape=none, color=%s, label=<<table><tr><td>Imported function %ld</td></tr></table>>];"
      prefix b.idx
      color
      desc.idx
  | Entry | Return _ -> "" (* not represented here *)
  | Control instr ->
    Printf.sprintf "block%s%d [shape=none, color=%s, label=<<table><tr><td colspan=\"2\">Control block %s%d</td></tr>%s<tr><td>%s</td><td>%s</td></tr>%s</table>>];"
      prefix b.idx
      color
      prefix b.idx
      (annot_str instr.annotation_before)
      (Instr.Label.to_string instr.label)
      (Instr.control_to_short_string instr.instr)
      (annot_str instr.annotation_after)

(** Return all the labels of the instructions directly contained in this block *)
let all_direct_instruction_labels (b : 'a t) : Instr.Label.Set.t =
  match b.content with
  | Control { label; _ } | Call { label; _ } -> Instr.Label.Set.singleton label
  | Data d -> Instr.Label.Set.of_list (List.map d ~f:(fun i -> i.label))
  | Entry | Return _ | Imported _ -> Instr.Label.Set.empty

(** Return all annotations *)
let all_annots (b : 'a t) : 'a list =
  match b.content with
  | Control { annotation_before; annotation_after; _ }
  | Call { annotation_before; annotation_after; _} -> [annotation_before; annotation_after]
  | Data d -> List.fold_left d ~init:[] ~f:(fun acc i -> [i.annotation_before; i.annotation_after] @ acc)
  | Entry | Return _ | Imported _ -> []

(** Map a function over annotations of the block *)
let map_annotations (b : 'a t) ~(f : 'a Instr.t -> 'b * 'b) (before : 'b) (after :'b) : 'b t =
  { b with content = begin match b.content with
        | Control c -> Control (Instr.map_annotation_control c ~f)
        | Call c -> Call (Instr.map_annotation_call c ~f)
        | Data instrs -> Data (List.map instrs ~f:(Instr.map_annotation_data ~f))
        | Return c -> Return (Instr.map_annotation_call c ~f)
        | Entry -> Entry
        | Imported desc -> Imported desc
      end;
           annotation_before = before;
           annotation_after = after;
  }

(** Clear the annotation of the block *)
let clear_annotation (b : 'a t) : unit t =
  map_annotations b ~f:(fun _ -> (), ()) () ()

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
