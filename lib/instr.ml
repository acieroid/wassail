open Core_kernel
open Helpers
open Wasm

module T = struct
  (** A label is a unique identifier for an instruction *)
  type label = int
  [@@deriving sexp, compare, equal]

  (** A container for an instruction with a label *)
  type ('a, 'b) labelled = {
    label : label; (** The label of the instruction *)
    instr : 'a; (** The instruction itself *)
    annotation_before: 'b; (** The annotation before the instruction *)
    annotation_after: 'b; (** The annotation after the instruction *)
  }
  [@@deriving sexp, compare, equal]

  (** An arity: it is a pair composed of the number of elements taken from the
     stack, and the number of elements put back on the stack *)
  type arity = int * int
  [@@deriving sexp, compare, equal]

  (** Data instructions *)
  type data =
    | Nop
    | Drop
    | Select
    | MemorySize | MemoryGrow
    | Const of Prim_value.t
    | Unary of Unop.t
    | Binary of Binop.t
    | Compare of Relop.t
    | Test of Testop.t
    | Convert of Convertop.t
    | LocalGet of int
    | LocalSet of int
    | LocalTee of int
    | GlobalGet of int
    | GlobalSet of int
    | Load of Memoryop.t
    | Store of Memoryop.t

  (** Control instructions *)
  and 'a control =
    | Block of arity * 'a t list
    | Loop of arity * 'a t list
    | If of arity * 'a t list * 'a t list
    | Call of arity * int
    | CallIndirect of arity * int
    | Br of int
    | BrIf of int
    | BrTable of int list * int
    | Return
    | Unreachable

  (** Labelled control instructions *)
  and 'a labelled_control = ('a control, 'a) labelled

  (** Labelled data instructions *)
  and 'a labelled_data = (data, 'a) labelled

  (** All instructions *)
  and 'a t =
    | Data of 'a labelled_data
    | Control of 'a labelled_control
  [@@deriving sexp, compare, equal]
end
include T

(** Converts a data instruction to its string representation *)
let data_to_string (instr : data) : string =
  match instr with
     | Nop -> "nop"
     | Drop -> "drop"
     | Select -> "select"
     | MemorySize -> "memory_size"
     | MemoryGrow -> "memory_grow"
     | Const v -> Printf.sprintf "const %s" (Prim_value.to_string v)
     | Binary b -> Printf.sprintf "binary %s" (Binop.to_string b)
     | Unary u -> Printf.sprintf "unary %s" (Unop.to_string u)
     | Compare r -> Printf.sprintf "compare %s" (Relop.to_string r)
     | Test t -> Printf.sprintf "test %s" (Testop.to_string t)
     | Convert t -> Printf.sprintf "cvt %s" (Convertop.to_string t)
     | LocalGet v -> Printf.sprintf "local.get %d" v
     | LocalSet v -> Printf.sprintf "local.set %d" v
     | LocalTee v -> Printf.sprintf "local.tee %d" v
     | GlobalGet v -> Printf.sprintf "global.get %d" v
     | GlobalSet v -> Printf.sprintf "global.set %d" v
     | Load op -> Printf.sprintf "load %s" (Memoryop.to_string op)
     | Store op -> Printf.sprintf "store %s" (Memoryop.to_string op)

(** Converts a control instruction to its string representation *)
let rec control_to_string ?sep:(sep : string = "\n") ?indent:(i : int = 0) (instr : 'a control) (annot_to_string : 'a -> string) : string =
  match instr with
  | Call (_, v) -> Printf.sprintf "call %d" v
  | CallIndirect (_, v) -> Printf.sprintf "call_indirect %d" v
  | Br b -> Printf.sprintf "br %d" b
  | BrIf b -> Printf.sprintf "brif %d" b
  | BrTable (t, b) -> Printf.sprintf "br_table %s %d" (String.concat ~sep:" " (List.map t ~f:(Printf.sprintf "%d"))) b
  | Return -> "return"
  | Unreachable -> "unreachable"
  | Block (_, instrs) -> Printf.sprintf "block%s%s" sep (list_to_string instrs annot_to_string ~indent:(i+2) ~sep:sep)
  | Loop (_, instrs) -> Printf.sprintf "loop%s%s" sep (list_to_string instrs annot_to_string ~indent:(i+2) ~sep:sep)
  | If (_, instrs1, instrs2) -> Printf.sprintf "if%s%s%selse%s%s" sep
                               (list_to_string instrs1 annot_to_string ~indent:(i+2) ~sep:sep) sep sep
                               (list_to_string instrs2 annot_to_string ~indent:(i+2) ~sep:sep)

(** Converts an instruction to its string representation *)
and to_string ?sep:(sep : string = "\n") ?indent:(i : int = 0) (instr : 'a t) (annot_to_string : 'a -> string): string =
  Printf.sprintf "%s%s" (String.make i ' ')
    (match instr with
     | Data instr -> data_to_string instr.instr
     | Control instr -> control_to_string instr.instr  annot_to_string ~sep:sep ~indent:i)
and list_to_string ?indent:(i : int = 0) ?sep:(sep : string = ", ") (l : 'a t list) (annot_to_string : 'a -> string) : string =
  String.concat ~sep:sep (List.map l ~f:(fun instr -> to_string instr annot_to_string ?sep:(Some sep) ?indent:(Some i)))

(** Converts a control expression to a shorter string *)
let control_to_short_string (instr : 'a control) : string =
  match instr with
  | Block _ -> "block"
  | Loop _ -> "loop"
  | If _ -> "if"
  | _ -> control_to_string instr (fun _ -> "")

(** The instruction counter *)
let counter : label ref = ref 0

(** Creates a fresh label *)
let new_label () : label =
  let v = !counter in
  counter := !counter + 1;
  v

(** Adds a label to a data instruction *)
let data_labelled (d : data) (annotation_before : 'a) (annotation_after : 'a) : 'a t =
  Data { instr = d; label = new_label (); annotation_before; annotation_after; }

(** Adds a label to a control instruction *)
let control_labelled (c : 'a control) (annotation_before : 'a) (annotation_after : 'a) : 'a t =
  Control { instr = c; label = new_label (); annotation_before; annotation_after }

(** Create an instruction from a WebAssembly instruction *)
let rec of_wasm (m : Ast.module_) (i : Ast.instr) : unit t =
  match i.it with
  | Ast.Nop -> data_labelled Nop () ()
  | Ast.Drop -> data_labelled Drop () ()
  | Ast.Block (st, instrs) ->
    let (arity_in, arity_out) = Wasm_helpers.arity_of_block st in
    assert (arity_in = 0); (* what does it mean to have arity_in > 0? *)
    assert (arity_out <= 1);
    let body = seq_of_wasm m instrs in
    control_labelled (Block ((arity_in, arity_out), body)) () ()
  | Ast.Const lit ->
    data_labelled (Const (Prim_value.of_wasm lit.it)) () ()
  | Ast.Binary bin ->
    data_labelled (Binary (Binop.of_wasm bin)) () ()
  | Ast.Compare rel ->
    data_labelled (Compare (Relop.of_wasm rel)) () ()
  | Ast.LocalGet l ->
    data_labelled (LocalGet (Index.of_wasm l)) () ()
  | Ast.LocalSet l ->
    data_labelled (LocalSet (Index.of_wasm l)) () ()
  | Ast.LocalTee l ->
    data_labelled (LocalTee (Index.of_wasm l)) () ()
  | Ast.BrIf label ->
    control_labelled (BrIf (Index.of_wasm label)) () ()
  | Ast.Br label ->
    control_labelled (Br (Index.of_wasm label)) () ()
  | Ast.BrTable (table, label) ->
    control_labelled (BrTable (List.map table ~f:Index.of_wasm, Index.of_wasm label)) () ()
  | Ast.Call f ->
    let (arity_in, arity_out) = Wasm_helpers.arity_of_fun m f in
    assert (arity_out <= 1);
    control_labelled (Call ((arity_in, arity_out), Index.of_wasm f)) () ()
  | Ast.Return ->
    control_labelled (Return) () ()
  | Ast.Unreachable ->
    control_labelled (Unreachable) () ()
  | Ast.Select ->
    data_labelled (Select) () ()
  | Ast.Loop (st, instrs) ->
    let (arity_in, arity_out) = Wasm_helpers.arity_of_block st in
    assert (arity_in = 0); (* what does it mean to have arity_in > 0 for a loop? *)
    assert (arity_out <= 1); (* TODO: support any arity out? *)
    let body = seq_of_wasm m instrs in
    control_labelled (Loop ((arity_in, arity_out), body)) () ()
  | Ast.If (st, instrs1, instrs2) ->
    let (arity_in, arity_out) = Wasm_helpers.arity_of_block st in
    let body1 = seq_of_wasm m instrs1 in
    let body2 = seq_of_wasm m instrs2 in
    control_labelled (If ((arity_in, arity_out), body1, body2)) () ()
  | Ast.CallIndirect f ->
    let (arity_in, arity_out) = Wasm_helpers.arity_of_fun_type m f in
    assert (arity_out <= 1);
    control_labelled (CallIndirect ((arity_in, arity_out), Index.of_wasm f)) () ()
  | Ast.GlobalGet g ->
    data_labelled (GlobalGet (Index.of_wasm g)) () ()
  | Ast.GlobalSet g ->
    data_labelled (GlobalSet (Index.of_wasm g)) () ()
  | Ast.Load op ->
    data_labelled (Load (Memoryop.of_wasm_load op)) () ()
  | Ast.Store op ->
    data_labelled (Store (Memoryop.of_wasm_store op)) () ()
  | Ast.MemorySize -> data_labelled (MemorySize) () ()
  | Ast.MemoryGrow -> data_labelled MemoryGrow () ()
  | Ast.Test op ->
    data_labelled (Test (Testop.of_wasm op)) () ()
  | Ast.Convert op ->
    data_labelled (Convert (Convertop.of_wasm op)) () ()
  | Ast.Unary op ->
    data_labelled (Unary (Unop.of_wasm op)) () ()

(** Creates a sequence of instructions from their Wasm representation *)
and seq_of_wasm (m : Ast.module_) (is : Ast.instr list) : unit t list =
  List.map is ~f:(of_wasm m)

(** Change the annotation of an instruction, where `data` is a map from instruction indices to the new annotation *)
let rec annotate (i : 'a t) (data : ('b * 'b) IntMap.t) : 'b t =
  match i with
  | Data d -> Data (annotate_data d data)
  | Control c -> Control (annotate_control c data)
and annotate_data (i : (data, 'a) labelled) (data : ('b * 'b) IntMap.t) : (data, 'b) labelled =
  let annotation_before, annotation_after = IntMap.find_exn data i.label in
  { i with annotation_before; annotation_after }
and annotate_control (i : ('a control, 'a) labelled) (data : ('b * 'b) IntMap.t) : ('b control, 'b) labelled =
  let annotation_before, annotation_after = IntMap.find_exn data i.label in
  { i with annotation_before; annotation_after;
           instr = match i.instr with
             | Block (arity, instrs) -> Block (arity, List.map instrs ~f:(fun i -> annotate i data))
             | Loop (arity, instrs) -> Loop (arity, List.map instrs ~f:(fun i -> annotate i data))
             | If (arity, then_, else_) -> If (arity,
                                               List.map then_ ~f:(fun i -> annotate i data),
                                               List.map else_ ~f:(fun i -> annotate i data))
             | Call (arity, f) -> Call (arity, f)
             | CallIndirect (arity, f) -> CallIndirect (arity, f)
             | Br n -> Br n
             | BrIf n -> BrIf n
             | BrTable (l, n) -> BrTable (l, n)
             | Return -> Return
             | Unreachable -> Unreachable }

let annotation_before (i : 'a t) : 'a =
  match i with
  | Data d -> d.annotation_before
  | Control c -> c.annotation_before

let annotation_after (i : 'a t) : 'a =
  match i with
  | Data d -> d.annotation_after
  | Control c -> c.annotation_after

let rec all_labels (i : 'a t) : IntSet.t =
  match i with
  | Data d -> IntSet.singleton d.label
  | Control c -> IntSet.add (begin match c.instr with
      | Block (_, instrs)
      | Loop (_, instrs) -> List.fold_left instrs ~init:IntSet.empty ~f:(fun acc i ->
          IntSet.union acc (all_labels i))
      | If (_, instrs1, instrs2) ->
        List.fold_left (instrs1 @ instrs2) ~init:IntSet.empty ~f:(fun acc i ->
          IntSet.union acc (all_labels i))
      | _ -> IntSet.empty
    end) c.label
