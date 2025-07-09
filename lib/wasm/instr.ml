open Core

module Label = struct
  (** The section in which an instruction is contained *)
  type section =
    | Function of Int32.t (** Instruction is part of the function with the given index *)
    | Elem of Int32.t (** Instruction is part of table elements with the given index *)
    | Data of Int32.t (** Instruction is part of data element with the given index *)
    | MergeInFunction of Int32.t (** Instruction is a merge instruction in the given function *)
    | Dummy (** Dummy section, introduced by an extra processing phase (e.g., slicing) *)
  [@@deriving sexp, compare, equal]

  let section_to_string (s : section) = match s with
    | Function n -> Printf.sprintf "%ld" n
    | Elem n -> Printf.sprintf "elem%ld" n
    | Data n -> Printf.sprintf "data%ld" n
    | MergeInFunction n -> Printf.sprintf "m%ld" n
    | Dummy -> "d"

  module T = struct
    (** A label is a unique identifier for an instruction *)
    type t = {
      section: section;
      id: int;
    }
    [@@deriving sexp, compare, equal]

    let to_string (l : t) : string = match l.section with
      | Function _ -> Printf.sprintf "%d" l.id (* Printed differently to have a cleaner output *)
      | _ -> Printf.sprintf "%s_%d" (section_to_string l.section) l.id

  end
  include T

  let maker (section : section) : unit -> t =
    let counter = ref 0 in
    fun () ->
      let id = !counter in
      counter := !counter + 1;
      { section; id }

  module Set = struct
    module T = struct
      include Set.Make(T)
      let to_string (t : t) : string = String.concat ~sep:"," (List.map ~f:T.to_string (Set.to_list t))
    end
    include Set
    include T
    include Test.HelpersForSet(T)
  end
  module Map = struct
    include Map
    include Map.Make(T)
  end

  (** Test data *)
  module Test = struct
    let lab ?fidx:(fidx : int32 = 0l) (n : int) = {
      section = Function fidx;
      id = n;
    }
    let merge ?fidx:(fidx : int32 = 0l) (n : int) = {
      section = MergeInFunction fidx;
      id = n;
    }
  end
end

module T = struct
  (** A container for an instruction with a label *)
  type ('a, 'b) labelled = {
    label : Label.t; (** The label of the instruction *)
    line_number: int; (** The line number of the instruction *)
    instr : 'a; (** The instruction itself *)
    annotation_before: 'b; (** The annotation before the instruction *)
    annotation_after: 'b; (** The annotation after the instruction *)
  }
  [@@deriving sexp, compare, equal]

  (** An arity: it is a pair composed of the number of elements taken from the
     stack, and the number of elements put back on the stack *)
  type arity = int * int
  [@@deriving sexp, compare, equal]

  (** The type of a block *)
  type block_type = Type.t list * Type.t list
  [@@deriving sexp, compare, equal]

  (** Data instructions *)
  type data =
    | Nop
    | Drop
    | Select of Type.t list option
    | MemorySize | MemoryGrow
    | MemoryFill | MemoryCopy | MemoryInit of Int32.t
    | Const of Prim_value.t
    | Unary of Unop.t
    | Binary of Binop.t
    | Compare of Relop.t
    | Test of Testop.t
    | Convert of Convertop.t
    | LocalGet of Int32.t
    | LocalSet of Int32.t
    | LocalTee of Int32.t
    | GlobalGet of Int32.t
    | GlobalSet of Int32.t
    | Load of Memoryop.t
    | Store of Memoryop.t
    | RefNull of Ref_type.t
    | RefFunc of Int32.t
    | RefIsNull

  (** Control instructions *)
  and 'a control =
    | Block of block_type * arity * 'a t list
    | Loop of block_type * arity * 'a t list
    | If of block_type * arity * 'a t list * 'a t list
    | Br of Int32.t
    | BrIf of Int32.t
    | BrTable of Int32.t list * Int32.t
    | Return
    | Unreachable
    | Merge (* Special instruction not existing in Wasm, used to handle control-flow merges *)

  (** Call instructions *)
  and call =
    | CallDirect of arity * (Type.t list * Type.t list) * Int32.t
    | CallIndirect of Int32.t * arity * (Type.t list * Type.t list) * Int32.t

  (** Labelled control instructions *)
  and 'a labelled_control = ('a control, 'a) labelled

  (** Labelled data instructions *)
  and 'a labelled_data = (data, 'a) labelled

  (** Labelled call instruction *)
  and 'a labelled_call = (call, 'a) labelled

  (** All instructions *)
  and 'a t =
    | Data of 'a labelled_data
    | Control of 'a labelled_control
    | Call of 'a labelled_call
  [@@deriving sexp, compare, equal]

end

include T

let call_types (instr : 'a labelled_call) : Type.t list * Type.t list = match instr.instr with
  | CallDirect (_, ts, _) -> ts
  | CallIndirect (_, _, ts, _) -> ts

let is_block (instr : 'a t) : bool = match instr with
  | Control { instr = Block _; _ } -> true
  | Control { instr = Loop _; _ } -> true
  | Control { instr = If _; _ } -> true
  | _ -> false

(** Return the label of an instruction *)
let label (instr : 'a t) : Label.t = match instr with
  | Data i -> i.label
  | Control i -> i.label
  | Call i -> i.label

(** Convert a data instruction to its string representation *)
let data_to_string (instr : data) : string =
  match instr with
  | Nop -> "nop"
  | Drop -> "drop"
  | Select _ -> "select"
  | MemorySize -> "memory.size"
  | MemoryGrow -> "memory.grow"
  | MemoryCopy -> "memory.copy"
  | MemoryFill -> "memory.fill"
  | MemoryInit m -> Printf.sprintf "memory.init %ld" m
  | Const v -> Printf.sprintf "%s.const %s" (Type.to_string (Prim_value.typ v)) (Prim_value.to_string v)
  | Binary b -> Printf.sprintf "%s" (Binop.to_string b)
  | Unary u -> Printf.sprintf "%s" (Unop.to_string u)
  | Compare r -> Printf.sprintf "%s" (Relop.to_string r)
  | Test t -> Printf.sprintf "%s" (Testop.to_string t)
  | Convert t -> Printf.sprintf "%s" (Convertop.to_string t)
  | LocalGet v -> Printf.sprintf "local.get %s" (Int32.to_string v)
  | LocalSet v -> Printf.sprintf "local.set %s" (Int32.to_string v)
  | LocalTee v -> Printf.sprintf "local.tee %s" (Int32.to_string v)
  | GlobalGet v -> Printf.sprintf "global.get %s" (Int32.to_string v)
  | GlobalSet v -> Printf.sprintf "global.set %s" (Int32.to_string v)
  | Load op ->
    let memop = Memoryop.to_string op in
    Printf.sprintf "%s.load%s%s%s"
      (Type.to_string op.typ)
      (Memoryop.suffix_to_string op true)
      (if String.is_empty memop then "" else " ")
      memop
  | Store op ->
    let memop = Memoryop.to_string op in
    Printf.sprintf "%s.store%s%s%s"
      (Type.to_string op.typ)
      (Memoryop.suffix_to_string op false)
      (if String.is_empty memop then "" else " ")
      memop
  | RefNull t -> Printf.sprintf "ref.null %s" (Ref_type.to_string t)
  | RefFunc f -> Printf.sprintf "ref.func %ld" f
  | RefIsNull -> "ref.isnull"

let call_to_string (instr : call) : string =
  match instr with
  | CallDirect (_, _, v) -> Printf.sprintf "call %s" (Int32.to_string v)
  | CallIndirect (_, _, _, v) -> Printf.sprintf "call_indirect (type %ld)" v

let block_type_to_string (bt : block_type) : string =
  let to_str (ts : Type.t list) : string = String.concat ~sep:" " (List.map ~f:Type.to_string ts) in
  match bt with
  | [], [] -> ""
  | i, [] -> Printf.sprintf " (param %s)" (to_str i)
  | [], o -> Printf.sprintf " (result %s)" (to_str o)
  | i, o -> Printf.sprintf " (param %s) (result %s)" (to_str i) (to_str o)

(** Converts a control instruction to its string representation *)
let rec control_to_string ?sep:(sep : string = "\n") ?indent:(i : int = 0) ?annot_str:(annot_to_string : 'a -> string = fun _ -> "") (instr : 'a control)  : string =
  match instr with
  | Br b -> Printf.sprintf "br %s" (Int32.to_string b)
  | BrIf b -> Printf.sprintf "br_if %s" (Int32.to_string b)
  | BrTable (t, b) -> Printf.sprintf "br_table %s %s" (String.concat ~sep:" " (List.map t ~f:Int32.to_string)) (Int32.to_string b)
  | Return -> "return"
  | Unreachable -> "unreachable"
  | Block (bt, _, instrs) -> Printf.sprintf "block%s%s%s%s%send" (block_type_to_string bt) sep (list_to_string instrs annot_to_string ~indent:(i+2) ~sep:sep) sep (String.make i ' ')
  | Loop (bt, _, instrs) -> Printf.sprintf "loop%s%s%s%s%send" (block_type_to_string bt) sep (list_to_string instrs annot_to_string ~indent:(i+2) ~sep:sep) sep (String.make i ' ')
  | If (bt, _, instrs1, []) -> Printf.sprintf "if%s%s%s%s%send"
                                 (block_type_to_string bt)
                                 sep (list_to_string instrs1 annot_to_string ~indent:(i+2) ~sep:sep) sep (String.make i ' ')
  | If (bt, _, instrs1, instrs2) -> Printf.sprintf "if%s%s%s%s%selse%s%s%s%send"
                                      (block_type_to_string bt)
                                      sep (list_to_string instrs1 annot_to_string ~indent:(i+2) ~sep:sep) sep (String.make i ' ')
                                      sep (list_to_string instrs2 annot_to_string ~indent:(i+2) ~sep:sep) sep (String.make i ' ')
  | Merge -> "merge"

(** Converts an instruction to its string representation *)
and to_string ?sep:(sep : string = "\n") ?indent:(i : int = 0) ?annot_str:(annot_to_string : 'a -> string = fun _ -> "") (instr : 'a t) : string =
  Printf.sprintf "%s%s" (String.make i ' ')
    (match instr with
     | Data instr -> data_to_string instr.instr
     | Control instr -> control_to_string instr.instr ~annot_str:annot_to_string ~sep:sep ~indent:i
     | Call instr -> call_to_string instr.instr)
    (* (Label.to_string (label instr)) *)
and list_to_string ?indent:(i : int = 0) ?sep:(sep : string = ", ") (l : 'a t list) (annot_to_string : 'a -> string) : string =
  String.concat ~sep:sep (List.map l ~f:(fun instr -> to_string instr ~annot_str:annot_to_string ?sep:(Some sep) ?indent:(Some i)))

module Set = struct
  module TUnit = struct
    type t = unit T.t
    [@@deriving sexp, compare, equal]
    let to_string (instr : unit T.t) = to_string instr
  end
  module T = struct
    include Set.Make(TUnit)
    let to_string (t : t) : string = String.concat ~sep:"," (List.map ~f:TUnit.to_string (Set.to_list t))
  end
  include T
    include Test.HelpersForSet(T)
  end


(** Converts a control expression to a shorter string *)
let control_to_short_string (instr : 'a control) : string =
  match instr with
  | Block _ -> "block"
  | Loop _ -> "loop"
  | If _ -> "if"
  | _ -> control_to_string instr ~annot_str:(fun _ -> "")

(** Converts an instruction to its mnemonic *)
let to_mnemonic (instr : 'a t) : string = match instr with
  | Data d -> begin match d.instr with
      | Nop -> "nop"
      | Drop -> "drop"
      | Select _ -> "select"
      | MemorySize -> "memory.size"
      | MemoryGrow -> "memory.grow"
      | MemoryCopy -> "memory.copy"
      | MemoryFill -> "memory.fill"
      | MemoryInit _ -> "memory.init"
      | Const v -> Printf.sprintf "%s.const" (Type.to_string (Prim_value.typ v))
      | Unary op -> Unop.to_mnemonic op
      | Binary op -> Binop.to_mnemonic op
      | Compare op -> Relop.to_mnemonic op
      | Test op -> Testop.to_mnemonic op
      | Convert op -> Convertop.to_mnemonic op
      | LocalGet _ -> "local.get"
      | LocalSet _ -> "local.set"
      | LocalTee _ -> "local.tee"
      | GlobalGet _ -> "global.get"
      | GlobalSet _ -> "global.set"
      | Load _ -> "load"
      | Store _ -> "store"
      | RefNull _ -> "ref.null"
      | RefFunc _ -> "ref.func"
      | RefIsNull -> "ref.isnull"
    end
  | Control c -> begin match c.instr with
      | Block (_, _, _) -> "block"
      | Loop (_, _, _) -> "loop"
      | If (_, _, _, _) -> "if"
      | Br _ -> "br"
      | BrIf _ -> "br_if"
      | BrTable (_, _) -> "br_table"
      | Return -> "return"
      | Unreachable -> "unreachable"
      | Merge -> "merge"
    end
  | Call c -> begin match c.instr with
      | CallDirect (_, _, _) -> "call"
      | CallIndirect (_, _, _, _) -> "call_indirect"
    end

(** Create an instruction from a WebAssembly instruction *)
let rec of_wasm (m : Wasm.Ast.module_) (new_label : unit -> Label.t) (i : Wasm.Ast.instr) : unit t =
  let line_number = i.at.left.line in
  (* Construct a labelled data instruction *)
  let data_labelled ?label:(lab : Label.t option) (instr : data) : unit t =
    let label = match lab with
      | Some l -> l
      | None -> new_label () in
    Data { instr; label; line_number; annotation_before = (); annotation_after = (); } in
  (* Construct a labelled control instruction *)
  let control_labelled ?label:(lab : Label.t option) (instr : 'a control) : 'a t =
    let label = match lab with
      | Some l -> l
      | None -> new_label () in
    Control { instr; label; line_number; annotation_before = (); annotation_after = (); } in
  let call_labelled ?label:(lab : Label.t option) (instr : call) : 'a t =
    let label = match lab with
      | Some l -> l
      | None -> new_label () in
    Call { instr; label; line_number; annotation_before = (); annotation_after = (); } in
  match i.it with
  | Nop -> data_labelled Nop
  | Drop -> data_labelled Drop
  | Block (st, instrs) ->
    let block_type = Wasm_helpers.type_of_block m st in
    let (arity_in, arity_out) = Wasm_helpers.arity_of_block m st in
    let label = new_label () in
    let body = seq_of_wasm m new_label instrs in
    control_labelled ~label:label (Block (block_type, (arity_in, arity_out), body))
  | Const lit ->
    data_labelled (Const (Prim_value.of_wasm_num lit.it))
  | Binary bin ->
    data_labelled (Binary (Binop.of_wasm bin))
  | Compare rel ->
    data_labelled (Compare (Relop.of_wasm rel))
  | LocalGet l ->
    data_labelled (LocalGet l.it)
  | LocalSet l ->
    data_labelled (LocalSet l.it)
  | LocalTee l ->
    data_labelled (LocalTee l.it)
  | BrIf label ->
    control_labelled (BrIf label.it)
  | Br label ->
    control_labelled (Br label.it)
  | BrTable (table, label) ->
    control_labelled (BrTable (List.map table ~f:(fun v -> v.it), label.it))
  | Call f ->
    let ((arity_in, arity_out), t) = Wasm_helpers.arity_and_type_of_fun m f in
    if arity_out > 1 then
      failwith "Unsupported: direct function call with more than one return value";
    call_labelled (CallDirect ((arity_in, arity_out), t, f.it))
  | CallIndirect (table, f) ->
    let ((arity_in, arity_out), t) = Wasm_helpers.arity_and_type_of_fun_type m f in
    if arity_out > 1 then
      failwith "Unsupported: indirect function call with more than one return value"
    else
    call_labelled (CallIndirect (table.it, (arity_in, arity_out), t, f.it))
  | Return ->
    control_labelled (Return)
  | Unreachable ->
    control_labelled (Unreachable)
  | Select types ->
    data_labelled (Select (Option.map ~f:(List.map ~f:Type.of_wasm) types))
  | Loop (st, instrs) ->
    let (arity_in, arity_out) = Wasm_helpers.arity_of_block m st in
    assert (arity_in = 0); (* what does it mean to have arity_in > 0 for a loop? *)
    assert (arity_out <= 1); (* TODO: support any arity out? *)
    let label = new_label () in
    let body = seq_of_wasm m new_label instrs in
    control_labelled ~label:label (Loop (Wasm_helpers.type_of_block m st, (arity_in, arity_out), body))
  | If (st, instrs1, instrs2) ->
    let (arity_in, arity_out) = Wasm_helpers.arity_of_block m st in
    let label = new_label () in
    let body1 = seq_of_wasm m new_label instrs1 in
    let body2 = seq_of_wasm m new_label instrs2 in
    control_labelled ~label:label (If (Wasm_helpers.type_of_block m st, (arity_in, arity_out), body1, body2))
  | GlobalGet g ->
    data_labelled (GlobalGet g.it)
  | GlobalSet g ->
    data_labelled (GlobalSet g.it)
  | Load op ->
    data_labelled (Load (Memoryop.of_wasm_load op))
  | Store op ->
    data_labelled (Store (Memoryop.of_wasm_store op))
  | MemorySize -> data_labelled MemorySize
  | MemoryGrow -> data_labelled MemoryGrow
  | MemoryFill -> data_labelled MemoryFill
  | MemoryCopy -> data_labelled MemoryCopy
  | MemoryInit m -> data_labelled (MemoryInit m.it)
  | Test op ->
    data_labelled (Test (Testop.of_wasm op))
  | Convert op ->
    data_labelled (Convert (Convertop.of_wasm op))
  | Unary op ->
    data_labelled (Unary (Unop.of_wasm op))
  | RefNull t ->
     data_labelled (RefNull (Ref_type.of_wasm t))
  | RefFunc f ->
     data_labelled (RefFunc f.it)
  | RefIsNull ->
     data_labelled RefIsNull
  | VecLoad _ | VecStore _ | VecLoadLane _ | VecStoreLane _
    | VecConst _ | VecTest _ | VecCompare _ | VecUnary _ | VecBinary _ | VecConvert _
    | VecShift _ | VecBitmask _ | VecTestBits _ | VecUnaryBits _ | VecBinaryBits _ | VecTernaryBits _
    | VecSplat _ | VecExtract _ | VecReplace _ -> Unsupported.vector_type ()
  | TableGet _ | TableSet _ | TableGrow _ | TableFill _ | TableCopy _ | TableInit _ | TableSize _ -> Unsupported.table_instructions ()
  | ElemDrop _ | DataDrop _ -> Unsupported.drop_2_instructions ()

(** Creates a sequence of instructions from their Wasm representation *)
and seq_of_wasm (m : Wasm.Ast.module_) (new_label : unit -> Label.t) (is : Wasm.Ast.instr list) : unit t list =
  List.map is ~f:(of_wasm m new_label)

let rec map_annotation (i : 'a t) ~(f : 'a t -> 'b * 'b) : 'b t =
  match i with
  | Data d -> Data (map_annotation_data d ~f)
  | Control c -> Control (map_annotation_control c ~f)
  | Call c -> Call (map_annotation_call c ~f)
and map_annotation_data (i : (data, 'a) labelled) ~(f : 'a t -> 'b * 'b) : (data, 'b) labelled =
  let (annotation_before, annotation_after) = f (Data i) in
  { i with annotation_before; annotation_after }
and map_annotation_call (i : (call, 'a) labelled) ~(f : 'a t -> 'b * 'b) : (call, 'b) labelled =
  let (annotation_before, annotation_after) = f (Call i) in
  { i with annotation_before; annotation_after }
and map_annotation_control (i : ('a control, 'a) labelled) ~(f : 'a t ->  'b * 'b) : ('b control, 'b) labelled =
  let (annotation_before, annotation_after) = f (Control i) in
  { i with annotation_before; annotation_after;
           instr = match i.instr with
             | Block (bt, arity, instrs) -> Block (bt, arity, List.map instrs ~f:(map_annotation ~f:f))
             | Loop (bt, arity, instrs) -> Loop (bt, arity, List.map instrs ~f:(map_annotation ~f:f))
             | If (bt, arity, then_, else_) -> If (bt, arity,
                                               List.map then_ ~f:(map_annotation ~f:f),
                                               List.map else_ ~f:(map_annotation ~f:f))
             | Br n -> Br n
             | BrIf n -> BrIf n
             | BrTable (l, n) -> BrTable (l, n)
             | Return -> Return
             | Unreachable -> Unreachable
             | Merge -> Merge}

let rec drop_labels (i : 'a t) : 'a t =
  match i with
  | Data d -> Data (drop_labels_data d)
  | Control c -> Control (drop_labels_control c)
  | Call c -> Call (drop_labels_call c)
and drop_labels_data (i : (data, 'a) labelled) : (data, 'a) labelled =
  { i with label = Label.{ section = Dummy; id = 0 }; line_number = 0; }
and drop_labels_call (i : (call, 'a) labelled) : (call, 'a) labelled =
  { i with label = Label.{ section = Dummy; id = 0}; line_number = 0; }
and drop_labels_control (i : ('a control, 'a) labelled) : ('a control, 'a) labelled =
  { i with label = Label.{ section = Dummy; id = 0 };
           line_number = 0;
           instr = match i.instr with
             | Block (bt, arity, instrs) -> Block (bt, arity, List.map instrs ~f:drop_labels)
             | Loop (bt, arity, instrs) -> Loop (bt, arity, List.map instrs ~f:drop_labels)
             | If (bt, arity, then_, else_) -> If (bt, arity,
                                               List.map then_ ~f:drop_labels,
                                                   List.map else_ ~f:drop_labels)
             | Br n -> Br n
             | BrIf n -> BrIf n
             | BrTable (l, n) -> BrTable (l, n)
             | Return -> Return
             | Unreachable -> Unreachable
             | Merge -> Merge }

let clear_annotation (i : 'a t) : unit t =
  map_annotation i ~f:(fun _ -> (), ())
let clear_annotation_data (i : (data, 'a) labelled) : (data, unit) labelled =
  map_annotation_data i ~f:(fun _ -> (), ())
let clear_annotation_control (i : ('a control, 'a) labelled) : (unit control, unit) labelled =
  map_annotation_control i ~f:(fun _ -> (), ())

let annotation_before (i : 'a t) : 'a =
  match i with
  | Data d -> d.annotation_before
  | Control c -> c.annotation_before
  | Call c -> c.annotation_before

let annotation_after (i : 'a t) : 'a =
  match i with
  | Data d -> d.annotation_after
  | Control c -> c.annotation_after
  | Call c -> c.annotation_after

let rec all_labels_no_blocks (i : 'a t) : Label.Set.t =
  match i with
  | Data d -> Label.Set.singleton d.label
  | Call c -> Label.Set.singleton c.label
  | Control c -> begin match c.instr with
      | Block (_, _, instrs)
      | Loop (_, _, instrs) -> List.fold_left instrs ~init:Label.Set.empty ~f:(fun acc i ->
          Label.Set.union acc (all_labels_no_blocks i))
      | If (_, _, instrs1, instrs2) ->
        List.fold_left (instrs1 @ instrs2) ~init:Label.Set.empty ~f:(fun acc i ->
          Label.Set.union acc (all_labels_no_blocks i))
      | _ -> Label.Set.singleton c.label
    end

let rec all_labels_no_merge (i : 'a t) : Label.Set.t =
  match i with
  | Data d -> Label.Set.singleton d.label
  | Call c -> Label.Set.singleton c.label
  | Control c -> begin match c.instr with
      | Block (_, _, instrs)
      | Loop (_, _, instrs) -> List.fold_left instrs ~init:(Label.Set.singleton c.label) ~f:(fun acc i ->
          Label.Set.union acc (all_labels_no_merge i))
      | If (_, _, instrs1, instrs2) ->
        List.fold_left (instrs1 @ instrs2) ~init:(Label.Set.singleton c.label) ~f:(fun acc i ->
          Label.Set.union acc (all_labels_no_merge i))
      | Merge -> Label.Set.empty
      | _ -> Label.Set.singleton c.label
    end


let rec all_labels_no_blocks_no_merge (i : 'a t) : Label.Set.t =
  match i with
  | Data d -> Label.Set.singleton d.label
  | Call c -> Label.Set.singleton c.label
  | Control c -> begin match c.instr with
      | Block (_, _, instrs)
      | Loop (_, _, instrs) -> List.fold_left instrs ~init:Label.Set.empty ~f:(fun acc i ->
          Label.Set.union acc (all_labels_no_blocks_no_merge i))
      | If (_, _, instrs1, instrs2) ->
        List.fold_left (instrs1 @ instrs2) ~init:Label.Set.empty ~f:(fun acc i ->
          Label.Set.union acc (all_labels_no_blocks_no_merge i))
      | Merge -> Label.Set.empty
      | _ -> Label.Set.singleton c.label
    end

let rec all_labels (i : 'a t) : Label.Set.t =
  match i with
  | Data d -> Label.Set.singleton d.label
  | Call c -> Label.Set.singleton c.label
  | Control c -> Label.Set.add (begin match c.instr with
      | Block (_, _, instrs)
      | Loop (_, _, instrs) -> List.fold_left instrs ~init:Label.Set.empty ~f:(fun acc i ->
          Label.Set.union acc (all_labels i))
      | If (_, _, instrs1, instrs2) ->
        List.fold_left (instrs1 @ instrs2) ~init:Label.Set.empty ~f:(fun acc i ->
          Label.Set.union acc (all_labels i))
      | _ -> Label.Set.empty
    end) c.label

let rec contains ~f:(f : 'a t -> bool) (instr : 'a t) : bool =
  if (f instr) then true
  else match instr with
    | Control { instr = Block (_, _, instrs); _ }
    | Control { instr = Loop (_, _, instrs); _ } ->
      Option.is_some (List.find instrs ~f:(contains ~f))
    | Control { instr = If (_, _, instrs1, instrs2); _ } ->
      Option.is_some (List.find (instrs1 @ instrs2) ~f:(contains ~f))
    | _ -> false

let instructions_contained_in (i : 'a t) : 'a t list = match i with
  | Data _ -> []
  | Call _ -> []
  | Control c -> match c.instr with
    | Block (_, _, instrs)
    | Loop (_, _, instrs) -> instrs
    | If (_, _, instrs1, instrs2) -> instrs1 @ instrs2
    | _ -> []

let line_number (i : 'a t) : int = match i with
  | Data d -> d.line_number
  | Call c -> c.line_number
  | Control c -> c.line_number
