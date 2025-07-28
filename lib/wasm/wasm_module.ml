open Core
open Helpers

module T = struct
  (** A WebAssembly module *)
  type func_desc = {
    idx: Int32.t; (* The index of the imported/exported function *)
    type_idx : Int32.t;
    name: string;
    arguments: Type.t list;
    returns: Type.t list;
  }
  [@@deriving sexp, compare, equal]

  type t = {
    start : Int32.t option;
    types : (Type.t list * Type.t list) list; (** The types declared in the module *)
    imported_globals : Global.t list; (** The imported globals *)
    imported_global_types : Type.t list; (** The types of the imported globals *)
    globals : Global.t list; (** The globals *)
    global_types : Type.t list; (** The types for the globals *)
    nglobals : Int32.t; (** The number of globals *)
    nfuncimports : Int32.t; (** The number of functions imported *)
    imports : Import.t list;
    imported_funcs : func_desc list; (** The description of the imported function: their id, name, and type *)
    exports : Export.t list;
    exported_funcs : func_desc list; (** The description of the exported functions: the id, name, and type *)
    funcs : Func_inst.t list; (** The functions defined in the module *)
    memories : Memory.t list; (** The memory types *)
    memory_insts : Memory_inst.t list; (** The memory instances *)
    tables : Table.t list; (** The table types *)
    table_insts : Table_inst.t list; (** The table instances *)
    datas : Data_segment.t list; (** The data segments containing initial data *)
    elems : Elem_segment.t list; (** The elem segments *)
  }
  [@@deriving sexp, compare, equal]
end
include T

(** Get the function instance for the function with index fidx *)
let get_funcinst (m : t) (fidx : Int32.t) : Func_inst.t =
  match List32.nth m.funcs Int32.(fidx-(Int32.of_int_exn (List.length m.imported_funcs))) with
  | Some v -> v
  | None -> failwith (Printf.sprintf "get_funcinst: no funcinst for function %ld" fidx)

let get_n_locals (m : t) (fidx : Int32.t) : int =
  let inst = get_funcinst m fidx in
  List.length inst.code.locals

let get_global_types (m : t) : Type.t list =
  m.imported_global_types @ m.global_types

(** Get the name of a function, if it has one *)
let get_funcname (m : t) (fidx : Int32.t) : string option =
  let funcinst = get_funcinst m fidx in
  funcinst.name

(** Get the index and name of all functions that have names, as a map from function name to its index *)
let get_funcnames (m : t) : int32 StringMap.t =
  let alist = List.filter_map m.funcs ~f:(fun funcinst ->
      Option.map funcinst.name ~f:(fun name -> (name, funcinst.idx))) in
  let imported = List.mapi
      (List.filter m.imports ~f:(function
           | { idesc = Import.FuncImport _; _ } -> true
           | _ -> false))
      ~f:(fun idx f -> (f.item_name, (Int32.of_int_exn idx))) in
  StringMap.of_alist_exn (alist @ imported)

(** Checks if a function is exported or not. An exported function has a name. *)
let is_exported (m : t) (fidx : Int32.t) : bool =
  let funcinst = get_funcinst m fidx in
  Option.is_some funcinst.name

(** Get the memory instance for the memory with index idx *)
let get_meminst (m : t) (idx : int) : Memory_inst.t =
  match List.nth m.memory_insts idx with
  | Some v -> v
  | None -> failwith "get_meminst nth exception"

(** Get the type with index tid *)
let get_type (m : t) (tid : Int32.t) : Type.t list * Type.t list =
  match List.nth m.types (Int32.to_int_exn tid) with
  | Some v -> v
  | None -> failwith "get_type nth exception"

(** Get the type of the function with index fidx *)
let get_func_type (m : t) (fidx : Int32.t) : Type.t list * Type.t list =
  if Int32.(fidx >= m.nfuncimports) then
      match List32.nth m.funcs Int32.(fidx-m.nfuncimports) with
      | Some v -> v.typ
      | None -> failwith "get_func_type nth exception"
  else
    match List32.nth m.imported_funcs fidx with
      | Some desc -> (desc.arguments, desc.returns)
      | None -> failwith "get_func_type nth exception"

(** Remove a function from the module *)
let remove_func (m : t) (fidx : Int32.t) : t =
  { m with funcs = List.filter m.funcs ~f:(fun f -> Int32.(f.idx <> fidx)) }

(** Replace a function in the module *)
let replace_func (m : t) (fidx : Int32.t) (finst : Func_inst.t) : t =
  { m with funcs = List.map m.funcs ~f:(fun f ->
        if Int32.(f.idx = fidx) then
          finst
        else
          f) }

(** Constructs a Wasm_module *)
let of_wasm (m : Wasm.Ast.module_) : t =
  let imported_funcs = List.filter_mapi m.it.imports ~f:(fun idx import -> match import.it.idesc.it with
      | FuncImport v ->
        let idx = Int32.of_int_exn idx in
        let name = Wasm.Ast.string_of_name import.it.item_name in
        let type_idx = v.it in
        let arguments, returns =
          match (List32.nth m.it.types type_idx) with
          | Some {it = Wasm.Types.FuncType (a, b); _} ->
            (List.map a ~f:Type.of_wasm, List.map b ~f:Type.of_wasm)
          | None -> failwith "of_wasm: nth error when looking for imports" in
        Some { idx; type_idx; name; arguments; returns }
      | _ -> None) in
  let nfuncimports = List32.length imported_funcs in
  let imported_globals = List.filter_map m.it.imports ~f:(fun import -> match import.it.idesc.it with
      | GlobalImport t -> Some (Global.of_wasm_import t)
      | _ -> None ) in
  let nimported_globals = List32.length imported_globals in
  let imported_global_types = List.map imported_globals ~f:(fun g -> g.gtype.typ) in
  let globals = List32.mapi m.it.globals ~f:(fun idx g -> Global.of_wasm m Int32.(idx+nimported_globals) g) in
  let nglobals = List32.length m.it.globals in
  let global_types = List.map m.it.globals ~f:(fun g -> match g.it.gtype with
      | Wasm.Types.GlobalType (t, _) -> Type.of_wasm t) in
  let funcs = List32.mapi m.it.funcs ~f:(fun i f -> Func_inst.of_wasm m Int32.(i+nfuncimports) f) in
  let ftype (fidx : Int32.t) : Type.t list * Type.t list = if Int32.(fidx < nfuncimports) then
      match List32.nth imported_funcs fidx with
      | Some desc -> (desc.arguments, desc.returns)
      | None -> failwith "of_wasm: nth error when looking for imported function type"
    else
      match List32.nth funcs Int32.(fidx-nfuncimports) with
      | Some f ->
        assert Int32.(f.idx = fidx);
        f.typ
      | None -> failwith (Printf.sprintf "of_wasm: nth error when looking for function type (function %ld unfound in type list of length %ld)" Int32.(fidx-nfuncimports) (List32.length funcs)) in
  let exports = List.map m.it.exports ~f:Export.of_wasm in
  let exported_funcs = List.filter_map m.it.exports ~f:(fun export -> match export.it.edesc.it with
      | FuncExport v ->
        let idx = v.it in
        let arguments, returns = ftype idx in
        Some {
          idx;
          type_idx = begin match List32.nth funcs Int32.(idx-nfuncimports) with
            | Some f -> f.type_idx
            | None -> failwith (Printf.sprintf "of_wasm: nth error when looking for function type (function %ld unfound in type list of length %ld)" Int32.(idx-nfuncimports) (List32.length funcs))
          end;
          name = Wasm.Ast.string_of_name export.it.name;
          arguments;
          returns;
        }
      | _ -> None) in
  let memories = List.map m.it.memories ~f:Memory.of_wasm in
  let memory_insts = List.filter_map m.it.imports ~f:(fun import -> match import.it.idesc.it with
      | MemoryImport m -> Some (Memory_inst.of_wasm_type m)
      | _ -> None) @ (List.map m.it.memories ~f:Memory_inst.of_wasm) in
  let datas = List32.mapi m.it.datas ~f:(Data_segment.of_wasm m) in
  let imports = List.map m.it.imports ~f:Import.of_wasm in
  let tables = List.map m.it.tables ~f:Table.of_wasm in
  let table_insts = List.map m.it.tables ~f:(fun t ->
        Table_inst.init
          (Table.of_wasm t)
          (List32.mapi m.it.elems ~f:(Elem_segment.of_wasm m))) in
  let start = Option.map m.it.start ~f:(fun v -> v.it.sfunc.it) in
  let elems = List32.mapi m.it.elems ~f:(Elem_segment.of_wasm m) in
  ({
    start;
    imports; nfuncimports; imported_funcs;
    exports; exported_funcs;
    funcs;
    datas; elems;
    imported_globals; imported_global_types; global_types; nglobals; globals;
    memories; memory_insts;
    tables; table_insts;
    types = List.map m.it.types ~f:(fun t -> match t.it with
        | Wasm.Types.FuncType (a, b) -> (List.map a ~f:Type.of_wasm,
                                         List.map b ~f:Type.of_wasm))
  })

let of_file (file : string) : t =
  apply_to_file file of_wasm

let of_string (string : string) : t =
  apply_to_string string of_wasm

let string_to_wasm_string (s : string) : string =
  let string_of_name n =
    let b = Buffer.create 16 in
    let escape uc =
       if uc < 0x20 || uc >= 0x7f then
         Buffer.add_string b (Printf.sprintf "\\%02x" uc)
       else begin
        let c = Char.of_int_exn uc in
        if Char.(c = '\"') || Char.(c = '\\') then Buffer.add_char b '\\';
        Buffer.add_char b c
      end
    in
    List.iter ~f:escape n;
    Buffer.contents b in
  string_of_name (List.map (String.to_list s) ~f:Char.to_int)

let to_string (m : t) : string =
  let buf = Buffer.create 8192 in
  let put (s : string) = Buffer.add_string buf s in
  let type_ (i : int) (t : Type.t list * Type.t list) =
    put "  (type (;";
    put (string_of_int i);
    put ";) (func";
    if not (List.is_empty (fst t)) then begin
      put (Printf.sprintf " (param %s)"
             (String.concat ~sep:" " (List.map (fst t) ~f:Type.to_string)))
    end;
    if not (List.is_empty (snd t)) then begin
      put (Printf.sprintf " (result %s)"
             (String.concat ~sep:" " (List.map (snd t) ~f:Type.to_string)))
    end;
    put "))\n" in
  let types () = List.iteri m.types ~f:type_ in
  let limits (l : Limits.t) = match l with
    | low, None -> put (Printf.sprintf "%ld" low)
    | low, Some high -> put (Printf.sprintf "%ld %ld" low high) in
  let last_memory_import = ref 0 in
  let next_memory_import () : int =
    last_memory_import := !last_memory_import + 1;
    !last_memory_import in
  let last_global_import = ref 0 in
  let next_global_import () : int =
    last_global_import := !last_global_import + 1;
    !last_global_import in
  let import (i : int) (import : Import.t) =
    put (Printf.sprintf "  (import \"%s\" \"%s\" " import.module_name import.item_name);
    begin match import.idesc with
    | FuncImport tidx ->
      put (Printf.sprintf "(func (;%d;) (type %ld))"
             i tidx)
    | TableImport _ -> ()  (* failwith "Unsupported: table import" (* should be easy to add, just need an example *) *)
    | MemoryImport l ->
      put (Printf.sprintf "(memory (;%d;) " (next_memory_import ()));
      limits l;
      put ")"
    | GlobalImport { typ; _ } ->
      put (Printf.sprintf "(global (;%d;) %s)" (next_global_import ()) (Type.to_string typ));
    end;
    put ")\n" in
  let imports () = List.iteri m.imports ~f:import in
  let func (i : Int32.t) (f : Func_inst.t) =
    put (Printf.sprintf "  (func (;%ld;) (type %ld)" i f.type_idx);
    if not (List.is_empty (fst f.typ)) then begin
      put (Printf.sprintf " (param %s)"
             (String.concat ~sep:" " (List.map (fst f.typ) ~f:Type.to_string)))
    end;
    if not (List.is_empty (snd f.typ)) then begin
      put (Printf.sprintf " (result %s)"
             (String.concat ~sep:" " (List.map (snd f.typ) ~f:Type.to_string)))
    end;
    put "\n";
    if not (List.is_empty f.code.locals) then begin
      put (Printf.sprintf "    (local %s)\n" (String.concat ~sep:" " (List.map f.code.locals ~f:Type.to_string)))
    end;
    put (Instr.list_to_string f.code.body ?indent:(Some 4) ?sep:(Some "\n") (fun () -> ""));
    put ")\n" in
  let funcs () = List.iteri m.funcs ~f:(fun i f -> func Int32.(m.nfuncimports + (of_int_exn i)) f) in
  let table (i : int) (t : Table.t) =
    put (Printf.sprintf "  (table (;%d;) " i);
    limits t.ttype;
    put " funcref)\n"
  in
  let tables () = List.iteri m.tables ~f:table in
  let memory (i : int) (memory : Memory.t) =
    put (Printf.sprintf "  (memory (;%d;) " i);
    limits memory.mtype;
    put ")\n" in
  let memories () = List.iteri m.memories ~f:memory in
  let export (export : Export.t) =
    put (Printf.sprintf "  (export \"%s\" " export.name);
    begin match export.edesc with
    | FuncExport n -> put (Printf.sprintf "(func %ld)" n)
    | TableExport n -> put (Printf.sprintf "(table %ld)" n)
    | MemoryExport n -> put (Printf.sprintf "(memory %ld)" n)
    | GlobalExport n -> put (Printf.sprintf "(global %ld)" n)
    end;
    put ")\n" in
  let exports () = List.iter m.exports ~f:export in
  let elem (elem : Elem_segment.t) =
    put (Printf.sprintf "(elem (;%ld;) " elem.idx);
    put (Printf.sprintf "(%s)" (Instr.list_to_string (Segment_mode.offset elem.emode) (fun () -> "")));
    put (Printf.sprintf " func %s)\n" (String.concat ~sep:" " (List.map ~f:(fun l -> Instr.list_to_string l (fun () -> "")) elem.einit))) in
  let elems () = List.iter m.elems ~f:elem in
  let data (data : Data_segment.t) =
    put (Printf.sprintf "  (data (;%ld;) " data.idx);
    put (Printf.sprintf "(offset %s)" (Instr.list_to_string (Segment_mode.offset data.dmode) (fun () -> "")));
    put "\"";
    put (string_to_wasm_string data.dinit); (* XXX: make sure to escape what is needed *)
    put "\")\n" in
  let datas () = List.iter m.datas ~f:data in
  let global (i : int) (g : Global.t) =
    put (Printf.sprintf "  (global (;%d;) %s" i
           (match g.gtype.mutability with
            | Mutable -> Printf.sprintf "(mut %s)" (Type.to_string g.gtype.typ)
            | Immutable -> Type.to_string g.gtype.typ));
    put (Printf.sprintf " (%s)" (Instr.list_to_string g.value (fun () -> "")));
    put ")\n" in
  let globals () = List.iteri m.globals ~f:global in

  put "(module\n";
  types ();
  imports ();
  funcs ();
  tables ();
  memories ();
  globals ();
  exports ();
  elems ();
  datas ();
  put ")";
  Buffer.contents buf

module Test = struct
  let%test_unit "construct Wasm_module from .wat files without erroring" =
    List.iter [
      "../../../benchmarks/benchmarksgame/binarytrees.wat";
      "../../../benchmarks/benchmarksgame/fankuchredux.wat";
      "../../../benchmarks/benchmarksgame/fasta.wat";
      "../../../benchmarks/benchmarksgame/k-nucleotide.wat";
      "../../../benchmarks/benchmarksgame/mandelbrot.wat";
      "../../../benchmarks/benchmarksgame/nbody.wat";
      "../../../benchmarks/benchmarksgame/reverse-complement.wat";
      "../../../benchmarks/benchmarksgame/spectral-norm.wat";
      "../../../benchmarks/polybench-clang/2mm.wat";
      "../../../benchmarks/polybench-clang/3mm.wat";
      "../../../benchmarks/polybench-clang/adi.wat";
      "../../../benchmarks/polybench-clang/atax.wat";
      "../../../benchmarks/polybench-clang/bicg.wat";
      "../../../benchmarks/polybench-clang/cholesky.wat";
      "../../../benchmarks/polybench-clang/correlation.wat";
      "../../../benchmarks/polybench-clang/covariance.wat";
      "../../../benchmarks/polybench-clang/deriche.wat";
      "../../../benchmarks/polybench-clang/doitgen.wat";
      "../../../benchmarks/polybench-clang/durbin.wat";
      "../../../benchmarks/polybench-clang/fdtd-2d.wat";
      "../../../benchmarks/polybench-clang/floyd-warshall.wat";
      "../../../benchmarks/polybench-clang/gemm.wat";
      "../../../benchmarks/polybench-clang/gemver.wat";
      "../../../benchmarks/polybench-clang/gesummv.wat";
      "../../../benchmarks/polybench-clang/gramschmidt.wat";
      "../../../benchmarks/polybench-clang/heat-3d.wat";
      "../../../benchmarks/polybench-clang/jacobi-1d.wat";
      "../../../benchmarks/polybench-clang/jacobi-2d.wat";
      "../../../benchmarks/polybench-clang/ludcmp.wat";
      "../../../benchmarks/polybench-clang/lu.wat";
      "../../../benchmarks/polybench-clang/mvt.wat";
      "../../../benchmarks/polybench-clang/nussinov.wat";
      "../../../benchmarks/polybench-clang/seidel-2d.wat";
      "../../../benchmarks/polybench-clang/symm.wat";
      "../../../benchmarks/polybench-clang/syr2k.wat";
      "../../../benchmarks/polybench-clang/syrk.wat";
      "../../../benchmarks/polybench-clang/trisolv.wat";
      "../../../benchmarks/polybench-clang/trmm.wat";
      "../../../test/element-section-func.wat";
    ] ~f:(fun program ->
        try
          let _ : t = of_file program in
          ()
        with e -> failwith (Printf.sprintf "Cannot parse %s: %s" program (Exn.to_string e)))

  let%test "get_func_type with indirect call to imported function" =
    let m: t = of_file "../../../test/call_indirect-with_imported_element.wat" in
    let (t1, t2) = get_func_type m 0l in
    List.is_empty t1 && (List.length t2) = 1 && Type.equal (List.hd_exn t2) Type.I32
end
