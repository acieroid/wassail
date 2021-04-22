open Core_kernel
open Helpers

module T = struct
  (** A WebAssembly module *)
  type t = {
    start : Int32.t option;
    types : (Type.t list * Type.t list) list; (** The types declared in the module *)
    globals : Global.t list; (** The globals *)
    global_types : Type.t list; (** The types for the globals *)
    nglobals : Int32.t; (** The number of globals *)
    nfuncimports : Int32.t; (** The number of functions imported *)
    imports : Import.t list;
    imported_funcs : (Int32.t * string * (Type.t list * Type.t list)) list; (** The description of the imported function: their id, name, and type *)
    exports : Export.t list;
    exported_funcs : (Int32.t * string * (Type.t list * Type.t list)) list; (** The description of the exported functions: the id, name, and type *)
    funcs : Func_inst.t list; (** The functions defined in the module *)
    memories : Memory.t list; (** The memory types *)
    memory_insts : Memory_inst.t list; (** The memory instances *)
    tables : Table.t list; (** The table types *)
    table_insts : Table_inst.t list; (** The table instances *)
    data : Segment.DataSegment.t list; (** The data segment containing initial data *)
    elems : Segment.ElemSegment.t list; (** The elem segment *)
  }
  [@@deriving sexp, compare, equal]
end
include T

(** Get the function instance for the function with index fidx *)
let get_funcinst (m : t) (fidx : Int32.t) : Func_inst.t =
  match List32.nth m.funcs Int32.(fidx-(Int32.of_int_exn (List.length m.imported_funcs))) with
  | Some v -> v
  | None -> failwith "get_funcinst nth exception"

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
  match List32.nth m.funcs Int32.(fidx-m.nfuncimports) with
  | Some v -> v.typ
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
        Some (Int32.of_int_exn idx, Wasm.Ast.string_of_name import.it.item_name,
              let type_idx = v.it in
              match (List32.nth m.it.types type_idx) with
              | Some {it = Wasm.Types.FuncType (a, b); _} ->
                (List.map a ~f:Type.of_wasm, List.map b ~f:Type.of_wasm)
              | None -> failwith "of_wasm: nth error when looking for imports")
      | _ -> None) in
  let nfuncimports = Wasm.Lib.List32.length imported_funcs in
  let globals = List32.mapi m.it.globals ~f:(Global.of_wasm m) in
  let nglobals = Wasm.Lib.List32.length m.it.globals in
  let global_types = List.map m.it.globals ~f:(fun g -> match g.it.gtype with
      | Wasm.Types.GlobalType (t, _) -> Type.of_wasm t) in
  let funcs = List32.mapi m.it.funcs ~f:(fun i f -> Func_inst.of_wasm m Int32.(i+nfuncimports) f) in
  let ftype (fidx : Int32.t) : Type.t list * Type.t list = if Int32.(fidx < nfuncimports) then
      match List32.nth imported_funcs fidx with
      | Some (_, _, typ) -> typ
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
        Some (idx, (Wasm.Ast.string_of_name export.it.name), ftype idx)
      | _ -> None) in
  let memories = List.map m.it.memories ~f:Memory.of_wasm in
  let memory_insts = List.filter_map m.it.imports ~f:(fun import -> match import.it.idesc.it with
      | MemoryImport m -> Some (Memory_inst.of_wasm_type m)
      | _ -> None) @ (List.map m.it.memories ~f:Memory_inst.of_wasm) in
  let data = List32.mapi m.it.data ~f:(Segment.DataSegment.of_wasm m) in
  let imports = List.map m.it.imports ~f:Import.of_wasm in
  let tables = List.map m.it.tables ~f:Table.of_wasm in
  let table_insts = List.map m.it.tables ~f:(fun t ->
        Table_inst.init
          (Table.of_wasm t)
          (List32.mapi m.it.elems ~f:(Elem.of_wasm m))) in
  let start = Option.map m.it.start ~f:(fun v -> v.it) in
  let elems = List32.mapi m.it.elems ~f:(Segment.ElemSegment.of_wasm m) in
  ({
    start;
    imports; nfuncimports; imported_funcs;
    exports; exported_funcs;
    funcs;
    data; elems;
    global_types; nglobals; globals;
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
    put "(type (;";
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
  let import (i : int) (import : Import.t) =
    put (Printf.sprintf "(import \"%s\" \"%s\" " import.module_name import.item_name);
    begin match import.idesc with
    | FuncImport tidx ->
      put (Printf.sprintf "(func (;%d;) (type %ld))"
             i tidx)
    | _ -> failwith "Non-function import not yet supported (this is easy to add)"
    end;
    put ")\n" in
  let imports () = List.iteri m.imports ~f:import in
  let func (i : Int32.t) (f : Func_inst.t) =
    put (Printf.sprintf "(func (;%ld;) (type %ld)\n" i f.type_idx);
    if not (List.is_empty f.code.locals) then begin
      put (Printf.sprintf "(local %s)\n" (String.concat ~sep:" " (List.map f.code.locals ~f:Type.to_string)))
    end;
    put (Instr.list_to_string f.code.body ?indent:(Some 2) ?sep:(Some "\n") (fun () -> ""));
    put "\n)\n" in
  let funcs () = List.iteri m.funcs ~f:(fun i f -> func Int32.(m.nfuncimports + (of_int_exn i)) f) in
  let limits (l : Limits.t) = match l with
    | low, None -> put (Printf.sprintf "%ld" low)
    | low, Some high -> put (Printf.sprintf "%ld %ld" low high) in
  let table (i : int) (t : Table.t) =
    put (Printf.sprintf "(table (;%d;) " i);
    limits t.ttype;
    put " funcref)\n"
  in
  let tables () = List.iteri m.tables ~f:table in
  let memory (i : int) (memory : Memory.t) =
    put (Printf.sprintf "(memory (;%d;) " i);
    limits memory.mtype;
    put ")\n" in
  let memories () = List.iteri m.memories ~f:memory in
  let export (export : Export.t) =
    put (Printf.sprintf "(export \"%s\" " export.name);
    begin match export.edesc with
    | FuncExport n -> put (Printf.sprintf "(func %ld)" n)
    | TableExport n -> put (Printf.sprintf "(table %ld)" n)
    | MemoryExport n -> put (Printf.sprintf "(memory %ld)" n)
    | GlobalExport n -> put (Printf.sprintf "(global %ld)" n)
    end;
    put ")\n" in
  let exports () = List.iter m.exports ~f:export in
  let elem (elem : Segment.ElemSegment.t) =
    put (Printf.sprintf "(elem (;%ld;) " elem.index);
    put (Printf.sprintf "(offset %s)" (Instr.list_to_string elem.offset (fun () -> "")));
    (* TODO: this is not the general case, check that this is enough *)
    put (Printf.sprintf " func %s)\n" (String.concat ~sep:" " (List.map elem.init ~f:Int32.to_string))) in
  let elems () = List.iter m.elems ~f:elem in
  let data (data : Segment.DataSegment.t) =
    put (Printf.sprintf "(data (;%ld;) " data.index);
    put (Printf.sprintf "(offset %s)" (Instr.list_to_string data.offset (fun () -> "")));
    put "\"";
    put (string_to_wasm_string data.init); (* TODO: make sure to escape what is needed *)
    put "\")\n" in
  let datas () = List.iter m.data ~f:data in
  let global (i : int) (g : Global.t) =
    put (Printf.sprintf "(global (;%d;) %s" i
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
      (*    "../../../benchmarks/benchmarksgame/binarytrees.wat"; *)
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
    ] ~f:(fun program ->
        try
          let _ : t = of_file program in
          ()
        with e -> failwith (Printf.sprintf "Cannot parse %s: %s" program (Exn.to_string e)))
end
