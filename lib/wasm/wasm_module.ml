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
    nimports : Int32.t; (** The number of function imported *)
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
  List32.nth_exn m.funcs Int32.(fidx-(Int32.of_int_exn (List.length m.imported_funcs)))

(** Get the memory instance for the memory with index idx *)
let get_meminst (m : t) (idx : int) : Memory_inst.t =
  List.nth_exn m.memory_insts idx

(** Get the type with index tid *)
let get_type (m : t) (tid : Int32.t) : Type.t list * Type.t list =
  List.nth_exn m.types (Int32.to_int_exn tid)

(** Get the type of the function with index fidx *)
let get_func_type (m : t) (fidx : Int32.t) : Type.t list * Type.t list =
  (Wasm.Lib.List32.nth m.funcs Int32.(fidx-m.nimports)).typ

(** Constructs a Wasm_module *)
let of_wasm (m : Wasm.Ast.module_) : t =
  let minst = Module_inst.of_wasm m in
  let imported_funcs = List.filter_mapi m.it.imports ~f:(fun idx import -> match import.it.idesc.it with
      | FuncImport v ->
        Some (Int32.of_int_exn idx, Wasm.Ast.string_of_name import.it.item_name,
              let type_idx = v.it in
              match (Wasm.Lib.List32.nth m.it.types type_idx) with
              | {it = Wasm.Types.FuncType (a, b); _} ->
                (List.map a ~f:Type.of_wasm, List.map b ~f:Type.of_wasm))
      | _ -> None) in
  let nimports = Wasm.Lib.List32.length imported_funcs in
  let globals = List32.mapi m.it.globals ~f:(Global.of_wasm m) in
  let nglobals = Wasm.Lib.List32.length m.it.globals in
  let global_types = List.map m.it.globals ~f:(fun g -> match g.it.gtype with
      | Wasm.Types.GlobalType (t, _) -> Type.of_wasm t) in
  let funcs = List32.mapi m.it.funcs ~f:(fun i f -> Func_inst.of_wasm m minst Int32.(i+nimports) f) in
  let ftype (fidx : Int32.t) : Type.t list * Type.t list = if Int32.(fidx < nimports) then
      match Wasm.Lib.List32.nth imported_funcs fidx with
      | (_, _, typ) -> typ
    else
      let f =  Wasm.Lib.List32.nth funcs Int32.(fidx-nimports) in
      assert Int32.(f.idx = fidx);
      f.typ in
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
    imports; nimports; imported_funcs;
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
