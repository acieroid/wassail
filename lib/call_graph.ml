open Core_kernel
open Helpers

(** A call graph *)
type t = {
  nodes : IntSet.t; (** Nodes or the  call graphs are function indices *)
  edges : IntSet.t IntMap.t; (** Edges are from one node to multiple nodes *)
}
[@@deriving sexp, compare, equal]

(** Builds a call graph for a module *)
let make (wasm_mod : Wasm_module.t) : t =
  let nodes = IntSet.of_list (List.init ((List.length wasm_mod.imported_funcs) + (List.length wasm_mod.funcs)) ~f:(fun i -> i)) in
  let rec collect_calls (f : int) (instr : 'a Instr.t) (edges : IntSet.t IntMap.t) : IntSet.t IntMap.t = match instr with
    | Control { instr = Call (_, f'); _ } ->
      IntMap.update edges f ~f:(function
          | None -> IntSet.singleton f'
          | Some fs -> IntSet.add fs f')
    | Control { instr = CallIndirect (_, typ); _ } ->
      let ftype = Wasm_module.get_type wasm_mod typ in
      let table = List.nth_exn wasm_mod.tables 0 in
      let funs = List.map (Table_inst.indices table) ~f:(fun idx -> Table_inst.get table idx) in
      let funs_with_matching_type = List.filter_map funs ~f:(function
          | Some fa -> if Stdlib.(ftype = Wasm_module.get_func_type wasm_mod fa) then Some fa else None
          | None -> None) in
      List.fold_left funs_with_matching_type
        ~init:edges
        ~f:(fun edges f' ->
            IntMap.update edges f ~f:(function
                | None -> IntSet.singleton f'
                | Some fs -> IntSet.add fs f'))
    | Control { instr = Block (_, instrs); _ }
    | Control { instr = Loop (_, instrs); _ } ->
      collect_calls_instrs f instrs edges
    | Control { instr = If (_, instrs1, instrs2); _ } ->
      collect_calls_instrs f (instrs1 @ instrs2) edges
    | _ -> edges
  and collect_calls_instrs (f : int) (instrs : 'a Instr.t list) (edges : IntSet.t IntMap.t) : IntSet.t IntMap.t =
    List.fold_left instrs ~init:edges ~f:(fun edges i -> collect_calls f i edges) in
  let edges = List.fold_left wasm_mod.funcs
      ~init:IntMap.empty
      ~f:(fun edges f ->
          List.fold_left f.code.body
            ~init:edges
            ~f:(fun edges i -> collect_calls f.idx i edges)) in
  { nodes; edges }

(** Convert call graph to its dot representation *)
let to_dot (cg : t) : string =
  Printf.sprintf "digraph \"Call graph\" {\n%s\n%s\n}"
    (String.concat ~sep:"\n"
       (List.map (IntSet.to_list cg.nodes) ~f:(fun n ->
            Printf.sprintf "node%d [shape=record, label=\"{%d}\"];" n n)))
    (String.concat ~sep:"\n" (List.concat_map (IntMap.to_alist cg.edges)
                                ~f:(fun (src, dsts) ->
                                    List.map (IntSet.to_list dsts) ~f:(fun dst ->
                                        Printf.sprintf "node%d -> node%d;\n" src dst))))

(** Remove nodes that are imported functions. Also removes all edges to these nodes *)
let remove_imports (cg : t) (nimports : int) : t =
  let nodes = IntSet.filter cg.nodes ~f:(fun n -> n >= nimports) in
  let edges = IntMap.filter_mapi cg.edges ~f:(fun ~key:src ~data:dsts ->
      if src >= nimports then
        let dsts' = IntSet.filter dsts ~f:(fun n -> n >= nimports) in
        if IntSet.is_empty dsts' then
          None
        else
          Some dsts'
      else
        (* drop all edges from imports (but there shouldn't be any in practice *)
        None
    ) in
  { nodes; edges }

(** Computes the SCC in topological order using Tarjan's algorithm.
    The implementation of Tarjan's algorithm follows the pseudo-code implementation of Wikipedia: https://en.wikipedia.org/wiki/Tarjan%27s_strongly_connected_components_algorithm *)
let scc_topological (cg : t) : (int list) list =
  let indices : int IntMap.t ref = ref IntMap.empty in
  let lowlinks : int IntMap.t ref = ref IntMap.empty in
  let on_stack : bool IntMap.t ref = ref IntMap.empty in
  let index = ref 0 in
  let sccs : (int list) list ref = ref [] in
  let stack = ref [] in
  let rec strong_connect (v : int) =
    indices := IntMap.set !indices ~key:v ~data:!index;
    lowlinks := IntMap.set !lowlinks ~key:v ~data:!index;
    index := !index + 1;
    stack := v :: !stack;
    on_stack := IntMap.set !on_stack ~key:v ~data:true;
    IntSet.iter
      (match IntMap.find cg.edges v with
       | Some ws -> ws
       | None -> IntSet.empty)
      ~f:(fun w ->
          match IntMap.find !indices w with
          | None -> (* w.index is undefined *)
            strong_connect w;
            lowlinks := IntMap.set !lowlinks
                ~key:v
                ~data:(min (IntMap.find_exn !lowlinks v) (IntMap.find_exn !lowlinks w));
          | Some windex ->
            begin match IntMap.find !on_stack w with
              | Some true ->
                lowlinks := IntMap.set !lowlinks
                    ~key:v
                    ~data:(min (IntMap.find_exn !lowlinks v) windex)
              | _ -> ()
            end);
    if IntMap.find_exn !lowlinks v = IntMap.find_exn !indices v then
      let rec make_scc (scc : int list) : int list =
        match !stack with
        | [] -> failwith "scc_topological: should not happen, stack is empty"
        | w :: rest ->
          stack := rest;
          on_stack := IntMap.set !on_stack ~key:w ~data:false;
          if w = v then
            w :: scc
          else
            make_scc (w :: scc)
      in
      sccs := make_scc [] :: !sccs
  in
  IntSet.iter cg.nodes ~f:(fun v -> match IntMap.find !indices v with
      | Some _ -> ()
      | None -> strong_connect v);
  !sccs

(** Provides a analysis schedule from a call graph, as SCCs in their reverse topological order *)
let analysis_schedule (cg : t) (nimports : int) : (int list) list =
  List.rev (scc_topological (remove_imports cg nimports))
