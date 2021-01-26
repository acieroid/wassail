open Core_kernel
open Helpers

(** A call graph *)
type t = {
  nodes : Int32Set.t; (** Nodes of the call graphs are function indices *)
  edges : Int32Set.t Int32Map.t; (** Edges are from one node to multiple nodes *)
}
[@@deriving sexp, compare, equal]

let indirect_call_targets (wasm_mod : Wasm_module.t) (_fidx : Int32.t) (_instr : Instr.Label.t) (typ : Int32.t) : Int32.t list =
  let ftype = Wasm_module.get_type wasm_mod typ in
  let table = List.nth_exn wasm_mod.table_insts 0 in
  let funs = List.map (Table_inst.indices table) ~f:(fun idx -> Table_inst.get table idx) in
  let funs_with_matching_type = List.filter_map funs ~f:(function
      | Some fa -> if Stdlib.(ftype = Wasm_module.get_func_type wasm_mod fa) then Some fa else None
      | None -> None) in
  funs_with_matching_type

(* TODO: call with
 Relational.Summary.initial_summaries (Cfg_builder.build_all module_) module_ `Top
   in order to get the right signature *)
let indirect_call_targets_refined summaries (wasm_mod : Wasm_module.t) (fidx : Int32.t) (instr : Instr.Label.t) (typ : Int32.t) : Int32.t list =
  let targets = indirect_call_targets wasm_mod fidx instr typ in
  Spec_inference.propagate_globals := false;
  Spec_inference.propagate_locals := false;
  Spec_inference.use_const := false;
  let cfg = Spec_analysis.analyze_intra1 wasm_mod fidx in
  let slicing_criteria = instr in
  Printf.printf "slicing criteria is: %s\n" (Instr.Label.to_string slicing_criteria);
  let sliced_cfg = Slicing.slice cfg slicing_criteria in
  Printf.printf "Rerunning spec inference\n%!";
  let annotated_sliced_cfg = Spec_inference.Intra.analyze wasm_mod sliced_cfg in
  Relational.Intra.init_summaries summaries;
  let res = Relational.Intra.analyze wasm_mod annotated_sliced_cfg in
  let annotated_instr = Instr.Label.Map.find_exn annotated_sliced_cfg.instructions slicing_criteria in
  let var_of_call_target = match List.hd (Instr.annotation_before annotated_instr).vstack with
    | Some spec -> spec
    | None -> failwith "indirect_call_targets_refined List.hd"
  in
  let relational_instr = Instr.Label.Map.find_exn res.instructions slicing_criteria in
  let domain_of_call_target = Instr.annotation_before relational_instr in
  List.filter targets ~f:(fun target ->
      match Relational.Domain.is_equal domain_of_call_target var_of_call_target target with
      | true, _ -> true (* can be equal *)
      | false, _ ->
        Printf.printf "Filtered one call!\n%!";
        false (* can't be equal *))

(** Builds a call graph for a module *)
let make (wasm_mod : Wasm_module.t) : t =
  let find_targets = indirect_call_targets_refined (Relational.Summary.initial_summaries (Cfg_builder.build_all wasm_mod) wasm_mod `Top) in
  let nodes = Int32Set.of_list (List.init ((List.length wasm_mod.imported_funcs) + (List.length wasm_mod.funcs)) ~f:(fun i -> Int32.of_int_exn i)) in
  let rec collect_calls (f : Int32.t) (instr : 'a Instr.t) (edges : Int32Set.t Int32Map.t) : Int32Set.t Int32Map.t = match instr with
    | Control { instr = Call (_, f'); _ } ->
      Int32Map.update edges f ~f:(function
          | None -> Int32Set.singleton f'
          | Some fs -> Int32Set.add fs f')
    | Control { instr = CallIndirect (_, typ); _ } ->
      List.fold_left (find_targets wasm_mod f (Instr.label instr) typ)
        ~init:edges
        ~f:(fun edges f' ->
            Int32Map.update edges f ~f:(function
                | None -> Int32Set.singleton f'
                | Some fs -> Int32Set.add fs f'))
    | Control { instr = Block (_, _, instrs); _ }
    | Control { instr = Loop (_, _, instrs); _ } ->
      collect_calls_instrs f instrs edges
    | Control { instr = If (_,_, instrs1, instrs2); _ } ->
      collect_calls_instrs f (instrs1 @ instrs2) edges
    | _ -> edges
  and collect_calls_instrs (f : Int32.t) (instrs : 'a Instr.t list) (edges : Int32Set.t Int32Map.t) : Int32Set.t Int32Map.t =
    List.fold_left instrs ~init:edges ~f:(fun edges i -> collect_calls f i edges) in
  let edges = List.fold_left wasm_mod.funcs
      ~init:Int32Map.empty
      ~f:(fun edges f ->
          List.fold_left f.code.body
            ~init:edges
            ~f:(fun edges i -> collect_calls f.idx i edges)) in
  { nodes; edges }

(** Convert call graph to its dot representation *)
let to_dot (cg : t) : string =
  Printf.sprintf "digraph \"Call graph\" {\n%s\n%s\n}"
    (String.concat ~sep:"\n"
       (List.map (Int32Set.to_list cg.nodes) ~f:(fun n ->
            Printf.sprintf "node%s [shape=record, label=\"{%s}\"];" (Int32.to_string n) (Int32.to_string n))))
    (String.concat ~sep:"\n" (List.concat_map (Int32Map.to_alist cg.edges)
                                ~f:(fun (src, dsts) ->
                                    List.map (Int32Set.to_list dsts) ~f:(fun dst ->
                                        Printf.sprintf "node%s -> node%s;\n" (Int32.to_string src) (Int32.to_string dst)))))

(** Remove nodes that are imported functions. Also removes all edges to these nodes *)
let remove_imports (cg : t) (nimports : Int32.t) : t =
  let nodes = Int32Set.filter cg.nodes ~f:(fun n -> Int32.(n >= nimports)) in
  let edges = Int32Map.filter_mapi cg.edges ~f:(fun ~key:src ~data:dsts ->
      if Int32.(src >= nimports) then
        let dsts' = Int32Set.filter dsts ~f:(fun n -> Int32.(n >= nimports)) in
        if Int32Set.is_empty dsts' then
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
let scc_topological (cg : t) : (Int32.t list) list =
  let indices : Int32.t Int32Map.t ref = ref Int32Map.empty in
  let lowlinks : Int32.t Int32Map.t ref = ref Int32Map.empty in
  let on_stack : bool Int32Map.t ref = ref Int32Map.empty in
  let index : Int32.t ref = ref 0l in
  let sccs : (Int32.t list) list ref = ref [] in
  let stack : Int32.t list ref = ref [] in
  let rec strong_connect (v : Int32.t) =
    indices := Int32Map.set !indices ~key:v ~data:!index;
    lowlinks := Int32Map.set !lowlinks ~key:v ~data:!index;
    index := Int32.(!index + 1l);
    stack := v :: !stack;
    on_stack := Int32Map.set !on_stack ~key:v ~data:true;
    Int32Set.iter
      (match Int32Map.find cg.edges v with
       | Some ws -> ws
       | None -> Int32Set.empty)
      ~f:(fun w ->
          match Int32Map.find !indices w with
          | None -> (* w.index is undefined *)
            strong_connect w;
            lowlinks := Int32Map.set !lowlinks
                ~key:v
                ~data:(Int32.min (Int32Map.find_exn !lowlinks v) (Int32Map.find_exn !lowlinks w));
          | Some windex ->
            begin match Int32Map.find !on_stack w with
              | Some true ->
                lowlinks := Int32Map.set !lowlinks
                    ~key:v
                    ~data:(Int32.min (Int32Map.find_exn !lowlinks v) windex)
              | _ -> ()
            end);
    if Int32.(Int32Map.find_exn !lowlinks v = Int32Map.find_exn !indices v) then
      let rec make_scc (scc : Int32.t list) : Int32.t list =
        match !stack with
        | [] -> failwith "scc_topological: should not happen, stack is empty"
        | w :: rest ->
          stack := rest;
          on_stack := Int32Map.set !on_stack ~key:w ~data:false;
          if Int32.(w = v) then
            w :: scc
          else
            make_scc (w :: scc)
      in
      sccs := make_scc [] :: !sccs
  in
  Int32Set.iter cg.nodes ~f:(fun v -> match Int32Map.find !indices v with
      | Some _ -> ()
      | None -> strong_connect v);
  !sccs

(** Provides a analysis schedule from a call graph, as SCCs in their reverse topological order *)
let analysis_schedule (cg : t) (nimports : Int32.t) : (Int32.t list) list =
  List.rev (scc_topological (remove_imports cg nimports))
