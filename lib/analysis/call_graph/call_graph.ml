open Core
open Helpers

(** An edge of the call graph *)
module Edge = struct
  type t = {
    target: int32; (* its target *)
    direct: bool; (* whether the call is direct or indirect *)
  }
  [@@deriving sexp, compare, equal]
end

module EdgeSet = struct
  include Set
  include Set.Make(Edge)
  let to_nodes (edges : t) : Int32Set.t =
    edges
    |> to_list
    |> List.map ~f:(function { target; _ } -> target)
    |> Int32Set.of_list
end

(** A call graph *)
type t = {
  nodes : Int32Set.t; (** Nodes of the call graphs are function indices *)
  edges : EdgeSet.t Int32Map.t; (** Edges between nodes *)
}
[@@deriving sexp, compare, equal]

(** Find an edge *)
let find_edge (cg : t) (node : int32) (target : int32) : Edge.t option =
  match Int32Map.find cg.edges node with
  | Some edges -> EdgeSet.find edges ~f:(fun edge -> Int32.(edge.target = target))
  | None -> None

let find_edge_exn (cg : t) (node : int32) (target : int32) : Edge.t =
  match find_edge cg node target with
  | Some edge -> edge
  | None -> failwith "find_edge_exn did not find an edge"

let indirect_call_targets (wasm_mod : Wasm_module.t) (typ : Int32.t) : Int32.t list =
  let ftype = Wasm_module.get_type wasm_mod typ in
  match List.hd wasm_mod.table_insts with
  | Some table ->
    (* TODO: this may be unsound, as the table may be modified at runtime by the host environment.
       If this is the case, we should only rely on the second case below *)
    let funs = List.map (Table_inst.indices table) ~f:(fun idx -> Table_inst.get table idx) in
    let funs_with_matching_type = List.filter_map funs ~f:(function
        | Some fa -> if Stdlib.(ftype = Wasm_module.get_func_type wasm_mod fa) then Some fa else None
        | None -> None) in
    funs_with_matching_type
  | None ->
    (* All functions with the proper type can be called. *)
    let funs = List.map wasm_mod.imported_funcs ~f:(fun (idx, _, _) -> idx) @ (List.map wasm_mod.funcs ~f:(fun f -> f.idx)) in
    let ftype = Wasm_module.get_type wasm_mod typ in
    (* These are all the functions with a valid type *)
   List.filter funs ~f:(fun idx -> Stdlib.(ftype = (Wasm_module.get_func_type wasm_mod( idx))))

(** Builds a call graph for a module *)
let make (wasm_mod : Wasm_module.t) : t =
  let find_targets = indirect_call_targets in
  (* Nodes of the call graph, i.e., all functions (imported and defined functions) *)
  let nodes = Int32Set.of_list (List.init ((List.length wasm_mod.imported_funcs) + (List.length wasm_mod.funcs)) ~f:(fun i -> Int32.of_int_exn i)) in
  let rec collect_calls (f : Int32.t) (instr : 'a Instr.t) (edges : EdgeSet.t Int32Map.t) : EdgeSet.t Int32Map.t = match instr with
    | Call { instr = CallDirect (_, _, f'); _ } ->
      let edge : Edge.t = { target = f'; direct = true } in
      Int32Map.update edges f ~f:(function
          | None -> EdgeSet.singleton edge
          | Some fs -> EdgeSet.add fs edge)
    | Call { instr = CallIndirect (_, _, _, typ); _ } ->
      List.fold_left (find_targets wasm_mod typ)
        ~init:edges
        ~f:(fun edges f' ->
            let edge : Edge.t = { target = f'; direct = false } in
            Int32Map.update edges f ~f:(function
                | None -> EdgeSet.singleton edge
                | Some fs -> EdgeSet.add fs edge))
    | Control { instr = Block (_, _, instrs); _ }
    | Control { instr = Loop (_, _, instrs); _ } ->
      collect_calls_instrs f instrs edges
    | Control { instr = If (_,_, instrs1, instrs2); _ } ->
      collect_calls_instrs f (instrs1 @ instrs2) edges
    | _ -> edges
  and collect_calls_instrs (f : Int32.t) (instrs : 'a Instr.t list) (edges : EdgeSet.t Int32Map.t) : EdgeSet.t Int32Map.t =
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
                                    List.map (EdgeSet.to_list dsts) ~f:(fun dst ->
                                        let extra = if dst.direct then "" else "[style=dashed]" in
                                        Printf.sprintf "node%s -> node%s %s;\n" (Int32.to_string src) (Int32.to_string dst.target) extra))))

(** Convert call graph to an adjacency list to be printed or saved to a file *)
let to_adjlist (cg : t) : string =
  let buf = Buffer.create 16 in
  Int32Map.iteri cg.edges ~f:(fun ~key:node ~data:edges ->
      EdgeSet.iter edges ~f:(function { target ; direct } ->
          let direct_str = if direct then "d" else "i" in
          Buffer.add_string buf (Printf.sprintf "%s %s %s\n"  (Int32.to_string node) (Int32.to_string target) direct_str)));
  Buffer.contents buf

(** Keeps only nodes reachable from the given root set *)
let keep_reachable (cg : t) (from : Int32Set.t) : t =
  let q = Queue.create () in
  Int32Set.iter from ~f:(Queue.enqueue q);
  let rec loop (new_cg : t) =
     match Queue.dequeue q with
    | None -> new_cg
    | Some node ->
      if Int32Set.mem new_cg.nodes node then
        (* Skip this one, we already visited it *)
        loop new_cg
      else
        (* Get all the directly reachable nodes *)
      let directly_reachable_nodes = match Int32Map.find cg.edges node with
        | None -> Int32Set.empty
        | Some edges -> EdgeSet.to_nodes edges in
      (* Enqueue all of them *)
      Int32Set.iter directly_reachable_nodes ~f:(Queue.enqueue q);
      (* Compute the new edges *)
      let edges_to_add : EdgeSet.t =
        directly_reachable_nodes
        |> Int32Set.to_list
        |> List.map ~f:(fun target -> find_edge_exn cg node target)
        |> EdgeSet.of_list in
      let old_edges : EdgeSet.t = match Int32Map.find cg.edges node with
        | Some edges -> edges
        | None -> EdgeSet.empty in
      (* and recurse with the nodes added to the CG *)
      loop {
        nodes = Int32Set.add new_cg.nodes node;
        edges = Int32Map.add_exn new_cg.edges ~key:node ~data:(EdgeSet.union old_edges edges_to_add)
      } in
  loop { nodes = Int32Set.empty; edges = Int32Map.empty }

(** Remove nodes that are imported functions. Also removes all edges to these nodes *)
let remove_imports (cg : t) (nimports : Int32.t) : t =
  let nodes = Int32Set.filter cg.nodes ~f:(fun n -> Int32.(n >= nimports)) in
  let edges = Int32Map.filter_mapi cg.edges ~f:(fun ~key:src ~data:dsts ->
      if Int32.(src >= nimports) then
        let dsts' = EdgeSet.filter dsts ~f:(fun edge -> Int32.(edge.target >= nimports)) in
        if EdgeSet.is_empty dsts' then
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
    EdgeSet.iter
      (match Int32Map.find cg.edges v with
       | Some ws -> ws
       | None -> EdgeSet.empty)
      ~f:(fun w ->
          match Int32Map.find !indices w.target with
          | None -> (* w.index is undefined *)
            strong_connect w.target;
            lowlinks := Int32Map.set !lowlinks
                ~key:v
                ~data:(Int32.min (Int32Map.find_exn !lowlinks v) (Int32Map.find_exn !lowlinks w.target));
          | Some windex ->
            begin match Int32Map.find !on_stack w.target with
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
