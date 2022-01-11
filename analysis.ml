open Core
open Wassail
open Utils

let mk_intra (desc : string) (analysis : Wasm_module.t -> Int32.t list -> 'a Int32Map.t) (print : Int32.t -> 'a -> unit) =
  Command.basic
    ~summary:desc
    Command.Let_syntax.(
    let%map_open filename = anon ("file" %: string)
    and funs = anon (sequence ("funs" %: int32)) in
    fun () ->
      let results = analysis (Wasm_module.of_file filename) funs in
      Int32Map.iteri results ~f:(fun ~key:id ~data:summary -> print id summary))


let spec_inference =
  mk_intra "Annotate the CFG with the inferred variables"
    (Analysis_helpers.mk_intra (fun _ _ -> Int32Map.empty) (fun _ _ annotated_cfg -> annotated_cfg))
    (fun fid annotated_cfg ->
       let file_out = Printf.sprintf "%ld.dot" fid in
       Out_channel.with_file file_out
         ~f:(fun ch ->
             Out_channel.output_string ch (Cfg.to_dot annotated_cfg ~annot_str:Spec.to_dot_string)))

let count_vars =
  mk_intra "Count the number of program variables generated for a function"
    (Analysis_helpers.mk_intra (fun _ _ -> Int32Map.empty)
       (fun _ wasm_mod annotated_cfg ->
          let module CountVarsIntra = Intra.Make(struct
              type annot_expected = Spec_inference.state
              type state = (Var.Set.t * int)
              [@@deriving equal, compare, sexp]
              type summary = state
              let init_summaries _ = ()
              let init_state _ = (Var.Set.empty, 0)
              let bottom_state _ = (Var.Set.empty, 0)
              let state_to_string _ = ""
              let join_state (s1, n1) (s2, n2) = (Var.Set.union s1 s2, max n1 n2)
              let widen_state = join_state
              let extract_vars (st : Spec.t) : Var.Set.t =
                Var.Set.filter ~f:(function
                    | Merge _ -> false
                    | _ -> true)
                  (Var.Set.union (Var.Set.of_list (Spec.get_or_fail st).vstack)
                     (Var.Set.union (Var.Set.of_list (Spec.get_or_fail st).locals)
                        (Var.Set.union (Var.Set.of_list (Spec.get_or_fail st).globals)
                           (Var.Set.of_list (List.concat_map (Var.OffsetMap.to_alist (Spec.get_or_fail st).memory) ~f:(fun ((a, _), b) -> [a; b]))))))
              let transfer before after (vars, n) =
                ((Var.Set.union vars
                    (Var.Set.union (extract_vars before) (extract_vars after))),
                 (max n (Var.Set.length (extract_vars after))))
              let control_instr_transfer (_mod : Wasm_module.t) (_cfg : annot_expected Cfg.t) (i : annot_expected Instr.labelled_control) (vars, n) =
                `Simple (transfer i.annotation_before i.annotation_after (vars, n))
              let data_instr_transfer (_mod : Wasm_module.t) (_cfg : annot_expected Cfg.t) (i : annot_expected Instr.labelled_data) (vars, n) =
                transfer i.annotation_before i.annotation_after (vars, n)
              let merge_flows _mod cfg _block (states : (int * state) list) =
                List.fold_left (List.map states ~f:snd) ~init:(bottom_state cfg) ~f:join_state
              let summary _cfg st = st
            end) in
          let result = CountVarsIntra.analyze wasm_mod annotated_cfg in
          let (vars, n) = CountVarsIntra.final_state annotated_cfg result in
          Printf.printf "Vars: %d, max: %d\n" (Var.Set.length vars) n;
          (vars, n)))
    (fun fid summary ->
       Printf.printf "Vars %ld: %d, max: %d\n" fid (Var.Set.length (fst summary)) (snd summary))

let taint_intra =
  mk_intra "Just like `intra`, but only performs the taint analysis" Taint.analyze_intra
    (fun fid data ->
       Printf.printf "function %ld: %s\n" fid (Taint.Summary.to_string (fst data)))

let taint_cfg =
  Command.basic
    ~summary:"Generate a DOT file representing the taint-annotated CFG of function [fid] from the wasm file [in], in file [out]"
    Command.Let_syntax.(
      let%map_open file_in = anon ("in" %: string) and
      file_out = anon ("out" %: string) and
      funs = anon (sequence ("funs" %: int32)) in
      fun () ->
        Spec_inference.use_const := false;
        let results = Taint.analyze_intra (Wasm_module.of_file file_in) funs in
        (* We only output the latest analyzed CFG *)
        let annotated_cfg = Option.value_exn (snd (Int32Map.find_exn results (List.last_exn funs))) in
        output_to_file file_out (Cfg.to_dot annotated_cfg ~annot_str:Taint.Domain.only_non_id_to_string))


let relational_intra =
  mk_intra "Perform intra-procedural analyses of functions defined in the wat file [file]. The functions analyzed correspond to the sequence of arguments [funs], for example intra foo.wat 1 2 1 analyzes function 1, followed by 2, and then re-analyzes 1 (which can produce different result, if 1 depends on 2)" Relational.analyze_intra
    (fun fid summary ->
       Printf.printf "function %ld: %s" fid (Relational.Summary.to_string summary))

let reltaint_intra =
  mk_intra "Perform intra-procedural analyses of functions defined in the wat file [file]. The functions analyzed correspond to the sequence of arguments [funs], for example intra foo.wat 1 2 1 analyzes function 1, followed by 2, and then re-analyzes 1 (which can produce different result, if 1 depends on 2)" Reltaint.analyze_intra
    (fun fid summary ->
       Printf.printf "function %ld: %s, %s" fid (Relational.Summary.to_string (fst summary)) (Taint.Summary.to_string (snd summary)))

let mk_inter (desc : string) (analysis : Wasm_module.t -> Int32.t list list -> 'a Int32Map.t) (print : Int32.t -> 'a -> unit) =
  Command.basic
    ~summary:desc
    Command.Let_syntax.(
      let%map_open filename = anon ("file" %: string)
      and sccs = anon (sequence ("funs" %: int32_comma_separated_list)) in
      fun () ->
        let results = analysis (Wasm_module.of_file filename) sccs in
        Int32Map.iteri results ~f:(fun ~key:id ~data: summary -> print id summary))

let taint_inter =
  mk_inter "Performs inter analysis of a set of functions in file [file]. [funs] is a list of comma-separated function ids, e.g., to analyze function 1, then analyze both function 2 and 3 as part of the same fixpoint computation, [funs] is 1 2,3. The full schedule for any file can be computed using the `schedule` target."
    Taint.analyze_inter
    (fun fid summary -> Printf.printf "function %ld: %s\n" fid (Taint.Summary.to_string summary))

let find_indirect_calls =
  Command.basic
    ~summary:"Find call_indirect instructions and shows the function in which they appear as well as their label"
    Command.Let_syntax.(
      let%map_open filename = anon ("file" %: string) in
      fun () ->
        let module_ = Wasm_module.of_file filename in
        List.iter module_.funcs ~f:(fun finst ->
            let cfg = Spec_analysis.analyze_intra1 module_ finst.idx in
            let indirect_calls = Slicing.find_call_indirect_instructions cfg in
            List.iter indirect_calls ~f:(fun label ->
                Printf.printf "function %ld, instruction %s\n" finst.idx (Instr.Label.to_string label))))
