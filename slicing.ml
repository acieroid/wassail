open Core
open Wassail
open Utils

let dependencies =
  Command.basic
    ~summary:"Produce a PDG for a given function"
    Command.Let_syntax.(
      let%map_open filename = anon ("file" %: string)
      and funidx = anon ("fun" %: int32)
      and dot_filename = anon ("out" %: string) in
      fun () ->
        Spec_inference.propagate_globals := false;
        Spec_inference.propagate_locals := false;
        Spec_inference.use_const := false;
        let module_ = Wasm_module.of_file filename in
        let cfg = Spec_analysis.analyze_intra1 module_ funidx in
        let use_def_annot = (Use_def.annotate module_ cfg) in
        let control_annot = (Control_deps.annotate_exact (Cfg.without_empty_nodes_with_no_predecessors cfg)) in
        Out_channel.with_file dot_filename
          ~f:(fun ch ->
              Out_channel.output_string ch (Cfg.to_dot cfg
                                              ~annot_str:Spec_domain.to_dot_string
                                              ~extra_data:(use_def_annot ^ control_annot))))

let cdg =
  Command.basic
    ~summary:"Produce a CDG for a given function"
    Command.Let_syntax.(
      let%map_open filename = anon ("file" %: string)
      and funidx = anon ("fun" %: int32)
      and dot_filename = anon ("out" %: string) in
      fun () ->
        Spec_inference.propagate_globals := false;
        Spec_inference.propagate_locals := false;
        Spec_inference.use_const := false;
        let module_ = Wasm_module.of_file filename in
        let cfg = Spec_analysis.analyze_intra1 module_ funidx in
        let control_annot = (Control_deps.annotate_exact cfg) in
        Out_channel.with_file dot_filename
          ~f:(fun ch ->
              Out_channel.output_string ch (Cfg.to_dot cfg
                                              ~include_edges:false
                                              ~extra_data:control_annot)))
let postdom =
  Command.basic
    ~summary:"Visualize the post-dominator tree of a function"
    Command.Let_syntax.(
      let%map_open filename = anon ("file" %: string)
      and funidx = anon ("fun" %: int32)
      and dot_filename = anon ("out" %: string) in
      fun () ->
        let module_ = Wasm_module.of_file filename in
        let cfg = Spec_analysis.analyze_intra1 module_ funidx in
        let tree : Tree.t = Dominance.cfg_post_dominator cfg in
        Out_channel.with_file dot_filename
          ~f:(fun ch ->
              Out_channel.output_string ch (Tree.to_dot tree)))

let all_labels (instrs : 'a Instr.t list) : Instr.Label.Set.t =
  List.fold_left instrs
    ~init:Instr.Label.Set.empty
    ~f:(fun acc instr ->
        Instr.Label.Set.union acc (Instr.all_labels_no_merge instr))


let slice_line_number =
  Command.basic
    ~summary:"Produce an executable program after slicing the given function at the given slicing criterion"
    Command.Let_syntax.(
      let%map_open filename = anon ("file" %: string)
      and funidx = anon ("fun" %: int32)
      and line_number = anon ("line-number" %: int)
      and outfile = anon ("output" %: string) 
      and no_pointer_analysis = flag "--no-pointers" no_arg ~doc:" Disables pointer analysis before slicing: les precise, but faster" 
      and trace = flag "--trace" no_arg ~doc:"Print an execution trace (may slow down execution)" in
      if trace then Wassail.Log.enable_info ();
      fun () ->
        Spec_inference.propagate_globals := false;
        Spec_inference.propagate_locals := false;
        Spec_inference.use_const := false;
        Log.info "Loading module";
        let module_ = Wasm_module.of_file filename in
        let cfg_raw = Cfg_builder.build module_ funidx in
        Log.info "Constructing CFG";
        let cfg = Cfg.without_empty_nodes_with_no_predecessors (*Spec_analysis.analyze_intra1 module_ funidx*)
        (Spec_inference.Intra.analyze module_ cfg_raw ()) in
        let pointer_analysis =
          if not no_pointer_analysis then
            (if trace then Wassail.Log.enable_info ();
            let (cfg_pointers, instructions_from_pointer_cfg, summaries) =
              Value_set.Options.show_intermediates := true;
              Value_set.run_pointer_analysis module_ cfg_raw funidx in
            let cfgdot = Cfg.to_dot cfg_pointers ~annot_str:Value_set.Domain.to_string in
            let () = Out_channel.write_all (filename ^ ".pointers.dot") ~data:cfgdot in
            Some (cfg_pointers, instructions_from_pointer_cfg, summaries))
          else
            None
        in
        let instr = match List.find (Cfg.all_instructions_list cfg) ~f:(fun instr -> (Instr.line_number instr) = line_number) with
             | None -> failwith "No instruction found at this line"
             | Some instr -> instr in
        let slicing_criterion = Instr.label instr in
        Log.info ("---------- Slicing of function " ^ (Int32.to_string funidx) ^ " ----------");
        Log.info (Printf.sprintf "Slicing criterion: %s" (Instr.Label.to_string slicing_criterion));
        let funcinst = Slicing.slice_to_funcinst module_ cfg (Cfg.all_instructions cfg) (Instr.Label.Set.singleton slicing_criterion) pointer_analysis in
        Log.info "done";
        (* let sliced_labels = all_labels funcinst.code.body in *)
        let module_ = Wasm_module.replace_func module_ funidx funcinst in
        Out_channel.with_file outfile
          ~f:(fun ch -> Out_channel.output_string ch (Wasm_module.to_string module_)))
