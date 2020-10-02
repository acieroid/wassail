open Core_kernel
open Helpers

let mk_intra
    (init_summaries : unit Cfg.t IntMap.t -> Wasm_module.t -> 'a IntMap.t)
    (analysis : 'a IntMap.t -> Wasm_module.t -> Spec_inference.state Cfg.t -> 'a)
  : string -> int list -> 'a IntMap.t = fun filename funs ->
  apply_to_textual filename (fun m ->
      let wasm_mod = Wasm_module.of_wasm m in
      let cfgs = Cfg_builder.build_all wasm_mod in
      List.fold_left funs
        ~init:(init_summaries cfgs wasm_mod)
        ~f:(fun summaries fid ->
            if fid < wasm_mod.nimports then begin
              Logging.info "This is an imported function, it does not have to be analyzed.\n";
              summaries
            end else
              let cfg = IntMap.find_exn cfgs fid in
              Logging.info (Printf.sprintf "---------- Spec analysis of function %d ----------" cfg.idx);
              let annotated_cfg = Spec.Intra.analyze wasm_mod cfg in
              let (summary : 'a) = analysis summaries wasm_mod annotated_cfg in
              IntMap.update summaries fid ~f:(fun _ -> summary)))

let mk_inter
    (init_summaries : unit Cfg.t IntMap.t -> Wasm_module.t -> 'a IntMap.t)
    (analysis : Wasm_module.t -> Spec_inference.state Cfg.t IntMap.t -> 'a IntMap.t)
  : string -> int list list -> 'a IntMap.t = fun filename sccs ->
  apply_to_textual filename (fun m ->
      let wasm_mod = Wasm_module.of_wasm m in
      let cfgs = Cfg_builder.build_all wasm_mod in
      let annotated_cfgs = IntMap.map cfgs ~f:(fun cfg -> Spec.Intra.analyze wasm_mod cfg) in
      List.fold_left sccs
        ~init:(init_summaries cfgs wasm_mod)
        ~f:(fun summaries funs ->
            let scc_cfgs = IntMap.filter_keys annotated_cfgs ~f:(fun idx -> List.mem funs idx ~equal:Stdlib.(=)) in
            let updated_summaries = analysis wasm_mod scc_cfgs in
            IntMap.fold updated_summaries
              ~init:summaries
              ~f:(fun ~key:idx ~data:sum acc ->
                  IntMap.set acc ~key:idx ~data:sum)))


