open Core
open Helpers

let mk_intra
    (init_data : unit Cfg.t Int32Map.t -> Wasm_module.t -> 'a Int32Map.t)
    (analysis : 'a Int32Map.t -> Wasm_module.t -> Spec.t Cfg.t -> 'a)
  : Wasm_module.t -> Int32.t list -> 'a Int32Map.t = fun wasm_mod funs ->
  let cfgs = Cfg_builder.build_all wasm_mod in
  List.fold_left funs
    ~init:(init_data cfgs wasm_mod)
    ~f:(fun summaries fid ->
        if Int32.(fid < wasm_mod.nfuncimports) then begin
          summaries
        end else
          let cfg = match Int32Map.find cfgs fid with
            | Some r -> r
            | None -> failwith "Analysis_helpers.mk_intra: can't find CFG" in
          let annotated_cfg, () = Spec_inference.Intra.analyze wasm_mod cfg Int32Map.empty in
          let (summary : 'a) = analysis summaries wasm_mod annotated_cfg in
          Int32Map.update summaries fid ~f:(fun _ -> summary))

let mk_inter
    (init_data : unit Cfg.t Int32Map.t -> Wasm_module.t -> 'a Int32Map.t)
    (analysis : Wasm_module.t -> Spec.t Cfg.t Int32Map.t -> 'a Int32Map.t -> 'a Int32Map.t)
  : Wasm_module.t -> Int32.t list list -> 'a Int32Map.t = fun wasm_mod sccs ->
  let cfgs = Cfg_builder.build_all wasm_mod in
  let annotated_cfgs = Int32Map.map cfgs ~f:(fun cfg -> Spec_inference.Intra.analyze wasm_mod cfg Int32Map.empty) in
  List.fold_left sccs
    ~init:(init_data cfgs wasm_mod)
    ~f:(fun summaries funs ->
        let scc_cfgs_and_summaries = Int32Map.filter_keys annotated_cfgs ~f:(fun idx -> List.mem funs idx ~equal:Stdlib.(=)) in
        let scc_cfgs = Int32Map.map ~f:fst scc_cfgs_and_summaries in
        let updated_summaries = analysis wasm_mod scc_cfgs summaries in
        Int32Map.fold updated_summaries
          ~init:summaries
          ~f:(fun ~key:idx ~data:sum acc ->
              Int32Map.set acc ~key:idx ~data:sum))


