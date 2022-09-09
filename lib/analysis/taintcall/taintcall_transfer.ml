open Core
open Helpers

module TaintTransfer = Taint_transfer.Make

(** Need to match the taint analysis expected annotations *)
type annot_expected = TaintTransfer.annot_expected

type state = Taintcall_domain.t
[@@deriving sexp, compare, equal]

let bottom : state = Taintcall_domain.bottom

let init_state (cfg : 'a Cfg.t) : state = (Taintcall_domain.Call.bottom, TaintTransfer.init_state cfg)

let bottom_state (cfg : 'a Cfg.t) : state = (Taintcall_domain.Call.bottom, TaintTransfer.bottom_state cfg)

let state_to_string (s : state) : string = Taintcall_domain.to_string s

let join_state (s1 : state) (s2 : state) : state = Taintcall_domain.join s1 s2

let widen_state (_s1 : state) (s2 : state) : state = s2 (* no widening *)

type summary = Taintcall_domain.Call.t * TaintTransfer.summary

let data_instr_transfer
    (module_ : Wasm_module.t)
    (cfg : annot_expected Cfg.t)
    (i : annot_expected Instr.labelled_data)
    (state : state)
  : state =
  (* No call can arise in a data instruction, so the state remains the same *)
  (fst state, TaintTransfer.data_instr_transfer module_ cfg i (snd state))

let control_instr_transfer
    (module_ : Wasm_module.t) (* The wasm module (read-only) *)
    (summaries : summary Int32Map.t) (* The summaries *)
    (cfg : annot_expected Cfg.t) (* The CFG analyzed *)
    (i : annot_expected Instr.labelled_control) (* The instruction *)
    (state : state) (* The pre state *)
  : [`Simple of state | `Branch of state * state ] =
  let apply_summary (f : Int32.t) (argsv : Taint_domain.Taint.t list) : Taintcall_domain.Call.t =
    match Int32Map.find summaries f with
    | None ->
      if Int32.(f < module_.nfuncimports) then begin
        Log.warn (Printf.sprintf "No summary found for function %ld (imported function): assuming taint is preserved" f);
        fst state
      end else
        failwith "Unexpected: analyzing a function that has no summary"
    | Some summary ->
      let substituted_summary =
        Int32Map.map (fst summary) ~f:(fun args ->
            (List.map args ~f:(fun arg -> Taint_domain.Taint.substitute arg (List.mapi argsv ~f:(fun i argv -> (Var.Local i, argv)))))) in
      Taintcall_domain.Call.join substituted_summary (fst state)
  in
  let add_call (to_state : Taintcall_domain.Call.t) (f : Int32.t) (arity : int * int) : Taintcall_domain.Call.t =
    let args = List.take (Spec.get_or_fail (fst i.annotation_before)).vstack (fst arity) in
    let argsv = List.map args ~f:(Taint_domain.get_taint (snd state)) in
    Int32Map.update to_state f ~f:(function
         | None -> argsv
         | Some old -> List.map2_exn old argsv ~f:Taint_domain.Taint.join)
  in
  match TaintTransfer.control_instr_transfer module_
          (Int32Map.map ~f:snd summaries) cfg i (snd state) with
  | `Simple sndstate' ->
    let apply_fun f arity =
      let args = List.take (Spec.get_or_fail (fst i.annotation_before)).vstack (fst arity) in
      add_call (apply_summary f (List.map args ~f:(Taint_domain.get_taint (snd state)))) f arity, sndstate'
    in
    begin match i.instr with
      | Call (arity, _, f) -> `Simple (apply_fun f arity)
      | CallIndirect (arity, _, typ) ->
        let funs_with_matching_type = Call_graph.indirect_call_targets module_ typ in
        `Simple (List.fold_left funs_with_matching_type
                   ~init:state
                   ~f:(fun acc idx ->
                       let (call, taint) = apply_fun idx arity in
                       (Taintcall_domain.Call.join call (fst acc),
                        Taint_domain.join taint (snd acc))))
      | Unreachable -> `Simple bottom
      | _ -> `Simple (fst state, sndstate')
    end
  | `Branch (s1, s2) -> `Branch ((fst state, s1), (fst state, s2))

let extract_summary (cfg : annot_expected Cfg.t) (analyzed_cfg : state Cfg.t) : summary =
  let out_state = Cfg.state_after_block analyzed_cfg analyzed_cfg.exit_block (init_state cfg) in
  (fst out_state, TaintTransfer.summary cfg (snd out_state))

let merge_flows (module_ : Wasm_module.t) (cfg : annot_expected Cfg.t) (block : annot_expected Basic_block.t) (states : (int * state) list) : state =
  (match List.reduce (List.map ~f:(fun x -> fst (snd x)) states) ~f:Taintcall_domain.Call.join with
   | None -> init_state cfg
   | Some s' -> s',
   TaintTransfer.merge_flows module_ cfg block (List.map states ~f:(fun x -> (fst x, (snd (snd x))))))
