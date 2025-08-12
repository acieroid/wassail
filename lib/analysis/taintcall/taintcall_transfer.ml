open Core
open Helpers

module TaintTransfer = Taint_transfer.Make

(** Need to match the taint analysis expected annotations *)
type annot_expected = TaintTransfer.annot_expected

module State = Taintcall_domain

let bottom : State.t = Taintcall_domain.bottom

let init (module_ : Wasm_module.t) (funcinst : Func_inst.t) : State.t =
  (Taintcall_domain.Call.bottom, TaintTransfer.init module_ funcinst)

let state_to_string (s : State.t) : string = Taintcall_domain.to_string s

let join_state (s1 : State.t) (s2 : State.t) : State.t = Taintcall_domain.join s1 s2

let widen_state (_s1 : State.t) (s2 : State.t) : State.t = s2 (* no widening *)

module Summary = struct
  type t = Taintcall_domain.Call.t * TaintTransfer.summary
end

type summary = Summary.t

let data
    (module_ : Wasm_module.t)
    (cfg : annot_expected Cfg.t)
    (i : annot_expected Instr.labelled_data)
    (state : State.t)
  : State.t =
  (* No call can arise in a data instruction, so the state remains the same *)
  (fst state, TaintTransfer.data module_ cfg i (snd state))

let control
    (module_ : Wasm_module.t) (* The wasm module (read-only) *)
    (cfg : annot_expected Cfg.t) (* The CFG analyzed *)
    (i : annot_expected Instr.labelled_control) (* The instruction *)
    (state : State.t) (* The pre state *)
  : [`Simple of State.t | `Branch of State.t * State.t ] =
  match TaintTransfer.control module_ cfg i (snd state) with
  | `Simple sndstate' ->
    begin match i.instr with
      | Unreachable -> `Simple bottom
      | _ -> `Simple (fst state, sndstate')
    end
  | `Branch (s1, s2) -> `Branch ((fst state, s1), (fst state, s2))

let apply_imported
    (_module : Wasm_module.t)
    (f : Int32.t)
    (_arity : int * int)
    (_i : annot_expected Instr.labelled_call)
    (state : State.t)
  : State.t =
  Log.warn (Printf.sprintf "No summary found for function %ld (imported function): assuming taint is preserved" f);
    state

let apply_summary
    (module_ : Wasm_module.t)
    (f : Int32.t)
    (arity : int * int)
    (i : annot_expected Instr.labelled_call)
    (state : State.t)
    (summary : summary)
    : State.t =
  let apply_summary (argsv : Taint_domain.Taint.t list) : Taintcall_domain.Call.t =
    let substituted_summary : Taintcall_domain.Call.t =
      Int32Map.map (fst summary) ~f:(fun args ->
          (List.map args ~f:(fun arg -> Taint_domain.Taint.substitute arg (List.mapi argsv ~f:(fun i argv -> (Var.Local i, argv)))))) in
    Taintcall_domain.Call.join substituted_summary (fst state)
  in
  let add_call (to_state : Taintcall_domain.Call.t) (f : Int32.t) (arity : int * int) : Taintcall_domain.Call.t =
    let args = List.take (Spec_domain.get_or_fail i.annotation_before).vstack (fst arity) in
    let argsv = List.map args ~f:(Taint_domain.get_taint (snd state)) in
    Int32Map.update to_state f ~f:(function
         | None -> argsv
         | Some old -> List.map2_exn old argsv ~f:Taint_domain.Taint.join)
  in
  let sndstate' = TaintTransfer.apply_summary module_ f arity i (snd state) (snd summary) in
  let args = List.take (Spec_domain.get_or_fail i.annotation_before).vstack (fst arity) in
  add_call (apply_summary (List.map args ~f:(Taint_domain.get_taint (snd state)))) f arity, sndstate'

let extract_summary (_module_ : Wasm_module.t) (cfg : annot_expected Cfg.t) (analyzed_cfg : State.t Cfg.t) : summary =
  let exit_block = Cfg.find_block_exn analyzed_cfg analyzed_cfg.exit_block in
  let out_state = exit_block.annotation_after in
  (fst out_state, Taint_summary.summary_of cfg (snd out_state))

let merge_flows
    (module_ : Wasm_module.t)
    (cfg : annot_expected Cfg.t)
    (block : annot_expected Basic_block.t)
    (predecessors : ('a Basic_block.t * State.t) list) : State.t =
  (match List.reduce (List.map ~f:(fun x -> fst (snd x)) predecessors) ~f:Taintcall_domain.Call.join with
   | None -> init module_ (Wasm_module.get_funcinst module_ cfg.idx)
   | Some s' -> s',
   TaintTransfer.merge_flows module_ cfg block (List.map predecessors ~f:(fun x -> (fst x, (snd (snd x))))))

let call_inter
    (_module : Wasm_module.t)
    (_cfg : annot_expected Cfg.t)
    (_instr : annot_expected Instr.labelled_call)
    (state : State.t)
  : State.t =
  state

let entry (_module_ : Wasm_module.t) (_cfg : annot_expected Cfg.t) (state : State.t) : State.t =
  state

let return (_module : Wasm_module.t) (_cfg : annot_expected Cfg.t) (_instr : annot_expected Instr.labelled_call) (_state_before_call : State.t) (state_after_call : State.t) : State.t =
  state_after_call

let imported (_module_ : Wasm_module.t) (_desc : Wasm_module.func_desc) : State.t -> State.t =
  failwith "TODO: taintcall imported"
