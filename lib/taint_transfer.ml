open Core_kernel

module Make = functor (Spec : Spec_inference.SPEC) -> struct
  (* TODO: Make summaries. A summary is the final state restricted to only reachable vars: args, globals, mems and return value. *)

  (** A taint value is a set of variables *)
  type taint = Spec_inference.VarSet.t
  [@@deriving sexp, compare, equal]

  (** Joining taints is simply taking their union *)
  let join_taint (t1 : taint) (t2 : taint) : taint = Spec_inference.VarSet.union t1 t2

  let taint_to_string (t : taint) : string = String.concat ~sep:"," (List.map (Spec_inference.VarSet.to_list t)
                                                                     ~f:Spec_inference.var_to_string)

  (** The state of the taint analysis is a map from variables to their taint values.
      If a variable is not bound in the state, it is assumed that its taint is bottom *)
  type state = taint Spec_inference.VarMap.t
  [@@deriving sexp, compare, equal]

  (** In the initial state, we only set the taint for for parameters and the globals. *)
  let init_state (cfg : Cfg.t) : state =
    Spec_inference.VarMap.of_alist_exn
      ((List.mapi cfg.arg_types ~f:(fun i _ -> (Spec_inference.Local i,
                                                Spec_inference.VarSet.singleton (Spec_inference.Local i)))) @
       (List.mapi cfg.global_types ~f:(fun i _ -> (Spec_inference.Global i,
                                                 Spec_inference.VarSet.singleton (Spec_inference.Global i)))))

  (** The bottom state does not contain any taint. *)
  let bottom_state (_cfg : Cfg.t) : state =
    Spec_inference.VarMap.empty

  let state_to_string (s : state) : string =
    Printf.sprintf "[%s]" (String.concat ~sep:", "
                             (List.map (Spec_inference.VarMap.to_alist s)
                                ~f:(fun (k, t) ->
                                    Printf.sprintf "%s: %s"
                                      (Spec_inference.var_to_string k)
                                      (taint_to_string t))))

  let join_state _mod _cfg _block (s1 : state) (s2 : state) : state =
    Spec_inference.VarMap.merge s1 s2 ~f:(fun ~key:_ v -> match v with
        | `Both (x, y) -> Some (Spec_inference.VarSet.union x y)
        | `Left x | `Right x -> Some x)

  let get_taint (s : state) (var : Spec_inference.var) : taint =
    match Spec_inference.VarMap.find s var with
    | Some t -> t
    | None -> Spec_inference.VarSet.empty

  (** Add taint to avariable *)
  let state_add_taint (s : state) (v : Spec_inference.var) (taint : Spec_inference.var) : state =
    Printf.printf "add taint: %s -> %s\n" (Spec_inference.var_to_string v) (Spec_inference.var_to_string taint);
    Spec_inference.VarMap.update s v ~f:(function
        | None -> get_taint s taint
        | Some t -> Spec_inference.VarSet.union t (get_taint s taint))

  type summary = unit
  let init_summaries _ = ()

let data_instr_transfer (_module_ : Wasm_module.t) (_cfg : Cfg.t) (i : Instr.data Instr.labelled) (state : state) : state =
  match i.instr with
  | Nop | MemorySize | Drop -> state
  | Select ->
    let ret = Spec.ret i.label in
    let (_c, v2, v1) = Spec.pop3 (Spec.pre i.label).vstack in
    (* TODO: could improve precision by checking the constraints on c: if it is precisely zero/not-zero, we can only include v1 or v2 *)
    state_add_taint (state_add_taint state ret v1) ret v2
  | LocalGet l ->
    state_add_taint state (Spec.ret i.label) (Spec.get_nth (Spec.pre i.label).locals l)
  | LocalSet l ->
    state_add_taint state (Spec.get_nth (Spec.pre i.label).locals l) (Spec.pop (Spec.pre i.label).locals)
  | LocalTee l ->
    state_add_taint
      (state_add_taint state (Spec.get_nth (Spec.pre i.label).locals l) (Spec.pop (Spec.pre i.label).locals))
      (Spec.ret i.label) (Spec.get_nth (Spec.pre i.label).locals l)
  | GlobalGet g ->
      state_add_taint state (Spec.ret i.label) (Spec.get_nth (Spec.pre i.label).globals g)
  | GlobalSet g ->
    state_add_taint state (Spec.get_nth (Spec.pre i.label).globals g) (Spec.pop (Spec.pre i.label).globals)
  | Const _ -> state
  | Binary _ | Compare _ ->
    let v1, v2 = Spec.pop2 (Spec.pre i.label).vstack in
    state_add_taint
      (state_add_taint state (Spec.ret i.label) v1)
      (Spec.ret i.label) v2
  | Test _ | Convert _ ->
    state_add_taint state (Spec.ret i.label) (Spec.pop (Spec.pre i.label).vstack)
  | Load _ -> failwith "TODO: load"
  | Store _ -> failwith "TODO: store"

let control_instr_transfer
    (_module_ : Wasm_module.t) (* The wasm module (read-only) *)
    (_cfg : Cfg.t) (* The CFG analyzed *)
    (i : Instr.control Instr.labelled) (* The instruction *)
    (state : state) (* The pre state *)
  : [`Simple of state | `Branch of state * state] =
  match i.instr with
  | Call (_arity, _f) -> failwith "taint summaries"
  | CallIndirect (_arity, _typ) ->
    (* TODO: we could rely on the constraints to know which function is called *)
    failwith "taint summaries"
  | Br _ -> `Simple state
  | BrIf _ | If _ -> `Branch (state, state)
  | Return -> `Simple state
  | Unreachable -> `Simple state
  | _ -> `Simple state

let merge_flows (module_ : Wasm_module.t) (cfg : Cfg.t) (block : Basic_block.t) (states : (int * state) list) : state =
  match states with
  | [] -> init_state cfg
  | _ ->
    (* one or multiple states *)
      begin match block.content with
        | ControlMerge ->
          (* block is a control-flow merge *)
          let spec = Spec.post_block block.idx in
          let states' = List.map states ~f:(fun (idx, s) ->
              (* get the spec after that state *)
              let spec' = Spec.post_block idx in
              (* equate all different variables in the post-state with the ones in the pre-state *)
              List.fold_left (Spec_inference.extract_different_vars spec spec')
                ~init:s
                ~f:(fun s (x, y) ->
                    (* TODO: should it be x y or y x? *)
                    state_add_taint s x y)) in
          (* And finally joins all the states *)
          List.reduce_exn states' ~f:(join_state module_ cfg block)
        | _ ->
          (* Not a control-flow merge, there should be a single predecessor *)
          begin match states with
            | (_, s) :: [] -> s
            | _ -> failwith (Printf.sprintf "Invalid block with multiple input states: %d" block.idx)
          end
      end

end
