open Core_kernel

(** Check if a variable is considered as being defined at the entry point *)
let entry_var (v : Var.t) : bool = match v with
  | Var _ | Merge (_, _) | Hole -> false
  | Local _ | Global _ | Return | Const _ -> true

(* Pairs of vars, only used to deal with variable equality so these are unordered pairs *)
module VarEq = struct
  module T = struct
    type t = Var.Set.t (* With the invariant that the size of the set is always 2 (in practice, it can be = 1 during the construction made by var_prop (e.g., if l0 = l0), but these elements will be filtred) *)
    [@@deriving sexp, compare, equal]
    let of_vars (v1 : Var.t) (v2 : Var.t) : t = Var.Set.of_list [v1; v2]
    let to_string (eq : t) : string =
      String.concat ~sep:"," (List.map ~f:Var.to_string (Var.Set.to_list eq))
    let other_side (eq : t) (v : Var.t) : Var.t =
      let without_v = Var.Set.remove eq v in
      if Var.Set.length without_v = 1 then
        match List.hd (Var.Set.to_list without_v) with
        | Some r -> r
        | None -> failwith "should not happen"
      else
        failwith "VarEq.other_side had an incorrect variable or equality"
    let contains (eq : t) (v : Var.t) : bool =
      Var.Set.mem eq v
  end
  include T
  module Set = struct
    include Set.Make(T)
    let to_string (vs : t) : string =
      String.concat ~sep:";" (List.map ~f:T.to_string (to_list vs))

    (** Check if there is one equality pair that contains the given variable *)
    let contains_var (vs : t) (v : Var.t) : bool =
      exists vs ~f:(fun eq -> Var.Set.mem eq v)

    let vars (vs : t) : Var.Set.t =
      fold vs ~init:Var.Set.empty ~f:Var.Set.union
  end
end

(** Return the equalities that arise between variable, from a data instruction *)
let eqs_data_instr (instr : (Instr.data, Spec.t) Instr.labelled) : VarEq.Set.t =
  match instr.instr with
  | Nop | Drop | Select | MemorySize | MemoryGrow
  | Unary _ | Binary _ | Compare _ | Test _ | Convert _ -> VarEq.Set.empty
  | Const n ->
    VarEq.Set.singleton (VarEq.of_vars (Spec_inference.top instr.annotation_after.vstack) (Var.Const n))
  | LocalGet l ->
    VarEq.Set.singleton (VarEq.of_vars (Spec_inference.top instr.annotation_after.vstack) (Spec_inference.get l instr.annotation_before.locals))
  | LocalSet l ->
    VarEq.Set.singleton (VarEq.of_vars (Spec_inference.get l instr.annotation_after.locals) (Spec_inference.top instr.annotation_before.vstack))
  | LocalTee l ->
    VarEq.Set.of_list
                    [VarEq.of_vars (Spec_inference.get l instr.annotation_after.locals) (Spec_inference.top instr.annotation_before.vstack);
                     VarEq.of_vars (Spec_inference.top instr.annotation_after.vstack) (Spec_inference.get l instr.annotation_before.locals)]
  | GlobalGet g ->
    VarEq.Set.singleton (VarEq.of_vars (Spec_inference.top instr.annotation_after.vstack) (Spec_inference.get g instr.annotation_before.globals))
  | GlobalSet g ->
    VarEq.Set.singleton (VarEq.of_vars (Spec_inference.get g instr.annotation_after.globals) (Spec_inference.top instr.annotation_before.vstack))
  | Load _ -> VarEq.Set.empty (* TODO (it is sound to ignore them, but shouldn't be too complicated to deal with this) *)
  | Store _ -> VarEq.Set.empty (* TODO: same *)

(** Perform variable propagation *)
let var_prop (cfg : Spec.t Cfg.t) : Spec.t Cfg.t =
  let init_spec = Spec_inference.init_state cfg in
  (* Go over each instruction and basic block, record all equality constraints that can be derived *)
  let equalities : VarEq.Set.t = List.fold_left (Cfg.all_blocks cfg)
      ~init:VarEq.Set.empty
      ~f:(fun eqs block ->
          match block.content with
          | Data instrs ->
            (* Go through the instructions of the block to gather their equalities *)
            List.fold_left instrs ~init:eqs ~f:(fun eqs instr -> VarEq.Set.union eqs (eqs_data_instr instr))
          | Control { instr = Merge; _ } ->
            (* Equate annotation after each predecessor of this merge block with the annotation after this merge block *)
            let spec = Cfg.state_after_block cfg block.idx init_spec in
            List.fold_left (Cfg.predecessors cfg block.idx)
              ~init:eqs
              ~f:(fun eqs pred ->
                  let pred_spec = Cfg.state_after_block cfg pred init_spec in
                  assert (List.length pred_spec.vstack = List.length spec.vstack);
                  assert (List.length pred_spec.locals = List.length spec.locals);
                  assert (List.length pred_spec.globals = List.length spec.globals);
                  VarEq.Set.union eqs
                    (VarEq.Set.of_list
                       ((List.map2_exn ~f:VarEq.of_vars pred_spec.vstack spec.vstack) @
                        (List.map2_exn ~f:VarEq.of_vars pred_spec.locals spec.locals) @
                        (List.map2_exn ~f:VarEq.of_vars pred_spec.globals spec.globals))))
          | Control _ -> eqs (* No equality arises from other control blocks *)
        )
  in
  (* Filter out tautologies *)
  let equalities = VarEq.Set.filter equalities ~f:(fun vs -> Var.Set.length vs = 2) in
  (* Filter out merge variables that are involved in more than one equality.
     For other variables, it is fine for them to be in multiple equalities (e.g., i0 = 1024, i1 = i0) *)
  let equalities = VarEq.Set.filter equalities ~f:(fun vs ->
      Var.Set.for_all vs ~f:(fun v ->
          match v with
          | Var.Merge _ ->
            (* If there is still one equality that contains v after removing the current equality *)
            (* NOTE: it could be perfectly safe in some case to keep the merge variables, but this case may be a bit complex to compute. As a result, the reduction in variables is not the same as if we would perform variable propagation directly. *)
            not (VarEq.Set.contains_var (VarEq.Set.remove equalities vs) v)
          | _ -> true)) in
  (* Compute equality classes *)
  let rec eq_classes_computation (worklist : Var.Set.t) (current_class : Var.Set.t) (classes : Var.Set.t list) (equalities : VarEq.Set.t) : Var.Set.t list =
    match Var.Set.choose worklist with
    | None ->
      (* No more variable on the worklist, we're done with the current class *)
      begin match VarEq.Set.choose equalities with
        | None ->
          (* We went through all equalities, we're done *)
          List.filter (current_class :: classes) ~f:(fun c -> not (Var.Set.is_empty c))
        | Some eq ->
          (* Compute the next equality class *)
          eq_classes_computation eq Var.Set.empty (current_class :: classes) (VarEq.Set.remove equalities eq)
      end
    | Some var ->
      if Var.Set.mem current_class var then
        (* Already in the class, we can ignore it *)
        eq_classes_computation (Var.Set.remove worklist var) current_class classes equalities
      else
        (* Add var to the current equality class,
           and add all equalities in which it is involved to the worklist *)
        let eqs = VarEq.Set.filter equalities ~f:(fun eq -> VarEq.contains eq var) in
        let vars = VarEq.Set.vars eqs in
        eq_classes_computation (Var.Set.union (Var.Set.remove worklist var) vars) (Var.Set.add current_class var) classes (VarEq.Set.diff equalities eqs) in
  let classes = eq_classes_computation Var.Set.empty Var.Set.empty [] equalities in
  let rewrite_target (vs : Var.Set.t) : Var.t =
    (* Compute the target of a rewrite for all variables in a given class.
       If one of the variable is an entry var, pick this one.
       Otherwise, simply return the least element according to the ordering on Var.t *)
    match Var.Set.find vs ~f:entry_var with
    | Some v -> v
    | None -> match Var.Set.min_elt vs with
      | Some v -> v
      | None -> failwith "var_prop: invalid equality class" in
  (* All variables that are involved in a single equality can then be propagated:
     they are replaced by the variable they are equal to.
     Precedence is given to variable defined at the entry point (locals, globals, constants, memory vars) *)
  let fannot (annot : Spec.t) : Spec.t = Spec.map_vars annot ~f:(fun (v : Var.t) ->
      match List.find classes ~f:(fun vs -> Var.Set.mem vs v) with
      | Some class_ ->
        (* Variable is part of an equality class, replace it by its target *)
        rewrite_target class_
      | None -> v) in
  Cfg.map_annotations cfg
    ~f:(fun i -> (fannot (Instr.annotation_before i), fannot (Instr.annotation_after i)))

let all_vars (cfg : Spec.t Cfg.t) : Var.Set.t =
  let vars : Var.Set.t ref = ref Var.Set.empty in
  let fannot (annot : Spec.t) : Spec.t = Spec.map_vars annot ~f:(fun (v : Var.t) ->
      vars := Var.Set.add !vars v;
      v) in
  let _cfg = Cfg.map_annotations cfg
      ~f:(fun i -> (fannot (Instr.annotation_before i), fannot (Instr.annotation_after i))) in
  !vars

let count_vars (cfg : Spec.t Cfg.t) : int =
  Var.Set.length (all_vars cfg)

module Test = struct
  let%test "var prop - simple test" =
    Spec_inference.propagate_globals := false;
    Spec_inference.propagate_locals := false;
    Spec_inference.use_const := false;
    let module_ = Wasm_module.of_string "(module
  (type (;0;) (func (param i32) (result i32)))
  (func (;test;) (type 0) (param i32) (result i32)
    local.get 0 ;; Instr 0, [i0], but clearly, i0 = l0
    i32.const 0 ;; Instr 1, [i1, i0], but clearly, i1 = 0
    i32.add     ;; Instr 2, [i2, i1, i0], with i2 = ret
  )
  (table (;0;) 1 1 funcref)
  (memory (;0;) 2)
  (global (;0;) (mut i32) (i32.const 66560)))" in
    let cfg = Spec_analysis.analyze_intra1 module_ 0l in
    Spec_inference.propagate_globals := true;
    Spec_inference.propagate_locals := true;
    Spec_inference.use_const := true;
    let result = var_prop cfg in
    let actual = all_vars result in
    let expected = Var.Set.of_list [Var.Return; Var.Local 0; Var.Global 0; Var.Const (Prim_value.of_int 0)] in
    Var.Set.check_equality ~actual ~expected

  let%test "var prop - big program" =
    let module_ = Wasm_module.of_file "../../../benchmarks/polybench-clang/trmm.wat" in
    Spec_inference.propagate_globals := false;
    Spec_inference.propagate_locals := false;
    Spec_inference.use_const := false;
    let cfg = Spec_analysis.analyze_intra1 module_ 14l in
    let actual = var_prop cfg in
    count_vars actual < count_vars cfg
end
