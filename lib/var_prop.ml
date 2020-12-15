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
        List.hd_exn (Var.Set.to_list without_v)
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
  end
end

(** Return the equalities that arise between variable, from an instruction *)
let eqs_data_instr (instr : (Instr.data, Spec.t) Instr.labelled) : VarEq.Set.t =
  match instr.instr with
  | Nop | Drop | Select | MemorySize | MemoryGrow
  | Unary _ | Binary _ | Compare _ | Test _ | Convert _ -> VarEq.Set.empty
  | Const n -> VarEq.Set.singleton (VarEq.of_vars (Spec_inference.top instr.annotation_after.vstack) (Var.Const n))
  | LocalGet l -> VarEq.Set.singleton (VarEq.of_vars (Spec_inference.top instr.annotation_after.vstack) (Spec_inference.get l instr.annotation_before.locals))
  | LocalSet l -> VarEq.Set.singleton (VarEq.of_vars (Spec_inference.get l instr.annotation_after.locals) (Spec_inference.top instr.annotation_before.vstack))
  | LocalTee l -> VarEq.Set.of_list
                    [VarEq.of_vars (Spec_inference.get l instr.annotation_after.locals) (Spec_inference.top instr.annotation_before.vstack);
                     VarEq.of_vars (Spec_inference.top instr.annotation_after.vstack) (Spec_inference.get l instr.annotation_before.locals)]
  | GlobalGet g -> VarEq.Set.singleton (VarEq.of_vars (Spec_inference.top instr.annotation_after.vstack) (Spec_inference.get g instr.annotation_before.globals))
  | GlobalSet g -> VarEq.Set.singleton (VarEq.of_vars (Spec_inference.get g instr.annotation_after.globals) (Spec_inference.top instr.annotation_before.vstack))
  | Load _ -> VarEq.Set.empty (* TODO (it is sound to ignore them, but shouldn't be too complicated to deal with this) *)
  | Store _ -> VarEq.Set.empty (* TODO: same *)

(** Perform variable propagation *)
let var_prop (cfg : Spec.t Cfg.t) : Spec.t Cfg.t =
  (* Go over each instruction and basic block, record all equality constraints that can be derived *)
  let equalities : VarEq.Set.t = List.fold_left (Cfg.all_blocks cfg)
      ~init:VarEq.Set.empty
      ~f:(fun eqs block ->
          match block.content with
          | Control _ -> eqs (* No equality arises from control blocks *)
          | Data instrs ->
            (* Go through the instructions of the block to gather their equalities *)
            List.fold_left instrs ~init:eqs ~f:(fun eqs instr -> VarEq.Set.union eqs (eqs_data_instr instr))
          | ControlMerge ->
            (* Equate annotation after each predecessor of this merge block with the annotation after this merge block *)
            let preds = List.map ~f:(Cfg.find_block_exn cfg) (Cfg.predecessors cfg block.idx) in
            List.fold_left preds
              ~init:eqs
              ~f:(fun eqs pred ->
                  VarEq.Set.union eqs
                    (VarEq.Set.of_list
                       ((List.map2_exn ~f:VarEq.of_vars pred.annotation_after.vstack block.annotation_after.vstack) @
                        (List.map2_exn ~f:VarEq.of_vars pred.annotation_after.locals block.annotation_after.locals) @
                        (List.map2_exn ~f:VarEq.of_vars pred.annotation_after.globals block.annotation_after.globals)))))
  in
  (* Filter out tautologies *)
  let equalities = VarEq.Set.filter equalities ~f:(fun vs -> Var.Set.length vs = 2) in
  (* Filter out variables that are involved in more than one equality *)
  let equalities = VarEq.Set.filter equalities ~f:(fun vs ->
      Var.Set.for_all vs ~f:(fun v ->
          (* If there is still one equality that contains v after removing the current equality *)
          if VarEq.Set.contains_var (VarEq.Set.remove equalities vs) v then begin
            Printf.printf "dropping equality: %s\n" (VarEq.to_string vs);
            false
          end else
            true)) in
  Printf.printf "equalities: %s\n" (VarEq.Set.to_string equalities);
  (* All variables that are involved in a single equality can then be propagated:
     they are replaced by the variable they are equal to.
     Precedence is given to variable defined at the entry point (locals, globals, constants, memory vars) *)
  (* TODO: need a way to map over annotations to change them.
     Then, for each annotation, we look at every variable. If it is part of an equality, it can be replaced (except if it is an "entry" variable *)
  Cfg.map_annotations cfg ~f:(fun (annot : Spec.t) ->
      Spec.map_vars annot ~f:(fun (v : Var.t) ->
          if entry_var v then
            (* Don't replace it, it is an entry var *)
            v
          else match VarEq.Set.find equalities ~f:(fun eq -> VarEq.contains eq v) with
            | Some eq ->
              let v' = VarEq.other_side eq v in
              Printf.printf "Replacing var %s by %s\n" (Var.to_string v) (Var.to_string v');
              v'
            | None -> v))

let%test "var prop - simple test" =
  Instr.reset_counter ();
  let module_ = Wasm_module.of_string "(module
  (type (;0;) (func (param i32) (result i32)))
  (func (;test;) (type 0) (param i32) (result i32)
    local.get 0 ;; Instr 0
    i32.const 0 ;; Instr 1
    i32.add     ;; Instr 2
  )
  (table (;0;) 1 1 funcref)
  (memory (;0;) 2)
  (global (;0;) (mut i32) (i32.const 66560)))" in
  let cfg = Spec_analysis.analyze_intra1 module_ 0 in
  let result = var_prop cfg in
  match (Cfg.find_block_exn result result.exit_block).annotation_after.vstack with
  | Var.Return :: [] -> true
  | _ -> false
