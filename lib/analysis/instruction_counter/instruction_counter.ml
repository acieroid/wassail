open Core
open Helpers


(** A a map from instructions (as strings) to the number of times it appears *)
type t = int StringMap.t

let increase_count (t : t) (instr : unit Instr.t) : t =
  StringMap.update t (Instr.to_mnemonic instr) ~f:(function
      | None -> 1
      | Some count -> count + 1)

(** Count for each type of instructions how many times it appears. *)
let count (wasm_mod : Wasm_module.t) : t =
  let rec go_over_instructions (counts : t) (fidx : Int32.t) (instrs : unit Instr.t list) : t =
    List.fold_left instrs ~init:counts
      ~f:(fun counts instr ->
          let counts' = go_over_instructions counts fidx (Instr.instructions_contained_in instr) in
          increase_count counts' instr) in
  List.fold_left wasm_mod.funcs ~init:StringMap.empty
    ~f:(fun acc func ->
      go_over_instructions acc func.idx func.code.body)

