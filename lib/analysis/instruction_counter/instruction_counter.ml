open Core_kernel
open Helpers

(** Count for each type of instructions how many times it appears, and in how many functions.
    Return a map from the instruction (as a string) to a pair of:
      - the number of times it appears, and
      - the index of functions in which it appears.
*)
let count (wasm_mod : Wasm_module.t) : (int * Int32Set.t) StringMap.t =
  let rec go_over_instructions (acc : (int * Int32Set.t) StringMap.t) (fidx : Int32.t) (instrs : unit Instr.t list) : (int * Int32Set.t) StringMap.t =
    List.fold_left instrs ~init:acc
      ~f:(fun acc instr ->
          let acc' = go_over_instructions acc fidx (Instr.instructions_contained_in instr) in
          StringMap.update acc' (Instr.to_mnemonic instr) ~f:(function
              | None -> (1, Int32Set.singleton fidx)
              | Some (count, funs) -> (count+1, Int32Set.add funs fidx))) in
  List.fold_left wasm_mod.funcs ~init:StringMap.empty
    ~f:(fun acc funcinst ->
      go_over_instructions acc funcinst.idx funcinst.code.body)

