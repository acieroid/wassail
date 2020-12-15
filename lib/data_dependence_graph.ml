open Core_kernel
open Helpers

module UseDefMap = struct
  module IntVarMap = Map.Make(struct
      (* TODO: deal with merge blocks: they don't have an "instruction index" *)
      type t = int * Var.t
      [@@deriving sexp, compare, equal]
    end)
  (** Use-definition chains, as a mapping from uses (as the index of the instruction that uses the variable, and the variable name) to their definition (as the index of the instruction that defines the variable)*)
  type t = int IntVarMap.t
  [@@deriving sexp, compare, equal]
  let empty : t = IntVarMap.empty
  let add (m : t) (use : int * Var.t) (def : int) =
    IntVarMap.add_exn m ~key:use ~data:def
end

(* TODO: To construct it: walk over each instruction, and collect uses and defines.
   There is exactly one define per variable.
   e.g., [] i32.const 0 [x] defines x
         [x, y] i32.add [z] defines z, uses x, y
   There may be more than one use!

   We can compute the following maps while walking over the instructions:
     uses: (int list) Var.Map.t (* The list of instructions that use a given variable *)
     def: int Var.Map.t (* The instruction that defines a given variable *)

   From this, it is a matter of folding over use to construct the DefUseMap:
     Given a use of v at instruction lab, add key:(lab, v) data:(lookup defs v)
*)
type t = UseDefMap.t
[@@deriving sexp, compare, equal]

(** Constructs a data dependence from the CFG of a function *)
let make (cfg : Spec.t Cfg.t) : t =
  (* The uses map will map variables to the indices of instructions that use them *)
  let uses_empty: (int list) Var.Map.t = Var.Map.empty in
  (* The defs map will map variables to the index of the instruciton that defines them.
     Because we are in SSA, there can only be one instruction that define a variable *)
  let defs_empty: int Var.Map.t = Var.Map.empty in
  let all_instrs : Spec.t Instr.t list = Cfg.all_instructions cfg in
  (* For each instruction, update the defs and uses map *)
  let (defs, uses) = List.fold_left all_instrs
    ~init:(defs_empty, uses_empty)
    ~f:(fun (defs, uses) instr -> match instr with
        | Instr.Data i ->
          let top_n_before n = List.take i.annotation_after.vstack n in
          let top_n_after n = List.take i.annotation_before.vstack n in
          let def vars =
            (* Instructions can only define the n top variables of the (after) stack *)
            List.fold_left vars ~init:defs ~f:(fun defs var ->
                (* add_exn because there can only be one definition for a variable *)
                Var.Map.add_exn defs ~key:var ~data:i.label) in
          let use vars =
            List.fold_left vars ~init:uses ~f:(fun uses var ->
                (* There can be multiple uses of a variable *)
                Var.Map.update uses var ~f:(function
                    | None -> [i.label]
                    | Some l -> i.label :: l)) in
          begin match i.instr with
            | Nop -> (defs, uses)
            | Drop -> (defs, use (top_n_before 1))
            | Select -> (def (top_n_after 1), use (top_n_before 3))
            | MemorySize -> (def (top_n_after 1), uses)
            | MemoryGrow -> (defs, use (top_n_before 1))
            | Const _ -> (def (top_n_after 1), uses)
            | Unary _ -> (def (top_n_after 1), use (top_n_before 1))
            | Binary _ -> (def (top_n_after 1), use (top_n_before 2))
            | Compare _ -> (def (top_n_after 1), use (top_n_before 2))
            | Test _ -> (def (top_n_after 1), use (top_n_before 1))
            | Convert _ -> (def (top_n_after 1), use (top_n_before 1))
            | LocalGet l -> (defs, use [List.nth_exn i.annotation_before.locals l]) (* use local l *)
            | LocalSet l -> (def [List.nth_exn i.annotation_after.locals l], uses) (* def local l *)
            | LocalTee l -> (def [List.nth_exn i.annotation_after.locals l], use (top_n_before 1)) (* def local l *)
            | GlobalGet g -> (defs, use [List.nth_exn i.annotation_before.globals g]) (* use global g *)
            | GlobalSet g -> (def [List.nth_exn i.annotation_after.globals g], use (top_n_before 1)) (* def global g *)
            | Load _ -> (defs, use (top_n_before 1)) (* use memory address *)
            | Store {offset; _} ->
              let addr = List.hd_exn i.annotation_before.vstack in
              (def [match Var.OffsetMap.find i.annotation_after.memory (addr, offset) with
                   | Some r -> r
                   | None -> failwith "Data_dependence_graph: can't find memory var"], use (top_n_before 2)) (* stores to memory value *)
          end
        | Instr.Control i ->
          let top_n_before n = List.take i.annotation_after.vstack n in
          let top_n_after n = List.take i.annotation_before.vstack n in
          let def vars =
            (* Instructions can only define the n top variables of the (after) stack *)
            List.fold_left vars ~init:defs ~f:(fun defs var ->
                (* add_exn because there can only be one definition for a variable *)
                Var.Map.add_exn defs ~key:var ~data:i.label) in
          let use vars =
            List.fold_left vars ~init:uses ~f:(fun uses var ->
                (* There can be multiple uses of a variable *)
                Var.Map.update uses var ~f:(function
                    | None -> [i.label]
                    | Some l -> i.label :: l)) in
          begin match i.instr with
            | Block _ | Loop _ -> (defs, uses) (* we handle instruction individually rather than through their block *)
            | If _ -> (defs, use (top_n_before 1)) (* We could say that if defines its "resulting" value, but that will be handled by the merge node *)
            | Call ((arity_in, arity_out), _) -> (def (top_n_after arity_out), use (top_n_before arity_in))
            | CallIndirect ((arity_in, arity_out), _) -> (def (top_n_after arity_out), use (top_n_before (arity_in + 1))) (* + 1 because we need to pop the index that will refer to the called function *)
            | Br _ -> (defs, uses)
            | BrIf _ -> (defs, use (top_n_before 1))
            | BrTable (_, _) -> (defs, use (top_n_before 1))
            | Return -> (defs, uses)
            | Unreachable -> (defs, uses)
          end) in
  (* Handle merge blocks: they define and use variables *)
  (* let merge_blocks = Cfg.all_merge_blocks cfg in
     let (defs, uses) = List.fold_left merge_blocks ~init:(defs, uses) ~f:(fun block -> *)
  (* TODO: handle merge nodes *)
  (*
     From this, it is a matter of folding over use to construct the DefUseMap:
     Given a use of v at instruction lab, add key:(lab, v) data:(lookup defs v) *)
  Var.Map.fold uses ~init:UseDefMap.empty ~f:(fun ~key:var ~data:uses map ->
      List.fold_left uses ~init:map ~f:(fun map use ->
          UseDefMap.add map (use, var) (match Var.Map.find defs var with
              | Some r -> r
              | None -> failwith "Data_dependence_graph: can't find def of var")))
  (* Derive a graph from def-use chains.
     The problem is simpler than for high-level languages, because we know there can only be one definition
     (and no other assignment!) to a program variable.
     Merge nodes just introduce longer chains. *)


(* TODO:
Tricky: removing instructions may break the shape of the stack!
To compute the PDG:
  - Compute a data-dependence graph
   - relies solely on the CFG?
   -> there is an edge from instruction x to y if x computes a value that may be used at node y in some feasible execution
      "A node y is data dependent on node x if:
        - exists v in Def(x) and Use(y), and
        - x is a reaching definition for y
  - Compute a control-dependence graph
   - relies on the CFG
   - and relies on a forward dominance tree!
 *)

(* Data dependence graph examples

1. local.get 0
2. i32.const 1
3. i32.add
4. local.get 0

1 -> 3
2 -> 3
and that's it.

should be simple for simple instructions.

Careful with merge nodes!

1. i32.const 0
2. local.get 0
3. if
4.   i32.const 1
    else
5.   i32.const 0
   end
6. <merge node>
7. i32.add

1 -> 7
2 -> 3
4 -> 6
5 -> 6
6 -> 7

We need to know for each instruction its use and defs -> that we know from our "variables"
Then 

*)

   (* For backward slicing, we can jump from the slicing criterion (i, v) (i is the instruction, v is the variable) to the instruction i' that defines v. Continue slicing from the uses of i' until we have an empty queue.

See https://www.sts.tuhh.de/pw-and-m-theses/2013/oehlm13.pdf, Algo. 2 p. 30
   (This only takes care of data dependences though!!!) *)
