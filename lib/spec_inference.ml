open Core_kernel
open Helpers

type var =
  | Var of int
  | Local of int (* nth local *)
  | Global of int (* nth global *)
  | MemoryKey of Instr.label * int
  | MemoryVal of Instr.label * int
  | MemoryValNew of Instr.label * int
  | Merge of int * int
[@@deriving sexp, compare, equal]

let var_to_string (v : var) : string = match v with
  | Var n -> Printf.sprintf "i%d" n
  | Local n -> Printf.sprintf "l%d" n
  | Global n -> Printf.sprintf "g%d" n
  | MemoryKey (l, v) -> Printf.sprintf "mk%d_%d" l v
  | MemoryVal (l, v) -> Printf.sprintf "mv%d_%d" l v
  | MemoryValNew (l, v) -> Printf.sprintf "mvnew%d_%d" l v
  | Merge (idx, n) -> Printf.sprintf "merge%d_%d" idx n

module Var = struct
  type t = var
  [@@deriving sexp, compare]
end

module VarMap = Map.Make(Var)
module VarSet = Set.Make(Var)

(** The state is a specification of the runtime components *)
type state = {
  vstack : var list;
  locals : var list;
  globals : var list;
  memory : var VarMap.t;
}
[@@deriving sexp, compare]

(** Returns all the variables contained in the state *)
let vars_of (s : state) : VarSet.t =
  VarSet.union (VarSet.of_list s.vstack)
    (VarSet.union (VarSet.of_list s.locals)
       (VarSet.union (VarSet.of_list s.globals)
          (VarSet.of_list
             (List.concat (List.map (VarMap.to_alist s.memory)
                             ~f:(fun (k, v) -> [k; v]))))))

(** Returns all the variables contained in the spec map *)
let vars (data : (state * state) IntMap.t) : VarSet.t =
  List.fold_left (IntMap.to_alist data)
    ~init:VarSet.empty
    ~f:(fun acc (_, (pre, post)) ->
        VarSet.union acc (VarSet.union (vars_of pre) (vars_of post)))

let extract_different_vars (s1 : state) (s2 : state) : (var * var) list =
  let f (l1 : var list) (l2 : var list) : (var * var) list =
    assert (List.length l1 = List.length l2);
    List.filter_map (List.map2_exn l1 l2 ~f:(fun v1 v2 -> (v1, v2, equal_var v1 v2)))
      ~f:(fun (v1, v2, eq) -> if eq then Some (v1, v2) else None) in
  (* TODO: what about memory ? *)
  (f s1.vstack s2.vstack) @ (f s1.locals s2.locals) @ (f s1.globals s2.globals)


let init_state (cfg : Cfg.t) : state = {
  vstack = []; (* the vstack is initially empty *)
  locals = List.mapi (cfg.arg_types @ cfg.local_types) ~f:(fun i _ -> Local i);
  globals = List.mapi cfg.global_types ~f:(fun i _ -> Global i);
  memory = begin
    let key (label : Instr.label) (n : int) : var = MemoryKey (label, n) in
    let value (label : Instr.label) (n : int) : var = MemoryVal (label, n) in
    List.fold_left (IntMap.data cfg.basic_blocks)
      ~init:VarMap.empty
      ~f:(fun m block -> match block.content with
          | Data instrs ->
            List.fold_left instrs
              ~init:m
              ~f:(fun m i -> match i.instr with
                  | Load op | Store op ->
                    begin match op with
                      | { typ = I32; sz = None; _ } ->
                        (* I32 is 4 bytes *)
                        VarMap.add_exn ~key:(key i.label 0) ~data:(value i.label 0)
                          (VarMap.add_exn ~key:(key i.label 1) ~data:(value i.label 1)
                             (VarMap.add_exn ~key:(key i.label 2) ~data:(value i.label 2)
                                (VarMap.add_exn ~key:(key i.label 3) ~data:(value i.label 3)
                                   m)))
                      | { typ = I64; sz = None; _ } ->
                        (* I64 is 8 bytes *)
                        VarMap.add_exn ~key:(key i.label 0) ~data:(value i.label 0)
                          (VarMap.add_exn ~key:(key i.label 1) ~data:(value i.label 1)
                             (VarMap.add_exn ~key:(key i.label 2) ~data:(value i.label 2)
                                (VarMap.add_exn ~key:(key i.label 3) ~data:(value i.label 3)
                                   (VarMap.add_exn ~key:(key i.label 4) ~data:(value i.label 4)
                                      (VarMap.add_exn ~key:(key i.label 5) ~data:(value i.label 5)
                                         (VarMap.add_exn ~key:(key i.label 6) ~data:(value i.label 6)
                                            (VarMap.add_exn ~key:(key i.label 7) ~data:(value i.label 7)
                                               m)))))))

                      | { typ = I32; sz = Some (Pack8, _); _ } ->
                        (* only one byte *)
                        VarMap.add_exn ~key:(key i.label 0) ~data:(value i.label 0) m
                      | _ -> failwith "unsupported memory op"
                    end
                  | _ -> m)
          | Control _ | ControlMerge | Nothing -> m)
  end;
}

let state_to_string (s : state) : string =
  Printf.sprintf "{\nvstack: [%s]\nlocals: [%s]\nglobals: [%s]\nmemory: [%s]\n}"
    (String.concat ~sep:", " (List.map s.vstack ~f:var_to_string))
    (String.concat ~sep:", " (List.map s.locals ~f:var_to_string))
    (String.concat ~sep:", " (List.map s.globals ~f:var_to_string))
    (String.concat ~sep:", " (List.map (VarMap.to_alist s.memory) ~f:(fun (k, v) -> Printf.sprintf "%s: %s" (var_to_string k) (var_to_string v))))

let join_state (s1 : state) (s2 : state) : state =
  (* States can only be joined if they actually match! (They should always match if they have to be joined) *)
  assert (compare_state s1 s2 = 0);
  s1

type result =
  | Uninitialized
  | Simple of state
  | Branch of state * state
[@@deriving sexp, compare]

(** Like List.drop, but raises an exception if the list does not contain enough element *)
let drop (n : int) (vstack : var list) =
  assert (List.length vstack >= n);
  List.drop vstack n

let get (n : int) (l : var list) = List.nth_exn l n

let set (n : int) (l : var list) (v : var) = List.mapi l ~f:(fun i v' -> if i = n then v else v')

let data_instr_transfer (_module_ : Wasm_module.t) (_cfg : Cfg.t) (i : Instr.data Instr.labelled) (state : state) : state =
  let key (n : int) : var = MemoryKey (i.label, n) in
  let value (n : int) : var = MemoryValNew (i.label, n) in
  let ret = Var i.label in
  match i.instr with
  | Nop -> state
  | MemorySize -> { state with vstack = ret :: state.vstack }
  | Drop -> { state with vstack = drop 1 state.vstack }
  | Select -> { state with vstack = ret :: (drop 3 state.vstack) }
  | LocalGet l -> { state with vstack = get l state.locals :: state.vstack }
  | LocalSet l -> { state with vstack = drop 1 state.vstack; locals = set l state.locals (List.hd_exn state.vstack) }
  | LocalTee l -> { state with locals = set l state.locals (List.hd_exn state.vstack) }
  | GlobalGet g -> { state with vstack = get g state.globals :: state.vstack }
  | GlobalSet g -> { state with globals = set g state.globals (List.hd_exn state.vstack) }
  | Const _ -> { state with vstack = ret :: state.vstack }
  | Compare _ -> { state with vstack = ret :: (drop 2 state.vstack) }
  | Binary _ -> { state with vstack = ret :: (drop 2 state.vstack) }
  | Test _ -> { state with vstack = ret :: (drop 1 state.vstack) }
  | Convert _ -> { state with vstack = ret :: (drop 1 state.vstack) }
  | Load _ ->
    (* We look up the value at the address *)
    (* TODO: load can load one byte, or more. For now, this is entirely encoded in the constraints *)
    (* let v = VarMap.find_exn state.memory (key 0) in *)
    { state with vstack = ret :: (drop 1 state.vstack) }
  | Store { typ = I32; sz = None; _ } ->
    { state with vstack = drop 2 state.vstack;
                 memory = List.fold_left
                     ~init:state.memory
                     ~f:(fun acc (k, v) -> VarMap.update acc k ~f:(fun _ -> v))
                     [(key 0, value 0); (key 1, value 1); (key 2, value 2); (key 3, value 3)] }
  | Store { typ = I64; sz = None; _ } ->
    { state with vstack = drop 2 state.vstack;
                 memory = List.fold_left
                     ~init:state.memory
                     ~f:(fun acc (k, v) -> VarMap.update acc k ~f:(fun _ -> v))
                     [(key 0, value 0); (key 1, value 1); (key 2, value 2); (key 3, value 3);
                      (key 4, value 4); (key 5, value 5); (key 6, value 6); (key 7, value 7)] }
  | Store { typ = I32; sz = Some (Pack8, _); _ } ->
    { state with vstack = drop 2 state.vstack;
                 memory = VarMap.update state.memory (key 0) ~f:(fun _ -> value 0) }
  | Store _ -> failwith "unsupported memory op"

let control_instr_transfer (_module_ : Wasm_module.t) (cfg : Cfg.t) (i : Instr.control Instr.labelled) (state : state) : result =
  let ret = Var i.label in
  match i.instr with
  | Call ((arity_in, arity_out), _)
  | CallIndirect ((arity_in, arity_out), _) ->
    Simple { state with vstack = (if arity_out = 1 then [ret] else []) @ (drop arity_in state.vstack) }
  | Br _ -> Simple state
  | BrIf _ | If _ ->
    Branch ({ state with vstack = drop 1 state.vstack },
            { state with vstack = drop 1 state.vstack })
  | Return -> Simple (if List.length cfg.return_types = 1 then
                          { state with vstack = [ret] }
                        else
                          { state with vstack = [] })
  | Unreachable -> Simple { state with vstack = [] }
  | _ -> failwith (Printf.sprintf "Unsupported control instruction: %s" (Instr.control_to_string i.instr))


let merge_flows (_module_ : Wasm_module.t) (cfg : Cfg.t) (block : Basic_block.t) (states : (int * state) list) : state =
  let counter = ref 0 in
  let new_var () : var =
    let res = Merge (block.idx, !counter) in
    counter := !counter + 1;
    res in
  let substitute (l1 : var list) (l2 : var list) : var list =
    List.map2_exn  l1 l2 ~f:(fun x y -> if Stdlib.(x = y) then x else new_var()) in
  match states with
  | [] -> init_state cfg
  | (_, s) :: [] -> s
  | (_, s1) :: rest -> begin match block.content with
      | ControlMerge ->
        List.fold_left rest
          ~init:s1
          ~f:(fun acc (_, s) ->
              assert (VarMap.equal (Stdlib.(=)) acc.memory s.memory); (* TODO: could be different in practice, and in that case we need to introduce new vars for their join *)
              { acc with vstack = substitute acc.vstack s.vstack;
                         locals = substitute acc.locals s.locals;
                         globals = substitute acc.globals s.globals })
      | _ -> failwith (Printf.sprintf "Invalid block with multiple input states: %d" block.idx)
      end
