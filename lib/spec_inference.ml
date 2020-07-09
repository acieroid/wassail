open Core_kernel
open Helpers

type summary = unit
let init_summaries _ = ()

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

(** Returns all variables contained in the memory of a state *)
let memvars (s : state) : var list =
  (List.concat (List.map (VarMap.to_alist s.memory)
                             ~f:(fun (k, v) -> [k; v])))

(** Returns all the variables contained in the state *)
let vars_of (s : state) : VarSet.t =
  VarSet.union (VarSet.of_list s.vstack)
    (VarSet.union (VarSet.of_list s.locals)
       (VarSet.union (VarSet.of_list s.globals)
          (VarSet.of_list (memvars s))))

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
      ~f:(fun (v1, v2, eq) -> if not eq then Some (v1, v2) else None) in
  let fmap (m1 : var VarMap.t) (m2 : var VarMap.t) : (var * var) list =
    assert (Stdlib.(=) (VarMap.keys m1) (VarMap.keys m2)); (* Memory keys never change (assumption) *)
    List.filter_map (VarMap.keys m1) ~f:(fun k ->
        let v1 = VarMap.find_exn m1 k in
        let v2 = VarMap.find_exn m2 k in
        if equal_var v1 v2 then None else Some (v1, v2)) in
  (f s1.vstack s2.vstack) @ (f s1.locals s2.locals) @ (f s1.globals s2.globals) @ (fmap s1.memory s2.memory)

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
                  | Load _op | Store _op ->
                    VarMap.add_exn ~key:(key i.label 0) ~data:(value i.label 0) m
                    (*
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
                    end*)
                  | _ -> m)
          | Control _ | ControlMerge -> m)
  end;
}

let bottom_state (cfg : Cfg.t) : state = init_state cfg (* TODO? *)

let state_to_string (s : state) : string =
  Printf.sprintf "{\nvstack: [%s]\nlocals: [%s]\nglobals: [%s]\nmemory: [%s]\n}"
    (String.concat ~sep:", " (List.map s.vstack ~f:var_to_string))
    (String.concat ~sep:", " (List.map s.locals ~f:var_to_string))
    (String.concat ~sep:", " (List.map s.globals ~f:var_to_string))
    (String.concat ~sep:", " (List.map (VarMap.to_alist s.memory) ~f:(fun (k, v) -> Printf.sprintf "%s: %s" (var_to_string k) (var_to_string v))))


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
  (* Printf.printf "transfer for %s with state %s\n" (Instr.data_to_string i.instr) (state_to_string state); *)
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
  | Store _ ->
    { state with vstack = drop 2 state.vstack;
                 memory = VarMap.update state.memory (key 0) ~f:(fun _ -> (value 0)) }
(*  | Store { typ = I32; sz = None; _ } ->
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
    | Store _ -> failwith "unsupported memory op" *)

let control_instr_transfer (_module_ : Wasm_module.t) (cfg : Cfg.t) (i : Instr.control Instr.labelled) (state : state) : result =
  let ret = Var i.label in
  match i.instr with
  | Call ((arity_in, arity_out), _) ->
    Simple { state with vstack = (if arity_out = 1 then [ret] else []) @ (drop arity_in state.vstack) }
  | CallIndirect ((arity_in, arity_out), _) ->
    (* Like call, but reads the function index from the vstack *)
    Simple { state with vstack = (if arity_out = 1 then [ret] else []) @ (drop (arity_in+1) state.vstack) }
  | Br _ -> Simple state
  | BrIf _ | If _ ->
    Branch ({ state with vstack = drop 1 state.vstack },
            { state with vstack = drop 1 state.vstack })
  | Return -> Simple (if List.length cfg.return_types = 1 then
                          { state with vstack = [List.hd_exn state.vstack] }
                        else
                          { state with vstack = [] })
  | Unreachable -> Simple { state with vstack = [] }
  | _ -> failwith (Printf.sprintf "Unsupported control instruction: %s" (Instr.control_to_string i.instr))


let merge (_module_ : Wasm_module.t) (cfg : Cfg.t) (block : Basic_block.t) (states : state list) : state =
  let counter = ref 0 in
  let new_var () : var =
    let res = Merge (block.idx, !counter) in
    counter := !counter + 1;
    res in
  match block.content with
  | ControlMerge -> 
    let st = match states with
      | [] -> init_state cfg
      | s :: _ -> s (* ensures the vstack has the right number of elements *)in
    (* TODO: for now we replace every variable. Less variables could be produced by avoiding replacing the same ones, but this is more tricky than it appears *)
    { vstack = List.map st.vstack ~f:(fun _ -> new_var());
      locals = List.map st.locals ~f:(fun _ -> new_var());
      globals = List.map st.globals ~f:(fun _ -> new_var());
      memory = VarMap.map st.memory ~f:(fun _ -> new_var()) }
  | _ ->
    (* not a control-flow merge, should only be one predecessor (or none if it is the entry point) *)
    begin match states with
      | [] -> init_state cfg
      | s :: [] -> s
      | _ ->  failwith (Printf.sprintf "Invalid block with multiple input states: %d" block.idx)
    end

let merge_flows (module_ : Wasm_module.t) (cfg : Cfg.t) (block : Basic_block.t) (states : (int * state) list) : state =
  begin match states with
  | _ :: _ :: _ -> begin match block.content with
      | ControlMerge -> ()
      | _ -> failwith (Printf.sprintf "Invalid block with multiple input states: %d" block.idx)
    end
  | _ -> ()
  end;
  merge module_ cfg block (List.map ~f:snd states)

let join_state (_module_ : Wasm_module.t) (_cfg : Cfg.t) (_block : Basic_block.t) (s1 : state) (s2 : state) : state =
  Printf.printf "[block %d] joining %s with %s\n" _block.idx( state_to_string s1) (state_to_string s2);
  assert (compare_state s1 s2 = 0);
  s1

module type SPEC_DATA = sig
  val instr_data : (state * state) IntMap.t
  val block_data : (state * state) IntMap.t
end

module type SPEC = sig
  val vars : var list
  val pre : Instr.label -> state
  val post : Instr.label -> state
  val pre_block : int -> state
  val post_block : int -> state
  val ret : Instr.label -> var

  val get_nth : var list -> int -> var
  val pop : var list -> var
  val pop2 : var list -> var * var
  val pop3 : var list -> var * var * var
end

module Spec (Data : SPEC_DATA) : SPEC = struct
  let vars : var list = VarSet.to_list (VarSet.union
                                          (vars Data.block_data)
                                          (vars Data.instr_data))
  let pre (label : Instr.label) : state = fst (IntMap.find_exn Data.instr_data label)
  let post (label : Instr.label) : state = snd (IntMap.find_exn Data.instr_data label)

  let pre_block (idx : int) : state = fst (IntMap.find_exn Data.block_data idx)
  let post_block (idx : int) : state = snd (IntMap.find_exn Data.block_data idx)

  let ret (label : Instr.label) : var =
    let spec = snd (IntMap.find_exn Data.instr_data label) in
    List.hd_exn spec.vstack

  let get_nth (l : var list) (x : int) : var = List.nth_exn l x

  let pop (vstack : var list) : var =
    match vstack with
    | hd :: _ -> hd
    | _ -> failwith "Invalid vstack"
  let pop2 (vstack : var list) : (var * var) =
    match vstack with
    | x :: y :: _ -> (x, y)
    | _ -> failwith "Invalid vstack"
  let pop3 (vstack : var list) : (var * var * var) =
    match vstack with
    | x :: y :: z :: _ -> (x, y, z)
    | _ -> failwith "Invalid vstack"

end
