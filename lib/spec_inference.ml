open Core_kernel
open Helpers

type summary = unit
let init_summaries _ = ()
let summary _ _ = ()

(** The state is a specification of the runtime components *)
type state = {
  vstack : Var.t list;
  locals : Var.t list;
  globals : Var.t list;
  memory : Var.t Var.Map.t;
}
[@@deriving sexp, compare, equal]

(** Returns all variables contained in the memory of a state *)
let memvars (s : state) : Var.t list =
  (List.concat (List.map (Var.Map.to_alist s.memory)
                             ~f:(fun (k, v) -> [k; v])))

(** Returns all the variables contained in the state *)
let vars_of (s : state) : Var.Set.t =
  Var.Set.union (Var.Set.of_list s.vstack)
    (Var.Set.union (Var.Set.of_list s.locals)
       (Var.Set.union (Var.Set.of_list s.globals)
          (Var.Set.of_list (memvars s))))

(** Returns all the variables contained in the spec map *)
let vars (data : (state * state) IntMap.t) : Var.Set.t =
  List.fold_left (IntMap.to_alist data)
    ~init:Var.Set.empty
    ~f:(fun acc (_, (pre, post)) ->
        Var.Set.union acc (Var.Set.union (vars_of pre) (vars_of post)))

let init_state (cfg : Cfg.t) : state = {
  vstack = []; (* the vstack is initially empty *)
  locals = List.mapi (cfg.arg_types @ cfg.local_types) ~f:(fun i _ -> Var.Local i);
  globals = List.mapi cfg.global_types ~f:(fun i _ -> Var.Global i);
  memory = begin
    let key (label : Instr.label) (n : int) : Var.t = MemoryKey (label, n) in
    let value (label : Instr.label) (n : int) : Var.t = MemoryVal (label, n) in
    List.fold_left (IntMap.data cfg.basic_blocks)
      ~init:Var.Map.empty
      ~f:(fun m block -> match block.content with
          | Data instrs ->
            List.fold_left instrs
              ~init:m
              ~f:(fun m i -> match i.instr with
                  | Load _op | Store _op ->
                    Var.Map.add_exn ~key:(key i.label 0) ~data:(value i.label 0) m
                    (*
                    begin match op with
                      | { typ = I32; sz = None; _ } ->
                        (* I32 is 4 bytes *)
                        Var.Map.add_exn ~key:(key i.label 0) ~data:(value i.label 0)
                          (Var.Map.add_exn ~key:(key i.label 1) ~data:(value i.label 1)
                             (Var.Map.add_exn ~key:(key i.label 2) ~data:(value i.label 2)
                                (Var.Map.add_exn ~key:(key i.label 3) ~data:(value i.label 3)
                                   m)))
                      | { typ = I64; sz = None; _ } ->
                        (* I64 is 8 bytes *)
                        Var.Map.add_exn ~key:(key i.label 0) ~data:(value i.label 0)
                          (Var.Map.add_exn ~key:(key i.label 1) ~data:(value i.label 1)
                             (Var.Map.add_exn ~key:(key i.label 2) ~data:(value i.label 2)
                                (Var.Map.add_exn ~key:(key i.label 3) ~data:(value i.label 3)
                                   (Var.Map.add_exn ~key:(key i.label 4) ~data:(value i.label 4)
                                      (Var.Map.add_exn ~key:(key i.label 5) ~data:(value i.label 5)
                                         (Var.Map.add_exn ~key:(key i.label 6) ~data:(value i.label 6)
                                            (Var.Map.add_exn ~key:(key i.label 7) ~data:(value i.label 7)
                                               m)))))))

                      | { typ = I32; sz = Some (Pack8, _); _ } ->
                        (* only one byte *)
                        Var.Map.add_exn ~key:(key i.label 0) ~data:(value i.label 0) m
                      | _ -> failwith "unsupported memory op" 
                    end*)
                  | _ -> m)
          | Control _ | ControlMerge -> m)
  end;
}

let bottom_state (cfg : Cfg.t) : state = init_state cfg (* TODO? *)

let state_to_string (s : state) : string =
  Printf.sprintf "{\nvstack: [%s]\nlocals: [%s]\nglobals: [%s]\nmemory: [%s]\n}"
    (String.concat ~sep:", " (List.map s.vstack ~f:Var.to_string))
    (String.concat ~sep:", " (List.map s.locals ~f:Var.to_string))
    (String.concat ~sep:", " (List.map s.globals ~f:Var.to_string))
    (String.concat ~sep:", " (List.map (Var.Map.to_alist s.memory) ~f:(fun (k, v) -> Printf.sprintf "%s: %s" (Var.to_string k) (Var.to_string v))))


let extract_different_vars (s1 : state) (s2 : state) : (Var.t * Var.t) list =
  let f (l1 : Var.t list) (l2 : Var.t list) : (Var.t * Var.t) list =
    assert (List.length l1 = List.length l2);
    List.filter_map (List.map2_exn l1 l2 ~f:(fun v1 v2 -> (v1, v2, Var.equal v1 v2)))
      ~f:(fun (v1, v2, eq) -> if not eq then Some (v1, v2) else None) in
  let fvstack (l1 : Var.t list) (l2 : Var.t list) : (Var.t * Var.t) list =
    (* Like f, but only checks a prefix.
       For example, it can sometimes happen that on one path we have [x, y] as the vstack, and another we have [y].
       We can safely assume that if the code has passed validation, then y will never be used.
       Hence, it is safe to treat the first vstack as if it was [x] *)
    let min_size = min (List.length l1) (List.length l2) in
    f (List.take l1 min_size) (List.take l2 min_size) in
  let fmap (m1 : Var.t Var.Map.t) (m2 : Var.t Var.Map.t) : (Var.t * Var.t) list =
    assert (Stdlib.(=) (Var.Map.keys m1) (Var.Map.keys m2)); (* Memory keys never change (assumption) *)
    List.filter_map (Var.Map.keys m1) ~f:(fun k ->
        let v1 = Var.Map.find_exn m1 k in
        let v2 = Var.Map.find_exn m2 k in
        if Var.equal v1 v2 then None else Some (v1, v2)) in
  (fvstack s1.vstack s2.vstack) @ (f s1.locals s2.locals) @ (f s1.globals s2.globals) @ (fmap s1.memory s2.memory)

(** Like List.drop, but raises an exception if the list does not contain enough element *)
let drop (n : int) (vstack : Var.t list) =
  assert (List.length vstack >= n);
  List.drop vstack n

let get (n : int) (l : Var.t list) = List.nth_exn l n

let set (n : int) (l : Var.t list) (v : Var.t) = List.mapi l ~f:(fun i v' -> if i = n then v else v')

let data_instr_transfer (_module_ : Wasm_module.t) (_cfg : Cfg.t) (i : Instr.data Instr.labelled) (state : state) : state =
  let key (n : int) : Var.t = MemoryKey (i.label, n) in
  let value (n : int) : Var.t = MemoryValNew (i.label, n) in
  let ret = Var.Var i.label in
  (* Printf.printf "transfer for %s with state %s\n" (Instr.data_to_string i.instr) (state_to_string state); *)
  match i.instr with
  | Nop -> state
  | MemorySize -> { state with vstack = ret :: state.vstack }
  | MemoryGrow -> { state with vstack = ret :: drop 1 state.vstack }
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
  | Unary _ -> { state with vstack = ret :: (drop 1 state.vstack) }
  | Test _ -> { state with vstack = ret :: (drop 1 state.vstack) }
  | Convert _ -> { state with vstack = ret :: (drop 1 state.vstack) }
  | Load _ ->
    (* We look up the value at the address *)
    (* TODO: load can load one byte, or more. For now, this is entirely encoded in the constraints *)
    (* let v = Var.Map.find_exn state.memory (key 0) in *)
    { state with vstack = ret :: (drop 1 state.vstack) }
  | Store _ ->
    { state with vstack = drop 2 state.vstack;
                 memory = Var.Map.update state.memory (key 0) ~f:(fun _ -> (value 0)) }
(*  | Store { typ = I32; sz = None; _ } ->
    { state with vstack = drop 2 state.vstack;
                 memory = List.fold_left
                     ~init:state.memory
                     ~f:(fun acc (k, v) -> Var.Map.update acc k ~f:(fun _ -> v))
                     [(key 0, value 0); (key 1, value 1); (key 2, value 2); (key 3, value 3)] }
  | Store { typ = I64; sz = None; _ } ->
    { state with vstack = drop 2 state.vstack;
                 memory = List.fold_left
                     ~init:state.memory
                     ~f:(fun acc (k, v) -> Var.Map.update acc k ~f:(fun _ -> v))
                     [(key 0, value 0); (key 1, value 1); (key 2, value 2); (key 3, value 3);
                      (key 4, value 4); (key 5, value 5); (key 6, value 6); (key 7, value 7)] }
  | Store { typ = I32; sz = Some (Pack8, _); _ } ->
    { state with vstack = drop 2 state.vstack;
                 memory = Var.Map.update state.memory (key 0) ~f:(fun _ -> value 0) }
    | Store _ -> failwith "unsupported memory op" *)

let control_instr_transfer (_module_ : Wasm_module.t) (cfg : Cfg.t) (i : Instr.control Instr.labelled) (state : state) : [`Simple of state | `Branch of state * state] =
  let ret = Var.Var i.label in
  match i.instr with
  | Call ((arity_in, arity_out), _) ->
    `Simple { state with vstack = (if arity_out = 1 then [ret] else []) @ (drop arity_in state.vstack) }
  | CallIndirect ((arity_in, arity_out), _) ->
    (* Like call, but reads the function index from the vstack *)
    `Simple { state with vstack = (if arity_out = 1 then [ret] else []) @ (drop (arity_in+1) state.vstack) }
  | Br _ -> `Simple state
  | BrIf _ | If _ ->
    `Branch ({ state with vstack = drop 1 state.vstack },
             { state with vstack = drop 1 state.vstack })
  | BrTable _ -> `Simple { state with vstack = drop 1 state.vstack }
  | Return -> `Simple (if List.length cfg.return_types = 1 then
                          { state with vstack = [List.hd_exn state.vstack] }
                        else
                          { state with vstack = [] })
  | Unreachable -> `Simple { state with vstack = [] }
  | _ -> failwith (Printf.sprintf "Unsupported control instruction: %s" (Instr.control_to_string i.instr))


let merge (_module_ : Wasm_module.t) (cfg : Cfg.t) (block : Basic_block.t) (states : state list) : state =
  let counter = ref 0 in
  let new_var () : Var.t =
    let res = Var.Merge (block.idx, !counter) in
    counter := !counter + 1;
    res in
  match block.content with
  | ControlMerge ->
    (* Ensures the vstack has the right number of elements.
       To do so, we take the max of the length of the vstacks that reach this point.
       Basically, max is needed because it could be that some flows have not been analyzed yet *)
    let st = match List.max_elt states ~compare:(fun s1 s2 -> Stdlib.compare (List.length s1.vstack) (List.length s2.vstack)) with
      | None -> init_state cfg
      | Some s -> s in
    (* TODO: for now we replace every variable. Less variables could be produced by avoiding replacing the same ones, but this is more tricky than it appears *)
    { vstack = List.map st.vstack ~f:(fun _ -> new_var());
      locals = List.map st.locals ~f:(fun _ -> new_var());
      globals = List.map st.globals ~f:(fun _ -> new_var());
      memory = Var.Map.map st.memory ~f:(fun _ -> new_var()) }
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

let join_state (s1 : state) (s2 : state) : state =
  assert (compare_state s1 s2 = 0);
  s1

(* No widening *)
let widen _ s2 = s2

module type SPEC_DATA = sig
  val instr_data : unit -> (state * state) IntMap.t
  val block_data : unit -> (state * state) IntMap.t
end

module type SPEC = sig
  val vars : unit -> Var.t list
  val pre : Instr.label -> state
  val post : Instr.label -> state
  val pre_block : int -> state
  val post_block : int -> state
  val ret : Instr.label -> Var.t

  val get_nth : Var.t list -> int -> Var.t
  val pop : Var.t list -> Var.t
  val pop2 : Var.t list -> Var.t * Var.t
  val pop3 : Var.t list -> Var.t * Var.t * Var.t
end

module Spec (Data : SPEC_DATA) : SPEC = struct
  let vars () : Var.t list = Var.Set.to_list (Var.Set.union
                                             (vars (Data.block_data ()))
                                             (vars (Data.instr_data ())))
  let pre (label : Instr.label) : state = fst (IntMap.find_exn (Data.instr_data ()) label)
  let post (label : Instr.label) : state = snd (IntMap.find_exn (Data.instr_data ()) label)

  let pre_block (idx : int) : state = fst (IntMap.find_exn (Data.block_data ()) idx)
  let post_block (idx : int) : state = snd (IntMap.find_exn (Data.block_data ()) idx)

  let ret (label : Instr.label) : Var.t =
    let spec = snd (IntMap.find_exn (Data.instr_data ()) label) in
    List.hd_exn spec.vstack

  let get_nth (l : Var.t list) (x : int) : Var.t = List.nth_exn l x

  let pop (vstack : Var.t list) : Var.t =
    match vstack with
    | hd :: _ -> hd
    | _ -> failwith "Invalid vstack"
  let pop2 (vstack : Var.t list) : (Var.t * Var.t) =
    match vstack with
    | x :: y :: _ -> (x, y)
    | _ -> failwith "Invalid vstack"
  let pop3 (vstack : Var.t list) : (Var.t * Var.t * Var.t) =
    match vstack with
    | x :: y :: z :: _ -> (x, y, z)
    | _ -> failwith "Invalid vstack"

end

