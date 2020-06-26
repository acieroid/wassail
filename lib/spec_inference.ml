open Core_kernel
open Helpers

(** The state is a specification of the runtime components *)
type state = {
  vstack : string list;
  locals : string list;
  globals : string list;
  memory : string StringMap.t;
}
[@@deriving sexp, compare]

let init_state (cfg : Cfg.t) : state = {
  vstack = []; (* the vstack is initially empty *)
  locals = (List.mapi cfg.arg_types ~f:(fun i _ -> Printf.sprintf "p%d" i)) @ (List.mapi cfg.local_types ~f:(fun i _ -> Printf.sprintf "l%d" i));
  globals = List.mapi cfg.global_types ~f:(fun i _ -> Printf.sprintf "g%d" i);
  memory = StringMap.empty; (* TODO *)
}

let state_to_string (s : state) : string =
  Printf.sprintf "{\nvstack: [%s]\nlocals: [%s]\nglobals: [%s]\nmemory: [%s]}"
    (String.concat ~sep:"," s.vstack)
    (String.concat ~sep:"," s.locals)
    (String.concat ~sep:"," s.globals)
    (String.concat ~sep:"," (List.map (StringMap.to_alist s.memory) ~f:(fun (k, v) -> Printf.sprintf "%s: %s" k v)))

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
let drop (n : int) (vstack : string list) =
  assert (List.length vstack >= n);
  List.drop vstack n

let get (n : int) (l : string list) = List.nth_exn l n

let set (n : int) (l : string list) (v : string) = List.mapi l ~f:(fun i v' -> if i = n then v else v')

let data_instr_transfer (i : Instr.data Instr.labelled) (state : state) : state =
  let ret = Printf.sprintf "i%d" i.label in
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
  | Load _ -> { state with vstack = ret :: (drop 1 state.vstack) }
  | Store _ -> { state with vstack = (drop 2 state.vstack) }

let control_instr_transfer (cfg : Cfg.t) (i : Instr.control Instr.labelled) (state : state) : result =
  let ret = Printf.sprintf "i%d" i.label in
  match i.instr with
  | Call ((arity_in, arity_out), _)
  | CallIndirect ((arity_in, arity_out), _) ->
    Simple { state with vstack = (if arity_out = 1 then [ret] else []) @ (drop arity_in state.vstack) }
  | Br _ -> Simple state
  | BrIf _ | If _ ->
    Branch ({ state with vstack = drop 1 state.vstack },
            { state with vstack = drop 1 state.vstack })
  | Return -> Simple (if List.length cfg.return_types = 1 then
                          { state with vstack = ["ret"] } (* ret is a special var for the return value *)
                        else
                          { state with vstack = [] })
  | Unreachable -> Simple { state with vstack = [] }
  | _ -> failwith (Printf.sprintf "Unsupported control instruction: %s" (Instr.control_to_string i.instr))

let transfer (_module_ : Wasm_module.t) (cfg : Cfg.t) (b : Basic_block.t) (state : state) : result =
  match b.content with
  | Data instrs ->
    Simple (List.fold_left instrs ~init:state ~f:(fun prestate instr -> data_instr_transfer instr prestate))
  | Control instr -> control_instr_transfer cfg instr state
  | Nothing -> Simple state
  | ControlMerge -> Simple state

let merge_flows (_module_ : Wasm_module.t) (cfg : Cfg.t) (block : Basic_block.t) (states : state list) : state =
  let counter = ref 0 in
  let new_var () : string =
    let res = Printf.sprintf "m%d_%d" block.idx !counter in
    counter := !counter + 1;
    res in
  let substitute (l1 : string list) (l2 : string list) : string list =
    List.map2_exn  l1 l2 ~f:(fun x y -> if Stdlib.(x = y) then x else new_var()) in
  match states with
  | [] -> init_state cfg
  | s :: [] -> s
  | s1 :: rest -> begin match block.content with
      | ControlMerge ->
        List.fold_left rest
          ~init:s1
          ~f:(fun acc s ->
              assert (StringMap.equal (Stdlib.(=)) acc.memory s.memory);
              { acc with vstack = substitute acc.vstack s.vstack;
                         locals = substitute acc.locals s.locals;
                         globals = substitute acc.globals s.globals })
      | _ -> failwith (Printf.sprintf "Invalid block with multiple input states: %d" block.idx)
      end
