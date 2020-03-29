(* A function summary *)
open Core_kernel
open Helpers

(* For now it just models the number of arguments taken by the function, and the result on the vstack (in practice, either 0 or 1 value) *)
type t = {
  nargs : int;
  result : Value.t list;
  globals: Globals.t;
  memory: Memory.t;
}
let to_string (s : t) : string =
  Printf.sprintf "Summary(params: %d, vstack: %s, mem: %s, globals: %s)"
    s.nargs
    (String.concat ~sep:", " (List.map s.result ~f:Value.to_string))
    (Memory.to_string s.memory)
    (Globals.to_string s.globals)

(* Constructs a summary given a CFG and a domain state resulting from that CFG *)
let make (cfg : Cfg.t) (state : Domain.state) : t = {
  nargs = fst cfg.arity;
  result = List.take state.vstack (snd cfg.arity);
  globals = state.globals;
  memory = state.memory;
}

(* Constructs an empty bottom summary given a CFG *)
let bottom (nglobals : int) (cfg : Cfg.t) : t = {
  nargs = fst cfg.arity;
  result = List.init (snd cfg.arity) ~f:(fun _ -> Value.bottom);
  globals = List.init nglobals ~f:Value.global;
  memory = Memory.initial
}

(* Apply the summary to a state, updating the vstack as if the function was
   called, AND updating the set of called functions *)
let apply (sum : t) (fidx : Var.t) (st : Domain.state) : Domain.state =
  Printf.printf "[fidx: %d] Applying summary %s to state %s!" fidx (to_string sum) (Domain.to_string st);
  let map = List.foldi st.globals ~init:(List.foldi (List.rev st.vstack) ~init:Value.ValueValueMap.ValueMap.empty
                                           ~f:(fun i acc v -> Value.ValueValueMap.ValueMap.add_exn acc
                                                  ~key:(Value.parameter i)
                                                  ~data:v))
      ~f:(fun i acc v -> Value.ValueValueMap.ValueMap.add_exn acc
             ~key:(Value.global i)
             ~data:v) in
  { st with
    vstack = sum.result @ (List.map (List.drop st.vstack sum.nargs) ~f:(fun v -> Value.adapt v map));
    calls = ValueListIntMap.IntMap.update st.calls fidx ~f:(function
        | None -> List.take st.vstack sum.nargs
        | Some vs -> Value.join_vlist_exn vs (List.take st.vstack sum.nargs));
    memory = Memory.join st.memory (Memory.adapt sum.memory map);
    globals = Globals.adapt sum.globals map; (* TODO: similarly, adapt globals *)
  }
