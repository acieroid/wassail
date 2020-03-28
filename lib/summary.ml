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
  Printf.printf "Applying summary %s to state %s!" (to_string sum) (Domain.to_string st);
  { st with
    vstack = sum.result @ (List.drop st.vstack sum.nargs );
    calls = ValueListIntMap.IntMap.update st.calls fidx ~f:(function
        | None -> List.take st.vstack sum.nargs
        | Some vs -> Value.join_vlist_exn vs (List.take st.vstack sum.nargs));
    memory = Memory.join st.memory sum.memory; (* TODO: adapt sum.memory in two ways: replace e.g., g0 by the value of g0 from st.globals; similar for parameters: p0 is the argument to the function, so replace p0 by its value (taken from the stack); finally: what about dereferences? These changes have to be propagated inside Op as well *)
    globals = sum.globals; (* TODO: similarly, adapt globals *)
  }
