(* A function summary *)
open Core_kernel

(* For now it just models the number of arguments taken by the function, and the result on the vstack (in practice, either 0 or 1 value) *)
type t = {
  nargs : int;
  typ: Type.t list * Type.t list;
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
  nargs = List.length cfg.arg_types;
  typ = (cfg.arg_types, cfg.return_types);
  result = List.take state.vstack (List.length cfg.return_types);
  globals = state.globals;
  memory = state.memory;
}

(* Constructs an empty bottom summary given a CFG *)
let bottom (cfg : Cfg.t) (module_ : Wasm_module.t) : t = {
  nargs = List.length cfg.arg_types;
  typ = [], [];
  result = List.map cfg.return_types ~f:Value.bottom;
  globals = List.mapi module_.globals ~f:(fun i g -> Value.global g.typ i);
  memory = Memory.initial
}

(* Apply the summary to a state, updating the vstack as if the function was
   called, AND updating the set of called functions *)
let apply (sum : t) (fidx : Var.t) (st : Domain.state) (module_ : Wasm_module.t) : Domain.state =
  Printf.printf "[fidx: %d] Applying summary %s to state %s!" fidx (to_string sum) (Domain.to_string st);
  let map = List.foldi st.globals ~init:(List.foldi (List.rev st.vstack) ~init:Value.ValueValueMap.ValueMap.empty
                                           ~f:(fun i acc v -> Value.ValueValueMap.ValueMap.add_exn acc
                                                  ~key:(Value.parameter I32 i).value
                                                  ~data:v.value))
      ~f:(fun i acc v -> Value.ValueValueMap.ValueMap.add_exn acc
             ~key:(Value.global (List.nth_exn module_.globals i).typ i).value
             ~data:v.value) in
  { st with
    vstack = sum.result @ (List.map (List.drop st.vstack sum.nargs) ~f:(fun v -> Value.adapt v map));
    memory = Memory.join st.memory (Memory.adapt sum.memory map);
    globals = Globals.adapt sum.globals map;
  }
