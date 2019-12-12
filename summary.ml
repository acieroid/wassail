(* A function summary *)
open Core_kernel
open Helpers

(* For now it just models the number of arguments taken by the function, and the result on the vstack (in practice, either 0 or 1 value) *)
type t = {
  nargs : int;
  result : Value.t list;
}
let to_string (s : t) : string =
  Printf.sprintf "Sum(%d, %s)" s.nargs
    (String.concat ~sep:", " (List.map s.result ~f:Value.to_string))

(* Constructs a summary given a CFG and a domain state resulting from that CFG *)
let make (cfg : Cfg.t) (state : Domain.state) : t = {
  nargs = fst cfg.arity;
  result = List.take state.vstack (snd cfg.arity);
}

(* Constructs an empty bottom summary given a CFG *)
let bottom (cfg : Cfg.t) : t = {
  nargs = fst cfg.arity;
  result = List.init (snd cfg.arity) ~f:(fun _ -> Value.bottom);
}

(* Apply the summary to a state, updating the vstack as if the function was
   called, AND updating the set of called functions *)
let apply (sum : t) (fidx : Var.t) (st : Domain.state) : Domain.state =
  { st with
    vstack = sum.result @ (List.drop st.vstack sum.nargs );
    calls = IntMap.update st.calls fidx ~f:(function
        | None -> List.take st.vstack sum.nargs
        | Some vs -> Value.join_vlist_exn vs (List.take st.vstack sum.nargs))
  }
