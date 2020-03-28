open Core_kernel
open Helpers

type state = {
  vstack : Vstack.t;
  locals : Locals.t;
  globals : Globals.t;
  memory : Memory.t;
  calls : ValueListIntMap.t; (* A map of function called, from function index to parameters given *)
}
[@@deriving sexp, compare, yojson]

let to_string (s : state) : string =
  Printf.sprintf "{vstack: [%s],\n locals: [%s],\n globals: [%s]\n, heap: %s\n}"
    (Vstack.to_string s.vstack)
    (Locals.to_string s.locals)
    (Globals.to_string s.globals)
    (Memory.to_string s.memory)

let init (args : Value.t list) (nlocals : int) (globals : Globals.t) (memory : Memory.t) = {
  vstack = [];
  locals = args @ (List.init nlocals ~f:(fun _ -> Value.zero));
  globals = globals;
  memory = memory;
  (* The list of calls is initially empty *)
  calls = ValueListIntMap.IntMap.empty;
}

let join (s1 : state) (s2 : state) : state = {
  vstack =
    if List.length s1.vstack <> List.length s2.vstack then
      (* Different length, probably one has not been analyzed yet. Just take the maximal one *)
      if List.length s1.vstack > List.length s2.vstack then begin
        assert (s2.vstack = []);
        s1.vstack
      end else begin
        assert (s1.vstack = []);
        s2.vstack
      end
    else
      List.map2_exn s1.vstack s2.vstack ~f:Value.join;
  locals = List.map2_exn s1.locals s2.locals ~f:Value.join;
  globals = Globals.join s1.globals s2.globals;
  memory = Memory.join s1.memory s2.memory;
  calls = ValueListIntMap.IntMap.merge s1.calls s2.calls ~f:(fun ~key:_ data -> match data with
      | `Both (a, b) -> Some (Value.join_vlist_exn a b)
      | `Left a -> Some a
      | `Right b -> Some b)
}
let join_opt (s1 : state option) (s2 : state option) : state option =
  match (s1, s2) with
  | Some s1, Some s2 -> Some (join s1 s2)
  | Some s1, None -> Some s1
  | None, Some s2 -> Some s2
  | None, None -> None
