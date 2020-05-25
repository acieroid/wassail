open Core_kernel

(** A state of the wasm VM *)
type state = {
  vstack : Vstack.t; (** The value stack *)
  locals : Locals.t; (** The locals *)
  globals : Globals.t; (** The globals *)
  memory : Memory.t; (** The linear memory *)
}
[@@deriving sexp, compare]

let to_string (s : state) : string =
  Printf.sprintf "{vstack: [%s],\n locals: [%s],\n globals: [%s],\n heap: %s\n}"
    (Vstack.to_string s.vstack)
    (Locals.to_string s.locals)
    (Globals.to_string s.globals)
    (Memory.to_string s.memory)

let init (args : Value.t list) (locals : Type.t list) (globals : Globals.t) (memory : Memory.t) = {
  vstack = [];
  locals = args @ (List.map locals ~f:Value.zero);
  globals = globals;
  memory = memory;
}

let join (s1 : state) (s2 : state) : state = {
  vstack =
    if List.length s1.vstack <> List.length s2.vstack then
      (* Different length, probably one has not been analyzed yet. Just take the maximal one *)
      if List.length s1.vstack > List.length s2.vstack then begin
        assert Stdlib.(s2.vstack = []);
        s1.vstack
      end else begin
        assert Stdlib.(s1.vstack = []);
        s2.vstack
      end
    else
      List.map2_exn s1.vstack s2.vstack ~f:Value.join;
  locals = List.map2_exn s1.locals s2.locals ~f:Value.join;
  globals = Globals.join s1.globals s2.globals;
  memory = Memory.join s1.memory s2.memory;
}

let join_opt (s1 : state) (s2 : state option) : state option =
  match s2 with
  | None -> Some s1
  | Some s2 -> Some (join s1 s2)
