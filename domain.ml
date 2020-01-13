open Core_kernel
open Helpers
(* The value stack is abstracted as a stack of values. It cannot grow unbounded so that is safe *)
type vstack = Value.t list
[@@deriving sexp, compare]
let pop (vstack : vstack) : (Value.t * vstack) =
  match vstack with
  | hd :: tl -> (hd, tl)
  | _ -> failwith "Invalid empty vstack"
(* Similarly, locals are finite for any function *)
type locals = Value.t list
[@@deriving sexp, compare]
let get_local (l : locals) (x : int) : Value.t = List.nth_exn l x
let set_local (l : locals) (x : int) (v' : Value.t) : locals = List.mapi l ~f:(fun i v -> if i = x then v' else v)

(* There are also a finite number of globals *)
type globals = Value.t list
[@@deriving sexp, compare]
let get_global (g : globals) (x : int) : Value.t = List.nth_exn g x
let set_global (g : globals) (x : int) (v' : Value.t) : globals = List.mapi g ~f:(fun i v -> if i = x then v' else v)

(* Representation of the memory using separation logic *)
type byte =
  | ByteInValue of (Value.t * int) (* (val, byte_position) *)
[@@deriving sexp, compare]
let byte_to_string (b : byte) : string = match b with
  | ByteInValue (v, b) -> begin match v.value with
      | Value.Bottom -> "Bottom"
      | Value.Const n -> Printf.sprintf "%d[%d]" (Option.value_exn (Int32.to_int n)) b
      | Value.Int -> Printf.sprintf "%s[%d]" (Value.sources_to_string v.sources) b
    end

type formula =
  | Emp
  | MapsTo of byte * byte
  | Star of formula * formula
[@@deriving sexp, compare]
let rec formula_to_string (f : formula) : string = match f with
  | Emp -> "emp"
  | MapsTo (b1, b2) -> Printf.sprintf "%s -> %s" (byte_to_string b1) (byte_to_string b2)
  | Star (f1, f2) -> Printf.sprintf "%s * %s" (formula_to_string f1) (formula_to_string f2)

let rec contains_maps_to (f : formula) (b1 : byte) (b2 : byte) = match f with
  | Emp -> false
  | MapsTo (b1', b2') -> compare_byte b1 b1' = 0 && compare_byte b2 b2' = 0
  | Star (f1, f2) -> contains_maps_to f1 b1 b2 || contains_maps_to f2 b1 b2

let rec maps_to (f : formula) (b : byte) =
  match f with
  | Emp -> None
  | MapsTo (b1, b2) -> if compare_byte b b1 = 0 then Some b2 else None
  | Star (f1, f2) -> begin match maps_to f1 b with
      | Some b2 -> Some b2
      | None -> maps_to f2 b
    end
let formula_mapsto_4bytes (f : formula) (i : Value.t) (offset : int) : Value.t option =
  match (maps_to f (ByteInValue (i, offset)),
         maps_to f (ByteInValue (i, offset + 1)),
         maps_to f (ByteInValue (i, offset + 2)),
         maps_to f (ByteInValue (i, offset + 3))) with
  | Some (ByteInValue (c0, _)), Some (ByteInValue (c1, _)), Some (ByteInValue (c2, _)), Some (ByteInValue (c3, _))
    when Value.compare c0 c1 = 0 && Value.compare c1 c2 = 0 && Value.compare c2 c3 = 0 -> Some c0
  | None, None, None, None -> None
  | _ -> failwith "TODO: formula_mapsto_4bytes"
let rec star (f1 : formula) (f2 : formula) = match f1 with
  | Emp -> f2
  | MapsTo (b1, b2) ->
    if contains_maps_to f2 b1 b2 then
      f2
    else
      Star (MapsTo (b1, b2), f2)
  | Star (f11, f12) ->
    star f11 (star f12 f2)

type memory = formula
[@@deriving sexp, compare]
let memory_to_string (m : memory) : string = formula_to_string m

type state = {
  vstack : vstack;
  locals : locals;
  globals : globals;
  memory : memory;
  calls : (Value.t list) IntMap.t; (* A map of function called, from function index to parameters given *)
}
[@@deriving sexp, compare]

let vstack_to_string (vstack : vstack) : string =
  String.concat ~sep:", " (List.map vstack ~f:Value.to_string)
let locals_to_string (locals : locals) : string =
  String.concat ~sep:", " (List.mapi locals ~f:(fun i v -> Printf.sprintf "%d: %s" i (Value.to_string v)))
let globals_to_string (globals : globals) : string =
  String.concat ~sep:", " (List.mapi globals ~f:(fun i v -> Printf.sprintf "%d: %s" i (Value.to_string v)))

let to_string (s : state) : string =
  Printf.sprintf "{vstack: [%s],\n locals: [%s],\n globals: [%s]\n, heap: %s\n}"
    (vstack_to_string s.vstack)
    (locals_to_string s.locals)
    (globals_to_string s.globals)
    (memory_to_string s.memory)

let init (args : Value.t list) (nlocals : int) (globals : globals) (memory : memory) = {
  vstack = [];
  locals = args @ (List.init nlocals ~f:(fun _ -> Value.zero I32Type));
  globals = globals;
  memory = memory;
  (* The list of calls is initially empty *)
  calls = IntMap.empty;
}
let join_globals (g1 : globals) (g2 : globals) : globals =
  Value.join_vlist_exn g1 g2
let join_memory (m1 : memory) (m2 : memory) : memory = match (m1, m2) with
  | Emp, x -> x
  | x, Emp -> x
  | x, y -> if compare_formula x y = 0 then
      x
    else
      (* TODO: implement join fully *)
      failwith (Printf.sprintf "failure to join memories %s and %s" (formula_to_string x) (formula_to_string y))

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
  globals = join_globals s1.globals s2.globals;
  memory = join_memory s1.memory s2.memory;
  calls = IntMap.merge s1.calls s2.calls ~f:(fun ~key:_ data -> match data with
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
