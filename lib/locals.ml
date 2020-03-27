open Core_kernel

type t = Value.t list
[@@deriving sexp, compare, yojson]
let get_local (l : t) (x : int) : Value.t = List.nth_exn l x
let set_local (l : t) (x : int) (v' : Value.t) : t = List.mapi l ~f:(fun i v -> if i = x then v' else v)

let to_string (locals : t) : string =
  String.concat ~sep:", " (List.mapi locals ~f:(fun i v -> Printf.sprintf "%d: %s" i (Value.to_string v)))
