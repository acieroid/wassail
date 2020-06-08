open Core_kernel

(* There are also a finite number of globals *)
type t = Value.t list
[@@deriving sexp, compare]
let get_global (g : t) (x : int) : Value.t = List.nth_exn g x
let set_global (g : t) (x : int) (v' : Value.t) : t = List.mapi g ~f:(fun i v -> if i = x then v' else v)

let to_string (globals : t) : string =
  String.concat ~sep:", " (List.mapi globals ~f:(fun i v -> Printf.sprintf "%d: %s" i (Value.to_string v)))

let join (g1 : t) (g2 : t) : t =
  Value.join_vlist_exn g1 g2
