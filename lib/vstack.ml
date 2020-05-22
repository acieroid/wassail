open Core_kernel

(* The value stack is abstracted as a stack of values. It cannot grow unbounded so that is safe *)
type t = Value.t list
[@@deriving sexp, compare]
let pop (vstack : t) : (Value.t * t) =
  match vstack with
  | hd :: tl -> (hd, tl)
  | _ -> failwith "Invalid empty vstack"

let to_string (vstack : t) : string =
  String.concat ~sep:", " (List.map vstack ~f:Value.to_string)
