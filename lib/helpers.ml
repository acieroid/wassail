open Core_kernel

(** Integers *)
module I = struct
  type t = int
  [@@deriving sexp, compare, equal]
end

(** Strings *)
module S = struct
  type t = string
  [@@deriving sexp, compare, equal]
end

(** Sets of integers *)
module IntSet = Set.Make(I)

(** Maps of integers *)
module IntMap = Map.Make(I)

(** Sets of strings *)
module StringSet = Set.Make(S)

(** Maps of strings *)
module StringMap = Map.Make(S)


(** Get the nth element of a list *)
let get_nth (l : 'a list) (n : int) : Var.t = List.nth_exn l n

(** Pop one element from a list *)
let pop (vstack : 'a list) : 'a =
  match vstack with
  | hd :: _ -> hd
  | _ -> failwith "Invalid vstack"

(** Pop two elements from a list *)
let pop2 (vstack : 'a list) : ('a * 'a) =
  match vstack with
  | x :: y :: _ -> (x, y)
  | _ -> failwith "Invalid vstack"

(** Pop 3 elements from a list *)
let pop3 (vstack : 'a list) : ('a * 'a * 'a) =
    match vstack with
    | x :: y :: z :: _ -> (x, y, z)
    | _ -> failwith "Invalid vstack"

