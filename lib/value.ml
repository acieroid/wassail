open Core_kernel

module T = struct
  (** A value is just represented by its (apron) variable *)
  type t = string
  [@@deriving sexp, compare, equal]
end
include T
include Comparator.Make(T)

let to_string (v : t) : string = v

let join (v1 : t) (v2 : t) : t =
  if Stdlib.(v1 = v2) then
    v1
  else
    failwith (Printf.sprintf "Value.join trying to join two distinct variables: %s and %s" v1 v2)

let join_vlist_exn (l1 : t list) (l2 : t list) : t list =
  List.map2_exn l1 l2 ~f:join
