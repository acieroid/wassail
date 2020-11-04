open Core_kernel

module T = struct
  type t =
    | Var of int (* variable resulting from an instruction *)
    | Local of int (* nth local *)
    | Global of int (* nth global *)
    | Merge of int * int (* merge variable (n, m) meaning from nth function, mth merge variable *)
    | Return (* return variable *)
    | Hole (* only temporary holes meant to be replaced by merge variables *)
    | Const of Prim_value.t
  [@@deriving sexp, compare, equal]
  type with_offset = t * int
  [@@deriving sexp, compare, equal]
end
include T

let to_string (v : t) : string = match v with
  | Var n -> Printf.sprintf "i%d" n
  | Local n -> Printf.sprintf "l%d" n
  | Global n -> Printf.sprintf "g%d" n
  | Merge (idx, n) -> Printf.sprintf "m%d_%d" idx n
  | Return -> "ret"
  | Hole -> "_"
  | Const n ->  Prim_value.to_string n

let list_to_string (vs : t list) : string = String.concat ~sep: ", " (List.map vs ~f:to_string)

module OffsetMap = Map.Make(struct
    type t = with_offset
    [@@deriving sexp, compare, equal]
  end)
module Map = Map.Make(T)
module Set = struct
  include Set.Make(T)
  let of_option (v : T.t option) : t =
    match v with
    | Some v -> singleton v
    | None -> empty
  let to_string (v : t) : string =
    String.concat ~sep:"," (List.map ~f:to_string (to_list v))
end
