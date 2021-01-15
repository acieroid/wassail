open Core_kernel

module T = struct
  type t =
    | Var of Instr.Label.t (* variable resulting from an instruction *)
    | Local of int (* nth local *)
    | Global of int (* nth global *)
    | Merge of int * int (* merge variable (n, m) meaning from nth function, mth merge variable *)
    | Return (* return variable *)
    | Hole (* only temporary holes meant to be replaced by merge variables *)
    | Const of Prim_value.t
  [@@deriving sexp, compare, equal]
  type with_offset = t * int
  [@@deriving sexp, compare, equal]

  let to_string (v : t) : string = match v with
  | Var l -> Printf.sprintf "i%s" (Instr.Label.to_string l)
  | Local n -> Printf.sprintf "l%d" n
  | Global n -> Printf.sprintf "g%d" n
  | Merge (idx, n) -> Printf.sprintf "m%d_%d" idx n
  | Return -> "ret"
  | Hole -> "_"
  | Const n ->  Prim_value.to_string n

  let list_to_string (vs : t list) : string = String.concat ~sep: ", " (List.map vs ~f:to_string)
end
include T

module OffsetMap = struct
  include Map.Make(struct
      type t = with_offset
      [@@deriving sexp, compare, equal]
    end)
  let map_vars (m : T.t t) ~(f: T.t -> T.t) : T.t t =
    of_alist_exn (List.map (to_alist m)
                    ~f:(fun ((k, offset), v) ->
                        ((f k, offset), f v)))
end

module Map = struct
  include Map.Make(T)
  let to_string (m : 'a t) (f : 'a -> string) : string =
    String.concat ~sep:", " (List.map (to_alist m) ~f:(fun (k, v) -> Printf.sprintf "%s -> %s" (to_string k) (f v)))
end
module Set = struct
  module S = struct
    include Set.Make(T)
    let to_string (v : t) : string =
      String.concat ~sep:"," (List.map ~f:to_string (to_list v))
  end
  include S
  include Test.Helpers(S)

  let of_option (v : T.t option) : t =
    match v with
    | Some v -> singleton v
    | None -> empty
end
