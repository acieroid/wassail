open Core_kernel

module T = struct
  type t =
    | Var of int
    | Local of int (* nth local *)
    | Global of int (* nth global *)
    | MemoryKey of int * int
    | MemoryVal of int * int
    | MemoryValNew of int * int
    | Merge of int * int
  [@@deriving sexp, compare, equal]
end
include T

let to_string (v : t) : string = match v with
  | Var n -> Printf.sprintf "i%d" n
  | Local n -> Printf.sprintf "l%d" n
  | Global n -> Printf.sprintf "g%d" n
  | MemoryKey (l, v) -> Printf.sprintf "mk%d_%d" l v
  | MemoryVal (l, v) -> Printf.sprintf "mv%d_%d" l v
  | MemoryValNew (l, v) -> Printf.sprintf "mvnew%d_%d" l v
  | Merge (idx, n) -> Printf.sprintf "merge%d_%d" idx n

module Map = Map.Make(T)
module Set = Set.Make(T)
