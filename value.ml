open Core
open Wasm
open Helpers

(** These are the values (and their abstractions) *)
module T = struct
  type value =
    | Bottom
    | Const of int32
    | Int
    (* XXX: values are actually i32/i64/f32/f64 *)
  [@@deriving sexp, compare]

  type t = {
    value : value;
    (* The possible sources of this value.
       One source is a pair of function id, function argument number *)
    sources : IntPairSet.t;
  }
  [@@deriving sexp, compare]
end
include T
include Comparator.Make(T)

let of_wasm (v : Values.value) : t =
  match v with
  | I32 x -> { value = Const x ; sources = IntPairSet.empty }
  | I64 _ -> failwith "unsupported type: I64"
  | F32 _ -> failwith "unsupported type: F32"
  | F64 _ -> failwith "unsupported type: F64"

let sources_to_string (sources : IntPairSet.t) : string =
  String.concat ~sep:"," (List.map (IntPairSet.to_list sources) ~f:(fun (faddr,arg) -> Printf.sprintf "f%d.arg%d" faddr arg))

let to_string (v : t) : string =
  match v.value with
  | Bottom -> "bottom"
  | Const n -> Printf.sprintf "%s@%s" (Int32.to_string n) (sources_to_string v.sources)
  | Int -> Printf.sprintf "int@%s" (sources_to_string v.sources)

(** Joins two values together *)
let join (v1 : t) (v2 : t) : t =
  match (v1.value, v2.value) with
  | (Bottom, _) -> v2
  | (_, Bottom) -> v1
  | (Const n1, Const n2) when n1 = n2 -> {
      value = Const n1;
      sources = IntPairSet.union v1.sources v2.sources
    }
  | (_, _) -> {
      value = Int;
      sources = IntPairSet.union v1.sources v2.sources
    }

(** Joins two value lists together, assuming they have the same length *)
let join_vlist_exn (v1 : t list) (v2 : t list) : t list =
  List.map2_exn v1 v2 ~f:join

let is_zero (v : t) =
  match v.value with
  | Bottom -> false
  | Const 0l -> true
  | Const _ -> false
  | Int -> true

let is_not_zero (v : t) =
  match v.value with
  | Bottom -> false
  | Const 0l -> false
  | _ -> true

let bottom : t = {
  value = Bottom;
  sources = IntPairSet.empty
}

let zero (t : Type.t) : t =
  match t with
  | I32Type -> {
      value = Const 0l;
      sources = IntPairSet.empty
    }
  | _ -> failwith "unsupported type"

let top (t : Type.t) (source : (int * int)) : t =
  match t with
  | I32Type -> {
      value = Int;
      sources = IntPairSet.singleton source
    }
  | _ -> failwith "unsupported type"

let top_no_source (t : Type.t) : t =
  match t with
  | I32Type -> {
      value = Int;
      sources = IntPairSet.empty
    }
  | _ -> failwith "unsupported type"

let list_to_string (l : t list) : string =
  String.concat ~sep:", " (List.map l ~f:to_string)

