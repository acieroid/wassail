open Core_kernel
open Wasm

(** These are the values (and their abstractions) *)
module T = struct
  type operator =
    | Plus | Minus | Times
  [@@deriving sexp, compare]
  type t =
    | Bottom
    | Const of int32
    | Interval of int32 * int32 (* [a,b] *)
    | LeftOpenInterval of int32 (* ]-inf,a] *)
    | RightOpenInterval of int32 (* [a,+inf[ *)
    | OpenInterval (* ]-inf,+inf[ *)
    | Parameter of int (* p0 *)
    | Global of int (* g0 *)
    | Op of operator * t * t (* g0-16 *)
    | Deref of t (* *g0 *)
    (* XXX: values are actually i32/i64/f32/f64, but we only support i32 *)
  [@@deriving sexp, compare]

end
include T
include Comparator.Make(T)

let of_wasm (v : Values.value) : t =
  match v with
  | I32 x -> Const x
  | I64 _ -> failwith "unsupported type: I64"
  | F32 _ -> failwith "unsupported type: F32"
  | F64 _ -> failwith "unsupported type: F64"

let rec to_string (v : t) : string = match v with
  | Bottom -> "bottom"
  | Const n -> (Int32.to_string n)
  | Interval (a, b) -> Printf.sprintf "[%s,%s]" (Int32.to_string a) (Int32.to_string b)
  | LeftOpenInterval b -> Printf.sprintf "]-inf,%s]" (Int32.to_string b)
  | RightOpenInterval a -> Printf.sprintf "[%s,+inf[" (Int32.to_string a)
  | OpenInterval -> "]-inf,+inf["
  | Parameter i -> Printf.sprintf "p%d" i
  | Global i -> Printf.sprintf "g%d" i
  | Op (op, left, right) -> Printf.sprintf "%s%s%s" (to_string left) (begin match op with
      | Plus -> "+"
      | Minus -> "-"
      | Times -> "*"
    end) (to_string right)
  | Deref v -> Printf.sprintf "*%s" (to_string v)

(* TODO: substitute param / globals upon calls *)


(** Joins two values together *)
let join (v1 : t) (v2 : t) : t =
  match (v1, v2) with
  | (Bottom, _) -> v2
  | (_, Bottom) -> v1
  | (Const n1, Const n2) when n1 = n2 -> Const n1
  | (Const n1, Const n2) -> Interval (min n1 n2, max n1 n2)
  | _ -> failwith "TODO: more cases when joining abstract values"

(** Joins two value lists together, assuming they have the same length *)
let join_vlist_exn (v1 : t list) (v2 : t list) : t list =
  List.map2_exn v1 v2 ~f:join

(* TODO: maybe these functions should take the memory as argument, and return an updated version of it? *)
let is_zero (v : t) =
  match v with
  | Bottom -> false
  | Const 0l -> true
  | Const _ -> false
  | Interval (a, b) -> a <= Int32.zero && b >= Int32.zero
  | LeftOpenInterval b -> b >= Int32.zero
  | RightOpenInterval a -> a <= Int32.zero
  | OpenInterval -> true
  | Parameter _ -> true
  | Global _ -> true
  | Op _ -> failwith "TODO"
  | Deref _ -> failwith "TODO"

let is_not_zero (v : t) =
  match v with
  | Bottom -> false
  | Const 0l -> false
  | Const _ -> true
  | Interval (a, b) -> not (a = 0l && b = 0l)
  | LeftOpenInterval _ -> true
  | RightOpenInterval _ -> true
  | OpenInterval -> true
  | Parameter _ -> true
  | Global _ -> true
  | Op _ -> failwith "TODO"
  | Deref _ -> failwith "TODO"

let bottom : t = Bottom

let zero (t : Type.t) : t =
  match t with
  | I32Type -> Const 0l
  | _ -> failwith "unsupported type"

let parameter (i : int) : t = Parameter i
let global (i : int) : t = Global i
let deref (addr : t) : t = Deref addr
let const (n : int32) : t = Const n
let bool : t = Interval (0l, 1l)
let top (source : string) : t = Logging.warn_imprecise source; OpenInterval

let list_to_string (l : t list) : string =
  String.concat ~sep:", " (List.map l ~f:to_string)

