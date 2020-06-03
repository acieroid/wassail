open Core_kernel


(** Primitive values, which are either i32 or i64 *)
module T = struct
  type t =
    | I32 of int32
    | I64 of int64
    (* TODO: f32 & f64 *)
  [@@deriving sexp, compare]
end

include T
include Comparator.Make(T)

let to_string (v : t) : string = match v with
  | I32 n -> Int32.to_string n
  | I64 n -> Int64.to_string n

let is_zero (v : t) : bool = match v with
  | I32 0l | I64 0L -> true
  | _ -> false

let is_one (v : t) : bool = match v with
  | I32 1l | I64 1L -> true
  | _ -> false

let is_positive (v : t) : bool = match v with
  | I32 n -> Int32.(n > 0l)
  | I64 n -> Int64.(n > 0L)

let byte_of (v : t) (byte : int) = match (v, byte) with
  | (I32 x, 0) -> I32 Int32.(x land  0xFFl)
  | (I32 x, 1) -> I32 Int32.(shift_left (x land 0xFF00l) 8)
  | (I32 x, 2) -> I32 Int32.(shift_left (x land 0xFF0000l) 16)
  | (I32 x, 3) -> I32 Int32.(shift_left (x land 0xFF000000l) 24)
  | (I64 x, 0) -> I64 Int64.(x land 0xFFL)
  | (I64 x, 1) -> I64 Int64.(shift_left (x land 0xFF00L) 8)
  | (I64 x, 2) -> I64 Int64.(shift_left (x land 0xFF0000L) 16)
  | (I64 x, 3) -> I64 Int64.(shift_left (x land 0xFF000000L) 24)
  | _ -> failwith "invalid call to byte_of"

let is (v : t) (n : int) : bool = match v with
  | I32 n' -> Int32.(n' = of_int_exn n)
  | I64 n' -> Int64.(n' = of_int_exn n)

(** Returns zero of type t *)
let zero_of_t (t : Type.t) : t = match t with
  | I32 -> I32 0l
  | I64 -> I64 0L
  | _ -> failwith "unsupported type"

(** Returns zero in the same type as v *)
let zero_of_same_t (v : t) : t = match v with
  | I32 _ -> I32 0l
  | I64 _ -> I64 0L

let of_int_t (v : t) (n : int) : t = match v with
  | I32 _ -> I32 (Int32.of_int_exn n)
  | I64 _ -> I64 (Int64.of_int_exn n)

let of_int (n : int) : t = I32 (Int32.of_int_exn n)

let add_int (v : t) (n : int) : t = match v with
  | I32 x -> I32 Int32.(x + (of_int_exn n))
  | I64 x -> I64 Int64.(x + (of_int_exn n))

let min (v1 : t) (v2 : t) : t  = match (v1, v2) with
  | (I32 x, I32 y) -> I32 (Int32.(min x y))
  | (I64 x, I64 y) -> I64 (Int64.(min x y))
  | _ -> failwith "comparing wrong values"

let max (v1 : t) (v2 : t) : t  = match (v1, v2) with
  | (I32 x, I32 y) -> I32 (Int32.(max x y))
  | (I64 x, I64 y) -> I64 (Int64.(max x y))
  | _ -> failwith "comparing wrong values"

(** Lift a wasm operation to PrimValue *)
let lift_wasm_bin
    (op32 : Wasm.I32.t -> Wasm.I32.t -> Wasm.I32.t)
    (op64 : Wasm.I64.t -> Wasm.I64.t -> Wasm.I64.t)
  : t -> t -> t = fun v1 v2 -> match (v1, v2) with
  | (I32 x, I32 y) -> I32 (op32 x y)
  | (I64 x, I64 y) -> I64 (op64 x y)
  | _ -> failwith "wrong types"

let lift_wasm_un
    (op32 : Wasm.I32.t -> Wasm.I32.t)
    (op64 : Wasm.I64.t -> Wasm.I64.t)
  : t -> t = function
  | I32 x -> I32 (op32 x)
  | I64 x -> I64 (op64 x)

let lift_wasm_test
    (op32 : Wasm.I32.t -> bool)
    (op64 : Wasm.I64.t -> bool)
  : t -> bool = function
  | I32 x -> op32 x
  | I64 x -> op64 x

let lift_wasm_rel
    (op32 : Wasm.I32.t -> Wasm.I32.t -> bool)
    (op64 : Wasm.I64.t -> Wasm.I64.t -> bool)
  : t -> t -> bool = fun v1 v2 -> match (v1, v2) with
  | (I32 x, I32 y) -> op32 x y
  | (I64 x, I64 y) -> op64 x y
  | _ -> failwith "wrong types"

let add = lift_wasm_bin Wasm.I32.add Wasm.I64.add
let sub = lift_wasm_bin Wasm.I32.sub Wasm.I64.sub
let mul = lift_wasm_bin Wasm.I32.mul Wasm.I64.mul
let div_s = lift_wasm_bin Wasm.I32.div_s Wasm.I64.div_s
let div_u = lift_wasm_bin Wasm.I32.div_u Wasm.I64.div_u
let rem_s = lift_wasm_bin Wasm.I32.rem_s Wasm.I64.rem_s
let rem_u = lift_wasm_bin Wasm.I32.rem_u Wasm.I64.rem_u
let and_ = lift_wasm_bin Wasm.I32.and_ Wasm.I64.and_
let or_ = lift_wasm_bin Wasm.I32.or_ Wasm.I64.or_
let xor = lift_wasm_bin Wasm.I32.xor Wasm.I64.xor
let shl = lift_wasm_bin Wasm.I32.shl Wasm.I64.shl
let shr_s = lift_wasm_bin Wasm.I32.shr_s Wasm.I64.shr_s
let shr_u = lift_wasm_bin Wasm.I32.shr_u Wasm.I64.shr_u
let rotl = lift_wasm_bin Wasm.I32.rotl Wasm.I64.rotl
let rotr = lift_wasm_bin Wasm.I32.rotr Wasm.I64.rotr

let clz = lift_wasm_un Wasm.I32.clz Wasm.I64.clz
let popcnt = lift_wasm_un Wasm.I32.popcnt Wasm.I64.popcnt

(* TODO extend_s is a wasm op that is not defined here*)

let eqz = lift_wasm_test Wasm.I32.eqz Wasm.I64.eqz

let eq = lift_wasm_rel Wasm.I32.eq Wasm.I64.eq
let ne = lift_wasm_rel Wasm.I32.ne Wasm.I64.ne
let lt_s = lift_wasm_rel Wasm.I32.lt_s Wasm.I64.lt_s
let lt_u = lift_wasm_rel Wasm.I32.lt_u Wasm.I64.lt_u
let le_s = lift_wasm_rel Wasm.I32.le_s Wasm.I64.le_s
let le_u = lift_wasm_rel Wasm.I32.le_u Wasm.I64.le_u
let gt_s = lift_wasm_rel Wasm.I32.gt_s Wasm.I64.gt_s
let gt_u = lift_wasm_rel Wasm.I32.gt_u Wasm.I64.gt_u
let ge_s = lift_wasm_rel Wasm.I32.ge_s Wasm.I64.ge_s
let ge_u = lift_wasm_rel Wasm.I32.ge_u Wasm.I64.ge_u

