open Core_kernel
open Wasm

module PrimValue = struct
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
end

(** These are the values (and their abstractions) *)
module T = struct
  type bytepos = int (* in practice, between 0 and 7 *)
  [@@deriving sexp, compare]
  type operator =
    | Plus | Minus | Times
    | Lt | LtE | Gt | GtE | Eq
    | And | Or
  [@@deriving sexp, compare]
  type symbolic =
    | Parameter of int (* p0 *)
    | Global of int (* g0 *)
    | Op of operator * value * value (* g0-16 *)
    | Bytes4 of value * value * value * value (* b0b1b2b3 *)
    | Byte of value * int (* x@0 is byte 0 of value x *)
    | Deref of value (* *g0 *)
    | Const of PrimValue.t
  and value =
    | Bottom
    | Interval of symbolic * symbolic (* [a,b] *)
    | LeftOpenInterval of symbolic (* ]-inf,a] *)
    | RightOpenInterval of symbolic (* [a,+inf[ *)
    | OpenInterval (* ]-inf,+inf[ *)
    | Symbolic of symbolic
  and byte = value * int (* a byte in a value *)
  [@@deriving sexp, compare]
  module ValueT = struct
    type t = value
    [@@deriving sexp, compare]
  end
  type t = {
    value: value;
    typ: Type.t
  }
  [@@deriving sexp, compare]
end
include T
include Comparator.Make(T)

let of_wasm (v : Values.value) : t =
  match v with
  | I32 x -> { value = Symbolic (Const (I32 x)); typ = Type.I32 }
  | I64 x -> { value = Symbolic (Const (I64 x)); typ = Type.I64 }
  | F32 _ -> failwith "unsupported type: F32"
  | F64 _ -> failwith "unsupported type: F64"

let rec value_to_string (v : value) : string = match v with
  | Bottom -> "bottom"
  | Interval (a, b) -> Printf.sprintf "[%s,%s]" (symbolic_to_string a) (symbolic_to_string b)
  | LeftOpenInterval b -> Printf.sprintf "]-inf,%s]" (symbolic_to_string b)
  | RightOpenInterval a -> Printf.sprintf "[%s,+inf[" (symbolic_to_string a)
  | OpenInterval -> "T"
  | Symbolic sym -> symbolic_to_string sym
and symbolic_to_string (v : symbolic) : string = match v with
  | Const n -> PrimValue.to_string n
  | Parameter i -> Printf.sprintf "p%d" i
  | Global i -> Printf.sprintf "g%d" i
  | Op (op, left, right) -> Printf.sprintf "%s%s%s" (value_to_string left) (begin match op with
      | Plus -> "+"
      | Minus -> "-"
      | Times -> "*"
      | Lt -> "<"
      | LtE -> "<="
      | Gt -> ">"
      | GtE -> ">="
      | Eq -> "="
      | And -> "&"
      | Or -> "|"
    end) (value_to_string right)
  | Deref v -> Printf.sprintf "*%s" (value_to_string v)
  | Bytes4 (b0, b1, b2, b3) -> Printf.sprintf "bytes[%s,%s,%s,%s]" (value_to_string b0) (value_to_string b1) (value_to_string b2) (value_to_string b3)
  | Byte (v, b) -> Printf.sprintf "%s@%d" (value_to_string v) b
let to_string (v : t) : string = value_to_string v.value

let rec simplify_symbolic (sym : symbolic) : symbolic =
  match sym with
  | (Op (Plus, a, Symbolic (Const z))) when PrimValue.is_zero z->
    (* a+0 is handled in simplify *)
    (Op (Plus, a, Symbolic (Const z)))
  | (Op (Plus, (Symbolic (Op (Minus, a, Symbolic (Const x)))), Symbolic (Const y))) when PrimValue.eq x y ->
    (* (a-x)+x = a *)
    (Op (Plus, a, Symbolic (Const (PrimValue.zero_of_same_t x))))
  | (Op (Plus, (Symbolic (Op (Minus, a, Symbolic (Const x)))), Symbolic (Const y))) when PrimValue.gt_s x y ->
    (* (a-x)+y when x > y = a-(x-y) *)
    simplify_symbolic (Op (Minus, simplify_value a, Symbolic (Const (PrimValue.sub x y))))
  | (Op (Plus, (Symbolic (Op (Minus, a, Symbolic (Const x)))), Symbolic (Const y))) when PrimValue.lt_s x y ->
    (* (a-x)+y when y > x = a+(y-x)*)
    simplify_symbolic (Op (Plus, simplify_value a, Symbolic (Const (PrimValue.sub y x))))
  | (Op (Plus, (Symbolic (Op (Plus, a, Symbolic (Const x)))), Symbolic (Const y))) ->
    (* (a+x)+y = a + (x+y) *)
    simplify_symbolic (Op (Plus, simplify_value a, Symbolic (Const (PrimValue.add x y))))
  | (Op (Minus, (Symbolic (Op (Minus, a, Symbolic (Const x)))), Symbolic (Const y))) ->
    (* (a-x)-y = a-(x+y) *)
    simplify_symbolic (Op (Minus, simplify_value a, Symbolic (Const (PrimValue.add x y))))
  | Op (Eq, Symbolic (Op (Lt, a, b)), Symbolic (Const (I32 0l))) ->
    (* a<b=0 = a>b *)
    Op (GtE, a, b)
  (* TODO: many more cases *)
  | (Global _) | (Parameter _) | Const _
  | Op (_, Symbolic (Global _), Symbolic (Const _))
  | Op (_, Symbolic (Parameter _), Symbolic (Const _)) ->
    (* These cases cannot be simplified *)
    sym
  | _ ->
    Logging.warn "cannot simplify" (Printf.sprintf "value %s" (symbolic_to_string sym));
    sym
and simplify_value (v : value) : value =
  match (match v with
      | Bottom -> Bottom
      | Interval (Const a, Const b) when PrimValue.eq a b -> Symbolic (Const a)
      | Interval (a, b) -> Interval (simplify_symbolic a, simplify_symbolic b)
      | LeftOpenInterval b -> LeftOpenInterval (simplify_symbolic b)
      | RightOpenInterval a -> RightOpenInterval (simplify_symbolic a)
      | OpenInterval -> OpenInterval
      | Symbolic sym -> Symbolic (simplify_symbolic sym))
  with
  | Symbolic (Op (Plus, a, Symbolic (Const z))) when PrimValue.is_zero z -> a
  | Symbolic (Op (Minus, a, Symbolic (Const z))) when PrimValue.is_zero z -> a
  | res -> res

let simplify (v : t) : t = { value = simplify_value v.value; typ = v.typ }

(** Checks if v1 subsumes v2 (i.e., v1 contains v2) *)
let value_subsumes (v1 : value) (v2 : value) : bool = match (v1, v2) with
  | _, _ when Stdlib.(v1 = v2) -> true
  | _, Bottom -> true
  | Bottom, _ -> false
  | Symbolic (Const n1), Symbolic (Const n2) -> PrimValue.eq n1 n2
  | Symbolic (Const n), Interval (Const a, Const b) -> PrimValue.(eq a b && eq a n)
  | Interval (Const a, Const b), Interval (Const a', Const b') -> PrimValue.(le_s a a' && ge_s b b')
  | Interval (Const a, Const b), Symbolic (Const n) -> PrimValue.(le_s a n && ge_s b n)
  | LeftOpenInterval (Const b), Symbolic (Const n) -> PrimValue.(ge_s b n)
  | LeftOpenInterval (Const b), Interval (_, Const b') -> PrimValue.(ge_s b b')
  | LeftOpenInterval (Const b), LeftOpenInterval (Const b') -> PrimValue.(ge_s b b')
  | RightOpenInterval (Const a), Symbolic (Const n) -> PrimValue.(le_s a n)
  | RightOpenInterval (Const a), Interval (Const a', _) -> PrimValue.(le_s a a')
  | RightOpenInterval (Const a), RightOpenInterval (Const a') -> PrimValue.(le_s a a')
  | OpenInterval, Symbolic (Const _) -> true
  | OpenInterval, Interval _ -> true
  | OpenInterval, LeftOpenInterval _ -> true
  | OpenInterval, RightOpenInterval _ -> true
  | OpenInterval, OpenInterval -> true
  | Symbolic (Op (_, Symbolic (Global i), Symbolic (Const x))), (* gi OP x *)
    Symbolic (Op (_, Symbolic (Global i'), Symbolic (Const x'))) (* gi' OP x' *)
    when i = i' ->
    PrimValue.eq x x'
  | Symbolic (Op (_, Symbolic (Parameter i), Symbolic (Const x))),
    Symbolic (Op (_, Symbolic (Parameter i'), Symbolic (Const x')))
    when i = i' ->
    PrimValue.eq x x'
  | Symbolic (Bytes4 (Symbolic (Deref OpenInterval),
                      Symbolic (Deref OpenInterval),
                      Symbolic (Deref OpenInterval),
                      Symbolic (Deref OpenInterval))), _ ->
    (* If this case ever applies, then we have definitely lost too much precision *)
    true
  | _, _ ->
    Logging.warn "SubsumesMightBeIncorrect" (Printf.sprintf "assuming %s does not subsume %s" (value_to_string v1) (value_to_string v2));
    (* Definitely not sound, these cases have to be investigated one by one *)
    false

let subsumes (v1 : t) (v2 : t) : bool = value_subsumes v1.value v2.value

let bottom (typ : Type.t) : t = { value = Bottom; typ = typ }
let i32_zero : t = { value = Symbolic (Const (I32 0l)); typ = I32 }
let i64_zero : t = { value = Symbolic (Const (I64 0L)); typ = I64 }
let i32_const (n : int32) : t = { value = Symbolic (Const (I32 n)); typ = I32 }
let i64_const (n : int64) : t = { value = Symbolic (Const (I64 n)); typ = I64 }
let zero (t : Type.t) : t = { value = Symbolic (Const (PrimValue.zero_of_t t)); typ = t }
let const (n : PrimValue.t) : t = match n with
  | I32 _ -> { value = Symbolic (Const n); typ = I32 }
  | I64 _ -> { value = Symbolic (Const n); typ = I64 }
let parameter (t : Type.t) (i : int) : t = { value = Symbolic (Parameter i); typ = t }
let global (i : int) : t = { value = Symbolic (Global i); typ = I32 } (* TODO: typ *)
let deref (addr : value) : t = { value = Symbolic (Deref addr); typ = I32 } (* TODO: typ *)
let bool : t = { value = Interval (Const (I32 0l), Const (I32 1l)); typ = I32 }
let top (source : string) : t = Logging.warn "TopCreated" (Printf.sprintf "Top value originating from: %s" source); { value = OpenInterval; typ = I32 } (* TODO: typ *)
let symbolic (t : Type.t) (sym : symbolic) : t = { value = Symbolic (simplify_symbolic sym); typ = t }
let list_to_string (l : t list) : string =
  String.concat ~sep:", " (List.map l ~f:to_string)

(** Joins two values together *)
let join (v1 : t) (v2 : t) : t =
  assert Stdlib.(v1.typ = v2.typ);
  let vres: value = match (v1.value, v2.value) with
  | (Bottom, _) -> v2.value
  | (_, Bottom) -> v1.value
  | (OpenInterval, _) | (_, OpenInterval) -> OpenInterval
  | (_, _) when Stdlib.(v1 = v2) -> v1.value 
  | (Symbolic (Const n1), Symbolic (Const n2)) when PrimValue.eq n1 n2 ->
    Symbolic (Const n1)
  | (Symbolic (Const n1), Symbolic (Const n2)) ->
    Interval (Const PrimValue.(min n1 n2), Const PrimValue.(max n1 n2))
  | (Symbolic (Const n), Interval (Const a, Const b)) -> Interval (Const PrimValue.(min a n), Const PrimValue.(max b n))
  | (Interval (Const a, Const b), Symbolic (Const n)) -> Interval (Const PrimValue.(min a n), Const PrimValue.(max b n))
  | (Interval (Const z, Const b), Interval (Const z', Const b')) when PrimValue.(is_zero z && is_zero z' && not (eq b b')) ->
    (* TODO: this is a very simple widening when the right bound is unstable *)
    RightOpenInterval (Const (PrimValue.zero_of_same_t z))
  | (Interval (Const a, Const b), Interval (Const a', Const b')) ->
    (* TODO: need widen to ensure convergence *)
    Interval (Const PrimValue.(min a a'), Const PrimValue.(max b b'))
  | (RightOpenInterval (Const a), RightOpenInterval (Const a')) -> RightOpenInterval (Const PrimValue.(min a a'))
  | (LeftOpenInterval (Const b), LeftOpenInterval (Const b')) -> LeftOpenInterval (Const PrimValue.(max b b'))
  | (Interval (Const a, _), RightOpenInterval (Const a')) -> RightOpenInterval (Const PrimValue.(min a a'))
  | (RightOpenInterval (Const a), Symbolic (Const c)) -> RightOpenInterval (Const PrimValue.(min a c))
  | (LeftOpenInterval (Const b), Symbolic (Const c)) -> LeftOpenInterval (Const PrimValue.(max b c))
  | (RightOpenInterval (Op (Plus, Symbolic x, Symbolic (Const _))), RightOpenInterval x') when (Stdlib.(=) x x') ->
    v2.value
  | (Symbolic (Const _), LeftOpenInterval (Parameter _))
  | (Symbolic (Const _), LeftOpenInterval (Op (_, Symbolic (Parameter _), _)))
  | (Symbolic (Const _), RightOpenInterval (Op (_, Symbolic (Parameter _), _)))
  | (Symbolic (Const _), RightOpenInterval (Parameter _)) ->
    Logging.warn "UnsoundAssumption" (Printf.sprintf "%s contains %s" (to_string v2) (to_string v1));
    v2.value
  | _ -> (top (Printf.sprintf "Value.join %s %s" (to_string v1) (to_string v2))).value in
  Logging.info (Printf.sprintf "join %s with %s gives %s" (to_string v1) (to_string v2) (value_to_string vres));
  { typ = v1.typ;
    value = vres }

(** Joins two value lists together, assuming they have the same length *)
let join_vlist_exn (v1 : t list) (v2 : t list) : t list =
  List.map2_exn v1 v2 ~f:join

(** Meet two values together *)
let meet (v1 : t) (v2 : t) : t =
  assert Stdlib.(v1.typ = v2.typ);
  let v = match (v1.value, v2.value) with
    | (Bottom, _)
    | (_, Bottom) -> Bottom
    | (_, _) when Stdlib.(v1 = v2) -> v1.value
    | (_, OpenInterval) -> v1.value
    | (OpenInterval, _) -> v2.value
  (* TODO:
     a) meet ]-inf,p2-1[ with [a,b].
        IF we know that p2-1>=a, then definitely we can say that the result is at least [a,p2-1]
     b) meet [p2,+inf] with [0,1] -> probably [0,1] is better here...
        But then we'll loop, and we'll have to meet [p2,+inf] with [0,2], then [0,3], then finally [0,+inf], hence [p2,+inf] is probably fine. Although just p2 is correct IN OUR EXAMPLE
 *)
  | _ ->
    Logging.warn "ImpreciseOperation" (Printf.sprintf "meet %s with %s" (to_string v1) (to_string v2));
    (* There are multiple "valid" choices here. We pick v1.
       There could be more precise choices however, examples:
       meet [0,1] [2,3] returns [0,1], but should return Bottom (not seen in practice)
       meet ]-inf,p0] [0,1] has no best result, as we don't know if p0<0... (seen in practice)
         -> we decide to keep the first value here, as we prefer abstract values with parameters in them
    *)
    v1.value
  in {
    value = v;
    typ = v1.typ
  }

(* TODO: maybe these functions should take the memory as argument, and return an updated version of it? *)
let is_zero (v : t) =
  match v.value with
  | Bottom -> false
  | Symbolic (Const z) when PrimValue.is_zero z -> true
  | Symbolic (Const _) -> false
  | Interval (Const a, Const b) -> PrimValue.(le_s a (PrimValue.zero_of_same_t a) && ge_s b (PrimValue.zero_of_same_t b))
  | Interval (_, Const b) -> PrimValue.ge_s b (PrimValue.zero_of_same_t b)
  | Interval (Const a, _) -> PrimValue.le_s a (PrimValue.zero_of_same_t a)
  | Interval _ -> true
  | LeftOpenInterval (Const b) -> PrimValue.ge_s b (PrimValue.zero_of_same_t b)
  | LeftOpenInterval _ -> true
  | RightOpenInterval (Const a) -> PrimValue.le_s a (PrimValue.zero_of_same_t a)
  | RightOpenInterval _ -> true
  | OpenInterval -> true
  | Symbolic _ -> true (* TODO: could be more precise here? Or not *)

let is_not_zero (v : t) =
  match v.value with
  | Bottom -> false
  | Symbolic (Const z) when PrimValue.is_zero z -> false
  | Symbolic (Const _) -> true
  | Interval (Const a, Const b) -> not PrimValue.(eq a (PrimValue.zero_of_same_t a) && eq b (PrimValue.zero_of_same_t b))
  | Interval _ -> true (* TODO could be more precise *)
  | LeftOpenInterval _ -> true
  | RightOpenInterval _ -> true
  | OpenInterval -> true
  | Symbolic _ -> true (* TODO: could be more precise here *)

let rec add_offset (v : value) (offset : int) : value =
  if offset = 0 then v else
    match v with
    | Bottom -> Bottom
    | Interval (Const a, Const b) -> Interval (Const PrimValue.(add_int a offset), Const PrimValue.(add_int b offset))
    | Interval (a, b) -> Interval (Op (Plus, Symbolic a, Symbolic (Const (PrimValue.of_int offset))), Op (Plus, Symbolic b, Symbolic (Const (PrimValue.of_int offset))))
    | LeftOpenInterval (Const b) -> LeftOpenInterval (Const PrimValue.(add_int b offset))
    | LeftOpenInterval b -> LeftOpenInterval (Op (Plus, Symbolic b, Symbolic (Const (PrimValue.of_int offset))))
    | RightOpenInterval (Const a) -> LeftOpenInterval (Const PrimValue.(add_int a offset))
    | RightOpenInterval a -> RightOpenInterval (Op (Plus, Symbolic a, Symbolic (Const (PrimValue.of_int offset))))
    | OpenInterval -> OpenInterval
    | Symbolic (Const n) -> Symbolic (Const PrimValue.(add_int n offset))
    (* TODO: choose between adding it to a or b? e.g., g0-16+8 is better represented as g0-8 than g0+8-16. Maybe introduce a simplification phase*)
    | Symbolic (Op (Plus, a, b)) -> simplify_value (Symbolic (Op (Plus, a, simplify_value (add_offset b offset))))
    | Symbolic (Op (Minus, a, b)) -> simplify_value (Symbolic (Op (Minus, a, simplify_value (add_offset b (- offset)))))
    | Symbolic (Op (Times, a, b)) -> simplify_value (Symbolic (Op (Plus, Symbolic (Op (Times, a, b)), Symbolic (Const (PrimValue.of_int offset)))))
    | Symbolic _ -> simplify_value (Symbolic (Op (Plus, v, Symbolic (Const (PrimValue.of_int offset)))))

module ValueValueMap = struct
  module ValueMap = Map.Make(ValueT)

  type t = ValueT.t ValueMap.t
  [@@deriving sexp, compare]
end

let rec adapt_value (v : value) (map : ValueValueMap.t) : value =
  match v with
  | Bottom
  | Interval _
  | LeftOpenInterval _
  | RightOpenInterval _
  | OpenInterval
    -> v
  | Symbolic (Parameter _)
  | Symbolic (Global _) ->
    begin match ValueValueMap.ValueMap.find map v with
      | Some v' ->
        Printf.printf "[ADAPT] %s into %s\n" (value_to_string v) (value_to_string v');
        v'
      | None -> failwith (Printf.sprintf "Cannot adapt value %ss" (value_to_string v))
    end
  | Symbolic (Op (op, v1, v2)) ->
    simplify_value (Symbolic (Op (op, (adapt_value v1 map), (adapt_value v2 map))))
  | Symbolic (Const _) -> v
  | Symbolic (Deref v) ->
    Symbolic (Deref (adapt_value v map))
  | Symbolic (Bytes4 (b0, b1, b2, b3)) -> Symbolic (Bytes4 (adapt_value b0 map, adapt_value b1 map, adapt_value b2 map, adapt_value b3 map))
  | Symbolic (Byte (v, b)) -> Symbolic (Byte (adapt_value v map, b))

let adapt (v : t) (map : ValueValueMap.t) : t =
  { value = adapt_value v.value map; typ = v.typ }
