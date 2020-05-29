open Core_kernel
open Wasm

(******************** Types *****************)

(** These are the values (and their abstractions) *)
module T = struct
  (** A byte position, to index bytes in i32/i64 values. Should always be between 0 and 7 in practice. *)
  type bytepos = int
  [@@deriving sexp, compare]

  (** Operators that can be used in symbolic values *)
  type operator =
    | Plus | Minus | Times (* TODO: minus should be removed, as a-x is also a+(-x) *)
    | Lt | LtE | Gt | GtE | Eq
    | And | Or | Xor
  [@@deriving sexp, compare]

  (** Symbolic values *)
  type symbolic =
    | Parameter of int (** A parameter, e.g. p0 *)
    | Global of int (** A global, e.g., g0 *)
    | Op of operator * value * value (** An operation on two values, e.g., g0-16 *)
    | Bytes4 of value * value * value * value (** Four bytes, b0b1b2b3, ordered as expected: b3 is the rightmost byte, and the righmost value in Bytes4 *)
    | Byte of value * int (** A specific byte of a value, e.g., x@0 is byte 0 of value x *)
    | Deref of value (** A dereference, e.g., *g0 *)
    | Const of Prim_value.t (** A constant value *)
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

(***************** Constructors ******************)

(** Constructs a value from a wasm value.
    @param v the wasm value *)
let of_wasm (v : Values.value) : t =
  match v with
  | I32 x -> { value = Symbolic (Const (I32 x)); typ = Type.I32 }
  | I64 x -> { value = Symbolic (Const (I64 x)); typ = Type.I64 }
  | F32 _ -> failwith "unsupported type: F32"
  | F64 _ -> failwith "unsupported type: F64"

(** Creates the bottom value.
    @param typ is the type of that value *)
let bottom (typ : Type.t) : t = { value = Bottom; typ = typ }

(** Creates a value from an i32 *)
let i32_const (n : int32) : t = { value = Symbolic (Const (I32 n)); typ = I32 }

(** Creates the zero value of the given type *)
let zero (t : Type.t) : t = { value = Symbolic (Const (Prim_value.zero_of_t t)); typ = t }

(** The true value, which is 1 *)
let true_ : t = i32_const 1l

(** The false value, which is 0 *)
let false_ : t = i32_const 0l

(** Any boolean, i.e., 0 or 1 *)
let bool : t = { value = Interval (Const (I32 0l), Const (I32 1l)); typ = I32 } (* could also just be defined join true_ false_ *)

(** Injects a parameter into the value domain
    @param typ the type of the parameter
    @param idx the index of the parameter *)
let parameter (typ : Type.t) (idx : int) : t = { value = Symbolic (Parameter idx); typ = typ }

(** Injects a global into the value domain
    @param typ the type of the global
    @param idx the index of the global *)
let global (typ : Type.t) (i : int) : t = { value = Symbolic (Global i); typ = typ }

let deref (addr : value) : t = { value = Symbolic (Deref addr); typ = I32 } (* TODO: typ *)

(** Creates the top value
    @param typ the type for this value
    @param source a string indicating where this value is originating from, to use as warning *)
let top (typ : Type.t) (source : string) : t =
  Logging.warn "TopCreated" (Printf.sprintf "Top value originating from: %s" source);
  { value = OpenInterval; typ = typ }

(********************** String conversion ******************)

let rec value_to_string (v : value) : string = match v with
  | Bottom -> "bottom"
  | Interval (a, b) -> Printf.sprintf "[%s,%s]" (symbolic_to_string a) (symbolic_to_string b)
  | LeftOpenInterval b -> Printf.sprintf "]-inf,%s]" (symbolic_to_string b)
  | RightOpenInterval a -> Printf.sprintf "[%s,+inf[" (symbolic_to_string a)
  | OpenInterval -> "T"
  | Symbolic sym -> symbolic_to_string sym
and symbolic_to_string (v : symbolic) : string = match v with
  | Const n -> Prim_value.to_string n
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
      | Or -> "or"
      | Xor -> "xor"
    end) (value_to_string right)
  | Deref v -> Printf.sprintf "*%s" (value_to_string v)
  | Bytes4 (b0, b1, b2, b3) -> Printf.sprintf "bytes[%s,%s,%s,%s]" (value_to_string b0) (value_to_string b1) (value_to_string b2) (value_to_string b3)
  | Byte (v, b) -> Printf.sprintf "%s@%d" (value_to_string v) b
let to_string (v : t) : string = value_to_string v.value
let list_to_string (l : t list) : string =
  String.concat ~sep:", " (List.map l ~f:to_string)


(*************** Simplifications ******************)

let rec simplify_symbolic (sym : symbolic) : symbolic =
  match sym with
  | (Op (Plus, a, Symbolic (Const z))) when Prim_value.is_zero z->
    (* a+0 is handled in simplify *)
    (Op (Plus, a, Symbolic (Const z)))
  | (Op (Plus, (Symbolic (Op (Minus, a, Symbolic (Const x)))), Symbolic (Const y))) when Prim_value.eq x y ->
    (* (a-x)+x = a *)
    (Op (Plus, a, Symbolic (Const (Prim_value.zero_of_same_t x))))
  | (Op (Plus, (Symbolic (Op (Minus, a, Symbolic (Const x)))), Symbolic (Const y))) when Prim_value.gt_s x y ->
    (* (a-x)+y when x > y = a-(x-y) *)
    simplify_symbolic (Op (Minus, simplify_value a, Symbolic (Const (Prim_value.sub x y))))
  | (Op (Plus, (Symbolic (Op (Minus, a, Symbolic (Const x)))), Symbolic (Const y))) when Prim_value.lt_s x y ->
    (* (a-x)+y when y > x = a+(y-x)*)
    simplify_symbolic (Op (Plus, simplify_value a, Symbolic (Const (Prim_value.sub y x))))
  | (Op (Plus, (Symbolic (Op (Plus, a, Symbolic (Const x)))), Symbolic (Const y))) ->
    (* (a+x)+y = a + (x+y) *)
    simplify_symbolic (Op (Plus, simplify_value a, Symbolic (Const (Prim_value.add x y))))
  | (Op (Minus, (Symbolic (Op (Minus, a, Symbolic (Const x)))), Symbolic (Const y))) ->
    (* (a-x)-y = a-(x+y) *)
    simplify_symbolic (Op (Minus, simplify_value a, Symbolic (Const (Prim_value.add x y))))
  | Op (Eq, Symbolic (Op (Lt, a, b)), Symbolic (Const (I32 0l))) ->
    (* a<b=0 = a>b *)
    Op (GtE, a, b)
  | Bytes4 (Symbolic (Const a), Symbolic (Const b), Symbolic (Const c), Symbolic (Const d)) ->
    (* First check that a | 0xFF000000 is 0, etc, because a, b, c, and d should be proper bytes *)
    assert (Prim_value.is_zero (Prim_value.and_ a (I32 0xFF000000l)) &&
            Prim_value.is_zero (Prim_value.and_ b (I32 0xFF0000l)) &&
            Prim_value.is_zero (Prim_value.and_ c (I32 0xFF00l)) &&
            Prim_value.is_zero (Prim_value.and_ d (I32 0xFFl)));
    Const (Prim_value.or_ a (Prim_value.or_ b (Prim_value.or_ c d)))
  | Bytes4 (Symbolic (Byte (Symbolic (Const a), byte_a)),
            Symbolic (Byte (Symbolic (Const b), byte_b)),
            Symbolic (Byte (Symbolic (Const c), byte_c)),
            Symbolic (Byte (Symbolic (Const d), byte_d))) ->
    Logging.warn "UNEXPECTED" "simplify called with %s, but this value should have been simplified first";
    Const (Prim_value.or_
             (Prim_value.shl (Prim_value.byte_of a byte_a) (I32 24l))
             (Prim_value.or_ (Prim_value.shl (Prim_value.byte_of b byte_b) (I32 16l))
                (Prim_value.or_ (Prim_value.shl (Prim_value.byte_of c byte_c) (I32 8l))
                   (Prim_value.byte_of d byte_d))))
  | Bytes4 (Symbolic (Byte (Symbolic x, 3)),
            Symbolic (Byte (Symbolic x', 2)),
            Symbolic (Byte (Symbolic x'', 1)),
            Symbolic (Byte (Symbolic x''', 0))) when Stdlib.(x = x' && x' = x'' && x'' = x''') ->
    x
  | Byte (Symbolic (Const x), b) ->
    Const (Prim_value.and_ x (I32 (Int32.shift_left 0xFFl  (b * 8))))
  (* TODO: many more cases *)
  | (Global _) | (Parameter _) | Const _
  | Byte (Symbolic (Global _), _) | Byte (Symbolic (Parameter _), _)
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
      | Interval (Const a, Const b) when Prim_value.eq a b -> Symbolic (Const a)
      | Interval (a, b) -> Interval (simplify_symbolic a, simplify_symbolic b)
      | LeftOpenInterval b -> LeftOpenInterval (simplify_symbolic b)
      | RightOpenInterval a -> RightOpenInterval (simplify_symbolic a)
      | OpenInterval -> OpenInterval
      | Symbolic sym -> Symbolic (simplify_symbolic sym))
  with
  | Symbolic (Op (Plus, a, Symbolic (Const z))) when Prim_value.is_zero z -> a
  | Symbolic (Op (Minus, a, Symbolic (Const z))) when Prim_value.is_zero z -> a
  | res -> res

let simplify (v : t) : t = { value = simplify_value v.value; typ = v.typ }

let symbolic (t : Type.t) (sym : symbolic) : t = { value = Symbolic (simplify_symbolic sym); typ = t }

(************************ Subsumption *********************)

(** Checks if v1 subsumes v2 (i.e., v1 contains v2) *)
let value_subsumes (v1 : value) (v2 : value) : bool = match (v1, v2) with
  | _, _ when Stdlib.(v1 = v2) -> true
  | _, Bottom -> true
  | Bottom, _ -> false
  | Symbolic (Const n1), Symbolic (Const n2) -> Prim_value.eq n1 n2
  | Symbolic (Const n), Interval (Const a, Const b) -> Prim_value.(eq a b && eq a n)
  | Interval (Const a, Const b), Interval (Const a', Const b') -> Prim_value.(le_s a a' && ge_s b b')
  | Interval (Const a, Const b), Symbolic (Const n) -> Prim_value.(le_s a n && ge_s b n)
  | LeftOpenInterval (Const b), Symbolic (Const n) -> Prim_value.(ge_s b n)
  | LeftOpenInterval (Const b), Interval (_, Const b') -> Prim_value.(ge_s b b')
  | LeftOpenInterval (Const b), LeftOpenInterval (Const b') -> Prim_value.(ge_s b b')
  | RightOpenInterval (Const a), Symbolic (Const n) -> Prim_value.(le_s a n)
  | RightOpenInterval (Const a), Interval (Const a', _) -> Prim_value.(le_s a a')
  | RightOpenInterval (Const a), RightOpenInterval (Const a') -> Prim_value.(le_s a a')
  | OpenInterval, _ -> true
  | _, OpenInterval -> false
  | Symbolic (Op (_, Symbolic (Global i), Symbolic (Const x))), (* gi OP x *)
    Symbolic (Op (_, Symbolic (Global i'), Symbolic (Const x'))) (* gi' OP x' *)
    when i = i' ->
    Prim_value.eq x x'
  | Symbolic (Op (_, Symbolic (Parameter i), Symbolic (Const x))),
    Symbolic (Op (_, Symbolic (Parameter i'), Symbolic (Const x')))
    when i = i' ->
    Prim_value.eq x x'
  | Symbolic (Bytes4 (Symbolic (Deref OpenInterval),
                      Symbolic (Deref OpenInterval),
                      Symbolic (Deref OpenInterval),
                      Symbolic (Deref OpenInterval))), _ ->
    (* If this case ever applies, then we probably have lost too much precision *)
    true
  | _, _ ->
    Logging.warn "SubsumesMightBeIncorrect" (Printf.sprintf "assuming %s does not subsume %s" (value_to_string v1) (value_to_string v2));
    (* Definitely not sound, these cases have to be investigated one by one *)
    false

let subsumes (v1 : t) (v2 : t) : bool = value_subsumes v1.value v2.value


(************************ Joining *****************)
(** Joins two values together *)
let join (v1 : t) (v2 : t) : t =
  assert Stdlib.(v1.typ = v2.typ);
  let vres: value = match (v1.value, v2.value) with
  | (Bottom, _) -> v2.value
  | (_, Bottom) -> v1.value
  | (OpenInterval, _) | (_, OpenInterval) -> OpenInterval
  | (_, _) when Stdlib.(v1 = v2) -> v1.value
  | (Symbolic (Const n1), Symbolic (Const n2)) when Prim_value.eq n1 n2 ->
    Symbolic (Const n1)
  | (Symbolic (Const n1), Symbolic (Const n2)) ->
    Interval (Const Prim_value.(min n1 n2), Const Prim_value.(max n1 n2))
  | (Symbolic (Const n), Interval (Const a, Const b)) -> Interval (Const Prim_value.(min a n), Const Prim_value.(max b n))
  | (Interval (Const a, Const b), Symbolic (Const n)) -> Interval (Const Prim_value.(min a n), Const Prim_value.(max b n))
  | (Interval (Const z, Const b), Interval (Const z', Const b')) when Prim_value.(is_zero z && is_zero z' && not (eq b b')) ->
    (* TODO: this is a very simple widening when the right bound is unstable *)
    RightOpenInterval (Const (Prim_value.zero_of_same_t z))
  | (Interval (Const a, Const b), Interval (Const a', Const b')) ->
    (* TODO: need widen to ensure convergence *)
    Interval (Const Prim_value.(min a a'), Const Prim_value.(max b b'))
  | (RightOpenInterval (Const a), RightOpenInterval (Const a')) -> RightOpenInterval (Const Prim_value.(min a a'))
  | (LeftOpenInterval (Const b), LeftOpenInterval (Const b')) -> LeftOpenInterval (Const Prim_value.(max b b'))
  | (Interval (Const a, _), RightOpenInterval (Const a')) -> RightOpenInterval (Const Prim_value.(min a a'))
  | (RightOpenInterval (Const a), Symbolic (Const c)) -> RightOpenInterval (Const Prim_value.(min a c))
  | (LeftOpenInterval (Const b), Symbolic (Const c)) -> LeftOpenInterval (Const Prim_value.(max b c))
  | (RightOpenInterval (Op (Plus, Symbolic x, Symbolic (Const _))), RightOpenInterval x') when (Stdlib.(=) x x') ->
    v2.value
  | (Symbolic (Op (Plus, Symbolic (Parameter i), Symbolic (Const a))), Symbolic (Parameter i')) when i = i'->
    (* p0+X joined with p0 is [p0,p0+X] *)
    Interval (Parameter i, Op (Plus, Symbolic (Parameter i), Symbolic (Const a)))
  | (Symbolic (Parameter i), Symbolic (Op (Plus, Symbolic (Parameter i'), Symbolic (Const a)))) when i = i' ->
    (* p0 joined with p0+X is [p0,p0+X] *)
    Interval (Parameter i, Op (Plus, Symbolic (Parameter i), Symbolic (Const a)))
  | (Symbolic (Const _), LeftOpenInterval (Parameter _))
  | (Symbolic (Const _), LeftOpenInterval (Op (_, Symbolic (Parameter _), _)))
  | (Symbolic (Const _), RightOpenInterval (Op (_, Symbolic (Parameter _), _)))
  | (Symbolic (Const _), RightOpenInterval (Parameter _)) ->
    Logging.warn "UnsoundAssumption" (Printf.sprintf "%s contains %s" (to_string v2) (to_string v1));
    v2.value
  | _ -> (top v1.typ (Printf.sprintf "Value.join %s %s" (to_string v1) (to_string v2))).value in
  (* Logging.info (Printf.sprintf "join %s with %s gives %s" (to_string v1) (to_string v2) (value_to_string vres)); *)
  { typ = v1.typ;
    value = vres }

let%test "bool is join true_ false_" = Stdlib.(bool = join true_ false_)

(** Joins two value lists together, assuming they have the same length *)
let join_vlist_exn (v1 : t list) (v2 : t list) : t list =
  List.map2_exn v1 v2 ~f:join

(********************* Meeting **********************)

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

(************** Checkers ******************)

(* TODO: maybe these functions should take the memory as argument, and return an updated version of it? *)
let is_zero (v : t) =
  match v.value with
  | Bottom -> false
  | Symbolic (Const z) when Prim_value.is_zero z -> true
  | Symbolic (Const _) -> false
  | Interval (Const a, Const b) -> Prim_value.(le_s a (Prim_value.zero_of_same_t a) && ge_s b (Prim_value.zero_of_same_t b))
  | Interval (_, Const b) -> Prim_value.ge_s b (Prim_value.zero_of_same_t b)
  | Interval (Const a, _) -> Prim_value.le_s a (Prim_value.zero_of_same_t a)
  | Interval _ -> true
  | LeftOpenInterval (Const b) -> Prim_value.ge_s b (Prim_value.zero_of_same_t b)
  | LeftOpenInterval _ -> true
  | RightOpenInterval (Const a) -> Prim_value.le_s a (Prim_value.zero_of_same_t a)
  | RightOpenInterval _ -> true
  | OpenInterval -> true
  | Symbolic _ -> true (* TODO: could be more precise here? Or not *)

let is_not_zero (v : t) =
  match v.value with
  | Bottom -> false
  | Symbolic (Const z) when Prim_value.is_zero z -> false
  | Symbolic (Const _) -> true
  | Interval (Const a, Const b) -> not Prim_value.(eq a (Prim_value.zero_of_same_t a) && eq b (Prim_value.zero_of_same_t b))
  | Interval _ -> true (* TODO could be more precise *)
  | LeftOpenInterval _ -> true
  | RightOpenInterval _ -> true
  | OpenInterval -> true
  | Symbolic _ -> true (* TODO: could be more precise here *)

(******************** Adaptation **********************)
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

(********************** Operations *********************)
let rec add_offset (v : value) (offset : int) : value =
  if offset = 0 then v else
    match v with
    | Bottom -> Bottom
    | Interval (Const a, Const b) -> Interval (Const Prim_value.(add_int a offset), Const Prim_value.(add_int b offset))
    | Interval (a, b) -> Interval (Op (Plus, Symbolic a, Symbolic (Const (Prim_value.of_int offset))), Op (Plus, Symbolic b, Symbolic (Const (Prim_value.of_int offset))))
    | LeftOpenInterval (Const b) -> LeftOpenInterval (Const Prim_value.(add_int b offset))
    | LeftOpenInterval b -> LeftOpenInterval (Op (Plus, Symbolic b, Symbolic (Const (Prim_value.of_int offset))))
    | RightOpenInterval (Const a) -> LeftOpenInterval (Const Prim_value.(add_int a offset))
    | RightOpenInterval a -> RightOpenInterval (Op (Plus, Symbolic a, Symbolic (Const (Prim_value.of_int offset))))
    | OpenInterval -> OpenInterval
    | Symbolic (Const n) -> Symbolic (Const Prim_value.(add_int n offset))
    (* TODO: choose between adding it to a or b? e.g., g0-16+8 is better represented as g0-8 than g0+8-16. Maybe introduce a simplification phase*)
    | Symbolic (Op (Plus, a, b)) -> simplify_value (Symbolic (Op (Plus, a, simplify_value (add_offset b offset))))
    | Symbolic (Op (Minus, a, b)) -> simplify_value (Symbolic (Op (Minus, a, simplify_value (add_offset b (- offset)))))
    | Symbolic (Op (Times, a, b)) -> simplify_value (Symbolic (Op (Plus, Symbolic (Op (Times, a, b)), Symbolic (Const (Prim_value.of_int offset)))))
    | Symbolic _ -> simplify_value (Symbolic (Op (Plus, v, Symbolic (Const (Prim_value.of_int offset)))))

let rec add (v1 : t) (v2 : t) : t =
  assert Stdlib.(v1.typ = v2.typ);
  let v = match (v1.value, v2.value) with
    | (_, Symbolic (Const z)) when Prim_value.is_zero z ->
      (* v + 0 is v *)
      v1.value
    | (Symbolic (Const n1), Symbolic (Const n2)) ->
      (* X+Y when X and Y are const, can directly be computed *)
      (Symbolic (Const (Prim_value.add n1 n2)))
    | (Symbolic (Op _), _) ->
      (* (a op b) + c, adds +c to the operations *)
      simplify_value (Symbolic (Op (Plus, v1.value, v2.value)))
    | (Symbolic (Parameter i), _) ->
      (* pi + c becomes pi+c as an op *)
      simplify_value (Symbolic (Op (Plus, Symbolic (Parameter i), v2.value)))
    | (Symbolic (Global i), _) ->
      (* g0 + c becomes g0+c as an op *)
      simplify_value (Symbolic (Op (Plus, Symbolic (Global i), v2.value)))
    | (Symbolic (Deref _), _) -> Symbolic (Op (Plus, v1.value, v2.value))
    | (Interval (Const a, Const b), Symbolic (Const n)) ->
      (* [a,b] + n becomes [a+n,b+n], where a, b and n are constants *)
      Interval (Const (Prim_value.add a n), Const (Prim_value.add b n))
    | (Interval (a, b), Symbolic (Const n)) ->
      (* [a,b] + n becomes [a+n, b+n] where a and b are any symbolic values, n is a constant.
         Only applies when a+n and b+n result in symbolic values *)
      begin match ((add { value = (Symbolic a); typ = v1.typ } { value = (Symbolic (Const n)); typ = v2.typ }).value,
                   (add { value = (Symbolic b); typ = v1.typ } { value = (Symbolic (Const n)); typ = v2.typ }).value) with
      | (Symbolic a', Symbolic b') -> Interval (a', b')
      | _ -> (top v1.typ (Printf.sprintf "add %s %s" (to_string v1) (to_string v2))).value
      end
    | (RightOpenInterval (Const x), Symbolic (Const y)) ->
      (* [x,+inf[ + y becomes [x+y,+inf[ *)
      RightOpenInterval (Const (Prim_value.add x y))
    | (LeftOpenInterval (Const x), Symbolic (Const y)) ->
      (* ]-inf,x] + y becomes ]-inf,x+y] *)
      RightOpenInterval (Const (Prim_value.add x y))
    | (RightOpenInterval (Parameter i), Symbolic (Const n)) ->
      (* [p0,+inf[ + n becomes [p0+n,+inf[ *)
      RightOpenInterval (Op (Plus, (Symbolic (Parameter i)), (Symbolic (Const n))))
    | _ -> (top v1.typ (Printf.sprintf "add %s %s" (to_string v1) (to_string v2))).value
  in { value = v; typ = v1.typ }

let sub (v1 : t) (v2 : t) : t =
  assert Stdlib.(v1.typ = v2.typ);
  match (v1.value, v2.value) with
  | (Symbolic (Const n1), Symbolic (Const n2)) -> { value = Symbolic (Const (Prim_value.sub n1 n2)); typ = v1.typ }
  | (Symbolic (Global _), Symbolic (Const _)) -> { value = simplify_value (Symbolic (Op (Minus, v1.value, v2.value))); typ = v1.typ }
  | _ -> top v1.typ (Printf.sprintf "sub %s %s" (to_string v1) (to_string v2))

let mul (v1 : t) (v2 : t) : t =
  assert Stdlib.(v1.typ = v2.typ);
  match (v1.value, v2.value) with
  | (Symbolic (Const n1), Symbolic (Const n2)) -> { value = Symbolic (Const (Prim_value.mul n1 n2)); typ = v1.typ }
  | _ -> top v1.typ (Printf.sprintf "mul %s %s" (to_string v1) (to_string v2))

let rem_s (v1 : t) (v2 : t) : t =
  assert Stdlib.(v1.typ = v2.typ);
  match (v1.value, v2.value) with
  | (Symbolic (Const n1), Symbolic (Const n2)) -> { value = Symbolic (Const (Prim_value.rem_s n1 n2)); typ = v1.typ }
  | _ -> top v1.typ (Printf.sprintf "rem_s %s %s" (to_string v1) (to_string v2))

let shl (v1 : t) (v2 : t) : t =
  assert Stdlib.(v1.typ = v2.typ);
  match (v1.value, v2.value) with
   (Symbolic (Const n1), Symbolic (Const n2)) -> { value = Symbolic (Const (Prim_value.shl n1 n2)); typ = v1.typ }
  (* | (Interval (Const a, Const b), Symbolic (Const 2l)) -> Interval (Const (Int32.( * ) a 4l), Const (Int32.( * ) b 4l)) *) (* TODO *)
  (* | (Symbolic _, Symbolic (Const 2l)) -> symbolic (Op (Times, v1, Symbolic (Const 4l))) *) (* TODO *)
  (* | (RightOpenInterval (Const 0l), Symbolic (Const 2l)) -> v1 *) (* TODO *)
  | _ -> top v1.typ (Printf.sprintf "shl %s %s" (to_string v1) (to_string v2))

let and_ (v1 : t) (v2 : t) : t =
  assert Stdlib.(v1.typ = v2.typ);
  match (v1.value, v2.value) with
  | (Symbolic (Const n1), Symbolic (Const n2)) -> { value = Symbolic (Const (Prim_value.(and_ n1 n2))); typ = v1.typ }
  | (_, Symbolic (Const one)) when Prim_value.is one 1 -> v1
  | (Symbolic (Const one), _) when Prim_value.is one 1 -> v1
  | _ -> { value = Symbolic (Op (And, v1.value, v2.value)); typ = v1.typ }

let or_ (v1 : t) (v2 : t) : t =
  match (v1.value, v2.value) with
  | (Symbolic (Const n1), Symbolic (Const n2)) -> { value = Symbolic (Const (Prim_value.(or_ n1 n2))); typ = v1.typ }
  | (_, Symbolic (Const one)) when Prim_value.is one 1 -> v1
  | (Symbolic (Const one), _) when Prim_value.is one 1 -> v1
  | _ -> { value = Symbolic (Op (Or, v1.value, v2.value)); typ = v1.typ }

let xor (v1 : t) (v2 : t) : t =
  match (v1.value, v2.value) with
  | (Symbolic (Const n1), Symbolic (Const n2)) -> { value = Symbolic (Const (Prim_value.(xor n1 n2))); typ = v1.typ }
  | _ -> { value = Symbolic (Op (Xor, v1.value, v2.value)); typ = v1.typ }
