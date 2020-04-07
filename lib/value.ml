open Core_kernel
open Wasm

(** These are the values (and their abstractions) *)
module T = struct
  type operator =
    | Plus | Minus | Times
    | Lt | LtE | Gt | GtE | Eq
  [@@deriving sexp, compare, yojson]
  type symbolic =
    | Parameter of int (* p0 *)
    | Global of int (* g0 *)
    | Op of operator * t * t (* g0-16 *)
    | Deref of t (* *g0 *)
    | Const of int32
  and t =
    | Bottom
    | Interval of symbolic * symbolic (* [a,b] *)
    | LeftOpenInterval of symbolic (* ]-inf,a] *)
    | RightOpenInterval of symbolic (* [a,+inf[ *)
    | OpenInterval (* ]-inf,+inf[ *)
    | Symbolic of symbolic
    (* XXX: values are actually i32/i64/f32/f64, but we only support i32 *)
  [@@deriving sexp, compare, yojson]

end
include T
include Comparator.Make(T)

let of_wasm (v : Values.value) : t =
  match v with
  | I32 x -> Symbolic (Const x)
  | I64 _ -> failwith "unsupported type: I64"
  | F32 _ -> failwith "unsupported type: F32"
  | F64 _ -> failwith "unsupported type: F64"

let rec to_string (v : t) : string = match v with
  | Bottom -> "bottom"
  | Interval (a, b) -> Printf.sprintf "[%s,%s]" (symbolic_to_string a) (symbolic_to_string b)
  | LeftOpenInterval b -> Printf.sprintf "]-inf,%s]" (symbolic_to_string b)
  | RightOpenInterval a -> Printf.sprintf "[%s,+inf[" (symbolic_to_string a)
  | OpenInterval -> "T"
  | Symbolic sym -> symbolic_to_string sym
and symbolic_to_string (v : symbolic) : string = match v with
  | Const n -> Int32.to_string n
  | Parameter i -> Printf.sprintf "p%d" i
  | Global i -> Printf.sprintf "g%d" i
  | Op (op, left, right) -> Printf.sprintf "%s%s%s" (to_string left) (begin match op with
      | Plus -> "+"
      | Minus -> "-"
      | Times -> "*"
      | Lt -> "<"
      | LtE -> "<="
      | Gt -> ">"
      | GtE -> ">="
      | Eq -> "="
    end) (to_string right)
  | Deref v -> Printf.sprintf "*%s" (to_string v)

let rec simplify_symbolic (sym : symbolic) : symbolic =
  match sym with
  | (Op (Plus, a, Symbolic (Const 0l))) ->
    (* a+0 is handled in simplify *)
    (Op (Plus, a, Symbolic (Const 0l)))
  | (Op (Plus, (Symbolic (Op (Minus, a, Symbolic (Const x)))), Symbolic (Const y))) when x = y ->
    (* (a-x)+x = a *)
    (Op (Plus, a, Symbolic (Const 0l)))
  | (Op (Plus, (Symbolic (Op (Minus, a, Symbolic (Const x)))), Symbolic (Const y))) when x > y ->
    (* (a-x)+y when x > y = a-(x-y) *)
    simplify_symbolic (Op (Minus, simplify a, Symbolic (Const (Int32.(-) x y))))
  | (Op (Plus, (Symbolic (Op (Minus, a, Symbolic (Const x)))), Symbolic (Const y))) when x < y ->
    (* (a-x)+y when y > x = a+(y-x)*)
    simplify_symbolic (Op (Plus, simplify a, Symbolic (Const (Int32.(-) y x))))
  | (Op (Plus, (Symbolic (Op (Plus, a, Symbolic (Const x)))), Symbolic (Const y))) ->
    (* (a+x)+y = a + (x+y) *)
    simplify_symbolic (Op (Plus, simplify a, Symbolic (Const (Int32.(+) x y))))
  | (Op (Minus, (Symbolic (Op (Minus, a, Symbolic (Const x)))), Symbolic (Const y))) ->
    (* (a-x)-y = a-(x+y) *)
    simplify_symbolic (Op (Minus, simplify a, Symbolic (Const (Int32.(+) x y))))
  | Op (Eq, Symbolic (Op (Lt, a, b)), Symbolic (Const 0l)) ->
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
and simplify (v : t) : t =
  match (match v with
      | Bottom -> Bottom
      | Interval (Const a, Const b) when a = b -> Symbolic (Const a)
      | Interval (a, b) -> Interval (simplify_symbolic a, simplify_symbolic b)
      | LeftOpenInterval b -> LeftOpenInterval (simplify_symbolic b)
      | RightOpenInterval a -> RightOpenInterval (simplify_symbolic a)
      | OpenInterval -> OpenInterval
      | Symbolic sym -> Symbolic (simplify_symbolic sym))
  with
  | Symbolic (Op (Plus, a, Symbolic (Const 0l))) -> a
  | Symbolic (Op (Minus, a, Symbolic (Const 0l))) -> a
  | res -> res


(** Checks if v1 subsumes v2 (i.e., v1 contains v2) *)
let subsumes (v1 : t) (v2 : t) : bool = match (v1, v2) with
  | _, _ when v1 = v2 -> true
  | _, Bottom -> true
  | Bottom, _ -> false
  | Symbolic (Const n1), Symbolic (Const n2) -> n1 = n2
  | Symbolic (Const n), Interval (Const a, Const b) -> a = b && a = n
  | Interval (Const a, Const b), Interval (Const a', Const b') -> a <= a' && b >= b'
  | Interval (Const a, Const b), Symbolic (Const n) -> a <= n && b >= n
  | LeftOpenInterval (Const b), Symbolic (Const n) -> b >= n
  | LeftOpenInterval (Const b), Interval (_, Const b') -> b >= b'
  | LeftOpenInterval (Const b), LeftOpenInterval (Const b') -> b >= b'
  | RightOpenInterval (Const a), Symbolic (Const n) -> a <= n
  | RightOpenInterval (Const a), Interval (Const a', _) -> a <= a'
  | RightOpenInterval (Const a), RightOpenInterval (Const a') -> a <= a'
  | OpenInterval, Symbolic (Const _) -> true
  | OpenInterval, Interval _ -> true
  | OpenInterval, LeftOpenInterval _ -> true
  | OpenInterval, RightOpenInterval _ -> true
  | OpenInterval, OpenInterval -> true
  | Symbolic (Op (_, Symbolic (Global i), Symbolic (Const x))), Symbolic (Op (_, Symbolic (Global i'), Symbolic (Const x'))) when i = i' ->
    x = x'
  | _, _ ->
    Logging.warn "SubsumesMightBeIncorrect" (Printf.sprintf "assuming %s does not subsume %s" (to_string v1) (to_string v2));
    false

let bottom : t = Bottom
let zero : t = Symbolic (Const 0l)
let parameter (i : int) : t = Symbolic (Parameter i)
let global (i : int) : t = Symbolic (Global i)
let deref (addr : t) : t = Symbolic (Deref addr)
let const (n : int32) : t = Symbolic (Const n)
let bool : t = Interval (Const 0l, Const 1l)
let top (source : string) : t = Logging.warn "TopCreated" (Printf.sprintf "Top value originating from: %s" source); OpenInterval
let symbolic (sym : symbolic) : t = Symbolic (simplify_symbolic sym)
let list_to_string (l : t list) : string =
  String.concat ~sep:", " (List.map l ~f:to_string)

(** Joins two values together *)
let join (v1 : t) (v2 : t) : t =
  let vres = match (v1, v2) with
  | (Bottom, _) -> v2
  | (_, Bottom) -> v1
  | (_, _) when v1 = v2 -> v1
  | (Symbolic (Const n1), Symbolic (Const n2)) when n1 = n2 -> const n1
  | (Symbolic (Const n1), Symbolic (Const n2)) -> Interval (Const (min n1 n2), Const (max n1 n2))
  | (Symbolic (Const n), Interval (Const a, Const b)) -> Interval (Const (min a n), Const (max b n))
  | (Interval (Const a, Const b), Symbolic (Const n)) -> Interval (Const (min a n), Const (max b n))
  | (Interval (Const 0l, Const b), Interval (Const 0l, Const b')) when b <> b' -> RightOpenInterval (Const 0l) (* TODO: this is a very simple widening when the right bound is unstable *)
  | (Interval (Const a, Const b), Interval (Const a', Const b')) -> Interval (Const (min a a'), Const (max b b')) (* TODO: need widen to ensure convergence *)
  | (RightOpenInterval (Const a), RightOpenInterval (Const a')) -> RightOpenInterval (Const (min a a'))
  | (LeftOpenInterval (Const b), LeftOpenInterval (Const b')) -> LeftOpenInterval (Const (max b b'))
  | (Interval (Const a, _), RightOpenInterval (Const a')) -> RightOpenInterval (Const (min a a'))
  | (RightOpenInterval (Const a), Symbolic (Const c)) -> RightOpenInterval (Const (min a c))
  | (LeftOpenInterval (Const b), Symbolic (Const c)) -> LeftOpenInterval (Const (max b c))
  | (RightOpenInterval (Op (Plus, Symbolic x, Symbolic (Const _))), RightOpenInterval x') when x = x' -> v2
  | (OpenInterval, Symbolic (Const _))
  | (Symbolic (Const _), OpenInterval)
  | (OpenInterval, Interval (Const _, Const _))
  | (Interval (Const _, Const _), OpenInterval)
    -> OpenInterval
  | (Symbolic (Const _), LeftOpenInterval (Parameter _))
  | (Symbolic (Const _), LeftOpenInterval (Op (_, Symbolic (Parameter _), _)))
  | (Symbolic (Const _), RightOpenInterval (Op (_, Symbolic (Parameter _), _)))
  | (Symbolic (Const _), RightOpenInterval (Parameter _)) ->
     Logging.warn "UnsoundAssumption" (Printf.sprintf "%s contains %s" (to_string v2) (to_string v1));
     v2
  | _ -> top (Printf.sprintf "Value.join %s %s" (to_string v1) (to_string v2)) in
  Logging.info (Printf.sprintf "join %s with %s gives %s" (to_string v1) (to_string v2) (to_string vres));
  vres

(** Joins two value lists together, assuming they have the same length *)
let join_vlist_exn (v1 : t list) (v2 : t list) : t list =
  List.map2_exn v1 v2 ~f:join

(** Meet two values together *)
let meet (v1 : t) (v2 : t) : t =
  match (v1, v2) with
  | (Bottom, _)
  | (_, Bottom) -> Bottom
  | (_, _) when v1 = v2 -> v1
  | (_, OpenInterval) -> v1
  | (OpenInterval, _) -> v2
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
    v1

(* TODO: maybe these functions should take the memory as argument, and return an updated version of it? *)
let is_zero (v : t) =
  match v with
  | Bottom -> false
  | Symbolic (Const 0l) -> true
  | Symbolic (Const _) -> false
  | Interval (Const a, Const b) -> a <= Int32.zero && b >= Int32.zero
  | Interval (_, Const b) -> b >= Int32.zero
  | Interval (Const a, _) -> a <= Int32.zero
  | Interval _ -> true
  | LeftOpenInterval (Const b) -> b >= Int32.zero
  | LeftOpenInterval _ -> true
  | RightOpenInterval (Const a) -> a <= Int32.zero
  | RightOpenInterval _ -> true
  | OpenInterval -> true
  | Symbolic _ -> true (* TODO: could be more precise here? Or not *)

let is_not_zero (v : t) =
  match v with
  | Bottom -> false
  | Symbolic (Const 0l) -> false
  | Symbolic (Const _) -> true
  | Interval (Const a, Const b) -> not (a = 0l && b = 0l)
  | Interval _ -> true (* TODO could be more precise *)
  | LeftOpenInterval _ -> true
  | RightOpenInterval _ -> true
  | OpenInterval -> true
  | Symbolic _ -> true (* TODO: could be more precise here *)

let rec add_offset (v : t) (offset : int) : t =
  if offset = 0 then v else
    let off = Int32.of_int_exn offset in
    match v with
    | Bottom -> Bottom
    | Interval (Const a, Const b) -> Interval (Const (Int32.(+) a off), Const (Int32.(+) b off))
    | Interval (a, b) -> Interval (Op (Plus, Symbolic a, const off), Op (Plus, Symbolic b, const off))
    | LeftOpenInterval (Const b) -> LeftOpenInterval (Const (Int32.(+) b off))
    | LeftOpenInterval b -> LeftOpenInterval (Op (Plus, Symbolic b, const off))
    | RightOpenInterval (Const a) -> LeftOpenInterval (Const (Int32.(+) a off))
    | RightOpenInterval a -> RightOpenInterval (Op (Plus, Symbolic a, const off))
    | OpenInterval -> OpenInterval
    | Symbolic (Const n) -> const (Int32.(+) n off)
    (* TODO: choose between adding it to a or b? e.g., g0-16+8 is better represented as g0-8 than g0+8-16. Maybe introduce a simplification phase*)
    | Symbolic (Op (Plus, a, b)) -> simplify (Symbolic (Op (Plus, a, simplify (add_offset b offset))))
    | Symbolic (Op (Minus, a, b)) -> simplify (Symbolic (Op (Minus, a, simplify (add_offset b (- offset)))))
    | Symbolic (Op (Times, a, b)) -> simplify (Symbolic (Op (Plus, Symbolic (Op (Times, a, b)), const off)))
    | Symbolic _ -> simplify (Symbolic (Op (Plus, v, const off)))

module ValueValueMap = struct
  module ValueMap = Map.Make(T)

  type t = T.t ValueMap.t
  [@@deriving sexp, compare]
end

let rec adapt (v : t) (map : ValueValueMap.t) : t =
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
    | Some v' -> Printf.printf "[ADAPT] %s into %s\n" (to_string v) (to_string v'); v'
    | None -> failwith (Printf.sprintf "Cannot adapt value %ss" (to_string v))
    end
  | Symbolic (Op (op, v1, v2)) ->
    simplify (Symbolic (Op (op, adapt v1 map, adapt v2 map)))
  | Symbolic (Const _) -> v
  | Symbolic (Deref v) ->
    Symbolic (Deref (adapt v map))
