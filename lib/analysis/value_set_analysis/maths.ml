(**
  {1 Maths}

  A small toolbox for number theory and bit–twiddling helpers, plus an
  {e extended} integer domain supporting ±∞.  The file provides:

  - An [ExtendedInt] module (integers with [Infinity] and [NegInfinity])
    with total ordering and arithmetic where undefined cases raise.
  - Basic number‑theory utilities: [gcd], [lcm], [extended_gcd], and
    a 2‑congruence Chinese Remainder Theorem solver [chinese_remainder].
  - String helpers to cancel symbolic "+"/"neg…" offsets.
  - Lightweight binary helpers in [Binary] to predict the first increment step
    at which each bit flips.

  The library functions are pure and side‑effect free. Unless noted otherwise,
  arithmetic on regular integers uses OCaml [int32] and therefore follows
  OCaml's fixed-width overflow behaviour.  When operations are mathematically
  undefined (e.g. ∞ − ∞, ∞/∞, division by 0), functions raise [Failure] with an
  explanatory message.
*)

open Core 

(**
  {1:extendedInt Extended integers}

  The [ExtendedInt] module augments signed 32‑bit integers with two sentinels
  [Infinity] and [NegInfinity].  It defines a total order consistent with the
  usual order on integers and implements arithmetic that agrees with the
  extended real line, except for mathematically undefined forms which raise.

  {b Conventions}
  - [Int x] embeds a 32‑bit integer [x].
  - Ordering: [NegInfinity < Int _ < Infinity].
  - Arithmetic short‑circuits on infinities when the result is well‑defined;
    finite arithmetic uses [int32] operations.
  - Undefined cases raise [Failure] with a clear message.
*)
module ExtendedInt = struct
  type t = 
    | Int of int32
    | Infinity
    | NegInfinity
  [@@deriving sexp, compare, equal]

  let (=) = equal
  let (<>) (x : t) (y : t) : bool = not (x = y)

  (** [to_string i] returns a human‑readable representation.
      - Finite values use [Int32.to_string].
      - [Infinity] is rendered as ["∞"].
      - [NegInfinity] is rendered as ["-∞"]. *)
  let to_string (i : t) : string =
    match i with 
    | Int i -> Int32.to_string i
    | Infinity -> "∞"
    | NegInfinity -> "-∞"

  (** [maximum x y] is the greater of [x] and [y] in the extended order. *)
  let maximum (x : t) (y : t) : t =
    match x, y with 
    | Infinity, _ | _, Infinity -> Infinity
    | NegInfinity, i | i, NegInfinity -> i
    | Int x, Int y -> Int (Int32.max x y)

  (** [minimum x y] is the lesser of [x] and [y] in the extended order. *)
  let minimum (x : t) (y : t) : t =
    match x, y with 
    | Infinity, i | i, Infinity -> i
    | NegInfinity, _ | _, NegInfinity -> NegInfinity
    | Int x, Int y -> Int (Int32.min x y)

  (** [less_than x y] is [true] iff [x < y] in the extended order. *)
  let less_than (x : t) (y : t) : bool =
    match x, y with
    | Int x, Int y -> Int32.compare x y < 0
    | NegInfinity, Int _ | NegInfinity, Infinity -> true
    | Int _, Infinity -> true
    | _ -> false 

  let (<) = less_than

  (** [is_positive x] returns [true] iff [x >= 0] in the extended order.
      In particular, zero is considered both positive and negative by this
      helper, because it is used as a sign test for interval arithmetic. *)
  let is_positive (x : t) : bool =
    match x with
    | Int 0l -> true
    | _ -> less_than (Int 0l) x

  (** [is_negative x] returns [true] iff [x <= 0] in the extended order.
      In particular, zero is considered both positive and negative by this
      helper, because it is used as a sign test for interval arithmetic. *)
  let is_negative (x : t) : bool =
    match x with
    | Int 0l -> true
    | _ -> less_than x (Int 0l)

  (** [plus x y] adds two extended integers.
      @raise Failure if the sum is undefined (i.e. [Infinity + NegInfinity] or
      [NegInfinity + Infinity]). *)
  let plus (x : t) (y : t) : t =
    match x, y with 
    | Infinity, NegInfinity | NegInfinity, Infinity -> invalid_arg "Infinity + NegInfinity is undefined"
    | Infinity, _ | _, Infinity -> Infinity
    | NegInfinity, _ | _, NegInfinity -> NegInfinity
    | Int x, Int y -> Int Int32.(x + y)
  
  let (+) = plus

  (** [minus x y] computes [x − y] in the extended domain.
      @raise Failure on undefined forms ([Infinity − Infinity] or
      [NegInfinity − NegInfinity]). *)
  let minus (x : t) (y : t) : t =
    match x, y with 
    | Infinity, Infinity ->  invalid_arg "Infinity - Infinity is undefined"
    | NegInfinity, NegInfinity -> invalid_arg "NegInfinity - NegInfinity is undefined"
    | NegInfinity, _ | _, Infinity -> NegInfinity
    | Infinity, _ | _, NegInfinity -> Infinity
    | Int x, Int y -> Int Int32.(x - y)

  let (-) = minus

  (** [times x y] multiplies two extended integers.
      By convention, [0 * Infinity] and [0 * NegInfinity] return [0]. *)
  let times (x : t) (y : t) : t =
    match x, y with 
    | Int 0l, _ | _, Int 0l -> Int 0l
    | Infinity, i | i, Infinity -> if is_positive i then Infinity else NegInfinity
    | NegInfinity, i | i, NegInfinity -> if is_negative i then Infinity else NegInfinity
    | Int x, Int y -> Int Int32.(x * y)

  let ( * ) = times
  
  (** [divide x y] computes integer division in the extended domain.
      Finite / finite uses truncating division on [int32].
      [Int _] divided by ±∞ yields [Int 0l].
      @raise Failure on division by zero or on indeterminate forms (±∞/±∞). *)
  let divide (x : t) (y : t) : t =
    match x, y with 
    | _, Int 0l -> invalid_arg "division by 0"
    | Infinity, Infinity | Infinity, NegInfinity | NegInfinity, Infinity | NegInfinity, NegInfinity 
      -> invalid_arg "division error: ±∞/±∞"
    | Int _, Infinity | Int _, NegInfinity -> Int 0l
    | Infinity, i -> if is_positive i then Infinity else NegInfinity
    | NegInfinity, i -> if is_negative i then Infinity else NegInfinity
    | Int x, Int y -> Int Int32.(x / y)

  let (/) = divide

  (** [divide_ceiling x y] divides and rounds toward +∞ when both operands are
      finite and the result is not exact; otherwise returns the exact quotient.
      Signs are determined from the product [x*y].  Delegates to [divide] for
      non‑finite cases, so finite values divided by ±∞ return [0]. *)
  let divide_ceiling (x : t) (y : t) : t =
    match x, y with
    | Int a, Int b when not Int32.(a % b = 0l) ->
      if is_positive (times x y) then
        plus (divide x y) (Int 1l)
      else
        divide x y
    | _ -> divide x y

  (** [divide_floor x y] divides and rounds toward −∞ when both operands are
      finite and the result is not exact; otherwise returns the exact quotient.
      Delegates to [divide] for non‑finite cases, so finite values divided by
      ±∞ return [0]. *)
  let divide_floor (x : t) (y : t) : t =
    match x, y with
    | Int a, Int b when not Int32.(a % b = 0l) ->
      if is_positive (times x y) then
        divide x y
      else
        minus (divide x y) (Int 1l)
    | _ -> divide x y
end

(** [gcd a b] is the greatest common divisor of [a] and [b] computed with
    Euclid’s algorithm over [int32].  This helper does not normalize signs:
    when negative inputs are provided, the sign follows OCaml's remainder
    behaviour.  In the intended use cases, moduli are expected to be positive. *)
let rec gcd (a : int32) (b : int32) : int32 =
  if Int32.(b = 0l || b = a) then 
    a
  else
    gcd b Int32.(a % b)

(** [lcm a b] is the least common multiple of [a] and [b].  If either input
    is zero, returns [0l].  Computed as [abs ((a*b) / gcd a b)] using [int32]
    arithmetic, so intermediate products may overflow.  In the intended use
    cases, moduli are expected to be positive. *)
let lcm (a : int32) (b : int32) : int32 =
  if Int32.(a = 0l || b = 0l) then 
    0l
  else if Int32.(a = b) then
    a 
  else 
    Int32.(abs ((a * b) / (gcd a b)))


(** Extended Euclid.
    [extended_gcd a b] returns [(d, x, y)] such that [d = gcd a b] according
    to this module's [gcd], and [d = a*x + b*y].  Coefficients [x] and [y] are
    not canonical.  In the intended use cases, inputs are non-negative.
    @return a triple [(d, x, y)]. *)
let extended_gcd (a : int32) (b : int32) : int32 * int32 * int32 =
  let rec aux (a : int32) (b : int32) x0 x1 y0 y1 =
    if Int32.(b = 0l) then 
      (a, x0, y0)
    else
      let q = Int32.(a / b) in
      aux b (Int32.(a % b)) x1 Int32.(x0 - (q * x1)) y1 Int32.(y0 - (q * y1))
  in
  aux a b 1l 0l 0l 1l

(** Chinese Remainder Theorem for two positive moduli.
    Solves the system [x ≡ b (mod a)] and [x ≡ b' (mod a')].  When a solution
    exists, returns [(x0, m)] where [m = lcm a a'] and [x0] is the unique
    solution modulo [m] with [0 ≤ x0 < m].
    @raise Failure if the congruences are incompatible, i.e. when
    [(b − b') mod gcd a a' ≠ 0].
    @raise Division_by_zero if one of the moduli is zero. *)
let chinese_remainder (a : int32) (b : int32) (a' : int32) (b' : int32) : int32 * int32 =
  let d = gcd a a' in
  if Int32.((b - b') % d <> 0l) then
    failwith "Incompatible congruences"
  else
    let a1 = Int32.(a / d) in
    let a1' = Int32.(a' / d) in
    let (_, u, _) = extended_gcd a1 a1' in
    let m = lcm a a' in
    let diff = Int32.(b' - b) in
    let k = Int32.(((diff / d) * u) % a1') in
    let n = Int32.(((a * k) + b) % m) in
    (Int32.((n + m) % m), m) (* Ensure positive result *)

(** [remove_first_occurrence ~of_ ~in_] removes the first occurrence of [of_]
    from the list [in_], preserving the order of the remaining elements. *)
let remove_first_occurrence ~(of_ : string) ~(in_ : string list) : string list =
    let rec aux acc = function
      | [] -> List.rev acc
      | y :: ys when String.equal of_ y -> List.rev_append acc ys
      | y :: ys -> aux (y :: acc) ys
    in
    aux [] in_

(** [cancel_negation ~pos ~neg] simplifies symbolic offset tokens.  Tokens are
    strings like ["a"; "b"; "negc"].  Every positive token [x] cancels one
    matching negative token [negx] when present.  Uncancelled positive tokens
    are added to the accumulator [neg], so callers should sort the final result
    if they need a canonical order. *)
let rec cancel_negation ~(pos : string list) ~(neg : string list) : string list =
  match pos with
  | [] -> neg
  | x :: xs ->
    let neg_x = "neg" ^ x in
    if List.mem neg neg_x ~equal:String.equal then
      cancel_negation ~pos:xs ~neg:(remove_first_occurrence ~of_:neg_x ~in_:neg)
    else
      cancel_negation ~pos:xs ~neg:(x :: neg)

(** [add_relative_offsets o1 o2] merges two "+"‑separated offset strings where
    negations are encoded by the prefix ["neg"].  Example:
    [add_relative_offsets "a+negc" "b+c"] = ["a+b"].  Empty strings are treated
    as neutral elements.  The result has matching positive/negative terms
    cancelled and the remaining terms re‑joined by "+" in sorted order. *)
let add_relative_offsets (o1 : string) (o2 : string) : string =
  if String.is_empty o1 then
    o2
  else if String.is_empty o2 then
    o1
  else
    let o1_list = String.split_on_chars o1 ~on:['+'] in
    let o2_list = String.split_on_chars o2 ~on:['+'] in
    let o_list = o1_list @ o2_list in
    let pos = 
      List.sort ~compare:String.compare 
        (List.filter o_list ~f:(fun o -> not (String.is_prefix o ~prefix:"neg")))
    in
    let neg = 
      List.sort ~compare:String.compare 
        (List.filter o_list ~f:(fun o -> String.is_prefix o ~prefix:"neg"))
    in
    let offsets = cancel_negation ~pos ~neg in
    String.concat ~sep:"+" (List.sort ~compare:String.compare offsets)

(** [negate_relative_offset offset] flips the sign of every "+"-separated
    symbolic offset token.  A token [x] becomes [negx], and a token [negx]
    becomes [x].  The returned tokens are sorted and re-joined by ["+"]. *)
let negate_relative_offset (offset : string) : string =
  if String.is_empty offset then
    ""
  else
    let offsets = String.split_on_chars offset ~on:['+'] in
    let offsets = List.map ~f:(fun s -> if String.is_prefix s ~prefix:"neg" then String.drop_prefix s 3 else "neg" ^ s) offsets in
    String.concat ~sep:"+" (List.sort ~compare:String.compare offsets)

(** [is_power_of_two x] returns [true] iff [x] is a strictly positive power of
    two. *)
  let is_power_of_two (x : int32) : bool =
    Int32.(x > 0l) && Int32.popcount x = 1

(** {1:binary Binary helpers}
    Utilities for reasoning about bit patterns on non-negative [int32] values.
    These functions are designed for static analyses where we care about the
    first step at which a given bit toggles under repeated increments.
*)
module Binary = struct

  (** [bit_flip x ~increment ~bit] returns the first positive step [n] such that
      bit [bit] differs between [x] and [x + n * increment].  Returns [0] when
      the bit will never flip, for example when [increment = 0] or when the
      increment cannot affect that low bit.  Requires [0 <= bit < 32] and
      [increment >= 0].  The implementation converts some intermediate values
      to OCaml [int], so it is intended for values that fit safely in [int]. *)
  let rec bit_flip (x : int32) ~(increment : int32) ~(bit : int) : int =
    assert (bit < 32 && bit >= 0);
    assert Int32.(increment >= 0l);
    if Int32.(increment = 0l) then
      0
    else
      let t = Int32.ctz increment in
      if t > 0 then
        if bit < t then
          0
        else
          bit_flip (Int32.shift_right x t) ~increment:(Int32.shift_right increment t) ~bit:(bit - t)
      else
        let m = Int.pow 2 (bit + 1)
        and h = Int.pow 2 bit in
        let r = Int32.to_int_exn x % m
        and s = Int32.to_int_exn increment % m in
        if s <= h then
          let distance_to_next_flip = if r < h then h - r else m - r in
          (distance_to_next_flip / s) + if (distance_to_next_flip % s) = 0 then 0 else 1
        else (* s > h *)
          let distance_to_next_flip = if r < h then r + 1 else r - (h - 1) in
          let s' = m - s in
          (distance_to_next_flip / s') + if (distance_to_next_flip % s') = 0 then 0 else 1

  (** [bit_flips x ~increment] maps [bit_flip x ~increment] over bit positions
      [0] through [31].
      @return a list of 32 integers where the [i]-th entry is the first flip
      time for bit [i], or [0] if that bit never flips. *)
  let bit_flips (x : int32) ~(increment : int32) : int list =
    List.init 32 ~f:(fun n -> n)
    |> List.map ~f:(fun bit -> bit_flip x ~increment ~bit)
end


(*
TTTTTTTTTTTTTTTTTTTTTTTEEEEEEEEEEEEEEEEEEEEEE   SSSSSSSSSSSSSSS TTTTTTTTTTTTTTTTTTTTTTT   SSSSSSSSSSSSSSS 
T:::::::::::::::::::::TE::::::::::::::::::::E SS:::::::::::::::ST:::::::::::::::::::::T SS:::::::::::::::S
T:::::::::::::::::::::TE::::::::::::::::::::ES:::::SSSSSS::::::ST:::::::::::::::::::::TS:::::SSSSSS::::::S
T:::::TT:::::::TT:::::TEE::::::EEEEEEEEE::::ES:::::S     SSSSSSST:::::TT:::::::TT:::::TS:::::S     SSSSSSS
TTTTTT  T:::::T  TTTTTT  E:::::E       EEEEEES:::::S            TTTTTT  T:::::T  TTTTTTS:::::S            
        T:::::T          E:::::E             S:::::S                    T:::::T        S:::::S            
        T:::::T          E::::::EEEEEEEEEE    S::::SSSS                 T:::::T         S::::SSSS         
        T:::::T          E:::::::::::::::E     SS::::::SSSSS            T:::::T          SS::::::SSSSS    
        T:::::T          E:::::::::::::::E       SSS::::::::SS          T:::::T            SSS::::::::SS  
        T:::::T          E::::::EEEEEEEEEE          SSSSSS::::S         T:::::T               SSSSSS::::S 
        T:::::T          E:::::E                         S:::::S        T:::::T                    S:::::S
        T:::::T          E:::::E       EEEEEE            S:::::S        T:::::T                    S:::::S
      TT:::::::TT      EE::::::EEEEEEEE:::::ESSSSSSS     S:::::S      TT:::::::TT      SSSSSSS     S:::::S
      T:::::::::T      E::::::::::::::::::::ES::::::SSSSSS:::::S      T:::::::::T      S::::::SSSSSS:::::S
      T:::::::::T      E::::::::::::::::::::ES:::::::::::::::SS       T:::::::::T      S:::::::::::::::SS 
      TTTTTTTTTTT      EEEEEEEEEEEEEEEEEEEEEE SSSSSSSSSSSSSSS         TTTTTTTTTTT       SSSSSSSSSSSSSSS   
*)

let%test_module "Maths tests" = (module struct
  open Binary

  let%test "Bitfield_tests" =
    print_endline "\n_______ ___________________ _______\n        Maths.Binary Module        \n------- ------------------- -------\n"; true
  let%test "bit_flip test no 1" =
    let x = 5l and y = 3l and i = 1 in
    let got = bit_flip x ~increment:y ~bit:i in
    Printf.printf "[bitFlip] x=%ld increment=%ld bit=%d -> %d\n" x y i got; Out_channel.flush stdout;
    got = 2

  let%test "bit_flip test no 2" =
    let x = 5l and y = 1l and i = 1 in
    let got = bit_flip x ~increment:y ~bit:i in
    Printf.printf "[bitFlip] x=%ld increment=%ld bit=%d -> %d\n" x y i got; Out_channel.flush stdout;
    got = 1

  let%test "bit_flip test no 3" =
    let x = 5l and y = 3l and i = 2 in
    let got = bit_flip x ~increment:y ~bit:i in
    Printf.printf "[bitFlip] x=%ld increment=%ld bit=%d -> %d\n" x y i got; Out_channel.flush stdout;
    got = 1

  let%test "bit_flip test no 4" =
    let x = 0l and y = 3l and i = 2 in
    let got = bit_flip x ~increment:y ~bit:i in
    Printf.printf "[bitFlip] x=%ld increment=%ld bit=%d -> %d\n" x y i got; Out_channel.flush stdout;
    got = 2

  let%test "bit_flip test no 5" =
    let x = 0l and y = 5l and i = 2 in
    let got = bit_flip x ~increment:y ~bit:i in
    Printf.printf "[bitFlip] x=%ld increment=%ld bit=%d -> %d\n" x y i got; Out_channel.flush stdout;
    got = 1

  let%test "bit_flip test no 6" =
    let x = 3l and y = 5l and i = 2 in
    let got = bit_flip x ~increment:y ~bit:i in
    Printf.printf "[bitFlip] x=%ld increment=%ld bit=%d -> %d\n" x y i got; Out_channel.flush stdout;
    got = 2

  let%test "bit_flip test no 7" =
    let x = 3l and y = 7l and i = 2 in
    let got = bit_flip x ~increment:y ~bit:i in
    Printf.printf "[bitFlip] x=%ld increment=%ld bit=%d -> %d\n" x y i got; Out_channel.flush stdout;
    got = 4

  let%test "bit_flip test no 8" =
    let x = 2l and y = 7l and i = 2 in
    let got = bit_flip x ~increment:y ~bit:i in
    Printf.printf "[bitFlip] x=%ld increment=%ld bit=%d -> %d\n" x y i got; Out_channel.flush stdout;
    got = 3

  let%test "bit_flip test no 9" =
    let x = 4l and y = 7l and i = 2 in
    let got = bit_flip x ~increment:y ~bit:i in
    Printf.printf "[bitFlip] x=%ld increment=%ld bit=%d -> %d\n" x y i got; Out_channel.flush stdout;
    got = 1

  let%test "bit_flip test no 10" =
    let x = 5l and y = 7l and i = 2 in
    let got = bit_flip x ~increment:y ~bit:i in
    Printf.printf "[bitFlip] x=%ld increment=%ld bit=%d -> %d\n" x y i got; Out_channel.flush stdout;
    got = 2

  let%test "bit_flip test no 11" =
    let x = 5l and y = 3l and i = 1 in
    let got = bit_flip x ~increment:y ~bit:i in
    Printf.printf "[bitFlip] x=%ld increment=%ld bit=%d -> %d\n" x y i got; Out_channel.flush stdout;
    got = 2

  let%test "bit_flip test no 12" =
    let x = 4l and y = 3l and i = 2 in
    let got = bit_flip x ~increment:y ~bit:i in
    Printf.printf "[bitFlip] x=%ld increment=%ld bit=%d -> %d\n" x y i got; Out_channel.flush stdout;
    got = 2

  let%test "bit_flip test no 13" =
    let x = 5l and y = 3l and i = 4 in
    let got = bit_flip x ~increment:y ~bit:i in
    Printf.printf "[bitFlip] x=%ld increment=%ld bit=%d -> %d\n" x y i got; Out_channel.flush stdout;
    got = 4

  let%test "bit_flip test no 14" =
    let x = 5l and y = 3l and i = 0 in
    let got = bit_flip x ~increment:y ~bit:i in
    Printf.printf "[bitFlip] x=%ld increment=%ld bit=%d -> %d\n" x y i got; Out_channel.flush stdout;
    got = 1

  let%test "bit_flip test no 15" =
    let x = 5l and y = 4l and i = 4 in
    let got = bit_flip x ~increment:y ~bit:i in
    Printf.printf "[bitFlip] x=%ld increment=%ld bit=%d -> %d\n" x y i got; Out_channel.flush stdout;
    got = 3

  let%test "bit_flip test no 16" =
    let x = 1l and y = 1l and i = 1 in
    let got = bit_flip x ~increment:y ~bit:i in
    Printf.printf "[bitFlip] x=%ld increment=%ld bit=%d -> %d\n" x y i got; Out_channel.flush stdout;
    got = 1

  let%test "bit_flip y=0 -> 0" =
    let x = 123l and y = 0l and i = 5 in
    let got = bit_flip x ~increment:y ~bit:i in
    Printf.printf "[bitFlip] x=%ld increment=%ld bit=%d -> %d\n" x y i got; Out_channel.flush stdout;
    got = 0

  let%test "bit_flip i < t -> 0" =
    let x = 1l and y = 8l and i = 1 in
    let got = bit_flip x ~increment:y ~bit:i in
    Printf.printf "[bitFlip] x=%ld increment=%ld bit=%d -> %d\n" x y i got; Out_channel.flush stdout;
    got = 0

  let%test "bit_flips 1" =
    let x = 5l and y = 3l in
    let got = bit_flips x ~increment:y in
    Printf.printf "[bitFlips] x=%ld increment=%ld -> %s\n" x y (List.to_string ~f:Int.to_string got); Out_channel.flush stdout; 
    got 
    |> List.to_array
    |> Array.for_alli ~f:(fun bit flip_time -> flip_time = (bit_flip x ~increment:y ~bit))

  let%test "bit_flips 2" =
    let x = 5l and y = 4l in
    let got = bit_flips x ~increment:y in
    Printf.printf "[bitFlips] x=%ld increment=%ld -> %s\n" x y (List.to_string ~f:Int.to_string got); Out_channel.flush stdout; 
    got 
    |> List.to_array
    |> Array.for_alli ~f:(fun bit flip_time -> flip_time = (bit_flip x ~increment:y ~bit))

  (* ---------------- ExtendedInt tests ---------------- *)
  open ExtendedInt

  let%test "ExtendedInt_tests" =
    print_endline "\n_______ ________________________ _______\n        Maths.ExtendedInt Module        \n------- ------------------------ -------\n"; true

  (* to_string *)
  let%test "extint to_string int" =
    let v = Int 42l in
    let got = to_string v in
    Printf.printf "[to_string] v=%s -> %s\n" (to_string v) got; Out_channel.flush stdout;
    String.equal got "42"

  let%test "extint to_string infinities" =
    let v1 = Infinity and v2 = NegInfinity in
    let s1 = to_string v1 and s2 = to_string v2 in
    Printf.printf "[to_string] v=%s -> %s; v=%s -> %s\n" (to_string v1) s1 (to_string v2) s2; Out_channel.flush stdout;
    String.equal s1 "∞" && String.equal s2 "-∞"

  (* maximum / minimum *)
  let%test "extint maximum" =
    let x = Int 3l and y = Int 5l in
    let got = maximum x y in
    Printf.printf "[maximum] x=%s y=%s -> %s\n" (to_string x) (to_string y) (to_string got); Out_channel.flush stdout;
    equal got (Int 5l)

  let%test "extint minimum" =
    let x = Int 3l and y = Int 5l in
    let got = minimum x y in
    Printf.printf "[minimum] x=%s y=%s -> %s\n" (to_string x) (to_string y) (to_string got); Out_channel.flush stdout;
    equal got (Int 3l)

  let%test "extint maximum with infinity" =
    let x = Int 10l and y = Infinity in
    let got = maximum x y in
    Printf.printf "[maximum] x=%s y=%s -> %s\n" (to_string x) (to_string y) (to_string got); Out_channel.flush stdout;
    equal got Infinity

  let%test "extint minimum with neginfinity" =
    let x = NegInfinity and y = Int 10l in
    let got = minimum x y in
    Printf.printf "[minimum] x=%s y=%s -> %s\n" (to_string x) (to_string y) (to_string got); Out_channel.flush stdout;
    equal got NegInfinity

  let%test "extint maximum equal" =
    let x = Int 4l and y = Int 4l in
    let got = maximum x y in
    Printf.printf "[maximum] x=%s y=%s -> %s\n" (to_string x) (to_string y) (to_string got); Out_channel.flush stdout;
    equal got (Int 4l)

  (* less_than *)
  let%test "extint less_than finite" =
    let x = Int 3l and y = Int 5l in
    let got = less_than x y in
    Printf.printf "[less_than] x=%s y=%s -> %b\n" (to_string x) (to_string y) got; Out_channel.flush stdout;
    got

  let%test "extint less_than vs infinity" =
    let x = Int 3l and y = Infinity in
    let got = less_than x y in
    Printf.printf "[less_than] x=%s y=%s -> %b\n" (to_string x) (to_string y) got; Out_channel.flush stdout;
    got

  let%test "extint less_than finite false" =
    let x = Int 5l and y = Int 3l in
    let got = less_than x y in
    Printf.printf "[less_than] x=%s y=%s -> %b\n" (to_string x) (to_string y) got; Out_channel.flush stdout;
    Bool.equal got false

  let%test "extint less_than equal false" =
    let x = Int 4l and y = Int 4l in
    let got = less_than x y in
    Printf.printf "[less_than] x=%s y=%s -> %b\n" (to_string x) (to_string y) got; Out_channel.flush stdout;
    Bool.equal got false

  let%test "extint less_than vs neginfinity false" =
    let x = Int 0l and y = NegInfinity in
    let got = less_than x y in
    Printf.printf "[less_than] x=%s y=%s -> %b\n" (to_string x) (to_string y) got; Out_channel.flush stdout;
    Bool.equal got false

  let%test "extint less_than infinity false" =
    let x = Infinity and y = Int 5l in
    let got = less_than x y in
    Printf.printf "[less_than] x=%s y=%s -> %b\n" (to_string x) (to_string y) got; Out_channel.flush stdout;
    Bool.equal got false

  let%test "extint less_than neginf true" =
    let x = NegInfinity and y = Int (-100l) in
    let got = less_than x y in
    Printf.printf "[less_than] x=%s y=%s -> %b\n" (to_string x) (to_string y) got; Out_channel.flush stdout;
    got

  (* is_positive / is_negative *)
  let%test "extint is_positive zero" =
    let x = Int 0l in
    let got = is_positive x in
    Printf.printf "[is_positive] x=%s -> %b\n" (to_string x) got; Out_channel.flush stdout;
    got

  let%test "extint is_negative zero" =
    let x = Int 0l in
    let got = is_negative x in
    Printf.printf "[is_negative] x=%s -> %b\n" (to_string x) got; Out_channel.flush stdout;
    got

  let%test "extint is_positive finite negative -> false" =
    let x = Int (-1l) in
    let got = is_positive x in
    Printf.printf "[is_positive] x=%s -> %b\n" (to_string x) got; Out_channel.flush stdout;
    Bool.equal got false

  let%test "extint is_negative finite positive -> false" =
    let x = Int 7l in
    let got = is_negative x in
    Printf.printf "[is_negative] x=%s -> %b\n" (to_string x) got; Out_channel.flush stdout;
    Bool.equal got false

  let%test "extint is_positive finite pos" =
    let x = Int 7l in
    let got = is_positive x in
    Printf.printf "[is_positive] x=%s -> %b\n" (to_string x) got; Out_channel.flush stdout; got

  let%test "extint is_negative finite neg" =
    let x = Int (-3l) in
    let got = is_negative x in
    Printf.printf "[is_negative] x=%s -> %b\n" (to_string x) got; Out_channel.flush stdout; got

  let%test "extint is_positive infinity" =
    let x = Infinity in
    let got = is_positive x in
    Printf.printf "[is_positive] x=%s -> %b\n" (to_string x) got; Out_channel.flush stdout; got

  let%test "extint is_negative neginfinity" =
    let x = NegInfinity in
    let got = is_negative x in
    Printf.printf "[is_negative] x=%s -> %b\n" (to_string x) got; Out_channel.flush stdout; got

  let%test "extint is_positive neginfinity -> false" =
    let x = NegInfinity in
    let got = is_positive x in
    Printf.printf "[is_positive] x=%s -> %b\n" (to_string x) got; Out_channel.flush stdout;
    Bool.equal got false

  let%test "extint is_negative infinity -> false" =
    let x = Infinity in
    let got = is_negative x in
    Printf.printf "[is_negative] x=%s -> %b\n" (to_string x) got; Out_channel.flush stdout;
    Bool.equal got false

  (* plus *)
  let%test "extint plus finite" =
    let x = Int 2l and y = Int 3l in
    let got = plus x y in
    Printf.printf "[plus] x=%s y=%s -> %s\n" (to_string x) (to_string y) (to_string got); Out_channel.flush stdout;
    equal got (Int 5l)

  let%test "extint plus raises" =
    let x = Infinity and y = NegInfinity in
    try let _ = plus x y in
        Printf.printf "[plus] x=%s y=%s -> (no failure)\n" (to_string x) (to_string y); Out_channel.flush stdout; false
    with Invalid_argument _ ->
      Printf.printf "[plus] x=%s y=%s -> fail\n" (to_string x) (to_string y); Out_channel.flush stdout; true

  let%test "extint plus infinity with finite" =
    let x = Infinity and y = Int 1l in
    let got = plus x y in
    Printf.printf "[plus] x=%s y=%s -> %s\n" (to_string x) (to_string y) (to_string got); Out_channel.flush stdout; equal got Infinity

  let%test "extint plus neginfinity with finite" =
    let x = NegInfinity and y = Int (-5l) in
    let got = plus x y in
    Printf.printf "[plus] x=%s y=%s -> %s\n" (to_string x) (to_string y) (to_string got); Out_channel.flush stdout; equal got NegInfinity

  let%test "extint plus raises (commuted)" =
    let x = NegInfinity and y = Infinity in
    (try let _ = plus x y in
         Printf.printf "[plus] x=%s y=%s -> (no failure)\n" (to_string x) (to_string y); Out_channel.flush stdout; false
     with Invalid_argument _ -> Printf.printf "[plus] x=%s y=%s -> fail\n" (to_string x) (to_string y); Out_channel.flush stdout; true)

  (* minus *)
  let%test "extint minus infinity - finite" =
    let x = Infinity and y = Int 9l in
    let got = minus x y in
    Printf.printf "[minus] x=%s y=%s -> %s\n" (to_string x) (to_string y) (to_string got); Out_channel.flush stdout; equal got Infinity

  let%test "extint minus finite - infinity" =
    let x = Int 9l and y = Infinity in
    let got = minus x y in
    Printf.printf "[minus] x=%s y=%s -> %s\n" (to_string x) (to_string y) (to_string got); Out_channel.flush stdout; equal got NegInfinity

  let%test "extint minus neginfinity - finite" =
    let x = NegInfinity and y = Int 2l in
    let got = minus x y in
    Printf.printf "[minus] x=%s y=%s -> %s\n" (to_string x) (to_string y) (to_string got); Out_channel.flush stdout; equal got NegInfinity

  let%test "extint minus finite - neginfinity" =
    let x = Int 2l and y = NegInfinity in
    let got = minus x y in
    Printf.printf "[minus] x=%s y=%s -> %s\n" (to_string x) (to_string y) (to_string got); Out_channel.flush stdout; equal got Infinity

  let%test "extint minus raises infinity - infinity" =
    let x = Infinity and y = Infinity in
    (try let _ = minus x y in
         Printf.printf "[minus] x=%s y=%s -> (no failure)\n" (to_string x) (to_string y); Out_channel.flush stdout; false
     with Invalid_argument _ -> Printf.printf "[minus] x=%s y=%s -> fail\n" (to_string x) (to_string y); Out_channel.flush stdout; true)

  let%test "extint minus raises neginf - neginf" =
    let x = NegInfinity and y = NegInfinity in
    (try let _ = minus x y in
         Printf.printf "[minus] x=%s y=%s -> (no failure)\n" (to_string x) (to_string y); Out_channel.flush stdout; false
     with Invalid_argument _ -> Printf.printf "[minus] x=%s y=%s -> fail\n" (to_string x) (to_string y); Out_channel.flush stdout; true)

  let%test "extint minus finite" =
    let x = Int 5l and y = Int 2l in
    let got = minus x y in
    Printf.printf "[minus] x=%s y=%s -> %s\n" (to_string x) (to_string y) (to_string got); Out_channel.flush stdout;
    equal got (Int 3l)

  (* times *)
  let%test "extint times zero shortcircuit" =
    let x = Int 0l and y = Infinity in
    let got = times x y in
    Printf.printf "[times] x=%s y=%s -> %s\n" (to_string x) (to_string y) (to_string got); Out_channel.flush stdout;
    equal got (Int 0l)

  let%test "extint times sign with infinity" =
    let x = Int (-2l) and y = Infinity in
    let got = times x y in
    Printf.printf "[times] x=%s y=%s -> %s\n" (to_string x) (to_string y) (to_string got); Out_channel.flush stdout;
    equal got NegInfinity

  let%test "extint times finite*finite neg" =
    let x = Int (-3l) and y = Int 2l in
    let got = times x y in
    Printf.printf "[times] x=%s y=%s -> %s\n" (to_string x) (to_string y) (to_string got); Out_channel.flush stdout; equal got (Int (-6l))

  let%test "extint times pos * neginf" =
    let x = Int 3l and y = NegInfinity in
    let got = times x y in
    Printf.printf "[times] x=%s y=%s -> %s\n" (to_string x) (to_string y) (to_string got); Out_channel.flush stdout; equal got NegInfinity

  let%test "extint times neg * neginf -> +inf" =
    let x = Int (-3l) and y = NegInfinity in
    let got = times x y in
    Printf.printf "[times] x=%s y=%s -> %s\n" (to_string x) (to_string y) (to_string got); Out_channel.flush stdout; equal got Infinity

  (* divide *)
  let%test "extint divide negative by positive" =
    let x = Int (-7l) and y = Int 2l in
    let got = divide x y in
    Printf.printf "[divide] x=%s y=%s -> %s\n" (to_string x) (to_string y) (to_string got); Out_channel.flush stdout; equal got (Int (-3l))

  let%test "extint divide by zero raises" =
    let x = Int 7l and y = Int 0l in
    (try let _ = divide x y in
         Printf.printf "[divide] x=%s y=%s -> (no failure)\n" (to_string x) (to_string y); Out_channel.flush stdout; false
     with Invalid_argument _ -> Printf.printf "[divide] x=%s y=%s -> fail\n" (to_string x) (to_string y); Out_channel.flush stdout; true)

  let%test "extint divide infinity / finite sign+" =
    let x = Infinity and y = Int 2l in
    let got = divide x y in
    Printf.printf "[divide] x=%s y=%s -> %s\n" (to_string x) (to_string y) (to_string got); Out_channel.flush stdout; equal got Infinity

  let%test "extint divide infinity / finite sign-" =
    let x = Infinity and y = Int (-2l) in
    let got = divide x y in
    Printf.printf "[divide] x=%s y=%s -> %s\n" (to_string x) (to_string y) (to_string got); Out_channel.flush stdout; equal got NegInfinity

  let%test "extint divide neginf / neg finite -> +inf" =
    let x = NegInfinity and y = Int (-2l) in
    let got = divide x y in
    Printf.printf "[divide] x=%s y=%s -> %s\n" (to_string x) (to_string y) (to_string got); Out_channel.flush stdout; equal got Infinity

  let%test "extint divide finite / neginf -> 0" =
    let x = Int 123l and y = NegInfinity in
    let got = divide x y in
    Printf.printf "[divide] x=%s y=%s -> %s\n" (to_string x) (to_string y) (to_string got); Out_channel.flush stdout; equal got (Int 0l)

  let%test "extint divide raises inf/inf" =
    let x = Infinity and y = Infinity in
    (try let _ = divide x y in
         Printf.printf "[divide] x=%s y=%s -> (no failure)\n" (to_string x) (to_string y); Out_channel.flush stdout; false
     with Invalid_argument _ -> Printf.printf "[divide] x=%s y=%s -> fail\n" (to_string x) (to_string y); Out_channel.flush stdout; true)

  let%test "extint divide finite" =
    let x = Int 7l and y = Int 2l in
    let got = divide x y in
    Printf.printf "[divide] x=%s y=%s -> %s\n" (to_string x) (to_string y) (to_string got); Out_channel.flush stdout;
    equal got (Int 3l)

  let%test "extint divide by infinity" =
    let x = Int 5l and y = Infinity in
    let got = divide x y in
    Printf.printf "[divide] x=%s y=%s -> %s\n" (to_string x) (to_string y) (to_string got); Out_channel.flush stdout;
    equal got (Int 0l)

  (* divide_ceiling / divide_floor *)
  let%test "extint divide_ceiling" =
    let x = Int 7l and y = Int 2l in
    let got = divide_ceiling x y in
    Printf.printf "[divide_ceiling] x=%s y=%s -> %s\n" (to_string x) (to_string y) (to_string got); Out_channel.flush stdout;
    equal got (Int 4l)

  let%test "extint divide_floor negative" =
    let x = Int (-7l) and y = Int 2l in
    let got = divide_floor x y in
    Printf.printf "[divide_floor] x=%s y=%s -> %s\n" (to_string x) (to_string y) (to_string got); Out_channel.flush stdout;
    equal got (Int (-4l))

  let%test "extint divide_ceiling exact" =
    let x = Int 8l and y = Int 2l in
    let got = divide_ceiling x y in
    Printf.printf "[divide_ceiling] x=%s y=%s -> %s\n" (to_string x) (to_string y) (to_string got); Out_channel.flush stdout; equal got (Int 4l)

  let%test "extint divide_floor exact negative" =
    let x = Int (-8l) and y = Int 2l in
    let got = divide_floor x y in
    Printf.printf "[divide_floor] x=%s y=%s -> %s\n" (to_string x) (to_string y) (to_string got); Out_channel.flush stdout; equal got (Int (-4l))

  (* ---------------- Number theory & string helpers ---------------- *)
  let%test "gcd basic" =
    let a, b = 54l, 24l in
    let got = gcd a b in
    Printf.printf "[gcd] a=%ld b=%ld -> %ld\n" a b got; Out_channel.flush stdout; Int32.equal got 6l

  let%test "lcm basic" =
    let a, b = 6l, 15l in
    let got = lcm a b in
    Printf.printf "[lcm] a=%ld b=%ld -> %ld\n" a b got; Out_channel.flush stdout; Int32.equal got 30l

  let%test "lcm zero" =
    let a, b = 0l, 15l in
    let got = lcm a b in
    Printf.printf "[lcm] a=%ld b=%ld -> %ld\n" a b got; Out_channel.flush stdout; Int32.equal got 0l

  let%test "extended_gcd identity" =
    let a, b = 99l, 78l in
    let d, x, y = extended_gcd a b in
    let lhs = Int32.(a * x + b * y) in
    Printf.printf "[extended_gcd] a=%ld b=%ld -> d=%ld, x=%ld, y=%ld ; ax+by=%ld\n" a b d x y lhs; Out_channel.flush stdout; Int32.equal d (gcd a b) && Int32.equal lhs d

  let%test "crt solvable" =
    let a, b = 3l, 2l and a', b' = 5l, 3l in
    let x0, m = chinese_remainder a b a' b' in
    Printf.printf "[crt] a=%ld b=%ld ; a'=%ld b'=%ld -> x0=%ld (mod %ld)\n" a b a' b' x0 m; Out_channel.flush stdout; Int32.equal m 15l && Int32.(x0 % 3l = 2l) && Int32.(x0 % 5l = 3l)

  let%test "crt incompatible raises" =
    let a, b = 4l, 1l and a', b' = 6l, 2l in
    (try let _ = chinese_remainder a b a' b' in
         Printf.printf "[crt] ... -> (no failure)\n"; Out_channel.flush stdout; false
     with Failure _ -> Printf.printf "[crt] ... -> fail\n"; Out_channel.flush stdout; true)

  let%test "remove_first_occurrence present" =
    let got = remove_first_occurrence ~of_:"b" ~in_:["a";"b";"c";"b"] in
    Printf.printf "[remove_first_occurrence] (a b c b) \\ b -> %s\n" (List.to_string ~f:Fn.id got); Out_channel.flush stdout; List.equal String.equal got ["a";"c";"b"]

  let%test "remove_first_occurrence absent" =
    let got = remove_first_occurrence ~of_:"x" ~in_:["a";"b"] in
    Printf.printf "[remove_first_occurrence] (a b) \\ x -> %s\n" (List.to_string ~f:Fn.id got); Out_channel.flush stdout; List.equal String.equal got ["a";"b"]

  let%test "cancel_negation basic" =
    let got = cancel_negation ~pos:["a";"b"] ~neg:["nega";"negx"] in
    Printf.printf "[cancel_negation] [%s] -> [%s]\n" (String.concat ~sep:"; " ["a";"b";"nega";"negx"]) (String.concat ~sep:"; " got); Out_channel.flush stdout; List.equal String.equal got ["b";"negx"]

  let%test "add_relative_offsets merge & cancel" =
    let got = add_relative_offsets "a+negc" "b+c" in
    Printf.printf "[add_relative_offsets] (a+negc) + (b+c) -> %s\n" got; Out_channel.flush stdout; String.equal got "a+b"

  let%test "extint divide_ceiling finite / infinity -> 0" =
    equal (divide_ceiling (Int 10l) Infinity) (Int 0l)

  let%test "extint divide_floor finite / infinity -> 0" =
    equal (divide_floor (Int 10l) Infinity) (Int 0l)

  let%test "is_power_of_two" =
    Bool.equal (is_power_of_two 0l) false
    && Bool.equal (is_power_of_two 1l) true
    && Bool.equal (is_power_of_two 2l) true
    && Bool.equal (is_power_of_two 3l) false
    && Bool.equal (is_power_of_two (-2l)) false

  let%test "negate_relative_offset" =
    String.equal (negate_relative_offset "a+negb+c") "b+nega+negc"

  let%test "add_relative_offsets empty left" =
    String.equal (add_relative_offsets "" "a+b") "a+b"

  let%test "add_relative_offsets empty right" =
    String.equal (add_relative_offsets "a+b" "") "a+b"
end)