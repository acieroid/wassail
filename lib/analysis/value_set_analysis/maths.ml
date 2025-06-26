(** Mathematical utilities for extended integers and number theory.
    Includes arithmetic operations over extended integers (-∞, ∞),
    greatest common divisors, least common multiples, and the 
    Chinese Remainder Theorem.
*)

open Core 

(** The [ExtendedInt] module defines an integer type extended with
    positive and negative infinity, along with arithmetic operations 
    and comparisons adapted to this domain.
*)
module ExtendedInt = struct
  type t = 
    | Int of int
    | Infinity
    | NegInfinity
  [@@deriving sexp, compare, equal]

  (** Convert an extended integer to a string representation. *)
  let to_string (i : t) : string =
    match i with 
    | Int i -> string_of_int i
    | Infinity -> "∞"
    | NegInfinity -> "-∞"

  (** Return the maximum of two extended integers. *)
  let maximum (x : t) (y : t) : t =
    match x, y with 
    | Infinity, _ | _, Infinity -> Infinity
    | NegInfinity, i | i, NegInfinity -> i
    | Int x, Int y -> Int (max x y)

  (** Return the minimum of two extended integers. *)
  let minimum (x : t) (y : t) : t =
    match x, y with 
    | Infinity, i | i, Infinity -> i
    | NegInfinity, _ | _, NegInfinity -> NegInfinity
    | Int x, Int y -> Int (min x y)

  (** Determine if the first extended integer is strictly less than the second. *)
  let less_than (x : t) (y : t) : bool =
    match x, y with 
    | Int x, Int y when x < y -> true 
    | NegInfinity, Int _ | NegInfinity, Infinity -> true
    | Int _, Infinity -> true
    | _ -> false 

  (** Add two extended integers. Raises if the result is undefined. *)
  let plus (x : t) (y : t) : t =
    match x, y with 
    | Infinity, NegInfinity | NegInfinity, Infinity -> failwith "undetermined addition"
    | Infinity, _ | _, Infinity -> Infinity
    | NegInfinity, _ | _, NegInfinity -> NegInfinity
    | Int x, Int y -> Int (x + y)

  (** Subtract the second extended integer from the first. Raises if the result is undefined. *)
  let minus (x : t) (y : t) : t =
    match x, y with 
    | Infinity, Infinity | NegInfinity, NegInfinity -> failwith "undetermined subtraction"
    | NegInfinity, _ | _, Infinity -> NegInfinity
    | Infinity, _ | _, NegInfinity -> Infinity
    | Int x, Int y -> Int (x - y)

  (** Multiply two extended integers. *)
  let times (x : t) (y : t) : t =
    match x, y with 
    | Int 0, _ | _, Int 0 -> Int 0
    | Infinity, i -> if less_than (Int 0) i then Infinity else NegInfinity
    | NegInfinity, i -> if less_than i (Int 0) then Infinity else NegInfinity
    | i, Infinity -> if less_than (Int 0) i then Infinity else NegInfinity
    | i, NegInfinity -> if less_than i (Int 0) then Infinity else NegInfinity
    | Int x, Int y -> Int (x * y)
  
  (** Divide the first extended integer by the second. Raises on division by zero or ∞/∞. *)
  let divide (x : t) (y : t) : t =
    match x, y with 
    | _, Int 0 -> failwith "division by 0"
    | Infinity, Infinity | Infinity, NegInfinity | NegInfinity, Infinity | NegInfinity, NegInfinity 
      -> failwith "division error: ±∞/±∞"
    | Int _, Infinity | Int _, NegInfinity -> Int 0
    | Infinity, i -> if less_than (Int 0) i then Infinity else NegInfinity
    | NegInfinity, i -> if less_than (Int 0) i then NegInfinity else Infinity
    | Int x, Int y -> Int (x / y)

  (** Divide two extended integers and round up to the nearest integer. *)
  let divide_ceiling (x : t) (y : t) : t =
    match x, y with
    | Int a, Int b when not (a mod b = 0) ->
      plus (divide x y) (Int 1)
    | _ -> divide x y
end

(** Compute the greatest common divisor (GCD) of two integers. *)
let rec gcd (a : int) (b : int) : int =
    if b = 0 then 
      a
    else 
      gcd b (a mod b)

(** Compute the least common multiple (LCM) of two integers. *)
let lcm a b =
  if a = 0 || b = 0 then 
    0
  else 
    abs (a * b) / gcd a b


(** Compute the extended greatest common divisor of two integers.
    Returns a triple [(d, x, y)] such that [d = gcd(a, b)] and [d = ax + by].
*)
let extended_gcd (a : int) (b : int) : int * int * int =
  let rec aux (a : int) (b : int) x0 x1 y0 y1 =
    if b = 0 then 
      (a, x0, y0)
    else
      let q = a / b in
      aux b (a mod b) x1 (x0 - q * x1) y1 (y0 - q * y1)
  in
  aux a b 1 0 0 1

(** Solve a system of two congruences using the Chinese Remainder Theorem.
    Given [x ≡ b mod a] and [x ≡ b' mod a'], returns [(x, lcm(a, a'))].
    Raises if the system has no solution.
*)
let chinese_remainder (a : int) (b : int) (a' : int) (b' : int) : int * int =
  let d = gcd a a' in
  if (b - b') mod d <> 0 then
    failwith "Incompatible congruences"
  else
    let a1 = a / d in
    let a1' = a' / d in
    let (_, u, _) = extended_gcd a1 a1' in
    let m = lcm a a' in
    let diff = b' - b in
    let k = (diff / d * u) mod a1' in
    let n = (a * k + b) mod m in
    ((n + m) mod m, m) (* Ensure positive result *)

let remove_first_occurence ~(of_ : string) ~(in_ : string list) : string list =
    let rec aux acc = function
      | [] -> List.rev acc
      | y :: ys when String.equal of_ y -> List.rev_append acc ys
      | y :: ys -> aux (y :: acc) ys
    in
    aux [] in_

  let rec cancel_negation ~(pos : string list) ~(neg : string list) : string list =
    match pos with
    | [] -> neg
    | x :: xs ->
      let neg_x = "neg" ^ x in
      if List.mem neg neg_x ~equal:String.equal then
        cancel_negation ~pos:xs ~neg:(remove_first_occurence ~of_:neg_x ~in_:neg)
      else
        cancel_negation ~pos:xs ~neg:(x :: neg)

  let add_relative_offsets (o1 : string) (o2 : string) : string =
    if String.equal o1 "" then
      o2
    else if String.equal o2 "" then
      o1
    else
      let o1_list = String.split_on_chars o1 ~on:['+'] in
      let o2_list = String.split_on_chars o2 ~on:['+'] in
      let o_list = o1_list @ o2_list in
      let pos = 
        List.rev 
          (List.sort ~compare:String.compare (List.fold o_list ~init:[] ~f:(fun acc o -> 
            if String.is_prefix o ~prefix:"neg" then
              acc
            else
              o :: acc)))
      in
      let neg = 
        List.sort ~compare:String.compare (List.fold o_list ~init:[] ~f:(fun acc o -> 
          if String.is_prefix o ~prefix:"neg" then
            o ::acc
          else
            acc))
      in
      let offsets = cancel_negation ~pos ~neg in
      String.concat ~sep:"+" offsets 



      (* let%test "add_relative_offsets cancels negation" =
      let result = add_relative_offsets "a+negb+negd" "c+b+d+b" in
      print_endline "Test: add_relative_offsets \"a+negb\" \"b+b+c\"";
      print_endline ("Result: " ^ result);
      print_endline "Expected: a+b+c";
      String.equal result "a+b+c" *)