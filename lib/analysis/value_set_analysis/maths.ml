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
    | Int of int32
    | Infinity
    | NegInfinity
  [@@deriving sexp, compare, equal]

  (** Convert an extended integer to a string representation. *)
  let to_string (i : t) : string =
    match i with 
    | Int i -> Int32.to_string i
    | Infinity -> "∞"
    | NegInfinity -> "-∞"

  (** Return the maximum of two extended integers. *)
  let maximum (x : t) (y : t) : t =
    match x, y with 
    | Infinity, _ | _, Infinity -> Infinity
    | NegInfinity, i | i, NegInfinity -> i
    | Int x, Int y -> Int (Int32.max x y)

  (** Return the minimum of two extended integers. *)
  let minimum (x : t) (y : t) : t =
    match x, y with 
    | Infinity, i | i, Infinity -> i
    | NegInfinity, _ | _, NegInfinity -> NegInfinity
    | Int x, Int y -> Int (Int32.min x y)

  (** Determine if the first extended integer is strictly less than the second. *)
  let less_than (x : t) (y : t) : bool =
    match x, y with 
    (* | Int x, Int y when x < y -> true  *)
    | Int x, Int y -> Int32.compare x y < 0
    | NegInfinity, Int _ | NegInfinity, Infinity -> true
    | Int _, Infinity -> true
    | _ -> false 

  (** Add two extended integers. Raises if the result is undefined. *)
  let plus (x : t) (y : t) : t =
    match x, y with 
    | Infinity, NegInfinity | NegInfinity, Infinity -> failwith "undetermined addition"
    | Infinity, _ | _, Infinity -> Infinity
    | NegInfinity, _ | _, NegInfinity -> NegInfinity
    | Int x, Int y -> Int (Int32.(+) x y)

  (** Subtract the second extended integer from the first. Raises if the result is undefined. *)
  let minus (x : t) (y : t) : t =
    match x, y with 
    | Infinity, Infinity | NegInfinity, NegInfinity -> failwith "undetermined subtraction"
    | NegInfinity, _ | _, Infinity -> NegInfinity
    | Infinity, _ | _, NegInfinity -> Infinity
    | Int x, Int y -> Int (Int32.(-) x  y)

  (** Multiply two extended integers. *)
  let times (x : t) (y : t) : t =
    match x, y with 
    | Int 0l, _ | _, Int 0l -> Int 0l
    | Infinity, i -> if less_than (Int 0l) i then Infinity else NegInfinity
    | NegInfinity, i -> if less_than i (Int 0l) then Infinity else NegInfinity
    | i, Infinity -> if less_than (Int 0l) i then Infinity else NegInfinity
    | i, NegInfinity -> if less_than i (Int 0l) then Infinity else NegInfinity
    | Int x, Int y -> Int (Int32.( * ) x  y)
  
  (** Divide the first extended integer by the second. Raises on division by zero or ∞/∞. *)
  let divide (x : t) (y : t) : t =
    match x, y with 
    | _, Int 0l -> failwith "division by 0"
    | Infinity, Infinity | Infinity, NegInfinity | NegInfinity, Infinity | NegInfinity, NegInfinity 
      -> failwith "division error: ±∞/±∞"
    | Int _, Infinity | Int _, NegInfinity -> Int 0l
    | Infinity, i -> if less_than (Int 0l) i then Infinity else NegInfinity
    | NegInfinity, i -> if less_than (Int 0l) i then NegInfinity else Infinity
    | Int x, Int y -> Int (Int32.(/) x  y)

  (** Divide two extended integers and round up to the nearest integer. *)
  let divide_ceiling (x : t) (y : t) : t =
    match x, y with
    (* | Int a, Int b when not ((Int32.to_int_exn a) mod (Int32.to_int_exn b) = 0) -> *)
    | Int a, Int b when not (Int32.(a % b = 0l)) ->
      plus (divide x y) (Int 1l)
    | _ -> divide x y
end

(** Compute the greatest common divisor (GCD) of two integers. *)
let rec gcd (a : int32) (b : int32) : int32 =
    if Int32.equal b 0l then 
      a
    else 
      gcd b (Int32.(%) a b)

(** Compute the least common multiple (LCM) of two integers. *)
let lcm (a : int32) (b : int32) : int32 =
  if Int32.equal a 0l || Int32.equal b 0l then 
    0l
  else 
    Int32.abs (Int32.(/) (Int32.( * ) a b) (gcd a b))


(** Compute the extended greatest common divisor of two integers.
    Returns a triple [(d, x, y)] such that [d = gcd(a, b)] and [d = ax + by].
*)
let extended_gcd (a : int32) (b : int32) : int32 * int32 * int32 =
  let rec aux (a : int32) (b : int32) x0 x1 y0 y1 =
    if Int32.equal b 0l then 
      (a, x0, y0)
    else
      let q = Int32.(/) a b in
      (* aux b (a mod b) x1 (x0 - q * x1) y1 (y0 - q * y1) *)
      aux b (Int32.(%) a b) x1 (Int32.(-) x0 (Int32.( * ) q x1)) y1 (Int32.(-) y0 (Int32.( * ) q y1))
  in
  aux a b 1l 0l 0l 1l

(** Solve a system of two congruences using the Chinese Remainder Theorem.
    Given [x ≡ b mod a] and [x ≡ b' mod a'], returns [(x, lcm(a, a'))].
    Raises if the system has no solution.
*)
let chinese_remainder (a : int32) (b : int32) (a' : int32) (b' : int32) : int32 * int32 =
  let d = gcd a a' in
  (* if (Int32.(-) b b') mod d <> 0 then *)
  if not (Int32.equal (Int32.(%) (Int32.(-) b b') d) 0l) then
    failwith "Incompatible congruences"
  else
    let a1 = Int32.(/) a d in
    let a1' = Int32.(/) a' d in
    let (_, u, _) = extended_gcd a1 a1' in
    let m = lcm a a' in
    let diff = Int32.(-) b' b in
    (* let k = (diff / d * u) mod a1' in *)
    let k = Int32.(%) (Int32.( * ) (Int32.(/) diff d) u) (a1') in
    (* let n = (a * k + b) mod m in *)
    let n = Int32.(%) (Int32.(+) (Int32.( * ) a k) b) m in
    (* ((n + m) mod m, m)  *)
    (Int32.(%) (Int32.(+) n m) m, m) (* Ensure positive result *)

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