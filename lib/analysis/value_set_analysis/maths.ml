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
    | Int x, Int y -> Int (Int32.(x / y))

  (** Divide two extended integers and round up to the nearest integer. *)
  let divide_ceiling (x : t) (y : t) : t =
    match x, y with
    | Int a, Int b when not (Int32.(a % b = 0l)) ->
      if less_than (times x y) (Int 0l) then
        divide x y
      else
        plus (divide x y) (Int 1l)
    | _ -> divide x y

  let divide_floor (x : t) (y : t) : t =
    match x, y with
    | Int a, Int b when not (Int32.(a % b = 0l)) ->
      if less_than (times x y) (Int 0l) then
        minus (divide x y) (Int 1l)
      else
        divide x y
    | _ -> divide x y
end

(** Compute the greatest common divisor (GCD) of two integers. *)
let rec gcd (a : int32) (b : int32) : int32 =
  (* print_endline ("a % b:" ^ Int32.to_string a ^ "  " ^Int32.to_string b); *)
  if Int32.equal b 0l then 
    a
  else 
    gcd b (Int32.(a % b))

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


module Binary = struct

  let number_of_trailing_zeros (x : int32) : int =
      let rec aux x n =
        if Int32.(x % 2l = 1l) then
          n
        else
          aux (Int32.(shift_right x 1)) (n + 1) 
      in
      aux x 0

  (* returns 0 if bit never flips *)
  let rec bitFlip (x : int32) (y : int32) (i : int) : int =
    assert (i < 32 && i >= 0);
    assert Int32.(y >= 0l);
    if Int32.(y = 0l) then
      0
    else
      let t = number_of_trailing_zeros y in
      if t > 0 then
        if i < t then
          0
        else
          bitFlip (Int32.shift_right x t) (Int32.shift_right y t) (i - t)
      else
        let m = Int.pow 2 (i + 1)
        and h = Int.pow 2 i in
        let r = Int32.to_int_exn x % m
        and s = Int32.to_int_exn y % m in
        if s <= h then
          let distance_to_next_flip = if r < h then h - r else m - r in
          (distance_to_next_flip / s) + if (distance_to_next_flip % s) = 0 then 0 else 1
        else (* s > h *)
          let distance_to_next_flip = if r < h then r + 1 else r - (h - 1) in
          let s' = m - s in
          (distance_to_next_flip / s') + if (distance_to_next_flip % s') = 0 then 0 else 1

  let bitFlips (x : int32) (y : int32) : int list =
    let idx = List.init 32 ~f:(fun n -> n) in
    List.map idx ~f:(fun i -> bitFlip x y i)




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

let%test_module "Binary tests" = (module struct
  open Binary

  let%test "Bitfield_tests" =
    print_endline "\n_______ ____________________________ _______\n        Maths.Binary Module        \n------- ---------------------------- -------\n"; true
  let%test "bitFlip test no 1" =
    let x = 5l
    and y = 3l in
    print_endline ("[bitFlip " ^ Int32.to_string x ^ " " ^ Int32.to_string y ^ "]");
    let k = bitFlip x y 1 in
    k = 2

  let%test "bitFlip test no 2" =
    let x = 5l 
    and y = 1l in
    print_endline ("[bitFlip " ^ Int32.to_string x ^ " " ^ Int32.to_string y ^ "]");
    let k = bitFlip x y 1 in
    k = 1

  let%test "bitFlip test no 3" =
    let x = 5l
    and y = 3l in
    print_endline ("[bitFlip " ^ Int32.to_string x ^ " " ^ Int32.to_string y ^ "]");
    let k = bitFlip x y 2 in
    k = 1

  let%test "bitFlip test no 4" =
    let x = 0l
    and y = 3l in
    print_endline ("[bitFlip " ^ Int32.to_string x ^ " " ^ Int32.to_string y ^ "]");
    let k = bitFlip x y 2 in
    k = 2

  let%test "bitFlip test no 5" =
    let x = 0l
    and y = 5l in
    print_endline ("[bitFlip " ^ Int32.to_string x ^ " " ^ Int32.to_string y ^ "]");
    let k = bitFlip x y 2 in
    k = 1

  let%test "bitFlip test no 6" =
    let x = 3l
    and y = 5l in
    print_endline ("[bitFlip " ^ Int32.to_string x ^ " " ^ Int32.to_string y ^ "]");
    let k = bitFlip x y 2 in
    k = 2

  let%test "bitFlip test no 7" =
    let x = 3l
    and y = 7l in
    print_endline ("[bitFlip " ^ Int32.to_string x ^ " " ^ Int32.to_string y ^ "]");
    let k = bitFlip x y 2 in
    k = 4

  let%test "bitFlip test no 8" =
    let x = 2l
    and y = 7l in
    print_endline ("[bitFlip " ^ Int32.to_string x ^ " " ^ Int32.to_string y ^ "]");
    let k = bitFlip x y 2 in
    k = 3

  let%test "bitFlip test no 9" =
    let x = 4l
    and y = 7l in
    print_endline ("[bitFlip " ^ Int32.to_string x ^ " " ^ Int32.to_string y ^ "]");
    let k = bitFlip x y 2 in
    k = 1

  let%test "bitFlip test no 10" =
    let x = 5l
    and y = 7l in
    print_endline ("[bitFlip " ^ Int32.to_string x ^ " " ^ Int32.to_string y ^ "]");
    let k = bitFlip x y 2 in
    k = 2

  let%test "bitFlip test no 11" =
    let x = 5l
    and y = 3l in
    print_endline ("[bitFlip " ^ Int32.to_string x ^ " " ^ Int32.to_string y ^ "]");
    let k = bitFlip x y 1 in
    k = 2

  let%test "bitFlip test no 12" =
    let x = 4l
    and y = 3l in
    print_endline ("[bitFlip " ^ Int32.to_string x ^ " " ^ Int32.to_string y ^ "]");
    let k = bitFlip x y 2 in
    k = 2

  let%test "bitFlip test no 13" =
    let x = 5l
    and y = 3l in
    let k = bitFlip x y 4 in
    print_endline ("[bitFlip " ^ Int32.to_string x ^ " " ^ Int32.to_string y ^ " 4" ^  "]\t" ^ string_of_int k);
    k = 4

  let%test "bitFlip test no 14" =
    let x = 5l
    and y = 3l in
    let k = bitFlip x y 0 in
    print_endline ("[bitFlip " ^ Int32.to_string x ^ " " ^ Int32.to_string y ^ " 0" ^  "]\t" ^ string_of_int k);
    k = 1

  let%test "bitFlip test no 15" =
    let x = 5l
    and y = 4l in
    let k = bitFlip x y 4 in
    print_endline ("[bitFlip " ^ Int32.to_string x ^ " " ^ Int32.to_string y ^ " 4" ^  "]\t" ^ string_of_int k);
    k = 3

  let%test "bitFlip test no 16" =
    let x = 1l
    and y = 1l in
    let k = bitFlip x y 1 in
    print_endline ("[bitFlip " ^ Int32.to_string x ^ " " ^ Int32.to_string y ^ " 1" ^  "]\t" ^ string_of_int k);
    k = 1

  let%test "bitFlips 1" =
    let x = 5l 
    and y = 3l in
    print_endline ("[bitFlips " ^ Int32.to_string x ^ " " ^ Int32.to_string y ^ "]");
    let k = bitFlips x y in
    print_endline (List.to_string ~f:string_of_int k);
    true

  let%test "bitFlips 2" =
    let x = 5l 
    and y = 4l in
    print_endline ("[bitFlips " ^ Int32.to_string x ^ " " ^ Int32.to_string y ^ "]");
    let k = bitFlips x y in
    print_endline (List.to_string ~f:string_of_int k);
    true

end)