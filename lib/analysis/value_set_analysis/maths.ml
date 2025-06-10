open Core 

module ExtendedInt = struct
  type t = 
    | Int of int
    | Infinity
    | NegInfinity
  [@@deriving sexp, compare, equal]

  (** Converts an extended int value to its string representation. *)
  let to_string (i : t) : string =
    match i with 
    | Int i -> string_of_int i
    | Infinity -> "∞"
    | NegInfinity -> "-∞"

  (** Computes the maximum of two values. *)
  let maximum (x : t) (y : t) : t =
    match x, y with 
    | Infinity, _ | _, Infinity -> Infinity
    | NegInfinity, i | i, NegInfinity -> i
    | Int x, Int y -> Int (max x y)

  (** Computes the minimum of two values. *)
  let minimum (x : t) (y : t) : t =
    match x, y with 
    | Infinity, i | i, Infinity -> i
    | NegInfinity, _ | _, NegInfinity -> NegInfinity
    | Int x, Int y -> Int (min x y)

  let less_than (x : t) (y : t) : bool =
    match x, y with 
    | Int x, Int y when x < y -> true 
    | NegInfinity, Int _ | NegInfinity, Infinity -> true
    | Int _, Infinity -> true
    | _ -> false 

  let plus (x : t) (y : t) : t =
    match x, y with 
    | Infinity, NegInfinity | NegInfinity, Infinity -> failwith "undetermined addition"
    | Infinity, _ | _, Infinity -> Infinity
    | NegInfinity, _ | _, NegInfinity -> NegInfinity
    | Int x, Int y -> Int (x + y)

  let minus (x : t) (y : t) : t =
    match x, y with 
    | Infinity, Infinity | NegInfinity, NegInfinity -> failwith "undetermined subtraction"
    | NegInfinity, _ | _, Infinity -> NegInfinity
    | Infinity, _ | _, NegInfinity -> Infinity
    | Int x, Int y -> Int (x - y)

  let times (x : t) (y : t) : t =
    match x, y with 
    | Int 0, _ | _, Int 0 -> Int 0
    | Infinity, i -> if less_than (Int 0) i then Infinity else NegInfinity
    | NegInfinity, i -> if less_than i (Int 0) then Infinity else NegInfinity
    | i, Infinity -> if less_than (Int 0) i then Infinity else NegInfinity
    | i, NegInfinity -> if less_than i (Int 0) then Infinity else NegInfinity
    | Int x, Int y -> Int (x * y)
  
    let divide (x : t) (y : t) : t =
      match x, y with 
      | _, Int 0 -> failwith "division by 0"
      | Infinity, Infinity | Infinity, NegInfinity | NegInfinity, Infinity | NegInfinity, NegInfinity -> failwith "division error: ±∞/±∞"
      | Int _, Infinity | Int _, NegInfinity -> Int 0
      | Infinity, i -> if less_than (Int 0) i then Infinity else NegInfinity
      | NegInfinity, i -> if less_than (Int 0) i then NegInfinity else Infinity
      | Int x, Int y -> Int (x / y)

end

let rec gcd (a : int) (b : int) : int =
    if b = 0 then 
      a
    else 
      gcd b (a mod b)

let lcm a b =
  if a = 0 || b = 0 then 
    0
  else 
    abs (a * b) / gcd a b


(* Extended Euclidean algorithm *)
let extended_gcd (a : int) (b : int) : int * int * int =
  let rec aux (a : int) (b : int) x0 x1 y0 y1 =
    if b = 0 then 
      (a, x0, y0)
    else
      let q = a / b in
      aux b (a mod b) x1 (x0 - q * x1) y1 (y0 - q * y1)
  in
  aux a b 1 0 0 1

(* Chinese Remainder Theorem for two moduli *)
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

(* Example usage
let () =
  let (n, m) = chinese_remainder 6 4 15 1 in
  Printf.printf "n ≡ %d mod %d\n" n m *)