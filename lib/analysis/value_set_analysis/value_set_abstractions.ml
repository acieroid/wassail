open Core
module RIC_module = Reduced_interval_congruence
open RIC_module
module Boolean = Boolean
module Bitfield = Bitfield


(** Abstract integer domain combining three sub‑domains:
    - [ValueSet] wrapping a Reduced Interval Congruence (RIC),
    - [Boolean] values with both boolean abstraction and numeric RIC,
    - [Bitfield] tristate bitfields.

    This domain is used in the value‑set analysis to uniformly represent
    the abstract result of arithmetic, logical, and bitwise operations.
*)
type t =
  | ValueSet of RIC.t
  | Boolean of Boolean.t
  | Bitfield of Bitfield.t
[@@deriving sexp, compare, equal]

let bottom : t = ValueSet RIC.Bottom

let top : t = ValueSet RIC.Top

let equal (x : t) (y : t) : bool =
  match x, y with
  | ValueSet x, ValueSet y -> RIC.equal x y
  | Boolean x, Boolean y -> Boolean.equal x y
  | Bitfield x, Bitfield y -> Bitfield.equal x y
  | ValueSet x, Bitfield y 
  | Bitfield y, ValueSet x -> (RIC.equal x (RIC.of_bitfield y)) && (Bitfield.equal (RIC.to_bitfield x) y)
  | _ -> false


let is_singleton (x : t) : bool =
  match x with
  | ValueSet x -> RIC.is_singleton x
  | Boolean x -> Boolean.is_singleton x
  | Bitfield x -> Bitfield.is_singleton x

let rec plus (x : t) (y : t) : t =
  match x, y with
  | ValueSet x, ValueSet y -> ValueSet (RIC.plus x y)
  | Boolean {numeric_value = x; _}, y -> plus (ValueSet x) y
  | x, Boolean {numeric_value = y; _} -> plus x (ValueSet y)
  | Bitfield x, y -> plus (ValueSet (RIC.of_bitfield x)) y
  | x, Bitfield y -> plus x (ValueSet (RIC.of_bitfield y))


let rec minus (x : t) (y : t) : t =
  match x, y with
  | ValueSet x, ValueSet y -> ValueSet (RIC.minus x y)
  | Boolean {numeric_value = x; _}, y -> minus (ValueSet x) y
  | x, Boolean {numeric_value = y; _} -> minus x (ValueSet y)
  | Bitfield x, y -> minus (ValueSet (RIC.of_bitfield x)) y
  | x, Bitfield y -> minus x (ValueSet (RIC.of_bitfield y))
  
(** [to_string x] returns a human‑readable representation of [x],
    delegating to the corresponding sub‑domain printer. *)
let to_string (x : t) : string =
  match x with
  | ValueSet x -> RIC.to_string x
  | Boolean x -> Boolean.to_string x
  | Bitfield x -> Bitfield.to_string x


(** [join x y] computes the least upper bound of [x] and [y].
    Dispatches to the most precise operator when both operands belong to
    the same sub‑domain, otherwise falls back to a RIC join. *)
let join (x : t) (y : t) : t =
  match x, y with
  | ValueSet x, ValueSet y
  | ValueSet x, Boolean {numeric_value = y; _} | Boolean {numeric_value = x; _}, ValueSet y -> 
    ValueSet (RIC.join x y)
  | Boolean x, Boolean y -> Boolean (Boolean.join x y)
  | Bitfield x, Bitfield y -> Bitfield (Bitfield.join x y)
  | ValueSet x, Bitfield y | Bitfield y, ValueSet x -> ValueSet (RIC.join x (RIC.of_bitfield y))
  | Boolean {numeric_value = x; _}, Bitfield y
  | Bitfield y, Boolean {numeric_value = x; _} -> 
    ValueSet (RIC.join x (RIC.of_bitfield y))


(** [meet x y] computes the greatest lower bound of [x] and [y].
    Mixed‑domain meets are over‑approximated via RIC joins; this yields a
    sound but intentionally coarse abstraction. *)
let meet (x : t) (y : t) : t =
  match x, y with
  | ValueSet x, ValueSet y -> ValueSet (RIC.meet x y)
  | Boolean x, Boolean y -> Boolean (Boolean.meet x y)
  | Bitfield x, Bitfield y -> Bitfield (Bitfield.meet x y)
  | ValueSet x, Boolean {numeric_value = y; _} | Boolean {numeric_value = x; _}, ValueSet y -> ValueSet (RIC.join x y)
  | ValueSet x, Bitfield y | Bitfield y, ValueSet x -> ValueSet (RIC.join x (RIC.of_bitfield y))
  | Boolean {numeric_value = x; _}, Bitfield y
  | Bitfield y, Boolean {numeric_value = x; _} -> ValueSet (RIC.join x (RIC.of_bitfield y))

(** [not_ x] applies bitwise/logical negation in the corresponding
    sub‑domain. *)
let not_ (x : t) : t =
  match x with
  | ValueSet x -> ValueSet (RIC.not_ x)
  | Boolean x -> Boolean (Boolean.not_ x)
  | Bitfield x -> Bitfield (Bitfield.not_ x)

(** [and_ x y] computes bitwise/logical conjunction.  Mixed cases fall
    back to the RIC operator after converting bitfields when needed. *)
let and_ (x : t) (y : t) : t =
  match x, y with
  | ValueSet x, ValueSet y -> ValueSet (RIC.and_ x y)
  | ValueSet x, Boolean {numeric_value = y; true_or_false = tf} 
  | Boolean {numeric_value = x; true_or_false = tf}, ValueSet y ->
    Boolean {numeric_value = RIC.and_ x y; true_or_false = tf}
  | Boolean x, Boolean y -> Boolean (Boolean.and_ x y)
  | Bitfield x, Bitfield y -> Bitfield (Bitfield.and_ x y)
  | Bitfield x, ValueSet y
  | ValueSet y, Bitfield x
  | Bitfield x, Boolean {numeric_value = y; _}
  | Boolean {numeric_value = y; _}, Bitfield x ->
    ValueSet (RIC.and_ (RIC.of_bitfield x) y)

(** [or_ x y] computes bitwise/logical disjunction.  Mixed cases fallback
    to the RIC operator. *)
let or_ (x : t) (y : t) : t =
  match x, y with
  | ValueSet x, ValueSet y
  | ValueSet x, Boolean {numeric_value = y; _} 
  | Boolean {numeric_value = x; _}, ValueSet y ->
    ValueSet (RIC.or_ x y)
  | Boolean x, Boolean y -> Boolean (Boolean.or_ x y)
  | Bitfield x, Bitfield y -> Bitfield (Bitfield.or_ x y)
  | Bitfield x, ValueSet y
  | ValueSet y, Bitfield x
  | Bitfield x, Boolean {numeric_value = y; _}
  | Boolean {numeric_value = y; _}, Bitfield x ->
    ValueSet (RIC.or_ (RIC.of_bitfield x) y)

(** [xor_ x y] computes bitwise exclusive‑or.  Mixed cases fallback to
    the RIC operator. *)
let xor_ (x : t) (y : t) : t =
  match x, y with
  | ValueSet x, ValueSet y
  | ValueSet x, Boolean {numeric_value = y; _} 
  | Boolean {numeric_value = x; _}, ValueSet y ->
    ValueSet (RIC.xor_ x y)
  | Boolean x, Boolean y -> Boolean (Boolean.xor_ x y)
  | Bitfield x, Bitfield y -> Bitfield (Bitfield.xor_ x y)
  | Bitfield x, ValueSet y
  | ValueSet y, Bitfield x
  | Bitfield x, Boolean {numeric_value = y; _}
  | Boolean {numeric_value = y; _}, Bitfield x ->
    ValueSet (RIC.xor_ (RIC.of_bitfield x) y)

(** [shift_left x y] approximates the left‑shift operation.  Operands
    are promoted to the most precise domain available. *)
let shift_left (x : t) (y : t) : t =
  match x, y with
  | ValueSet x, ValueSet y
  | ValueSet x, Boolean {numeric_value = y; _}
  | Boolean {numeric_value = x; _}, ValueSet y
  | Boolean {numeric_value = x; _}, Boolean {numeric_value = y; _} ->
    ValueSet (RIC.shift_left x y)
  | ValueSet x, Bitfield y 
  | Boolean {numeric_value = x; _}, Bitfield y -> 
    ValueSet (RIC.shift_left x (RIC.of_bitfield y))
  | Bitfield x, ValueSet y
  | Bitfield x, Boolean {numeric_value = y; _} ->
    ValueSet (RIC.shift_left (RIC.of_bitfield x) y)
  | Bitfield x, Bitfield y -> Bitfield (Bitfield.shift_left x y)


(** [shift_right_u x y] approximates unsigned right shift. *)
let shift_right_u (x : t) (y : t) : t =
  match x, y with
  | ValueSet x, ValueSet y
  | ValueSet x, Boolean {numeric_value = y; _}
  | Boolean {numeric_value = x; _}, ValueSet y
  | Boolean {numeric_value = x; _}, Boolean {numeric_value = y; _} ->
    ValueSet (RIC.shift_right_u x y)
  | ValueSet x, Bitfield y 
  | Boolean {numeric_value = x; _}, Bitfield y -> 
    ValueSet (RIC.shift_right_u x (RIC.of_bitfield y))
  | Bitfield x, ValueSet y
  | Bitfield x, Boolean {numeric_value = y; _} ->
    ValueSet (RIC.shift_right_u (RIC.of_bitfield x) y)
  | Bitfield x, Bitfield y -> Bitfield (Bitfield.shift_right_unsigned x y)

(** [shift_right_s x y] approximates signed right shift. *)
let shift_right_s (x : t) (y : t) : t =
  match x, y with
  | ValueSet x, ValueSet y
  | ValueSet x, Boolean {numeric_value = y; _}
  | Boolean {numeric_value = x; _}, ValueSet y
  | Boolean {numeric_value = x; _}, Boolean {numeric_value = y; _} ->
    ValueSet (RIC.shift_right_s x y)
  | ValueSet x, Bitfield y 
  | Boolean {numeric_value = x; _}, Bitfield y -> 
    ValueSet (RIC.shift_right_s x (RIC.of_bitfield y))
  | Bitfield x, ValueSet y
  | Bitfield x, Boolean {numeric_value = y; _} ->
    ValueSet (RIC.shift_right_s (RIC.of_bitfield x) y)
  | Bitfield x, Bitfield y -> Bitfield (Bitfield.shift_right_signed x y)

(** [extract_relative_offset x] returns the symbolic base offset when
    [x] is a [ValueSet] containing a relative RIC, otherwise [""]. *)
let extract_relative_offset (x : t) : string =
  match x with
  | ValueSet (RIC {offset = (o, _); _}) -> o
  | _ -> ""

(** [update_relative_offset x actual_values] rewrites the relative offset
    of [x] (when it is a [ValueSet]) according to the mapping of
    symbolic offsets to concrete RICs. *)
let update_relative_offset (x : t) (actual_values : RIC.t String.Map.t) : t =
  match x with
  | ValueSet ric_ -> ValueSet (RIC.update_relative_offset ~ric_ ~actual_values)
  | _ -> x

let negative (x : t) : t =
  match x with
  | ValueSet x
  | Boolean {numeric_value = x; _} -> ValueSet (RIC.negative x)
  | Bitfield x -> ValueSet (RIC.negative (RIC.of_bitfield x))

let add_consts (x : int32) (y : int32) : t =
  ValueSet (RIC.ric (0l, Int 0l, Int 0l, ("", Int32.(x + y))))

let sub_consts (x : int32) (y : int32) : t=
  add_consts y Int32.(-x)

let add_const (x : t) (y : int32) : t =
  match x with
  | ValueSet x 
  | Boolean {numeric_value = x; _} -> ValueSet (RIC.add_offset x y)
  | Bitfield x -> ValueSet (RIC.add_offset (RIC.of_bitfield x) y)

let sub_const (x : int32) (y : t) : t =
  add_const y Int32.(-x)

let i32_add (x : t) (y : t) : t =
  match x, y with
  | ValueSet x, ValueSet y
  | ValueSet x, Boolean {numeric_value = y; _}
  | Boolean {numeric_value = x; _}, ValueSet y 
  | Boolean {numeric_value = x; _}, Boolean {numeric_value = y; _} ->
    ValueSet (RIC.plus x y)
  | Bitfield x, ValueSet y
  | ValueSet y, Bitfield x
  | Bitfield x, Boolean {numeric_value = y; _}
  | Boolean {numeric_value = y; _}, Bitfield x ->
    ValueSet (RIC.plus (RIC.of_bitfield x) y)
  | Bitfield x, Bitfield y -> ValueSet (RIC.plus (RIC.of_bitfield x) (RIC.of_bitfield y))

let i32_sub (x : t) (y : t) : t =
  i32_add (negative x) y


let rec may_overlap ~(store_size : int32) ~(load_size : int32) ~(store_vs : t) ~(load_vs : t) : bool =
  match store_vs, load_vs with
  | Bitfield vs1, _ -> may_overlap ~store_size ~load_size ~store_vs:(ValueSet (RIC.of_bitfield vs1)) ~load_vs
  | _, Bitfield vs2 -> may_overlap ~store_size ~load_size ~store_vs ~load_vs:(ValueSet (RIC.of_bitfield vs2))
  | Boolean {numeric_value; _}, _ -> may_overlap ~store_size ~load_size ~store_vs:(ValueSet numeric_value) ~load_vs
  | _, Boolean {numeric_value; _} -> may_overlap ~store_size ~load_size ~store_vs ~load_vs:(ValueSet numeric_value)
  | ValueSet vs1, ValueSet vs2 -> RIC.may_overlap ~store_size ~load_size vs1 vs2


let eqz ?(var : Variable.t option) (x : t) : t =
  match x with
  | Boolean x -> Boolean (Boolean.eqz x)
  | ValueSet ric -> Boolean (Boolean.eqz (Boolean.of_RIC ~ric ~var:(Option.value_exn var)))
  | Bitfield bf -> Boolean (Boolean.eqz (Boolean.of_bitfield ~bf ~var:(Option.value_exn var)))


let rec count_trailing_zeros (vs : t) : t =
  match vs with
  | ValueSet Top | Boolean { numeric_value = RIC.Top; _ } | Bitfield Top -> ValueSet (RIC.ric (1l, Int 0l, Int 32l, ("", 32l)))
  | ValueSet Bottom | Boolean { numeric_value = RIC.Bottom; _ } | Bitfield Bottom -> ValueSet RIC.Bottom
  | ValueSet vs -> count_trailing_zeros (Bitfield (vs |> RIC.to_bitfield))
  | Boolean { numeric_value = vs; _ } -> count_trailing_zeros (Bitfield (vs |> RIC.to_bitfield))
  | Bitfield (Bit bf) ->
    let tristate = Int32.(bf.ones land bf.zeros) in
    let max = Int32.(bf.ones land (lnot bf.zeros)) |> Int32.ctz in
    ValueSet (
      max |> List.init ~f:(fun x -> x)
          |> List.filter ~f:(fun x -> Int32.equal (Int32.rem (Int32.shift_right_logical tristate x) 2l) 1l)
          |> List.append [max]
          |> List.fold ~init:RIC.Bottom ~f:(fun acc x -> RIC.join acc (RIC.constant (Int32.of_int_exn x))))

let reverse_bit_order (i : int32) : int32 =
  List.init 32 ~f:(fun x -> x)
  |> List.fold ~init:0l ~f:(fun acc bit_index ->
       let bit = Int32.(bit_and i (shift_left 1l bit_index)) in
       if Int32.equal bit 0l then acc
       else Int32.bit_or acc (Int32.shift_left 1l (31 - bit_index)))

let rec count_leading_zeros (vs : t) : t =
  match vs with
  | ValueSet Top | Boolean { numeric_value = RIC.Top; _ } | Bitfield Top -> ValueSet (RIC.ric (1l, Int 0l, Int 32l, ("", 32l)))
  | ValueSet Bottom | Boolean { numeric_value = RIC.Bottom; _ } | Bitfield Bottom -> ValueSet RIC.Bottom
  | ValueSet vs -> count_leading_zeros (Bitfield (vs |> RIC.to_bitfield))
  | Boolean { numeric_value = vs; _ } -> count_leading_zeros (Bitfield (vs |> RIC.to_bitfield))
  | Bitfield (Bit { ones; zeros }) -> count_trailing_zeros (Bitfield (Bit { ones = reverse_bit_order ones; zeros = reverse_bit_order zeros})) 

let rec population_count (vs : t) : t =
  match vs with
  | ValueSet Top | Boolean { numeric_value = RIC.Top; _ } | Bitfield Top -> ValueSet (RIC.ric (1l, Int 0l, Int 32l, ("", 32l)))
  | ValueSet Bottom | Boolean { numeric_value = RIC.Bottom; _ } | Bitfield Bottom -> ValueSet RIC.Bottom
  | ValueSet vs -> population_count (Bitfield (vs |> RIC.to_bitfield))
  | Boolean { numeric_value = vs; _ } -> population_count (Bitfield (vs |> RIC.to_bitfield))
  | Bitfield (Bit { ones; zeros }) ->
    let max = Int32.popcount ones in
    let min = max - (Int32.(ones land zeros) |> Int32.popcount) in
    ValueSet (RIC.ric (1l, Int (Int32.of_int_exn min), Int (Int32.of_int_exn max), ("", 0l)))





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

  let%test "Bitfield_tests" =
    print_endline "\n_______ ____________________________ _______\n        Value-set abstractions        \n------- ---------------------------- -------\n"; true

  let%test "count_trailing_zeros" =
    print_string ("[count_trailing_zeros] ");
    let bf = Bitfield (Bit { zeros = 0b1111_1111_1111_1111_1100_1101_1101_1111l; 
                              ones = 0b0000_0000_0000_0000_0011_0010_1110_1010l}) in
    let () = bf |> to_string |> print_string in
    let trailing_zeros = bf |> count_trailing_zeros in
    let () = " --trailing zeros--> " ^ to_string trailing_zeros |> print_endline in
    trailing_zeros |> equal (ValueSet (RIC.ric (2l, Int 0l, Int 2l, ("", 1l))))


  let%test "count_leading_zeros" =
    print_string ("[count_leading_zeros] ");
    let bf = Bitfield (Bit { zeros = 0b1111_1111_1111_1111_1111_1101_1101_1111l; 
                              ones = 0b0000_0000_0010_0010_0010_0010_1110_1010l}) in
    let () = bf |> to_string |> print_string in
    let leading_zeros = bf |> count_leading_zeros in
    let () = " --leading zeros--> " ^ to_string leading_zeros |> print_endline in
    leading_zeros |> equal (ValueSet (RIC.ric (4l, Int 0l, Int 3l, ("", 10l))))

  let%test "population_count" =
    print_string ("[population_count] ");
    let bf = Bitfield (Bit { zeros = 0b1111_1111_1111_1111_1111_1101_1101_1111l; 
                              ones = 0b0000_0000_0010_0010_0010_0010_1110_1010l}) in
    let () = bf |> to_string |> print_string in
    let pop_count = bf |> population_count in
    let () = " --population count--> " ^ to_string pop_count |> print_endline in
    pop_count |> equal (ValueSet (RIC.ric (1l, Int 2l, Int 9l, ("", 0l))))
end)
    