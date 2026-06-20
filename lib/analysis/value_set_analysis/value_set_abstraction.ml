open Core
open Reduced_interval_congruence


(** Abstract integer domain combining three sub-domains:
    - [ValueSet] wraps a Reduced Interval Congruence (RIC),
    - [Boolean] tracks conditional refinements together with a numeric RIC,
    - [Bitfield] tracks known, impossible, and unknown bits.

    This domain represents the abstract result of arithmetic, logical, and
    bitwise operations in the value-set analysis.
*)
type t =
  | ValueSet of RIC.t
  | Boolean of Boolean.t
  | Bitfield of Bitfield.t
[@@deriving sexp, compare, equal]

(** The empty abstract value. *)
let bottom : t = ValueSet RIC.Bottom
(** The unknown abstract integer value. *)
let top : t = ValueSet RIC.Top

(** [equal x y] tests whether [x] and [y] represent the same abstract set.
    Mixed Boolean cases compare through their numeric abstraction. Mixed
    ValueSet/Bitfield cases require both conversions to be exact. *)
let rec equal (x : t) (y : t) : bool =
  match x, y with
  | ValueSet x, ValueSet y -> RIC.(x = y)
  | Boolean x, Boolean y -> Boolean.(x = y)
  | Boolean x, y | y, Boolean x -> equal (ValueSet x.numeric_value) y
  | Bitfield x, Bitfield y -> Bitfield.(x = y)
  (* Mixed ValueSet/Bitfield equality holds only when both
   over-approximating conversions are lossless. *)
  | ValueSet x, Bitfield y 
  | Bitfield y, ValueSet x -> RIC.(x = of_bitfield y) && Bitfield.(y = RIC.to_bitfield x)
let (=) = equal
let (<>) (x : t) (y : t) : bool = not (x = y)


(** [is_singleton x] returns [true] when [x] represents exactly one value. *)
let is_singleton (x : t) : bool =
  match x with
  | ValueSet x -> RIC.is_singleton x
  | Boolean x -> Boolean.is_singleton x
  | Bitfield x -> Bitfield.is_singleton x

  
(** [to_string x] returns a human‑readable representation of [x],
    delegating to the corresponding sub‑domain printer. *)
let to_string (x : t) : string =
  match x with
  | ValueSet x -> RIC.to_string x
  | Boolean x -> Boolean.to_string x
  | Bitfield x -> Bitfield.to_string x


(** [join x y] computes an upper bound of [x] and [y]. Same-domain operands
    use their own join; mixed-domain operands fall back to RIC. *)
let join (x : t) (y : t) : t =
  match x, y with
  | ValueSet Bottom, other
  | Bitfield Bottom, other 
  | Boolean {numeric_value=RIC.Bottom; _}, other
  | other, ValueSet Bottom 
  | other, Bitfield Bottom
  | other, Boolean {numeric_value=RIC.Bottom; _} -> other
  | ValueSet x, ValueSet y
  | ValueSet x, Boolean {numeric_value = y; _} 
  | Boolean {numeric_value = x; _}, ValueSet y -> ValueSet (RIC.join x y)
  | Boolean x, Boolean y -> Boolean (Boolean.join x y)
  | Bitfield x, Bitfield y -> Bitfield (Bitfield.join x y)
  | ValueSet x, Bitfield y 
  | Bitfield y, ValueSet x -> ValueSet RIC.(join x (of_bitfield y))
  | Boolean {numeric_value = x; _}, Bitfield y
  | Bitfield y, Boolean {numeric_value = x; _} -> ValueSet RIC.(join x (of_bitfield y))


(** [meet x y] computes a lower bound of [x] and [y]. Same-domain operands
    use their own meet. Mixed-domain operands meet after conversion to RIC. *)
let meet (x : t) (y : t) : t =
  match x, y with
  | ValueSet x, ValueSet y -> ValueSet (RIC.meet x y)
  | Boolean x, Boolean y -> Boolean (Boolean.meet x y)
  | Bitfield x, Bitfield y -> Bitfield (Bitfield.meet x y)
  | ValueSet x, Boolean {numeric_value = y; _} 
  | Boolean {numeric_value = x; _}, ValueSet y -> ValueSet (RIC.meet x y)
  | ValueSet x, Bitfield y 
  | Bitfield y, ValueSet x -> ValueSet RIC.(meet x (of_bitfield y))
  | Boolean {numeric_value = x; _}, Bitfield y
  | Bitfield y, Boolean {numeric_value = x; _} -> ValueSet RIC.(meet x (of_bitfield y))

(** [and_ x y] computes bitwise/logical conjunction. Same-domain operands use
    their own operator; mixed-domain operands are approximated through RIC or
    Bitfield conversions. *)
let and_ (x : t) (y : t) : t =
  match x, y with
  | ValueSet x, ValueSet y -> ValueSet RIC.(x &. y)
  | ValueSet x, Boolean {numeric_value = y; true_or_false = tf} 
  | Boolean {numeric_value = x; true_or_false = tf}, ValueSet y ->
    Boolean {numeric_value = RIC.(x &. y); true_or_false = Boolean.True_or_false.weaken_false_branch tf}
  | Boolean x, Boolean y -> Boolean Boolean.(x &. y)
  | Bitfield x, Bitfield y -> Bitfield Bitfield.(x &. y)
  | Bitfield x, ValueSet y
  | ValueSet y, Bitfield x -> ValueSet RIC.(of_bitfield Bitfield.(x &. to_bitfield y))
  | Bitfield x, Boolean {numeric_value = y; true_or_false = tf}
  | Boolean {numeric_value = y; true_or_false = tf}, Bitfield x -> 
    Boolean {numeric_value = RIC.(of_bitfield Bitfield.(x &. to_bitfield y)); 
             true_or_false = Boolean.True_or_false.weaken_false_branch tf}
let (&.) = and_

(** [or_ x y] computes bitwise/logical disjunction. Same-domain operands use
    their own operator; mixed-domain operands are approximated through RIC or
    Bitfield conversions. *)
let or_ (x : t) (y : t) : t =
  match x, y with
  | ValueSet x, ValueSet y -> ValueSet RIC.(x |. y)
  | ValueSet x, Boolean {numeric_value = y; true_or_false = tf} 
  | Boolean {numeric_value = x; true_or_false = tf}, ValueSet y ->
    Boolean {numeric_value = RIC.(x |. y); true_or_false = Boolean.True_or_false.weaken_true_branch tf}
  | Boolean x, Boolean y -> Boolean Boolean.(x |. y)
  | Bitfield x, Bitfield y -> Bitfield Bitfield.(x |. y)
  | Bitfield x, ValueSet y
  | ValueSet y, Bitfield x -> ValueSet RIC.(of_bitfield Bitfield.(x |. to_bitfield y))
  | Bitfield x, Boolean {numeric_value = y; true_or_false = tf}
  | Boolean {numeric_value = y; true_or_false = tf}, Bitfield x -> 
    Boolean {numeric_value = RIC.(of_bitfield Bitfield.(x |. to_bitfield y));
             true_or_false = Boolean.True_or_false.weaken_true_branch tf}
let (|.) = or_

(** [xor_ x y] computes bitwise exclusive-or. Same-domain operands use their
    own operator; mixed-domain operands are approximated through RIC or
    Bitfield conversions. *)
let xor_ (x : t) (y : t) : t =
  match x, y with
  | ValueSet x, ValueSet y
  | ValueSet x, Boolean {numeric_value = y; _} 
  | Boolean {numeric_value = x; _}, ValueSet y -> ValueSet RIC.(x <+> y)
  | Boolean x, Boolean y -> Boolean Boolean.(x <+> y)
  | Bitfield x, Bitfield y -> Bitfield Bitfield.(x <+> y)
  | Bitfield x, ValueSet y
  | ValueSet y, Bitfield x
  | Bitfield x, Boolean {numeric_value = y; _}
  | Boolean {numeric_value = y; _}, Bitfield x -> ValueSet RIC.(of_bitfield Bitfield.(x <+> to_bitfield y))
let (<+>) = xor_

(** [shift_left x y] approximates left shift. Bitfield/Bitfield operands use
    the Bitfield operator; other cases are approximated through RIC or
    Bitfield conversions. *)
let shift_left (x : t) (y : t) : t =
  match x, y with
  | ValueSet x, ValueSet y
  | ValueSet x, Boolean {numeric_value = y; _}
  | Boolean {numeric_value = x; _}, ValueSet y
  | Boolean {numeric_value = x; _}, Boolean {numeric_value = y; _} -> ValueSet RIC.(x << y)
  | ValueSet x, Bitfield y 
  | Boolean {numeric_value = x; _}, Bitfield y -> ValueSet RIC.(x << of_bitfield y)
  | Bitfield x, ValueSet y
  | Bitfield x, Boolean {numeric_value = y; _} -> ValueSet RIC.(of_bitfield Bitfield.(x << to_bitfield y))
  | Bitfield x, Bitfield y -> Bitfield Bitfield.(x << y)
let (<<) = shift_left


(** [shift_right_u x y] approximates unsigned right shift. Bitfield/Bitfield
    operands use the Bitfield operator; other cases are approximated through
    RIC or Bitfield conversions. *)
let shift_right_u (x : t) (y : t) : t =
  match x, y with
  | ValueSet x, ValueSet y
  | ValueSet x, Boolean {numeric_value = y; _}
  | Boolean {numeric_value = x; _}, ValueSet y
  | Boolean {numeric_value = x; _}, Boolean {numeric_value = y; _} -> ValueSet RIC.(x >>. y)
  | ValueSet x, Bitfield y 
  | Boolean {numeric_value = x; _}, Bitfield y -> ValueSet RIC.(x >>. of_bitfield y)
  | Bitfield x, ValueSet y
  | Bitfield x, Boolean {numeric_value = y; _} -> ValueSet RIC.(of_bitfield Bitfield.(x >>. to_bitfield y))
  | Bitfield x, Bitfield y -> Bitfield Bitfield.(x >>. y)
let (>>.) = shift_right_u

(** [shift_right_s x y] approximates signed right shift. Bitfield/Bitfield
    operands use the Bitfield operator; other cases are approximated through
    RIC or Bitfield conversions. *)
let shift_right_s (x : t) (y : t) : t =
  match x, y with
  | ValueSet x, ValueSet y
  | ValueSet x, Boolean {numeric_value = y; _}
  | Boolean {numeric_value = x; _}, ValueSet y
  | Boolean {numeric_value = x; _}, Boolean {numeric_value = y; _} -> ValueSet RIC.(x >>- y)
  | ValueSet x, Bitfield y 
  | Boolean {numeric_value = x; _}, Bitfield y -> ValueSet RIC.(x >>- of_bitfield y)
  | Bitfield x, ValueSet y
  | Bitfield x, Boolean {numeric_value = y; _} -> ValueSet RIC.(of_bitfield Bitfield.(x >>- to_bitfield y))
  | Bitfield x, Bitfield y -> Bitfield Bitfield.(x >>- y)
let (>>-) = shift_right_s

(** [extract_relative_offset x] returns the symbolic base offset when
    [x] is a [ValueSet] containing a relative RIC, otherwise [""]. *)
let extract_relative_offset (x : t) : string =
  match x with
  | ValueSet r | Boolean {numeric_value = r; _} -> RIC.extract_relative_offset r
  | _ -> ""

(** [update_relative_offset x actual_values] rewrites the relative offset of
    the numeric component of [x] according to [actual_values]. Booleans are
    returned as [ValueSet]s, so conditional refinements are dropped. Bitfields
    are left unchanged. *)
let update_relative_offset (x : t) (actual_values : RIC.t String.Map.t) : t =
  match x with
  | ValueSet ric_ | Boolean {numeric_value=ric_; _} -> ValueSet (RIC.update_relative_offset ~ric_ ~actual_values)
  | _ -> x

(** [negative x] over-approximates integer negation. *)
let negative (x : t) : t =
  match x with
  | ValueSet x
  | Boolean {numeric_value = x; _} -> ValueSet RIC.(negative x)
  | Bitfield x -> ValueSet RIC.(x |> of_bitfield |> negative)

(** [add_consts x y] returns the abstract sum of the two concrete constants. *)
let add_consts (x : int32) (y : int32) : t =
  ValueSet RIC.(constant x + constant y)

(** [sub_consts ~subtract_this ~from] returns the abstract value of
    [from - subtract_this]. *)
let sub_consts ~(subtract_this : int32) ~(from : int32) : t =
  ValueSet RIC.(constant from - constant subtract_this)

(** [add_const x y] adds the concrete offset [y] to [x]. *)
let add_const (x : t) (y : int32) : t =
  match x with
  | ValueSet x 
  | Boolean {numeric_value = x; _} -> ValueSet RIC.(add_offset x y)
  | Bitfield x -> ValueSet RIC.(add_offset (of_bitfield x) y)

(** [sub_const ~subtract_this ~from] subtracts the concrete value
    [subtract_this] from [from]. *)
let sub_const ~(subtract_this : int32) ~(from : t) : t =
  add_const from Int32.(-subtract_this)

(** [plus x y] implements Wasm [i32.add] on abstract values. *)
let plus (x : t) (y : t) : t =
  match x, y with
  | ValueSet x, ValueSet y
  | ValueSet x, Boolean {numeric_value = y; _}
  | Boolean {numeric_value = x; _}, ValueSet y 
  | Boolean {numeric_value = x; _}, Boolean {numeric_value = y; _} -> ValueSet RIC.(x + y)
  | Bitfield x, ValueSet y
  | ValueSet y, Bitfield x
  | Bitfield x, Boolean {numeric_value = y; _}
  | Boolean {numeric_value = y; _}, Bitfield x -> ValueSet RIC.(of_bitfield x + y)
  | Bitfield x, Bitfield y -> ValueSet RIC.(of_bitfield x + of_bitfield y)
let (+) = plus

(** [minus ~subtract_this ~from] implements Wasm [i32.sub], computing
    [from - subtract_this]. *)
let minus ~(subtract_this : t) ~(from : t) : t =
  plus (negative subtract_this) from
let (-) (lhs : t) (rhs : t) : t = minus ~subtract_this:rhs ~from:lhs

(** [may_overlap ~store_size ~load_size ~store_vs ~load_vs] returns [true]
    when a store at [store_vs] may overlap a load at [load_vs]. *)
let rec may_overlap ~(store_size : int32) ~(load_size : int32) ~(store_vs : t) ~(load_vs : t) : bool =
  match store_vs, load_vs with
  | Bitfield vs1, _ -> may_overlap ~store_size ~load_size ~store_vs:(ValueSet (RIC.of_bitfield vs1)) ~load_vs
  | _, Bitfield vs2 -> may_overlap ~store_size ~load_size ~store_vs ~load_vs:(ValueSet (RIC.of_bitfield vs2))
  | Boolean {numeric_value; _}, _ -> may_overlap ~store_size ~load_size ~store_vs:(ValueSet numeric_value) ~load_vs
  | _, Boolean {numeric_value; _} -> may_overlap ~store_size ~load_size ~store_vs ~load_vs:(ValueSet numeric_value)
  | ValueSet store_ric, ValueSet load_ric -> RIC.may_overlap ~store_size ~load_size ~store_ric ~load_ric


(** [eqz ?var x] implements Wasm [i32.eqz]. The optional variable is used
    when a non-Boolean value must be lifted to the Boolean domain. *)
let eqz ~(var : Variable.t) (x : t) : t =
  match x with
  | Boolean x -> Boolean (Boolean.eqz x)
  | ValueSet ric -> Boolean Boolean.(ric |> of_RIC ~var |> eqz)
  | Bitfield bf -> Boolean Boolean.(bf |> of_bitfield ~var |> eqz)


(** [count_trailing_zeros vs] implements Wasm [i32.ctz]. *)
let rec count_trailing_zeros (vs : t) : t =
  match vs with
  | ValueSet Top 
  | Boolean { numeric_value = RIC.Top; _ } 
  | Bitfield Top -> ValueSet (RIC.ric (1l, Int 0l, Int 32l, ("", 0l)))
  | ValueSet Bottom 
  | Boolean { numeric_value = RIC.Bottom; _ } 
  | Bitfield Bottom -> ValueSet RIC.Bottom
  | ValueSet vs
  | Boolean { numeric_value = vs; _ } -> count_trailing_zeros (Bitfield (vs |> RIC.to_bitfield))
  | Bitfield (Bit bf) ->
    let tristate = Int32.(bf.ones land bf.zeros) in
    let max = Int32.(bf.ones land (lnot bf.zeros)) |> Int32.ctz in
    ValueSet (
      max |> List.init ~f:(fun x -> x)
          |> List.filter ~f:(fun x -> Int32.(1l = (rem (shift_right_logical tristate x) 2l)))
          |> List.append [max]
          |> List.fold ~init:RIC.Bottom ~f:(fun acc x -> RIC.(join acc (constant (Int32.of_int_exn x)))))

(** [count_leading_zeros vs] implements Wasm [i32.clz]. *)
let rec count_leading_zeros (vs : t) : t =
  match vs with
  | ValueSet Top 
  | Boolean { numeric_value = RIC.Top; _ } 
  | Bitfield Top -> ValueSet (RIC.ric (1l, Int 0l, Int 32l, ("", 0l)))
  | ValueSet Bottom 
  | Boolean { numeric_value = RIC.Bottom; _ } 
  | Bitfield Bottom -> ValueSet RIC.Bottom
  | ValueSet vs
  | Boolean { numeric_value = vs; _ } -> count_leading_zeros (Bitfield (vs |> RIC.to_bitfield))
  | Bitfield bf -> Bitfield Bitfield.(bf |> reverse_bit_order) |> count_trailing_zeros

(** [population_count vs] implements Wasm [i32.popcnt]. *)
let rec population_count (vs : t) : t =
  match vs with
  | ValueSet Top 
  | Boolean { numeric_value = RIC.Top; _ } 
  | Bitfield Top -> ValueSet (RIC.ric (1l, Int 0l, Int 32l, ("", 0l)))
  | ValueSet Bottom 
  | Boolean { numeric_value = RIC.Bottom; _ } 
  | Bitfield Bottom -> ValueSet RIC.Bottom
  | ValueSet vs
  | Boolean { numeric_value = vs; _ } -> population_count (Bitfield (vs |> RIC.to_bitfield))
  | Bitfield (Bit { ones; zeros }) ->
    let max = Int32.popcount ones in
    let min = Int.(-) max (Int32.(ones land zeros) |> Int32.popcount) in
    ValueSet (RIC.ric (1l, Int (Int32.of_int_exn min), Int (Int32.of_int_exn max), ("", 0l)))


(** [may_be_false x] returns [true] when [x] may represent zero. *)
let may_be_false : t -> bool = function
  | ValueSet vs
  | Boolean {numeric_value = vs; _} -> RIC.may_be_false vs
  | Bitfield bf -> Bitfield.may_be_false bf

(** [may_be_true x] returns [true] when [x] may represent a non-zero value. *)
let may_be_true : t -> bool = function
  | ValueSet vs
  | Boolean {numeric_value = vs; _} -> RIC.may_be_true vs
  | Bitfield bf -> Bitfield.may_be_true bf


(** [compare_values var1 var2 vs1_true vs1_false vs2_true vs2_false]
    builds the abstract boolean result of a comparison. The result is true
    when both true refinements are non-bottom, and false when both false
    refinements are non-bottom. Non-constant variables keep their branch
    refinements in the Boolean domain. *)
let compare_values
    (var1 : Var.t)
    (var2 : Var.t)
    (vs1_true : RIC.t)
    (vs1_false : RIC.t)
    (vs2_true : RIC.t)
    (vs2_false : RIC.t)
  : t =
  let numeric_value = RIC.(join
    (if vs1_false <> Bottom && vs2_false <> Bottom then zero else Bottom)
    (if vs1_true <> Bottom && vs2_true <> Bottom then one else Bottom))
  in
  let true_or_false =
    List.fold [ var1, vs1_true, vs1_false; var2, vs2_true, vs2_false ]
      ~init:Variable.Map.empty
      ~f:(fun acc (var, true_, false_) ->
        match var with
        | Var.Const _ -> acc
        | _ ->
          Variable.Map.set
            acc
            ~key:(Variable.Var var)
            ~data:Boolean.{True_or_false.true_; false_})
  in
  if Variable.Map.is_empty true_or_false then
    ValueSet numeric_value
  else
    Boolean {true_or_false; numeric_value}


(** [less_or_equal (var1, vs1) (var2, vs2)] implements [var1 <= var2].
    Comparable offsets produce a Boolean with branch refinements; otherwise
    the result is approximated as either true or false. *)
let less_or_equal (var1, vs1 : Var.t * RIC.t) (var2, vs2 : Var.t * RIC.t) : t =
  if not (RIC.comparable_offsets vs1 vs2) then
    ValueSet RIC.(join one zero)
  else
    let vs2_neg = RIC.remove_lower_bound vs2 in
    let vs2_pos = RIC.(remove_upper_bound vs2  + one) in
    let vs1_true = RIC.meet vs1 vs2_neg in
    let vs1_false = RIC.meet vs1 vs2_pos in
    let vs1_neg = RIC.(remove_lower_bound vs1 - one) in
    let vs1_pos = RIC.remove_upper_bound vs1 in
    let vs2_true = RIC.meet vs2 vs1_pos in
    let vs2_false = RIC.meet vs2 vs1_neg in
    compare_values var1 var2 vs1_true vs1_false vs2_true vs2_false

(** [less_than (var1, vs1) (var2, vs2)] implements [var1 < var2].
    Comparable offsets produce a Boolean with branch refinements; otherwise
    the result is approximated as either true or false. *)
let less_than (var1, vs1 : Var.t * RIC.t) (var2, vs2 : Var.t * RIC.t) : t =
  if not (RIC.comparable_offsets vs1 vs2) then
    ValueSet RIC.(join one zero)
  else
    let vs2_neg = RIC.(remove_lower_bound vs2 - one) in
    let vs2_pos = RIC.remove_upper_bound vs2 in
    let vs1_true = RIC.meet vs1 vs2_neg in
    let vs1_false = RIC.meet vs1 vs2_pos in
    let vs1_neg = RIC.remove_lower_bound vs1 in
    let vs1_pos = RIC.(remove_upper_bound vs1 + one) in
    let vs2_true = RIC.meet vs2 vs1_pos in
    let vs2_false = RIC.meet vs2 vs1_neg in
    compare_values var1 var2 vs1_true vs1_false vs2_true vs2_false

(** [are_equal_or_not ~not_equal (var1, vs1) (var2, vs2)] implements equality
    when [not_equal] is [false], and disequality when [not_equal] is [true].
    Singleton operands refine the false branch by removing the compared value. *)
let are_equal_or_not ?(not_equal : bool = false) (var1, vs1 : Var.t * RIC.t) (var2, vs2 : Var.t * RIC.t) : t =
  if not (RIC.comparable_offsets vs1 vs2) then
    ValueSet RIC.(join one zero)
  else
    let vs1_true = RIC.meet vs1 vs2 in
    let vs1_false = 
      if RIC.is_singleton vs2 then
        RIC.remove ~this:vs2 ~from:vs1
        |> List.fold ~init:RIC.Bottom ~f:(fun acc x -> RIC.join acc x)
      else
        vs1
    in
    let vs2_true = RIC.meet vs1 vs2 in
    let vs2_false = 
      if RIC.is_singleton vs1 then
        RIC.remove ~this:vs1 ~from:vs2
        |> List.fold ~init:RIC.Bottom ~f:(fun acc x -> RIC.join acc x)
      else
        vs2
    in
    if not_equal then
      compare_values var1 var2 vs1_false vs1_true vs2_false vs2_true
    else
      compare_values var1 var2 vs1_true vs1_false vs2_true vs2_false


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

let%test_module "value-set abstraction tests" = (module struct

  let%test "value-set abstraction tests" =
    print_endline "\n_______ ____________________________ _______\n        Value-set abstractions        \n------- ---------------------------- -------\n"; true

  let test_label name = Printf.sprintf "%-45s" name

  let%test "equal_value_set_bitfield_exact_singleton" =
    let value_set = ValueSet (RIC.constant 42l) in
    let bitfield = Bitfield (Bitfield.singleton 42l) in
    let actual = equal value_set bitfield in
    let expected = true in
    let passed = Bool.equal actual expected in
    print_endline (Printf.sprintf "%s %s = %s -> %b%s"
      (test_label "[ValueSetAbstractions.equal]")
      (to_string value_set)
      (to_string bitfield)
      actual
      (if passed then "" else Printf.sprintf " (expected %b)" expected));
    passed

  let%test "equal_value_set_bitfield_inexact_conversion" =
    let value_set = ValueSet (RIC.ric (2l, Int 0l, Int 1l, ("", 0l))) in
    let bitfield = Bitfield (Bitfield.singleton 0l) in
    let actual = equal value_set bitfield in
    let expected = false in
    let passed = Bool.equal actual expected in
    print_endline (Printf.sprintf "%s %s = %s -> %b%s"
      (test_label "[ValueSetAbstractions.equal]")
      (to_string value_set)
      (to_string bitfield)
      actual
      (if passed then "" else Printf.sprintf " (expected %b)" expected));
    passed

  let%test "equal_value_set_bitfield_inexact_conversion2" =
    let value_set = ValueSet (RIC.ric (2l, Int 0l, Int 1l, ("", 1l))) in
    let bitfield = Bitfield (Bitfield.of_string "011") in
    let actual = equal value_set bitfield in
    let expected = false in
    let passed = Bool.equal actual expected in
    print_endline (Printf.sprintf "%s %s = %s -> %b%s"
      (test_label "[ValueSetAbstractions.equal]")
      (to_string value_set)
      (to_string bitfield)
      actual
      (if passed then "" else Printf.sprintf " (expected %b)" expected));
    passed

  let%test "may_overlap_mixed_bitfield_value_set" =
    let store_vs = Bitfield (Bitfield.singleton 4l) in
    let load_vs = ValueSet (RIC.constant 7l) in
    let actual = may_overlap
      ~store_size:4l
      ~load_size:1l
      ~store_vs
      ~load_vs in
    let expected = true in
    let passed = Bool.equal actual expected in
    print_endline (Printf.sprintf "%s store %s[4] / load %s[1] -> %b%s"
      (test_label "[ValueSetAbstractions.may_overlap]")
      (to_string store_vs)
      (to_string load_vs)
      actual
      (if passed then "" else Printf.sprintf " (expected %b)" expected));
    passed


  let%test "may_overlap_mixed_bitfield_value_set_adjacent" =
    let store_vs = Bitfield (Bitfield.singleton 4l) in
    let load_vs = ValueSet (RIC.constant 8l) in
    let actual = may_overlap
      ~store_size:4l
      ~load_size:1l
      ~store_vs
      ~load_vs in
    let expected = false in
    let passed = Bool.equal actual expected in
    print_endline (Printf.sprintf "%s store %s[4] / load %s[1] -> %b%s"
      (test_label "[ValueSetAbstractions.may_overlap]")
      (to_string store_vs)
      (to_string load_vs)
      actual
      (if passed then "" else Printf.sprintf " (expected %b)" expected));
    passed

  let%test "i32_sub_mixed_value_set_bitfield" =
    let from = ValueSet (RIC.constant 10l) in
    let subtract_this = Bitfield (Bitfield.singleton 3l) in
    let actual = minus ~subtract_this ~from in
    let expected = ValueSet (RIC.constant 7l) in
    let passed = equal actual expected in
    print_endline (Printf.sprintf "%s %s - %s -> %s%s"
      (test_label "[ValueSetAbstractions.i32_sub]")
      (to_string from)
      (to_string subtract_this)
      (to_string actual)
      (if passed then "" else Printf.sprintf " (expected %s)" (to_string expected)));
    passed

  let%test "shift_left_mixed_bitfield_value_set" =
    let x = Bitfield (Bitfield.singleton 3l) in
    let y = ValueSet (RIC.constant 2l) in
    let actual = shift_left x y in
    let expected = ValueSet (RIC.constant 12l) in
    let passed = equal actual expected in
    print_endline (Printf.sprintf "%s %s << %s -> %s%s"
      (test_label "[ValueSetAbstractions.shift_left]")
      (to_string x)
      (to_string y)
      (to_string actual)
      (if passed then "" else Printf.sprintf " (expected %s)" (to_string expected)));
    passed

  let%test "shift_right_s_negative_singleton" =
    let x = Bitfield (Bitfield.singleton (-8l)) in
    let y = ValueSet (RIC.constant 1l) in
    let actual = shift_right_s x y in
    let expected = ValueSet (RIC.constant (-4l)) in
    let passed = equal actual expected in
    print_endline (Printf.sprintf "%s %s >>s %s -> %s%s"
      (test_label "[ValueSetAbstractions.shift_right_s]")
      (to_string x)
      (to_string y)
      (to_string actual)
      (if passed then "" else Printf.sprintf " (expected %s)" (to_string expected)));
    passed

  let%test "shift_right_u_negative_singleton" =
    let x = Bitfield (Bitfield.singleton (-8l)) in
    let y = ValueSet (RIC.constant 1l) in
    let actual = shift_right_u x y in
    let expected = ValueSet (RIC.constant 2147483644l) in
    let passed = equal actual expected in
    print_endline (Printf.sprintf "%s %s >>u %s -> %s%s"
      (test_label "[ValueSetAbstractions.shift_right_u]")
      (to_string x)
      (to_string y)
      (to_string actual)
      (if passed then "" else Printf.sprintf " (expected %s)" (to_string expected)));
    passed

  let%test "and_mixed_bitfield_value_set" =
    let x = Bitfield (Bitfield.singleton 0b1100l) in
    let y = ValueSet (RIC.constant 0b1010l) in
    let actual = and_ x y in
    let expected = ValueSet (RIC.constant 0b1000l) in
    let passed = equal actual expected in
    print_endline (Printf.sprintf "%s %s & %s -> %s%s"
      (test_label "[ValueSetAbstractions.and_]")
      (to_string x)
      (to_string y)
      (to_string actual)
      (if passed then "" else Printf.sprintf " (expected %s)" (to_string expected)));
    passed

  let%test "or_mixed_bitfield_value_set" =
    let x = Bitfield (Bitfield.singleton 0b1100l) in
    let y = ValueSet (RIC.constant 0b1010l) in
    let actual = or_ x y in
    let expected = ValueSet (RIC.constant 0b1110l) in
    let passed = equal actual expected in
    print_endline (Printf.sprintf "%s %s | %s -> %s%s"
      (test_label "[ValueSetAbstractions.or_]")
      (to_string x)
      (to_string y)
      (to_string actual)
      (if passed then "" else Printf.sprintf " (expected %s)" (to_string expected)));
    passed

  let%test "xor_mixed_bitfield_value_set" =
    let x = Bitfield (Bitfield.singleton 0b1100l) in
    let y = ValueSet (RIC.constant 0b1010l) in
    let actual = xor_ x y in
    let expected = ValueSet (RIC.constant 0b0110l) in
    let passed = equal actual expected in
    print_endline (Printf.sprintf "%s %s xor %s -> %s%s"
      (test_label "[ValueSetAbstractions.xor_]")
      (to_string x)
      (to_string y)
      (to_string actual)
      (if passed then "" else Printf.sprintf " (expected %s)" (to_string expected)));
    passed

  let%test "meet_mixed_value_set_boolean" =
    let value_set = ValueSet (RIC.constant 2l) in
    let boolean = Boolean Boolean.top in
    let actual = meet value_set boolean in
    let expected = ValueSet (RIC.constant 2l) in
    let passed = equal actual expected in
    print_endline (Printf.sprintf "%s %s meet %s -> %s%s"
      (test_label "[ValueSetAbstractions.meet]")
      (to_string value_set)
      (to_string boolean)
      (to_string actual)
      (if passed then "" else Printf.sprintf " (expected %s)" (to_string expected)));
    passed


  let%test "meet_mixed_value_set_boolean_disjoint_numeric" =
    let value_set = ValueSet (RIC.constant 2l) in
    let boolean = Boolean { Boolean.top with numeric_value = RIC.constant 0l } in
    let actual = meet value_set boolean in
    let expected = ValueSet RIC.Bottom in
    let passed = equal actual expected in
    print_endline (Printf.sprintf "%s %s meet %s -> %s%s"
      (test_label "[ValueSetAbstractions.meet]")
      (to_string value_set)
      (to_string boolean)
      (to_string actual)
      (if passed then "" else Printf.sprintf " (expected %s)" (to_string expected)));
    passed

  let%test "join_mixed_value_set_boolean_returns_value_set" =
    let value_set = ValueSet (RIC.constant 2l) in
    let boolean = Boolean { Boolean.top with numeric_value = RIC.constant 4l } in
    let actual = join value_set boolean in
    let expected = ValueSet (RIC.ric (2l, Int 1l, Int 2l, ("", 0l))) in
    let passed = equal actual expected in
    print_endline (Printf.sprintf "%s %s join %s -> %s%s"
      (test_label "[ValueSetAbstractions.join]")
      (to_string value_set)
      (to_string boolean)
      (to_string actual)
      (if passed then "" else Printf.sprintf " (expected %s)" (to_string expected)));
    passed

  let%test "eqz_value_set_creates_boolean" =
    let var = Variable.Var (Var.Other "test") in
    let value_set = ValueSet (RIC.ric (2l, Int 0l, Int 3l, ("", 0l))) in
    let actual = eqz ~var value_set in
    let expected = Boolean {
      Boolean.numeric_value = RIC.ric (1l, Int 0l, Int 1l, ("", 0l));
      true_or_false =
        Variable.Map.singleton var {
          Boolean.True_or_false.true_ = RIC.constant 0l;
          false_ = RIC.ric (2l, Int 1l, Int 3l, ("", 0l));
        };
    } in
    let passed = equal actual expected in
    print_endline (Printf.sprintf "%s eqz %s -> %s%s"
      (test_label "[ValueSetAbstractions.eqz]")
      (to_string value_set)
      (to_string actual)
      (if passed then "" else Printf.sprintf " (expected %s)" (to_string expected)));
    passed

  let%test "eqz_bitfield_creates_boolean" =
    let var = Variable.Var (Var.Other "test") in
    let bitfield = Bitfield (Bitfield.of_string "0:") in
    let actual = eqz ~var bitfield in
    let expected = Boolean {
      Boolean.numeric_value = RIC.ric (1l, Int 0l, Int 1l, ("", 0l));
      true_or_false =
        Variable.Map.singleton var {
          Boolean.True_or_false.true_ = RIC.constant 0l;
          false_ = RIC.constant 1l;
        };
    } in
    let passed = equal actual expected in
    print_endline (Printf.sprintf "%s eqz %s -> %s%s"
      (test_label "[ValueSetAbstractions.eqz]")
      (to_string bitfield)
      (to_string actual)
      (if passed then "" else Printf.sprintf " (expected %s)" (to_string expected)));
    passed

  let%test "and_mixed_value_set_boolean_preserves_refinements" =
    let var = Variable.Var (Var.Other "test") in
    let value_set = ValueSet (RIC.constant 0b1100l) in
    let refinements =
      Variable.Map.singleton var {
        Boolean.True_or_false.true_ = RIC.constant 42l;
        false_ = RIC.zero;
      } in
    let boolean = Boolean {
      Boolean.numeric_value = RIC.constant 0b1010l;
      true_or_false = refinements;
    } in
    let actual = and_ value_set boolean in
    let refinements2 =
      Variable.Map.singleton var {
        Boolean.True_or_false.true_ = RIC.constant 42l;
        false_ = RIC.(join zero (constant 42l));
      } in
    let expected = Boolean {
      Boolean.numeric_value = RIC.constant 0b1000l;
      true_or_false = refinements2;
    } in
    let passed = equal actual expected in
    print_endline (Printf.sprintf "%s %s & %s -> %s%s"
      (test_label "[ValueSetAbstractions.and_]")
      (to_string value_set)
      (to_string boolean)
      (to_string actual)
      (if passed then "" else Printf.sprintf " (expected %s)" (to_string expected)));
    passed

  let%test "or_mixed_value_set_boolean_weakens_true_refinement" =
    let var = Variable.Var (Var.Other "test") in
    let value_set = ValueSet (RIC.constant 0b0100l) in
    let refinements =
      Variable.Map.singleton var {
        Boolean.True_or_false.true_ = RIC.constant 42l;
        false_ = RIC.zero;
      } in
    let boolean = Boolean {
      Boolean.numeric_value = RIC.constant 0b1010l;
      true_or_false = refinements;
    } in
    let actual = or_ value_set boolean in
    let expected_refinements =
      Variable.Map.singleton var {
        Boolean.True_or_false.true_ = RIC.(join zero (constant 42l));
        false_ = RIC.zero;
      } in
    let expected = Boolean {
      Boolean.numeric_value = RIC.constant 0b1110l;
      true_or_false = expected_refinements;
    } in
    let passed = equal actual expected in
    print_endline (Printf.sprintf "%s %s | %s -> %s%s"
      (test_label "[ValueSetAbstractions.or_]")
      (to_string value_set)
      (to_string boolean)
      (to_string actual)
      (if passed then "" else Printf.sprintf " (expected %s)" (to_string expected)));
    passed

  let%test "and_mixed_zero_value_set_boolean_weakens_false_refinement" =
    let var = Variable.Var (Var.Other "test") in
    let value_set = ValueSet (RIC.constant 0l) in
    let refinements =
      Variable.Map.singleton var {
        Boolean.True_or_false.true_ = RIC.constant 42l;
        false_ = RIC.zero;
      } in
    let boolean = Boolean {
      Boolean.numeric_value = RIC.constant 0b1010l;
      true_or_false = refinements;
    } in
    let actual = and_ value_set boolean in
    let refinements2 =
      Variable.Map.singleton var {
        Boolean.True_or_false.true_ = RIC.constant 42l;
        false_ = RIC.(join zero (constant 42l));
      } in
    let expected = Boolean {
      Boolean.numeric_value = RIC.zero;
      true_or_false = refinements2;
    } in
    let passed = equal actual expected in
    print_endline (Printf.sprintf "%s %s & %s -> %s%s"
      (test_label "[ValueSetAbstractions.and_]")
      (to_string value_set)
      (to_string boolean)
      (to_string actual)
      (if passed then "" else Printf.sprintf " (expected %s)" (to_string expected)));
    passed

  let%test "and_mixed_bitfield_boolean_weakens_false_refinement" =
    let var = Variable.Var (Var.Other "test") in
    let bitfield = Bitfield (Bitfield.singleton 0b1100l) in
    let refinements =
      Variable.Map.singleton var {
        Boolean.True_or_false.true_ = RIC.constant 42l;
        false_ = RIC.zero;
      } in
    let boolean = Boolean {
      Boolean.numeric_value = RIC.constant 0b1010l;
      true_or_false = refinements;
    } in
    let actual = and_ bitfield boolean in
    let expected_refinements =
      Variable.Map.singleton var {
        Boolean.True_or_false.true_ = RIC.constant 42l;
        false_ = RIC.(join zero (constant 42l));
      } in
    let expected = Boolean {
      Boolean.numeric_value = RIC.constant 0b1000l;
      true_or_false = expected_refinements;
    } in
    let passed = equal actual expected in
    print_endline (Printf.sprintf "%s %s & %s -> %s%s"
      (test_label "[ValueSetAbstractions.and_]")
      (to_string bitfield)
      (to_string boolean)
      (to_string actual)
      (if passed then "" else Printf.sprintf " (expected %s)" (to_string expected)));
    passed

  let%test "and_mixed_boolean_bitfield_weakens_false_refinement" =
    let var = Variable.Var (Var.Other "test") in
    let bitfield = Bitfield (Bitfield.singleton 0b1100l) in
    let refinements =
      Variable.Map.singleton var {
        Boolean.True_or_false.true_ = RIC.constant 42l;
        false_ = RIC.zero;
      } in
    let boolean = Boolean {
      Boolean.numeric_value = RIC.constant 0b1010l;
      true_or_false = refinements;
    } in
    let actual = and_ boolean bitfield in
    let expected_refinements =
      Variable.Map.singleton var {
        Boolean.True_or_false.true_ = RIC.constant 42l;
        false_ = RIC.(join zero (constant 42l));
      } in
    let expected = Boolean {
      Boolean.numeric_value = RIC.constant 0b1000l;
      true_or_false = expected_refinements;
    } in
    let passed = equal actual expected in
    print_endline (Printf.sprintf "%s %s & %s -> %s%s"
      (test_label "[ValueSetAbstractions.and_]")
      (to_string boolean)
      (to_string bitfield)
      (to_string actual)
      (if passed then "" else Printf.sprintf " (expected %s)" (to_string expected)));
    passed

  let%test "or_mixed_bitfield_boolean_weakens_true_refinement" =
    let var = Variable.Var (Var.Other "test") in
    let bitfield = Bitfield (Bitfield.singleton 0b0100l) in
    let refinements =
      Variable.Map.singleton var {
        Boolean.True_or_false.true_ = RIC.constant 42l;
        false_ = RIC.zero;
      } in
    let boolean = Boolean {
      Boolean.numeric_value = RIC.constant 0b1010l;
      true_or_false = refinements;
    } in
    let actual = or_ bitfield boolean in
    let expected_refinements =
      Variable.Map.singleton var {
        Boolean.True_or_false.true_ = RIC.(join zero (constant 42l));
        false_ = RIC.zero;
      } in
    let expected = Boolean {
      Boolean.numeric_value = RIC.constant 0b1110l;
      true_or_false = expected_refinements;
    } in
    let passed = equal actual expected in
    print_endline (Printf.sprintf "%s %s | %s -> %s%s"
      (test_label "[ValueSetAbstractions.or_]")
      (to_string bitfield)
      (to_string boolean)
      (to_string actual)
      (if passed then "" else Printf.sprintf " (expected %s)" (to_string expected)));
    passed

  let%test "or_mixed_boolean_bitfield_weakens_true_refinement" =
    let var = Variable.Var (Var.Other "test") in
    let bitfield = Bitfield (Bitfield.singleton 0b0100l) in
    let refinements =
      Variable.Map.singleton var {
        Boolean.True_or_false.true_ = RIC.constant 42l;
        false_ = RIC.zero;
      } in
    let boolean = Boolean {
      Boolean.numeric_value = RIC.constant 0b1010l;
      true_or_false = refinements;
    } in
    let actual = or_ boolean bitfield in
    let expected_refinements =
      Variable.Map.singleton var {
        Boolean.True_or_false.true_ = RIC.(join zero (constant 42l));
        false_ = RIC.zero;
      } in
    let expected = Boolean {
      Boolean.numeric_value = RIC.constant 0b1110l;
      true_or_false = expected_refinements;
    } in
    let passed = equal actual expected in
    print_endline (Printf.sprintf "%s %s | %s -> %s%s"
      (test_label "[ValueSetAbstractions.or_]")
      (to_string boolean)
      (to_string bitfield)
      (to_string actual)
      (if passed then "" else Printf.sprintf " (expected %s)" (to_string expected)));
    passed

  let%test "xor_mixed_bitfield_boolean_returns_value_set" =
    let var = Variable.Var (Var.Other "test") in
    let bitfield = Bitfield (Bitfield.singleton 0b1100l) in
    let boolean = Boolean {
      Boolean.numeric_value = RIC.constant 0b1010l;
      true_or_false =
        Variable.Map.singleton var {
          Boolean.True_or_false.true_ = RIC.constant 42l;
          false_ = RIC.zero;
        };
    } in
    let actual = xor_ bitfield boolean in
    let expected = ValueSet (RIC.constant 0b0110l) in
    let passed = equal actual expected in
    print_endline (Printf.sprintf "%s %s xor %s -> %s%s"
      (test_label "[ValueSetAbstractions.xor_]")
      (to_string bitfield)
      (to_string boolean)
      (to_string actual)
      (if passed then "" else Printf.sprintf " (expected %s)" (to_string expected)));
    passed

  let%test "xor_mixed_boolean_bitfield_returns_value_set" =
    let var = Variable.Var (Var.Other "test") in
    let bitfield = Bitfield (Bitfield.singleton 0b1100l) in
    let boolean = Boolean {
      Boolean.numeric_value = RIC.constant 0b1010l;
      true_or_false =
        Variable.Map.singleton var {
          Boolean.True_or_false.true_ = RIC.constant 42l;
          false_ = RIC.zero;
        };
    } in
    let actual = xor_ boolean bitfield in
    let expected = ValueSet (RIC.constant 0b0110l) in
    let passed = equal actual expected in
    print_endline (Printf.sprintf "%s %s xor %s -> %s%s"
      (test_label "[ValueSetAbstractions.xor_]")
      (to_string boolean)
      (to_string bitfield)
      (to_string actual)
      (if passed then "" else Printf.sprintf " (expected %s)" (to_string expected)));
    passed

  let%test "update_relative_offset_boolean_returns_value_set" =
    let boolean = Boolean { Boolean.top with numeric_value = RIC.ric (4l, Int 0l, Int 2l, ("x", 1l)) } in
    let x_value = RIC.constant 8l in
    let actual_values =
      String.Map.empty
      |> Map.add_exn ~key:"x" ~data:x_value in
    let actual = update_relative_offset boolean actual_values in
    let expected = ValueSet (RIC.ric (4l, Int 0l, Int 2l, ("", 9l))) in
    let passed = equal actual expected in
    print_endline (Printf.sprintf "%s %s with x = %s -> %s%s"
      (test_label "[ValueSetAbstractions.update_relative_offset]")
      (to_string boolean)
      (RIC.to_string x_value)
      (to_string actual)
      (if passed then "" else Printf.sprintf " (expected %s)" (to_string expected)));
    passed

  let%test "count_trailing_zeros" =
    let bf = Bitfield (Bit { zeros = 0b1111_1111_1111_1111_1100_1101_1101_1111l;
                              ones = 0b0000_0000_0000_0000_0011_0010_1110_1010l}) in
    let actual = count_trailing_zeros bf in
    let expected = ValueSet (RIC.ric (2l, Int 0l, Int 2l, ("", 1l))) in
    let passed = equal actual expected in
    print_endline (Printf.sprintf "%s %s -> %s%s"
      (test_label "[ValueSetAbstractions.count_trailing_zeros]")
      (to_string bf)
      (to_string actual)
      (if passed then "" else Printf.sprintf " (expected %s)" (to_string expected)));
    passed


  let%test "count_leading_zeros" =
    let bf = Bitfield (Bit { zeros = 0b1111_1111_1111_1111_1111_1101_1101_1111l;
                              ones = 0b0000_0000_0010_0010_0010_0010_1110_1010l}) in
    let actual = count_leading_zeros bf in
    let expected = ValueSet (RIC.ric (4l, Int 0l, Int 3l, ("", 10l))) in
    let passed = equal actual expected in
    print_endline (Printf.sprintf "%s %s -> %s%s"
      (test_label "[ValueSetAbstractions.count_leading_zeros]")
      (to_string bf)
      (to_string actual)
      (if passed then "" else Printf.sprintf " (expected %s)" (to_string expected)));
    passed

  let%test "population_count" =
    let bf = Bitfield (Bit { zeros = 0b1111_1111_1111_1111_1111_1101_1101_1111l;
                              ones = 0b0000_0000_0010_0010_0010_0010_1110_1010l}) in
    let actual = population_count bf in
    let expected = ValueSet (RIC.ric (1l, Int 2l, Int 9l, ("", 0l))) in
    let passed = equal actual expected in
    print_endline (Printf.sprintf "%s %s -> %s%s"
      (test_label "[ValueSetAbstractions.population_count]")
      (to_string bf)
      (to_string actual)
      (if passed then "" else Printf.sprintf " (expected %s)" (to_string expected)));
    passed

  let%test "shift_left: Top by constant is not Bottom" =
    let x = ValueSet RIC.Top in
    let shift = ValueSet (RIC.constant 2l) in
    let actual = shift_left x shift in
    let expected = ValueSet RIC.Bottom in
    let passed = actual <> expected in
    print_endline (Printf.sprintf "%s %s << %s -> %s%s"
      (test_label "[ValueSetAbstractions.shift_left]")
      (to_string x)
      (to_string shift)
      (to_string actual)
      (if passed then "" else Printf.sprintf " (expected anything but %s)" (to_string expected)));
    passed

  let%test "shift_left: relative offset by constant is not Bottom" =
    let x = ValueSet (RIC.relative_ric "l0") in
    let shift = ValueSet (RIC.constant 2l) in
    let actual = shift_left x shift in
    let expected = ValueSet (RIC.ric (4l, NegInfinity, Infinity, ("", 0l))) in
    let passed = actual = expected in
    print_endline (Printf.sprintf "%s %s << %s -> %s%s"
      (test_label "[ValueSetAbstractions.shift_left]")
      (to_string x)
      (to_string shift)
      (to_string actual)
      (if passed then "" else Printf.sprintf " (expected %s)" (to_string expected)));
    passed
end)
    