open Core
open Maths

let (|>>) (c, i : 'a * 'b) (f : 'a -> 'b -> 'c) : 'c = f c i

(** {2 Interval}

    Intervals use extended integers for bounds: [−∞], finite [Int n], and [∞].
    [Top] is the whole line (]−∞,∞[), [Bottom] is ∅. Operators implement the usual
    lattice on intervals.
*)

  type interval = {
    lower_bound : ExtendedInt.t;
    upper_bound : ExtendedInt.t
  }

  (** Abstract values for intervals.
      - [Top] : ]−∞,∞[
      - [Bottom] : ∅
      - [Interval {lower_bound; upper_bound}] : closed interval with possibly
        unbounded endpoints. *)
  type t =
    | Top
    | Bottom
    | Interval of interval

  let make (lower_bound : ExtendedInt.t) (upper_bound : ExtendedInt.t) : t = Interval {lower_bound; upper_bound}

  (** [equal i1 i2]
      Set equality, collapsing canonical Top/Bottom normalizations. *)
  let equal (i1 : t) (i2 : t) : bool =
    match i1, i2 with 
    | Top, Top | Bottom, Bottom -> true  
    | Top, Interval {lower_bound = NegInfinity; upper_bound = Infinity}
    | Interval {lower_bound = NegInfinity; upper_bound = Infinity}, Top -> true
    | Bottom, Interval {lower_bound = l; upper_bound = u} 
    | Interval {lower_bound = l; upper_bound = u}, Bottom 
      when ExtendedInt.(u < l) -> true
    | Interval {lower_bound = l1; upper_bound = u1},
      Interval {lower_bound = l2; upper_bound = u2} 
      when ExtendedInt.(u1 < l1) && ExtendedInt.(u2 < l2) -> true
    | Interval i1, Interval i2 -> 
      (ExtendedInt.(i1.lower_bound = i2.lower_bound) 
      && ExtendedInt.(i1.upper_bound = i2.upper_bound))
    | _ -> false

  let (=) = equal
  let (<>) (i1 : t) (i2 : t) : bool = not (i1 = i2)

  (** [to_string i]
      Math‑style printer: "[a,b]", "]−∞,b]", "[a,∞[", or "ℤ"/"∅" as canonical cases. *)
  let to_string ?(no_reduction : bool = false) (i : t) : string =
    match i with 
    | Top -> "ℤ"
    | Interval _ when i = Top -> 
      if no_reduction then 
        "]" ^ ExtendedInt.to_string NegInfinity ^ ", " ^ ExtendedInt.to_string Infinity ^ "[" 
      else 
        "ℤ"
    | Bottom -> "∅"
    | Interval _ when i = Bottom -> "∅"
    | Interval i ->
      begin match i.lower_bound, i.upper_bound with 
        | Int l, Infinity -> 
          "[" ^ Int32.to_string l ^ "," ^ ExtendedInt.to_string Infinity ^ "["
        | NegInfinity, Int u -> 
          "]" ^ ExtendedInt.to_string NegInfinity ^ "," ^ Int32.to_string u ^ "]"
        | Int l, Int u when Int32.compare l u <= 0 -> "[" ^ Int32.to_string l ^ "," ^ Int32.to_string u ^ "]"
        | _ -> assert false
      end

  (** [join i1 i2]
      Least upper bound with Top/Bottom neutral/absorbing behavior. *)
  let join (i1 : t) (i2 : t) : t =
    let join =
      match i1, i2 with 
      | Top, _ | _, Top -> Top
      | Bottom, i | i, Bottom -> i
      | i1, i2 when i1 = Bottom -> i2
      | i1, i2 when i2 = Bottom -> i1
      | Interval i1, Interval i2 ->
        (ExtendedInt.minimum i1.lower_bound i2.lower_bound,
        ExtendedInt.maximum i1.upper_bound i2.upper_bound)
        |>> make
    in
    if join = Top then Top else if join = Bottom then Bottom else join

  (** [meet i1 i2]
      Greatest lower bound (intersection) with empty‑set detection. *)
  let meet (i1 : t) (i2 : t) : t =
    let meet =
      match i1, i2 with 
      | Bottom, _ | _, Bottom -> Bottom 
      | Top, i | i, Top -> i
      | i1, _ when i1 = Bottom -> Bottom
      | _, i2 when i2 = Bottom -> Bottom
      | Interval i1, Interval i2 ->
        (ExtendedInt.maximum i1.lower_bound i2.lower_bound,
        ExtendedInt.minimum i1.upper_bound i2.upper_bound)
        |>> make
    in if meet = Top then Top else if meet = Bottom then Bottom else meet

  (** [widen i1 i2]
      Standard widening: when a bound moves outward, drop it to the corresponding
      infinity; otherwise keep the previous bound. *)
  let widen (i1 : t) (i2 : t) : t =
    match i1, i2 with 
    | Top, _ | _, Top -> Top
    | Bottom, i | i, Bottom -> i
    | i1, _ when i1 = Bottom -> i2
    | _, i2 when i2 = Bottom -> i1
    | Interval {lower_bound = l1; upper_bound = u1}, 
      Interval {lower_bound = l2; upper_bound = u2} ->
        ((if ExtendedInt.(l2 < l1) then ExtendedInt.NegInfinity else l1),
        if ExtendedInt.(u1 < u2) then ExtendedInt.Infinity else u1)
        |>> make

  (** [plus i1 i2]
      Minkowski sum of intervals on extended integers. *)
  let plus (i1 : t) (i2 : t) : t =
    match i1, i2 with
    | Top, _ | _, Top -> Top
    | Bottom, _ | _, Bottom -> Bottom
    | Interval {lower_bound = l1; upper_bound = u1}, 
      Interval {lower_bound = l2; upper_bound = u2} ->
        (ExtendedInt.(l1 + l2), ExtendedInt.(u1 + u2)) |>> make

  let (+) = plus

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

let%test_module "Interval tests" = (module struct
  let%test "RIC_tests" =
  print_endline "\n_______ ____________________________ _______\n        Interval module        \n------- ---------------------------- -------\n"; true

  (* Top is equal to Top. *)
  let%test "equal_top_top" =
    equal Top Top

  (* Bottom is equal to Bottom. *)
  let%test "equal_bottom_bottom" =
    equal Bottom Bottom

  (* Top is not equal to Bottom. *)
  let%test "not_equal_top_bottom" =
    not (equal Top Bottom)

  (* Two intervals with same bounds are equal. *)
  let%test "equal_same_bounds" =
    equal
      (Interval {lower_bound = Int 0l; upper_bound = Int 5l})
      (Interval {lower_bound = Int 0l; upper_bound = Int 5l})

  (* Intervals with different lower bounds are not equal. *)
  let%test "not_equal_diff_lower" =
    not (equal
          (Interval {lower_bound = Int 1l; upper_bound = Int 5l})
          (Interval {lower_bound = Int 0l; upper_bound = Int 5l}))

  (* Intervals with different upper bounds are not equal. *)
  let%test "not_equal_diff_upper" =
    not (equal
          (Interval {lower_bound = Int 0l; upper_bound = Int 6l})
          (Interval {lower_bound = Int 0l; upper_bound = Int 5l}))

  (* Intervals with one infinite bound are not equal to finite-bounded intervals. *)
  let%test "not_equal_finite_vs_infinite" =
    not (equal
          (Interval {lower_bound = NegInfinity; upper_bound = Int 5l})
          (Interval {lower_bound = Int 0l; upper_bound = Int 5l}))

  let%test "equal_neginf_Infinity" =
    equal
      (Interval {lower_bound = NegInfinity; upper_bound = Infinity})
      (Interval {lower_bound = NegInfinity; upper_bound = Infinity})

  (* to_string of Top should be "ℤ". *)
  let%test "to_string_top" =
    let s = to_string Top in
    print_endline ("[to_string]     Top → " ^ s);
    String.equal s "ℤ"

  (* to_string of Bottom should be "∅". *)
  let%test "to_string_bottom" =
    let s = to_string Bottom in
    print_endline ("[to_string]     Bottom → " ^ s);
    String.equal s "∅"

  (* to_string of [0, 5] should be "[0, 5]". *)
  let%test "to_string_0_5" =
    let s = to_string (Interval {lower_bound = Int 0l; upper_bound = Int 5l}) in
    print_endline ("[to_string]     [0,5] → " ^ s);
    String.equal s "[0,5]"

  (* to_string of [-∞, 10] should be "[-∞, 10]". *)
  let%test "to_string_neg_inf_10" =
    let s = to_string (Interval {lower_bound = NegInfinity; upper_bound = Int 10l}) in
    print_endline ("[to_string]     ]-∞,10] → " ^ s);
    String.equal s "]-∞,10]"

  (* to_string of [4, ∞] should be "[4, ∞]". *)
  let%test "to_string_4_pos_inf" =
    let s = to_string (Interval {lower_bound = Int 4l; upper_bound = Infinity}) in
    print_endline ("[to_string]     [4,∞[ → " ^ s);
    String.equal s "[4,∞["

  (* to_string of [-∞, ∞] should be "ℤ". *)
  let%test "to_string_neg_inf_pos_inf" =
    let s = to_string (Interval {lower_bound = NegInfinity; upper_bound = Infinity}) in
    print_endline ("[to_string]     [-∞,∞] → " ^ s);
    String.equal s "ℤ"


  (* join of Top with anything is Top. *)
  let%test "join_top" =
    let joined = join Top (Interval {lower_bound = Int 0l; upper_bound = Int 5l}) in
    print_endline ("[JOIN of intervals]     Top ⊔ [0, 5] → " ^ to_string joined);
    equal joined Top

  (* join of Bottom with an interval is that  *)
  let%test "join_bottom" =
    let i = Interval {lower_bound = Int 2l; upper_bound = Int 4l} in
    let joined = join Bottom i in
    print_endline ("[JOIN of intervals]     ⊥ ⊔ [2, 4] → " ^ to_string joined);
    equal joined i

  (* join of two intervals gives correct bounds. *)
  let%test "[JOIN of intervals]     join_0_5_3_10" =
    let a = Interval {lower_bound = Int 0l; upper_bound = Int 5l} in
    let b = Interval {lower_bound = Int 3l; upper_bound = Int 10l} in
    let joined = join a b in
    print_endline ("[JOIN of intervals]     [0, 5] ⊔ [3, 10] → " ^ to_string joined);
    equal joined (Interval {lower_bound = Int 0l; upper_bound = Int 10l})

  (* join of two disjoint intervals gives correct bounds. *)
  let%test "join_0_5_7_10" =
    let a = Interval {lower_bound = Int 0l; upper_bound = Int 5l} in
    let b = Interval {lower_bound = Int 7l; upper_bound = Int 10l} in
    let joined = join a b in
    print_endline ("[JOIN of intervals]     [0, 5] ⊔ [7, 10] → " ^ to_string joined);
    equal joined (Interval {lower_bound = Int 0l; upper_bound = Int 10l})

  (* meet of Top and an interval is that  *)
  let%test "meet_top" =
    let i = Interval {lower_bound = Int 1l; upper_bound = Int 7l} in
    let met = meet Top i in
    print_endline ("[MEET of intervals]     Top ⊓ [1, 7] → " ^ to_string met);
    equal met i

  (* meet of disjoint intervals is Bottom. *)
  let%test "meet_disjoint" =
    let a = Interval {lower_bound = Int 0l; upper_bound = Int 2l} in
    let b = Interval {lower_bound = Int 5l; upper_bound = Int 10l} in
    let met = meet a b in
    print_endline ("[MEET of intervals]     [0, 2] ⊓ [5, 10] → " ^ to_string met);
    equal met Bottom

  (* meet of overlapping intervals gives the intersection. *)
  let%test "meet_overlap" =
    let a = Interval {lower_bound = Int 0l; upper_bound = Int 10l} in
    let b = Interval {lower_bound = Int 5l; upper_bound = Int 15l} in
    let met = meet a b in
    print_endline ("[MEET of intervals]     [0, 10] ⊓ [5, 15] → " ^ to_string met);
    equal met (Interval {lower_bound = Int 5l; upper_bound = Int 10l})

  (* join with negative infinity preserves the unbounded lower side. *)
  let%test "join_neg_inf_with_finite" =
    let a = Interval {lower_bound = NegInfinity; upper_bound = Int 5l} in
    let b = Interval {lower_bound = Int 0l; upper_bound = Int 10l} in
    let joined = join a b in
    print_endline ("[JOIN of intervals]     ]-∞,5] ⊔ [0,10] → " ^ to_string joined);
    equal joined (Interval {lower_bound = NegInfinity; upper_bound = Int 10l})

  (* join with positive infinity preserves the unbounded upper side. *)
  let%test "join_pos_inf_with_finite" =
    let a = Interval {lower_bound = Int 0l; upper_bound = Infinity} in
    let b = Interval {lower_bound = Int (-5l); upper_bound = Int 10l} in
    let joined = join a b in
    print_endline ("[JOIN of intervals]     [0,∞[ ⊔ [-5,10] → " ^ to_string joined);
    equal joined (Interval {lower_bound = Int (-5l); upper_bound = Infinity})

  (* joining intervals that cover both infinities gives Top. *)
  let%test "join_neg_inf_with_pos_inf_gives_top" =
    let a = Interval {lower_bound = NegInfinity; upper_bound = Int 5l} in
    let b = Interval {lower_bound = Int 0l; upper_bound = Infinity} in
    let joined = join a b in
    print_endline ("[JOIN of intervals]     ]-∞,5] ⊔ [0,∞[ → " ^ to_string joined);
    equal joined Top

  (* canonical empty intervals behave like Bottom in joins. *)
  let%test "join_empty_interval_left" =
    let empty = Interval {lower_bound = Int 5l; upper_bound = Int 0l} in
    let i = Interval {lower_bound = Int 1l; upper_bound = Int 3l} in
    let joined = join empty i in
    print_endline ("[JOIN of intervals]     empty ⊔ [1,3] → " ^ to_string joined);
    equal joined i

  (* canonical empty intervals behave like Bottom in joins. *)
  let%test "join_empty_interval_right" =
    let i = Interval {lower_bound = Int 1l; upper_bound = Int 3l} in
    let empty = Interval {lower_bound = Int 5l; upper_bound = Int 0l} in
    let joined = join i empty in
    print_endline ("[JOIN of intervals]     [1,3] ⊔ empty → " ^ to_string joined);
    equal joined i

  (* meet with opposite infinities keeps only the finite overlap. *)
  let%test "meet_neg_inf_with_pos_inf_overlap" =
    let a = Interval {lower_bound = NegInfinity; upper_bound = Int 10l} in
    let b = Interval {lower_bound = Int 0l; upper_bound = Infinity} in
    let met = meet a b in
    print_endline ("[MEET of intervals]     ]-∞,10] ⊓ [0,∞[ → " ^ to_string met);
    equal met (Interval {lower_bound = Int 0l; upper_bound = Int 10l})

  (* meet with opposite infinities can be empty. *)
  let%test "meet_neg_inf_with_pos_inf_disjoint" =
    let a = Interval {lower_bound = NegInfinity; upper_bound = Int 0l} in
    let b = Interval {lower_bound = Int 1l; upper_bound = Infinity} in
    let met = meet a b in
    print_endline ("[MEET of intervals]     ]-∞,0] ⊓ [1,∞[ → " ^ to_string met);
    equal met Bottom

  (* meet of intervals touching at one endpoint gives a singleton  *)
  let%test "meet_touching_boundaries" =
    let a = Interval {lower_bound = Int 0l; upper_bound = Int 5l} in
    let b = Interval {lower_bound = Int 5l; upper_bound = Int 10l} in
    let met = meet a b in
    print_endline ("[MEET of intervals]     [0,5] ⊓ [5,10] → " ^ to_string met);
    equal met (Interval {lower_bound = Int 5l; upper_bound = Int 5l})

  (* widening keeps the interval unchanged when bounds do not move outward. *)
  let%test "widen_same_interval" =
    let a = Interval {lower_bound = Int 0l; upper_bound = Int 5l} in
    let widened = widen a a in
    print_endline ("[WIDEN of intervals]    [0,5] ▽ [0,5] → " ^ to_string widened);
    equal widened a

  (* widening drops the lower bound to -∞ when it moves downward. *)
  let%test "widen_lower_moves_downward" =
    let previous = Interval {lower_bound = Int 0l; upper_bound = Int 5l} in
    let next = Interval {lower_bound = Int (-1l); upper_bound = Int 5l} in
    let widened = widen previous next in
    print_endline ("[WIDEN of intervals]    [0,5] ▽ [-1,5] → " ^ to_string widened);
    equal widened (Interval {lower_bound = NegInfinity; upper_bound = Int 5l})

  (* widening drops the upper bound to +∞ when it moves upward. *)
  let%test "widen_upper_moves_upward" =
    let previous = Interval {lower_bound = Int 0l; upper_bound = Int 5l} in
    let next = Interval {lower_bound = Int 0l; upper_bound = Int 6l} in
    let widened = widen previous next in
    print_endline ("[WIDEN of intervals]    [0,5] ▽ [0,6] → " ^ to_string widened);
    equal widened (Interval {lower_bound = Int 0l; upper_bound = Infinity})

  (* widening drops both bounds when both move outward. *)
  let%test "widen_both_bounds_move_outward" =
    let previous = Interval {lower_bound = Int 0l; upper_bound = Int 5l} in
    let next = Interval {lower_bound = Int (-1l); upper_bound = Int 6l} in
    let widened = widen previous next in
    print_endline ("[WIDEN of intervals]    [0,5] ▽ [-1,6] → " ^ to_string widened);
    equal widened Top

  (* widening Bottom with an interval returns that  *)
  let%test "widen_bottom_left" =
    let i = Interval {lower_bound = Int 2l; upper_bound = Int 4l} in
    let widened = widen Bottom i in
    print_endline ("[WIDEN of intervals]    Bottom ▽ [2,4] → " ^ to_string widened);
    equal widened i

  (* widening an interval with Bottom returns that  *)
  let%test "widen_bottom_right" =
    let i = Interval {lower_bound = Int 2l; upper_bound = Int 4l} in
    let widened = widen i Bottom in
    print_endline ("[WIDEN of intervals]    [2,4] ▽ Bottom → " ^ to_string widened);
    equal widened i

  (* sum of two finite intervals adds both bounds. *)
  let%test "sum_finite_intervals" =
    let a = Interval {lower_bound = Int 1l; upper_bound = Int 3l} in
    let b = Interval {lower_bound = Int 4l; upper_bound = Int 6l} in
    let sum = plus a b in
    print_endline ("[SUM of intervals]      [1,3] + [4,6] → " ^ to_string sum);
    equal sum (Interval {lower_bound = Int 5l; upper_bound = Int 9l})

  (* sum of a finite interval and an interval unbounded above is unbounded above. *)
  let%test "sum_finite_with_pos_inf" =
    let a = Interval {lower_bound = Int 1l; upper_bound = Int 3l} in
    let b = Interval {lower_bound = Int 4l; upper_bound = Infinity} in
    let sum = plus a b in
    print_endline ("[SUM of intervals]      [1,3] + [4,∞[ → " ^ to_string sum);
    equal sum (Interval {lower_bound = Int 5l; upper_bound = Infinity})

  (* sum of an interval unbounded below and a finite interval is unbounded below. *)
  let%test "sum_neg_inf_with_finite" =
    let a = Interval {lower_bound = NegInfinity; upper_bound = Int 3l} in
    let b = Interval {lower_bound = Int 4l; upper_bound = Int 6l} in
    let sum = plus a b in
    print_endline ("[SUM of intervals]      ]-∞,3] + [4,6] → " ^ to_string sum);
    equal sum (Interval {lower_bound = NegInfinity; upper_bound = Int 9l})

  (* sum with Top returns Top. *)
  let%test "sum_top_left" =
    let i = Interval {lower_bound = Int 1l; upper_bound = Int 3l} in
    let sum = plus Top i in
    print_endline ("[SUM of intervals]      Top + [1,3] → " ^ to_string sum);
    equal sum Top

  (* sum with Top returns Top. *)
  let%test "sum_top_right" =
    let i = Interval {lower_bound = Int 1l; upper_bound = Int 3l} in
    let sum = plus i Top in
    print_endline ("[SUM of intervals]      [1,3] + Top → " ^ to_string sum);
    equal sum Top

  (* sum with Bottom currently returns the other interval, matching the implementation. *)
  let%test "sum_bottom_left" =
    let i = Interval {lower_bound = Int 1l; upper_bound = Int 3l} in
    let sum = plus Bottom i in
    print_endline ("[SUM of intervals]      Bottom + [1,3] → " ^ to_string sum);
    equal sum Bottom

  (* sum with Bottom currently returns the other interval, matching the implementation. *)
  let%test "sum_bottom_right" =
    let i = Interval {lower_bound = Int 1l; upper_bound = Int 3l} in
    let sum = plus i Bottom in
    print_endline ("[SUM of intervals]      [1,3] + Bottom → " ^ to_string sum);
    equal sum Bottom
end)