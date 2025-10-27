open Core 
open Maths
open Bitfield

(** The Reduced Interval Congruence (RIC) domain for pointer analysis.

    This module defines an abstract domain used to represent possible integer values in
    pointer analysis. It combines:
    - A congruence domain for arithmetic progressions of the form [stride * ℤ + offset].
    - An interval domain for bounding integer ranges.

    RIC values are reduced for precision and support standard abstract operations such as
    join, meet, widening, addition, and subtraction.
*)
module RIC = struct

  (** The congruence domain for arithmetic progressions of the form [stride * ℤ + offset].

      Offsets may be absolute or relative. Provides standard abstract operations and
      equality up to congruence equivalence.
  *)
  module Congruence = struct
    type congruence = {
      stride : int32;
      offset : string * int32
    }

    type t =
      | Top (* ℤ *)
      | Bottom (* ∅ *)
      | Congruence of congruence

    (** [equal c1 c2] returns [true] if congruences [c1] and [c2] are equivalent. *)
    let equal (c1 : t) (c2 : t) : bool =
      match c1, c2 with 
      | Top, Top | Bottom, Bottom -> true
      | Top, Congruence { stride = 1l; _ }
      | Congruence { stride = 1l; _ }, Top -> true 
      | Congruence { stride = 1l; _ }, Congruence { stride = 1l; _ } -> true
      | Congruence { stride = 0l; offset = (v1, o1) }, Congruence { stride = 0l; offset = (v2, o2) } ->
        String.equal v1 v2 && Int32.equal o1 o2
      | Congruence { stride = s1; offset = (v1, o1) }, Congruence { stride = s2; offset = (v2, o2) } ->
        (* Int32.equal s1 s2 && String.equal v1 v2 && (o1 - o2) mod s1 = 0 *)
        Int32.equal s1 s2 && String.equal v1 v2 && Int32.(Int32.(%) (Int32.(-) o1  o2) s1 = 0l)
      | _ -> false

    let to_string (c : t) : string =
      match c with 
      | Top -> "ℤ" 
      | Bottom -> "∅"
      | Congruence { stride = s; offset = offset } ->
        let stride = 
          if Int32.equal s 0l then
            ""
          else
            (if (Int32.equal s 1l) then "" else Int32.to_string s) ^ "ℤ" in
        let offset = 
          match offset with 
          | ("", offset) ->
            if Int32.equal offset 0l then
              ""
            else
              Int32.to_string offset
          | (var, offset) ->
            if Int32.equal offset 0l then
              var
            else
              "(" ^ var ^ "+" ^ Int32.to_string offset ^ ")"
        in
        match stride, offset with 
        | "", "" -> "∅"
        | "", offset -> offset 
        | "ℤ", _ -> "ℤ"
        | stride, "" -> stride 
        | stride, offset -> stride ^ "+" ^ offset

    (** [join c1 c2] returns the least upper bound (union) of [c1] and [c2]. *)
    let join (c1 : t) (c2 : t) : t =
      match c1, c2 with 
      | Top, _ | _, Top -> Top 
      | Bottom, c | c, Bottom -> c 
      | Congruence { offset = (_, o1); _ }, Congruence { offset = (_, o2); _ } 
        when Int32.(o1 = (-2147483648l)) || Int32.(o2 = (-2147483648l)) -> Top
      | Congruence { stride = s1; offset = (v1, o1) }, Congruence { stride = s2; offset = (v2, o2) } ->
        if String.equal v1 v2 then
          let new_offset = (v1, Int32.min o1 o2) in
          (* print_endline(Int32.to_string o1 ^ " - " ^ Int32.to_string o2 ^ " = " ^ Int32.to_string Int32.(o1 - o2)); *)
          let stride_difference = Int32.abs (Int32.(o1 - o2)) in
          (* print_endline ("calculating new stride: s1=" ^ Int32.to_string s1 ^ " s2=" ^ Int32.to_string s2 ^ " stride difference=" ^ Int32.to_string stride_difference); *)
          let new_stride = gcd (gcd s1 s2) stride_difference in
          Congruence { stride = new_stride; offset = new_offset }
        else
          Top

    (** [meet c1 c2] returns the greatest lower bound (intersection) of [c1] and [c2]. *)
    let meet (c1 : t) (c2 : t) : t =
      match c1, c2 with
      | Top, c | c, Top -> c 
      | Bottom, _ | _, Bottom -> Bottom 
      | Congruence {stride = 0l; offset = (v1, o1)}, Congruence {stride = 0l; offset = (v2, o2)} ->
        if String.equal v1 v2 && Int32.equal o1 o2 then c1 else Bottom
      | Congruence {stride = 0l; offset = (v1, o1)}, Congruence {stride = s2; offset = (v2, o2)} ->
        if String.equal v1 v2 && Int32.((Int32.(%) o1 s2) = o2) then Congruence {stride = 0l; offset = (v1, o1)} else Bottom
      | Congruence {stride = s2; offset = (v2, o2)}, Congruence {stride = 0l; offset = (v1, o1)} ->
        if String.equal v1 v2 && Int32.(Int32.(%) o1 s2 = o2) then Congruence {stride = 0l; offset = (v1, o1)} else Bottom
      | Congruence {stride = s1; offset = (v1, o1)}, Congruence {stride = s2; offset = (v2, o2)} ->
        let gcd_stride = gcd s1 s2 in 
        if String.equal v1 v2 && Int32.(Int32.(%) (Int32.(-) o1 o2) gcd_stride = 0l) then 
              let new_stride = lcm s1 s2 in 
              let new_offset, _ = chinese_remainder s1 o1 s2 o2 in
              let new_offset = (v1, new_offset) in
              Congruence {stride = new_stride; offset = new_offset}
        else
          Bottom
    
    let widen (c1 :t) (c2 : t) : t = join c1 c2

    (** [sum c1 c2] returns the over-approximation of the sum [c1 + c2]. *)
    let sum (c1 : t) (c2 : t) : t =
      match c1, c2 with
      | Top, _ | _, Top -> Top
      | Bottom, c | c, Bottom -> c
      | Congruence {stride = s1; offset = (v1, o1)},
        Congruence {stride = s2; offset = (v2, o2)} ->
          Congruence {stride = gcd s1 s2; offset = add_relative_offsets v1 v2, Int32.(+) o1 o2}
  end

  (** The integer interval domain, representing bounded or unbounded intervals using extended integers.

      Supports standard abstract operations such as join, meet, and widening.
  *)
  module Interval = struct
    type interval = {
      lower_bound : ExtendedInt.t;
      upper_bound : ExtendedInt.t
    }

    type t =
      | Top (* ℤ *)
      | Bottom (* ∅ *)
      | Interval of interval

    let equal (i1 : t) (i2 : t) : bool =
      match i1, i2 with 
      | Top, Top | Bottom, Bottom -> true  
      | Top, Interval {lower_bound = NegInfinity; upper_bound = Infinity}
      | Interval {lower_bound = NegInfinity; upper_bound = Infinity}, Top -> true
      | Bottom, Interval {lower_bound = l; upper_bound = u} 
        when ExtendedInt.less_than u l -> true
      | Interval {lower_bound = l; upper_bound = u}, Bottom 
        when ExtendedInt.less_than u l -> true
      | Interval {lower_bound = l1; upper_bound = u1},
        Interval {lower_bound = l2; upper_bound = u2} 
        when ExtendedInt.less_than u1 l1 && ExtendedInt.less_than u2 l2 -> true
      | Interval i1, Interval i2 -> 
        (ExtendedInt.equal i1.lower_bound i2.lower_bound 
        && ExtendedInt.equal i1.upper_bound i2.upper_bound)
      | _ -> false

    let to_string (i : t) : string =
      match i with 
      | Top -> "ℤ"
      | Interval _ when equal Top i -> "ℤ"
      | Bottom -> "∅"
      | Interval _ when equal Bottom i -> "∅"
      | Interval i ->
        begin match i.lower_bound, i.upper_bound with 
          | Int l, Infinity -> 
            "[" ^ Int32.to_string l ^ "," ^ ExtendedInt.to_string Infinity ^ "["
          | NegInfinity, Int u -> 
            "]" ^ ExtendedInt.to_string NegInfinity ^ "," ^ Int32.to_string u ^ "]"
          (* | Int l, Int u when l <= u -> "[" ^ string_of_int l ^ "," ^ string_of_int u ^ "]" *)
          | Int l, Int u when Int32.compare l u <= 0 -> "[" ^ Int32.to_string l ^ "," ^ Int32.to_string u ^ "]"
          | _ -> assert false
        end

    (** [join i1 i2] returns the least upper bound (union) of [i1] and [i2]. *)
    let join (i1 : t) (i2 : t) : t =
      let join =
        match i1, i2 with 
        | Top, _ | _, Top -> Top
        | Bottom, i | i, Bottom -> i
        | i1, i2 when equal Bottom i1 -> i2
        | i1, i2 when equal Bottom i2 -> i1
        | Interval i1, Interval i2 -> Interval {
          lower_bound = ExtendedInt.minimum i1.lower_bound i2.lower_bound;
          upper_bound = ExtendedInt.maximum i1.upper_bound i2.upper_bound
        }
      in
      if equal join Top then Top else if equal Bottom join then Bottom else join

    (** [meet i1 i2] returns the greatest lower bound (intersection) of [i1] and [i2]. *)
    let meet (i1 : t) (i2 : t) : t =
      let meet =
        match i1, i2 with 
        | Bottom, _ | _, Bottom -> Bottom 
        | Top, i | i, Top -> i
        | i1, _ when equal Bottom i1 -> Bottom
        | _, i2 when equal Bottom i2 -> Bottom
        | Interval i1, Interval i2 ->
          let lower = ExtendedInt.maximum i1.lower_bound i2.lower_bound in
          let upper = ExtendedInt.minimum i1.upper_bound i2.upper_bound in 
          Interval {lower_bound = lower; upper_bound = upper}
      in if equal meet Top then Top else if equal Bottom meet then Bottom else meet

    let widen (i1 : t) (i2 : t) : t =
      match i1, i2 with 
      | Top, _ | _, Top -> Top
      | Bottom, i | i, Bottom -> i
      | i1, _ when equal Bottom i1 -> i2
      | _, i2 when equal Bottom i2 -> i1
      | Interval {lower_bound = l1; upper_bound = u1}, 
        Interval {lower_bound = l2; upper_bound = u2} ->
          let lower = if ExtendedInt.less_than l2 l1 then ExtendedInt.NegInfinity else l1 in
          let upper = if ExtendedInt.less_than u1 u2 then ExtendedInt.Infinity else u1 in
          Interval {lower_bound = lower; upper_bound = upper}

    (** [sum i1 i2] returns the over-approximation of the sum [i1 + i2]. *)
    let sum (i1 : t) (i2 : t) : t =
      match i1, i2 with
      | Top, _ | _, Top -> Top
      | Bottom, i | i, Bottom -> i
      | Interval {lower_bound = l1; upper_bound = u1}, 
        Interval {lower_bound = l2; upper_bound = u2} ->
          let new_lower = ExtendedInt.plus l1 l2 in
          let new_upper = ExtendedInt.plus u1 u2 in
          Interval {lower_bound = new_lower; upper_bound = new_upper}
  end

  (** A reduced interval congruence value: [stride * {lower_bound..upper_bound} + offset]. *)
  type ric = {
    stride : int32;
    lower_bound : ExtendedInt.t;
    upper_bound : ExtendedInt.t;
    offset : string * int32;
  }
  [@@deriving sexp, compare, equal]

  (** An abstract value in the RIC domain. *)
  type t =
    | Top
    | Bottom
    | RIC of ric
  [@@deriving sexp, compare, equal]

  (** [reduce r] normalizes the RIC [r] by shifting the offset and resetting the lower bound to 0. *)
  let rec reduce (r : t) : t =
    match r with 
    | Top -> Top 
    | Bottom -> Bottom 
    | RIC {stride = s; lower_bound = Int l; upper_bound = u; offset = ("", o)} when Int32.(s*l+o < 0b11000000000000000000000000000000l) ->
      reduce (RIC {stride = s; lower_bound = NegInfinity; upper_bound = u; offset = ("", o)})
    | RIC {stride = s; lower_bound = l; upper_bound = Int u; offset = ("", o)} when Int32.(s*u+o > 0b01100000000000000000000000000000l) ->
      reduce (RIC {stride = s; lower_bound = l; upper_bound = Infinity; offset = ("", o)})
    | RIC {stride = s; lower_bound = l; upper_bound = u; offset = o} ->
      if ExtendedInt.less_than u l then Bottom else 
      if (Int32.equal s 1l && ExtendedInt.equal NegInfinity l && ExtendedInt.equal Infinity u) then 
        Top 
      else if Int32.(s = 0l) then 
        RIC {stride = 0l; lower_bound = Int 0l; upper_bound = Int 0l; offset = o} 
      else
        let new_offset =
          match o, l with 
          | (var, o), Int l -> (var, Int32.(o + s * l))
          | (var, o), NegInfinity -> (var, Int32.(%) o s)
          | _ -> assert false
        in
        let new_lower =
          match l with 
          | NegInfinity -> ExtendedInt.NegInfinity
          | Int _ -> Int 0l
          | Infinity -> assert false 
        in 
        let new_upper =
          match o, l, u with
          | _, _, Infinity -> ExtendedInt.Infinity
          | _, Int l, Int u -> Int (Int32.(u - l))
          | (_, o), NegInfinity, u -> 
            ExtendedInt.plus u (ExtendedInt.Int Int32.(o / s - if (o % s <> 0l) && o < 0l then 1l else 0l))
          | _ -> assert false
        in
        let new_stride = 
          if ExtendedInt.equal new_lower new_upper then 
            0l (* Singleton *)
          else 
            s 
        in
        RIC {stride = new_stride; lower_bound = new_lower; upper_bound = new_upper; offset = new_offset}

  (** [ric (s, l, u, o)] constructs and reduces a RIC from stride [s], bounds [l] and [u], and offset [o]. *)
  let ric (r : int32 * ExtendedInt.t * ExtendedInt.t * (string * int32)) : t =
    reduce (
      match r with 
      | s, l, u, o -> RIC {stride = s; lower_bound = l; upper_bound = u; offset = o}
    )

  let negative_integers = ric (1l, NegInfinity, Int (-1l), ("", 0l))

  let positive_integers = ric (1l, Int 0l, Infinity, ("", 0l))

  (** [relative_ric var] returns a RIC with zero stride and offset set to the variable [var]. *)
  let relative_ric (var : string) : t =
    ric (0l, Int 0l, Int 0l, (var, 0l))

  let spans_neg_inf_to_pos_inf (r : t) : bool =
    match r with
    | Top
    | RIC {lower_bound = NegInfinity; upper_bound = Infinity; _} -> true
    | _ -> false

  (** [is_singleton r] returns [true] if [r] contains exactly one value. *)
  let is_singleton (r : t) : bool =
    let r = reduce r in
    match r with
    | RIC {stride = 0l; _} -> true
    | _ -> false

  (** [extract_relative_offset r] returns the variable name used in the offset of [r]. *)
  let extract_relative_offset (r : t) : string =
    match r with
    | Bottom | Top -> ""
    | RIC {offset = (relative, _); _} -> relative

  let is_stack (r : t) : bool =
    match r with
    | RIC {offset = (relative, _); _} -> String.equal relative "g0"
    | _ -> false
  
  (** [set_relative_offset r offset] sets the variable name in the offset of [r] to [offset]. *)
  let set_relative_offset (r : t) (offset : string) : t =
    match r with
    | RIC {stride = s; lower_bound = l; upper_bound = u; offset = ("", o)} ->
      ric (s, l, u, (offset, o))
    | _ -> r

  (** [remove_relative_offset r] returns [r] with its relative variable removed (set to ""). *)
  let remove_relative_offset (r : t) : t =
    match r with
    | Top -> Top
    | Bottom -> Bottom
    | RIC {stride = s; lower_bound = l; upper_bound = u; offset = (_, o)} -> 
      ric (s, l, u, ("", o))

  (** [of_congruence_and_interval c i] constructs a RIC value from congruence [c] and interval [i],
      and reduces the result for consistency. *)
  let of_congruence_and_interval (c : Congruence.t) (i : Interval.t) : t =
    match c with
    | Top ->
      begin match i with 
        | Top -> Top
        | Bottom -> Bottom
        | Interval {lower_bound = l; upper_bound = u} ->
          ric (1l, l, u, ("", 0l))
      end
    | c when Congruence.equal Top c ->
      begin match i with 
        | Top -> Top
        | Bottom -> Bottom
        | Interval {lower_bound = l; upper_bound = u} ->
          ric (1l, l, u, ("", 0l))
      end
    | Bottom -> Bottom
    | Congruence {stride = 0l; offset = o} ->
      begin match i with
        | Bottom -> Bottom
        | _ -> ric (0l, Int 0l, Int 0l, o)
      end
    | Congruence {stride = s; offset = (var, o)} ->
      begin match i with 
        | Top -> reduce (ric (s, ExtendedInt.NegInfinity, ExtendedInt.Infinity, (var, o)))
        | Bottom -> Bottom
        | Interval {lower_bound = l; upper_bound = u} ->
          let lower = 
            ExtendedInt.divide_ceiling (ExtendedInt.minus l (ExtendedInt.Int o)) (ExtendedInt.Int s) in
          let upper = 
            ExtendedInt.divide_floor (ExtendedInt.minus u (ExtendedInt.Int o)) (ExtendedInt.Int s) in
          ric (s, lower, upper, (var, o))
      end

  (** [to_congruence_and_interval r] decomposes [r] into its congruence and interval components. *)
  let to_congruence_and_interval (r : t) : Congruence.t * Interval.t =
    (* let r = reduce r in *)
    match r with 
    | Top -> Top, Top
    | Bottom -> Bottom, Bottom 
    | RIC {stride = s; offset = (var, o); lower_bound = l; upper_bound = u} ->
      Congruence {stride = s; offset = (var, if Int32.equal s 0l then o else Int32.(%) o s)},
      Interval {lower_bound = ExtendedInt.plus (ExtendedInt.Int o) (ExtendedInt.times (ExtendedInt.Int s) l); 
                upper_bound = ExtendedInt.plus (ExtendedInt.Int o) (ExtendedInt.times (ExtendedInt.Int s) u)}
      
  (** [equal r1 r2] returns [true] if [r1] and [r2] represent the same set. *)
  let equal (ric1 : t) (ric2 : t) : bool =
    let ric1, ric2 = reduce ric1, reduce ric2 in
    match ric1, ric2 with 
    | Top, Top | Bottom, Bottom -> true
    | RIC {stride = s1; lower_bound = l1; upper_bound = u1; offset = (v1, o1)},  
      RIC {stride = s2; lower_bound = l2; upper_bound = u2; offset = (v2, o2)} ->
        Int32.equal s1 s2 
        && ExtendedInt.equal l1 l2 
        && ExtendedInt.equal u1 u2 
        && String.equal v1 v2 && Int32.equal o1 o2
    | _ -> false

  (** [comparable_offsets r1 r2] returns true if two RICs can be compared, i.e., they have matching base variables. *)
  let comparable_offsets (ric1 : t) (ric2 : t) : bool =
    match ric1, ric2 with
    | RIC {offset = (v1, _); _},
      RIC {offset = (v2, _); _} -> String.equal v1 v2 
    | _ -> true

  (** [to_string r] returns a human-readable string representation of RIC [r]. *)
  let to_string (r : t) : string =
    let r = reduce r in
    match r with
    | Top -> "⊤"
    | Bottom -> "⊥"
    | RIC {stride = s; lower_bound = l; upper_bound = u; offset = o} ->
      let offset = 
        match o with 
        | (var, i) -> 
          if Int32.equal i 0l then
            (if not (String.equal var "") then "+" else "") ^ var
          else if String.equal var "" then
            (if Int32.compare i 0l > 0 then "+" else "") ^ Int32.to_string (Int32.abs i)
          else
            "+(" ^ var ^ ((if Int32.compare i 0l > 0 then "+" else "") ^ Int32.to_string i) ^ ")"
      in
      let interval = Interval.to_string (Interval.Interval {lower_bound = l; upper_bound = u}) in
      let stride = if Int32.equal s 1l then "" else Int32.to_string s in
      match stride, interval, offset with 
      | "0", _, "" -> "0"
      | "0", _, o -> if String.equal "+" (String.sub o ~pos:0 ~len:1) then String.sub o ~pos:1 ~len:(String.length o - 1) else o
      | "", "]-∞,∞[", _ | "", "ℤ", _ -> "⊤"
      | _ -> stride ^ interval ^ offset

  (** [meet r1 r2] returns the intersection of [r1] and [r2]. *)
  let meet (ric1 : t) (ric2 : t) : t =
    if comparable_offsets ric1 ric2 then
      let relative_offset = 
        match ric1, ric2 with
        | RIC _, _ -> extract_relative_offset ric1
        | _, RIC _ -> extract_relative_offset ric2
        | _ -> "" in
      let ric1 = remove_relative_offset ric1 in
      let ric2 = remove_relative_offset ric2 in
      let (c1, i1) = to_congruence_and_interval ric1 in
      let (c2, i2) = to_congruence_and_interval ric2 in
      let c = Congruence.meet c1 c2 in
      let i = Interval.meet i1 i2 in
      let m = of_congruence_and_interval c i in
      match m with
      | Top | Bottom -> m
      | _ -> set_relative_offset m relative_offset
    else
      Bottom

  let negative_part (r : t) : t =
    meet r negative_integers

  let positive_part (r : t) : t =
    meet r positive_integers
  
  (** [disjoint r1 r2] returns [true] if [r1] and [r2] have no values in common. *)
  let disjoint (ric1 : t) (ric2 : t) : bool =
    equal Bottom (meet ric1 ric2)

  (** [join r1 r2] returns the union (over-approximation) of [r1] and [r2]. *)
  let join (ric1 : t) (ric2 : t) : t =
    if comparable_offsets ric1 ric2 then
      let relative_offset = 
        match ric1, ric2 with
        | RIC _, _ -> extract_relative_offset ric1
        | _, RIC _ -> extract_relative_offset ric2
        | _ -> "" in
      let ric1 = remove_relative_offset ric1 in
      let ric2 = remove_relative_offset ric2 in
      let (c1, i1) = to_congruence_and_interval ric1 in
      let (c2, i2) = to_congruence_and_interval ric2 in
      let c = Congruence.join c1 c2 in
      let i = Interval.join i1 i2 in 
      let j = of_congruence_and_interval c i in
      match j with
      | Top | Bottom -> j
      | _ -> set_relative_offset j relative_offset
    else
      Top

  (** [is_subset r1 ~of_:r2] returns [true] if [r1] is a subset of [r2]. *)
  let is_subset (ric1 : t) ~(of_ : t) : bool =
    let m = meet ric1 of_ in
    let j = join ric1 of_ in 
    match m, j with 
    | m, j when equal m ric1 && equal j of_ -> true 
    | m, _ when equal m ric1 -> assert false
    | _, j when equal j of_ -> assert false
    | _ -> false

  (** [subsumes r1 r2] returns [true] if [r1] over-approximates [r2]. *)
  let subsumes (ric1 : t) (ric2 : t) : bool = 
    is_subset ric2 ~of_:ric1

  (** [complement r] returns a list of RICs representing the complement of [r]. *)
  let complement (r : t) : t list =
    match reduce r with
    | Top -> [Bottom]
    | Bottom -> [Top]
    | RIC {stride = s; lower_bound = l; upper_bound = u; offset = (v, o)} ->
      let inferior_RIC = 
        if ExtendedInt.equal l NegInfinity then []
        else [ric (1l, NegInfinity, ExtendedInt.minus l (Int 1l), (v, o))]
      in
      let superior_RIC =
        if ExtendedInt.equal u Infinity then []
        else [ric (1l,
                   ExtendedInt.plus (Int (Int32.(+) o (if Int32.equal s 0l then 1l else s))) (ExtendedInt.times (Int s) u),
                   Infinity,
                   (v, 0l))]
      in
      let overlapping_complement =
        if Int32.equal s 0l then []
        else List.init (Int32.to_int_exn s - 1) ~f:(fun i -> ric (s, l, u, (v, Int32.(+) (Int32.(+) o (Int32.of_int_exn i)) 1l)))
      in
      inferior_RIC @ overlapping_complement @ superior_RIC

  (** [add_offset r c] returns [r] with its offset increased by [c]. *)
  let add_offset (r : t) (c : int32) : t =
    match r with
    | Top -> Top 
    | Bottom -> Bottom
    | RIC {stride = s; lower_bound = l; upper_bound = u; offset = (v,o)} -> 
      ric (s, l, u, (v, Int32.(o + c)))

  (** [remove_lower_bound r] returns [r] with its lower bound removed (set to -∞). *)
  let remove_lower_bound (r : t) : t =
    let r = reduce r in
    match r with 
    | Top -> Top 
    | Bottom -> Bottom 
    | RIC {stride = s; lower_bound = _; upper_bound = u; offset = o} -> 
      ric (1l, NegInfinity, ExtendedInt.times (Int s) u, o)
        (* if s = 0 then 
          ric (1, NegInfinity, u, o) 
        else
          ric (s, NegInfinity, u, o) *)

  (** [remove_upper_bound r] returns [r] with its upper bound removed (set to ∞). *)
  let remove_upper_bound (r : t) : t =
    let r = reduce r in
    match r with 
    | Top -> Top 
    | Bottom -> Bottom 
    | RIC {stride = s; lower_bound = l; upper_bound = _; offset = o} -> 
      ric (1l, ExtendedInt.times (Int s) l, Infinity, o)
      (* reduce (
        if s = 0 then
          ric (1, l, Infinity, o)
        else
          ric (s, l, Infinity, o)
      ) *)

  (** [widen r ~relative_to] returns the widening of [r] with respect to [relative_to]. *)
  let widen (ric1 : t) ~(relative_to : t) : t =
    if comparable_offsets ric1 relative_to then
      let relative_offset = 
        match ric1, relative_to with
        | RIC _, _ -> extract_relative_offset ric1
        | _, RIC _ -> extract_relative_offset relative_to
        | _ -> "" in
      let ric1 = remove_relative_offset ric1 in
      let relative_to = remove_relative_offset relative_to in
      let (c1, i1) = to_congruence_and_interval ric1 in
      let (c2, i2) = to_congruence_and_interval relative_to in
      let widened_c = Congruence.widen c1 c2 in
      let widened_i = Interval.widen i1 i2 in 
      let w = of_congruence_and_interval widened_c widened_i in
       match w with
      | Top | Bottom -> w
      | _ -> set_relative_offset w relative_offset
    else
      Top

  (** [partially_accessed ~by ~size] returns RICs that may be partially accessed by a memory access of [size] bytes. *)
  let partially_accessed ~(by : t) ~(size : int32) : t list =
    let r = reduce by in
    if equal Top r then 
      [Top]
    else if spans_neg_inf_to_pos_inf r then (* TODO: Test this case! *)
      match r with
      | RIC {stride = s; _} when Int32.(s <= size) -> [Top]
      | RIC _ -> 
        List.fold (List.init (2 * (Int32.to_int_exn size) - 1) ~f:(fun i -> Int32.neg (Int32.(+) size (Int32.(+) 1l (Int32.of_int_exn i)))))
          ~init:[]
          ~f:(fun acc i -> (add_offset r i) :: acc)
      | _ -> assert false
    else
      let rec aux (i : int32) (acc : t list) : t list =
        match i, r with
        | _, Top -> [Top] 
        | _, Bottom -> []
        | i, RIC {stride = s; lower_bound = Int _; _} when Int32.(i = (if Int32.(s = 0l) then size else Int32.min size (Int32.(s - 3l)))) -> acc
        | i, RIC {stride = s; lower_bound = NegInfinity; _} when Int32.(i + 3l + size - 1l = max (-4l) (size - s - 1l)) -> acc
        | i, RIC {stride = s; lower_bound = Int l; upper_bound = Int u; offset = (v, o)} ->
          aux Int32.(i + 1l) (ric (s, Int l, (if Int32.(s <> 0l) then Int Int32.(l + ((u - l + 1l) * s - i + size - 1l) / s - 1l) else Int 0l), (v, Int32.(+) o i)) :: acc)
        | i, RIC {upper_bound = Infinity; _} ->
          aux Int32.(i + 1l) (add_offset r i :: acc)
        | i, RIC {lower_bound = NegInfinity; _} -> 
          (* print_endline (Int32.to_string i); *)
          let offset = Int32.(i + 3l + size - 1l) in
          (* print_endline ("new: " ^ Int32.to_string offset); *)
          aux Int32.(i - 1l) (add_offset r offset :: acc)
        | _ -> assert false
      in
      let accessed = aux (-3l) []
      in
      (* if Int32.(size <> 4l) || (not (is_singleton r)) && (match r with | RIC {stride = s; _} when Int32.(s < 4l) -> true | _ -> false) then *)
      if Int32.(size <> 4l) || (not (is_singleton r)) && (match r with | RIC {stride = s; _} -> Int32.(s < 4l) | _ -> false) then
        List.dedup_and_sort ~compare:compare accessed
      else
        List.dedup_and_sort ~compare:compare (List.filter ~f:(fun x -> not (equal x r)) accessed)

  
  (** The result of a memory access: fully and partially accessed RICs. *)
  type accessed = {
    fully : t;
    partially : t list
  }
  
  (** [accessed ~value_set ~size] returns the fully and partially accessed RICs for a memory access 
      of [size] bytes from [value_set]. *)
  let accessed ~(value_set : t) ~(size : int32) : accessed =
    let stride = match value_set with | RIC {stride = s; _} -> s | Top -> 1l | Bottom -> 0l in
    let f = if Int32.(size = 4l) && (Int32.(stride >= 4l) || is_singleton value_set) then value_set else Bottom in
    let p = partially_accessed ~by:value_set ~size:size in
    {fully = f; partially = p}

  (** [plus r1 r2] returns the over-approximation of the sum [r1] + [r2]. *)
  let plus (ric1 : t) (ric2 : t) : t =
    match ric1, ric2 with
    | Top, _ | _, Top -> Top
    | Bottom, Bottom -> Bottom
    | Bottom, _ ->
      Log.warn ("Adding Bottom state to an RIC - result will return the RIC unchanged: " ^ to_string ric2);
      ric2
    | _, Bottom -> 
      Log.warn ("Adding Bottom state to an RIC - result will return the RIC unchanged: " ^ to_string ric1);
      ric1
    | _ ->
      let offset1 = extract_relative_offset ric1 in
      let offset2 = extract_relative_offset ric2 in
      let relative_offset = add_relative_offsets offset1 offset2 in
      let ric1 = remove_relative_offset ric1 in
      let ric2 = remove_relative_offset ric2 in
      let c1, i1 = to_congruence_and_interval ric1 and
      c2, i2 = to_congruence_and_interval ric2 in
      let new_congruence = Congruence.sum c1 c2 and
      new_interval = Interval.sum i1 i2 in
      let result = of_congruence_and_interval new_congruence new_interval in
      set_relative_offset result relative_offset

  (** [negative r] returns the RIC representing the negation [-r]. *)
  let negative (r : t) : t =
    match r with
    | Top -> Top
    | Bottom -> Bottom
    | RIC {stride = s; lower_bound = l; upper_bound = u; offset = ("", o)} ->
      ric (s, ExtendedInt.times (Int (-1l)) u, ExtendedInt.times (Int (-1l)) l, ("", Int32.(- o)))
    | RIC {stride = s; lower_bound = l; upper_bound = u; offset = (v, o)} ->
      if String.is_prefix v ~prefix:"neg" then
        ric (s, ExtendedInt.times (Int (-1l)) u, ExtendedInt.times (Int (-1l)) l, (String.drop_prefix v 3, Int32.(- o)))
      else
        ric (s, ExtendedInt.times (Int (-1l)) u, ExtendedInt.times (Int (-1l)) l, ("neg" ^ v, Int32.(- o)))

  (** [minus r1 r2] returns the over-approximation of the difference [r1] - [r2]. *)
  let minus (ric1 : t) (ric2 : t) : t =
    if not (comparable_offsets ric1 ric2) then
      Top
    else
      let ric1 = remove_relative_offset ric1 in
      let ric2 = remove_relative_offset ric2 in
      let negative_ric2 = negative ric2 in
      plus ric1 negative_ric2

  (** [remove ~this ~from] returns the parts of [from] that are not in [this]. *)
  let remove ~(this : t) ~(from : t) : t list =
    let disjoint_memory = !Value_set_options.disjoint_memory_spaces in
    if disjoint_memory && not (comparable_offsets this from) then
      [from]
    else if comparable_offsets this from then
      (let this_complement = complement this in
      List.filter ~f:(fun r -> not (equal r Bottom)) (List.map ~f:(meet from) this_complement))
    else
      []

  let update_relative_offset ~(ric_ : t) ~(actual_values : t String.Map.t) : t =
    let offset = extract_relative_offset ric_ in
    match Map.find actual_values offset with
    | None -> ric_
    | Some r ->
      let ric_ = remove_relative_offset ric_ in
      plus r ric_


  let is_power_of_two (x : int32) : bool =
    Int32.(x > 0l && (x land (x - 1l)) = 0l)


  let common_msb (x : int32) (y : int32) : Bitfield.t * int = 
    let bf = Bitfield.of_set (Set.of_list (module Int32) [x; y]) in
    let rec aux (bf : Bitfield.t) (n : int) : Bitfield.t * int =
      if Bitfield.is_singleton bf then
        bf, n
      else
        let bf = Bitfield.shift_right_unsigned bf Bitfield.one in
        aux bf (n + 1)
    in
    aux bf 0

  let rec nth_bit (x : int32) (n : int32) : int32 =
    if Int32.(n = 0l) then
      Int32.(x % 2l)
    else
      nth_bit Int32.(shift_left x 1) Int32.(n - 1l)


  let to_bitfield (r : t) : Bitfield.t =
    assert (equal (negative_part r) Bottom || equal (positive_part r) Bottom); (* We assume all values are of the same sign *)
    let r = reduce r in
    match r with
    | Bottom -> Bottom
    | Top -> Top
    | RIC {lower_bound = NegInfinity; upper_bound = Infinity; _} -> assert false (* We need to avoid this situation at all cost *)
    | RIC {lower_bound = NegInfinity; stride = s; offset = ("", o); _} -> 
      if is_power_of_two s then
        let ones = Int32.(-s lor o)
        and zeros = Int32.(shift_right_logical (shift_left (-s lor (lnot o)) 1) 1) in (* TODO: instead of lor, define addition of bitfields*)
        Bitfield.Bit {zeros; ones}
      else
        Top
    | RIC {upper_bound = Infinity; stride = s; offset = ("", o); _} -> 
      if is_power_of_two s then
        let ones = Int32.(shift_right_logical (shift_left (-s lor o) 1) 1)
        and zeros = Int32.(-s lor (lnot o)) in
        Bitfield.Bit {zeros; ones}
      else
        Top
    | RIC {offset = (o, _); _} when not (String.equal "" o) -> Bitfield.Top
    | RIC {stride = s; lower_bound = Int l; upper_bound = Int u; offset = (_, o)} -> 
      (* let integers = 
        Set.of_list
          (module Int32)
          (List.init  
            (Int32.to_int_exn (Int32.(u - l + 1l))) 
            ~f:(fun n -> Int32.(s * (Int32.of_int_exn n) + o))) in
        Bitfield.of_set integers *)
      let number_of_steps = Int32.to_int_exn Int32.(u - l) in
      let lowest_value = Int32.(s * l + o) in
      let bit_flips = Maths.Binary.bitFlips lowest_value s in
      let bit_flips = List.map bit_flips ~f:(fun n -> n <> 0 && n <= number_of_steps) in
      let powers_of_two = List.init 32 ~f:(fun n -> Int32.shift_left 1l n) in
      let ones = 
        List.fold2_exn 
          bit_flips 
          powers_of_two 
          ~init:0l 
          ~f:(fun acc flip bit -> 
            if flip then 
              Int32.(acc + bit) 
            else 
              let initial_bit = Int32.(bit land lowest_value) in
              Int32.(acc + initial_bit))
      and zeros =
        List.fold2_exn 
          bit_flips 
          powers_of_two 
          ~init:0l 
          ~f:(fun acc flip bit -> 
            if flip then 
              Int32.(acc + bit) 
            else 
              let initial_bit = Int32.(bit land (lnot lowest_value)) in
              Int32.(acc + initial_bit))
      in
      Bitfield.Bit {ones; zeros}
    | _ -> assert false

  
  let of_bitfield (bf : Bitfield.t) : t =
    match bf with
    | Top -> Top
    | Bottom -> Bottom
    | bf ->
      let offset = Bitfield.constant_value bf in
      let power_of_two, lower_bound, upper_bound = Bitfield.variable_values bf in
      ric(Int32.of_int_exn (Int.pow 2 power_of_two), Int lower_bound, Int upper_bound, ("", offset))

  let bitwise_and (ric1 : t) (ric2 : t) : t =
    let pos1 = meet ric1 positive_integers
    and neg1 = meet ric1 negative_integers
    and pos2 = meet ric2 positive_integers
    and neg2 = meet ric2 negative_integers in
    let bf_neg1 = to_bitfield neg1
    and bf_pos1 = to_bitfield pos1
    and bf_pos2 = to_bitfield pos2
    and bf_neg2 = to_bitfield neg2 in
    (* print_endline ("neg1: " ^ Bitfield.to_string bf_neg1);
    print_endline ("pos1: " ^ Bitfield.to_string bf_pos1);
    print_endline ("neg2: " ^ Bitfield.to_string bf_neg2);
    print_endline ("pos1: " ^ Bitfield.to_string bf_pos1); *)
    let bf_pp = Bitfield.and_ bf_pos1 bf_pos2
    and bf_pn = Bitfield.and_ bf_pos1 bf_neg2
    and bf_nn = Bitfield.and_ bf_neg1 bf_neg2
    and bf_np = Bitfield.and_ bf_neg1 bf_pos2 in
    let pos_pos = of_bitfield bf_pp
    and pos_neg = of_bitfield bf_pn
    and neg_neg = of_bitfield bf_nn
    and neg_pos = of_bitfield bf_np in
    join (join pos_pos pos_neg) (join neg_neg neg_pos)

  let bitwise_or (ric1 : t) (ric2 : t) : t =
    let pos1 = meet ric1 positive_integers
    and neg1 = meet ric1 negative_integers
    and pos2 = meet ric2 positive_integers
    and neg2 = meet ric2 negative_integers in
    let bf_neg1 = to_bitfield neg1
    and bf_pos1 = to_bitfield pos1
    and bf_pos2 = to_bitfield pos2
    and bf_neg2 = to_bitfield neg2 in
    let bf_pp = Bitfield.or_ bf_pos1 bf_pos2
    and bf_pn = Bitfield.or_ bf_pos1 bf_neg2
    and bf_nn = Bitfield.or_ bf_neg1 bf_neg2
    and bf_np = Bitfield.or_ bf_neg1 bf_pos2 in
    let pos_pos = of_bitfield bf_pp
    and pos_neg = of_bitfield bf_pn
    and neg_neg = of_bitfield bf_nn
    and neg_pos = of_bitfield bf_np in
    join (join pos_pos pos_neg) (join neg_neg neg_pos)

  let bitwise_xor (ric1 : t) (ric2 : t) : t =
    let pos1 = meet ric1 positive_integers
    and neg1 = meet ric1 negative_integers
    and pos2 = meet ric2 positive_integers
    and neg2 = meet ric2 negative_integers in
    let bf_neg1 = to_bitfield neg1
    and bf_pos1 = to_bitfield pos1
    and bf_pos2 = to_bitfield pos2
    and bf_neg2 = to_bitfield neg2 in
    let bf_pp = Bitfield.xor_ bf_pos1 bf_pos2
    and bf_pn = Bitfield.xor_ bf_pos1 bf_neg2
    and bf_nn = Bitfield.xor_ bf_neg1 bf_neg2
    and bf_np = Bitfield.xor_ bf_neg1 bf_pos2 in
    let pos_pos = of_bitfield bf_pp
    and pos_neg = of_bitfield bf_pn
    and neg_neg = of_bitfield bf_nn
    and neg_pos = of_bitfield bf_np in
    join (join pos_pos pos_neg) (join neg_neg neg_pos)

  let not_ (r : t) : t =
    let pos = meet r positive_integers
    and neg = meet r negative_integers in
    let bf_pos = to_bitfield pos 
    and bf_neg = to_bitfield neg in
    let not_pos = Bitfield.not_ bf_pos
    and not_neg = Bitfield.not_ bf_neg in
    join (of_bitfield not_pos) (of_bitfield not_neg)

  let shift_left (ric1 : t) (ric2 : t) : t =
    let pos = meet ric1 positive_integers
    and neg = meet ric1 negative_integers in
    let bf1_pos = to_bitfield pos
    and bf1_neg = to_bitfield neg
    and bf2 = to_bitfield (meet ric2 positive_integers) in
    let bf_pos = Bitfield.shift_left bf1_pos bf2
    and bf_neg = Bitfield.shift_left bf1_neg bf2 in
    join (of_bitfield bf_pos) (of_bitfield bf_neg)

  let shift_right_u (ric1 : t) (ric2 : t) : t =
    let pos = meet ric1 positive_integers
    and neg = meet ric1 negative_integers in
    let bf1_pos = to_bitfield pos
    and bf1_neg = to_bitfield neg
    and bf2 = to_bitfield (meet ric2 positive_integers) in
    (* print_endline ("pos: " ^ Bitfield.to_string bf1_pos);
    print_endline ("neg: " ^ Bitfield.to_string bf1_neg); *)
    let bf_pos = Bitfield.shift_right_unsigned bf1_pos bf2
    and bf_neg = Bitfield.shift_right_unsigned bf1_neg bf2 in
    (* print_endline ("shr_pos: " ^ Bitfield.to_string bf_pos);
    print_endline ("shr_neg: " ^ Bitfield.to_string bf_neg); *)
    join (of_bitfield bf_pos) (of_bitfield bf_neg)

  let shift_right_s (ric1 : t) (ric2 : t) : t =
    let pos = meet ric1 positive_integers
    and neg = meet ric1 negative_integers in
    let bf1_pos = to_bitfield pos
    and bf1_neg = to_bitfield neg
    and bf2 = to_bitfield (meet ric2 positive_integers) in
    let bf_pos = Bitfield.shift_right_signed bf1_pos bf2
    and bf_neg = Bitfield.shift_right_signed bf1_neg bf2 in
    join (of_bitfield bf_pos) (of_bitfield bf_neg)

  
end


module RICSet = struct
  include Set
  module S = struct
    include Set.Make(RIC)
    let to_string (r : t) : string =
      String.concat ~sep:"; " (List.map ~f:RIC.to_string (Set.to_list r))


    (* TODO: RICSET.union that checks for included RICs... *)
  end
  include Set
  include S
  include Test.Helpers(S)
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

let%test_module "RIC tests" = (module struct
  open RIC

  let%test "RIC_tests" =
    print_endline "\n_______ ____________________________ _______\n        Reduced interval congruences        \n------- ---------------------------- -------\n"; true

  (*
      cccccccccccccccc   ooooooooooo   nnnn  nnnnnnnn       ggggggggg   gggggrrrrr   rrrrrrrrr   
    cc:::::::::::::::c oo:::::::::::oo n:::nn::::::::nn    g:::::::::ggg::::gr::::rrr:::::::::r  
  c:::::::::::::::::co:::::::::::::::on::::::::::::::nn  g:::::::::::::::::gr:::::::::::::::::r 
  c:::::::cccccc:::::co:::::ooooo:::::onn:::::::::::::::ng::::::ggggg::::::ggrr::::::rrrrr::::::r
  c::::::c     ccccccco::::o     o::::o  n:::::nnnn:::::ng:::::g     g:::::g  r:::::r     r:::::r
  c:::::c             o::::o     o::::o  n::::n    n::::ng:::::g     g:::::g  r:::::r     rrrrrrr
  c:::::c             o::::o     o::::o  n::::n    n::::ng:::::g     g:::::g  r:::::r            
  c::::::c     ccccccco::::o     o::::o  n::::n    n::::ng::::::g    g:::::g  r:::::r            
  c:::::::cccccc:::::co:::::ooooo:::::o  n::::n    n::::ng:::::::ggggg:::::g  r:::::r            
  c:::::::::::::::::co:::::::::::::::o   n::::n    n::::n g::::::::::::::::g  r:::::r            
    cc:::::::::::::::c oo:::::::::::oo   n::::n    n::::n  gg::::::::::::::g  r:::::r            
      cccccccccccccccc   ooooooooooo     nnnnnn    nnnnnn    gggggggg::::::g  rrrrrrr            
                                                                    g:::::g                     
                                                        gggggg      g:::::g                     
                                                        g:::::gg   gg:::::g                     
                                                          g::::::ggg:::::::g                     
                                                          gg:::::::::::::g                      
                                                            ggg::::::ggg                        
                                                                gggggg                      
  *)
  let%test_module "Congruence tests" = (module struct

    let%test "Congruence_tests" =
      print_endline "------- CONGRUENCE MODULE -------\n"; true

    (* Top should be equal to Congruence with stride 1 and offset 0. *)
    let%test "Congruence: top equals stride1 offset0" =
      Congruence.equal Congruence.Top (Congruence.Congruence {stride = 1l; offset = ("", 0l)})

    (* Top should be equal to Congruence with stride 1 and offset (g0, 10). *)
    let%test "Congruence: top equals stride1 offset(\"g0\", 10)" =
      Congruence.equal Congruence.Top (Congruence.Congruence {stride = 1l; offset = ("g0", 10l)})

    (* Congruences with same stride and same absolute offset are equal. *)
    let%test "Congruence: same stride same absolute offset" =
      Congruence.equal (Congruence.Congruence {stride = 3l; offset = ("", 5l)})
                       (Congruence.Congruence {stride = 3l; offset = ("", 5l)})

    (* Congruences with same stride and same absolute offset are not equal but equivalent. *)
    let%test "Congruence: same stride, equivalent absolute offsets" =
      Congruence.equal (Congruence.Congruence {stride = 3l; offset = ("", 5l)})
                       (Congruence.Congruence {stride = 3l; offset = ("", 2l)})

    (* Congruences with same stride and same absolute offset are not equal but equivalent. *)
    let%test "Congruence: same stride, equivalent relative offsets" =
      Congruence.equal (Congruence.Congruence {stride = 3l; offset = ("v", 5l)})
                       (Congruence.Congruence {stride = 3l; offset = ("v", 2l)})

    (* Congruences with different strides are not equal. *)
    let%test "Congruence: different strides" =
      not (Congruence.equal (Congruence.Congruence {stride = 3l; offset = ("", 5l)})
                            (Congruence.Congruence {stride = 4l; offset = ("", 5l)}))

    (* Congruences with different absolute offsets are not equal. *)
    let%test "Congruence: different absolute offsets that are not equivalent" =
      not (Congruence.equal (Congruence.Congruence {stride = 3l; offset = ("", 5l)})
                            (Congruence.Congruence {stride = 3l; offset = ("", 6l)}))

    (* Bottom is equal to Bottom. *)
    let%test "Congruence: bottom equals bottom" =
      Congruence.equal Congruence.Bottom Congruence.Bottom

    (* Bottom is not equal to Top. *)
    let%test "Congruence: bottom not equal top" =
      not (Congruence.equal Congruence.Bottom Congruence.Top)

    (* Relative congruences with same var and offset are equal. *)
    let%test "Congruence: same relative var and offset" =
      Congruence.equal
        (Congruence.Congruence {stride = 2l; offset = ("x", 4l)})
        (Congruence.Congruence {stride = 2l; offset = ("x", 4l)})

    (* Relative congruences with same var but non equivalent offsets are not equal. *)
    let%test "Congruence: same var, non equivalent offsets" =
      not (Congruence.equal
             (Congruence.Congruence {stride = 2l; offset = ("x", 4l)})
             (Congruence.Congruence {stride = 2l; offset = ("x", 5l)}))

    (* Relative congruences with different vars are not equal. *)
    let%test "Congruence: different vars" =
      not (Congruence.equal
             (Congruence.Congruence {stride = 2l; offset = ("x", 4l)})
             (Congruence.Congruence {stride = 2l; offset = ("y", 4l)}))

    (* Congruences with same offset but different strides are not equal. *)
    let%test "Congruence: same offset different strides" =
      not (Congruence.equal
             (Congruence.Congruence {stride = 2l; offset = ("x", 4l)})
             (Congruence.Congruence {stride = 3l; offset = ("x", 4l)}))

    (* Top is not equal to Bottom. *)
    let%test "Congruence: top not equal bottom" =
      not (Congruence.equal Congruence.Top Congruence.Bottom)

    (* Top is not equal to Congruence with stride 2 and offset 0. *)
    let%test "Congruence: top not equal stride2 offset0" =
      not (Congruence.equal Congruence.Top (Congruence.Congruence {stride = 2l; offset = ("", 0l)}))

    (* Stride is equal to 1, one relative offset, one absolute offset *)
    let%test "Congruence: both strides are equal to 1" =
      Congruence.equal (Congruence.Congruence {stride = 1l; offset = ("", 0l)}) 
                       (Congruence.Congruence {stride = 1l; offset = ("v", 7l)})

    (* to string of Top should be "ℤ". *)
    let%test "Congruence: to string top" =
      String.equal (Congruence.to_string Congruence.Top) "ℤ"

    (* to_string of Bottom should be "⊥". *)
    let%test "Congruence: to_string_bottom" =
      String.equal (Congruence.to_string Congruence.Bottom) "∅"

    (* to_string of stride 1, absolute offset 0 should be "ℤ". *)
    let%test "Congruence: to_string stride1 offset0" =
      String.equal
        (Congruence.to_string (Congruence.Congruence {stride = 1l; offset = ("", 0l)}))
        "ℤ"

    (* to_string of stride 2, absolute offset 3 should be "2ℤ + 3". *)
    let%test "Congruence: to_string stride2 offset3" =
      String.equal
        (Congruence.to_string (Congruence.Congruence {stride = 2l; offset = ("", 3l)}))
        "2ℤ+3"

    (* to_string of stride 4, relative offset ("x", 1) should be "4ℤ + (x + 1)". *)
    let%test "Congruence: to_string relative" =
      String.equal
        (Congruence.to_string (Congruence.Congruence {stride = 4l; offset = ("x", 1l)}))
        "4ℤ+(x+1)"
    
    (* to_string of stride 4, relative offset ("x", 1) should be "4ℤ + (x + 1)". *)
    let%test "Congruence: to_string relative offset0" =
      String.equal
        (Congruence.to_string (Congruence.Congruence {stride = 10l; offset = ("x", 0l)}))
        "10ℤ+x"

    (* Joining Top with anything should return Top. *)
    let%test "Congruence: join_top_left" =
      Congruence.equal
        (Congruence.join Congruence.Top (Congruence.Congruence {stride = 2l; offset = ("", 0l)}))
        Congruence.Top

    let%test "Congruence: join_top_right" =
      Congruence.equal
        (Congruence.join (Congruence.Congruence {stride = 2l; offset = ("v", 3l)}) Congruence.Top)
        Congruence.Top

    (* Joining Bottom with a congruence should return that congruence. *)
    let%test "Congruence: join_bottom_left" =
      Congruence.equal
        (Congruence.join Congruence.Bottom (Congruence.Congruence {stride = 27l; offset = ("", 36l)}))
        (Congruence.Congruence {stride = 27l; offset = ("", 36l)})

    let%test "Congruence: join_bottom_right" =
      Congruence.equal
        (Congruence.join (Congruence.Congruence {stride = 2l; offset = ("", 3l)}) Congruence.Bottom)
        (Congruence.Congruence {stride = 2l; offset = ("", 3l)})

    (* Joining two equal congruences returns the same congruence. *)
    let%test "Congruence: join_equal_congruences" =
      let c = Congruence.Congruence {stride = 4l; offset = ("", 1l)} in
      Congruence.equal (Congruence.join c c) c

    (* Joining congruences with same offset but different stride returns least common multiple stride. *)
    let%test "Congruence: join_same_offset_diff_stride" =
      Congruence.equal
        (Congruence.join
          (Congruence.Congruence {stride = 2l; offset = ("", 3l)})
          (Congruence.Congruence {stride = 4l; offset = ("", 3l)}))
        (Congruence.Congruence {stride = 2l; offset = ("", 3l)})  (* gcd(2,4)=2; offset=3 *)

    (* Joining congruences with different offsets. *)
    let%test "Congruence: join (2ℤ+3) and (2ℤ+2) => Top" =
      let c1 = Congruence.Congruence {stride = 2l; offset = ("", 3l)} in
      let c2 = Congruence.Congruence {stride = 2l; offset = ("", 2l)} in 
      let j = Congruence.join c1 c2 in 
      print_endline ("[JOIN of congruences]     (" ^ Congruence.to_string c1 ^ ") ⊔ (" ^ Congruence.to_string c2 ^ ") = " ^ Congruence.to_string j);
      Congruence.equal j Congruence.Top

    (* Joining congruences with different offsets. *)
    let%test "Congruence: join (10ℤ+3) and (10ℤ+8) => 5ℤ+3" =
      let c1 = Congruence.Congruence {stride = 10l; offset = ("", 3l)} in
      let c2 = Congruence.Congruence {stride = 10l; offset = ("", 8l)} in 
      let j = Congruence.join c1 c2 in 
      print_endline ("[JOIN of congruences]     (" ^ Congruence.to_string c1 ^ ") ⊔ (" ^ Congruence.to_string c2 ^ ") = " ^ Congruence.to_string j);
      Congruence.equal j (Congruence.Congruence {stride = 5l; offset = ("", 3l)})

    (* Joining congruences with different types of offsets should return Top. *)
    let%test "Congruence: join_relative_and_absolute" =
      let c1 = (Congruence.Congruence {stride = 2l; offset = ("", 3l)}) in
      let c2 = (Congruence.Congruence {stride = 2l; offset = ("x", 3l)}) in
      let j = Congruence.join c1 c2 in
      print_endline ("[JOIN of congruences]     (" ^ Congruence.to_string c1 ^ ") ⊔ (" ^ Congruence.to_string c2 ^ ") = " ^ Congruence.to_string j);
      Congruence.equal j Congruence.Top

    (* Joining congruences with equivalent relative offsets. *)
    let%test "Congruence: join_equivalent_relative_offsets" =
      let c1 = Congruence.Congruence {stride = 20l; offset = ("v", 3l)} in
      let c2 = Congruence.Congruence {stride = 20l; offset = ("v", 23l)} in 
      let j = Congruence.join c1 c2 in 
      print_endline ("[JOIN of congruences]     (" ^ Congruence.to_string c1 ^ ") ⊔ (" ^ Congruence.to_string c2 ^ ") = " ^ Congruence.to_string j);
      Congruence.equal j c1

    (* Joining congruences with equivalent relative offsets. *)
    let%test "Congruence: join_non_equivalent_relative_offsets" =
      let c1 = Congruence.Congruence {stride = 20l; offset = ("v", 3l)} in
      let c2 = Congruence.Congruence {stride = 20l; offset = ("v", 21l)} in 
      let j = Congruence.join c1 c2 in 
      print_endline ("[JOIN of congruences]     (" ^ Congruence.to_string c1 ^ ") ⊔ (" ^ Congruence.to_string c2 ^ ") = " ^ Congruence.to_string j);
      Congruence.equal j (Congruence.Congruence {stride = 2l; offset = ("v", 1l)}
      )

    (* Joining congruences with different relative variables returns Top. *)
    let%test "Congruence: join_diff_relative_vars" =
      let c1 = Congruence.Congruence {stride = 20l; offset = ("v", 3l)} in
      let c2 = Congruence.Congruence {stride = 20l; offset = ("x", 23l)} in 
      let j = Congruence.join c1 c2 in 
      print_endline ("[JOIN of congruences]     (" ^ Congruence.to_string c1 ^ ") ⊔ (" ^ Congruence.to_string c2 ^ ") = " ^ Congruence.to_string j);
      Congruence.equal j Congruence.Top

    (* Meeting Bottom with anything should return Bottom. *)
    let%test "Congruence: meet_bottom_left" =
      Congruence.equal
        (Congruence.meet Congruence.Bottom (Congruence.Congruence {stride = 2l; offset = ("", 0l)}))
        Congruence.Bottom

    let%test "Congruence: meet_bottom_right" =
      Congruence.equal
        (Congruence.meet (Congruence.Congruence {stride = 2l; offset = ("v", 3l)}) Congruence.Bottom)
        Congruence.Bottom

    (* Meeting Top with a congruence should return that congruence. *)
    let%test "Congruence: meet_top_left" =
      Congruence.equal
        (Congruence.meet Congruence.Top (Congruence.Congruence {stride = 27l; offset = ("", 36l)}))
        (Congruence.Congruence {stride = 27l; offset = ("", 36l)})

    let%test "Congruence: meet_top_right" =
      Congruence.equal
        (Congruence.meet (Congruence.Congruence {stride = 2l; offset = ("", 3l)}) Congruence.Top)
        (Congruence.Congruence {stride = 2l; offset = ("", 3l)})

    (* Meeting two equal congruences returns the same congruence. *)
    let%test "Congruence: meet_equal_congruences" =
      let c = Congruence.Congruence {stride = 4l; offset = ("", 1l)} in
      Congruence.equal (Congruence.meet c c) c

    (* Meeting congruences with same offset but different stride. *)
    let%test "Congruence: meet_same_offset_diff_stride" =
      let c1 = (Congruence.Congruence {stride = 2l; offset = ("", 3l)}) in
      let c2 = (Congruence.Congruence {stride = 4l; offset = ("", 3l)}) in
      let m = Congruence.meet c1 c2 in
      print_endline ("[MEET of congruences]     (" ^ Congruence.to_string c1 ^ ") ⊓ (" ^ Congruence.to_string c2 ^ ") = " ^ Congruence.to_string m);
      Congruence.equal m c2

      (* Meeting congruences with same offset but different stride. *)
    let%test "Congruence: meet_same_offset_diff_stride2" =
      let c1 = (Congruence.Congruence {stride = 2l; offset = ("", 3l)}) in
      let c2 = (Congruence.Congruence {stride = 5l; offset = ("", 3l)}) in
      let m = Congruence.meet c1 c2 in
      print_endline ("[MEET of congruences]     (" ^ Congruence.to_string c1 ^ ") ⊓ (" ^ Congruence.to_string c2 ^ ") = " ^ Congruence.to_string m);
      Congruence.equal m (Congruence.Congruence {stride = 10l; offset = ("", 3l)}) (* stride = lcm 2 5*)

    (* Meeting congruences with different offsets. *)
    let%test "Congruence: meet (2ℤ + 3) and (2ℤ + 2) => Top" =
      let c1 = Congruence.Congruence {stride = 2l; offset = ("", 3l)} in
      let c2 = Congruence.Congruence {stride = 2l; offset = ("", 2l)} in 
      let m = Congruence.meet c1 c2 in 
      print_endline ("[MEET of congruences]     (" ^ Congruence.to_string c1 ^ ") ⊓ (" ^ Congruence.to_string c2 ^ ") = " ^ Congruence.to_string m);
      Congruence.equal m Congruence.Bottom

    (* Meeting congruences with different offsets. *)
    let%test "Congruence: meet (10ℤ + 3) and (10ℤ + 8) => Bottom" =
      let c1 = Congruence.Congruence {stride = 10l; offset = ("", 3l)} in
      let c2 = Congruence.Congruence {stride = 10l; offset = ("", 8l)} in 
      let m = Congruence.meet c1 c2 in 
      print_endline ("[MEET of congruences]     (" ^ Congruence.to_string c1 ^ ") ⊓ (" ^ Congruence.to_string c2 ^ ") = " ^ Congruence.to_string m);
      Congruence.equal m Congruence.Bottom

    (* Joining congruences with different types of offsets should return Top. *)
    let%test "Congruence: meet_relative_and_absolute" =
      let c1 = (Congruence.Congruence {stride = 2l; offset = ("", 3l)}) in
      let c2 = (Congruence.Congruence {stride = 2l; offset = ("x", 3l)}) in
      let m = Congruence.meet c1 c2 in
      print_endline ("[MEET of congruences]     (" ^ Congruence.to_string c1 ^ ") ⊓ (" ^ Congruence.to_string c2 ^ ") = " ^ Congruence.to_string m);
      Congruence.equal m Congruence.Bottom

    (* Meeting congruences with equivalent relative offsets. *)
    let%test "Congruence: meet_equivalent_relative_offsets" =
      let c1 = Congruence.Congruence {stride = 20l; offset = ("v", 3l)} in
      let c2 = Congruence.Congruence {stride = 20l; offset = ("v", 23l)} in 
      let m = Congruence.meet c1 c2 in 
      print_endline ("[MEET of congruences]     (" ^ Congruence.to_string c1 ^ ") ⊓ (" ^ Congruence.to_string c2 ^ ") = " ^ Congruence.to_string m);
      Congruence.equal m c1

    let%test "Congruence: meet with singleton congruence" =
      let c1 = Congruence.Congruence {stride = 0l; offset = ("v", 3l)} in
      let c2 = Congruence.Congruence {stride = 20l; offset = ("v", 3l)} in
      let  m = Congruence.meet c1 c2 in 
      print_endline ("[MEET of congruences]     (" ^ Congruence.to_string c1 ^ ") ⊓ (" ^ Congruence.to_string c2 ^ ") = " ^ Congruence.to_string m);
      Congruence.equal m (Congruence.Congruence {stride = 0l; offset = ("v", 3l)})

    (* Meeting congruences with equivalent relative offsets. *)
    let%test "Congruence: join_non_equivalent_relative_offsets" =
      let c1 = Congruence.Congruence {stride = 20l; offset = ("v", 3l)} in
      let c2 = Congruence.Congruence {stride = 20l; offset = ("v", 21l)} in 
      let m = Congruence.meet c1 c2 in 
      print_endline ("[MEET of congruences]     (" ^ Congruence.to_string c1 ^ ") ⊓ (" ^ Congruence.to_string c2 ^ ") = " ^ Congruence.to_string m);
      Congruence.equal m Congruence.Bottom

    (* Meeting congruences with different relative variables returns Bottom. *)
    let%test "Congruence: join_diff_relative_vars" =
      let c1 = Congruence.Congruence {stride = 20l; offset = ("v", 3l)} in
      let c2 = Congruence.Congruence {stride = 20l; offset = ("x", 23l)} in 
      let m = Congruence.meet c1 c2 in 
      print_endline ("[MEET of congruences]     (" ^ Congruence.to_string c1 ^ ") ⊔ (" ^ Congruence.to_string c2 ^ ") = " ^ Congruence.to_string m);
      Congruence.equal m Congruence.Bottom
  end)

  (*                                                                                         
      iiii                             tttt                                                  
     i::::i                         ttt:::t                                                  
      iiii                          t:::::t                                                  
                                    t:::::t                                                  
    iiiiiii nnnn  nnnnnnnn    ttttttt:::::ttttttt        eeeeeeeeeeee    rrrrr   rrrrrrrrr   
    i:::::i n:::nn::::::::nn  t:::::::::::::::::t      ee::::::::::::ee  r::::rrr:::::::::r  
    i::::i n::::::::::::::nn t:::::::::::::::::t     e::::::eeeee:::::eer:::::::::::::::::r 
    i::::i nn:::::::::::::::ntttttt:::::::tttttt    e::::::e     e:::::err::::::rrrrr::::::r
    i::::i   n:::::nnnn:::::n      t:::::t          e:::::::eeeee::::::e r:::::r     r:::::r
    i::::i   n::::n    n::::n      t:::::t          e:::::::::::::::::e  r:::::r     rrrrrrr
    i::::i   n::::n    n::::n      t:::::t          e::::::eeeeeeeeeee   r:::::r            
    i::::i   n::::n    n::::n      t:::::t    tttttte:::::::e            r:::::r            
    i::::::i  n::::n    n::::n      t::::::tttt:::::te::::::::e           r:::::r            
    i::::::i  n::::n    n::::n      tt::::::::::::::t e::::::::eeeeeeee   r:::::r            
    i::::::i  n::::n    n::::n        tt:::::::::::tt  ee:::::::::::::e   r:::::r            
    iiiiiiii  nnnnnn    nnnnnn          ttttttttttt      eeeeeeeeeeeeee   rrrrrrr  *)
  let%test_module "Interval tests" = (module struct
    let%test "Congruence_tests" =
      print_endline "\n------- Interval MODULE -------\n"; true

    (* Top is equal to Top. *)
    let%test "equal_top_top" =
      Interval.equal Interval.Top Interval.Top

    (* Bottom is equal to Bottom. *)
    let%test "equal_bottom_bottom" =
      Interval.equal Interval.Bottom Interval.Bottom

    (* Top is not equal to Bottom. *)
    let%test "not_equal_top_bottom" =
      not (Interval.equal Interval.Top Interval.Bottom)

    (* Two intervals with same bounds are equal. *)
    let%test "equal_same_bounds" =
      Interval.equal
        (Interval.Interval {lower_bound = Int 0l; upper_bound = Int 5l})
        (Interval.Interval {lower_bound = Int 0l; upper_bound = Int 5l})

    (* Intervals with different lower bounds are not equal. *)
    let%test "not_equal_diff_lower" =
      not (Interval.equal
            (Interval.Interval {lower_bound = Int 1l; upper_bound = Int 5l})
            (Interval.Interval {lower_bound = Int 0l; upper_bound = Int 5l}))

    (* Intervals with different upper bounds are not equal. *)
    let%test "not_equal_diff_upper" =
      not (Interval.equal
            (Interval.Interval {lower_bound = Int 0l; upper_bound = Int 6l})
            (Interval.Interval {lower_bound = Int 0l; upper_bound = Int 5l}))

    (* Intervals with one infinite bound are not equal to finite-bounded intervals. *)
    let%test "not_equal_finite_vs_infinite" =
      not (Interval.equal
            (Interval.Interval {lower_bound = NegInfinity; upper_bound = Int 5l})
            (Interval.Interval {lower_bound = Int 0l; upper_bound = Int 5l}))

    let%test "equal_neginf_Infinity" =
      Interval.equal
        (Interval.Interval {lower_bound = NegInfinity; upper_bound = Infinity})
        (Interval.Interval {lower_bound = NegInfinity; upper_bound = Infinity})

    (* to_string of Top should be "ℤ". *)
    let%test "to_string_top" =
      let s = Interval.to_string Interval.Top in
      print_endline ("[Interval.to_string]     Top → " ^ s);
      String.equal s "ℤ"

    (* to_string of Bottom should be "∅". *)
    let%test "to_string_bottom" =
      let s = Interval.to_string Interval.Bottom in
      print_endline ("[Interval.to_string]     Bottom → " ^ s);
      String.equal s "∅"

    (* to_string of [0, 5] should be "[0, 5]". *)
    let%test "to_string_0_5" =
      let s = Interval.to_string (Interval.Interval {lower_bound = Int 0l; upper_bound = Int 5l}) in
      print_endline ("[Interval.to_string]     [0,5] → " ^ s);
      String.equal s "[0,5]"

    (* to_string of [-∞, 10] should be "[-∞, 10]". *)
    let%test "to_string_neg_inf_10" =
      let s = Interval.to_string (Interval.Interval {lower_bound = NegInfinity; upper_bound = Int 10l}) in
      print_endline ("[Interval.to_string]     ]-∞,10] → " ^ s);
      String.equal s "]-∞,10]"

    (* to_string of [4, ∞] should be "[4, ∞]". *)
    let%test "to_string_4_pos_inf" =
      let s = Interval.to_string (Interval.Interval {lower_bound = Int 4l; upper_bound = Infinity}) in
      print_endline ("[Interval.to_string]     [4,∞[ → " ^ s);
      String.equal s "[4,∞["

    (* to_string of [-∞, ∞] should be "ℤ". *)
    let%test "to_string_neg_inf_pos_inf" =
      let s = Interval.to_string (Interval.Interval {lower_bound = NegInfinity; upper_bound = Infinity}) in
      print_endline ("[Interval.to_string]     [-∞,∞] → " ^ s);
      String.equal s "ℤ"


    (* join of Top with anything is Top. *)
    let%test "join_top" =
      let joined = Interval.join Interval.Top (Interval.Interval {lower_bound = Int 0l; upper_bound = Int 5l}) in
      print_endline ("[JOIN of intervals]     Top ⊔ [0, 5] → " ^ Interval.to_string joined);
      Interval.equal joined Interval.Top

    (* join of Bottom with an interval is that interval. *)
    let%test "join_bottom" =
      let i = Interval.Interval {lower_bound = Int 2l; upper_bound = Int 4l} in
      let joined = Interval.join Interval.Bottom i in
      print_endline ("[JOIN of intervals]     ⊥ ⊔ [2, 4] → " ^ Interval.to_string joined);
      Interval.equal joined i

    (* join of two intervals gives correct bounds. *)
    let%test "[JOIN of intervals]     join_0_5_3_10" =
      let a = Interval.Interval {lower_bound = Int 0l; upper_bound = Int 5l} in
      let b = Interval.Interval {lower_bound = Int 3l; upper_bound = Int 10l} in
      let joined = Interval.join a b in
      print_endline ("[JOIN of intervals]     [0, 5] ⊔ [3, 10] → " ^ Interval.to_string joined);
      Interval.equal joined (Interval.Interval {lower_bound = Int 0l; upper_bound = Int 10l})

    (* join of two disjoint intervals gives correct bounds. *)
    let%test "join_0_5_7_10" =
      let a = Interval.Interval {lower_bound = Int 0l; upper_bound = Int 5l} in
      let b = Interval.Interval {lower_bound = Int 7l; upper_bound = Int 10l} in
      let joined = Interval.join a b in
      print_endline ("[JOIN of intervals]     [0, 5] ⊔ [7, 10] → " ^ Interval.to_string joined);
      Interval.equal joined (Interval.Interval {lower_bound = Int 0l; upper_bound = Int 10l})

    (* meet of Top and an interval is that interval. *)
    let%test "meet_top" =
      let i = Interval.Interval {lower_bound = Int 1l; upper_bound = Int 7l} in
      let met = Interval.meet Interval.Top i in
      print_endline ("[MEET of intervals]     Top ⊓ [1, 7] → " ^ Interval.to_string met);
      Interval.equal met i

    (* meet of disjoint intervals is Bottom. *)
    let%test "meet_disjoint" =
      let a = Interval.Interval {lower_bound = Int 0l; upper_bound = Int 2l} in
      let b = Interval.Interval {lower_bound = Int 5l; upper_bound = Int 10l} in
      let met = Interval.meet a b in
      print_endline ("[MEET of intervals]     [0, 2] ⊓ [5, 10] → " ^ Interval.to_string met);
      Interval.equal met Interval.Bottom

    (* meet of overlapping intervals gives the intersection. *)
    let%test "meet_overlap" =
      let a = Interval.Interval {lower_bound = Int 0l; upper_bound = Int 10l} in
      let b = Interval.Interval {lower_bound = Int 5l; upper_bound = Int 15l} in
      let met = Interval.meet a b in
      print_endline ("[MEET of intervals]     [0, 10] ⊓ [5, 15] → " ^ Interval.to_string met);
      Interval.equal met (Interval.Interval {lower_bound = Int 5l; upper_bound = Int 10l})
  end)


  (*                     iiii                      
                        i::::i                     
                         iiii                      
                                                    
    rrrrr   rrrrrrrrr   iiiiiii     cccccccccccccccc
    r::::rrr:::::::::r  i:::::i   cc:::::::::::::::c
    r:::::::::::::::::r  i::::i  c:::::::::::::::::c
    rr::::::rrrrr::::::r i::::i c:::::::cccccc:::::c
    r:::::r     r:::::r i::::i c::::::c     ccccccc
    r:::::r     rrrrrrr i::::i c:::::c             
    r:::::r             i::::i c:::::c             
    r:::::r             i::::i c::::::c     ccccccc
    r:::::r            i::::::ic:::::::cccccc:::::c
    r:::::r            i::::::i c:::::::::::::::::c
    r:::::r            i::::::i  cc:::::::::::::::c
    rrrrrrr            iiiiiiii    cccccccccccccccc*)
  let%test_module "RIC tests" = (module struct
    let%test "Congruence_tests" =
      print_endline "\n------- RIC MODULE -------\n"; true

    let%test "reduce_top" =
      print_endline ("[RIC.reduce]     ⊤ → " ^ to_string (reduce Top));
      RIC.equal (reduce Top) Top

    let%test "reduce 2[-inf, -40]-11" = 
      let r = ric (2l, NegInfinity, Int (-40l), ("", -9l)) in
      print_endline (to_string r);
      true

    let%test "reduce_bottom" =
      print_endline ("[RIC.reduce]     ⊥ → " ^ to_string (reduce Bottom));
      RIC.equal (reduce Bottom) Bottom

    let%test "reduce_identity" =
      let r = ric (2l, Int 0l, Int 5l, ("", 3l)) in
      print_endline ("[RIC.reduce]     " ^ to_string r ^ " → " ^ to_string (reduce r));
      RIC.equal (reduce r) r

    let%test "reduce_to_bottom" =
      let r = ric (4l, Int 10l, Int 5l, ("", 0l)) in
      print_endline ("[RIC.reduce]     " ^ to_string r ^ " → " ^ to_string (reduce r));
      RIC.equal (reduce r) Bottom

    let%test "reduce_to_top" =
      let r = ric (1l, NegInfinity, Infinity, ("", 0l)) in
      print_endline ("[RIC.reduce]     " ^ to_string r ^ " → " ^ to_string (reduce r));
      RIC.equal (reduce r) Top

    let%test "reduce_stride_zero" =
      let r = ric (0l, Int 2l, Int 2l, ("", 5l)) in
      let reduced = reduce r in
      print_endline ("[RIC.reduce]     " ^ to_string r ^ " → " ^ to_string reduced);
      RIC.equal reduced (ric (0l, Int 0l, Int 0l, ("", 5l)))

    let%test "reduce_relative_offset" =
      let r = ric (3l, Int 2l, Int 5l, ("x", 4l)) in
      let reduced = reduce r in
      print_endline ("[RIC.reduce]     " ^ to_string r ^ " → " ^ to_string reduced);
      RIC.equal reduced (ric (3l, Int 0l, Int 3l, ("x", 10l)))

    let%test "reduce_shift_and_normalize" =
      let original = ric (4l, Int 1l, Int 2l, ("", 1l)) in
      let reduced = reduce original in
      print_endline ("[RIC.reduce]     (4[1,2]+1) → " ^ to_string reduced);
      RIC.equal reduced (ric (4l, Int 0l, Int 1l, ("", 5l)))

    let%test "reduce_neg_infinity" =
      let original = ric (4l, NegInfinity, Int 5l, ("", 10l)) in
      let reduced = reduce original in
      print_endline ("[RIC.reduce]     (4[-∞,5]+10) → " ^ to_string reduced);
      RIC.equal reduced (ric (4l, NegInfinity, Int 5l, ("", 10l)))

    let%test "of_congruence_and_interval_top_top" =
      let r = of_congruence_and_interval Congruence.Top Interval.Top in
      print_endline ("[of congruence and interval]     (ℤ, ℤ) → " ^ to_string r);
      RIC.equal r Top

    let%test "of_congruence_and_interval_bottom_top" =
      let r = of_congruence_and_interval Congruence.Bottom Interval.Top in
      print_endline ("[of congruence and interval]     (⊥, ℤ) → " ^ to_string r);
      RIC.equal r Bottom

    let%test "of_congruence_and_interval_top_bottom" =
      let r = of_congruence_and_interval Congruence.Top Interval.Bottom in
      print_endline ("[of congruence and interval]     (ℤ, ⊥) → " ^ to_string r);
      RIC.equal r Bottom

    let%test "of_congruence_and_interval_abs" =
      let c = Congruence.Congruence {stride = 2l; offset = ("", 1l)} in
      let i = Interval.Interval {lower_bound = Int 1l; upper_bound = Int 7l} in
      let r = of_congruence_and_interval c i in
      print_endline ("[of congruence and interval]     (2ℤ + 1, [1, 7]) → " ^ to_string r);
      RIC.equal r (ric (2l, Int 0l, Int 3l, ("", 1l)))  (* (1 + 2*[0..3]) = {1,3,5,7} *)

    let%test "of_congruence_and_interval_relative" =
      let c = Congruence.Congruence {stride = 3l; offset = ("x", 2l)} in
      let i = Interval.Interval {lower_bound = Int 5l; upper_bound = Int 17l} in
      let r = of_congruence_and_interval c i in
      print_endline ("[of congruence and interval]     (3ℤ + (x+2), [5, 17]) → " ^ to_string r);
      RIC.equal r (ric (3l, Int 1l, Int 5l, ("x", 2l)))  (* (x+2 + 3*[1..5]) = [5..17] *)

    let%test "of_congruence_and_interval_stride_0" =
      let c = Congruence.Congruence {stride = 0l; offset = ("", 8l)} in
      let i = Interval.Interval {lower_bound = Int 8l; upper_bound = Int 8l} in
      let r = of_congruence_and_interval c i in
      print_endline ("[of congruence and interval]     (0ℤ + 8, [8, 8]) → " ^ to_string r);
      RIC.equal r (ric (0l, Int 0l, Int 0l, ("", 8l)))

    let%test "to_congruence_and_interval_top" =
      let c, i = to_congruence_and_interval Top in
      print_endline ("[to congruence and interval]     Top → (" ^ Congruence.to_string c ^ ", " ^ Interval.to_string i ^ ")");
      Congruence.equal c Congruence.Top && Interval.equal i Interval.Top

    let%test "to_congruence_and_interval_bottom" =
      let c, i = to_congruence_and_interval Bottom in
      print_endline ("[to congruence and interval]     Bottom → (" ^ Congruence.to_string c ^ ", " ^ Interval.to_string i ^ ")");
      Congruence.equal c Congruence.Bottom && Interval.equal i Interval.Bottom

    let%test "to_congruence_and_interval_absolute" =
      let r = ric (2l, Int 0l, Int 3l, ("", 5l)) in
      let c, i = to_congruence_and_interval r in
      print_endline ("[to congruence and interval]     (2[0, 3] + 5) → (" ^ Congruence.to_string c ^ ", " ^ Interval.to_string i ^ ")");
      Congruence.equal c (Congruence.Congruence {stride = 2l; offset = ("", 5l)}) &&
      Interval.equal i (Interval.Interval {lower_bound = Int 5l; upper_bound = Int 11l})

    let%test "to_congruence_and_interval_absolute2" =
      let r = ric (3l, Int 1l, Int 4l, ("", 2l)) in
      let c, i = to_congruence_and_interval r in
      print_endline ("[to congruence and interval]     (3[1, 4] + 2) → (" ^ Congruence.to_string c ^ ", " ^ Interval.to_string i ^ ")");
      Congruence.equal c (Congruence.Congruence {stride = 3l; offset = ("", 2l)}) &&
      Interval.equal i (Interval.Interval {lower_bound = Int 5l; upper_bound = Int 14l})

    let%test "to_congruence_and_interval_relative" =
      let r = ric (3l, Int 1l, Int 4l, ("x", 2l)) in
      let c, i = to_congruence_and_interval r in
      print_endline ("[to congruence and interval]     (3[1, 4] + (x + 2)) → (" ^ Congruence.to_string c ^ ", " ^ Interval.to_string i ^ ")");
      Congruence.equal c (Congruence.Congruence {stride = 3l; offset = ("x", 2l)}) &&
      Interval.equal i (Interval.Interval {lower_bound = Int 5l; upper_bound = Int 14l})

    let%test "to_congruence_and_interval_stride_zero" =
      let r = ric (0l, Int 0l, Int 0l, ("", 7l)) in
      let c, i = to_congruence_and_interval r in
      print_endline ("[to congruence and interval]     (0[0, 0] + 7) → (" ^ Congruence.to_string c ^ ", " ^ Interval.to_string i ^ ")");
      Congruence.equal c (Congruence.Congruence {stride = 0l; offset = ("", 7l)}) &&
      Interval.equal i (Interval.Interval {lower_bound = Int 7l; upper_bound = Int 7l})

    let%test "equal_top_top" =
      RIC.equal Top Top

    let%test "equal_bottom_bottom" =
      RIC.equal Bottom Bottom

    let%test "equal_same_ric" =
      let r1 = ric (2l, Int 0l, Int 3l, ("x", 4l)) in
      let r2 = ric (2l, Int 0l, Int 3l, ("x", 4l)) in
      RIC.equal r1 r2

    let%test "not_equal_different_stride" =
      let r1 = ric (2l, Int 0l, Int 3l, ("x", 4l)) in
      let r2 = ric (3l, Int 0l, Int 3l, ("x", 4l)) in
      not (RIC.equal r1 r2)

    let%test "not_equal_different_bounds" =
      let r1 = ric (2l, Int 0l, Int 3l, ("x", 4l)) in
      let r2 = ric (2l, Int 1l, Int 3l, ("x", 4l)) in
      not (RIC.equal r1 r2)

    let%test "not_equal_different_offset" =
      let r1 = ric (2l, Int 0l, Int 3l, ("x", 4l)) in
      let r2 = ric (2l, Int 0l, Int 3l, ("x", 5l)) in
      not (RIC.equal r1 r2)

    let%test "equal_equivalent_after_reduce" =
      let r1 = ric (2l, Int 1l, Int 4l, ("", 3l)) in
      let r2 = ric (2l, Int 0l, Int 3l, ("", 5l)) in
      RIC.equal r1 r2

    let%test "equal_neg_infinity_shifted_absolute" =
      let r1 = ric (3l, NegInfinity, Int 3l, ("", 10l)) in
      let r2 = ric (3l, NegInfinity, Int 5l, ("", 4l)) in
      print_endline ("[RIC.equal]     " ^ to_string r1 ^ " = " ^ to_string r2 ^ " → " ^ string_of_bool (equal r1 r2));
      equal r1 r2

    let%test "equal_neg_infinity_shifted_relative" =
      let r1 = ric (3l, NegInfinity, Int 3l, ("x", 10l)) in
      let r2 = ric (3l, NegInfinity, Int 5l, ("x", 4l)) in
      print_endline ("[RIC.equal]     " ^ to_string r1 ^ " = " ^ to_string r2 ^ " → " ^ string_of_bool (equal r1 r2));
      equal r1 r2

    let%test "to_string_top" =
      let s = to_string Top in
      print_endline ("[RIC.to_string]     Top → " ^ s);
      String.equal s "⊤"

    let%test "to_string_bottom" =
      let s = to_string Bottom in
      print_endline ("[RIC.to_string]     Bottom → " ^ s);
      String.equal s "⊥"

    let%test "to_string_absolute" =
      let s = to_string (ric (2l, Int 0l, Int 4l, ("", 5l))) in
      print_endline ("[RIC.to_string]     2[0,4]+5 → " ^ s);
      String.equal s "2[0,4]+5"

    let%test "to_string_relative" =
      let s = to_string (ric (3l, Int 0l, Int 3l, ("x", 2l))) in
      print_endline ("[RIC.to_string]     3[0,3]+(x+2) → " ^ s);
      String.equal s "3[0,3]+(x+2)"

    let%test "to_string_relative_offset_0" =
      let s = to_string (ric (3l, Int 0l, Int 3l, ("x", 0l))) in
      print_endline ("[RIC.to_string]     3[0,3]+x → " ^ s);
      String.equal s "3[0,3]+x"

    let%test "to_string_stride_1" =
      let s = to_string (ric (1l, Int 0l, Int 3l, ("", 0l))) in
      print_endline ("[RIC.to_string]     1[0,3]+0 → " ^ s);
      String.equal s "[0,3]"

    let%test "meet {1,2,3,4} with {2,4,6,8}" =
      let r1 = ric (1l, Int 1l, Int 4l, ("", 0l)) in
      let r2 = ric (2l, Int 0l, Int 4l, ("", 0l)) in
      let m = meet r1 r2 in
      let expected = ric (2l, Int 1l, Int 2l, ("", 0l)) in
      print_endline ("[MEET of RICs]     " ^ to_string r1 ^ " ⊓ " ^ to_string r2 ^ " = " ^ to_string m);
      RIC.equal m expected

    let%test "meet_top_and_r" =
      let r = ric (2l, Int 0l, Int 4l, ("", 5l)) in
      let m = meet Top r in
      print_endline ("[MEET of RICs]     ⊤ ⊓ " ^ to_string r ^ " → " ^ to_string m);
      RIC.equal m r

    let%test "meet_bottom_and_r" =
      let r = ric (2l, Int 0l, Int 4l, ("", 5l)) in
      let m = meet Bottom r in
      print_endline ("[MEET of RICs]     ⊥ ⊓ " ^ to_string r ^ " → " ^ to_string m);
      RIC.equal m Bottom

    let%test "meet_regular" =
      let r1 = ric (2l, Int 0l, Int 4l, ("", 1l)) in
      let r2 = ric (4l, Int 0l, Int 2l, ("", 1l)) in
      let m = meet r1 r2 in
      print_endline ("[MEET of RICs]     " ^ to_string r1 ^ " ⊓ " ^ to_string r2 ^ " → " ^ to_string m);
      RIC.equal m (ric (4l, Int 0l, Int 2l, ("", 1l)))

    let%test "meet_disjoint" =
      let r1 = ric (2l, Int 0l, Int 1l, ("", 1l)) in
      let r2 = ric (2l, Int 0l, Int 1l, ("", 2l)) in
      let m = meet r1 r2 in
      print_endline ("[MEET of RICs]     " ^ to_string r1 ^ " ⊓ " ^ to_string r2 ^ " → " ^ to_string m);
      RIC.equal m Bottom

    let%test "meet [0,1]+(g0-43) and [0,1]+(g0-42)" =
      let r1 = ric (1l, Int 0l, Int 1l, ("g0", -43l)) in
      let r2 = ric (1l, Int 0l, Int 1l, ("g0", -42l)) in
      let m = meet r1 r2 in
      print_endline ("[MEET of RICs]     " ^ to_string r1 ^ " ⊓ " ^ to_string r2 ^ " → " ^ to_string m);
      RIC.equal m (ric (0l, Int 0l, Int 0l, ("g0", -42l)))

    let%test "join_top_and_r" =
      let r = ric (2l, Int 0l, Int 4l, ("", 5l)) in
      let j = join Top r in
      print_endline ("[JOIN of RICs]     ⊤ ⊔ " ^ to_string r ^ " → " ^ to_string j);
      RIC.equal j Top

    let%test "join 2[0,1]-4 and 2[0,3]" =
      let r1 = ric (2l, Int 0l, Int 1l, ("", -4l)) 
      and r2 = ric (2l, Int 0l, Int 3l, ("", 0l)) in
      let j = join r1 r2 in
      RIC.equal j (ric (2l, Int 0l, Int 5l, ("", -4l)))

    let%test "join_bottom_and_r" =
      let r = ric (2l, Int 0l, Int 4l, ("", 5l)) in
      let j = join Bottom r in
      print_endline ("[JOIN of RICs]     ⊥ ⊔ " ^ to_string r ^ " → " ^ to_string j);
      RIC.equal j r

    let%test "join_bottom_(g0-44)" =
      let r = ric (0l, Int 0l, Int 0l, ("g0", -44l)) in
      let j = join Bottom r in
      print_endline ("[JOIN of RICs]     ⊥ ⊔ " ^ to_string r ^ " → " ^ to_string j);
      RIC.equal j r

    let%test "join 3[0,1]+(g0-43) and (g0-42)" =
      let r1 = ric (3l, Int 0l, Int 1l, ("g0", -43l)) in
      let r2 = ric (0l, Int 0l, Int 0l, ("g0", -42l)) in
      let j = join r1 r2 in
      print_endline ("[JOIN of RICs]     " ^ to_string r1 ^ " ⊔ " ^ to_string r2 ^ " → " ^ to_string j);
      RIC.equal j (ric (1l, Int 0l, Int 3l, ("g0", -43l)))

    let%test "join_regular" =
      let r1 = ric (2l, Int 0l, Int 2l, ("", 1l)) in
      let r2 = ric (2l, Int 3l, Int 4l, ("", 1l)) in
      let j = join r1 r2 in
      print_endline ("[JOIN of RICs]     " ^ to_string r1 ^ " ⊔ " ^ to_string r2 ^ " → " ^ to_string j);
      RIC.equal j (ric (2l, Int 0l, Int 4l, ("", 1l)))

    let%test "join_different_stride" =
      let r1 = ric (2l, Int 0l, Int 2l, ("", 1l)) in
      let r2 = ric (4l, Int 1l, Int 2l, ("", 1l)) in
      let j = join r1 r2 in
      print_endline ("[JOIN of RICs]     " ^ to_string r1 ^ " ⊔ " ^ to_string r2 ^ " → " ^ to_string j);
      RIC.equal j (ric (2l, Int 0l, Int 4l, ("", 1l)))
    
    let%test "join_different_offsets" =
      let r1 = ric (2l, Int 0l, Int 2l, ("", 1l)) in
      let r2 = ric (0l, Int 0l, Int 0l, ("a", 0l)) in
      let j = join r1 r2 in
      print_endline ("[JOIN of RICs]     " ^ to_string r1 ^ " ⊔ " ^ to_string r2 ^ " → " ^ to_string j);
      RIC.equal j RIC.Top

    let%test "subset_of_top" =
      let a = ric (2l, Int 0l, Int 3l, ("", 1l)) in
      let b = Top in
      let result = is_subset a ~of_:b in
      print_endline ( "[subset_of_top]     " ^ to_string a ^ " ⊆ " ^ to_string b ^ " → " ^ string_of_bool result);
      result

    let%test "subset_of_bottom" =
      let a = Bottom in
      let b = ric (2l, Int 0l, Int 3l, ("", 1l)) in
      let result = is_subset a ~of_:b in
      print_endline ("[subset_of_bottom]     " ^ to_string a ^ " ⊆ " ^ to_string b ^ " → " ^ string_of_bool result);
      result

    let%test "bottom_subset_of_bottom" =
      let a = Bottom in
      let b = Bottom in
      let result = is_subset a ~of_:b in
      print_endline ("[bottom_subset_of_bottom]     " ^ to_string a ^ " ⊆ " ^ to_string b ^ " → " ^ string_of_bool result);
      result

    let%test "top_subset_of_top" =
      let a = Top in
      let b = Top in
      let result = is_subset a ~of_:b in
      print_endline ("[top_subset_of_top]     " ^ to_string a ^ " ⊆ " ^ to_string b ^ " → " ^ string_of_bool result);
      result

    let%test "subset_of_itself" =
      let r = ric (4l, Int 0l, Int 2l, ("", 8l)) in
      let result = is_subset r ~of_:r in
      print_endline ("[subset_of_itself]     " ^ to_string r ^ " ⊆ " ^ to_string r ^ " → " ^ string_of_bool result);
      result

    let%test "smaller_range_subset" =
      let r1 = ric (2l, Int 0l, Int 2l, ("", 1l)) in
      let r2 = ric (2l, Int 0l, Int 4l, ("", 1l)) in
      let result = is_subset r1 ~of_:r2 in
      print_endline ("[smaller_range_subset]     " ^ to_string r1 ^ " ⊆ " ^ to_string r2 ^ " → " ^ string_of_bool result);
      result

    let%test "different_stride_not_subset" =
      let r1 = ric (4l, Int 0l, Int 2l, ("", 8l)) in
      let r2 = ric (2l, Int 0l, Int 2l, ("", 8l)) in
      let result = not (is_subset r1 ~of_:r2) in
      print_endline ("[different_stride_not_subset     ]" ^ to_string r1 ^ " ⊆ " ^ to_string r2 ^ " → " ^ string_of_bool (not result));
      result

    let%test "different_offset_not_subset" =
      let r1 = ric (2l, Int 0l, Int 2l, ("", 3l)) in
      let r2 = ric (2l, Int 0l, Int 2l, ("", 5l)) in
      let result = not (is_subset r1 ~of_:r2) in
      print_endline ("[different_offset_not_subset]     " ^ to_string r1 ^ " ⊆ " ^ to_string r2 ^ " → " ^ string_of_bool (not result));
      result

    let%test "overlap_not_subset" =
      let r1 = ric (2l, Int 0l, Int 2l, ("", 3l)) in
      let r2 = ric (2l, Int 1l, Int 3l, ("", 3l)) in
      let result = not (is_subset r1 ~of_:r2) in
      print_endline ("[overlap_not_subset]     " ^ to_string r1 ^ " ⊆ " ^ to_string r2 ^ " → " ^ string_of_bool (not result));
      result

    let%test "subset_of_relative_equal" =
      let r1 = ric (5l, Int 1l, Int 2l, ("x", 0l)) in
      let r2 = ric (5l, Int 0l, Int 3l, ("x", 0l)) in
      let result = is_subset r1 ~of_:r2 in
      print_endline ("[subset_of_relative_equal]     " ^ to_string r1 ^ " ⊆ " ^ to_string r2 ^ " → " ^ string_of_bool result);
      result

    let%test "subset_of_relative_different_stride" =
      let r1 = ric (4l, Int 0l, Int 2l, ("x", 0l)) in
      let r2 = ric (2l, Int 0l, Int 2l, ("x", 0l)) in
      let result = not (is_subset r1 ~of_:r2) in
      print_endline ("[subset_of_relative_different_stride]     " ^ to_string r1 ^ " ⊆ " ^ to_string r2 ^ " → " ^ string_of_bool (not result));
      result

    let%test "top_is_not_subset_of_bottom" =
      let a = Top in
      let b = Bottom in
      let result = not (is_subset a ~of_:b) in
      print_endline ("[top_is_not_subset_of_bottom     ]" ^ to_string a ^ " ⊆ " ^ to_string b ^ " → " ^ string_of_bool (not result));
      result

    let%test "complement of Top is [Bottom]" =
      match complement Top with
      | [Bottom] -> true
      | _ -> false

    let%test "complement of Bottom is [Top]" =
      match complement Bottom with
      | [Top] -> true
      | _ -> false
      
    let%test "complement of finite 2[0,2]+4" =
      let r = ric (2l, Int 0l, Int 2l, ("", 4l)) in
      let c = complement r in
      print_endline ("complement of " ^ to_string r ^ " → [  " ^ String.concat ~sep:"; " (List.map c ~f:to_string) ^ "  ]");
      List.length c = 3 &&
      List.mem ~equal c (ric (1l, NegInfinity, Int 0l, ("", 3l))) &&
      List.mem ~equal c (ric (2l, Int 0l, Int 2l, ("", 5l))) &&
      List.mem ~equal c (ric (1l, Int 0l, Infinity, ("", 10l)))

      
    let%test "complement of singleton 0[0,0]+7" =
      let r = ric (0l, Int 0l, Int 0l, ("", 7l)) in
      let c = complement r in
      print_endline ("complement of " ^ to_string r ^ " → [  " ^ String.concat ~sep:"; " (List.map c ~f:to_string) ^ "  ]");
      List.length c = 2 &&
      List.mem ~equal c (ric (1l, NegInfinity, Int 0l, ("", 6l))) &&
      List.mem ~equal c (ric (1l, Int 0l, Infinity, ("", 8l)))
      
    let%test "complement of 3[0,1]+2 has 4 parts" =
      let r = ric (3l, Int 0l, Int 1l, ("", 2l)) in
      let c = complement r in
      print_endline ("complement of " ^ to_string r ^ " → [  " ^ String.concat ~sep:"; " (List.map c ~f:to_string) ^ "  ]");
      List.length c = 4

    let%test "complement of relative 4[0,1]+(x+1)" =
      let r = ric (4l, Int 0l, Int 1l, ("x", 1l)) in
      let c = complement r in
      print_endline ("complement of " ^ to_string r ^ " → [  " ^ String.concat ~sep:"; " (List.map c ~f:to_string) ^ "  ]");
      List.length c = 5 &&
      List.exists c ~f:(fun r' -> String.equal (to_string r')  "]-∞,0]+x") &&
      List.exists c ~f:(fun r' -> String.equal (to_string r')  "4[0,1]+(x+2)") &&
      List.exists c ~f:(fun r' -> String.equal (to_string r')  "[0,∞[+(x+9)")

    let%test "complement preserves relative offset" =
      let r = ric (5l, Int 1l, Int 3l, ("g", 0l)) in
      let offsets = List.map ~f:(function RIC {offset = (v, _); _} -> v | _ -> "") (complement r) in
      List.for_all offsets ~f:(String.equal "g")

    let%test "complement with unbounded upper and stride = 1 gives no superior_RIC" =
      let r = ric (1l, Int 0l, Infinity, ("", 0l)) in
      let c = complement r in
      print_endline ("complement of " ^ to_string r ^ " → [  " ^ String.concat ~sep:"; " (List.map c ~f:to_string) ^ "  ]");
      not (List.exists c ~f:(fun r' -> match r' with RIC {lower_bound = Int _; upper_bound = Infinity; _} -> true | _ -> false))

    let%test "complement with unbounded lower and stride = 1 gives no inferior_RIC" =
      let r = ric (1l, NegInfinity, Int 5l, ("", 0l)) in
      let c = complement r in
      print_endline ("complement of " ^ to_string r ^ " → [  " ^ String.concat ~sep:"; " (List.map c ~f:to_string) ^ "  ]");
      not (List.exists c ~f:(fun r' -> match r' with RIC {lower_bound = NegInfinity; upper_bound = Int _; _} -> true | _ -> false))

    let%test "complement of 2[0,1]+3 includes shifted classes" =
      let r = ric (2l, Int 0l, Int 1l, ("", 3l)) in
      let comp = complement r in
      print_endline ("complement of " ^ to_string r ^ " → [  " ^ String.concat ~sep:"; " (List.map comp ~f:to_string) ^ "  ]");
      List.exists comp ~f:(fun r' -> match r' with RIC {stride = 2l; lower_bound = Int 0l; offset = ("", 4l); _} -> true | _ -> false)
  
    let%test "complement_top" =
      let r = Top in
      let compl = complement r in
      print_endline ("[complement of a RIC]     " ^ to_string r ^ " → [  " ^ String.concat ~sep:"; " (List.map compl ~f:to_string) ^ "  ]");
      List.equal equal compl [Bottom]

    let%test "complement_bottom" =
      let r = Bottom in
      let compl = complement r in
      print_endline ("[complement of a RIC]     " ^ to_string r ^ " → [  " ^ String.concat ~sep:"; " (List.map compl ~f:to_string) ^ "  ]");
      List.equal equal compl  [Top]

    let%test "complement_finite_absolute" =
      let r = ric (2l, Int 0l, Int 2l, ("", 1l)) in
      let compl = complement r in
      print_endline ("[complement of a RIC]     " ^ to_string r ^ " → [  " ^ String.concat ~sep:"; " (List.map compl ~f:to_string) ^ "  ]");
      List.length compl = 3

    let%test "complement_finite_relative" =
      let r = ric (3l, Int 1l, Int 3l, ("x", 2l)) in
      let compl = complement r in
      print_endline ("[complement of a RIC]     " ^ to_string r ^ " → [  " ^ String.concat ~sep:"; " (List.map compl ~f:to_string) ^ "  ]");
      List.length compl = 4

    let%test "complement_infinite_lower" =
      let r = ric (2l, NegInfinity, Int 2l, ("", 0l)) in
      let compl = complement r in
      print_endline ("[complement of a RIC]     " ^ to_string r ^ " → [  " ^ String.concat ~sep:"; " (List.map compl ~f:to_string) ^ "  ]");
      List.length compl = 2

    let%test "complement_infinite_upper" =
      let r = ric (2l, Int 0l, Infinity, ("", 0l)) in
      let compl = complement r in
      print_endline ("[complement of a RIC]     " ^ to_string r ^ " → [  " ^ String.concat ~sep:"; " (List.map compl ~f:to_string) ^ "  ]");
      List.length compl = 2

    let%test "complement_infinite_both" =
      let r = ric (2l, NegInfinity, Infinity, ("", 0l)) in
      let compl = complement r in
      print_endline ("[complement of a RIC]     " ^ to_string r ^ " → [  " ^ String.concat ~sep:"; " (List.map compl ~f:to_string) ^ "  ]");
      List.length compl = 1

    let%test "complement_stride_zero" =
      let r = ric (0l, Int 0l, Int 0l, ("", 5l)) in
      let compl = complement r in
      print_endline ("[complement of a RIC]     " ^ to_string r ^ " → [  " ^ String.concat ~sep:"; " (List.map compl ~f:to_string) ^ "  ]");
      List.length compl = 2

    let%test "complement_relative_stride_zero" =
      let r = ric (0l, Int 0l, Int 0l, ("x", 3l)) in
      let compl = complement r in
      print_endline ("[complement of a RIC]     " ^ to_string r ^ " → [  " ^ String.concat ~sep:"; " (List.map compl ~f:to_string) ^ "  ]");
      List.length compl = 2

    let%test "complement_nontrivial" =
      let r = ric (4l, Int 2l, Int 5l, ("a", 7l)) in
      let compl = complement r in
      print_endline ("[complement of a RIC]     " ^ to_string r ^ " → [  " ^ String.concat ~sep:"; " (List.map compl ~f:to_string) ^ "  ]");
      List.length compl = 5 

    let%test "complement of {2,4}" =
      let r = ric (2l, Int 0l, Int 1l, ("", 2l)) in
      let compl = complement r in
      print_endline ("[complement of a RIC]     " ^ to_string r ^ " → [  " ^ String.concat ~sep:"; " (List.map compl ~f:to_string) ^ "  ]");
      List.length compl = 3 

    let%test "add_offset_to_top" =
      let r = Top in
      let result = add_offset r 5l in
      print_endline ("[add offset to RIC]     ⊤ ⊞ 5 → " ^ to_string result);
      RIC.equal result Top

    let%test "add_offset_to_bottom" =
      let r = Bottom in
      let result = add_offset r 3l in
      print_endline ("[add offset to RIC]     ⊥ ⊞ 3 → " ^ to_string result);
      RIC.equal result Bottom

    let%test "add_offset_to_absolute" =
      let r = ric (4l, Int 0l, Int 2l, ("", 4l)) in
      let result = add_offset r 12l in
      print_endline ("[add offset to RIC]     (4[0, 2] + 4) ⊞ 12 → " ^ to_string result);
      RIC.equal result (ric (4l, Int 0l, Int 2l, ("", 16l)))

    let%test "add_offset_to_relative" =
      let r = ric (3l, Int 0l, Int 2l, ("x", 1l)) in
      let result = add_offset r (-2l) in
      print_endline ("[add offset to RIC]     (3[0, 2] + (x+1)) ⊞ (-2) → " ^ to_string result);
      RIC.equal result (ric (3l, Int 0l, Int 2l, ("x", -1l)))

    let%test "add_offset_to_stride_zero" =
      let r = ric (0l, Int 0l, Int 0l, ("", 5l)) in
      let result = add_offset r 10l in
      print_endline ("[add offset to RIC]     (0[0, 0] + 5) ⊞ 10 → " ^ to_string result);
      RIC.equal result (ric (0l, Int 0l, Int 0l, ("", 15l)))

    let%test "add_negative_offset_negInfinity" =
      let r = ric(4l, NegInfinity, Int 0l, ("a", 0l)) in
      let result = add_offset r (-1l) in
      print_endline ("[add offset to RIC]     (4[-∞, 0] + a) ⊞ (-1) → " ^ to_string result);
      RIC.equal result (ric(4l, NegInfinity, Int 0l, ("a", (-1l))))

    let%test "remove_lower_bound_top" =
      let r = Top in
      let result = remove_lower_bound r in
      print_endline ("[remove_lower_bound]     ⊤ → " ^ to_string result);
      RIC.equal result Top

    let%test "remove_lower_bound_bottom" =
      let r = Bottom in
      let result = remove_lower_bound r in
      print_endline ("[remove_lower_bound]     ⊥ → " ^ to_string result);
      RIC.equal result Bottom

    let%test "remove_lower_bound_regular" =
      let r = ric (3l, Int 2l, Int 5l, ("x", 4l)) in
      let result = remove_lower_bound r in
      print_endline ("[remove_lower_bound]     " ^ to_string r ^ " → " ^ to_string result);
      RIC.equal result (ric (1l, NegInfinity, Int 15l, ("x", 4l)))

    let%test "remove_upper_bound_top" =
      let r = Top in
      let result = remove_upper_bound r in
      print_endline ("[remove_upper_bound]     ⊤ → " ^ to_string result);
      RIC.equal result Top

    let%test "remove_upper_bound_bottom" =
      let r = Bottom in
      let result = remove_upper_bound r in
      print_endline ("[remove_upper_bound]     ⊥ → " ^ to_string result);
      RIC.equal result Bottom

    let%test "remove_upper_bound_regular" =
      let r = ric (2l, Int 1l, Int 4l, ("", 2l)) in
      let result = remove_upper_bound r in
      print_endline ("[remove_upper_bound]     " ^ to_string r ^ " → " ^ to_string result);
      RIC.equal result (ric (1l, Int 2l, Infinity, ("", 2l)))

    let%test "widen_top_and_any" =
      let r = ric (3l, Int 0l, Int 4l, ("", 5l)) in
      let w = widen Top ~relative_to:r in
      print_endline ("[RIC.widen]     ⊤ (" ^ to_string r ^ ") → " ^ to_string w);
      RIC.equal w Top

    let%test "widen_bottom_and_any" =
      let r = ric (3l, Int 0l, Int 4l, ("", 5l)) in
      let w = widen Bottom ~relative_to:r in
      print_endline ("[RIC.widen]      ⊥ (" ^ to_string r ^ ") → " ^ to_string w);
      RIC.equal w r

    let%test "widen_regular_to_infinite_upper" =
      let r1 = ric (4l, Int 0l, Int 1l, ("", 0l)) in
      let r2 = ric (4l, Int 0l, Int 2l, ("", 0l)) in
      let expected = ric (4l, Int 0l, Infinity, ("", 0l)) in
      let result = widen r1 ~relative_to:r2 in
      print_endline ("[RIC.widen]      (" ^ to_string r1 ^ ") (" ^ to_string r2 ^ ") → " ^ to_string result);
      RIC.equal result expected

    let%test "widen_extend_lower_bound" =
      let r1 = ric (2l, Int 1l, Int 4l, ("", 3l)) in
      let r2 = ric (2l, Int 0l, Int 4l, ("", 3l)) in
      let expected = ric (2l, NegInfinity, Int 4l, ("", 3l)) in
      let result = widen r1 ~relative_to:r2 in
      print_endline ("[RIC.widen]      (" ^ to_string r1 ^ ") (" ^ to_string r2 ^ ") → " ^ to_string result);
      RIC.equal result expected

    let%test "widen_shifted_relative_offsets" =
      let r1 = ric (3l, Int 1l, Int 3l, ("x", 7l)) in
      let r2 = ric (3l, Int 1l, Int 5l, ("x", 7l)) in
      let expected = ric (3l, Int 1l, Infinity, ("x", 7l)) in
      let result = widen r1 ~relative_to:r2 in
      print_endline ("[RIC.widen]      (" ^ to_string r1 ^ ") (" ^ to_string r2 ^ ") → " ^ to_string result);
      RIC.equal result expected

    let%test "widen_self" =
      let r = ric (4l, Int 0l, Int 3l, ("", 5l)) in
      let result = widen r ~relative_to:r in
      print_endline ("[RIC.widen]      (" ^ to_string r ^ ") (" ^ to_string r ^ ") → " ^ to_string result);
      RIC.equal result r

    let%test "widen_different_offsets" =
      let r1 = ric (20l, Int 0l, Int 3l, ("", 3l)) in
      let r2 = ric (20l, Int 0l, Int 3l, ("", 7l)) in
      let result = widen r1 ~relative_to:r2 in
      print_endline ("[RIC.widen]      (" ^ to_string r1 ^ ") (" ^ to_string r2 ^ ") → " ^ to_string result);
      RIC.equal result (ric (4l, Int 0l, Infinity, ("", 3l)))

    let%test "partially_accessed_by_singleton_size_2" =
      let r = ric(0l, Int 0l, Int 0l, ("", 8l)) in
      let p_accessed = partially_accessed ~by:r ~size:2l in
      let expected = [ric(0l, Int 0l, Int 0l, ("", 5l));
                      ric(0l, Int 0l, Int 0l, ("", 6l));
                      ric(0l, Int 0l, Int 0l, ("", 7l));
                      ric(0l, Int 0l, Int 0l, ("", 8l));
                      ric(0l, Int 0l, Int 0l, ("", 9l))] in
      print_endline ("[(size 2) partially_accessed]     " ^ to_string r ^ " → [  " ^ String.concat ~sep:"; " (List.map p_accessed ~f:to_string) ^ "  ]");
      List.equal RIC.equal p_accessed expected

    let%test "partially_accessed_by_singleton_size_4" =
      let r = ric(0l, Int 0l, Int 0l, ("", 8l)) in
      let p_accessed = partially_accessed ~by:r ~size:4l in
      let expected = [ric(0l, Int 0l, Int 0l, ("", 5l));
                      ric(0l, Int 0l, Int 0l, ("", 6l));
                      ric(0l, Int 0l, Int 0l, ("", 7l));
                      ric(0l, Int 0l, Int 0l, ("", 9l));
                      ric(0l, Int 0l, Int 0l, ("", 10l));
                      ric(0l, Int 0l, Int 0l, ("", 11l))] in
      print_endline ("[(size 4) partially_accessed]     " ^ to_string r ^ " → [  " ^ String.concat ~sep:"; " (List.map p_accessed ~f:to_string) ^ "  ]");
      List.equal RIC.equal p_accessed expected

    let%test "partially_accessed_by_RIC_size_1" =
      let r = ric(4l, Int 0l, Int 1l, ("", 8l)) in
      let p_accessed = partially_accessed ~by:r ~size:1l in
      let expected = [ric(4l, Int 0l, Int 1l, ("", 5l));
                      ric(4l, Int 0l, Int 1l, ("", 6l));
                      ric(4l, Int 0l, Int 1l, ("", 7l));
                      ric(4l, Int 0l, Int 1l, ("", 8l))] in
      print_endline ("[(size 1) partially_accessed]     " ^ to_string r ^ " → [  " ^ String.concat ~sep:"; " (List.map p_accessed ~f:to_string) ^ "  ]");
      List.equal RIC.equal p_accessed expected

    let%test "partially_accessed_by_RIC_size_2" =
      let r = ric(4l, Int 0l, Int 1l, ("", 8l)) in
      let p_accessed = partially_accessed ~by:r ~size:2l in
      let expected = [ric(4l, Int 0l, Int 1l, ("", 6l));
                      ric(4l, Int 0l, Int 1l, ("", 7l));
                      ric(4l, Int 0l, Int 1l, ("", 8l));
                      ric(4l, Int 0l, Int 2l, ("", 5l))] in
      print_endline ("[(size 2) partially_accessed]     " ^ to_string r ^ " → [  " ^ String.concat ~sep:"; " (List.map p_accessed ~f:to_string) ^ "  ]");
      List.equal RIC.equal p_accessed expected

    let%test "partially_accessed_by_RIC_size_3" =
      let r = ric(4l, Int 0l, Int 1l, ("", 8l)) in
      let p_accessed = partially_accessed ~by:r ~size:3l in
      let expected = [ric(4l, Int 0l, Int 1l, ("", 7l));
                      ric(4l, Int 0l, Int 1l, ("", 8l));
                      ric(4l, Int 0l, Int 2l, ("", 5l));
                      ric(4l, Int 0l, Int 2l, ("", 6l))] in
      print_endline ("[(size 3) partially_accessed]     " ^ to_string r ^ " → [  " ^ String.concat ~sep:"; " (List.map p_accessed ~f:to_string) ^ "  ]");
      List.equal RIC.equal p_accessed expected

    let%test "partially_accessed_by_RIC_size_5" =
      let r = ric(4l, Int 0l, Int 1l, ("", 8l)) in
      let p_accessed = partially_accessed ~by:r ~size:5l in
      let expected = [ric(4l, Int 0l, Int 2l, ("", 5l));
                      ric(4l, Int 0l, Int 2l, ("", 6l));
                      ric(4l, Int 0l, Int 2l, ("", 7l));
                      ric(4l, Int 0l, Int 2l, ("", 8l))] in
      print_endline ("[(size 5) partially_accessed]     " ^ to_string r ^ " → [  " ^ String.concat ~sep:"; " (List.map p_accessed ~f:to_string) ^ "  ]");
      List.equal RIC.equal p_accessed expected

    let%test "partially_accessed_by_RIC_size_2_stride_5" =
      let r = ric(5l, Int 0l, Int 1l, ("", 8l)) in
      let p_accessed = partially_accessed ~by:r ~size:2l in
      let expected = [ric(5l, Int 0l, Int 1l, ("", 5l));
                      ric(5l, Int 0l, Int 1l, ("", 6l));
                      ric(5l, Int 0l, Int 1l, ("", 7l));
                      ric(5l, Int 0l, Int 1l, ("", 8l));
                      ric(5l, Int 0l, Int 1l, ("", 9l))] in
      print_endline ("[(size 2 stride 5) partially_accessed]     " ^ to_string r ^ " → [  " ^ String.concat ~sep:"; " (List.map p_accessed ~f:to_string) ^ "  ]");
      List.equal RIC.equal p_accessed expected

    let%test "partially_accessed_by_RIC_size_2_stride_6" =
      let r = ric(6l, Int 0l, Int 1l, ("", 8l)) in
      let p_accessed = partially_accessed ~by:r ~size:2l in
      let expected = [ric(6l, Int 0l, Int 1l, ("", 5l));
                      ric(6l, Int 0l, Int 1l, ("", 6l));
                      ric(6l, Int 0l, Int 1l, ("", 7l));
                      ric(6l, Int 0l, Int 1l, ("", 8l));
                      ric(6l, Int 0l, Int 1l, ("", 9l))] in
      print_endline ("[(size 2 stride 6) partially_accessed]     " ^ to_string r ^ " → [  " ^ String.concat ~sep:"; " (List.map p_accessed ~f:to_string) ^ "  ]");
      List.equal RIC.equal p_accessed expected

    let%test "partially_accessed_by_RIC_size_2_stride_7" =
      let r = ric(7l, Int 0l, Int 1l, ("", 8l)) in
      let p_accessed = partially_accessed ~by:r ~size:2l in
      let expected = [ric(7l, Int 0l, Int 1l, ("", 5l));
                      ric(7l, Int 0l, Int 1l, ("", 6l));
                      ric(7l, Int 0l, Int 1l, ("", 7l));
                      ric(7l, Int 0l, Int 1l, ("", 8l));
                      ric(7l, Int 0l, Int 1l, ("", 9l))] in
      print_endline ("[(size 2 stride 7) partially_accessed]     " ^ to_string r ^ " → [  " ^ String.concat ~sep:"; " (List.map p_accessed ~f:to_string) ^ "  ]");
      List.equal RIC.equal p_accessed expected

    let%test "partially_accessed_by_RIC_size_3_stride_2" =
      let r = ric(2l, Int 0l, Int 1l, ("", 8l)) in
      let p_accessed = partially_accessed ~by:r ~size:3l in
      let expected = [ric(2l, Int 0l, Int 3l, ("", 5l));
                      ric(2l, Int 0l, Int 3l, ("", 6l))] in
      print_endline ("[(size 3 stride 2) partially_accessed]     " ^ to_string r ^ " → [  " ^ String.concat ~sep:"; " (List.map p_accessed ~f:to_string) ^ "  ]");
      List.equal RIC.equal p_accessed expected

    let%test "partially_accessed_by_RIC_size_2_stride_2" =
      let r = ric(2l, Int 0l, Int 1l, ("", 8l)) in
      let p_accessed = partially_accessed ~by:r ~size:2l in
      let expected = [ric(2l, Int 0l, Int 2l, ("", 6l));
                      ric(2l, Int 0l, Int 3l, ("", 5l))] in
      print_endline ("[(size 2 stride 2) partially_accessed]     " ^ to_string r ^ " → [  " ^ String.concat ~sep:"; " (List.map p_accessed ~f:to_string) ^ "  ]");
      List.equal RIC.equal p_accessed expected

    let%test "partially_accessed_by_RIC_size_4_stride_2" =
      let r = ric(2l, Int 0l, Int 1l, ("", 8l)) in
      let p_accessed = partially_accessed ~by:r ~size:4l in
      let expected = [ric(2l, Int 0l, Int 3l, ("", 6l));
                      ric(2l, Int 0l, Int 4l, ("", 5l))] in
      print_endline ("[(size 4 stride 2) partially_accessed]     " ^ to_string r ^ " → [  " ^ String.concat ~sep:"; " (List.map p_accessed ~f:to_string) ^ "  ]");
      List.equal RIC.equal p_accessed expected

    let%test "partially_accessed_by_RIC_size_1_stride_2" =
      let r = ric(2l, Int 0l, Int 1l, ("", 8l)) in
      let p_accessed = partially_accessed ~by:r ~size:1l in
      let expected = [ric(2l, Int 0l, Int 2l, ("", 5l));
                      ric(2l, Int 0l, Int 2l, ("", 6l))] in
      print_endline ("[(size 1 stride 2) partially_accessed]     " ^ to_string r ^ " → [  " ^ String.concat ~sep:"; " (List.map p_accessed ~f:to_string) ^ "  ]");
      List.equal RIC.equal p_accessed expected

    let%test "partially_accessed_by_RIC_size4_bigger_than_stride3" =
      let r = ric(3l, Int 0l, Int 1l, ("", 8l)) in
      let p_accessed = partially_accessed ~by:r ~size:4l in
      let expected = [ric(3l, Int 0l, Int 2l, ("", 6l));
                      ric(3l, Int 0l, Int 2l, ("", 7l));
                      ric(3l, Int 0l, Int 3l, ("", 5l))] in
      print_endline ("[(size 4 stride 3) partially_accessed]     " ^ to_string r ^ " → [  " ^ String.concat ~sep:"; " (List.map p_accessed ~f:to_string) ^ "  ]");
      List.equal RIC.equal p_accessed expected

    let%test "partially_accessed_by_RIC_size4_bigger_than_stride2" =
      let r = ric(2l, Int 0l, Int 2l, ("", 8l)) in
      let p_accessed = partially_accessed ~by:r ~size:4l in
      let expected = [ric(2l, Int 0l, Int 4l, ("", 6l));
                      ric(2l, Int 0l, Int 5l, ("", 5l))] in
      print_endline ("[(size 4 stride 2) partially_accessed]     " ^ to_string r ^ " → [  " ^ String.concat ~sep:"; " (List.map p_accessed ~f:to_string) ^ "  ]");
      List.equal RIC.equal p_accessed expected

    let%test "partially_accessed_by_Top_size_2" =
      let r = Top in
      let p_accessed = partially_accessed ~by:r ~size:2l in
      let expected = [Top] in
      print_endline ("[(size 2) partially_accessed]     " ^ to_string r ^ " → [  " ^ String.concat ~sep:"; " (List.map p_accessed ~f:to_string) ^ "  ]");
      List.equal RIC.equal p_accessed expected

    let%test "partially_accessed_by_Top_size_4" =
      let r = Top in
      let p_accessed = partially_accessed ~by:r ~size:4l in
      let expected = [RIC.Top] in
      print_endline ("[(size 4) partially_accessed]     " ^ to_string r ^ " → [  " ^ String.concat ~sep:"; " (List.map p_accessed ~f:to_string) ^ "  ]");
      List.equal RIC.equal p_accessed expected

    let%test "partially_accessed_by_RIC_size_4_stride_4" =
      let r = ric(4l, Int 0l, Int 1l, ("", 8l)) in
      let p_accessed = partially_accessed ~by:r ~size:4l in
      let expected = [ric(4l, Int 0l, Int 2l, ("", 5l));
                      ric(4l, Int 0l, Int 2l, ("", 6l));
                      ric(4l, Int 0l, Int 2l, ("", 7l))] in
      print_endline ("[(size 4 stride 4) partially_accessed]     " ^ to_string r ^ " → [  " ^ String.concat ~sep:"; " (List.map p_accessed ~f:to_string) ^ "  ]");
      List.equal RIC.equal p_accessed expected

    let%test "partially_accessed_by_RIC_size_4_stride_5" =
      let r = ric(5l, Int 0l, Int 1l, ("", 8l)) in
      let p_accessed = partially_accessed ~by:r ~size:4l in
      let expected = [ric(5l, Int 0l, Int 1l, ("", 7l));
                      ric(5l, Int 0l, Int 1l, ("", 9l));
                      ric(5l, Int 0l, Int 2l, ("", 5l));
                      ric(5l, Int 0l, Int 2l, ("", 6l))] in
      print_endline ("[(size 4 stride 5) partially_accessed]     " ^ to_string r ^ " → [  " ^ String.concat ~sep:"; " (List.map p_accessed ~f:to_string) ^ "  ]");
      List.equal RIC.equal p_accessed expected


    let%test "partially_accessed_by_4[10,inf[_size_4" =
      let r = ric(4l,Int 0l, Infinity, ("a", 10l)) in
      let p_accessed = partially_accessed ~by:r ~size:4l in
      let expected = [ric(4l, Int 0l, Infinity, ("a", 7l));
                      ric(4l, Int 0l, Infinity, ("a", 8l));
                      ric(4l, Int 0l, Infinity, ("a", 9l))] in
      print_endline ("[(size 4 stride 4 +inf) partially_accessed]     " ^ to_string r ^ " → [  " ^ String.concat ~sep:"; " (List.map p_accessed ~f:to_string) ^ "  ]");
      List.equal RIC.equal p_accessed expected

    let%test "partially_accessed_by_4[10,inf[_size_5" =
      let r = ric(4l,Int 0l, Infinity, ("a", 10l)) in
      let p_accessed = partially_accessed ~by:r ~size:5l in
      let expected = [ric(4l, Int 0l, Infinity, ("a", 7l));
                      ric(4l, Int 0l, Infinity, ("a", 8l));
                      ric(4l, Int 0l, Infinity, ("a", 9l));
                      ric(4l, Int 0l, Infinity, ("a", 10l))] in
      print_endline ("[(size 5 stride 4 +inf) partially_accessed]     " ^ to_string r ^ " → [  " ^ String.concat ~sep:"; " (List.map p_accessed ~f:to_string) ^ "  ]");
      List.equal RIC.equal p_accessed expected


    let%test "partially_accessed_by_4[10,inf[_size_3" =
      let r = ric(4l,Int 0l, Infinity, ("a", 10l)) in
      let p_accessed = partially_accessed ~by:r ~size:3l in
      let expected = [ric(4l, Int 0l, Infinity, ("a", 7l));
                      ric(4l, Int 0l, Infinity, ("a", 8l));
                      ric(4l, Int 0l, Infinity, ("a", 9l));
                      ric(4l, Int 0l, Infinity, ("a", 10l))] in
      print_endline ("[(size 3 stride 4 +inf) partially_accessed]     " ^ to_string r ^ " → [  " ^ String.concat ~sep:"; " (List.map p_accessed ~f:to_string) ^ "  ]");
      List.equal RIC.equal p_accessed expected


    let%test "partially_accessed_by_4[10,inf[_size_1" =
      let r = ric(7l,Int 0l, Infinity, ("a", 10l)) in
      let p_accessed = partially_accessed ~by:r ~size:1l in
      let expected = [ric(7l, Int 0l, Infinity, ("a", 7l));
                      ric(7l, Int 0l, Infinity, ("a", 8l));
                      ric(7l, Int 0l, Infinity, ("a", 9l));
                      ric(7l, Int 0l, Infinity, ("a", 10l))] in
      print_endline ("[(size 1 stride 7  +inf) partially_accessed]     " ^ to_string r ^ " → [  " ^ String.concat ~sep:"; " (List.map p_accessed ~f:to_string) ^ "  ]");
      List.equal RIC.equal p_accessed expected


    let%test "partially_accessed_by_4[10,inf[_size_2" =
      let r = ric(7l,Int 0l, Infinity, ("a", 10l)) in
      let p_accessed = partially_accessed ~by:r ~size:2l in
      let expected = [ric(7l, Int 0l, Infinity, ("a", 7l));
                      ric(7l, Int 0l, Infinity, ("a", 8l));
                      ric(7l, Int 0l, Infinity, ("a", 9l));
                      ric(7l, Int 0l, Infinity, ("a", 10l));
                      ric(7l, Int 0l, Infinity, ("a", 11l))] in
      print_endline ("[(size 2 stride 7  +inf) partially_accessed]     " ^ to_string r ^ " → [  " ^ String.concat ~sep:"; " (List.map p_accessed ~f:to_string) ^ "  ]");
      List.equal RIC.equal p_accessed expected

    let%test "partially_accessed_by_4[10,inf[_size_4" =
      let r = ric(7l,Int 0l, Infinity, ("a", 10l)) in
      let p_accessed = partially_accessed ~by:r ~size:4l in
      let expected = [ric(7l, Int 0l, Infinity, ("a", 7l));
                      ric(7l, Int 0l, Infinity, ("a", 8l));
                      ric(7l, Int 0l, Infinity, ("a", 9l));
                      ric(7l, Int 0l, Infinity, ("a", 11l));
                      ric(7l, Int 0l, Infinity, ("a", 12l));
                      ric(7l, Int 0l, Infinity, ("a", 13l))] in
      print_endline ("[(size 4 stride 7  +inf) partially_accessed]     " ^ to_string r ^ " → [  " ^ String.concat ~sep:"; " (List.map p_accessed ~f:to_string) ^ "  ]");
      List.equal RIC.equal p_accessed expected

    let%test "partially_accessed_by_4]-inf,0]_size_4" =
      let r = ric(4l, NegInfinity, Int 0l, ("a", 0l)) in
      let p_accessed = partially_accessed ~by:r ~size:4l in
      let expected = [ric(4l, NegInfinity, Int 0l, ("a", 1l));
                      ric(4l, NegInfinity, Int 0l, ("a", 2l));
                      ric(4l, NegInfinity, Int 0l, ("a", 3l))] in
      print_endline ("[(size 4 stride 4 -inf) partially_accessed]     " ^ to_string r ^ " → [  " ^ String.concat ~sep:"; " (List.map p_accessed ~f:to_string) ^ "  ]");
      List.equal RIC.equal p_accessed expected

    let%test "partially_accessed_by_4]-inf,0]_size_5" =
      let r = ric(4l,NegInfinity , Int 0l, ("a", 0l)) in
      let p_accessed = partially_accessed ~by:r ~size:5l in
      let expected = [ric(4l, NegInfinity, Int 0l, ("a", 1l));
                      ric(4l, NegInfinity, Int 0l, ("a", 2l));
                      ric(4l, NegInfinity, Int 0l, ("a", 3l));
                      ric(4l, NegInfinity, Int 0l, ("a", 4l))] in
      print_endline ("[(size 5 stride 4 -inf) partially_accessed]     " ^ to_string r ^ " → [  " ^ String.concat ~sep:"; " (List.map p_accessed ~f:to_string) ^ "  ]");
      (* print_endline (String.concat ~sep:"; " (List.map expected ~f:to_string)); *)
      List.equal RIC.equal p_accessed expected

    let%test "partially_accessed_by_4]-inf,0]_size_6" =
      let r = ric(4l,NegInfinity , Int 0l, ("a", 0l)) in
      let p_accessed = partially_accessed ~by:r ~size:6l in
      let expected = [ric(4l, NegInfinity, Int 0l, ("a", 2l));
                      ric(4l, NegInfinity, Int 0l, ("a", 3l));
                      ric(4l, NegInfinity, Int 0l, ("a", 4l));
                      ric(4l, NegInfinity, Int 0l, ("a", 5l))] in
      print_endline ("[(size 6 stride 4 -inf) partially_accessed]     " ^ to_string r ^ " → [  " ^ String.concat ~sep:"; " (List.map p_accessed ~f:to_string) ^ "  ]");
      List.equal RIC.equal p_accessed expected


    let%test "partially_accessed_by_4]-inf,0]_size_3" =
      let r = ric(4l, NegInfinity, Int 0l, ("a", 0l)) in
      let p_accessed = partially_accessed ~by:r ~size:3l in
      let expected = [ric(4l, NegInfinity, Int 0l, ("a", -1l));
                      ric(4l, NegInfinity, Int 0l, ("a", 0l));
                      ric(4l, NegInfinity, Int 0l, ("a", 1l));
                      ric(4l, NegInfinity, Int 0l, ("a", 2l))] in
      print_endline ("[(size 3 stride 4 -inf) partially_accessed]     " ^ to_string r ^ " → [  " ^ String.concat ~sep:"; " (List.map p_accessed ~f:to_string) ^ "  ]");
      List.equal RIC.equal p_accessed expected


    let%test "partially_accessed_by_7]-inf,0]_size_1" =
      let r = ric(7l, NegInfinity, Int 0l, ("a", 0l)) in
      let p_accessed = partially_accessed ~by:r ~size:1l in
      let expected = [ric(7l, NegInfinity, Int 0l, ("a", -3l));
                      ric(7l, NegInfinity, Int 0l, ("a", -2l));
                      ric(7l, NegInfinity, Int 0l, ("a", -1l));
                      ric(7l, NegInfinity, Int 0l, ("a", 0l))] in
      print_endline ("[(size 1 stride 7  -inf) partially_accessed]     " ^ to_string r ^ " → [  " ^ String.concat ~sep:"; " (List.map p_accessed ~f:to_string) ^ "  ]");
      List.equal RIC.equal p_accessed expected


    let%test "partially_accessed_by_7]-inf,0]_size_2" =
      let r = ric(7l,NegInfinity, Int 0l, ("a", 0l)) in
      let p_accessed = partially_accessed ~by:r ~size:2l in
      let expected = [ric(7l,NegInfinity, Int 0l, ("a", -3l));
                      ric(7l,NegInfinity, Int 0l, ("a", -2l));
                      ric(7l,NegInfinity, Int 0l, ("a", -1l));
                      ric(7l,NegInfinity, Int 0l, ("a", 0l));
                      ric(7l,NegInfinity, Int 0l, ("a", 1l))] in
      print_endline ("[(size 2 stride 7  -inf) partially_accessed]     " ^ to_string r ^ " → [  " ^ String.concat ~sep:"; " (List.map p_accessed ~f:to_string) ^ "  ]");
      List.equal RIC.equal p_accessed expected

    let%test "partially_accessed_by_7]-inf,0]_size_4" =
      let r = ric(7l, NegInfinity, Int 0l, ("a", 0l)) in
      let p_accessed = partially_accessed ~by:r ~size:4l in
      let expected = [ric(7l, NegInfinity, Int 0l, ("a", -3l));
                      ric(7l, NegInfinity, Int 0l, ("a", -2l));
                      ric(7l, NegInfinity, Int 0l, ("a", -1l));
                      ric(7l, NegInfinity, Int 0l, ("a", 1l));
                      ric(7l, NegInfinity, Int 0l, ("a", 2l));
                      ric(7l, NegInfinity, Int 0l, ("a", 3l))] in
      print_endline ("[(size 4 stride 7  -inf) partially_accessed]     " ^ to_string r ^ " → [  " ^ String.concat ~sep:"; " (List.map p_accessed ~f:to_string) ^ "  ]");
      List.equal RIC.equal p_accessed expected

    let%test "RIC.plus: absolute + absolute" =
      let r1 = ric (4l, Int 0l, Int 2l, ("", 1l)) in
      let r2 = ric (2l, Int 1l, Int 3l, ("", 3l)) in
      let result = plus r1 r2 in
      let expected = ric (2l, Int 1l, Int 7l, ("", 4l)) in
      print_endline ("[RIC.plus]     (" ^ to_string r1 ^ ") ⊕ (" ^ to_string r2 ^ ") = " ^ to_string result);
      RIC.equal result expected

    let%test "RIC.plus: relative + relative same var" =
      let r1 = ric (3l, Int 0l, Int 2l, ("x", 4l)) in
      let r2 = ric (6l, Int 1l, Int 3l, ("x", 1l)) in
      let result = plus r1 r2 in
      let expected = ric (3l, Int 0l, Int 6l, ("x+x", 11l)) in
      print_endline ("[RIC.plus]     (" ^ to_string r1 ^ ") ⊕ (" ^ to_string r2 ^ ") = " ^ to_string result);
      RIC.equal result expected

    let%test "RIC.plus: absolute + relative" =
      let r1 = ric (3l, Int 0l, Int 2l, ("", 1l)) in
      let r2 = ric (2l, Int 1l, Int 2l, ("x", 4l)) in
      let result = plus r1 r2 in
      let expected = ric(1l, Int 0l, Int 8l, ("x",7l)) in 
      print_endline ("[RIC.plus]     (" ^ to_string r1 ^ ") ⊕ (" ^ to_string r2 ^ ") = " ^ to_string result);
      RIC.equal result expected

    let%test "RIC.plus: relative + relative different vars" =
      let r1 = ric (3l, Int 0l, Int 2l, ("x", 1l)) in
      let r2 = ric (2l, Int 1l, Int 3l, ("y", 2l)) in
      let result = plus r1 r2 in
      let expected = ric (1l, Int 0l, Int 10l, ("x+y", 5l)) in 
      print_endline ("[RIC.plus]     (" ^ to_string r1 ^ ") ⊕ (" ^ to_string r2 ^ ") = " ^ to_string result);
      RIC.equal result expected

    let%test "RIC.plus: Top + anything = Top" =
      let r = ric (3l, Int 0l, Int 2l, ("", 4l)) in
      let result = plus Top r in
      print_endline ("[RIC.plus]     ⊤ ⊕ " ^ to_string r ^ " = " ^ to_string result);
      RIC.equal result Top

    let%test "RIC.plus: Bottom + anything = anything" =
      let r = ric (3l, Int 0l, Int 2l, ("", 4l)) in
      let result = plus Bottom r in
      print_endline ("[RIC.plus]     ⊥ ⊕ " ^ to_string r ^ " = " ^ to_string result);
      RIC.equal result r

    let%test "RIC.plus: singleton + singleton" =
      let r1 = ric (0l, Int 0l, Int 0l, ("", 7l)) in
      let r2 = ric (0l, Int 0l, Int 0l, ("", 3l)) in
      let result = plus r1 r2 in
      let expected = ric (0l, Int 0l, Int 0l, ("", 10l)) in
      print_endline ("[RIC.plus]     (7) ⊕ (3) = " ^ to_string result);
      RIC.equal result expected

    let%test "RIC.plus: stride propagation with normalization" =
      let r1 = ric (4l, Int 1l, Int 2l, ("", 0l)) in
      let r2 = ric (2l, Int 0l, Int 1l, ("", 2l)) in
      let result = plus r1 r2 in
      let expected = ric (2l, Int 0l, Int 3l, ("", 6l)) in
      print_endline ("[RIC.plus]     (4[1,2]) ⊕ (2[0,1]+2) = " ^ to_string result);
      RIC.equal result expected

    let%test "RIC.plus: relative + relative same variable string" =
      let r1 = ric (4l, Int 1l, Int 2l, ("x", 0l)) in
      let r2 = ric (2l, Int 0l, Int 1l, ("x", 2l)) in
      let result = plus r1 r2 in
      let expected = ric (2l, Int 0l, Int 3l, ("x+x", 6l)) in
      print_endline ("[RIC.plus]     (4[1,2]+x) ⊕ (2[0,1]+(x+2)) = " ^ to_string result);
      RIC.equal result expected

    let%test "RIC.plus: one Top one Bottom = Top" =
      let result = plus Top Bottom in
      print_endline ("[RIC.plus]     ⊤ ⊕ ⊥ = " ^ to_string result);
      RIC.equal result Top

    let%test "RIC.plus: both Top = Top" =
      let result = plus Top Top in
      print_endline ("[RIC.plus]     ⊤ ⊕ ⊤ = " ^ to_string result);
      RIC.equal result Top

    let%test "RIC.plus: infinite boundary 1" =
      let r1 = ric (4l, Int 0l, Infinity, ("", 3l)) in
      let r2 = ric (6l, Int 0l, Int 2l, ("", 2l)) in
      let result = plus r1 r2 in
      let expected = ric (2l, Int 0l, Infinity, ("", 5l)) in
      print_endline ("[RIC.plus]     (" ^ to_string r1 ^ ") ⊕ (" ^ to_string r2 ^ ") = " ^ to_string result);
      RIC.equal result expected

    let%test "remove_top_from_top" =
      let ric1 = Top in
      let ric2 = Top in
      let result = remove ~this:ric1 ~from:ric2 in
      print_endline ("[RIC.remove]     " ^ to_string ric2 ^ " \\ " ^ to_string ric1 ^ " = [" ^ String.concat ~sep:"; " (List.map result ~f:to_string) ^ "]");
      List.equal equal result []

    let%test "remove_bottom_from_top" =
      let ric1 = Bottom in
      let ric2 = Top in
      let result = remove ~this:ric1 ~from:ric2 in
      print_endline ("[RIC.remove]     " ^ to_string ric2 ^ " \\ " ^ to_string ric1 ^ " = [" ^ String.concat ~sep:"; " (List.map result ~f:to_string) ^ "]");
      List.equal equal result [Top]

    let%test "remove_top_from_bottom" =
      let ric1 = Top in
      let ric2 = Bottom in
      let result = remove ~this:ric1 ~from:ric2 in
      print_endline ("[RIC.remove]     " ^ to_string ric2 ^ " \\ " ^ to_string ric1 ^ " = [" ^ String.concat ~sep:"; " (List.map result ~f:to_string) ^ "]");
      List.equal equal result []

    let%test "remove_bottom_from_bottom" =
      let ric1 = Bottom in
      let ric2 = Bottom in
      let result = remove ~this:ric1 ~from:ric2 in
      print_endline ("[RIC.remove]     " ^ to_string ric2 ^ " \\ " ^ to_string ric1 ^ " = [" ^ String.concat ~sep:"; " (List.map result ~f:to_string) ^ "]");
      List.equal equal result []

    let%test "remove_singleton_from_itself" =
      let ric1 = ric (0l, Int 0l, Int 0l, ("", 5l)) in
      let ric2 = ric1 in
      let result = remove ~this:ric1 ~from:ric2 in
      print_endline ("[RIC.remove]     " ^ to_string ric2 ^ " \\ " ^ to_string ric1 ^ " = [" ^ String.concat ~sep:"; " (List.map result ~f:to_string) ^ "]");
      List.equal equal result []

    let%test "remove_singleton_not_in" =
      let ric1 = ric (0l, Int 0l, Int 0l, ("", 9l)) in
      let ric2 = ric (2l, Int 0l, Int 3l, ("", 1l)) in
      let result = remove ~this:ric1 ~from:ric2 in
      print_endline ("[RIC.remove]     " ^ to_string ric2 ^ " \\ " ^ to_string ric1 ^ " = [" ^ String.concat ~sep:"; " (List.map result ~f:to_string) ^ "]");
      List.equal equal result [ric2]

    let%test "remove_partial_overlap" =
      let ric1 = ric (2l, Int 0l, Int 2l, ("", 1l)) in
      let ric2 = ric (2l, Int 0l, Int 4l, ("", 1l)) in
      let result = remove ~this:ric1 ~from:ric2 in
      print_endline ("[RIC.remove]     " ^ to_string ric2 ^ " \\ " ^ to_string ric1 ^ " = [" ^ String.concat ~sep:"; " (List.map result ~f:to_string) ^ "]");
      List.equal equal result [ric (2l, Int 3l, Int 4l, ("", 1l))]

    let%test "remove_entire_range" =
      let ric1 = ric (2l, Int 0l, Int 4l, ("", 1l)) in
      let ric2 = ric1 in
      let result = remove ~this:ric1 ~from:ric2 in
      print_endline ("[RIC.remove]     " ^ to_string ric2 ^ " \\ " ^ to_string ric1 ^ " = [" ^ String.concat ~sep:"; " (List.map result ~f:to_string) ^ "]");
      List.equal equal result []

    let%test "remove_smaller_range" =
      let ric1 = ric (2l, Int 0l, Int 2l, ("", 1l)) in
      let ric2 = ric (2l, Int 0l, Int 4l, ("", 1l)) in
      let result = remove ~this:ric1 ~from:ric2 in
      print_endline ("[RIC.remove]     " ^ to_string ric2 ^ " \\ " ^ to_string ric1 ^ " = [" ^ String.concat ~sep:"; " (List.map result ~f:to_string) ^ "]");
      List.equal equal result [ric (2l, Int 3l, Int 4l, ("", 1l))]

    let%test "remove_different_relative_offsets" =
      let ric1 = ric (2l, Int 0l, Int 2l, ("", 2l)) in
      let ric2 = ric (2l, Int 0l, Int 4l, ("x", 1l)) in
      let result = remove ~this:ric1 ~from:ric2 in
      print_endline ("[RIC.remove]     " ^ to_string ric2 ^ " \\ " ^ to_string ric1 ^ " = [" ^ String.concat ~sep:"; " (List.map result ~f:to_string) ^ "]");
      List.equal equal result []

    let%test "remove_with_relative_offsets" =
      let ric1 = ric (3l, Int 1l, Int 2l, ("x", 1l)) in
      let ric2 = ric (3l, Int 0l, Int 4l, ("x", 1l)) in
      let result = remove ~this:ric1 ~from:ric2 in
      print_endline ("[RIC.remove]     " ^ to_string ric2 ^ " \\ " ^ to_string ric1 ^ " = [" ^ String.concat ~sep:"; " (List.map result ~f:to_string) ^ "]");
      List.equal equal result [ric (3l, Int 0l, Int 0l, ("x", 1l)); ric (3l, Int 3l, Int 4l, ("x", 1l))]

    let%test "remove_full_complement_overlap" =
      let this = ric (2l, Int 0l, Int 2l, ("", 1l)) in
      let from = ric (1l, Int 0l, Int 6l, ("", 1l)) in
      let result = remove ~this ~from in
      let expected = [ric (2l, Int 0l, Int 2l, ("", 2l)); ric (0l, Int 0l, Int 0l, ("", 7l))] in
      print_endline ("[RIC.remove]     " ^ to_string from ^ " \\ " ^ to_string this ^ " = [" ^ String.concat ~sep:"; " (List.map result ~f:to_string) ^ "]");
      List.equal equal result expected

    let%test "remove {2,4} from {0,2,4,6,8}" =
      let this = ric(2l, Int 1l, Int 2l, ("", 0l)) in
      let from = ric(2l, Int 0l, Int 4l, ("", 0l)) in
      let result = remove ~this ~from in
      let expected = [ric(0l,Int 0l, Int 0l, ("", 0l)); ric (2l, Int 0l, Int 1l, ("", 6l))] in
      print_endline ("[RIC.remove]     " ^ to_string from ^ " \\ " ^ to_string this ^ " = [" ^ String.concat ~sep:"; " (List.map result ~f:to_string) ^ "]");
      List.equal equal result expected

    let%test "2[0,2]+1024 to bitfield" =
      let r = ric(2l, Int 0l, Int 2l, ("", 1024l)) in
      let bf = to_bitfield r in
      let bf_string = Bitfield.to_string bf in
      print_endline ("[2[0,2]+1024 to bitfield]\t" ^ bf_string);
      String.equal "00000000000000000000010000000::0" bf_string

    let%test "3[0,2]+32 to bitfield" =
      let r = ric(3l, Int 0l, Int 2l, ("", 32l)) in
      let bf = to_bitfield r in
      let bf_string = Bitfield.to_string bf in
      print_endline bf_string;
      String.equal "00000000000000000000000000100:::" bf_string

    let%test "2[0,2]+2 to bitfield" =
      let r = ric(2l, Int 0l, Int 2l, ("", 2l)) in
      let bf = to_bitfield r in
      let bf_string = Bitfield.to_string bf in
      String.equal "00000000000000000000000000000::0" bf_string

    let%test "2[0,3]+2 to bitfield" =
      let r = ric(2l, Int 0l, Int 3l, ("", 2l)) in
      let bf = to_bitfield r in
      let bf_string = Bitfield.to_string bf in
      String.equal "0000000000000000000000000000:::0" bf_string


    let%test "to_bitfield then of_bitfield 2[0,2] + 1024 = 2[0,3] + 1024" =
      let r = ric(2l, Int 0l, Int 2l, ("", 1024l)) in
      let bf = to_bitfield r in
      let result = of_bitfield bf in
      equal result (ric (2l, Int 0l, Int 3l, ("", 1024l)))

    let%test "to_bitfield then of_bitfield 3[0,2] + 32 = [0,7] + 32" =
      let r = ric(3l, Int 0l, Int 2l, ("", 32l)) in
      let bf = to_bitfield r in
      let result = of_bitfield bf in
      equal result (ric (1l, Int 0l, Int 7l, ("", 32l)))

    let%test "to_bitfield then of_bitfield 2[0,2] + 2 = 2[0,3]" =
      let r = ric(2l, Int 0l, Int 2l, ("", 2l)) in
      let bf = to_bitfield r in
      let result = of_bitfield bf in
      equal result (ric (2l, Int 0l, Int 3l, ("", 0l)))

    let%test "to_bitfield then of_bitfield 2[0,3] + 2 = 2[0,7]" =
      let r = ric(2l, Int 0l, Int 3l, ("", 2l)) in
      let bf = to_bitfield r in
      let result = of_bitfield bf in
      equal result (ric (2l, Int 0l, Int 7l, ("", 0l)))

    let%test "meet negative_integers" =
      let ric1 = ric (2l, Int (-10l), Int 10l, ("", 0l)) in
      let m = meet negative_integers ric1 in
      print_endline ("negative_integers: " ^ to_string negative_integers ^ " meet: " ^ to_string m);
      equal m (ric (2l, Int (-10l), Int (-1l), ("", 0l)))

    let%test "16 and Top" =
      let ric16 = ric(0l, Int 0l, Int 0l, ("", 16l)) in
      let a = bitwise_and ric16 RIC.Top in
      equal a (ric(16l, Int 0l, Int 1l, ("", 0l)))

    let%test "negative part of 16Z" =
      let ric16Z = ric(16l, NegInfinity, Infinity, ("", 0l)) in
      let n = negative_part ric16Z in
      print_endline (to_string n);
      let bf = to_bitfield n in
      print_endline (Bitfield.to_string bf);
      match bf with
      | Bit {zeros = 0b1111111111111111111111111111111l; ones = 0b11111111111111111111111111110000l} -> true
      | _ -> false


    let%test "common_msb" =
      let x = 0b00100110110011001100l 
      and y = 0b00100110110101001100l in
      let result, n = common_msb x y in
      print_endline (string_of_int n ^ "    " ^ Bitfield.to_string result);
      match result, n with
      | Bit {zeros = 0b11111111111111111111111011001001l; ones = 0b00100110110l}, 9 -> true
      | _ -> false


    let%test "big RIC conversion" =
      let r = ric (4l, Int 0l, Int 134217727l, ("", 3l)) in
      let bf = to_bitfield r in
      print_endline (to_string r ^ "   ---to-bitfield--->   " ^ Bitfield.to_string bf);
      let bf2 = Bitfield.shift_right_unsigned bf Bitfield.one in
      print_endline ("\t>> 1 :\t" ^ Bitfield.to_string bf2);
      let ric2 = of_bitfield bf2 in
      print_endline ("\tBack to RIC: " ^ to_string ric2);
      true

    let%test "big RIC conversion 2" =
      let r = ric (1l, Int 0l, Int 268435485l, ("", 1l)) in
      let bf = to_bitfield r in
      print_endline (to_string r ^ "   ---to-bitfield--->   " ^ Bitfield.to_string bf);
      let bf2 = Bitfield.shift_left bf Bitfield.one in
      let bf2 = Bitfield.shift_left bf2 Bitfield.one in
      let bf2 = Bitfield.shift_left bf2 Bitfield.one in
      print_endline ("\t<< 3 :\t" ^ Bitfield.to_string bf2);
      let ric2 = of_bitfield bf2 in
      print_endline ("\tBack to RIC: " ^ to_string ric2);
      true

    let%test "Top and Top" =
      let top_and_top = bitwise_and Top Top in
      print_endline ("Top & Top = " ^ to_string top_and_top);
      let bf_top_pos = to_bitfield (meet Top positive_integers) in
      print_endline ("Top+   ---to-bitfield--->   " ^ Bitfield.to_string bf_top_pos);
      let bf_top_neg = to_bitfield (meet Top negative_integers) in
      print_endline ("Top-   ---to-bitfield--->   " ^ Bitfield.to_string bf_top_neg);
      let and1 = Bitfield.and_ bf_top_pos bf_top_pos
      and and2 = Bitfield.and_ bf_top_pos bf_top_neg
      and and3 = Bitfield.and_ bf_top_neg bf_top_neg
      in
      print_endline (Bitfield.to_string and1);
      print_endline (Bitfield.to_string and2);
      print_endline (Bitfield.to_string and3);
      let and1 = of_bitfield and1
      and and2 = of_bitfield and2
      and and3 = of_bitfield and3
      in
      print_endline ("and1 = " ^ to_string and1 ^ "\tand2 = " ^ to_string and2 ^ "\tand3 = " ^ to_string and3);
      let and_result = join (join and1 and2) and3 in
      let _ = (match and3 with
      | RIC {offset = ("", o); _} -> print_endline ("offset= " ^ Int32.to_string o);
      | _ -> ();) in
      print_endline ("Top and Top = " ^ to_string and_result);
      true

    let%test "of_bitfield 1:::::::::::::::::::::::::::::::" =
      let bf = Bitfield.Bit {zeros = 0b1111111111111111111111111111111l; ones = 0b11111111111111111111111111111111l} in
      print_endline ("bitfield = " ^ Bitfield.to_string bf);
      let r = of_bitfield bf in
      print_endline ("---to-RIC--->   " ^ to_string r);
      equal r (ric (1l, NegInfinity, Int (-1l), ("", 0l)))
  end)
end)

    
    