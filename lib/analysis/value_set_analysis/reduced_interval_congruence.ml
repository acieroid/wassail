open Core 

open Maths

(** Reduced Interval Congruence (RIC) Domain for Pointer Analysis

    This module defines an abstract domain for representing sets of possible integer values in 
    pointer analysis.

    The RIC abstraction combines:
    - A congruence domain for arithmetic progressions: `stride * ℤ + offset`
    - An interval domain for bounding values with a lower and upper bound

    These domains are reduced together to ensure consistency and precision.
    The RIC domain supports standard abstract operations such as join, meet, widening, and 
    arithmetic operations, and is used for modeling address sets in static analyses of memory.
*)
module RIC = struct

  (** Arithmetic Congruence Domain

      Represents integer sets of the form `stride * ℤ + offset`, where the offset may be absolute or 
      relative. Provides standard abstract operations and equality up to congruence equivalence.
  *)
  module Congruence = struct
    type congruence = {
      stride : int;
      offset : string * int
    }

    type t =
      | Top (* ℤ *)
      | Bottom (* ∅ *)
      | Congruence of congruence

    (** [equal c1 c2] checks structural and semantic equality between two congruence elements. *)
    let equal (c1 : t) (c2 : t) : bool =
      match c1, c2 with 
      | Top, Top -> true
      | Top, Congruence c when c.stride = 1 -> true 
      | Congruence c, Top when c.stride = 1 -> true
      | Bottom, Bottom -> true
      | Congruence c1, Congruence c2 when c1.stride = 1 && c2.stride = 1 -> true
      | Congruence c1, Congruence c2 when c1.stride = 0 && c2.stride = 0 ->
        begin match c1.offset, c2.offset with 
          | (v1, o1), (v2, o2) -> String.equal v1 v2 && o1 = o2
        end
      | Congruence c1, Congruence c2 ->
        c1.stride = c2.stride && 
        begin match c1.offset, c2.offset with 
          | (v1, o1), (v2, o2) -> String.equal v1 v2 && (o1 - o2) mod c1.stride = 0
        end
      | _ -> false

    let to_string (c : t) : string =
      match c with 
      | Top -> "ℤ" 
      | Bottom -> "∅"
      | Congruence c ->
        let stride = 
          if c.stride = 0 then
            ""
          else
            (if (c.stride = 1) then "" else string_of_int c.stride) ^ "ℤ" in
        let offset = 
          match c.offset with 
          | ("", offset) ->
            if offset = 0 then
              ""
            else
              string_of_int offset
          | (var, offset) ->
            if offset = 0 then
              var
            else
              "(" ^ var ^ "+" ^ string_of_int offset ^ ")"
        in
        match stride, offset with 
        | "", "" -> "∅"
        | "", offset -> offset 
        | "ℤ", _ -> "ℤ"
        | stride, "" -> stride 
        | stride, offset -> stride ^ "+" ^ offset

    let to_string2 (c : t) : string =
      match c with 
      | Top -> "ℤ"
      | Bottom ->  "∅"
      | Congruence {stride = s; offset = (v, o)} -> 
        "stride=" ^ string_of_int s ^ "; offset=(" ^ v ^ ", " ^ string_of_int o ^ ")"

    (** [join c1 c2] computes the least upper bound (union) of two congruences. *)
    let join (c1 : t) (c2 : t) : t =
      match c1, c2 with 
      | Top, _ | _, Top -> Top 
      | Bottom, c -> c
      | c, Bottom -> c 
      | Congruence c1, Congruence c2 ->
        begin match c1.offset, c2.offset with 
          | ("", o1), ("", o2) -> 
            let new_offset = ("", (min o1 o2)) in 
            let new_stride = gcd (gcd c1.stride c2.stride) (abs (o1 - o2)) in
            Congruence {stride = new_stride; offset = new_offset}
          | (v1, o1), (v2, o2) when String.equal v1 v2 ->
            let new_offset = (v1, min o1 o2) in
            let new_stride = gcd (gcd c1.stride c2.stride) (abs (o1 - o2)) in 
            Congruence {stride = new_stride; offset = new_offset}
          | _ -> Top
        end

    (** [meet c1 c2] computes the greatest lower bound (meet) of two congruences. *)
    let meet (c1 : t) (c2 : t) : t =
      match c1, c2 with
      | Top, c -> c
      | c, Top -> c 
      | Bottom, _ | _, Bottom -> Bottom 
      | Congruence {stride = 0; offset = (v1, o1)}, 
        Congruence {stride = 0; offset = (v2, o2)} when String.equal v1 v2 && o1 = o2 -> c1
      | Congruence {stride = 0; offset = (v1, o1)}, 
        Congruence {stride = s2; offset = (v2, o2)} when String.equal v1 v2 -> 
          if o1 mod s2 = o2 then c1 else Bottom
      | Congruence {stride = s2; offset = (v2, o2)}, 
        Congruence {stride = 0; offset = (v1, o1)} when String.equal v1 v2 -> 
          if o1 mod s2 = o2 then c2 else Bottom
      | Congruence c1, Congruence c2 ->
        let gcd_stride = gcd c1.stride c2.stride in 
        begin match c1.offset, c2.offset with 
          | (v1, o1), (v2, o2) 
            when String.equal v1 v2 && (o1 - o2) mod gcd_stride = 0 -> 
              let new_stride = lcm c1.stride c2.stride in 
              let new_offset, _ = chinese_remainder c1.stride o1 c2.stride o2 in
              let new_offset = (v1, new_offset) in
              Congruence {stride = new_stride; offset = new_offset}
          | _ -> Bottom
        end
    
    let widen (c1 :t) (c2 : t) : t = join c1 c2

    let sum (c1 : t) (c2 : t) : t =
      match c1, c2 with
      | Top, _ | _, Top -> Top
      | Bottom, c -> c
      | c, Bottom -> c
      | Congruence {offset = (v1,_); _}, 
        Congruence {offset = (v2, _); _} when not (String.equal v1 v2) -> Top
      | Congruence {stride = s1; offset = (v,o1)}, 
        Congruence {stride = s2; offset = (_, o2); _} ->
          let new_stride = gcd s1 s2 in
          let new_offset = (v, o1 + o2) in
          Congruence {stride = new_stride; offset = new_offset}
  end

  (** Integer Interval Domain

      Represents bounded or unbounded intervals over integers using extended integers.
      Supports standard abstract domain operations such as join, meet, and widening.
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
      | Top, Top -> true 
      | Top, Interval {lower_bound = NegInfinity; upper_bound = Infinity} -> true
      | Interval {lower_bound = NegInfinity; upper_bound = Infinity}, Top -> true
      | Bottom, Bottom -> true 
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
            "[" ^ string_of_int l ^ "," ^ ExtendedInt.to_string Infinity ^ "["
          | NegInfinity, Int u -> 
            "]" ^ ExtendedInt.to_string NegInfinity ^ "," ^ string_of_int u ^ "]"
          | Infinity, _ | _, NegInfinity -> assert false 
          | Int l, Int u when l <= u -> "[" ^ string_of_int l ^ "," ^ string_of_int u ^ "]"
          | _ -> assert false
        end

    (** [join i1 i2] computes the least upper bound (union) of two intervals. *)
    let join (i1 : t) (i2 : t) : t =
      let join =
        match i1, i2 with 
        | Top, _ | _, Top -> Top
        | Bottom, i -> i
        | i, Bottom -> i
        | i1, i2 when equal Bottom i1 -> i2
        | i1, i2 when equal Bottom i2 -> i1
        | Interval i1, Interval i2 -> Interval {
          lower_bound = ExtendedInt.minimum i1.lower_bound i2.lower_bound;
          upper_bound = ExtendedInt.maximum i1.upper_bound i2.upper_bound
        }
      in
      if equal join Top then Top else if equal Bottom join then Bottom else join

    (** [meet i1 i2] computes the greatest lower bound (meet) of two intervals. *)
    let meet (i1 : t) (i2 : t) : t =
      let meet =
        match i1, i2 with 
        | Bottom, _ | _, Bottom -> Bottom 
        | Top, i -> i
        | i, Top -> i
        | i1, _ when equal Bottom i1 -> Bottom
        | _, i2 when equal Bottom i2 -> Bottom
        | Interval i1, Interval i2 ->
          let lower = ExtendedInt.maximum i1.lower_bound i2.lower_bound in
          let upper = ExtendedInt.minimum i1.upper_bound i2.upper_bound in 
          Interval {lower_bound = lower; upper_bound = upper}
      in if equal meet Top then Top else if equal Bottom meet then Bottom else meet

    let widen (i1 : t) (i2 : t) : t =
      match i1, i2 with 
      | Top, _ -> Top
      | _, Top -> Top
      | Bottom, _ -> i2
      | _, Bottom -> i1
      | i1, _ when equal Bottom i1 -> i2
      | _, i2 when equal Bottom i2 -> i1
      | Interval {lower_bound = l1; upper_bound = u1}, 
        Interval {lower_bound = l2; upper_bound = u2} ->
          let lower = if ExtendedInt.less_than l2 l1 then ExtendedInt.NegInfinity else l1 in
          let upper = if ExtendedInt.less_than u1 u2 then ExtendedInt.Infinity else u1 in
          Interval {lower_bound = lower; upper_bound = upper}

    let sum (i1 : t) (i2 : t) : t =
      match i1, i2 with
      | Top, _ | _, Top -> Top
      | Bottom, i -> i
      | i, Bottom -> i
      | Interval {lower_bound = l1; upper_bound = u1}, 
        Interval {lower_bound = l2; upper_bound = u2} ->
          let new_lower = ExtendedInt.plus l1 l2 in
          let new_upper = ExtendedInt.plus u1 u2 in
          Interval {lower_bound = new_lower; upper_bound = new_upper}
      
  end

  (** RIC values combine a congruence with an interval and an offset.

      - [stride]: spacing between values
      - [lower_bound, upper_bound]: bounds on the integer factor
      - [offset]: base offset (absolute or relative)
  *)
  type ric = {
    stride : int;
    lower_bound : ExtendedInt.t;
    upper_bound : ExtendedInt.t;
    offset : string * int;
  }
  [@@deriving sexp, compare, equal]

  (** Abstract value in the RIC domain. *)
  type t =
    | Top 
    | Bottom 
    | RIC of ric
  [@@deriving sexp, compare, equal]

  (** [reduce r] normalizes the representation of a RIC value. 

      Ensures the interval starts at 0 and shifts the offset accordingly.
      Returns [Top] or [Bottom] if the value represents the full or empty set.
  *)
  let reduce (r : t) : t =
    match r with 
    | Top -> Top 
    | Bottom -> Bottom 
    | RIC {stride = s; lower_bound = l; upper_bound = u; offset = o} ->
      if ExtendedInt.less_than u l then Bottom else 
      if (s = 1 && ExtendedInt.equal NegInfinity l && ExtendedInt.equal Infinity u) then 
        Top 
      else if (s = 0) then 
        RIC {stride = 0; lower_bound = Int 0; upper_bound = Int 0; offset = o} 
      else
        let new_offset =
          match o, l with 
          | (var, o), Int l -> (var, o + s * l)
          | (var, o), NegInfinity -> (var, o mod s)
          | _ -> assert false
        in
        let new_lower =
          match l with 
          | NegInfinity -> ExtendedInt.NegInfinity
          | Int _ -> Int 0
          | Infinity -> assert false 
        in 
        let new_upper =
          match o, l, u with
          | _, _, Infinity -> ExtendedInt.Infinity
          | _, Int l, Int u -> Int (u - l)
          | (_, o), NegInfinity, u -> 
            ExtendedInt.plus u (ExtendedInt.divide (ExtendedInt.Int o) (ExtendedInt.Int s))
          | _ -> assert false
        in
        let new_stride = 
          if ExtendedInt.equal new_lower new_upper then 
            0 (* Singleton *)
          else 
            s 
        in
        RIC {stride = new_stride; lower_bound = new_lower; upper_bound = new_upper; offset = new_offset}

  let ric (r : int * ExtendedInt.t * ExtendedInt.t * (string * int)) : t =
    reduce (
      match r with 
      | s, l, u, o -> RIC {stride = s; lower_bound = l; upper_bound = u; offset = o}
    )

  let is_singleton (r : t) : bool =
    let r = reduce r in
    match r with
    | RIC {stride = 0; _} -> true
    | _ -> false

  (** [of_congruence_and_interval c i] constructs a RIC value from a congruence and interval.

      Automatically reduces the result for consistency.
  *)
  let of_congruence_and_interval (c : Congruence.t) (i : Interval.t) : t =
    match c with
    | Top ->
      begin match i with 
        | Top -> Top
        | Bottom -> Bottom
        | Interval {lower_bound = l; upper_bound = u} ->
          ric (1, l, u, ("", 0))
      end
    | c when Congruence.equal Top c ->
      begin match i with 
        | Top -> Top
        | Bottom -> Bottom
        | Interval {lower_bound = l; upper_bound = u} ->
          ric (1, l, u, ("", 0))
      end
    | Bottom -> Bottom
    | Congruence {stride = 0; offset = o} ->
      begin match i with
        | Bottom -> Bottom
        | _ -> ric (0, Int 0, Int 0, o)
      end
    | Congruence {stride = s; offset = (var, o)} ->
      begin match i with 
        | Top -> reduce (ric (s, ExtendedInt.NegInfinity, ExtendedInt.Infinity, (var, o)))
        | Bottom -> Bottom
        | Interval {lower_bound = l; upper_bound = u} ->
          let lower = 
            ExtendedInt.divide (ExtendedInt.minus l (ExtendedInt.Int o)) (ExtendedInt.Int s) in
          let upper = 
            ExtendedInt.divide (ExtendedInt.minus u (ExtendedInt.Int o)) (ExtendedInt.Int s) in
          ric (s, lower, upper, (var, o))
      end

  (** [to_congruence_and_interval r] decomposes a RIC value into its congruence and interval components.

      The result captures the semantics of the RIC value.
  *)
  let to_congruence_and_interval (r : t) : Congruence.t * Interval.t =
    (* let r = reduce r in *)
    match r with 
    | Top -> Top, Top
    | Bottom -> Bottom, Bottom 
    | RIC {stride = s; offset = (var, o); lower_bound = l; upper_bound = u} ->
      Congruence {stride = s; offset = (var, if s = 0 then o else o mod s)},
      Interval {lower_bound = ExtendedInt.plus (ExtendedInt.Int o) (ExtendedInt.times (ExtendedInt.Int s) l); 
                upper_bound = ExtendedInt.plus (ExtendedInt.Int o) (ExtendedInt.times (ExtendedInt.Int s) u)}
      
    

  (** [equal r1 r2] returns true if two RIC values represent the same set. *)
  let equal (ric1 : t) (ric2 : t) : bool =
    let ric1, ric2 = reduce ric1, reduce ric2 in
    match ric1, ric2 with 
    | Top, Top | Bottom, Bottom -> true
    | RIC {stride = s1; lower_bound = l1; upper_bound = u1; offset = o1},  
      RIC {stride = s2; lower_bound = l2; upper_bound = u2; offset = o2} ->
        s1 = s2 && ExtendedInt.equal l1 l2 && ExtendedInt.equal u1 u2 &&
        begin match o1, o2 with 
          | (v1, o1), (v2, o2) -> String.equal v1 v2 && o1 = o2
        end
    | _ -> false

  (** [comparable_offsets r1 r2] returns true if two RICs can be compared, i.e., they have matching base variables. *)
  let comparable_offsets (ric1 : t) (ric2 : t) : bool =
    match ric1, ric2 with
    | RIC {offset = (v1, _); _},
      RIC {offset = (v2, _); _} -> String.equal v1 v2 
    | _ -> true

  let to_string (r : t) : string =
    let r = reduce r in
    match r with
    | Top -> "⊤"
    | Bottom -> "⊥"
    | RIC {stride = s; lower_bound = l; upper_bound = u; offset = o} ->
      let offset = 
        match o with 
        | (var, i) -> 
          if i = 0 then
            (if not (String.equal var "") then "+" else "") ^ var
          else if String.equal var "" then
            (if i > 0 then "+" else "-") ^ string_of_int (abs i)
          else
            "+(" ^ var ^ ((if i > 0 then "+" else "-") ^ string_of_int (abs i)) ^ ")"
      in
      let interval = Interval.to_string (Interval.Interval {lower_bound = l; upper_bound = u}) in
      let stride = if s = 1 then "" else string_of_int s in
      match stride, interval, offset with 
      | "0", _, "" -> "⊥"
      | "0", _, o -> if String.equal "+" (String.sub o ~pos:0 ~len:1) then String.sub o ~pos:1 ~len:(String.length o - 1) else o
      | "", "]-∞,∞[", _ -> "⊤"
      | _ -> stride ^ interval ^ offset


  (** [of_list l] constructs a RIC value that represents exactly the integers in [l]. *)
  let of_list (l : int list) : t =
    let l = List.dedup_and_sort ~compare:Int.compare l in
    match l with
    | [] -> Bottom
    | x1 :: rest ->
      let stride = List.fold ~init:0 ~f:(fun acc x -> gcd acc (abs (x - x1))) rest in
      let list_minimum = List.fold ~init:x1 ~f:min rest in
      let offset = ("", list_minimum) in
      let congruence = Congruence.Congruence {stride = stride; offset = offset} in
      let lower = ExtendedInt.Int list_minimum in 
      let upper = ExtendedInt.Int (List.fold ~init:x1 ~f:max rest) in
      let interval = Interval.Interval {lower_bound = lower; upper_bound = upper} in
      of_congruence_and_interval congruence interval

  (** [meet r1 r2] returns the intersection of two RIC values. *)
  let meet (ric1 : t) (ric2 : t) : t =
    let (c1, i1) = to_congruence_and_interval ric1 in
    let (c2, i2) = to_congruence_and_interval ric2 in
    let c = Congruence.meet c1 c2 in
    let i = Interval.meet i1 i2 in 
    of_congruence_and_interval c i

  (** [join r1 r2] returns the union over-approximation of two RIC values. *)
  let join (ric1 : t) (ric2 : t) : t =
    let (c1, i1) = to_congruence_and_interval ric1 in
    let (c2, i2) = to_congruence_and_interval ric2 in
    let c = Congruence.join c1 c2 in
    let i = Interval.join i1 i2 in 
    of_congruence_and_interval c i

  (** [is_subset r1 ~of_:r2] returns true if [r1] is a subset of [r2]. *)
  let is_subset (ric1 : t) ~(of_ : t) : bool =
    let m = meet ric1 of_ in
    let j = join ric1 of_ in 
    match m, j with 
    | m, j when equal m ric1 && equal j of_ -> true 
    | m, _ when equal m ric1 -> assert false
    | _, j when equal j of_ -> assert false
    | _ -> false

  (** [subsumes r1 r2] returns true if [r1] over-approximates [r2]. *)
  let subsumes (ric1 : t) (ric2 : t) : bool = 
    is_subset ric2 ~of_:ric1

  (** [complement r] returns a list of RICs that together represent the complement of [r]. *)
  let complement (r : t) : t list =
    let r = reduce r in
    match r with
    | Top -> [Bottom]
    | Bottom -> [Top]
    | RIC {stride = s; lower_bound = l; upper_bound = u; offset = (v, o)} ->
      let inferior_RIC = if ExtendedInt.equal l NegInfinity then [] else [(ric (1, NegInfinity, ExtendedInt.minus l (Int 1), (v, o)))] in
      let superior_RIC = if ExtendedInt.equal u Infinity then [] else [ric (1, ExtendedInt.plus (Int (o + if s = 0 then 1 else s)) (ExtendedInt.times (Int s) u) , Infinity, (v, 0))] in 
      let rec make_overlapping_complement (i : int) (acc : t list) : t list =
        match i with
        | i when i = s -> acc
        | i -> make_overlapping_complement (i + 1) (ric (s, l, u, (v, o + i)) :: acc)
      in
      let overlapping_complement = if s = 0 then [] else make_overlapping_complement 1 [] in
      inferior_RIC @ overlapping_complement @ superior_RIC

  (** [add_offset r c] shifts the offset of [r] by integer [c]. *)
  let add_offset (r : t) (c : int) : t =
    match r with
    | Top -> Top 
    | Bottom -> Bottom
    | RIC {stride = s; lower_bound = l; upper_bound = u; offset = (v,o)} -> reduce (ric (s, l, u, (v, o + c)))

  (** [remove_lower_bound r] removes the lower bound constraint in [r]. *)
  let remove_lower_bound (r : t) : t =
    let r = reduce r in
    match r with 
    | Top -> Top 
    | Bottom -> Bottom 
    | RIC {stride = s; lower_bound = _; upper_bound = u; offset = o} -> 
      reduce (
        if s = 0 then 
          ric (1, NegInfinity, u, o) 
        else
          ric (s, NegInfinity, u, o)
      )

  (** [remove_upper_bound r] removes the upper bound constraint in [r]. *)
  let remove_upper_bound (r : t) : t =
    let r = reduce r in
    match r with 
    | Top -> Top 
    | Bottom -> Bottom 
    | RIC {stride = s; lower_bound = l; upper_bound = _; offset = o} -> 
      reduce (
        if s = 0 then
          ric (1, l, Infinity, o)
        else
          ric (s, l, Infinity, o)
      )

  (** [widen r ~relative_to] computes the widening of [r] relative to the previous value. *)
  let widen (ric1 : t) ~(relative_to : t) : t =
    let (c1, i1) = to_congruence_and_interval ric1 in
    let (c2, i2) = to_congruence_and_interval relative_to in
    let widened_c = Congruence.widen c1 c2 in
    let widened_i = Interval.widen i1 i2 in 
    of_congruence_and_interval widened_c widened_i 

  (** [partially_accessed ~by ~size] returns RICs that may partially overlap with [by] of given size. *)
  let partially_accessed ~(by : t) ~(size : int) : t list =
    let r = reduce by in
    let rec aux (i : int) (acc : t list) : t list =
      match i, r with
      | _, Top -> [Top] 
      | _, Bottom -> []
      | i, RIC {stride = s; lower_bound = Int _; _} when i = if s = 0 then size else min size (s - 3) -> acc
      | i, RIC {stride = s; lower_bound = NegInfinity; _} when i + 3 + size - 1 = max (-4) (size - s - 1)-> acc
      | i, RIC {stride = s; lower_bound = Int l; upper_bound = Int u; offset = (v, o)} ->
        aux (i + 1) (ric (s, Int l, (if s <> 0 then Int (l + ((u - l + 1) * s - i + size - 1) / s - 1) else Int 0), (v, o + i)) :: acc)
      | i, RIC {upper_bound = Infinity; _} ->
        aux (i + 1) (add_offset r i :: acc)
      | i, RIC {lower_bound = NegInfinity; _} -> 
        aux (i - 1) (add_offset r (i + 3 + size - 1) :: acc)
      | _ -> assert false
    in

    let accessed = aux (-3) []
    in
    if size <> 4 || (not (is_singleton r)) && (match r with | RIC {stride = s; _} when s < 4 -> true | _ -> false) then
      List.dedup_and_sort ~compare:compare accessed
    else
      List.dedup_and_sort ~compare:compare (List.filter ~f:(fun x -> not (equal x r)) accessed)

  
  (** Access classification: fully and partially accessed RICs. *)
  type accessed = {
    fully : t;
    partially : t list
  }
  (** [accessed ~value_set size] computes memory cells fully and partially accessed. *)
  let accessed ~(value_set : t) (size : int) : accessed =
    let stride = match value_set with | RIC {stride = s; _} -> s | Top -> 1 | Bottom -> 0 in
    let f = if size = 4 && stride >= 4 then value_set else Bottom in
    let p = partially_accessed ~by:value_set ~size:size in
    {fully = f; partially = p}

  (** [plus r1 r2] returns the sum over-approximation of [r1] and [r2]. *)
  let plus (ric1 : t) (ric2 : t) : t =
    match ric1, ric2 with
    | Top, _ | _, Top -> Top
    | Bottom, Bottom -> Bottom
    | Bottom, _ ->
      let warning_msg = "Adding Bottom state to an RIC - result will return the RIC unchanged: " ^ to_string ric2 in
      Log.warn warning_msg;
      ric2
    | _, Bottom -> 
      let warning_msg = "Adding Bottom state to an RIC - result will return the RIC unchanged: " ^ to_string ric1 in
      Log.warn warning_msg;
      ric1
    | _ ->
      if comparable_offsets ric1 ric2 then
        let c1, i1 = to_congruence_and_interval ric1 and
        c2, i2 = to_congruence_and_interval ric2 in
        let new_congruence = Congruence.sum c1 c2 and
        new_interval = Interval.sum i1 i2 in
        of_congruence_and_interval new_congruence new_interval
      else
        Top

  (** [negative r] returns the RIC representing [-r]. *)
  let negative (r : t) : t =
    match r with
    | Top -> Top
    | Bottom -> Bottom
    | RIC {stride = s; lower_bound = l; upper_bound = u; offset = ("", o)} ->
      ric (s, ExtendedInt.times (Int (-1)) u, ExtendedInt.times (Int (-1)) l, ("", - o))
    | _ -> assert false

  (** [remove_relative_offset r] returns a copy of [r] with a concrete (empty) variable. *)
  let remove_relative_offset (r : t) : t =
    match r with
    | Top -> Top
    | Bottom -> Bottom
    | RIC {stride = s; lower_bound = l; upper_bound = u; offset = (_, o)} -> 
      ric (s, l, u, ("", o))

  (** [minus r1 r2] returns the difference over-approximation of [r1] and [r2]. *)
  let minus (ric1 : t) (ric2 : t) : t =
    if not (comparable_offsets ric1 ric2) then
      Top
    else
      let ric1 = remove_relative_offset ric1 in
      let ric2 = remove_relative_offset ric2 in
      let negative_ric2 = negative ric2 in
      plus ric1 negative_ric2

  (** [remove ~this ~from] returns parts of [from] that are not in [this]. *)
  let remove ~(this : t) ~(from : t) : t list =
    if comparable_offsets this from then
      let this_complement = complement this in
      List.filter ~f:(fun r -> not (equal r Bottom)) (List.map ~f:(meet from) this_complement)
    else
      []

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

    (* Top should be equal to Congruence with stride 1 and offset 0. *)
    let%test "Congruence: top equals stride1 offset0" =
      Congruence.equal Congruence.Top (Congruence.Congruence {stride = 1; offset = ("", 0)})

    (* Top should be equal to Congruence with stride 1 and offset (g0, 10). *)
    let%test "Congruence: top equals stride1 offset(\"g0\", 10)" =
      Congruence.equal Congruence.Top (Congruence.Congruence {stride = 1; offset = ("g0", 10)})

    (* Congruences with same stride and same absolute offset are equal. *)
    let%test "Congruence: same stride same absolute offset" =
      Congruence.equal (Congruence.Congruence {stride = 3; offset = ("", 5)})
                       (Congruence.Congruence {stride = 3; offset = ("", 5)})

    (* Congruences with same stride and same absolute offset are not equal but equivalent. *)
    let%test "Congruence: same stride, equivalent absolute offsets" =
      Congruence.equal (Congruence.Congruence {stride = 3; offset = ("", 5)})
                       (Congruence.Congruence {stride = 3; offset = ("", 2)})

    (* Congruences with same stride and same absolute offset are not equal but equivalent. *)
    let%test "Congruence: same stride, equivalent relative offsets" =
      Congruence.equal (Congruence.Congruence {stride = 3; offset = ("v", 5)})
                       (Congruence.Congruence {stride = 3; offset = ("v", 2)})

    (* Congruences with different strides are not equal. *)
    let%test "Congruence: different strides" =
      not (Congruence.equal (Congruence.Congruence {stride = 3; offset = ("", 5)})
                            (Congruence.Congruence {stride = 4; offset = ("", 5)}))

    (* Congruences with different absolute offsets are not equal. *)
    let%test "Congruence: different absolute offsets that are not equivalent" =
      not (Congruence.equal (Congruence.Congruence {stride = 3; offset = ("", 5)})
                            (Congruence.Congruence {stride = 3; offset = ("", 6)}))

    (* Bottom is equal to Bottom. *)
    let%test "Congruence: bottom equals bottom" =
      Congruence.equal Congruence.Bottom Congruence.Bottom

    (* Bottom is not equal to Top. *)
    let%test "Congruence: bottom not equal top" =
      not (Congruence.equal Congruence.Bottom Congruence.Top)

    (* Relative congruences with same var and offset are equal. *)
    let%test "Congruence: same relative var and offset" =
      Congruence.equal
        (Congruence.Congruence {stride = 2; offset = ("x", 4)})
        (Congruence.Congruence {stride = 2; offset = ("x", 4)})

    (* Relative congruences with same var but non equivalent offsets are not equal. *)
    let%test "Congruence: same var, non equivalent offsets" =
      not (Congruence.equal
             (Congruence.Congruence {stride = 2; offset = ("x", 4)})
             (Congruence.Congruence {stride = 2; offset = ("x", 5)}))

    (* Relative congruences with different vars are not equal. *)
    let%test "Congruence: different vars" =
      not (Congruence.equal
             (Congruence.Congruence {stride = 2; offset = ("x", 4)})
             (Congruence.Congruence {stride = 2; offset = ("y", 4)}))

    (* Congruences with same offset but different strides are not equal. *)
    let%test "Congruence: same offset different strides" =
      not (Congruence.equal
             (Congruence.Congruence {stride = 2; offset = ("x", 4)})
             (Congruence.Congruence {stride = 3; offset = ("x", 4)}))

    (* Top is not equal to Bottom. *)
    let%test "Congruence: top not equal bottom" =
      not (Congruence.equal Congruence.Top Congruence.Bottom)

    (* Top is not equal to Congruence with stride 2 and offset 0. *)
    let%test "Congruence: top not equal stride2 offset0" =
      not (Congruence.equal Congruence.Top (Congruence.Congruence {stride = 2; offset = ("", 0)}))

    (* Stride is equal to 1, one relative offset, one absolute offset *)
    let%test "Congruence: both strides are equal to 1" =
      Congruence.equal (Congruence.Congruence {stride = 1; offset = ("", 0)}) 
                       (Congruence.Congruence {stride = 1; offset = ("v", 7)})

    (* to string of Top should be "ℤ". *)
    let%test "Congruence: to string top" =
      String.equal (Congruence.to_string Congruence.Top) "ℤ"

    (* to_string of Bottom should be "⊥". *)
    let%test "Congruence: to_string_bottom" =
      String.equal (Congruence.to_string Congruence.Bottom) "∅"

    (* to_string of stride 1, absolute offset 0 should be "ℤ". *)
    let%test "Congruence: to_string stride1 offset0" =
      String.equal
        (Congruence.to_string (Congruence.Congruence {stride = 1; offset = ("", 0)}))
        "ℤ"

    (* to_string of stride 2, absolute offset 3 should be "2ℤ + 3". *)
    let%test "Congruence: to_string stride2 offset3" =
      String.equal
        (Congruence.to_string (Congruence.Congruence {stride = 2; offset = ("", 3)}))
        "2ℤ+3"

    (* to_string of stride 4, relative offset ("x", 1) should be "4ℤ + (x + 1)". *)
    let%test "Congruence: to_string relative" =
      String.equal
        (Congruence.to_string (Congruence.Congruence {stride = 4; offset = ("x", 1)}))
        "4ℤ+(x+1)"
    
    (* to_string of stride 4, relative offset ("x", 1) should be "4ℤ + (x + 1)". *)
    let%test "Congruence: to_string relative offset0" =
      String.equal
        (Congruence.to_string (Congruence.Congruence {stride = 10; offset = ("x", 0)}))
        "10ℤ+x"

    (* Joining Top with anything should return Top. *)
    let%test "Congruence: join_top_left" =
      Congruence.equal
        (Congruence.join Congruence.Top (Congruence.Congruence {stride = 2; offset = ("", 0)}))
        Congruence.Top

    let%test "Congruence: join_top_right" =
      Congruence.equal
        (Congruence.join (Congruence.Congruence {stride = 2; offset = ("v", 3)}) Congruence.Top)
        Congruence.Top

    (* Joining Bottom with a congruence should return that congruence. *)
    let%test "Congruence: join_bottom_left" =
      Congruence.equal
        (Congruence.join Congruence.Bottom (Congruence.Congruence {stride = 27; offset = ("", 36)}))
        (Congruence.Congruence {stride = 27; offset = ("", 36)})

    let%test "Congruence: join_bottom_right" =
      Congruence.equal
        (Congruence.join (Congruence.Congruence {stride = 2; offset = ("", 3)}) Congruence.Bottom)
        (Congruence.Congruence {stride = 2; offset = ("", 3)})

    (* Joining two equal congruences returns the same congruence. *)
    let%test "Congruence: join_equal_congruences" =
      let c = Congruence.Congruence {stride = 4; offset = ("", 1)} in
      Congruence.equal (Congruence.join c c) c

    (* Joining congruences with same offset but different stride returns least common multiple stride. *)
    let%test "Congruence: join_same_offset_diff_stride" =
      Congruence.equal
        (Congruence.join
          (Congruence.Congruence {stride = 2; offset = ("", 3)})
          (Congruence.Congruence {stride = 4; offset = ("", 3)}))
        (Congruence.Congruence {stride = 2; offset = ("", 3)})  (* gcd(2,4)=2; offset=3 *)

    (* Joining congruences with different offsets. *)
    let%test "Congruence: join (2ℤ+3) and (2ℤ+2) => Top" =
      let c1 = Congruence.Congruence {stride = 2; offset = ("", 3)} in
      let c2 = Congruence.Congruence {stride = 2; offset = ("", 2)} in 
      let j = Congruence.join c1 c2 in 
      print_endline ("(" ^ Congruence.to_string c1 ^ ") ⊔ (" ^ Congruence.to_string c2 ^ ") = " ^ Congruence.to_string j);
      Congruence.equal j Congruence.Top

    (* Joining congruences with different offsets. *)
    let%test "Congruence: join (10ℤ+3) and (10ℤ+8) => 5ℤ+3" =
      let c1 = Congruence.Congruence {stride = 10; offset = ("", 3)} in
      let c2 = Congruence.Congruence {stride = 10; offset = ("", 8)} in 
      let j = Congruence.join c1 c2 in 
      print_endline ("(" ^ Congruence.to_string c1 ^ ") ⊔ (" ^ Congruence.to_string c2 ^ ") = " ^ Congruence.to_string j);
      Congruence.equal j (Congruence.Congruence {stride = 5; offset = ("", 3)})

    (* Joining congruences with different types of offsets should return Top. *)
    let%test "Congruence: join_relative_and_absolute" =
      let c1 = (Congruence.Congruence {stride = 2; offset = ("", 3)}) in
      let c2 = (Congruence.Congruence {stride = 2; offset = ("x", 3)}) in
      let j = Congruence.join c1 c2 in
      print_endline ("(" ^ Congruence.to_string c1 ^ ") ⊔ (" ^ Congruence.to_string c2 ^ ") = " ^ Congruence.to_string j);
      Congruence.equal j Congruence.Top

    (* Joining congruences with equivalent relative offsets. *)
    let%test "Congruence: join_equivalent_relative_offsets" =
      let c1 = Congruence.Congruence {stride = 20; offset = ("v", 3)} in
      let c2 = Congruence.Congruence {stride = 20; offset = ("v", 23)} in 
      let j = Congruence.join c1 c2 in 
      print_endline ("(" ^ Congruence.to_string c1 ^ ") ⊔ (" ^ Congruence.to_string c2 ^ ") = " ^ Congruence.to_string j);
      Congruence.equal j c1

    (* Joining congruences with equivalent relative offsets. *)
    let%test "Congruence: join_non_equivalent_relative_offsets" =
      let c1 = Congruence.Congruence {stride = 20; offset = ("v", 3)} in
      let c2 = Congruence.Congruence {stride = 20; offset = ("v", 21)} in 
      let j = Congruence.join c1 c2 in 
      print_endline ("(" ^ Congruence.to_string c1 ^ ") ⊔ (" ^ Congruence.to_string c2 ^ ") = " ^ Congruence.to_string j);
      Congruence.equal j (Congruence.Congruence {stride = 2; offset = ("v", 1)}
      )

    (* Joining congruences with different relative variables returns Top. *)
    let%test "Congruence: join_diff_relative_vars" =
      let c1 = Congruence.Congruence {stride = 20; offset = ("v", 3)} in
      let c2 = Congruence.Congruence {stride = 20; offset = ("x", 23)} in 
      let j = Congruence.join c1 c2 in 
      print_endline ("(" ^ Congruence.to_string c1 ^ ") ⊔ (" ^ Congruence.to_string c2 ^ ") = " ^ Congruence.to_string j);
      Congruence.equal j Congruence.Top

    (* Meeting Bottom with anything should return Bottom. *)
    let%test "Congruence: meet_bottom_left" =
      Congruence.equal
        (Congruence.meet Congruence.Bottom (Congruence.Congruence {stride = 2; offset = ("", 0)}))
        Congruence.Bottom

    let%test "Congruence: meet_bottom_right" =
      Congruence.equal
        (Congruence.meet (Congruence.Congruence {stride = 2; offset = ("v", 3)}) Congruence.Bottom)
        Congruence.Bottom

    (* Meeting Top with a congruence should return that congruence. *)
    let%test "Congruence: meet_top_left" =
      Congruence.equal
        (Congruence.meet Congruence.Top (Congruence.Congruence {stride = 27; offset = ("", 36)}))
        (Congruence.Congruence {stride = 27; offset = ("", 36)})

    let%test "Congruence: meet_top_right" =
      Congruence.equal
        (Congruence.meet (Congruence.Congruence {stride = 2; offset = ("", 3)}) Congruence.Top)
        (Congruence.Congruence {stride = 2; offset = ("", 3)})

    (* Meeting two equal congruences returns the same congruence. *)
    let%test "Congruence: meet_equal_congruences" =
      let c = Congruence.Congruence {stride = 4; offset = ("", 1)} in
      Congruence.equal (Congruence.meet c c) c

    (* Meeting congruences with same offset but different stride. *)
    let%test "Congruence: meet_same_offset_diff_stride" =
      let c1 = (Congruence.Congruence {stride = 2; offset = ("", 3)}) in
      let c2 = (Congruence.Congruence {stride = 4; offset = ("", 3)}) in
      let m = Congruence.meet c1 c2 in
      print_endline ("(" ^ Congruence.to_string c1 ^ ") ⊓ (" ^ Congruence.to_string c2 ^ ") = " ^ Congruence.to_string m);
      Congruence.equal m c2

      (* Meeting congruences with same offset but different stride. *)
    let%test "Congruence: meet_same_offset_diff_stride2" =
      let c1 = (Congruence.Congruence {stride = 2; offset = ("", 3)}) in
      let c2 = (Congruence.Congruence {stride = 5; offset = ("", 3)}) in
      let m = Congruence.meet c1 c2 in
      print_endline ("(" ^ Congruence.to_string c1 ^ ") ⊓ (" ^ Congruence.to_string c2 ^ ") = " ^ Congruence.to_string m);
      Congruence.equal m (Congruence.Congruence {stride = 10; offset = ("", 3)}) (* stride = lcm 2 5*)

    (* Meeting congruences with different offsets. *)
    let%test "Congruence: meet (2ℤ + 3) and (2ℤ + 2) => Top" =
      let c1 = Congruence.Congruence {stride = 2; offset = ("", 3)} in
      let c2 = Congruence.Congruence {stride = 2; offset = ("", 2)} in 
      let m = Congruence.meet c1 c2 in 
      print_endline ("(" ^ Congruence.to_string c1 ^ ") ⊓ (" ^ Congruence.to_string c2 ^ ") = " ^ Congruence.to_string m);
      Congruence.equal m Congruence.Bottom

    (* Meeting congruences with different offsets. *)
    let%test "Congruence: meet (10ℤ + 3) and (10ℤ + 8) => Bottom" =
      let c1 = Congruence.Congruence {stride = 10; offset = ("", 3)} in
      let c2 = Congruence.Congruence {stride = 10; offset = ("", 8)} in 
      let m = Congruence.meet c1 c2 in 
      print_endline ("(" ^ Congruence.to_string c1 ^ ") ⊓ (" ^ Congruence.to_string c2 ^ ") = " ^ Congruence.to_string m);
      Congruence.equal m Congruence.Bottom

    (* Joining congruences with different types of offsets should return Top. *)
    let%test "Congruence: meet_relative_and_absolute" =
      let c1 = (Congruence.Congruence {stride = 2; offset = ("", 3)}) in
      let c2 = (Congruence.Congruence {stride = 2; offset = ("x", 3)}) in
      let m = Congruence.meet c1 c2 in
      print_endline ("(" ^ Congruence.to_string c1 ^ ") ⊓ (" ^ Congruence.to_string c2 ^ ") = " ^ Congruence.to_string m);
      Congruence.equal m Congruence.Bottom

    (* Meeting congruences with equivalent relative offsets. *)
    let%test "Congruence: meet_equivalent_relative_offsets" =
      let c1 = Congruence.Congruence {stride = 20; offset = ("v", 3)} in
      let c2 = Congruence.Congruence {stride = 20; offset = ("v", 23)} in 
      let m = Congruence.meet c1 c2 in 
      print_endline ("(" ^ Congruence.to_string c1 ^ ") ⊓ (" ^ Congruence.to_string c2 ^ ") = " ^ Congruence.to_string m);
      Congruence.equal m c1

    let%test "Congruence: meet with singleton congruence" =
      let c1 = Congruence.Congruence {stride = 0; offset = ("v", 3)} in
      let c2 = Congruence.Congruence {stride = 20; offset = ("v", 3)} in
      let  m = Congruence.meet c1 c2 in 
      print_endline ("(" ^ Congruence.to_string c1 ^ ") ⊓ (" ^ Congruence.to_string c2 ^ ") = " ^ Congruence.to_string m);
      Congruence.equal m (Congruence.Congruence {stride = 0; offset = ("v", 3)})

    (* Meeting congruences with equivalent relative offsets. *)
    let%test "Congruence: join_non_equivalent_relative_offsets" =
      let c1 = Congruence.Congruence {stride = 20; offset = ("v", 3)} in
      let c2 = Congruence.Congruence {stride = 20; offset = ("v", 21)} in 
      let m = Congruence.meet c1 c2 in 
      print_endline ("(" ^ Congruence.to_string c1 ^ ") ⊓ (" ^ Congruence.to_string c2 ^ ") = " ^ Congruence.to_string m);
      Congruence.equal m Congruence.Bottom

    (* Meeting congruences with different relative variables returns Bottom. *)
    let%test "Congruence: join_diff_relative_vars" =
      let c1 = Congruence.Congruence {stride = 20; offset = ("v", 3)} in
      let c2 = Congruence.Congruence {stride = 20; offset = ("x", 23)} in 
      let m = Congruence.meet c1 c2 in 
      print_endline ("(" ^ Congruence.to_string c1 ^ ") ⊔ (" ^ Congruence.to_string c2 ^ ") = " ^ Congruence.to_string m);
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
        (Interval.Interval {lower_bound = Int 0; upper_bound = Int 5})
        (Interval.Interval {lower_bound = Int 0; upper_bound = Int 5})

    (* Intervals with different lower bounds are not equal. *)
    let%test "not_equal_diff_lower" =
      not (Interval.equal
            (Interval.Interval {lower_bound = Int 1; upper_bound = Int 5})
            (Interval.Interval {lower_bound = Int 0; upper_bound = Int 5}))

    (* Intervals with different upper bounds are not equal. *)
    let%test "not_equal_diff_upper" =
      not (Interval.equal
            (Interval.Interval {lower_bound = Int 0; upper_bound = Int 6})
            (Interval.Interval {lower_bound = Int 0; upper_bound = Int 5}))

    (* Intervals with one infinite bound are not equal to finite-bounded intervals. *)
    let%test "not_equal_finite_vs_infinite" =
      not (Interval.equal
            (Interval.Interval {lower_bound = NegInfinity; upper_bound = Int 5})
            (Interval.Interval {lower_bound = Int 0; upper_bound = Int 5}))

    let%test "equal_neginf_Infinity" =
      Interval.equal
        (Interval.Interval {lower_bound = NegInfinity; upper_bound = Infinity})
        (Interval.Interval {lower_bound = NegInfinity; upper_bound = Infinity})

    (* to_string of Top should be "ℤ". *)
    let%test "to_string_top" =
      let s = Interval.to_string Interval.Top in
      print_endline ("Top → " ^ s);
      String.equal s "ℤ"

    (* to_string of Bottom should be "∅". *)
    let%test "to_string_bottom" =
      let s = Interval.to_string Interval.Bottom in
      print_endline ("Bottom → " ^ s);
      String.equal s "∅"

    (* to_string of [0, 5] should be "[0, 5]". *)
    let%test "to_string_0_5" =
      let s = Interval.to_string (Interval.Interval {lower_bound = Int 0; upper_bound = Int 5}) in
      print_endline ("[0,5] → " ^ s);
      String.equal s "[0,5]"

    (* to_string of [-∞, 10] should be "[-∞, 10]". *)
    let%test "to_string_neg_inf_10" =
      let s = Interval.to_string (Interval.Interval {lower_bound = NegInfinity; upper_bound = Int 10}) in
      print_endline ("]-∞,10] → " ^ s);
      String.equal s "]-∞,10]"

    (* to_string of [4, ∞] should be "[4, ∞]". *)
    let%test "to_string_4_pos_inf" =
      let s = Interval.to_string (Interval.Interval {lower_bound = Int 4; upper_bound = Infinity}) in
      print_endline ("[4,∞[ → " ^ s);
      String.equal s "[4,∞["

    (* to_string of [-∞, ∞] should be "ℤ". *)
    let%test "to_string_neg_inf_pos_inf" =
      let s = Interval.to_string (Interval.Interval {lower_bound = NegInfinity; upper_bound = Infinity}) in
      print_endline ("[-∞,∞] → " ^ s);
      String.equal s "ℤ"


    (* join of Top with anything is Top. *)
    let%test "join_top" =
      let joined = Interval.join Interval.Top (Interval.Interval {lower_bound = Int 0; upper_bound = Int 5}) in
      print_endline ("Top ⊔ [0, 5] → " ^ Interval.to_string joined);
      Interval.equal joined Interval.Top

    (* join of Bottom with an interval is that interval. *)
    let%test "join_bottom" =
      let i = Interval.Interval {lower_bound = Int 2; upper_bound = Int 4} in
      let joined = Interval.join Interval.Bottom i in
      print_endline ("⊥ ⊔ [2, 4] → " ^ Interval.to_string joined);
      Interval.equal joined i

    (* join of two intervals gives correct bounds. *)
    let%test "join_0_5_3_10" =
      let a = Interval.Interval {lower_bound = Int 0; upper_bound = Int 5} in
      let b = Interval.Interval {lower_bound = Int 3; upper_bound = Int 10} in
      let joined = Interval.join a b in
      print_endline ("[0, 5] ⊔ [3, 10] → " ^ Interval.to_string joined);
      Interval.equal joined (Interval.Interval {lower_bound = Int 0; upper_bound = Int 10})

    (* join of two disjoint intervals gives correct bounds. *)
    let%test "join_0_5_7_10" =
      let a = Interval.Interval {lower_bound = Int 0; upper_bound = Int 5} in
      let b = Interval.Interval {lower_bound = Int 7; upper_bound = Int 10} in
      let joined = Interval.join a b in
      print_endline ("[0, 5] ⊔ [7, 10] → " ^ Interval.to_string joined);
      Interval.equal joined (Interval.Interval {lower_bound = Int 0; upper_bound = Int 10})

    (* meet of Top and an interval is that interval. *)
    let%test "meet_top" =
      let i = Interval.Interval {lower_bound = Int 1; upper_bound = Int 7} in
      let met = Interval.meet Interval.Top i in
      print_endline ("Top ⊓ [1, 7] → " ^ Interval.to_string met);
      Interval.equal met i

    (* meet of disjoint intervals is Bottom. *)
    let%test "meet_disjoint" =
      let a = Interval.Interval {lower_bound = Int 0; upper_bound = Int 2} in
      let b = Interval.Interval {lower_bound = Int 5; upper_bound = Int 10} in
      let met = Interval.meet a b in
      print_endline ("[0, 2] ⊓ [5, 10] → " ^ Interval.to_string met);
      Interval.equal met Interval.Bottom

    (* meet of overlapping intervals gives the intersection. *)
    let%test "meet_overlap" =
      let a = Interval.Interval {lower_bound = Int 0; upper_bound = Int 10} in
      let b = Interval.Interval {lower_bound = Int 5; upper_bound = Int 15} in
      let met = Interval.meet a b in
      print_endline ("[0, 10] ⊓ [5, 15] → " ^ Interval.to_string met);
      Interval.equal met (Interval.Interval {lower_bound = Int 5; upper_bound = Int 10})
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
    let%test "add_offset_to_top" =
      let r = Top in
      let result = add_offset r 5 in
      print_endline ("⊤ ⊞ 5 → " ^ to_string result);
      RIC.equal result Top

    let%test "add_offset_to_bottom" =
      let r = Bottom in
      let result = add_offset r 3 in
      print_endline ("⊥ ⊞ 3 → " ^ to_string result);
      RIC.equal result Bottom

    let%test "add_offset_to_absolute" =
      let r = ric (4, Int 0, Int 2, ("", 4)) in
      let result = add_offset r 12 in
      print_endline ("(4[0, 2] + 4) ⊞ 12 → " ^ to_string result);
      RIC.equal result (ric (4, Int 0, Int 2, ("", 16)))

    let%test "add_offset_to_relative" =
      let r = ric (3, Int 0, Int 2, ("x", 1)) in
      let result = add_offset r (-2) in
      print_endline ("(3[0, 2] + (x+1)) ⊞ (-2) → " ^ to_string result);
      RIC.equal result (ric (3, Int 0, Int 2, ("x", -1)))

    let%test "add_offset_to_stride_zero" =
      let r = ric (0, Int 0, Int 0, ("", 5)) in
      let result = add_offset r 10 in
      print_endline ("(0[0, 0] + 5) ⊞ 10 → " ^ to_string result);
      RIC.equal result (ric (0, Int 0, Int 0, ("", 15)))

    let%test "reduce_top" =
      RIC.equal (reduce Top) Top

    let%test "reduce_bottom" =
      RIC.equal (reduce Bottom) Bottom

    let%test "reduce_identity" =
      let r = ric (2, Int 0, Int 5, ("", 3)) in
      RIC.equal (reduce r) r

    let%test "reduce_to_bottom" =
      let r = ric (4, Int 10, Int 5, ("", 0)) in
      RIC.equal (reduce r) Bottom

    let%test "reduce_to_top" =
      let r = ric (1, NegInfinity, Infinity, ("", 0)) in
      RIC.equal (reduce r) Top

    let%test "reduce_stride_zero" =
      let r = ric (0, Int 2, Int 2, ("", 5)) in
      let reduced = reduce r in
      RIC.equal reduced (ric (0, Int 0, Int 0, ("", 5)))

    let%test "reduce_relative_offset" =
      let r = ric (3, Int 2, Int 5, ("x", 4)) in
      let reduced = reduce r in
      RIC.equal reduced (ric (3, Int 0, Int 3, ("x", 10)))

    let%test "reduce_shift_and_normalize" =
      let original = ric (4, Int 1, Int 2, ("", 1)) in
      let reduced = reduce original in
      print_endline ("reduce (4[1, 2] + 1) → " ^ to_string reduced);
      RIC.equal reduced (ric (4, Int 0, Int 1, ("", 5)))

    let%test "reduce_neg_infinity" =
      let original = ric (4, NegInfinity, Int 5, ("", 10)) in
      let reduced = reduce original in
      print_endline ("reduce (4[-∞, 5] + 10) → " ^ to_string reduced);
      RIC.equal reduced (ric (4, NegInfinity, Int 5, ("", 10)))

    let%test "equal_top_top" =
      RIC.equal Top Top

    let%test "equal_bottom_bottom" =
      RIC.equal Bottom Bottom

    let%test "equal_same_ric" =
      let r1 = ric (2, Int 0, Int 3, ("x", 4)) in
      let r2 = ric (2, Int 0, Int 3, ("x", 4)) in
      RIC.equal r1 r2

    let%test "not_equal_different_stride" =
      let r1 = ric (2, Int 0, Int 3, ("x", 4)) in
      let r2 = ric (3, Int 0, Int 3, ("x", 4)) in
      not (RIC.equal r1 r2)

    let%test "not_equal_different_bounds" =
      let r1 = ric (2, Int 0, Int 3, ("x", 4)) in
      let r2 = ric (2, Int 1, Int 3, ("x", 4)) in
      not (RIC.equal r1 r2)

    let%test "not_equal_different_offset" =
      let r1 = ric (2, Int 0, Int 3, ("x", 4)) in
      let r2 = ric (2, Int 0, Int 3, ("x", 5)) in
      not (RIC.equal r1 r2)

    let%test "equal_equivalent_after_reduce" =
      let r1 = ric (2, Int 1, Int 4, ("", 3)) in
      let r2 = ric (2, Int 0, Int 3, ("", 5)) in
      RIC.equal r1 r2

    let%test "to_string_top" =
      let s = to_string Top in
      print_endline ("Top → " ^ s);
      String.equal s "⊤"

    let%test "to_string_bottom" =
      let s = to_string Bottom in
      print_endline ("Bottom → " ^ s);
      String.equal s "⊥"

    let%test "to_string_absolute" =
      let s = to_string (ric (2, Int 0, Int 4, ("", 5))) in
      print_endline ("2[0,4]+5 → " ^ s);
      String.equal s "2[0,4]+5"

    let%test "to_string_relative" =
      let s = to_string (ric (3, Int 0, Int 3, ("x", 2))) in
      print_endline ("3[0,3]+(x+2) → " ^ s);
      String.equal s "3[0,3]+(x+2)"

    let%test "to_string_relative_offset_0" =
      let s = to_string (ric (3, Int 0, Int 3, ("x", 0))) in
      print_endline ("3[0,3]+x → " ^ s);
      String.equal s "3[0,3]+x"

    let%test "to_string_stride_1" =
      let s = to_string (ric (1, Int 0, Int 3, ("", 0))) in
      print_endline ("1[0,3]+0 → " ^ s);
      String.equal s "[0,3]"

    let%test "of_list_empty" =
      RIC.equal (of_list []) Bottom

    let%test "of_list_singleton" =
      let r = of_list [5] in
      print_endline ("of_list [5] → " ^ to_string r);
      RIC.equal r (ric (0, Int 0, Int 0, ("", 5)))

    let%test "of_list_regular" =
      let r = of_list [1; 3; 5; 7] in
      print_endline ("of_list [1; 3; 5; 7] → " ^ to_string r);
      RIC.equal r (ric (2, Int 0, Int 3, ("", 1)))

    let%test "of_list_irregular_stride" =
      let r = of_list [4; 10; 16] in
      print_endline ("of_list [4; 10; 16] → " ^ to_string r);
      RIC.equal r (ric (6, Int 0, Int 2, ("", 4)))

    let%test "of_list_negative_values" =
      let r = of_list [-36; -28; -20] in
      print_endline ("of_list [-36; -28; -20] → " ^ to_string r);
      RIC.equal r (ric (8, Int 0, Int 2, ("", -36)))

    let%test "of_congruence_and_interval_top_top" =
      let r = of_congruence_and_interval Congruence.Top Interval.Top in
      print_endline ("(ℤ, ℤ) → " ^ to_string r);
      RIC.equal r Top

    let%test "of_congruence_and_interval_bottom_top" =
      let r = of_congruence_and_interval Congruence.Bottom Interval.Top in
      print_endline ("(⊥, ℤ) → " ^ to_string r);
      RIC.equal r Bottom

    let%test "of_congruence_and_interval_top_bottom" =
      let r = of_congruence_and_interval Congruence.Top Interval.Bottom in
      print_endline ("(ℤ, ⊥) → " ^ to_string r);
      RIC.equal r Bottom

    let%test "of_congruence_and_interval_abs" =
      let c = Congruence.Congruence {stride = 2; offset = ("", 1)} in
      let i = Interval.Interval {lower_bound = Int 1; upper_bound = Int 7} in
      let r = of_congruence_and_interval c i in
      print_endline ("(2ℤ + 1, [1, 7]) → " ^ to_string r);
      RIC.equal r (ric (2, Int 0, Int 3, ("", 1)))  (* (1 + 2*[0..3]) = {1,3,5,7} *)

    let%test "of_congruence_and_interval_relative" =
      let c = Congruence.Congruence {stride = 3; offset = ("x", 2)} in
      let i = Interval.Interval {lower_bound = Int 5; upper_bound = Int 17} in
      let r = of_congruence_and_interval c i in
      print_endline ("(3ℤ + (x+2), [5, 17]) → " ^ to_string r);
      RIC.equal r (ric (3, Int 1, Int 5, ("x", 2)))  (* (x+2 + 3*[1..5]) = [5..17] *)

    let%test "of_congruence_and_interval_stride_0" =
      let c = Congruence.Congruence {stride = 0; offset = ("", 8)} in
      let i = Interval.Interval {lower_bound = Int 8; upper_bound = Int 8} in
      let r = of_congruence_and_interval c i in
      print_endline ("(0ℤ + 8, [8, 8]) → " ^ to_string r);
      RIC.equal r (ric (0, Int 0, Int 0, ("", 8)))

    let%test "to_congruence_and_interval_top" =
      let c, i = to_congruence_and_interval Top in
      print_endline ("Top → (" ^ Congruence.to_string c ^ ", " ^ Interval.to_string i ^ ")");
      Congruence.equal c Congruence.Top && Interval.equal i Interval.Top

    let%test "to_congruence_and_interval_bottom" =
      let c, i = to_congruence_and_interval Bottom in
      print_endline ("Bottom → (" ^ Congruence.to_string c ^ ", " ^ Interval.to_string i ^ ")");
      Congruence.equal c Congruence.Bottom && Interval.equal i Interval.Bottom

    let%test "to_congruence_and_interval_absolute" =
      let r = ric (2, Int 0, Int 3, ("", 5)) in
      let c, i = to_congruence_and_interval r in
      print_endline ("(2[0, 3] + 5) → (" ^ Congruence.to_string c ^ ", " ^ Interval.to_string i ^ ")");
      Congruence.equal c (Congruence.Congruence {stride = 2; offset = ("", 5)}) &&
      Interval.equal i (Interval.Interval {lower_bound = Int 5; upper_bound = Int 11})

    let%test "to_congruence_and_interval_absolute2" =
      let r = ric (3, Int 1, Int 4, ("", 2)) in
      let c, i = to_congruence_and_interval r in
      print_endline ("(3[1, 4] + 2) → (" ^ Congruence.to_string c ^ ", " ^ Interval.to_string i ^ ")");
      Congruence.equal c (Congruence.Congruence {stride = 3; offset = ("", 2)}) &&
      Interval.equal i (Interval.Interval {lower_bound = Int 5; upper_bound = Int 14})

    let%test "to_congruence_and_interval_relative" =
      let r = ric (3, Int 1, Int 4, ("x", 2)) in
      let c, i = to_congruence_and_interval r in
      print_endline ("(3[1, 4] + (x + 2)) → (" ^ Congruence.to_string c ^ ", " ^ Interval.to_string i ^ ")");
      Congruence.equal c (Congruence.Congruence {stride = 3; offset = ("x", 2)}) &&
      Interval.equal i (Interval.Interval {lower_bound = Int 5; upper_bound = Int 14})

    let%test "to_congruence_and_interval_stride_zero" =
      let r = ric (0, Int 0, Int 0, ("", 7)) in
      let c, i = to_congruence_and_interval r in
      print_endline ("(0[0, 0] + 7) → (" ^ Congruence.to_string c ^ ", " ^ Interval.to_string i ^ ")");
      Congruence.equal c (Congruence.Congruence {stride = 0; offset = ("", 7)}) &&
      Interval.equal i (Interval.Interval {lower_bound = Int 7; upper_bound = Int 7})

    (* --- Additional tests for RIC.meet and RIC.join --- *)
    let%test "meet_top_and_r" =
      let r = ric (2, Int 0, Int 4, ("", 5)) in
      let m = meet Top r in
      print_endline ("⊤ ⊓ " ^ to_string r ^ " → " ^ to_string m);
      RIC.equal m r

    let%test "meet_bottom_and_r" =
      let r = ric (2, Int 0, Int 4, ("", 5)) in
      let m = meet Bottom r in
      print_endline ("⊥ ⊓ " ^ to_string r ^ " → " ^ to_string m);
      RIC.equal m Bottom

    let%test "meet_regular" =
      let r1 = ric (2, Int 0, Int 4, ("", 1)) in
      let r2 = ric (4, Int 0, Int 2, ("", 1)) in
      let m = meet r1 r2 in
      print_endline (to_string r1 ^ " ⊓ " ^ to_string r2 ^ " → " ^ to_string m);
      RIC.equal m (ric (4, Int 0, Int 2, ("", 1)))

    let%test "meet_disjoint" =
      let r1 = ric (2, Int 0, Int 1, ("", 1)) in
      let r2 = ric (2, Int 0, Int 1, ("", 2)) in
      let m = meet r1 r2 in
      print_endline (to_string r1 ^ " ⊓ " ^ to_string r2 ^ " → " ^ to_string m);
      RIC.equal m Bottom

    let%test "join_top_and_r" =
      let r = ric (2, Int 0, Int 4, ("", 5)) in
      let j = join Top r in
      print_endline ("⊤ ⊔ " ^ to_string r ^ " → " ^ to_string j);
      RIC.equal j Top

    let%test "join_bottom_and_r" =
      let r = ric (2, Int 0, Int 4, ("", 5)) in
      let j = join Bottom r in
      print_endline ("⊥ ⊔ " ^ to_string r ^ " → " ^ to_string j);
      RIC.equal j r

    let%test "join_regular" =
      let r1 = ric (2, Int 0, Int 2, ("", 1)) in
      let r2 = ric (2, Int 3, Int 4, ("", 1)) in
      let j = join r1 r2 in
      print_endline (to_string r1 ^ " ⊔ " ^ to_string r2 ^ " → " ^ to_string j);
      RIC.equal j (ric (2, Int 0, Int 4, ("", 1)))

    let%test "join_different_stride" =
      let r1 = ric (2, Int 0, Int 2, ("", 1)) in
      let r2 = ric (4, Int 1, Int 2, ("", 1)) in
      let j = join r1 r2 in
      print_endline (to_string r1 ^ " ⊔ " ^ to_string r2 ^ " → " ^ to_string j);
      RIC.equal j (ric (2, Int 0, Int 4, ("", 1)))


    (* --- Additional tests for RIC.subset_of --- *)
    let%test "subset_of_top" =
      let a = ric (2, Int 0, Int 3, ("", 1)) in
      let b = Top in
      let result = is_subset a ~of_:b in
      print_endline (to_string a ^ " ⊆ " ^ to_string b ^ " → " ^ string_of_bool result ^ " [subset_of_top]");
      result

    let%test "subset_of_bottom" =
      let a = Bottom in
      let b = ric (2, Int 0, Int 3, ("", 1)) in
      let result = is_subset a ~of_:b in
      print_endline (to_string a ^ " ⊆ " ^ to_string b ^ " → " ^ string_of_bool result ^ " [subset_of_bottom]");
      result

    let%test "bottom_subset_of_bottom" =
      let a = Bottom in
      let b = Bottom in
      let result = is_subset a ~of_:b in
      print_endline (to_string a ^ " ⊆ " ^ to_string b ^ " → " ^ string_of_bool result ^ " [bottom_subset_of_bottom]");
      result

    let%test "top_subset_of_top" =
      let a = Top in
      let b = Top in
      let result = is_subset a ~of_:b in
      print_endline (to_string a ^ " ⊆ " ^ to_string b ^ " → " ^ string_of_bool result ^ " [top_subset_of_top]");
      result

    let%test "subset_of_itself" =
      let r = ric (4, Int 0, Int 2, ("", 8)) in
      let result = is_subset r ~of_:r in
      print_endline (to_string r ^ " ⊆ " ^ to_string r ^ " → " ^ string_of_bool result ^ " [subset_of_itself]");
      result

    let%test "smaller_range_subset" =
      let r1 = ric (2, Int 0, Int 2, ("", 1)) in
      let r2 = ric (2, Int 0, Int 4, ("", 1)) in
      let result = is_subset r1 ~of_:r2 in
      print_endline (to_string r1 ^ " ⊆ " ^ to_string r2 ^ " → " ^ string_of_bool result ^ " [smaller_range_subset]");
      result

    let%test "different_stride_not_subset" =
      let r1 = ric (4, Int 0, Int 2, ("", 8)) in
      let r2 = ric (2, Int 0, Int 2, ("", 8)) in
      let result = not (is_subset r1 ~of_:r2) in
      print_endline (to_string r1 ^ " ⊆ " ^ to_string r2 ^ " → " ^ string_of_bool (not result) ^ " [different_stride_not_subset]");
      result

    let%test "different_offset_not_subset" =
      let r1 = ric (2, Int 0, Int 2, ("", 3)) in
      let r2 = ric (2, Int 0, Int 2, ("", 5)) in
      let result = not (is_subset r1 ~of_:r2) in
      print_endline (to_string r1 ^ " ⊆ " ^ to_string r2 ^ " → " ^ string_of_bool (not result) ^ " [different_offset_not_subset]");
      result

    let%test "overlap_not_subset" =
      let r1 = ric (2, Int 0, Int 2, ("", 3)) in
      let r2 = ric (2, Int 1, Int 3, ("", 3)) in
      let result = not (is_subset r1 ~of_:r2) in
      print_endline (to_string r1 ^ " ⊆ " ^ to_string r2 ^ " → " ^ string_of_bool (not result) ^ " [overlap_not_subset]");
      result

    let%test "subset_of_relative_equal" =
      let r1 = ric (5, Int 1, Int 2, ("x", 0)) in
      let r2 = ric (5, Int 0, Int 3, ("x", 0)) in
      let result = is_subset r1 ~of_:r2 in
      print_endline (to_string r1 ^ " ⊆ " ^ to_string r2 ^ " → " ^ string_of_bool result ^ " [subset_of_relative_equal]");
      result

    let%test "subset_of_relative_different_stride" =
      let r1 = ric (4, Int 0, Int 2, ("x", 0)) in
      let r2 = ric (2, Int 0, Int 2, ("x", 0)) in
      let result = not (is_subset r1 ~of_:r2) in
      print_endline (to_string r1 ^ " ⊆ " ^ to_string r2 ^ " → " ^ string_of_bool (not result) ^ " [subset_of_relative_different_stride]");
      result

    let%test "top_is_not_subset_of_bottom" =
      let a = Top in
      let b = Bottom in
      let result = not (is_subset a ~of_:b) in
      print_endline (to_string a ^ " ⊆ " ^ to_string b ^ " → " ^ string_of_bool (not result) ^ " [top_is_not_subset_of_bottom]");
      result

    (* --- Tests for remove_lower_bound and remove_upper_bound --- *)
    let%test "remove_lower_bound_top" =
      let r = Top in
      let result = remove_lower_bound r in
      print_endline ("remove_lower_bound ⊤ → " ^ to_string result);
      RIC.equal result Top

    let%test "remove_lower_bound_bottom" =
      let r = Bottom in
      let result = remove_lower_bound r in
      print_endline ("remove_lower_bound ⊥ → " ^ to_string result);
      RIC.equal result Bottom

    let%test "remove_lower_bound_regular" =
      let r = ric (3, Int 2, Int 5, ("x", 4)) in
      let result = remove_lower_bound r in
      print_endline ("remove_lower_bound " ^ to_string r ^ " → " ^ to_string result);
      RIC.equal result (ric (3, NegInfinity, Int 5, ("x", 4)))

    let%test "remove_upper_bound_top" =
      let r = Top in
      let result = remove_upper_bound r in
      print_endline ("remove_upper_bound ⊤ → " ^ to_string result);
      RIC.equal result Top

    let%test "remove_upper_bound_bottom" =
      let r = Bottom in
      let result = remove_upper_bound r in
      print_endline ("remove_upper_bound ⊥ → " ^ to_string result);
      RIC.equal result Bottom

    let%test "remove_upper_bound_regular" =
      let r = ric (2, Int 1, Int 4, ("", 2)) in
      let result = remove_upper_bound r in
      print_endline ("remove_upper_bound " ^ to_string r ^ " → " ^ to_string result);
      RIC.equal result (ric (2, Int 1, Infinity, ("", 2)))

    let%test "equal_neg_infinity_shifted_absolute" =
      let r1 = ric (3, NegInfinity, Int 3, ("", 10)) in
      let r2 = ric (3, NegInfinity, Int 5, ("", 4)) in
      print_endline (to_string r1 ^ " = " ^ to_string r2 ^ " → " ^ string_of_bool (equal r1 r2));
      equal r1 r2

    let%test "equal_neg_infinity_shifted_relative" =
      let r1 = ric (3, NegInfinity, Int 3, ("x", 10)) in
      let r2 = ric (3, NegInfinity, Int 5, ("x", 4)) in
      print_endline (to_string r1 ^ " = " ^ to_string r2 ^ " → " ^ string_of_bool (equal r1 r2));
      equal r1 r2

    (* --- Additional widen tests --- *)
    let%test "widen_top_and_any" =
      let r = ric (3, Int 0, Int 4, ("", 5)) in
      let w = widen Top ~relative_to:r in
      print_endline ("widen ⊤ (" ^ to_string r ^ ") → " ^ to_string w);
      RIC.equal w Top

    let%test "widen_bottom_and_any" =
      let r = ric (3, Int 0, Int 4, ("", 5)) in
      let w = widen Bottom ~relative_to:r in
      print_endline ("widen ⊥ (" ^ to_string r ^ ") → " ^ to_string w);
      RIC.equal w r

    let%test "widen_regular_to_infinite_upper" =
      let r1 = ric (4, Int 0, Int 1, ("", 0)) in
      let r2 = ric (4, Int 0, Int 2, ("", 0)) in
      let expected = ric (4, Int 0, Infinity, ("", 0)) in
      let result = widen r1 ~relative_to:r2 in
      print_endline ("widen (" ^ to_string r1 ^ ") (" ^ to_string r2 ^ ") → " ^ to_string result);
      RIC.equal result expected

    let%test "widen_extend_lower_bound" =
      let r1 = ric (2, Int 1, Int 4, ("", 3)) in
      let r2 = ric (2, Int 0, Int 4, ("", 3)) in
      let expected = ric (2, NegInfinity, Int 4, ("", 3)) in
      let result = widen r1 ~relative_to:r2 in
      print_endline ("widen (" ^ to_string r1 ^ ") (" ^ to_string r2 ^ ") → " ^ to_string result);
      RIC.equal result expected

    let%test "widen_shifted_relative_offsets" =
      let r1 = ric (3, Int 1, Int 3, ("x", 7)) in
      let r2 = ric (3, Int 1, Int 5, ("x", 7)) in
      let expected = ric (3, Int 1, Infinity, ("x", 7)) in
      let result = widen r1 ~relative_to:r2 in
      print_endline ("widen (" ^ to_string r1 ^ ") (" ^ to_string r2 ^ ") → " ^ to_string result);
      RIC.equal result expected

    let%test "widen_self" =
      let r = ric (4, Int 0, Int 3, ("", 5)) in
      let result = widen r ~relative_to:r in
      print_endline ("widen (" ^ to_string r ^ ") (" ^ to_string r ^ ") → " ^ to_string result);
      RIC.equal result r

    let%test "widen_different_offsets" =
      let r1 = ric (20, Int 0, Int 3, ("", 3)) in
      let r2 = ric (20, Int 0, Int 3, ("", 7)) in
      let result = widen r1 ~relative_to:r2 in
      print_endline ("widen (" ^ to_string r1 ^ ") (" ^ to_string r2 ^ ") → " ^ to_string result);
      RIC.equal result (ric (4, Int 0, Infinity, ("", 3)))

    (* Additional tests for complement *)
    let%test "complement of Top is [Bottom]" =
      match complement Top with
      | [Bottom] -> true
      | _ -> false

    let%test "complement of Bottom is [Top]" =
      match complement Bottom with
      | [Top] -> true
      | _ -> false

      
    let%test "complement of finite 2[0,2]+4" =
      let r = ric (2, Int 0, Int 2, ("", 4)) in
      let c = complement r in
      print_endline ("complement of " ^ to_string r ^ " → [  " ^ String.concat ~sep:"; " (List.map c ~f:to_string) ^ "  ]");
      List.length c = 3 &&
      List.mem ~equal c (ric (1, NegInfinity, Int 0, ("", 3))) &&
      List.mem ~equal c (ric (2, Int 0, Int 2, ("", 5))) &&
      List.mem ~equal c (ric (1, Int 0, Infinity, ("", 10)))

      
    let%test "complement of singleton 0[0,0]+7" =
      let r = ric (0, Int 0, Int 0, ("", 7)) in
      let c = complement r in
     print_endline ("complement of " ^ to_string r ^ " → [  " ^ String.concat ~sep:"; " (List.map c ~f:to_string) ^ "  ]");
      List.length c = 2 &&
      List.mem ~equal c (ric (1, NegInfinity, Int 0, ("", 6))) &&
      List.mem ~equal c (ric (1, Int 0, Infinity, ("", 8)))

      
    let%test "complement of 3[0,1]+2 has 4 parts" =
      let r = ric (3, Int 0, Int 1, ("", 2)) in
      let c = complement r in
      print_endline ("complement of " ^ to_string r ^ " → [  " ^ String.concat ~sep:"; " (List.map c ~f:to_string) ^ "  ]");
      List.length c = 4

    
    let%test "complement of relative 4[0,1]+(x+1)" =
      let r = ric (4, Int 0, Int 1, ("x", 1)) in
      let c = complement r in
      print_endline ("complement of " ^ to_string r ^ " → [  " ^ String.concat ~sep:"; " (List.map c ~f:to_string) ^ "  ]");
      List.length c = 5 &&
      List.exists c ~f:(fun r' -> String.equal (to_string r')  "]-∞,0]+x") &&
      List.exists c ~f:(fun r' -> String.equal (to_string r')  "4[0,1]+(x+2)") &&
      List.exists c ~f:(fun r' -> String.equal (to_string r')  "[0,∞[+(x+9)")

    let%test "complement preserves relative offset" =
      let r = ric (5, Int 1, Int 3, ("g", 0)) in
      let offsets = List.map ~f:(function RIC {offset = (v, _); _} -> v | _ -> "") (complement r) in
      List.for_all offsets ~f:(String.equal "g")


    let%test "complement with unbounded upper and stride = 1 gives no superior_RIC" =
      let r = ric (1, Int 0, Infinity, ("", 0)) in
      let c = complement r in
      print_endline ("complement of " ^ to_string r ^ " → [  " ^ String.concat ~sep:"; " (List.map c ~f:to_string) ^ "  ]");
      not (List.exists c ~f:(fun r' -> match r' with RIC {lower_bound = Int _; upper_bound = Infinity; _} -> true | _ -> false))

    let%test "complement with unbounded lower and stride = 1 gives no inferior_RIC" =
      let r = ric (1, NegInfinity, Int 5, ("", 0)) in
      let c = complement r in
      print_endline ("complement of " ^ to_string r ^ " → [  " ^ String.concat ~sep:"; " (List.map c ~f:to_string) ^ "  ]");
      not (List.exists c ~f:(fun r' -> match r' with RIC {lower_bound = NegInfinity; upper_bound = Int _; _} -> true | _ -> false))

    let%test "complement of 2[0,1]+3 includes shifted classes" =
      let r = ric (2, Int 0, Int 1, ("", 3)) in
      let comp = complement r in
      print_endline ("complement of " ^ to_string r ^ " → [  " ^ String.concat ~sep:"; " (List.map comp ~f:to_string) ^ "  ]");
      List.exists comp ~f:(fun r' -> match r' with RIC {stride = 2; lower_bound = Int 0; offset = ("", 4); _} -> true | _ -> false)
  
    (* --- Tests for complement --- *)
    let%test "complement_top" =
      let r = Top in
      let compl = complement r in
      print_endline ("complement of " ^ to_string r ^ " → [  " ^ String.concat ~sep:"; " (List.map compl ~f:to_string) ^ "  ]");
      List.equal equal compl [Bottom]

    let%test "complement_bottom" =
      let r = Bottom in
      let compl = complement r in
      print_endline ("complement of " ^ to_string r ^ " → [  " ^ String.concat ~sep:"; " (List.map compl ~f:to_string) ^ "  ]");
      List.equal equal compl  [Top]

    let%test "complement_finite_absolute" =
      let r = ric (2, Int 0, Int 2, ("", 1)) in
      let compl = complement r in
      print_endline ("complement of " ^ to_string r ^ " → [  " ^ String.concat ~sep:"; " (List.map compl ~f:to_string) ^ "  ]");
      List.length compl = 3

    let%test "complement_finite_relative" =
      let r = ric (3, Int 1, Int 3, ("x", 2)) in
      let compl = complement r in
      print_endline ("complement of " ^ to_string r ^ " → [  " ^ String.concat ~sep:"; " (List.map compl ~f:to_string) ^ "  ]");
      List.length compl = 4

    let%test "complement_infinite_lower" =
      let r = ric (2, NegInfinity, Int 2, ("", 0)) in
      let compl = complement r in
      print_endline ("complement of " ^ to_string r ^ " → [  " ^ String.concat ~sep:"; " (List.map compl ~f:to_string) ^ "  ]");
      List.length compl = 2

    let%test "complement_infinite_upper" =
      let r = ric (2, Int 0, Infinity, ("", 0)) in
      let compl = complement r in
      print_endline ("complement of " ^ to_string r ^ " → [  " ^ String.concat ~sep:"; " (List.map compl ~f:to_string) ^ "  ]");
      List.length compl = 2

    let%test "complement_infinite_both" =
      let r = ric (2, NegInfinity, Infinity, ("", 0)) in
      let compl = complement r in
      print_endline ("complement of " ^ to_string r ^ " → [  " ^ String.concat ~sep:"; " (List.map compl ~f:to_string) ^ "  ]");
      List.length compl = 1

    let%test "complement_stride_zero" =
      let r = ric (0, Int 0, Int 0, ("", 5)) in
      let compl = complement r in
      print_endline ("complement of " ^ to_string r ^ " → [  " ^ String.concat ~sep:"; " (List.map compl ~f:to_string) ^ "  ]");
      List.length compl = 2

    let%test "complement_relative_stride_zero" =
      let r = ric (0, Int 0, Int 0, ("x", 3)) in
      let compl = complement r in
      print_endline ("complement of " ^ to_string r ^ " → [  " ^ String.concat ~sep:"; " (List.map compl ~f:to_string) ^ "  ]");
      List.length compl = 2

    let%test "complement_nontrivial" =
      let r = ric (4, Int 2, Int 5, ("a", 7)) in
      let compl = complement r in
      print_endline ("complement of " ^ to_string r ^ " → [  " ^ String.concat ~sep:"; " (List.map compl ~f:to_string) ^ "  ]");
      List.length compl = 5 


    let%test "partially_accessed_by_singleton_size_2" =
      let r = ric(0, Int 0, Int 0, ("", 8)) in
      let p_accessed = partially_accessed ~by:r ~size:2 in
      let expected = [ric(0, Int 0, Int 0, ("", 5));
                      ric(0, Int 0, Int 0, ("", 6));
                      ric(0, Int 0, Int 0, ("", 7));
                      ric(0, Int 0, Int 0, ("", 8));
                      ric(0, Int 0, Int 0, ("", 9))] in
      print_endline ("(size 2) partially_accessed of " ^ to_string r ^ " → [  " ^ String.concat ~sep:"; " (List.map p_accessed ~f:to_string) ^ "  ]");
      List.equal RIC.equal p_accessed expected

    let%test "partially_accessed_by_singleton_size_4" =
      let r = ric(0, Int 0, Int 0, ("", 8)) in
      let p_accessed = partially_accessed ~by:r ~size:4 in
      let expected = [ric(0, Int 0, Int 0, ("", 5));
                      ric(0, Int 0, Int 0, ("", 6));
                      ric(0, Int 0, Int 0, ("", 7));
                      ric(0, Int 0, Int 0, ("", 9));
                      ric(0, Int 0, Int 0, ("", 10));
                      ric(0, Int 0, Int 0, ("", 11))] in
      print_endline ("(size 4) partially_accessed of " ^ to_string r ^ " → [  " ^ String.concat ~sep:"; " (List.map p_accessed ~f:to_string) ^ "  ]");
      List.equal RIC.equal p_accessed expected

    let%test "partially_accessed_by_RIC_size_1" =
      let r = ric(4, Int 0, Int 1, ("", 8)) in
      let p_accessed = partially_accessed ~by:r ~size:1 in
      let expected = [ric(4, Int 0, Int 1, ("", 5));
                      ric(4, Int 0, Int 1, ("", 6));
                      ric(4, Int 0, Int 1, ("", 7));
                      ric(4, Int 0, Int 1, ("", 8))] in
      print_endline ("(size 1) partially_accessed of " ^ to_string r ^ " → [  " ^ String.concat ~sep:"; " (List.map p_accessed ~f:to_string) ^ "  ]");
      List.equal RIC.equal p_accessed expected

    let%test "partially_accessed_by_RIC_size_2" =
      let r = ric(4, Int 0, Int 1, ("", 8)) in
      let p_accessed = partially_accessed ~by:r ~size:2 in
      let expected = [ric(4, Int 0, Int 1, ("", 6));
                      ric(4, Int 0, Int 1, ("", 7));
                      ric(4, Int 0, Int 1, ("", 8));
                      ric(4, Int 0, Int 2, ("", 5))] in
      print_endline ("(size 2) partially_accessed of " ^ to_string r ^ " → [  " ^ String.concat ~sep:"; " (List.map p_accessed ~f:to_string) ^ "  ]");
      List.equal RIC.equal p_accessed expected

    let%test "partially_accessed_by_RIC_size_3" =
      let r = ric(4, Int 0, Int 1, ("", 8)) in
      let p_accessed = partially_accessed ~by:r ~size:3 in
      let expected = [ric(4, Int 0, Int 1, ("", 7));
                      ric(4, Int 0, Int 1, ("", 8));
                      ric(4, Int 0, Int 2, ("", 5));
                      ric(4, Int 0, Int 2, ("", 6))] in
      print_endline ("(size 3) partially_accessed of " ^ to_string r ^ " → [  " ^ String.concat ~sep:"; " (List.map p_accessed ~f:to_string) ^ "  ]");
      List.equal RIC.equal p_accessed expected

    let%test "partially_accessed_by_RIC_size_5" =
      let r = ric(4, Int 0, Int 1, ("", 8)) in
      let p_accessed = partially_accessed ~by:r ~size:5 in
      let expected = [ric(4, Int 0, Int 2, ("", 5));
                      ric(4, Int 0, Int 2, ("", 6));
                      ric(4, Int 0, Int 2, ("", 7));
                      ric(4, Int 0, Int 2, ("", 8))] in
      print_endline ("(size 5) partially_accessed of " ^ to_string r ^ " → [  " ^ String.concat ~sep:"; " (List.map p_accessed ~f:to_string) ^ "  ]");
      List.equal RIC.equal p_accessed expected

    let%test "partially_accessed_by_RIC_size_2_stride_5" =
      let r = ric(5, Int 0, Int 1, ("", 8)) in
      let p_accessed = partially_accessed ~by:r ~size:2 in
      let expected = [ric(5, Int 0, Int 1, ("", 5));
                      ric(5, Int 0, Int 1, ("", 6));
                      ric(5, Int 0, Int 1, ("", 7));
                      ric(5, Int 0, Int 1, ("", 8));
                      ric(5, Int 0, Int 1, ("", 9))] in
      print_endline ("(size 2 stride 5) partially_accessed of " ^ to_string r ^ " → [  " ^ String.concat ~sep:"; " (List.map p_accessed ~f:to_string) ^ "  ]");
      List.equal RIC.equal p_accessed expected

    let%test "partially_accessed_by_RIC_size_2_stride_6" =
      let r = ric(6, Int 0, Int 1, ("", 8)) in
      let p_accessed = partially_accessed ~by:r ~size:2 in
      let expected = [ric(6, Int 0, Int 1, ("", 5));
                      ric(6, Int 0, Int 1, ("", 6));
                      ric(6, Int 0, Int 1, ("", 7));
                      ric(6, Int 0, Int 1, ("", 8));
                      ric(6, Int 0, Int 1, ("", 9))] in
      print_endline ("(size 2 stride 6) partially_accessed of " ^ to_string r ^ " → [  " ^ String.concat ~sep:"; " (List.map p_accessed ~f:to_string) ^ "  ]");
      List.equal RIC.equal p_accessed expected

    let%test "partially_accessed_by_RIC_size_2_stride_7" =
      let r = ric(7, Int 0, Int 1, ("", 8)) in
      let p_accessed = partially_accessed ~by:r ~size:2 in
      let expected = [ric(7, Int 0, Int 1, ("", 5));
                      ric(7, Int 0, Int 1, ("", 6));
                      ric(7, Int 0, Int 1, ("", 7));
                      ric(7, Int 0, Int 1, ("", 8));
                      ric(7, Int 0, Int 1, ("", 9))] in
      print_endline ("(size 2 stride 7) partially_accessed of " ^ to_string r ^ " → [  " ^ String.concat ~sep:"; " (List.map p_accessed ~f:to_string) ^ "  ]");
      List.equal RIC.equal p_accessed expected

    let%test "partially_accessed_by_RIC_size_3_stride_2" =
      let r = ric(2, Int 0, Int 1, ("", 8)) in
      let p_accessed = partially_accessed ~by:r ~size:3 in
      let expected = [ric(2, Int 0, Int 3, ("", 5));
                      ric(2, Int 0, Int 3, ("", 6))] in
      print_endline ("(size 3 stride 2) partially_accessed of " ^ to_string r ^ " → [  " ^ String.concat ~sep:"; " (List.map p_accessed ~f:to_string) ^ "  ]");
      List.equal RIC.equal p_accessed expected

    let%test "partially_accessed_by_RIC_size_2_stride_2" =
      let r = ric(2, Int 0, Int 1, ("", 8)) in
      let p_accessed = partially_accessed ~by:r ~size:2 in
      let expected = [ric(2, Int 0, Int 2, ("", 6));
                      ric(2, Int 0, Int 3, ("", 5))] in
      print_endline ("(size 2 stride 2) partially_accessed of " ^ to_string r ^ " → [  " ^ String.concat ~sep:"; " (List.map p_accessed ~f:to_string) ^ "  ]");
      List.equal RIC.equal p_accessed expected

    let%test "partially_accessed_by_RIC_size_4_stride_2" =
      let r = ric(2, Int 0, Int 1, ("", 8)) in
      let p_accessed = partially_accessed ~by:r ~size:4 in
      let expected = [ric(2, Int 0, Int 3, ("", 6));
                      ric(2, Int 0, Int 4, ("", 5))] in
      print_endline ("(size 4 stride 2) partially_accessed of " ^ to_string r ^ " → [  " ^ String.concat ~sep:"; " (List.map p_accessed ~f:to_string) ^ "  ]");
      List.equal RIC.equal p_accessed expected

    let%test "partially_accessed_by_RIC_size_1_stride_2" =
      let r = ric(2, Int 0, Int 1, ("", 8)) in
      let p_accessed = partially_accessed ~by:r ~size:1 in
      let expected = [ric(2, Int 0, Int 2, ("", 5));
                      ric(2, Int 0, Int 2, ("", 6))] in
      print_endline ("(size 1 stride 2) partially_accessed of " ^ to_string r ^ " → [  " ^ String.concat ~sep:"; " (List.map p_accessed ~f:to_string) ^ "  ]");
      List.equal RIC.equal p_accessed expected

    let%test "partially_accessed_by_RIC_size4_bigger_than_stride3" =
      let r = ric(3, Int 0, Int 1, ("", 8)) in
      let p_accessed = partially_accessed ~by:r ~size:4 in
      let expected = [ric(3, Int 0, Int 2, ("", 6));
                      ric(3, Int 0, Int 2, ("", 7));
                      ric(3, Int 0, Int 3, ("", 5))] in
      print_endline ("(size 4 stride 3) partially_accessed of " ^ to_string r ^ " → [  " ^ String.concat ~sep:"; " (List.map p_accessed ~f:to_string) ^ "  ]");
      List.equal RIC.equal p_accessed expected

    let%test "partially_accessed_by_RIC_size4_bigger_than_stride2" =
      let r = ric(2, Int 0, Int 2, ("", 8)) in
      let p_accessed = partially_accessed ~by:r ~size:4 in
      let expected = [ric(2, Int 0, Int 4, ("", 6));
                      ric(2, Int 0, Int 5, ("", 5))] in
      print_endline ("(size 4 stride 2) partially_accessed of " ^ to_string r ^ " → [  " ^ String.concat ~sep:"; " (List.map p_accessed ~f:to_string) ^ "  ]");
      List.equal RIC.equal p_accessed expected

    let%test "partially_accessed_by_Top_size_2" =
      let r = Top in
      let p_accessed = partially_accessed ~by:r ~size:2 in
      let expected = [Top] in
      print_endline ("(size 2) partially_accessed of " ^ to_string r ^ " → [  " ^ String.concat ~sep:"; " (List.map p_accessed ~f:to_string) ^ "  ]");
      List.equal RIC.equal p_accessed expected

    let%test "partially_accessed_by_Top_size_4" =
      let r = Top in
      let p_accessed = partially_accessed ~by:r ~size:4 in
      let expected = [] in
      print_endline ("(size 4) partially_accessed of " ^ to_string r ^ " → [  " ^ String.concat ~sep:"; " (List.map p_accessed ~f:to_string) ^ "  ]");
      List.equal RIC.equal p_accessed expected

    let%test "partially_accessed_by_RIC_size_4_stride_4" =
      let r = ric(4, Int 0, Int 1, ("", 8)) in
      let p_accessed = partially_accessed ~by:r ~size:4 in
      let expected = [ric(4, Int 0, Int 2, ("", 5));
                      ric(4, Int 0, Int 2, ("", 6));
                      ric(4, Int 0, Int 2, ("", 7))] in
      print_endline ("(size 4 stride 4) partially_accessed of " ^ to_string r ^ " → [  " ^ String.concat ~sep:"; " (List.map p_accessed ~f:to_string) ^ "  ]");
      List.equal RIC.equal p_accessed expected

    let%test "partially_accessed_by_RIC_size_4_stride_5" =
      let r = ric(5, Int 0, Int 1, ("", 8)) in
      let p_accessed = partially_accessed ~by:r ~size:4 in
      let expected = [ric(5, Int 0, Int 1, ("", 7));
                      ric(5, Int 0, Int 1, ("", 9));
                      ric(5, Int 0, Int 2, ("", 5));
                      ric(5, Int 0, Int 2, ("", 6))] in
      print_endline ("(size 4 stride 5) partially_accessed of " ^ to_string r ^ " → [  " ^ String.concat ~sep:"; " (List.map p_accessed ~f:to_string) ^ "  ]");
      List.equal RIC.equal p_accessed expected


    let%test "partially_accessed_by_4[10,inf[_size_4" =
      let r = ric(4,Int 0, Infinity, ("a", 10)) in
      let p_accessed = partially_accessed ~by:r ~size:4 in
      let expected = [ric(4, Int 0, Infinity, ("a", 7));
                      ric(4, Int 0, Infinity, ("a", 8));
                      ric(4, Int 0, Infinity, ("a", 9))] in
      print_endline ("(size 4 stride 4 +inf) partially_accessed of " ^ to_string r ^ " → [  " ^ String.concat ~sep:"; " (List.map p_accessed ~f:to_string) ^ "  ]");
      List.equal RIC.equal p_accessed expected

    let%test "partially_accessed_by_4[10,inf[_size_5" =
      let r = ric(4,Int 0, Infinity, ("a", 10)) in
      let p_accessed = partially_accessed ~by:r ~size:5 in
      let expected = [ric(4, Int 0, Infinity, ("a", 7));
                      ric(4, Int 0, Infinity, ("a", 8));
                      ric(4, Int 0, Infinity, ("a", 9));
                      ric(4, Int 0, Infinity, ("a", 10))] in
      print_endline ("(size 5 stride 4 +inf) partially_accessed of " ^ to_string r ^ " → [  " ^ String.concat ~sep:"; " (List.map p_accessed ~f:to_string) ^ "  ]");
      List.equal RIC.equal p_accessed expected


    let%test "partially_accessed_by_4[10,inf[_size_3" =
      let r = ric(4,Int 0, Infinity, ("a", 10)) in
      let p_accessed = partially_accessed ~by:r ~size:3 in
      let expected = [ric(4, Int 0, Infinity, ("a", 7));
                      ric(4, Int 0, Infinity, ("a", 8));
                      ric(4, Int 0, Infinity, ("a", 9));
                      ric(4, Int 0, Infinity, ("a", 10))] in
      print_endline ("(size 3 stride 4 +inf) partially_accessed of " ^ to_string r ^ " → [  " ^ String.concat ~sep:"; " (List.map p_accessed ~f:to_string) ^ "  ]");
      List.equal RIC.equal p_accessed expected


    let%test "partially_accessed_by_4[10,inf[_size_1" =
      let r = ric(7,Int 0, Infinity, ("a", 10)) in
      let p_accessed = partially_accessed ~by:r ~size:1 in
      let expected = [ric(7, Int 0, Infinity, ("a", 7));
                      ric(7, Int 0, Infinity, ("a", 8));
                      ric(7, Int 0, Infinity, ("a", 9));
                      ric(7, Int 0, Infinity, ("a", 10))] in
      print_endline ("(size 1 stride 7  +inf) partially_accessed of " ^ to_string r ^ " → [  " ^ String.concat ~sep:"; " (List.map p_accessed ~f:to_string) ^ "  ]");
      List.equal RIC.equal p_accessed expected


    let%test "partially_accessed_by_4[10,inf[_size_2" =
      let r = ric(7,Int 0, Infinity, ("a", 10)) in
      let p_accessed = partially_accessed ~by:r ~size:2 in
      let expected = [ric(7, Int 0, Infinity, ("a", 7));
                      ric(7, Int 0, Infinity, ("a", 8));
                      ric(7, Int 0, Infinity, ("a", 9));
                      ric(7, Int 0, Infinity, ("a", 10));
                      ric(7, Int 0, Infinity, ("a", 11))] in
      print_endline ("(size 2 stride 7  +inf) partially_accessed of " ^ to_string r ^ " → [  " ^ String.concat ~sep:"; " (List.map p_accessed ~f:to_string) ^ "  ]");
      List.equal RIC.equal p_accessed expected

    let%test "partially_accessed_by_4[10,inf[_size_4" =
      let r = ric(7,Int 0, Infinity, ("a", 10)) in
      let p_accessed = partially_accessed ~by:r ~size:4 in
      let expected = [ric(7, Int 0, Infinity, ("a", 7));
                      ric(7, Int 0, Infinity, ("a", 8));
                      ric(7, Int 0, Infinity, ("a", 9));
                      ric(7, Int 0, Infinity, ("a", 11));
                      ric(7, Int 0, Infinity, ("a", 12));
                      ric(7, Int 0, Infinity, ("a", 13))] in
      print_endline ("(size 4 stride 7  +inf) partially_accessed of " ^ to_string r ^ " → [  " ^ String.concat ~sep:"; " (List.map p_accessed ~f:to_string) ^ "  ]");
      List.equal RIC.equal p_accessed expected

    let%test "partially_accessed_by_4]-inf,0]_size_4" =
      let r = ric(4, NegInfinity, Int 0, ("a", 0)) in
      let p_accessed = partially_accessed ~by:r ~size:4 in
      let expected = [ric(4, NegInfinity, Int 0, ("a", 1));
                      ric(4, NegInfinity, Int 0, ("a", 2));
                      ric(4, NegInfinity, Int 0, ("a", 3))] in
      print_endline ("(size 4 stride 4 -inf) partially_accessed of " ^ to_string r ^ " → [  " ^ String.concat ~sep:"; " (List.map p_accessed ~f:to_string) ^ "  ]");
      List.equal RIC.equal p_accessed expected

    let%test "partially_accessed_by_4]-inf,0]_size_5" =
      let r = ric(4,NegInfinity , Int 0, ("a", 0)) in
      let p_accessed = partially_accessed ~by:r ~size:5 in
      let expected = [ric(4, NegInfinity, Int 0, ("a", 1));
                      ric(4, NegInfinity, Int 0, ("a", 2));
                      ric(4, NegInfinity, Int 0, ("a", 3));
                      ric(4, NegInfinity, Int 0, ("a", 4))] in
      print_endline ("(size 5 stride 4 -inf) partially_accessed of " ^ to_string r ^ " → [  " ^ String.concat ~sep:"; " (List.map p_accessed ~f:to_string) ^ "  ]");
      (* print_endline (String.concat ~sep:"; " (List.map expected ~f:to_string)); *)
      List.equal RIC.equal p_accessed expected

    let%test "partially_accessed_by_4]-inf,0]_size_6" =
      let r = ric(4,NegInfinity , Int 0, ("a", 0)) in
      let p_accessed = partially_accessed ~by:r ~size:6 in
      let expected = [ric(4, NegInfinity, Int 0, ("a", 2));
                      ric(4, NegInfinity, Int 0, ("a", 3));
                      ric(4, NegInfinity, Int 0, ("a", 4));
                      ric(4, NegInfinity, Int 0, ("a", 5))] in
      print_endline ("(size 6 stride 4 -inf) partially_accessed of " ^ to_string r ^ " → [  " ^ String.concat ~sep:"; " (List.map p_accessed ~f:to_string) ^ "  ]");
      List.equal RIC.equal p_accessed expected


    let%test "partially_accessed_by_4]-inf,0]_size_3" =
      let r = ric(4, NegInfinity, Int 0, ("a", 0)) in
      let p_accessed = partially_accessed ~by:r ~size:3 in
      let expected = [ric(4, NegInfinity, Int 0, ("a", -1));
                      ric(4, NegInfinity, Int 0, ("a", 0));
                      ric(4, NegInfinity, Int 0, ("a", 1));
                      ric(4, NegInfinity, Int 0, ("a", 2))] in
      print_endline ("(size 3 stride 4 -inf) partially_accessed of " ^ to_string r ^ " → [  " ^ String.concat ~sep:"; " (List.map p_accessed ~f:to_string) ^ "  ]");
      List.equal RIC.equal p_accessed expected


    let%test "partially_accessed_by_4]-inf,0]_size_1" =
      let r = ric(7, NegInfinity, Int 0, ("a", 0)) in
      let p_accessed = partially_accessed ~by:r ~size:1 in
      let expected = [ric(7, NegInfinity, Int 0, ("a", -3));
                      ric(7, NegInfinity, Int 0, ("a", -2));
                      ric(7, NegInfinity, Int 0, ("a", -1));
                      ric(7, NegInfinity, Int 0, ("a", 0))] in
      print_endline ("(size 1 stride 7  -inf) partially_accessed of " ^ to_string r ^ " → [  " ^ String.concat ~sep:"; " (List.map p_accessed ~f:to_string) ^ "  ]");
      List.equal RIC.equal p_accessed expected


    let%test "partially_accessed_by_4]-inf,0]_size_2" =
      let r = ric(7,NegInfinity, Int 0, ("a", 0)) in
      let p_accessed = partially_accessed ~by:r ~size:2 in
      let expected = [ric(7,NegInfinity, Int 0, ("a", -3));
                      ric(7,NegInfinity, Int 0, ("a", -2));
                      ric(7,NegInfinity, Int 0, ("a", -1));
                      ric(7,NegInfinity, Int 0, ("a", 0));
                      ric(7,NegInfinity, Int 0, ("a", 1))] in
      print_endline ("(size 2 stride 7  -inf) partially_accessed of " ^ to_string r ^ " → [  " ^ String.concat ~sep:"; " (List.map p_accessed ~f:to_string) ^ "  ]");
      List.equal RIC.equal p_accessed expected

    let%test "partially_accessed_by_4]-inf,0]_size_4" =
      let r = ric(7, NegInfinity, Int 0, ("a", 0)) in
      let p_accessed = partially_accessed ~by:r ~size:4 in
      let expected = [ric(7, NegInfinity, Int 0, ("a", -3));
                      ric(7, NegInfinity, Int 0, ("a", -2));
                      ric(7, NegInfinity, Int 0, ("a", -1));
                      ric(7, NegInfinity, Int 0, ("a", 1));
                      ric(7, NegInfinity, Int 0, ("a", 2));
                      ric(7, NegInfinity, Int 0, ("a", 3))] in
      print_endline ("(size 4 stride 7  -inf) partially_accessed of " ^ to_string r ^ " → [  " ^ String.concat ~sep:"; " (List.map p_accessed ~f:to_string) ^ "  ]");
      List.equal RIC.equal p_accessed expected

    (* ----------- Additional RIC.plus tests ----------- *)
    let%test "RIC.plus: absolute + absolute" =
      let r1 = ric (4, Int 0, Int 2, ("", 1)) in
      let r2 = ric (2, Int 1, Int 3, ("", 3)) in
      let result = plus r1 r2 in
      let expected = ric (2, Int 1, Int 7, ("", 4)) in
      print_endline ("(" ^ to_string r1 ^ ") ⊕ (" ^ to_string r2 ^ ") = " ^ to_string result);
      RIC.equal result expected

    let%test "RIC.plus: relative + relative same var" =
      let r1 = ric (3, Int 0, Int 2, ("x", 4)) in
      let r2 = ric (6, Int 1, Int 3, ("x", 1)) in
      let result = plus r1 r2 in
      let expected = ric (3, Int 0, Int 6, ("x", 11)) in
      print_endline ("(" ^ to_string r1 ^ ") ⊕ (" ^ to_string r2 ^ ") = " ^ to_string result);
      RIC.equal result expected

    let%test "RIC.plus: absolute + relative = Top" =
      let r1 = ric (3, Int 0, Int 2, ("", 1)) in
      let r2 = ric (2, Int 1, Int 2, ("x", 4)) in
      let result = plus r1 r2 in
      print_endline ("(" ^ to_string r1 ^ ") ⊕ (" ^ to_string r2 ^ ") = " ^ to_string result);
      RIC.equal result Top

    let%test "RIC.plus: relative + relative different vars = Top" =
      let r1 = ric (3, Int 0, Int 2, ("x", 1)) in
      let r2 = ric (2, Int 1, Int 3, ("y", 2)) in
      let result = plus r1 r2 in
      print_endline ("(" ^ to_string r1 ^ ") ⊕ (" ^ to_string r2 ^ ") = " ^ to_string result);
      RIC.equal result Top

    let%test "RIC.plus: Top + anything = Top" =
      let r = ric (3, Int 0, Int 2, ("", 4)) in
      let result = plus Top r in
      print_endline ("⊤ ⊕ " ^ to_string r ^ " = " ^ to_string result);
      RIC.equal result Top

    let%test "RIC.plus: Bottom + anything = anything" =
      let r = ric (3, Int 0, Int 2, ("", 4)) in
      let result = plus Bottom r in
      print_endline ("⊥ ⊕ " ^ to_string r ^ " = " ^ to_string result);
      RIC.equal result r

    let%test "RIC.plus: singleton + singleton" =
      let r1 = ric (0, Int 0, Int 0, ("", 7)) in
      let r2 = ric (0, Int 0, Int 0, ("", 3)) in
      let result = plus r1 r2 in
      let expected = ric (0, Int 0, Int 0, ("", 10)) in
      print_endline ("(7) ⊕ (3) = " ^ to_string result);
      RIC.equal result expected

    let%test "RIC.plus: stride propagation with normalization" =
      let r1 = ric (4, Int 1, Int 2, ("", 0)) in
      let r2 = ric (2, Int 0, Int 1, ("", 2)) in
      let result = plus r1 r2 in
      let expected = ric (2, Int 0, Int 3, ("", 6)) in
      print_endline ("(4[1,2]) ⊕ (2[0,1]+2) = " ^ to_string result);
      RIC.equal result expected

    let%test "RIC.plus: relative + relative same variable string" =
      let r1 = ric (4, Int 1, Int 2, ("x", 0)) in
      let r2 = ric (2, Int 0, Int 1, ("x", 2)) in
      let result = plus r1 r2 in
      let expected = ric (2, Int 0, Int 3, ("x", 6)) in
      print_endline ("(4[1,2]+x) ⊕ (2[0,1]+(x+2)) = " ^ to_string result);
      RIC.equal result expected

    let%test "RIC.plus: one Top one Bottom = Top" =
      let result = plus Top Bottom in
      print_endline ("⊤ ⊕ ⊥ = " ^ to_string result);
      RIC.equal result Top

    let%test "RIC.plus: both Top = Top" =
      let result = plus Top Top in
      print_endline ("⊤ ⊕ ⊤ = " ^ to_string result);
      RIC.equal result Top

    let%test "RIC.plus: infinite boundary 1" =
      let r1 = ric (4, Int 0, Infinity, ("", 3)) in
      let r2 = ric (6, Int 0, Int 2, ("", 2)) in
      let result = plus r1 r2 in
      let expected = ric (2, Int 0, Infinity, ("", 5)) in
      print_endline ("(" ^ to_string r1 ^ ") ⊕ (" ^ to_string r2 ^ ") = " ^ to_string result);
      RIC.equal result expected


    (* Tests for set difference *)
    let%test "remove_top_from_top" =
      let ric1 = Top in
      let ric2 = Top in
      let result = remove ~this:ric1 ~from:ric2 in
      print_endline ("remove: " ^ to_string ric2 ^ " \\ " ^ to_string ric1 ^ " = [" ^ String.concat ~sep:"; " (List.map result ~f:to_string) ^ "]");
      List.equal equal result []

    let%test "remove_bottom_from_top" =
      let ric1 = Bottom in
      let ric2 = Top in
      let result = remove ~this:ric1 ~from:ric2 in
      print_endline ("remove: " ^ to_string ric2 ^ " \\ " ^ to_string ric1 ^ " = [" ^ String.concat ~sep:"; " (List.map result ~f:to_string) ^ "]");
      List.equal equal result [Top]

    let%test "remove_top_from_bottom" =
      let ric1 = Top in
      let ric2 = Bottom in
      let result = remove ~this:ric1 ~from:ric2 in
      print_endline ("remove: " ^ to_string ric2 ^ " \\ " ^ to_string ric1 ^ " = [" ^ String.concat ~sep:"; " (List.map result ~f:to_string) ^ "]");
      List.equal equal result []

    let%test "remove_bottom_from_bottom" =
      let ric1 = Bottom in
      let ric2 = Bottom in
      let result = remove ~this:ric1 ~from:ric2 in
      print_endline ("remove: " ^ to_string ric2 ^ " \\ " ^ to_string ric1 ^ " = [" ^ String.concat ~sep:"; " (List.map result ~f:to_string) ^ "]");
      List.equal equal result []

    let%test "remove_singleton_from_itself" =
      let ric1 = ric (0, Int 0, Int 0, ("", 5)) in
      let ric2 = ric1 in
      let result = remove ~this:ric1 ~from:ric2 in
      print_endline ("remove: " ^ to_string ric2 ^ " \\ " ^ to_string ric1 ^ " = [" ^ String.concat ~sep:"; " (List.map result ~f:to_string) ^ "]");
      List.equal equal result []

    let%test "remove_singleton_not_in" =
      let ric1 = ric (0, Int 0, Int 0, ("", 9)) in
      let ric2 = ric (2, Int 0, Int 3, ("", 1)) in
      let result = remove ~this:ric1 ~from:ric2 in
      print_endline ("remove: " ^ to_string ric2 ^ " \\ " ^ to_string ric1 ^ " = [" ^ String.concat ~sep:"; " (List.map result ~f:to_string) ^ "]");
      List.equal equal result [ric2]

    let%test "remove_partial_overlap" =
      let ric1 = ric (2, Int 0, Int 2, ("", 1)) in
      let ric2 = ric (2, Int 0, Int 4, ("", 1)) in
      let result = remove ~this:ric1 ~from:ric2 in
      print_endline ("remove: " ^ to_string ric2 ^ " \\ " ^ to_string ric1 ^ " = [" ^ String.concat ~sep:"; " (List.map result ~f:to_string) ^ "]");
      List.equal equal result [ric (2, Int 3, Int 4, ("", 1))]

    let%test "remove_entire_range" =
      let ric1 = ric (2, Int 0, Int 4, ("", 1)) in
      let ric2 = ric1 in
      let result = remove ~this:ric1 ~from:ric2 in
      print_endline ("remove: " ^ to_string ric2 ^ " \\ " ^ to_string ric1 ^ " = [" ^ String.concat ~sep:"; " (List.map result ~f:to_string) ^ "]");
      List.equal equal result []

    let%test "remove_smaller_range" =
      let ric1 = ric (2, Int 0, Int 2, ("", 1)) in
      let ric2 = ric (2, Int 0, Int 4, ("", 1)) in
      let result = remove ~this:ric1 ~from:ric2 in
      print_endline ("remove: " ^ to_string ric2 ^ " \\ " ^ to_string ric1 ^ " = [" ^ String.concat ~sep:"; " (List.map result ~f:to_string) ^ "]");
      List.equal equal result [ric (2, Int 3, Int 4, ("", 1))]

    let%test "remove_different_relative_offsets" =
      let ric1 = ric (2, Int 0, Int 2, ("", 2)) in
      let ric2 = ric (2, Int 0, Int 4, ("x", 1)) in
      let result = remove ~this:ric1 ~from:ric2 in
      print_endline ("remove: " ^ to_string ric2 ^ " \\ " ^ to_string ric1 ^ " = [" ^ String.concat ~sep:"; " (List.map result ~f:to_string) ^ "]");
      List.equal equal result []

    let%test "remove_with_relative_offsets" =
      let ric1 = ric (3, Int 1, Int 2, ("x", 1)) in
      let ric2 = ric (3, Int 0, Int 4, ("x", 1)) in
      let result = remove ~this:ric1 ~from:ric2 in
      print_endline ("remove: " ^ to_string ric2 ^ " \\ " ^ to_string ric1 ^ " = [" ^ String.concat ~sep:"; " (List.map result ~f:to_string) ^ "]");
      List.equal equal result [ric (3, Int 0, Int 0, ("x", 1)); ric (3, Int 3, Int 4, ("x", 1))]

    let%test "remove_full_complement_overlap" =
      let this = ric (2, Int 0, Int 2, ("", 1)) in
      let from = ric (1, Int 0, Int 6, ("", 1)) in
      let result = remove ~this ~from in
      let expected = [ric (2, Int 0, Int 2, ("", 2)); ric (0, Int 0, Int 0, ("", 7))] in
      print_endline ("remove: " ^ to_string from ^ " \\ " ^ to_string this ^ " = [" ^ String.concat ~sep:"; " (List.map result ~f:to_string) ^ "]");
      List.equal equal result expected


  end)
end)

    
    