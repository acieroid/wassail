open Core 
open Maths

type bitfield = Bitfield.t

(* TODO: put this somewhere else *)
let (|>>) (c, i : 'a * 'b) (f : 'a -> 'b -> 'c) : 'c = f c i

(** {1 Reduced Interval–Congruence (RIC)}

    {b Purpose.} An abstract domain for machine integers and pointers that
    combines a {i congruence} component (arithmetic progressions of the form
    [stride * ℤ + offset]) with an {i interval} component (closed range of steps).
    Values are kept in a {i reduced} normal form so that standard lattice laws hold
    by construction.

    {b Universe and notation}
    - [⊤] (Top) is the set of all signed 32‑bit integers; [⊥] (Bottom) is the empty set.
    - A concrete RIC is written [s[l,u]+o] and denotes { [o + s·k | k ∈ [l..u]] } with
      [l], [u] in extended integers (±∞). Offsets may be absolute [("", c)] or
      relative [(var, c)] to a base symbol (e.g., ["g0"]).

    {b Lattice}
    - Order: [x ≤ y] iff [x ⊓ y = x] (equivalently [x ⊔ y = y]).
    - [⊓] is intersection; [⊔] is union over‑approximation.
    - Widening [▽] follows the componentwise widenings of congruences and intervals.

    {b Reduction invariant}
    - Finite lower bounds are shifted so that the step interval starts at 0.
    - [s = 0] represents a singleton (exact value).
    - [s = 1 ∧ l = −∞ ∧ u = +∞] is canonical [⊤].

    {b Relative offsets}
    - RICs may carry a symbolic base in the offset. Binary operators first drop the
      base (operate absolutely), then reattach the base when both sides are comparable.
    - When bases are incomparable, operations conservatively return [⊤] (join) or [⊥]
      (meet) as appropriate.
*)
module RIC = struct
  
  type congruence = Congruence.t
  type interval = Interval.t

  (** {2 RIC values}
      A reduced value [stride * [lower_bound..upper_bound] + offset]. The reduction
      invariant maintains [lower_bound = 0] when finite, and folds special cases into
      canonical [⊤]/[⊥]/singleton forms. *)
  type ric = {
    stride : int32;
    lower_bound : ExtendedInt.t;
    upper_bound : ExtendedInt.t;
    offset : string * int32;
  }
  [@@deriving sexp, compare, equal]

  (** Abstract values in the RIC domain.
      - [Top] : all 32‑bit integers
      - [Bottom] : empty set
      - [RIC r] : concrete reduced interval‑congruence *)
  type t =
    | Top
    | Bottom
    | RIC of ric
  [@@deriving sexp, compare]

  (** [reduce r]
      Normalize a RIC:
      1) shift offsets so a finite lower bound becomes 0;
      2) collapse [⊤]/[⊥]/singleton cases;
      3) tighten bounds when stride/offset imply overflow toward ±∞.
      Idempotent. *)
  let rec reduce (r : t) : t =
    match r with 
    | Top -> Top 
    | Bottom -> Bottom 
    | RIC {stride = s; lower_bound = Int l; upper_bound = u; offset = ("", o)} when Int32.(s*l+o < 0b11000000000000000000000000000000l) ->
      reduce (RIC {stride = s; lower_bound = NegInfinity; upper_bound = u; offset = ("", o)})
    | RIC {stride = s; lower_bound = l; upper_bound = Int u; offset = ("", o)} when Int32.(s*u+o > 0b01100000000000000000000000000000l) ->
      reduce (RIC {stride = s; lower_bound = l; upper_bound = Infinity; offset = ("", o)})
    | RIC {stride = s; lower_bound = l; upper_bound = u; offset = o} ->
      if ExtendedInt.(u < l) then Bottom else 
      if (Int32.(s = 1l) && ExtendedInt.(NegInfinity = l) && ExtendedInt.(Infinity = u)) then 
        Top 
      else if Int32.(s = 0l) then 
        RIC {stride = 0l; lower_bound = Int 0l; upper_bound = Int 0l; offset = o} 
      else
        let new_offset =
          match o, l with 
          | (var, o), Int l -> var, Int32.(o + s * l)
          | (var, o), NegInfinity -> var, Int32.(o % s)
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
          | _, Int l, Int u -> Int Int32.(u - l)
          | (_, o), NegInfinity, u -> 
            ExtendedInt.(u + (Int Int32.(o / s - if (o % s <> 0l) && o < 0l then 1l else 0l)))
          | _ -> assert false
        in
        let new_stride = 
          if ExtendedInt.(new_lower = new_upper) then 
            0l (* Singleton *)
          else 
            s 
        in
        RIC {stride = new_stride; lower_bound = new_lower; upper_bound = new_upper; offset = new_offset}

  (** [ric (s,l,u,o)]
      Smart constructor that builds then immediately reduces the RIC. *)
  let ric (r : int32 * ExtendedInt.t * ExtendedInt.t * (string * int32)) : t =
    reduce (
      match r with 
      | s, l, u, o -> RIC {stride = s; lower_bound = l; upper_bound = u; offset = o}
    )

  let zero = ric (0l, Int 0l, Int 0l, ("", 0l))

  let one = ric (0l, Int 0l, Int 0l, ("", 1l))

  (** [to_string r]
      Normalized math‑style printer: e.g. ["2[0,3]+(x+1)"] or ["⊤"/"⊥"]. *)
  let to_string ?(no_reduction : bool = false) (r : t) : string =
    let r = if no_reduction then r else reduce r in
    match r with
    | Top -> "⊤"
    | Bottom -> "⊥"
    | RIC {stride = s; lower_bound = l; upper_bound = u; offset = o} ->
      let offset = 
        match o with 
        | (var, i) -> 
          if Int32.(i = 0l) then
            (if String.(var <> "") then "+" else "") ^ var
          else if String.is_empty var then
            Int32.(if i > 0l then "+" else if i < 0l then "-" else "") ^ Int32.to_string (Int32.abs i)
          else
            "+(" ^ var ^ ((if Int32.compare i 0l > 0 then "+" else "") ^ Int32.to_string i) ^ ")"
      in
      let interval = Interval.to_string ~no_reduction ((l, u) |>> Interval.make) in
      let stride = if Int32.(s = 1l) then "" else Int32.to_string s in
      let out = 
        if no_reduction then 
          stride ^ interval ^ offset 
        else
          match stride, interval, offset with 
          | "0", _, "" -> "0"
          | "0", _, o -> if String.equal "+" (String.sub o ~pos:0 ~len:1) then String.sub o ~pos:1 ~len:(String.length o - 1) else o
          | "", "]-∞,∞[", _ | "", "ℤ", _ -> "⊤"
          | _ -> stride ^ interval ^ offset
      in
      out

  (** All negative integers. *)
  let negative_integers = ric (1l, NegInfinity, Int (-1l), ("", 0l))

  (** All non‑negative integers. *)
  let positive_integers = ric (1l, Int 0l, Infinity, ("", 0l))

  (** [relative_ric var]
      Singleton [0] expressed relative to base symbol [var]. *)
  let relative_ric (var : string) : t =
    ric (0l, Int 0l, Int 0l, (var, 0l))

  let of_int32 (i : int32) : t = ric (0l, Int 0l, Int 0l, ("", i))
  let constant : int32 -> t = of_int32 

  (** [spans_neg_inf_to_pos_inf r]
      [true] iff [r] covers ]−∞,∞[ (or is [Top]). *)
  let spans_neg_inf_to_pos_inf (r : t) : bool =
    match r with
    | Top
    | RIC {lower_bound = NegInfinity; upper_bound = Infinity; _} -> true
    | _ -> false

  (** [is_singleton r]
      [true] iff [r] denotes exactly one value (i.e., reduced stride = 0). *)
  let is_singleton (r : t) : bool =
    let r = reduce r in
    match r with
    | RIC {stride = 0l; _} -> true
    | _ -> false

  (** [extract_relative_offset r]
      Returns the base symbol used in [r]'s offset (or ["" ] if absolute). *)
  let extract_relative_offset (r : t) : string =
    match r with
    | Bottom | Top -> ""
    | RIC {offset = (relative, _); _} -> relative

  (** [is_stack r]
      Heuristic predicate: [true] when the base symbol is ["g0"]. *)
  let is_stack (r : t) : bool =
    match r with
    | RIC {offset = (relative, _); _} -> String.equal relative "g0"
    | _ -> false
  
  (** [set_relative_offset r name]
      Attach base symbol [name] to an absolute RIC. *)
  let set_relative_offset (r : t) ~(relative_offset : string) : t =
    match r with
    | Top -> Top
    | RIC {stride = s; lower_bound = l; upper_bound = u; offset = ("", o)} ->
      ric (s, l, u, (relative_offset, o))
    | _ -> Log.error ("set_relative_offset : " ^ to_string r ^ " ... " ^ relative_offset);
      assert false

  (** [remove_relative_offset r]
      Drop the base symbol, making the offset absolute. *)
  let remove_relative_offset (r : t) : t =
    match r with
    | Top -> Top
    | Bottom -> Bottom
    | RIC {stride = s; lower_bound = l; upper_bound = u; offset = (_, o)} -> 
      ric (s, l, u, ("", o))

  (** [of_congruence_and_interval c i]
      Combine components into a (reduced) RIC. Relative offsets are preserved; [⊤]/[⊥]
      propagate naturally. [stride=0] produces a singleton. *)
  let of_congruence_and_interval (c : congruence) (i : interval) : t =
    let c = if Congruence.(Top = c) then Congruence.Top else if Congruence.(Bottom = c) then Bottom else c in
    match c, i with
    | Top, Top -> Top
    | Top, Interval {lower_bound = l; upper_bound = u} -> ric (1l, l, u, ("", 0l))
    | Bottom, _ | _, Bottom -> Bottom
    | Congruence {stride = 0l; offset = o}, Interval {lower_bound = NegInfinity; upper_bound = Infinity} -> ric (0l, Int 0l, Int 0l, o)
    | Congruence {stride = 0l; offset = ("", o)}, Interval {lower_bound = NegInfinity; upper_bound = Int u} -> 
      if Int32.(u >= o) then
        ric (0l, Int 0l, Int 0l, ("", o))
      else
        Bottom
    | Congruence {stride = 0l; offset = ("", o)}, Interval {lower_bound = Int l; upper_bound = Infinity} -> 
      if Int32.(l <= o) then
        ric (0l, Int 0l, Int 0l, ("", o))
      else
        Bottom
    | Congruence {stride = 0l; offset = ("", o)}, Interval {lower_bound = Int l; upper_bound = Int u} -> 
      if Int32.(l <= o) && Int32.(u >= o) then
        ric (0l, Int 0l, Int 0l, ("", o))
      else
        Bottom
    | Congruence {stride = 0l; offset = (var, _)}, _ when String.is_empty var -> Top
    | Congruence {stride = s; offset = (var, o)}, Top -> ric (s, ExtendedInt.NegInfinity, ExtendedInt.Infinity, (var, o))
    | Congruence {stride = s; offset = (var, o)}, Interval {lower_bound = l; upper_bound = u} ->
      if String.is_empty var then
        let lower = 
          ExtendedInt.divide_ceiling ExtendedInt.(l - Int o) (ExtendedInt.Int s) in
        let upper = 
          ExtendedInt.divide_floor ExtendedInt.(u - Int o) (ExtendedInt.Int s) in
        ric (s, lower, upper, (var, o))
      else
        (* Since the congruence is unknown due to the variable offset, we must include
        all integers in the interval. This is an over-approximation. *)
        ric (1l, l, u, ("", 0l))

  (** [to_congruence_and_interval r]
      Decompose a (possibly reduced) RIC back to its components. The congruence
      offset is normalized modulo [stride] when [stride≠0]. *)
  let to_congruence_and_interval (r : t) : congruence * interval =
    match r with 
    | Top -> Top, Top
    | Bottom -> Bottom, Bottom 
    | RIC {stride = s; offset = (var, o); lower_bound = l; upper_bound = u} ->
      (s, (var, if Int32.(s = 0l) then o else Int32.(o % s))) 
      |>> Congruence.make,
      if String.is_empty var then
        (ExtendedInt.(Int o + (Int s * l)), ExtendedInt.(Int o + (Int s * u))) 
        |>> Interval.make
      else
        Top
      
  (** [equal r1 r2]
      Canonical equality after [reduce]. *)
  let equal (ric1 : t) (ric2 : t) : bool =
    let ric1, ric2 = reduce ric1, reduce ric2 in
    match ric1, ric2 with 
    | Top, Top | Bottom, Bottom -> true
    | RIC {stride = s1; lower_bound = l1; upper_bound = u1; offset = (v1, o1)},  
      RIC {stride = s2; lower_bound = l2; upper_bound = u2; offset = (v2, o2)} ->
        Int32.(s1 = s2) 
        && ExtendedInt.(l1 = l2 && u1 = u2)
        && String.(v1 = v2) && Int32.(o1 = o2)
    | _ -> false

  let (=) = equal
  let (<>) (r1 : t) (r2 : t) : bool = not (r1 = r2)

  (** [comparable_offsets r1 r2]
      [true] iff base symbols match (or both are absolute). *)
  let comparable_offsets (ric1 : t) (ric2 : t) : bool =
    match ric1, ric2 with
    | RIC {offset = (v1, _); _},
      RIC {offset = (v2, _); _} -> String.(v1 = v2) 
    | _ -> true

  (** [meet r1 r2]
      Intersection. If bases differ, returns [⊥]. Otherwise intersects components after
      temporarily dropping the base symbol, then re‑attaches it. *)
  let meet (ric1 : t) (ric2 : t) : t =
    if comparable_offsets ric1 ric2 then
      let relative_offset = 
        match ric1, ric2 with
        | RIC _, _ -> extract_relative_offset ric1
        | _, RIC _ -> extract_relative_offset ric2
        | _ -> "" in
      let ric1 = remove_relative_offset ric1
      and ric2 = remove_relative_offset ric2 in
      let (c1, i1) = to_congruence_and_interval ric1
      and (c2, i2) = to_congruence_and_interval ric2 in
      let m = 
        (Congruence.meet c1 c2, Interval.meet i1 i2) 
        |>> of_congruence_and_interval in
      match m with
      | Top | Bottom -> m
      | _ -> set_relative_offset m ~relative_offset
    else
      Bottom

  (** [negative_part r]
      Intersection of [r] with negative integers. *)
  let negative_part (r : t) : t =
    assert (extract_relative_offset r |> String.is_empty);
    meet r negative_integers

  (** [positive_part r]
      Intersection of [r] with non‑negative integers. *)
  let positive_part (r : t) : t =
    assert (extract_relative_offset r |> String.is_empty);
    meet r positive_integers
  
  (** [are_disjoint r1 r2]
      [true] iff [r1 ⊓ r2 = ⊥]. *)
  let are_disjoint (ric1 : t) (ric2 : t) : bool =
    assert (comparable_offsets ric1 ric2);
    (meet ric1 ric2) = Bottom

  (** [join r1 r2]
      Least upper bound. Requires comparable bases; otherwise [⊤]. Joins components
      then reinstates the base symbol. *)
  let join (ric1 : t) (ric2 : t) : t =
    if comparable_offsets ric1 ric2 then
      let relative_offset = 
        match ric1, ric2 with
        | RIC _, _ -> extract_relative_offset ric1
        | _, RIC _ -> extract_relative_offset ric2
        | _ -> "" in
      let ric1 = remove_relative_offset ric1
      and ric2 = remove_relative_offset ric2 in
      let (c1, i1) = to_congruence_and_interval ric1
      and (c2, i2) = to_congruence_and_interval ric2 in
      let j = 
        (Congruence.join c1 c2, Interval.join i1 i2) 
        |>> of_congruence_and_interval in
      match j with
      | Top | Bottom -> j
      | _ -> set_relative_offset j ~relative_offset
    else
      Top

  (** [complement r]
      Set‑theoretic complement returned as a small list of RICs. For bounded
      progressions, includes the exterior ranges and (when [stride>0]) the interleaving
      gaps between consecutive addresses. [⊤] → [[⊥]]; [⊥] → [[⊤]]. *)
  let complement (r : t) : t list =
    match reduce r with
    | Top -> [Bottom]
    | Bottom -> [Top]
    | RIC {stride = s; lower_bound = l; upper_bound = u; offset = (v, o)} ->
      let inferior_RIC = 
        if ExtendedInt.(l = NegInfinity) then []
        else [ric (1l, NegInfinity, ExtendedInt.(l - Int 1l), (v, o))]
      in
      let superior_RIC =
        if ExtendedInt.equal u Infinity then []
        else [ric (1l,
                   ExtendedInt.(Int (Int32.(o + if s = 0l then 1l else s)) + (Int s * u)),
                   Infinity,
                   (v, 0l))]
      in
      let overlapping_complement =
        if Int32.(s = 0l) then []
        else List.init (Int32.to_int_exn s - 1) ~f:(fun i -> ric (s, l, u, (v, Int32.(o + (of_int_exn i) + 1l))))
      in
      inferior_RIC @ overlapping_complement @ superior_RIC

  (** [add_offset r c]
      Shift the offset by a constant [c]. *)
  let add_offset (r : t) (c : int32) : t =
    match r with
    | Top -> Top 
    | Bottom -> Bottom
    | RIC {stride = s; lower_bound = l; upper_bound = u; offset = (v,o)} -> 
      ric (s, l, u, (v, Int32.(o + c)))

  (** [remove_lower_bound r]
    Over-approximate [r] by dropping its lower bound.
    The result keeps only the maximal concrete value of [r], so the stride
    is weakened to 1. *)
  let remove_lower_bound (r : t) : t =
    let r = reduce r in
    match r with 
    | Top -> Top 
    | Bottom -> Bottom 
    | RIC {stride = s; lower_bound = _; upper_bound = u; offset = o} -> 
      ric (1l, NegInfinity, ExtendedInt.(Int s * u), o)

  (** [remove_upper_bound r]
    Over-approximate [r] by dropping its upper bound.
    The result keeps only the minimal concrete value of [r], so the stride
    is weakened to 1. *)
  let remove_upper_bound (r : t) : t =
    let r = reduce r in
    match r with 
    | Top -> Top 
    | Bottom -> Bottom 
    | RIC {stride = s; lower_bound = l; upper_bound = _; offset = o} -> 
      ric (1l, ExtendedInt.(Int s * l), Infinity, o)

  (** [widen r ~relative_to]
      Componentwise widening after removing relative bases; the base is reattached when
      present and comparable. Incomparable bases yield [⊤]. *)
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
      let w =
        (Congruence.widen c1 c2, Interval.widen i1 i2) 
        |>> of_congruence_and_interval in
      match w with
      | Top | Bottom -> w
      | _ -> set_relative_offset w ~relative_offset
    else
      Top

  (** [partially_accessed ~by ~size]
      Addresses within [by] that may be {i partially} touched by a memory access of
      [size] bytes. Returns a finite list of RICs; may include [⊤] when unavoidable
      imprecision arises (e.g., dense progressions spanning both sides of the origin). *)
  let partially_accessed ~(by : t) ~(size : int32) : t list =
    let r = reduce by in
    let remove_fully_accessed (accessed : t list) : t list =
      if Int32.(size <> 4l) || (not (is_singleton r)) && (match r with | RIC {stride = s; _} -> Int32.(s < 4l) | _ -> false) then
        List.dedup_and_sort ~compare accessed
      else
        List.dedup_and_sort ~compare (List.filter ~f:(fun x -> not (equal x r)) accessed)
    in
    if r = Top then 
      [Top]
    else if spans_neg_inf_to_pos_inf r then
      match r with
      | RIC {stride = s; _} when Int32.(size <> 4l) && Int32.(s < size + 4l) -> [Top]
      | RIC _ -> 
        List.init (Int32.to_int_exn size + 3) ~f:(fun n -> Int32.of_int_exn (n-3))
        |> List.fold ~init:[] ~f:(fun acc x -> add_offset r x :: acc)
        |> remove_fully_accessed
      | _ -> assert false
    else
      let rec aux (i : int32) (acc : t list) : t list =
        match i, r with
        | _, Top -> [Top] 
        | _, Bottom -> []
        | i, RIC {stride = s; lower_bound = Int _; _} when Int32.(i = (if s = 0l then size else min size (s - 3l))) -> acc
        | i, RIC {stride = s; lower_bound = NegInfinity; _} when Int32.(i + 3l + size - 1l = max (-4l) (size - s - 1l)) -> acc
        | i, RIC {stride = s; lower_bound = Int l; upper_bound = Int u; offset = (v, o)} ->
          aux Int32.(i + 1l) (ric (s, Int l, (if Int32.(s <> 0l) then Int Int32.(l + ((u - l + 1l) * s - i + size - 1l) / s - 1l) else Int 0l), (v, Int32.(o + i))) :: acc)
        | i, RIC {upper_bound = Infinity; _} ->
          aux Int32.(i + 1l) (add_offset r i :: acc)
        | i, RIC {lower_bound = NegInfinity; _} ->
          let offset = Int32.(i + 3l + size - 1l) in
          aux Int32.(i - 1l) (add_offset r offset :: acc)
        | _ -> assert false
      in
      aux (-3l) [] |> remove_fully_accessed

  
  (** Result of a memory access: fully touched region and partially touched regions. *)
  type accessed_memory = {
    fully : t;
    partially : t list
  }
  
  (** [accessed ~value_set ~size]
      Split a memory access into fully vs partially accessed RICs. For 32‑bit loads,
      [fully] is kept when stride ≥ 4 (or the set is a singleton); the remainder is
      reported in [partially]. *)
  let accessed ~(value_set : t) ~(size : int32) : accessed_memory =
    let stride = match value_set with | RIC {stride = s; _} -> s | Top -> 1l | Bottom -> 0l in
    let fully = if Int32.(size = 4l && (stride >= 4l || is_singleton value_set)) then value_set else Bottom in
    let partially = partially_accessed ~by:value_set ~size:size in
    {fully; partially}
  
  (** [may_overlap ~store_size ~load_size ~store_ric ~load_ric]
    Returns [true] iff a store and a load may access at least one common
    byte in memory. *)
  let may_overlap ~(store_size : int32) ~(load_size : int32) ~(store_ric : t) ~(load_ric : t) =
    let touched_by_store = 
      List.init (Int32.to_int_exn store_size) ~f:(fun i -> add_offset store_ric (Int32.of_int_exn i))
    and touched_by_load =
       List.init (Int32.to_int_exn load_size) ~f:(fun i -> add_offset load_ric (Int32.of_int_exn i)) in
    List.exists touched_by_store ~f:(fun addr_touched_by_store ->
      List.exists touched_by_load ~f:(fun addr_touched_by_load ->
        if addr_touched_by_store = Bottom || addr_touched_by_load = Bottom then
          false
        else if comparable_offsets addr_touched_by_store addr_touched_by_load then
          not (are_disjoint addr_touched_by_store addr_touched_by_load)
        else
          true))

  (** [plus r1 r2]
      Abstract addition with base‑symbol composition. Components use [sum], then the
      resulting base is reattached via [add_relative_offsets]. *)
  let plus (ric1 : t) (ric2 : t) : t =
    match ric1, ric2 with
    | Top, _ | _, Top -> Top
    (* | Bottom, Bottom -> Bottom
    | Bottom, _ -> ric2
    | _, Bottom -> ric1 *)
    | Bottom, _ | _, Bottom -> Bottom
    | _ ->
      let relative_offset = 
        (extract_relative_offset ric1, extract_relative_offset ric2)
        |>> add_relative_offsets in
      let c1, i1 = ric1 |> remove_relative_offset |> to_congruence_and_interval
      and c2, i2 = ric2 |> remove_relative_offset |> to_congruence_and_interval in
      (Congruence.(c1 + c2), Interval.(i1 + i2))
      |>> of_congruence_and_interval
      |> set_relative_offset ~relative_offset
  let (+) = plus

  (** [negative r]
      Pointwise negation: flip bounds and offset; preserve (transformed) relativeness. *)
  let negative (r : t) : t =
    match r with
    | Top -> Top
    | Bottom -> Bottom
    | RIC {stride = s; lower_bound = l; upper_bound = u; offset = (v, o)} ->
      let new_offset = negate_relative_offset v in
      ric (s, ExtendedInt.(Int (-1l) * u), ExtendedInt.(Int (-1l) * l), (new_offset, Int32.(- o)))

  (** [minus r1 r2]
      Compute [r1 + (−r2)]. *)
  let minus (ric1 : t) (ric2 : t) : t = ric1 + (negative ric2)
  let (-) = minus

  (** [remove ~this ~from]
      Set difference returned as a small list of RICs. *)
  let remove ~(this : t) ~(from : t) : t list =
    if comparable_offsets this from then
      this |> complement 
      |> List.map ~f:(meet from)
      |> List.filter ~f:(fun r -> r <> Bottom)
    else
      []

  (** [update_relative_offset ~ric_ ~actual_values]
      Resolve symbolic base names by substituting concrete RICs from [actual_values].
      Concatenated bases like ["negx+y"] are interpreted as signed components. *)
  let update_relative_offset ~(ric_ : t) ~(actual_values : t String.Map.t) : t =
    ric_ 
    |> extract_relative_offset 
    |> String.split_on_chars ~on:['+'] 
    |> List.fold
        ~init:(remove_relative_offset ric_)
        ~f:(fun acc offset ->
          let is_negative = String.is_prefix offset ~prefix:"neg" in
          let var = if is_negative then String.drop_prefix offset 3 else offset in
          match Map.find actual_values var with
          | None -> acc + (relative_ric offset)
          | Some r -> 
            acc + if is_negative then negative r else r)
            

  (** [to_bitfield r]
      Sound conversion from a sign‑uniform RIC to the bitfield abstraction. May yield
      [Top] when information is insufficient (e.g., non power‑of‑two stride with
      unbounded side). Precondition: all values share the same sign. *)
  let to_bitfield (r : t) : bitfield =
    let r = reduce r in
    match r with
    | Bottom -> Bottom
    | Top -> Top
    | RIC {offset = (o, _); _} when String.(o <> "") -> Bitfield.Top
    | RIC {stride = s; lower_bound = NegInfinity; upper_bound = Infinity; offset = (_, o)} 
    | RIC {lower_bound = NegInfinity; upper_bound = Int _; stride = s; offset = ("", o)}
    | RIC {lower_bound = Int _; upper_bound = Infinity; stride = s; offset = ("", o)} ->
      let nb_unaffected_bits = Int32.(ctz s) in
      let constant_bits = Int32.(o % (shift_left 1l nb_unaffected_bits)) in
      let ones = Int32.(shift_left 0xFFFFFFFFl nb_unaffected_bits + constant_bits) in
      let ones = Int32.shift_right_logical (Int32. shift_left ones 1) 1 
        |> Int32.(+) (if negative_part r <> Bottom then 0x80000000l else 0l) in
      let zeros = Int32.(0xFFFFFFFFl - constant_bits) in
      let zeros = Int32.shift_right_logical (Int32. shift_left zeros 1) 1 
        |> Int32.(+) (if positive_part r <> Bottom then 0x80000000l else 0l) in
      Bitfield.Bit {ones; zeros}
    | RIC {stride = s; lower_bound = Int l; upper_bound = Int u; offset = (_, o)} -> 
      let number_of_steps = Int32.to_int_exn Int32.(u - l) in
      let lowest_value = Int32.(s * l + o) in
      let bit_flips = Maths.Binary.bit_flips lowest_value ~increment:s 
        |> List.map ~f:(fun n -> Int.(n <> 0) && n <= number_of_steps) in
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

  
  (** [of_bitfield bf]
      Over‑approximating conversion from bitfield to RIC: uses the constant part as an
      offset and the number of trailing variable bits to recover a stride and step range. *)
  let of_bitfield (bf : bitfield) : t =
    match bf with
    | Top -> Top
    | Bottom -> Bottom
    | Bit bf ->
      let r = Bitfield.to_RIC (Bit bf) in
      ric (r.stride, r.lower_bound, r.upper_bound, r.offset)

  (** [binop_logical op r1 r2]
      Helper: split each operand by sign, convert to bitfields, apply [op], then join
      the four quadrants back in RIC space. *)
  let binop_logical (binop : bitfield -> bitfield -> bitfield) (ric1 : t) (ric2 : t) : t =
    let pos1 = meet ric1 positive_integers
    and neg1 = meet ric1 negative_integers
    and pos2 = meet ric2 positive_integers
    and neg2 = meet ric2 negative_integers in
    let bf_neg1 = to_bitfield neg1
    and bf_pos1 = to_bitfield pos1
    and bf_pos2 = to_bitfield pos2
    and bf_neg2 = to_bitfield neg2 in
    let bf_pp = binop bf_pos1 bf_pos2
    and bf_pn = binop bf_pos1 bf_neg2
    and bf_nn = binop bf_neg1 bf_neg2
    and bf_np = binop bf_neg1 bf_pos2 in
    let pos_pos = of_bitfield bf_pp
    and pos_neg = of_bitfield bf_pn
    and neg_neg = of_bitfield bf_nn
    and neg_pos = of_bitfield bf_np in
    join (join pos_pos pos_neg) (join neg_neg neg_pos)

  (** [and_ r1 r2] — bitwise AND via bitfield composition over sign splits. *)
  let and_ = binop_logical Bitfield.and_
  let (&.) = and_

  (** [or_ r1 r2] — bitwise OR via bitfield composition over sign splits. *)
  let or_ = binop_logical Bitfield.or_
  let (|.) = or_

  (** [xor_ r1 r2] — bitwise XOR via bitfield composition over sign splits. *)
  let xor_ = binop_logical Bitfield.xor_
  let (<+>) = xor_

  (** [shift shift_op r1 r2]
      Generic shifter: split by sign, convert to bitfields, apply [shift_op] with a
      non‑negative shift amount, then join. *)
  let shift (shift_op : bitfield -> bitfield -> bitfield) (ric1 : t) (shift : t) : t =
    let ric1 = if String.(extract_relative_offset ric1 = "") then ric1 else Top in
    let shift = if String.(extract_relative_offset shift = "") then shift else Top in
    let pos = meet ric1 positive_integers
    and neg = meet ric1 negative_integers in
    let bf1_pos = to_bitfield pos
    and bf1_neg = to_bitfield neg
    and bf2 = to_bitfield (meet shift positive_integers) in
    let bf_pos = shift_op bf1_pos bf2
    and bf_neg = shift_op bf1_neg bf2 in
    join (of_bitfield bf_pos) (of_bitfield bf_neg)

  (** [shift_left r1 r2] — arithmetic left shift (join over allowed amounts). *)
  let shift_left = shift Bitfield.shift_left
  let (<<) = shift_left

  (** [shift_right_u r1 r2] — logical right shift (join over allowed amounts). *)
  let shift_right_u = shift Bitfield.shift_right_unsigned
  let (>>.) = shift_right_u

  (** [shift_right_s r1 r2] — arithmetic (sign‑extending) right shift (join over amounts). *)
  let shift_right_s = shift Bitfield.shift_right_signed
  let (>>-) = shift_right_s

  (** [may_be_false r]
      Interprets [r] as a boolean: [true] iff 0 is compatible with [r]. *)
  let may_be_false (r : t) : bool =
    match r with
    | Top -> true
    | Bottom -> false
    | RIC {offset = (s, _); _} when String.(s <> "") -> true
    | _ -> not (are_disjoint r zero)
  
  (** [may_be_true r]
      Interprets [r] as a boolean: [true] iff some non‑zero value is compatible with [r]
      (or [Top]). *)
  let may_be_true (r : t) : bool =
    let r = reduce r in
    match r with
    | Top -> true
    | Bottom -> false
    (* | RIC {offset = (s, _); _} when String.(s <> "") -> true *)
    | r -> r <> zero

end


module RICSet = struct
  include Set
  module S = struct
    include Set.Make(RIC)
    let to_string (r : t) : string =
      String.concat ~sep:"; " (List.map ~f:RIC.to_string (Set.to_list r))
  
  let add ~(ric : RIC.t) (set : t) : t =
    add set ric
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

  let%test "reduce_top" =
    print_endline ("[RIC.reduce]     ⊤ → " ^ to_string (reduce Top));
    RIC.equal (reduce Top) Top

  let no_reduction = true 

  let%test "reduce 2[-inf, -40]-11" = 
    let r = RIC {stride=2l; lower_bound=NegInfinity; upper_bound=Int (-40l); offset=("", -9l)} in
    let red = reduce r in (* ric (2l, NegInfinity, Int (-40l), ("", -9l)) in *)
    print_endline ("[RIC.reduce]     " ^ to_string ~no_reduction r ^ " → " ^ to_string red);
    equal red (RIC {stride=2l; lower_bound=NegInfinity; upper_bound=Int (-45l); offset=("", 1l)})

  let%test "reduce_bottom" =
    print_endline ("[RIC.reduce]     ⊥ → " ^ to_string (reduce Bottom));
    equal (reduce Bottom) Bottom

  let%test "reduce_identity" =
    let r = RIC {stride=2l; lower_bound=Int 0l; upper_bound=Int 5l; offset=("", 3l)} in
    print_endline ("[RIC.reduce]     " ^ to_string ~no_reduction r ^ " → " ^ to_string (reduce r));
    equal (reduce r) r

  let%test "reduce_to_bottom" =
    let r = RIC {stride=4l; lower_bound=Int 10l; upper_bound=Int 5l; offset=("", 0l)} in
    print_endline ("[RIC.reduce]     " ^ to_string ~no_reduction r ^ " → " ^ to_string (reduce r));
    RIC.equal (reduce r) Bottom

  let%test "reduce_to_top" =
    let r = RIC {stride=1l; lower_bound=NegInfinity; upper_bound=Infinity; offset=("", 0l)} in
    print_endline ("[RIC.reduce]     " ^ to_string ~no_reduction r ^ " → " ^ to_string (reduce r));
    RIC.equal (reduce r) Top

  let%test "reduce_stride_zero" =
    let r = RIC {stride=0l; lower_bound=Int 2l; upper_bound=Int 2l; offset=("", 5l)} in
    let reduced = reduce r in
    print_endline ("[RIC.reduce]     " ^ to_string ~no_reduction r ^ " → " ^ to_string reduced);
    RIC.equal reduced (RIC {stride=0l; lower_bound=Int 0l; upper_bound=Int 0l; offset=("", 5l)})

  let%test "reduce_relative_offset" =
    let r = RIC {stride=3l; lower_bound= Int 2l; upper_bound=Int 5l; offset=("x", 4l)} in
    let reduced = reduce r in
    print_endline ("[RIC.reduce]     " ^ to_string ~no_reduction r ^ " → " ^ to_string reduced);
    RIC.equal reduced (RIC {stride=3l; lower_bound=Int 0l; upper_bound=Int 3l; offset= ("x", 10l)})

  let%test "reduce_shift_and_normalize" =
    let original = RIC {stride=4l; lower_bound= Int 1l; upper_bound=Int 2l; offset=("", 1l)} in
    let reduced = reduce original in
    print_endline ("[RIC.reduce]     " ^ to_string ~no_reduction original ^ " → " ^ to_string reduced);
    RIC.equal reduced (RIC {stride=4l; lower_bound=Int 0l; upper_bound=Int 1l; offset= ("", 5l)})

  let%test "reduce_neg_infinity" =
    let original = RIC {stride=4l; lower_bound=NegInfinity; upper_bound=Int 5l; offset=("", 10l)} in
    let reduced = reduce original in
    print_endline ("[RIC.reduce]     " ^ to_string ~no_reduction original ^ " → " ^ to_string reduced);
    RIC.equal reduced (RIC {stride=4l; lower_bound=NegInfinity; upper_bound=Int 7l; offset= ("", 2l)})

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
    print_endline ("[of congruence and interval]     (" ^ Congruence.to_string Congruence.Top ^ ", " ^ Interval.to_string Interval.Bottom ^ ") → " ^ to_string r);
    RIC.equal r Bottom

  let%test "of_congruence_and_interval_abs" =
    let c = Congruence.Congruence {stride = 2l; offset = ("", 1l)} in
    let i = Interval.Interval {lower_bound = Int 1l; upper_bound = Int 7l} in
    let r = of_congruence_and_interval c i in
    print_endline ("[of congruence and interval]     (" ^ Congruence.to_string c ^ ", " ^ Interval.to_string i ^ ") → " ^ to_string r);
    RIC.equal r (ric (2l, Int 0l, Int 3l, ("", 1l)))  (* (1 + 2*[0..3]) = {1,3,5,7} *)

  let%test "of_congruence_and_interval_relative" =
    let c = Congruence.Congruence {stride = 3l; offset = ("x", 2l)} in
    let i = Interval.Interval {lower_bound = Int 5l; upper_bound = Int 17l} in
    let r = of_congruence_and_interval c i in
    print_endline ("[of congruence and interval]     (" ^ Congruence.to_string c ^ ", " ^ Interval.to_string i ^ ") → " ^ to_string r);
    RIC.equal r (ric (1l, Int 5l, Int 17l, ("", 0l)))

  let%test "of_congruence_and_interval_stride_0" =
    let c = Congruence.Congruence {stride = 0l; offset = ("", 8l)} in
    let i = Interval.Interval {lower_bound = Int 8l; upper_bound = Int 8l} in
    let r = of_congruence_and_interval c i in
    print_endline ("[of congruence and interval]     (" ^ Congruence.to_string c ^ ", " ^ Interval.to_string i ^ ") → " ^ to_string r);
    RIC.equal r (ric (0l, Int 0l, Int 0l, ("", 8l)))

  let%test "of_congruence_and_interval_stride_0_inclusive_interval" =
    let c = Congruence.Congruence {stride = 0l; offset = ("", 8l)} in
    let i = Interval.Interval {lower_bound = Int 5l; upper_bound = Int 13l} in
    let r = of_congruence_and_interval c i in
    print_endline ("[of congruence and interval]     (" ^ Congruence.to_string c ^ ", " ^ Interval.to_string i ^ ") → " ^ to_string r);
    RIC.equal r (ric (0l, Int 0l, Int 0l, ("", 8l)))

  let%test "of_congruence_and_interval_stride_0_disjoint_interval" =
    let c = Congruence.Congruence {stride = 0l; offset = ("", 8l)} in
    let i = Interval.Interval {lower_bound = Int 16l; upper_bound = Int 32l} in
    let r = of_congruence_and_interval c i in
    print_endline ("[of congruence and interval]     (" ^ Congruence.to_string c ^ ", " ^ Interval.to_string i ^ ") → " ^ to_string r);
    RIC.equal r Bottom

  let%test "of_congruence_and_interval_tightens_absolute_congruence_to_interval" =
    let c = Congruence.Congruence {stride = 4l; offset = ("", 1l)} in
    let i = Interval.Interval {lower_bound = Int 0l; upper_bound = Int 20l} in
    let r = of_congruence_and_interval c i in
    print_endline ("[of congruence and interval]     (" ^ Congruence.to_string c ^ ", " ^ Interval.to_string i ^ ") → " ^ to_string r);
    RIC.equal r (ric (4l, Int 0l, Int 4l, ("", 1l)))

  let%test "of_congruence_and_interval_tightens_absolute_congruence_with_negative_lower_bound" =
    let c = Congruence.Congruence {stride = 4l; offset = ("", 1l)} in
    let i = Interval.Interval {lower_bound = Int (-10l); upper_bound = Int 10l} in
    let r = of_congruence_and_interval c i in
    print_endline ("[of congruence and interval]     (" ^ Congruence.to_string c ^ ", " ^ Interval.to_string i ^ ") → " ^ to_string r);
    RIC.equal r (ric (4l, Int 0l, Int 4l, ("", -7l)))

  let%test "of_congruence_and_interval_relative_offset_over_approximates_interval" =
    let c = Congruence.Congruence {stride = 4l; offset = ("x", 1l)} in
    let i = Interval.Interval {lower_bound = Int 0l; upper_bound = Int 20l} in
    let r = of_congruence_and_interval c i in
    print_endline ("[of congruence and interval]     (" ^ Congruence.to_string c ^ ", " ^ Interval.to_string i ^ ") → " ^ to_string r);
    RIC.equal r (ric (1l, Int 0l, Int 20l, ("", 0l)))

  let%test "of_congruence_and_interval_relative_offset_with_unbounded_interval_over_approximates_to_top" =
    let c = Congruence.Congruence {stride = 4l; offset = ("x", 1l)} in
    let i = Interval.Interval {lower_bound = NegInfinity; upper_bound = Infinity} in
    let r = of_congruence_and_interval c i in
    print_endline ("[of congruence and interval]     (" ^ Congruence.to_string c ^ ", " ^ Interval.to_string i ^ ") → " ^ to_string r);
    RIC.equal r Top


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
    Interval.equal i Interval.Top

  let%test "to_congruence_and_interval_stride_zero" =
    let r = ric (0l, Int 0l, Int 0l, ("", 7l)) in
    let c, i = to_congruence_and_interval r in
    print_endline ("[to congruence and interval]     (0[0, 0] + 7) → (" ^ Congruence.to_string c ^ ", " ^ Interval.to_string i ^ ")");
    Congruence.equal c (Congruence.Congruence {stride = 0l; offset = ("", 7l)}) &&
    Interval.equal i (Interval.Interval {lower_bound = Int 7l; upper_bound = Int 7l})

  let%test "equal_top_top" =
    let result = RIC.equal Top Top in
    print_endline ("[RIC.equal]     ⊤ = ⊤ → " ^ string_of_bool result);
    result

  let%test "equal_bottom_bottom" =
    let result = RIC.equal Bottom Bottom in
    print_endline ("[RIC.equal]     ⊥ = ⊥ → " ^ string_of_bool result);
    result

  let%test "equal_same_ric" =
    let r1 = RIC { stride = 2l; lower_bound = Int 0l; upper_bound = Int 3l; offset = ("", 4l) } in
    let r2 = RIC { stride = 2l; lower_bound = Int 0l; upper_bound = Int 3l; offset = ("", 4l) } in
    let result = RIC.equal r1 r2 in
    print_endline ("[RIC.equal]     " ^ to_string ~no_reduction r1 ^ " = " ^ to_string ~no_reduction r2 ^ " → " ^ string_of_bool result);
    result

  let%test "not_equal_different_stride" =
    let r1 = RIC { stride = 2l; lower_bound = Int 0l; upper_bound = Int 3l; offset = ("", 4l) } in
    let r2 = RIC { stride = 3l; lower_bound = Int 0l; upper_bound = Int 3l; offset = ("", 4l) } in
    let result = RIC.equal r1 r2 in
    print_endline ("[RIC.equal]     " ^ to_string ~no_reduction r1 ^ " = " ^ to_string ~no_reduction r2 ^ " → " ^ string_of_bool result);
    not result

  let%test "not_equal_different_bounds" =
    let r1 = RIC { stride = 2l; lower_bound = Int 0l; upper_bound = Int 3l; offset = ("", 4l) } in
    let r2 = RIC { stride = 2l; lower_bound = Int 1l; upper_bound = Int 3l; offset = ("", 4l) } in
    let result = RIC.equal r1 r2 in
    print_endline ("[RIC.equal]     " ^ to_string ~no_reduction r1 ^ " = " ^ to_string ~no_reduction r2 ^ " → " ^ string_of_bool result);
    not result

  let%test "not_equal_different_offset" =
    let r1 = RIC { stride = 2l; lower_bound = Int 0l; upper_bound = Int 3l; offset = ("x", 4l) } in
    let r2 = RIC { stride = 2l; lower_bound = Int 0l; upper_bound = Int 3l; offset = ("x", 5l) } in
    let result = RIC.equal r1 r2 in
    print_endline ("[RIC.equal]     " ^ to_string ~no_reduction r1 ^ " = " ^ to_string ~no_reduction r2 ^ " → " ^ string_of_bool result);
    not result

  let%test "equal_equivalent_after_reduce" =
    let r1 = RIC { stride = 2l; lower_bound = Int 1l; upper_bound = Int 4l; offset = ("", 3l) } in
    let r2 = RIC { stride = 2l; lower_bound = Int 0l; upper_bound = Int 3l; offset = ("", 5l) } in
    let result = RIC.equal r1 r2 in
    print_endline ("[RIC.equal]     " ^ to_string ~no_reduction r1 ^ " = " ^ to_string ~no_reduction r2 ^ " → " ^ string_of_bool result);
    result

  let%test "equal_neg_infinity_shifted_absolute" =
    let r1 = RIC { stride = 3l; lower_bound = NegInfinity; upper_bound = Int 3l; offset = ("", 10l) } in
    let r2 = RIC { stride = 3l; lower_bound = NegInfinity; upper_bound = Int 5l; offset = ("", 4l) } in
    print_endline ("[RIC.equal]     " ^ to_string ~no_reduction r1 ^ " = " ^ to_string ~no_reduction r2 ^ " → " ^ string_of_bool (equal r1 r2));
    equal r1 r2

  let%test "equal_neg_infinity_shifted_relative" =
    let r1 = RIC { stride = 3l; lower_bound = NegInfinity; upper_bound = Int 3l; offset = ("x", 10l) } in
    let r2 = RIC { stride = 3l; lower_bound = NegInfinity; upper_bound = Int 5l; offset = ("x", 4l) } in
    print_endline ("[RIC.equal]     " ^ to_string ~no_reduction r1 ^ " = " ^ to_string ~no_reduction r2 ^ " → " ^ string_of_bool (equal r1 r2));
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
    print_endline ("[RIC.meet]     " ^ to_string r1 ^ " ⊓ " ^ to_string r2 ^ " = " ^ to_string m);
    RIC.equal m expected

  let%test "meet_top_and_r" =
    let r = ric (2l, Int 0l, Int 4l, ("", 5l)) in
    let m = meet Top r in
    print_endline ("[RIC.meet]     ⊤ ⊓ " ^ to_string r ^ " → " ^ to_string m);
    RIC.equal m r

  let%test "meet_bottom_and_r" =
    let r = ric (2l, Int 0l, Int 4l, ("", 5l)) in
    let m = meet Bottom r in
    print_endline ("[RIC.meet]     ⊥ ⊓ " ^ to_string r ^ " → " ^ to_string m);
    RIC.equal m Bottom

  let%test "meet_regular" =
    let r1 = ric (2l, Int 0l, Int 4l, ("", 1l)) in
    let r2 = ric (4l, Int 0l, Int 2l, ("", 1l)) in
    let m = meet r1 r2 in
    print_endline ("[RIC.meet]     " ^ to_string r1 ^ " ⊓ " ^ to_string r2 ^ " → " ^ to_string m);
    RIC.equal m (ric (4l, Int 0l, Int 2l, ("", 1l)))

  let%test "meet_disjoint" =
    let r1 = ric (2l, Int 0l, Int 1l, ("", 1l)) in
    let r2 = ric (2l, Int 0l, Int 1l, ("", 2l)) in
    let m = meet r1 r2 in
    print_endline ("[RIC.meet]     " ^ to_string r1 ^ " ⊓ " ^ to_string r2 ^ " → " ^ to_string m);
    RIC.equal m Bottom

  let%test "meet [0,1]+(g0-43) and [0,1]+(g0-42)" =
    let r1 = ric (1l, Int 0l, Int 1l, ("g0", -43l)) in
    let r2 = ric (1l, Int 0l, Int 1l, ("g0", -42l)) in
    let m = meet r1 r2 in
    print_endline ("[RIC.meet]     " ^ to_string r1 ^ " ⊓ " ^ to_string r2 ^ " → " ^ to_string m);
    RIC.equal m (ric (0l, Int 0l, Int 0l, ("g0", -42l)))

  let%test "join_top_and_r" =
    let r = ric (2l, Int 0l, Int 4l, ("", 5l)) in
    let j = join Top r in
    print_endline ("[RIC.join]     ⊤ ⊔ " ^ to_string r ^ " → " ^ to_string j);
    RIC.equal j Top

  let%test "join 2[0,1]-4 and 2[0,3]" =
    let r1 = ric (2l, Int 0l, Int 1l, ("", -4l)) 
    and r2 = ric (2l, Int 0l, Int 3l, ("", 0l)) in
    let j = join r1 r2 in
    RIC.equal j (ric (2l, Int 0l, Int 5l, ("", -4l)))

  let%test "join_bottom_and_r" =
    let r = ric (2l, Int 0l, Int 4l, ("", 5l)) in
    let j = join Bottom r in
    print_endline ("[RIC.join]     ⊥ ⊔ " ^ to_string r ^ " → " ^ to_string j);
    RIC.equal j r

  let%test "join_bottom_(g0-44)" =
    let r = ric (0l, Int 0l, Int 0l, ("g0", -44l)) in
    let j = join Bottom r in
    print_endline ("[RIC.join]     ⊥ ⊔ " ^ to_string r ^ " → " ^ to_string j);
    RIC.equal j r

  let%test "join 3[0,1]+(g0-43) and (g0-42)" =
    let r1 = ric (3l, Int 0l, Int 1l, ("g0", -43l)) in
    let r2 = ric (0l, Int 0l, Int 0l, ("g0", -42l)) in
    let j = join r1 r2 in
    print_endline ("[RIC.join]     " ^ to_string r1 ^ " ⊔ " ^ to_string r2 ^ " → " ^ to_string j);
    RIC.equal j (ric (1l, Int 0l, Int 3l, ("g0", -43l)))

  let%test "join_regular" =
    let r1 = ric (2l, Int 0l, Int 2l, ("", 1l)) in
    let r2 = ric (2l, Int 3l, Int 4l, ("", 1l)) in
    let j = join r1 r2 in
    print_endline ("[RIC.join]     " ^ to_string r1 ^ " ⊔ " ^ to_string r2 ^ " → " ^ to_string j);
    RIC.equal j (ric (2l, Int 0l, Int 4l, ("", 1l)))

  let%test "join_different_stride" =
    let r1 = ric (2l, Int 0l, Int 2l, ("", 1l)) in
    let r2 = ric (4l, Int 1l, Int 2l, ("", 1l)) in
    let j = join r1 r2 in
    print_endline ("[RIC.join]     " ^ to_string r1 ^ " ⊔ " ^ to_string r2 ^ " → " ^ to_string j);
    RIC.equal j (ric (2l, Int 0l, Int 4l, ("", 1l)))
  
  let%test "join_different_offsets" =
    let r1 = ric (2l, Int 0l, Int 2l, ("", 1l)) in
    let r2 = ric (0l, Int 0l, Int 0l, ("a", 0l)) in
    let j = join r1 r2 in
    print_endline ("[RIC.join]     " ^ to_string r1 ^ " ⊔ " ^ to_string r2 ^ " → " ^ to_string j);
    RIC.equal j RIC.Top

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
    print_endline ("[RIC.complement]     " ^ to_string r ^ " → [  " ^ String.concat ~sep:"; " (List.map c ~f:to_string) ^ "  ]");
    Int.(List.length c = 3) &&
    List.mem ~equal c (ric (1l, NegInfinity, Int 0l, ("", 3l))) &&
    List.mem ~equal c (ric (2l, Int 0l, Int 2l, ("", 5l))) &&
    List.mem ~equal c (ric (1l, Int 0l, Infinity, ("", 10l)))

    
  let%test "complement of singleton 0[0,0]+7" =
    let r = ric (0l, Int 0l, Int 0l, ("", 7l)) in
    let c = complement r in
    print_endline ("[RIC.complement]     " ^ to_string r ^ " → [  " ^ String.concat ~sep:"; " (List.map c ~f:to_string) ^ "  ]");
    Int.(List.length c = 2) &&
    List.mem ~equal c (ric (1l, NegInfinity, Int 0l, ("", 6l))) &&
    List.mem ~equal c (ric (1l, Int 0l, Infinity, ("", 8l)))
    
  let%test "complement of 3[0,1]+2 has 4 parts" =
    let r = ric (3l, Int 0l, Int 1l, ("", 2l)) in
    let c = complement r in
    print_endline ("[RIC.complement]     " ^ to_string r ^ " → [  " ^ String.concat ~sep:"; " (List.map c ~f:to_string) ^ "  ]");
    Int.(List.length c = 4)

  let%test "complement of relative 4[0,1]+(x+1)" =
    let r = ric (4l, Int 0l, Int 1l, ("x", 1l)) in
    let c = complement r in
    print_endline ("[RIC.complement]     " ^ to_string r ^ " → [  " ^ String.concat ~sep:"; " (List.map c ~f:to_string) ^ "  ]");
    Int.(List.length c = 5) &&
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
    print_endline ("[RIC.complement]     " ^ to_string r ^ " → [  " ^ String.concat ~sep:"; " (List.map c ~f:to_string) ^ "  ]");
    not (List.exists c ~f:(fun r' -> match r' with RIC {lower_bound = Int _; upper_bound = Infinity; _} -> true | _ -> false))

  let%test "complement with unbounded lower and stride = 1 gives no inferior_RIC" =
    let r = ric (1l, NegInfinity, Int 5l, ("", 0l)) in
    let c = complement r in
    print_endline ("[RIC.complement]     " ^ to_string r ^ " → [  " ^ String.concat ~sep:"; " (List.map c ~f:to_string) ^ "  ]");
    not (List.exists c ~f:(fun r' -> match r' with RIC {lower_bound = NegInfinity; upper_bound = Int _; _} -> true | _ -> false))

  let%test "complement of 2[0,1]+3 includes shifted classes" =
    let r = ric (2l, Int 0l, Int 1l, ("", 3l)) in
    let comp = complement r in
    print_endline ("[RIC.complement]     " ^ to_string r ^ " → [  " ^ String.concat ~sep:"; " (List.map comp ~f:to_string) ^ "  ]");
    List.exists comp ~f:(fun r' -> match r' with RIC {stride = 2l; lower_bound = Int 0l; offset = ("", 4l); _} -> true | _ -> false)

  let%test "complement_top" =
    let r = Top in
    let compl = complement r in
    print_endline ("[RIC.complement]     " ^ to_string r ^ " → [  " ^ String.concat ~sep:"; " (List.map compl ~f:to_string) ^ "  ]");
    List.equal equal compl [Bottom]

  let%test "complement_bottom" =
    let r = Bottom in
    let compl = complement r in
    print_endline ("[RIC.complement]     " ^ to_string r ^ " → [  " ^ String.concat ~sep:"; " (List.map compl ~f:to_string) ^ "  ]");
    List.equal equal compl  [Top]

  let%test "complement_finite_absolute" =
    let r = ric (2l, Int 0l, Int 2l, ("", 1l)) in
    let compl = complement r in
    print_endline ("[RIC.complement]     " ^ to_string r ^ " → [  " ^ String.concat ~sep:"; " (List.map compl ~f:to_string) ^ "  ]");
    Int.(List.length compl = 3)

  let%test "complement_finite_relative" =
    let r = ric (3l, Int 1l, Int 3l, ("x", 2l)) in
    let compl = complement r in
    print_endline ("[RIC.complement]     " ^ to_string r ^ " → [  " ^ String.concat ~sep:"; " (List.map compl ~f:to_string) ^ "  ]");
    Int.(List.length compl = 4)

  let%test "complement_infinite_lower" =
    let r = ric (2l, NegInfinity, Int 2l, ("", 0l)) in
    let compl = complement r in
    print_endline ("[RIC.complement]     " ^ to_string r ^ " → [  " ^ String.concat ~sep:"; " (List.map compl ~f:to_string) ^ "  ]");
    Int.(List.length compl =  2)

  let%test "complement_infinite_upper" =
    let r = ric (2l, Int 0l, Infinity, ("", 0l)) in
    let compl = complement r in
    print_endline ("[RIC.complement]     " ^ to_string r ^ " → [  " ^ String.concat ~sep:"; " (List.map compl ~f:to_string) ^ "  ]");
    Int.(List.length compl = 2)

  let%test "complement_infinite_both" =
    let r = ric (2l, NegInfinity, Infinity, ("", 0l)) in
    let compl = complement r in
    print_endline ("[RIC.complement]     " ^ to_string r ^ " → [  " ^ String.concat ~sep:"; " (List.map compl ~f:to_string) ^ "  ]");
    Int.(List.length compl = 1)

  let%test "complement_stride_zero" =
    let r = ric (0l, Int 0l, Int 0l, ("", 5l)) in
    let compl = complement r in
    print_endline ("[RIC.complement]     " ^ to_string r ^ " → [  " ^ String.concat ~sep:"; " (List.map compl ~f:to_string) ^ "  ]");
    Int.(List.length compl = 2)

  let%test "complement_relative_stride_zero" =
    let r = ric (0l, Int 0l, Int 0l, ("x", 3l)) in
    let compl = complement r in
    print_endline ("[RIC.complement]     " ^ to_string r ^ " → [  " ^ String.concat ~sep:"; " (List.map compl ~f:to_string) ^ "  ]");
    Int.(List.length compl = 2)

  let%test "complement_nontrivial" =
    let r = ric (4l, Int 2l, Int 5l, ("a", 7l)) in
    let compl = complement r in
    print_endline ("[RIC.complement]     " ^ to_string r ^ " → [  " ^ String.concat ~sep:"; " (List.map compl ~f:to_string) ^ "  ]");
    Int.(List.length compl = 5)

  let%test "complement of {2,4}" =
    let r = ric (2l, Int 0l, Int 1l, ("", 2l)) in
    let compl = complement r in
    print_endline ("[RIC.complement]     " ^ to_string r ^ " → [  " ^ String.concat ~sep:"; " (List.map compl ~f:to_string) ^ "  ]");
    Int.(List.length compl = 3) 

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
    print_endline ("[add offset to RIC]     (4[0,2] + 4) ⊞ 12 → " ^ to_string result);
    RIC.equal result (ric (4l, Int 0l, Int 2l, ("", 16l)))

  let%test "add_offset_to_relative" =
    let r = ric (3l, Int 0l, Int 2l, ("x", 1l)) in
    let result = add_offset r (-2l) in
    print_endline ("[add offset to RIC]     (3[0,2] + (x+1)) ⊞ (-2) → " ^ to_string result);
    RIC.equal result (ric (3l, Int 0l, Int 2l, ("x", -1l)))

  let%test "add_offset_to_stride_zero" =
    let r = ric (0l, Int 0l, Int 0l, ("", 5l)) in
    let result = add_offset r 10l in
    print_endline ("[add offset to RIC]     (0[0,0] + 5) ⊞ 10 → " ^ to_string result);
    RIC.equal result (ric (0l, Int 0l, Int 0l, ("", 15l)))

  let%test "add_negative_offset_negInfinity" =
    let r = ric(4l, NegInfinity, Int 0l, ("a", 0l)) in
    let result = add_offset r (-1l) in
    print_endline ("[add offset to RIC]     (4[-∞,0] + a) ⊞ (-1) → " ^ to_string result);
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
    print_endline ("[remove_lower_bound]     " ^ to_string ~no_reduction r ^ " → " ^ to_string result);
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
    print_endline ("[remove_upper_bound]     " ^ to_string ~no_reduction r ^ " → " ^ to_string result);
    RIC.equal result (ric (1l, Int 2l, Infinity, ("", 2l)))

  let%test "widen_top_and_any" =
    let r = ric (3l, Int 0l, Int 4l, ("", 5l)) in
    let w = widen Top ~relative_to:r in
    print_endline ("[RIC.widen]      ⊤ ∇ " ^ to_string r ^ " → " ^ to_string w);
    RIC.equal w Top

  let%test "widen_bottom_and_any" =
    let r = ric (3l, Int 0l, Int 4l, ("", 5l)) in
    let w = widen Bottom ~relative_to:r in
    print_endline ("[RIC.widen]      ⊥ ∇ " ^ to_string r ^ " → " ^ to_string w);
    RIC.equal w r

  let%test "widen_regular_to_infinite_upper" =
    let r1 = ric (4l, Int 0l, Int 1l, ("", 0l)) in
    let r2 = ric (4l, Int 0l, Int 2l, ("", 0l)) in
    let expected = ric (4l, Int 0l, Infinity, ("", 0l)) in
    let result = widen r1 ~relative_to:r2 in
    print_endline ("[RIC.widen]      " ^ to_string r1 ^ " ∇ " ^ to_string r2 ^ " → " ^ to_string result);
    RIC.equal result expected

  let%test "widen_extend_lower_bound" =
    let r1 = ric (2l, Int 1l, Int 4l, ("", 3l)) in
    let r2 = ric (2l, Int 0l, Int 4l, ("", 3l)) in
    let expected = ric (2l, NegInfinity, Int 4l, ("", 3l)) in
    let result = widen r1 ~relative_to:r2 in
    print_endline ("[RIC.widen]      " ^ to_string r1 ^ " ∇ " ^ to_string r2 ^ " → " ^ to_string result);
    RIC.equal result expected

  let%test "widen_shifted_relative_offsets" =
    let r1 = ric (3l, Int 1l, Int 3l, ("x", 7l)) in
    let r2 = ric (3l, Int 1l, Int 5l, ("x", 7l)) in
    let expected = ric (3l, Int 1l, Infinity, ("x", 7l)) in
    let result = widen r1 ~relative_to:r2 in
    print_endline ("[RIC.widen]      " ^ to_string r1 ^ " ∇ " ^ to_string r2 ^ " → " ^ to_string result);
    RIC.equal result expected

  let%test "widen_self" =
    let r = ric (4l, Int 0l, Int 3l, ("", 5l)) in
    let result = widen r ~relative_to:r in
    print_endline ("[RIC.widen]      " ^ to_string r ^ " ∇ " ^ to_string r ^ " → " ^ to_string result);
    RIC.equal result r

  let%test "widen_different_offsets" =
    let r1 = ric (20l, Int 0l, Int 3l, ("", 3l)) in
    let r2 = ric (20l, Int 0l, Int 3l, ("", 7l)) in
    let result = widen r1 ~relative_to:r2 in
    print_endline ("[RIC.widen]      " ^ to_string r1 ^ " ∇ " ^ to_string r2 ^ " → " ^ to_string result);
    RIC.equal result (ric (4l, Int 0l, Infinity, ("", 3l)))

  let%test_unit "RIC.widen keeps bounded RIC unchanged when bounds do not grow" =
    let r1 = RIC.ric (2l, Int 0l, Int 10l, ("", 0l)) in
    let r2 = RIC.ric (2l, Int 2l, Int 8l, ("", 0l)) in
    let result = RIC.widen r1 ~relative_to:r2 in
    print_endline ("[RIC.widen]      " ^ to_string r1 ^ " ∇ " ^ to_string r2 ^ " → " ^ to_string result);
    assert (RIC.equal r1 result)


  let%test_unit "RIC.widen keeps lower-unbounded RIC unchanged when upper bound shrinks" =
    let r1 = RIC.ric (1l, NegInfinity, Int 10l, ("", 0l)) in
    let r2 = RIC.ric (1l, NegInfinity, Int 6l, ("", 0l)) in
    let result = RIC.widen r1 ~relative_to:r2 in
    print_endline ("[RIC.widen]      " ^ to_string r1 ^ " ∇ " ^ to_string r2 ^ " → " ^ to_string result);
    assert (RIC.equal r1 result)


  let%test_unit "RIC.widen keeps upper-unbounded RIC unchanged when lower bound rises" =
    let r1 = RIC.ric (1l, Int 0l, Infinity, ("", 0l)) in
    let r2 = RIC.ric (1l, Int 4l, Infinity, ("", 0l)) in
    let result = RIC.widen r1 ~relative_to:r2 in
    print_endline ("[RIC.widen]      " ^ to_string r1 ^ " ∇ " ^ to_string r2 ^ " → " ^ to_string result);
    assert (RIC.equal r1 result)


  

  (* Partially accessed: infinite span, stride=4, size=2
    Expect the band of 2*size-1 = 3 neighboring residue classes: -1, 0, +1. *)
  let%test "partially_accessed_infinite_stride4_size2" =
    let by = ric (4l, NegInfinity, Infinity, ("", 0l)) in
    let got = partially_accessed ~by ~size:2l in
    let expected = [RIC.Top] in
    print_endline ("[(size 2 stride 4  -inf..+inf) partially_accessed]     " ^ to_string by ^
                    " → [  " ^ String.concat ~sep:"; " (List.map ~f:to_string got) ^ "  ]");
    List.equal equal got expected

  let%test "partially_accessed_infinite_stride6_size2" =
    let by = ric (6l, NegInfinity, Infinity, ("", 1l)) in
    let got = partially_accessed ~by ~size:2l in
    let expected = [ric(6l, NegInfinity, Infinity, ("", 0l));
                    ric(6l, NegInfinity, Infinity, ("", 1l));
                    ric(6l, NegInfinity, Infinity, ("", 2l));
                    ric(6l, NegInfinity, Infinity, ("", 4l));
                    ric(6l, NegInfinity, Infinity, ("", 5l))] in
    print_endline ("[(size 2 stride 6  -inf..+inf) partially_accessed]     " ^ to_string by ^
                    " → [  " ^ String.concat ~sep:"; " (List.map ~f:to_string got) ^ "  ]");
    List.equal equal got expected

  let%test "partially_accessed_infinite_stride7_size2" =
    let by = ric (7l, NegInfinity, Infinity, ("x", 1l)) in
    let got = partially_accessed ~by ~size:2l in
    let expected = [ric(7l, NegInfinity, Infinity, ("x", 0l));
                    ric(7l, NegInfinity, Infinity, ("x", 1l));
                    ric(7l, NegInfinity, Infinity, ("x", 2l));
                    ric(7l, NegInfinity, Infinity, ("x", 5l));
                    ric(7l, NegInfinity, Infinity, ("x", 6l))] in
    print_endline ("[(size 2 stride 6  -inf..+inf) partially_accessed]     " ^ to_string by ^
                    " → [  " ^ String.concat ~sep:"; " (List.map ~f:to_string got) ^ "  ]");
    List.equal equal got expected

  let%test "partially_accessed_infinite_stride9_size4" =
    let by = ric (9l, NegInfinity, Infinity, ("x", 1l)) in
    let got = partially_accessed ~by ~size:4l in
    let expected = [ric(9l, NegInfinity, Infinity, ("x", 0l));
                    ric(9l, NegInfinity, Infinity, ("x", 2l));
                    ric(9l, NegInfinity, Infinity, ("x", 3l));
                    ric(9l, NegInfinity, Infinity, ("x", 4l));
                    ric(9l, NegInfinity, Infinity, ("x", 7l));
                    ric(9l, NegInfinity, Infinity, ("x", 8l));] in
    print_endline ("[(size 4 stride 9  -inf..+inf) partially_accessed]     " ^ to_string by ^
                    " → [  " ^ String.concat ~sep:"; " (List.map ~f:to_string got) ^ "  ]");
    List.equal equal got expected

  let%test "partially_accessed_infinite_stride3_size4_relative" =
    let by = ric (3l, NegInfinity, Infinity, ("x", 1l)) in
    let got = partially_accessed ~by ~size:4l in
    let expected = [ric(3l, NegInfinity, Infinity, ("x", 0l));
                    ric(3l, NegInfinity, Infinity, ("x", 1l));
                    ric(3l, NegInfinity, Infinity, ("x", 2l));] in
    print_endline ("[(size 4 stride 3  -inf..+inf) partially_accessed]     " ^ to_string by ^
                    " → [  " ^ String.concat ~sep:"; " (List.map ~f:to_string got) ^ "  ]");
    List.equal equal got expected

  let%test "partially_accessed_by_singleton_size_4" =
    let r = ric(0l, Int 0l, Int 0l, ("", 8l)) in
    let p_accessed = partially_accessed ~by:r ~size:4l in
    let expected = [ric(0l, Int 0l, Int 0l, ("", 5l));
                    ric(0l, Int 0l, Int 0l, ("", 6l));
                    ric(0l, Int 0l, Int 0l, ("", 7l));
                    ric(0l, Int 0l, Int 0l, ("", 9l));
                    ric(0l, Int 0l, Int 0l, ("", 10l));
                    ric(0l, Int 0l, Int 0l, ("", 11l))] in
    print_endline ("[(size 4 singleton) partially_accessed]     " ^ to_string r ^ " → [  " ^ String.concat ~sep:"; " (List.map p_accessed ~f:to_string) ^ "  ]");
    List.equal RIC.equal p_accessed expected

  let%test "partially_accessed_by_relative_singleton_size_4" =
    let r = ric(0l, Int 0l, Int 0l, ("x", 8l)) in
    let p_accessed = partially_accessed ~by:r ~size:4l in
    let expected = [ric(0l, Int 0l, Int 0l, ("x", 5l));
                    ric(0l, Int 0l, Int 0l, ("x", 6l));
                    ric(0l, Int 0l, Int 0l, ("x", 7l));
                    ric(0l, Int 0l, Int 0l, ("x", 9l));
                    ric(0l, Int 0l, Int 0l, ("x", 10l));
                    ric(0l, Int 0l, Int 0l, ("x", 11l))] in
    print_endline ("[(size 4 relative singleton) partially_accessed]     " ^ to_string r ^ " → [  " ^ String.concat ~sep:"; " (List.map p_accessed ~f:to_string) ^ "  ]");
    List.equal RIC.equal p_accessed expected

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

  let%test "partially_accessed_by_RIC_size_3" =
    let r = ric(4l, Int 0l, Int 1l, ("", 8l)) in
    let p_accessed = partially_accessed ~by:r ~size:4l in
    let expected = [ric(4l, Int 0l, Int 2l, ("", 5l));
                    ric(4l, Int 0l, Int 2l, ("", 6l));
                    ric(4l, Int 0l, Int 2l, ("", 7l))] in
    print_endline ("[(size 4) partially_accessed]     " ^ to_string r ^ " → [  " ^ String.concat ~sep:"; " (List.map p_accessed ~f:to_string) ^ "  ]");
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


  let%test "may_not_overlap" =
    let load_ric = ric (0l, Int 0l, Int 0l, ("", 0l)) in
    let store_ric = ric (0l, Int 0l, Int 0l, ("", 4l)) in
    let store_size = 4l in
    let load_size = 4l in
    let result = may_overlap ~store_size ~load_size ~store_ric ~load_ric in
    print_endline ("[RIC.may_overlap] load(" ^ to_string load_ric ^ ", size:" ^ Int32.to_string load_size ^ ") ... store(" ^ to_string store_ric ^ ", size:" ^ Int32.to_string store_size ^ ") --> " ^ string_of_bool result);
    not result

  let%test "may_overlap" =
    let load_ric = ric (0l, Int 0l, Int 0l, ("", 0l)) in
    let store_ric = ric (0l, Int 0l, Int 0l, ("", 4l)) in
    let store_size = 4l in
    let load_size = 5l in
    let result = may_overlap ~store_size ~load_size ~store_ric ~load_ric in
    print_endline ("[RIC.may_overlap] load(" ^ to_string load_ric ^ ", size:" ^ Int32.to_string load_size ^ ") ... store(" ^ to_string store_ric ^ ", size:" ^ Int32.to_string store_size ^ ") --> " ^ string_of_bool result);
    result

  let%test "may_overlap_different_relative_offsets_conservative" =
    let load_ric = ric (0l, Int 0l, Int 0l, ("x", 0l)) in
    let store_ric = ric (0l, Int 0l, Int 0l, ("y", 1024l)) in
    let store_size = 4l in
    let load_size = 4l in
    let result = may_overlap ~store_size ~load_size ~store_ric ~load_ric in
    print_endline ("[RIC.may_overlap] load(" ^ to_string load_ric ^ ", size:" ^ Int32.to_string load_size ^ ") ... store(" ^ to_string store_ric ^ ", size:" ^ Int32.to_string store_size ^ ") --> " ^ string_of_bool result);
    result

  let%test "may_not_overlap_with_bottom_load" =
    let load_ric = Bottom in
    let store_ric = ric (0l, Int 0l, Int 0l, ("", 4l)) in
    let store_size = 4l in
    let load_size = 4l in
    let result = may_overlap ~store_size ~load_size ~store_ric ~load_ric in
    print_endline ("[RIC.may_overlap] load(" ^ to_string load_ric ^ ", size:" ^ Int32.to_string load_size ^ ") ... store(" ^ to_string store_ric ^ ", size:" ^ Int32.to_string store_size ^ ") --> " ^ string_of_bool result);
    not result

  let%test "may_not_overlap_with_bottom_store" =
    let load_ric = ric (0l, Int 0l, Int 0l, ("", 4l)) in
    let store_ric = Bottom in
    let store_size = 4l in
    let load_size = 4l in
    let result = may_overlap ~store_size ~load_size ~store_ric ~load_ric in
    print_endline ("[RIC.may_overlap] load(" ^ to_string load_ric ^ ", size:" ^ Int32.to_string load_size ^ ") ... store(" ^ to_string store_ric ^ ", size:" ^ Int32.to_string store_size ^ ") --> " ^ string_of_bool result);
    not result

  let%test "may_overlap_with_top_store" =
    let load_ric = ric (0l, Int 0l, Int 0l, ("", 4l)) in
    let store_ric = Top in
    let store_size = 4l in
    let load_size = 4l in
    let result = may_overlap ~store_size ~load_size ~store_ric ~load_ric in
    print_endline ("[RIC.may_overlap] load(" ^ to_string load_ric ^ ", size:" ^ Int32.to_string load_size ^ ") ... store(" ^ to_string store_ric ^ ", size:" ^ Int32.to_string store_size ^ ") --> " ^ string_of_bool result);
    result

  let%test "may_overlap_store_RIC_load_singleton" =
    let load_ric = ric (0l, Int 0l, Int 0l, ("", 10l)) in
    let store_ric = ric (4l, Int 0l, Int 3l, ("", 0l)) in
    let store_size = 4l in
    let load_size = 2l in
    let result = may_overlap ~store_size ~load_size ~store_ric ~load_ric in
    print_endline ("[RIC.may_overlap] load(" ^ to_string load_ric ^ ", size:" ^ Int32.to_string load_size ^ ") ... store(" ^ to_string store_ric ^ ", size:" ^ Int32.to_string store_size ^ ") --> " ^ string_of_bool result);
    result

  let%test "may_not_overlap_store_RIC_load_singleton_gap" =
    let load_ric = ric (0l, Int 0l, Int 0l, ("", 18l)) in
    let store_ric = ric (8l, Int 0l, Int 3l, ("", 0l)) in
    let store_size = 2l in
    let load_size = 6l in
    let result = may_overlap ~store_size ~load_size ~store_ric ~load_ric in
    print_endline ("[RIC.may_overlap] load(" ^ to_string load_ric ^ ", size:" ^ Int32.to_string load_size ^ ") ... store(" ^ to_string store_ric ^ ", size:" ^ Int32.to_string store_size ^ ") --> " ^ string_of_bool result);
    not result

  let%test "may_overlap_store_RIC_load_singleton_by_last_byte" =
    let load_ric = ric (0l, Int 0l, Int 0l, ("", 11l)) in
    let store_ric = ric (8l, Int 0l, Int 2l, ("", 0l)) in
    let store_size = 4l in
    let load_size = 1l in
    let result = may_overlap ~store_size ~load_size ~store_ric ~load_ric in
    print_endline ("[RIC.may_overlap] load(" ^ to_string load_ric ^ ", size:" ^ Int32.to_string load_size ^ ") ... store(" ^ to_string store_ric ^ ", size:" ^ Int32.to_string store_size ^ ") --> " ^ string_of_bool result);
    result

  let%test "may_overlap_dense_RIC_size_1" =
    let load_ric = ric (2l, Int 0l, Int 4l, ("", 0l)) in
    let store_ric = ric (2l, Int 0l, Int 4l, ("", 1l)) in
    let store_size = 1l in
    let load_size = 1l in
    let result = may_overlap ~store_size ~load_size ~store_ric ~load_ric in
    print_endline ("[RIC.may_overlap] load(" ^ to_string load_ric ^ ", size:" ^ Int32.to_string load_size ^ ") ... store(" ^ to_string store_ric ^ ", size:" ^ Int32.to_string store_size ^ ") --> " ^ string_of_bool result);
    not result

  let%test "may_overlap_dense_RIC_size_2" =
    let load_ric = ric (2l, Int 0l, Int 4l, ("", 0l)) in
    let store_ric = ric (2l, Int 0l, Int 4l, ("", 1l)) in
    let store_size = 2l in
    let load_size = 1l in
    let result = may_overlap ~store_size ~load_size ~store_ric ~load_ric in
    print_endline ("[RIC.may_overlap] load(" ^ to_string load_ric ^ ", size:" ^ Int32.to_string load_size ^ ") ... store(" ^ to_string store_ric ^ ", size:" ^ Int32.to_string store_size ^ ") --> " ^ string_of_bool result);
    result

  let%test "may_overlap_negative_addresses" =
    let load_ric = ric (4l, Int (-3l), Int (-1l), ("", 0l)) in
    let store_ric = ric (0l, Int 0l, Int 0l, ("", -8l)) in
    let store_size = 4l in
    let load_size = 4l in
    let result = may_overlap ~store_size ~load_size ~store_ric ~load_ric in
    print_endline ("[RIC.may_overlap] load(" ^ to_string load_ric ^ ", size:" ^ Int32.to_string load_size ^ ") ... store(" ^ to_string store_ric ^ ", size:" ^ Int32.to_string store_size ^ ") --> " ^ string_of_bool result);
    result

  let%test "may_not_overlap_negative_addresses_gap" =
    let load_ric = ric (8l, Int (-3l), Int (-1l), ("", 0l)) in
    let store_ric = ric (0l, Int 0l, Int 0l, ("", -4l)) in
    let store_size = 4l in
    let load_size = 4l in
    let result = may_overlap ~store_size ~load_size ~store_ric ~load_ric in
    print_endline ("[RIC.may_overlap] load(" ^ to_string load_ric ^ ", size:" ^ Int32.to_string load_size ^ ") ... store(" ^ to_string store_ric ^ ", size:" ^ Int32.to_string store_size ^ ") --> " ^ string_of_bool result);
    not result

  let%test "may_overlap_semi_infinite_negative_RIC" =
    let load_ric = ric (4l, NegInfinity, Int 0l, ("", 0l)) in
    let store_ric = ric (0l, Int 0l, Int 0l, ("", -1024l)) in
    let store_size = 4l in
    let load_size = 4l in
    let result = may_overlap ~store_size ~load_size ~store_ric ~load_ric in
    print_endline ("[RIC.may_overlap] load(" ^ to_string load_ric ^ ", size:" ^ Int32.to_string load_size ^ ") ... store(" ^ to_string store_ric ^ ", size:" ^ Int32.to_string store_size ^ ") --> " ^ string_of_bool result);
    result

  let%test "may_overlap_large_load_reaches_store" =
    let load_ric = ric (0l, Int 0l, Int 0l, ("", 0l)) in
    let store_ric = ric (0l, Int 0l, Int 0l, ("", 7l)) in
    let store_size = 1l in
    let load_size = 8l in
    let result = may_overlap ~store_size ~load_size ~store_ric ~load_ric in
    print_endline ("[RIC.may_overlap] load(" ^ to_string load_ric ^ ", size:" ^ Int32.to_string load_size ^ ") ... store(" ^ to_string store_ric ^ ", size:" ^ Int32.to_string store_size ^ ") --> " ^ string_of_bool result);
    result

  let%test "may_not_overlap_large_load_stops_before_store" =
    let load_ric = ric (0l, Int 0l, Int 0l, ("", 0l)) in
    let store_ric = ric (0l, Int 0l, Int 0l, ("", 8l)) in
    let store_size = 1l in
    let load_size = 8l in
    let result = may_overlap ~store_size ~load_size ~store_ric ~load_ric in
    print_endline ("[RIC.may_overlap] load(" ^ to_string load_ric ^ ", size:" ^ Int32.to_string load_size ^ ") ... store(" ^ to_string store_ric ^ ", size:" ^ Int32.to_string store_size ^ ") --> " ^ string_of_bool result);
    not result

  let%test "may_overlap_adjacent_singletons" =
    let load_ric = ric (0l, Int 0l, Int 0l, ("", 0l)) in
    let store_ric = ric (0l, Int 0l, Int 0l, ("", 4l)) in
    let store_size = 1l in
    let load_size = 4l in
    let result = may_overlap ~store_size ~load_size ~store_ric ~load_ric in
    print_endline ("[RIC.may_overlap] load(" ^ to_string load_ric ^ ", size:" ^ Int32.to_string load_size ^ ") ... store(" ^ to_string store_ric ^ ", size:" ^ Int32.to_string store_size ^ ") --> " ^ string_of_bool result);
    not result

  let%test "may_overlap_singletons_touch_at_last_byte" =
    let load_ric = ric (0l, Int 0l, Int 0l, ("", 0l)) in
    let store_ric = ric (0l, Int 0l, Int 0l, ("", 3l)) in
    let store_size = 1l in
    let load_size = 4l in
    let result = may_overlap ~store_size ~load_size ~store_ric ~load_ric in
    print_endline ("[RIC.may_overlap] load(" ^ to_string load_ric ^ ", size:" ^ Int32.to_string load_size ^ ") ... store(" ^ to_string store_ric ^ ", size:" ^ Int32.to_string store_size ^ ") --> " ^ string_of_bool result);
    result

  let%test "may_overlap_bounded_RIC_exact_address" =
    let load_ric = ric (4l, Int 0l, Int 2l, ("", 0l)) in
    let store_ric = ric (0l, Int 0l, Int 0l, ("", 8l)) in
    let store_size = 4l in
    let load_size = 4l in
    let result = may_overlap ~store_size ~load_size ~store_ric ~load_ric in
    print_endline ("[RIC.may_overlap] load(" ^ to_string load_ric ^ ", size:" ^ Int32.to_string load_size ^ ") ... store(" ^ to_string store_ric ^ ", size:" ^ Int32.to_string store_size ^ ") --> " ^ string_of_bool result);
    result

  let%test "may_not_overlap_bounded_RIC_gap" =
    let load_ric = ric (8l, Int 0l, Int 2l, ("", 0l)) in
    let store_ric = ric (0l, Int 0l, Int 0l, ("", 4l)) in
    let store_size = 4l in
    let load_size = 4l in
    let result = may_overlap ~store_size ~load_size ~store_ric ~load_ric in
    print_endline ("[RIC.may_overlap] load(" ^ to_string load_ric ^ ", size:" ^ Int32.to_string load_size ^ ") ... store(" ^ to_string store_ric ^ ", size:" ^ Int32.to_string store_size ^ ") --> " ^ string_of_bool result);
    not result

  let%test "may_overlap_bounded_RIC_partial_byte" =
    let load_ric = ric (8l, Int 0l, Int 2l, ("", 0l)) in
    let store_ric = ric (0l, Int 0l, Int 0l, ("", 3l)) in
    let store_size = 2l in
    let load_size = 4l in
    let result = may_overlap ~store_size ~load_size ~store_ric ~load_ric in
    print_endline ("[RIC.may_overlap] load(" ^ to_string load_ric ^ ", size:" ^ Int32.to_string load_size ^ ") ... store(" ^ to_string store_ric ^ ", size:" ^ Int32.to_string store_size ^ ") --> " ^ string_of_bool result);
    result

  let%test "may_overlap_semi_infinite_positive_RIC" =
    let load_ric = ric (4l, Int 0l, Infinity, ("", 0l)) in
    let store_ric = ric (0l, Int 0l, Int 0l, ("", 1024l)) in
    let store_size = 4l in
    let load_size = 4l in
    let result = may_overlap ~store_size ~load_size ~store_ric ~load_ric in
    print_endline ("[RIC.may_overlap] load(" ^ to_string load_ric ^ ", size:" ^ Int32.to_string load_size ^ ") ... store(" ^ to_string store_ric ^ ", size:" ^ Int32.to_string store_size ^ ") --> " ^ string_of_bool result);
    result

  let%test "may_not_overlap_semi_infinite_positive_RIC_stride_gap" =
    let load_ric = ric (8l, Int 0l, Infinity, ("", 0l)) in
    let store_ric = ric (0l, Int 0l, Int 0l, ("", 4l)) in
    let store_size = 4l in
    let load_size = 4l in
    let result = may_overlap ~store_size ~load_size ~store_ric ~load_ric in
    print_endline ("[RIC.may_overlap] load(" ^ to_string load_ric ^ ", size:" ^ Int32.to_string load_size ^ ") ... store(" ^ to_string store_ric ^ ", size:" ^ Int32.to_string store_size ^ ") --> " ^ string_of_bool result);
    not result

  let%test "may_overlap_with_top_load" =
    let load_ric = Top in
    let store_ric = ric (0l, Int 0l, Int 0l, ("", 4l)) in
    let store_size = 4l in
    let load_size = 4l in
    let result = may_overlap ~store_size ~load_size ~store_ric ~load_ric in
    print_endline ("[RIC.may_overlap] load(" ^ to_string load_ric ^ ", size:" ^ Int32.to_string load_size ^ ") ... store(" ^ to_string store_ric ^ ", size:" ^ Int32.to_string store_size ^ ") --> " ^ string_of_bool result);
    result

  let%test "may_overlap_same_relative_offset" =
    let load_ric = ric (4l, Int 0l, Int 2l, ("x", 0l)) in
    let store_ric = ric (0l, Int 0l, Int 0l, ("x", 8l)) in
    let store_size = 4l in
    let load_size = 4l in
    let result = may_overlap ~store_size ~load_size ~store_ric ~load_ric in
    print_endline ("[RIC.may_overlap] load(" ^ to_string load_ric ^ ", size:" ^ Int32.to_string load_size ^ ") ... store(" ^ to_string store_ric ^ ", size:" ^ Int32.to_string store_size ^ ") --> " ^ string_of_bool result);
    result

  let%test "may_overlap_different_relative_offsets_conservative" =
    let load_ric = ric (0l, Int 0l, Int 0l, ("x", 0l)) in
    let store_ric = ric (0l, Int 0l, Int 0l, ("y", 1024l)) in
    let store_size = 4l in
    let load_size = 4l in
    let result = may_overlap ~store_size ~load_size ~store_ric ~load_ric in
    print_endline ("[RIC.may_overlap] load(" ^ to_string load_ric ^ ", size:" ^ Int32.to_string load_size ^ ") ... store(" ^ to_string store_ric ^ ", size:" ^ Int32.to_string store_size ^ ") --> " ^ string_of_bool result);
    result

  










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

  let%test "RIC.plus: Bottom + anything = Bottom" =
    let r = ric (3l, Int 0l, Int 2l, ("", 4l)) in
    let result = plus Bottom r in
    print_endline ("[RIC.plus]     ⊥ ⊕ " ^ to_string r ^ " = " ^ to_string result);
    result = Bottom

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

  let%test "RIC.minus_1" =
    let r1 = ric (4l, Int 0l, Int 2l, ("", 7l)) in
    let r2 = ric (3l, Int 0l, Int 1l, ("", 2l)) in
    let result = r1 - r2 in
    let expected = ric (1l, Int 0l, Int 11l, ("", 2l)) in
    print_endline ("[RIC.minus]     (" ^ to_string r1 ^ ") ⊖ (" ^ to_string r2 ^ ") = " ^ to_string result);
    result = expected

  let%test "RIC.minus_2" =
    let r1 = ric (4l, Int 0l, Int 2l, ("", 2l)) in
    let r2 = ric (14l, Int 0l, Int 1l, ("", 7l)) in
    let result = r1 - r2 in
    let expected = ric (2l, Int 0l, Int 11l, ("", -19l)) in
    print_endline ("[RIC.minus]     (" ^ to_string r1 ^ ") ⊖ (" ^ to_string r2 ^ ") = " ^ to_string result);
    result = expected

  let%test "RIC.minus_2_relative" =
    let r1 = ric (4l, Int 0l, Int 2l, ("x", 2l)) in
    let r2 = ric (14l, Int 0l, Int 1l, ("x", 7l)) in
    let result = r1 - r2 in
    let expected = ric (2l, Int 0l, Int 11l, ("", -19l)) in
    print_endline ("[RIC.minus]     (" ^ to_string r1 ^ ") ⊖ (" ^ to_string r2 ^ ") = " ^ to_string result);
    result = expected

  let%test "RIC.minus_2_relative_different_offsets" =
    let r1 = ric (4l, Int 0l, Int 2l, ("x", 2l)) in
    let r2 = ric (14l, Int 0l, Int 1l, ("y", 7l)) in
    let result = r1 - r2 in
    let expected = ric (2l, Int 0l, Int 11l, ("negy+x", -19l)) in
    print_endline ("[RIC.minus]     (" ^ to_string r1 ^ ") ⊖ (" ^ to_string r2 ^ ") = " ^ to_string result);
    result = expected

  let%test "RIC.minus_2_relative_negative_offset" =
    let r1 = ric (4l, Int 0l, Int 2l, ("x", 2l)) in
    let r2 = ric (14l, Int 0l, Int 1l, ("negy", 7l)) in
    let result = r1 - r2 in
    let expected = ric (2l, Int 0l, Int 11l, ("x+y", -19l)) in
    print_endline ("[RIC.minus]     (" ^ to_string r1 ^ ") ⊖ (" ^ to_string r2 ^ ") = " ^ to_string result);
    result = expected

  let%test "RIC.minus_2_relative_negative_offset_2" =
    let r1 = ric (4l, Int 0l, Int 2l, ("x", 2l)) in
    let r2 = ric (14l, Int 0l, Int 1l, ("negy+x", 7l)) in
    let result = r1 - r2 in
    let expected = ric (2l, Int 0l, Int 11l, ("y", -19l)) in
    print_endline ("[RIC.minus]     (" ^ to_string r1 ^ ") ⊖ (" ^ to_string r2 ^ ") = " ^ to_string result);
    result = expected

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

  let actual_values =
    String.Map.empty
    |> Map.set ~key:"x" ~data:(of_int32 4l)
    |> Map.set ~key:"y" ~data:Top
    |> Map.set ~key:"z" ~data:(ric (4l, Int 0l, Int 7l, ("x", 3l)))
    |> Map.set ~key:"w" ~data:(ric (5l, Int 0l, Infinity, ("", 14l)))

  let%test "update_relative_offset: x" =
    let r = ric (0l, Int 0l, Int 0l, ("x", 2l)) in
    let result = update_relative_offset ~ric_:r ~actual_values in
    let expected = of_int32 6l in
    print_endline ("[RIC.update_relative_offset]     " ^ to_string r ^ " -> " ^ to_string result);
    result = expected

  let%test "update_relative_offset: negx" =
    let r = ric (0l, Int 0l, Int 0l, ("negx", 10l)) in
    let result = update_relative_offset ~ric_:r ~actual_values in
    let expected = of_int32 6l in
    print_endline ("[RIC.update_relative_offset]     " ^ to_string r ^ " -> " ^ to_string result);
    equal result expected

  let%test "update_relative_offset: x+negx cancels out" =
    let r = ric (0l, Int 0l, Int 0l, ("x+negx", 3l)) in
    let result = update_relative_offset ~ric_:r ~actual_values in
    let expected = of_int32 3l in
    print_endline ("[RIC.update_relative_offset]     " ^ to_string r ^ " -> " ^ to_string result);
    equal result expected

  let%test "update_relative_offset: unknown variable is preserved" =
    let r = ric (0l, Int 0l, Int 0l, ("unknown", 5l)) in
    let result = update_relative_offset ~ric_:r ~actual_values in
    let expected = ric (0l, Int 0l, Int 0l, ("unknown", 5l)) in
    print_endline ("[RIC.update_relative_offset]     " ^ to_string r ^ " -> " ^ to_string result);
    equal result expected

  let%test "update_relative_offset: y becomes Top" =
    let r = ric (0l, Int 0l, Int 0l, ("y", 0l)) in
    let result = update_relative_offset ~ric_:r ~actual_values in
    print_endline ("[RIC.update_relative_offset]     " ^ to_string r ^ " -> " ^ to_string result);
    equal result Top

  let%test "update_relative_offset: w preserves infinite RIC" =
    let r = ric (0l, Int 0l, Int 0l, ("w", 1l)) in
    let result = update_relative_offset ~ric_:r ~actual_values in
    let expected = ric (5l, Int 0l, Infinity, ("", 15l)) in
    print_endline ("[RIC.update_relative_offset]     " ^ to_string r ^ " -> " ^ to_string result);
    equal result expected

  let%test "update_relative_offset: z keeps nested relative offset" =
    let r = ric (0l, Int 0l, Int 0l, ("z", 0l)) in
    let result = update_relative_offset ~ric_:r ~actual_values in
    let expected = ric (4l, Int 0l, Int 7l, ("x", 3l)) in
    print_endline ("[RIC.update_relative_offset]     " ^ to_string r ^ " -> " ^ to_string result);
    equal result expected

  let%test "update_relative_offset: x+z combines substituted values" =
    let r = ric (0l, Int 0l, Int 0l, ("x+z", 0l)) in
    let result = update_relative_offset ~ric_:r ~actual_values in
    let expected = ric (4l, Int 0l, Int 7l, ("x", 7l)) in
    print_endline ("[RIC.update_relative_offset]     " ^ to_string r ^ " -> " ^ to_string result);
    equal result expected

  let%test "update_relative_offset: negz" =
    let r = ric (0l, Int 0l, Int 0l, ("negz", 0l)) in
    let result = update_relative_offset ~ric_:r ~actual_values in
    let expected = ric (4l, Int (-7l), Int 0l, ("negx", -3l)) in
    print_endline ("[RIC.update_relative_offset]     " ^ to_string r ^ " -> " ^ to_string result);
    equal result expected

  let%test "update_relative_offset: absolute value unchanged" =
    let r = of_int32 42l in
    let result = update_relative_offset ~ric_:r ~actual_values in
    print_endline ("[RIC.update_relative_offset]     " ^ to_string r ^ " -> " ^ to_string result);
    equal result r

  let%test "to_bitfield_-inf_to_+inf_not_power_of_two_stride" =
    let r = ric (10l, NegInfinity, Infinity, ("", 5l)) in
    let expected = Bitfield.Bit {zeros=0xFFFFFFFEl; ones=0xFFFFFFFFl} in
    let result = to_bitfield r in
    print_endline ("[RIC.to_bitfield]     " ^ to_string r ^ " ---to-bitfield---> " ^ Bitfield.to_string result);
    Bitfield.(result = expected)
  
  let%test "to_bitfield_-inf_to_+inf_not_power_of_two_stride2" =
    let r = ric (120l, NegInfinity, Infinity, ("", 2l)) in
    let expected = Bitfield.Bit {zeros=0xFFFFFFFDl; ones=0xFFFFFFFAl} in
    let result = to_bitfield r in
    print_endline ("[RIC.to_bitfield]     " ^ to_string r ^ " ---to-bitfield---> " ^ Bitfield.to_string result);
    Bitfield.(result = expected)

  let%test "to_bitfield_-inf_to_+inf_power_of_two_stride" =
    let r = RIC {stride=16l; lower_bound=NegInfinity; upper_bound=Infinity; offset=("", 102l)} in
    let expected = Bitfield.Bit {zeros=0b1111_1111_1111_1111_1111_1111_1111_1001l; ones=0b1111_1111_1111_1111_1111_1111_1111_0110l} in
    let result = to_bitfield r in
    print_endline ("[RIC.to_bitfield]     " ^ to_string r ^ " ---to-bitfield---> " ^ Bitfield.to_string result);
    Bitfield.(result = expected)
  
  let%test "2[0,2]+1024 to bitfield" =
    let r = ric(2l, Int 0l, Int 2l, ("", 1024l)) in
    let bf = to_bitfield r in
    let bf_string = Bitfield.to_string bf in
    print_endline ("[RIC.to_bitfield]     " ^ to_string r ^ " ---to-bitfield---> " ^ bf_string);
    String.equal "bf_010000000::0" bf_string

  let%test "3[0,2]+32 to bitfield" =
    let r = ric(3l, Int 0l, Int 2l, ("", 32l)) in
    let bf = to_bitfield r in
    let bf_string = Bitfield.to_string bf in
    print_endline ("[RIC.to_bitfield]     " ^ to_string r ^ " ---to-bitfield---> " ^ bf_string);
    String.equal "bf_0100:::" bf_string

  let%test "2[0,2]+2 to bitfield" =
    let r = ric(2l, Int 0l, Int 2l, ("", 2l)) in
    let bf = to_bitfield r in
    let bf_string = Bitfield.to_string bf in
    print_endline ("[RIC.to_bitfield]     " ^ to_string r ^ " ---to-bitfield---> " ^ bf_string);
    String.equal "bf_0::0" bf_string

  let%test "2[0,3]+2 to bitfield" =
    let r = ric(2l, Int 0l, Int 3l, ("", 2l)) in
    let bf = to_bitfield r in
    let bf_string = Bitfield.to_string bf in
    print_endline ("[RIC.to_bitfield]     " ^ to_string r ^ " ---to-bitfield---> " ^ bf_string);
    String.equal "bf_0:::0" bf_string

  let%test "to_bitfield_neg_infinity_power_of_two_stride" =
    let s = 8l and o = -5l in
    let r = ric (s, NegInfinity, Int 0l, ("", o)) in
    let expected = Bitfield.join (Bit {ones = (-8l); zeros = (Int32.shift_right_logical (Int32.shift_left (-8l) 1)1)}) (Bitfield.singleton (-5l)) in
    let result = to_bitfield r in
    print_endline ("[RIC.to_bitfield]     " ^ to_string r ^ " ---to-bitfield---> " ^ Bitfield.to_string result);
    Bitfield.equal result expected
  
  let%test "to_bitfield_neg_infinity_power_of_two_stride_span_neg_and_pos" =
    let s = 8l and o = -5l in
    let r = ric (s, NegInfinity, Int 10l, ("", o)) in
    let expected = Bitfield.join (Bit {ones = (-8l); zeros = -8l}) (Bitfield.singleton (-5l)) in
    let result = to_bitfield r in
    print_endline ("[RIC.to_bitfield]     " ^ to_string r ^ " ---to-bitfield---> " ^ Bitfield.to_string result);
    Bitfield.equal result expected

  let%test "to_bitfield_neg_infinity_not_power_of_two_stride" =
    let s = 12l and o = -7l in
    let r = ric (s, NegInfinity, Int 0l, ("", o)) in
    let expected = Bitfield.Bit {zeros = 0b01111111111111111111111111111110l; ones = 0b11111111111111111111111111111101l} in
    let result = to_bitfield r in
    print_endline ("[RIC.to_bitfield]     " ^ to_string r ^ " ---to-bitfield---> " ^ Bitfield.to_string result);
    Bitfield.equal result expected

  let%test "to_bitfield_pos_infinity_power_of_two_stride" =
    let s = 8l and o = 3l in
    let r = ric (s, Int 0l, Infinity, ("", o)) in
    let expected = Bitfield.Bit {zeros = 0b11111111111111111111111111111100l; ones = 0b01111111111111111111111111111011l} in
    let result = to_bitfield r in
    print_endline ("[RIC.to_bitfield]     " ^ to_string r ^ " ---to-bitfield---> " ^ Bitfield.to_string result);
    Bitfield.equal result expected


  let%test "to_bitfield_pos_infinity_power_of_two_stride" =
    let s = 8l and o = -35l in
    let r = ric (s, Int 0l, Infinity, ("", o)) in
    let expected = Bitfield.Bit {zeros = 0b1111_1111_1111_1111_1111_1111_1111_1010l; ones = 0b1111_1111_1111_1111_1111_1111_1111_1101l} in
    let result = to_bitfield r in
    print_endline ("[RIC.to_bitfield]     " ^ to_string r ^ " ---to-bitfield---> " ^ Bitfield.to_string result);
    Bitfield.equal result expected

  let%test "to_bitfield_pos_infinity_not_power_of_two_stride" =
    let s = 10l and o = 5l in
    let r = ric (s, Int 0l, Infinity, ("", o)) in
    let expected = Bitfield.Bit {zeros = 0b11111111111111111111111111111110l; ones = 0b01111111111111111111111111111111l} in
    let result = to_bitfield r in
    print_endline ("[RIC.to_bitfield]     " ^ to_string r ^ " ---to-bitfield---> " ^ Bitfield.to_string result);
    Bitfield.equal result expected


  let%test "to_bitfield then of_bitfield 2[0,2] + 1024 = 2[0,3] + 1024" =
    let r = ric(2l, Int 0l, Int 2l, ("", 1024l)) in
    let bf = to_bitfield r in
    let bf_string = Bitfield.to_string bf in
    let result = of_bitfield bf in
    print_endline ("[RIC.to_bitfield_then_of_bitfield]     " ^ to_string r ^ " ---to-bitfield---> " ^ bf_string
      ^ " ---of-bitfield---> " ^ to_string result);
    result = ric (2l, Int 0l, Int 3l, ("", 1024l))

  let%test "to_bitfield then of_bitfield 3[0,2] + 32 = [0,7] + 32" =
    let r = ric(3l, Int 0l, Int 2l, ("", 32l)) in
    let bf = to_bitfield r in
    let bf_string = Bitfield.to_string bf in
    let result = of_bitfield bf in
    print_endline ("[RIC.to_bitfield_then_of_bitfield]     " ^ to_string r ^ " ---to-bitfield---> " ^ bf_string
      ^ " ---of-bitfield---> " ^ to_string result);
    result = ric (1l, Int 0l, Int 7l, ("", 32l))

  let%test "to_bitfield then of_bitfield 2[0,2] + 2 = 2[0,3]" =
    let r = ric(2l, Int 0l, Int 2l, ("", 2l)) in
    let bf = to_bitfield r in
    let bf_string = Bitfield.to_string bf in
    let result = of_bitfield bf in
    print_endline ("[RIC.to_bitfield_then_of_bitfield]     " ^ to_string r ^ " ---to-bitfield---> " ^ bf_string
      ^ " ---of-bitfield---> " ^ to_string result);
    result = ric (2l, Int 0l, Int 3l, ("", 0l))

  let%test "to_bitfield then of_bitfield 2[0,3] + 2 = 2[0,7]" =
    let r = ric(2l, Int 0l, Int 3l, ("", 2l)) in
    let bf = to_bitfield r in
    let bf_string = Bitfield.to_string bf in
    let result = of_bitfield bf in
    print_endline ("[RIC.to_bitfield_then_of_bitfield]     " ^ to_string r ^ " ---to-bitfield---> " ^ bf_string
      ^ " ---of-bitfield---> " ^ to_string result);
    result = ric (2l, Int 0l, Int 7l, ("", 0l))


  let%test "of_bitfield :::::::::::::::::::::::::::::::1" =
    let bf = Bitfield.Bit {zeros = 0xFFFFFFFEl; ones = 0xFFFFFFFFl} in
    let r = of_bitfield bf in
    print_endline
      (Printf.sprintf 
        "[RIC.of_bitfield] %s   ---to-RIC--->   %s"
        (Bitfield.to_string bf)
        (to_string r));
    equal r (ric (2l, NegInfinity, Infinity, ("", 1l)))

  let%test "of_bitfield :0:00000000000000000000000001001" =
    let bf = Bitfield.of_string ":0:00000000000000000000000001001" in
    let r = of_bitfield bf in
    let bf2 = to_bitfield r in
    print_endline
      (Printf.sprintf 
        "[RIC.of_bitfield] %s   ---to-RIC--->   %s   ---to-bitfield--->   %s"
        (Bitfield.to_string bf)
        (to_string r)
        (Bitfield.to_string bf2));
    Bitfield.(meet bf bf2 = bf)
  
  let%test "of_bitfield :00:0000000000000000000000001001" =
    let bf = Bitfield.of_string ":00:0000000000000000000000001001" in
    let r = of_bitfield bf in
    let bf2 = to_bitfield r in
    print_endline
      (Printf.sprintf 
        "[RIC.of_bitfield] %s   ---to-RIC--->   %s   ---to-bitfield--->   %s"
        (Bitfield.to_string bf)
        (to_string r)
        (Bitfield.to_string bf2));
    Bitfield.(meet bf bf2 = bf)

  let%test "of_bitfield 1:::::::::::::::::::::::::::::::" =
    let bf = Bitfield.Bit {zeros = 0b1111111111111111111111111111111l; ones = 0b11111111111111111111111111111111l} in
    let r = of_bitfield bf in
    print_endline
      (Printf.sprintf 
        "[RIC.of_bitfield] %s   ---to-RIC--->   %s"
        (Bitfield.to_string bf)
        (to_string r));
    equal r (ric (1l, NegInfinity, Int (-1l), ("", 0l)))

  let%test "of_bitfield 011:0:1001" =
    let bf = Bitfield.Bit {zeros = 0b11111111111111111111111001110110l; ones = 0b111011001l} in
    let r = of_bitfield bf in
    print_endline
      (Printf.sprintf 
        "[RIC.of_bitfield] %s   ---to-RIC--->   %s"
        (Bitfield.to_string bf)
        (to_string r));
    equal r (ric (16l, Int 0l, Int 5l, ("", 393l)))
  
  let%test "of_bitfield 1111111111111111111111111110:1:0" =
    let bf = Bitfield.Bit {zeros = 0b11011l; ones = 0b11111111111111111111111111101110l} in
    let r = of_bitfield bf in
    print_endline
      (Printf.sprintf 
        "[RIC.of_bitfield] %s   ---to-RIC--->   %s"
        (Bitfield.to_string bf)
        (to_string r));
    equal r (ric (2l, Int 0l, Int 5l, ("", (-28l))))

  let%test "of_bitfield :0000000000000000000000000:1::10" =
    let bf = Bitfield.Bit {zeros = 0b11111111111111111111111111101101l; ones = 0b10000000000000000000000000111110l} in
    let r = of_bitfield bf in
    print_endline
      (Printf.sprintf 
        "[RIC.of_bitfield] %s   ---to-RIC--->   %s"
        (Bitfield.to_string bf)
        (to_string r));
    equal r (ric (4l, Int 0l, Int 0b100000000000000000000000001011l, ("", Int32.(18l + 0b10000000000000000000000000000000l))))

  let%test "of_bitfield ::1::000000000000000000000010010" =
    let bf = Bitfield.Bit {zeros = 0b11011111111111111111111111101101l; ones = 0b11111000000000000000000000010010l} in
    let r = of_bitfield bf in
    print_endline
      (Printf.sprintf 
        "[RIC.of_bitfield] %s   ---to-RIC--->   %s"
        (Bitfield.to_string bf)
        (to_string r));
    equal r (ric (0b1000000000000000000000000000l, Int 0l, Int 0b11011l, ("", 0b10100000000000000000000000010010l)))


  let%test "of_bitfield_then_to_bitfield 011:0:1001" =
    let bf = Bitfield.Bit {zeros = 0b11111111111111111111111001110110l; ones = 0b111011001l} in
    let r = of_bitfield bf in
    let bf2 = to_bitfield r in
    print_endline
      (Printf.sprintf 
        "[RIC.of_bitfield_then_to_bitfield] %s   ---to-RIC--->   %s   ---to-Bitfield--->   %s"
        (Bitfield.to_string bf)
        (to_string r)
        (Bitfield.to_string bf2));
    r = (ric (16l, Int 0l, Int 5l, ("", 393l)))
    && String.(Bitfield.to_string bf2 = "bf_011:::1001")

  let%test "of_bitfield |> to_bitfield |> of_bitfield |> to_bitfield |> of_bitfield 1111111111111111111111111110:1:0" =
    let bf = Bitfield.Bit {zeros = 0b11011l; ones = 0b11111111111111111111111111101110l} in
    let r = of_bitfield bf in
    let bf2 = to_bitfield r in
    let r2 = of_bitfield bf2 in
    let bf3 = to_bitfield r2 in
    let r3 = of_bitfield bf3 in
    print_endline
      (Printf.sprintf 
        "[RIC.of_bitfield/to_bitfield...] %s   ---to-RIC--->   %s   ---to-bitfield--->   %s   ---to-RIC--->   %s   ---to-bitfield--->   %s   ---to-RIC--->   %s"
        (Bitfield.to_string bf)
        (to_string r)
        (Bitfield.to_string bf2)
        (to_string r2)
        (Bitfield.to_string bf3)
        (to_string r3));
    r = ric (2l, Int 0l, Int 5l, ("", (-28l)))
    && String.(Bitfield.to_string bf2 = "bf_10:::0")
    && r2 =  ric (2l, Int 0l, Int 7l, ("", (-32l)))
    && String.(Bitfield.to_string bf3 = "bf_10:::0")
    && r3 = ric (2l, Int 0l, Int 7l, ("", -32l))








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


  

  let%test "meet negative_integers" =
    let ric1 = ric (2l, Int (-10l), Int 10l, ("", 0l)) in
    let m = meet negative_integers ric1 in
    print_endline ("negative_integers: " ^ to_string negative_integers ^ " meet: " ^ to_string m);
    equal m (ric (2l, Int (-10l), Int (-1l), ("", 0l)))

  let%test "16 and Top" =
    let ric16 = ric(0l, Int 0l, Int 0l, ("", 16l)) in
    let a = and_ ric16 RIC.Top in
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




  

  let%test "Top and Top" =
    let top_and_top = and_ Top Top in
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


  let%test "may_be_false zero" =
    let result = RIC.may_be_false RIC.zero in
    Printf.printf "[may_be_false]     %s -> %s\n"
    (RIC.to_string RIC.zero)
    (string_of_bool result);
    result

  let%test "may_be_false not singleton" =
    let result = RIC.(ric (1l, Int 0l, Int 27l, ("", 0l)) |> may_be_false) in
    Printf.printf "[may_be_false]     %s -> %s\n"
    (RIC.to_string RIC.zero)
    (string_of_bool result);
    result

  let%test "may_be_false no zero" =
    let result = RIC.(ric (1l, Int 0l, Int 27l, ("", 3l)) |> may_be_false) in
    Printf.printf "[may_be_false]     %s -> %s\n"
    (RIC.to_string RIC.zero)
    (string_of_bool result);
    not result

  let%test "may_be_false relative" =
    let result = RIC.(ric (1l, Int 0l, Int 27l, ("a", 3l)) |> may_be_false) in
    Printf.printf "[may_be_false]     %s -> %s\n"
    (RIC.to_string RIC.zero)
    (string_of_bool result);
    result

  let%test "may_be_true zero" =
    let result = RIC.may_be_true RIC.zero in
    Printf.printf "[may_be_true]     %s -> %s\n"
    (RIC.to_string RIC.zero)
    (string_of_bool result);
    not result

  let%test "may_be_true not singleton" =
    let result = RIC.(ric (1l, Int 0l, Int 27l, ("", 0l)) |> may_be_true) in
    Printf.printf "[may_be_true]     %s -> %s\n"
    (RIC.to_string RIC.zero)
    (string_of_bool result);
    result

  let%test "may_be_true no zero" =
    let result = RIC.(ric (1l, Int 0l, Int 27l, ("", 3l)) |> may_be_true) in
    Printf.printf "[may_be_true]     %s -> %s\n"
    (RIC.to_string RIC.zero)
    (string_of_bool result);
    result

  let%test "may_be_true relative" =
    let result = RIC.(ric (1l, Int 0l, Int 27l, ("a", 3l)) |> may_be_true) in
    Printf.printf "[may_be_true]     %s -> %s\n"
    (RIC.to_string RIC.zero)
    (string_of_bool result);
    result
end)

    
    