open Core 

open Maths

(** This module implements Reduced Interval Congruence (RIC) abstraction for pointer analysis.

    It combines two abstract domains:
    - A congruence domain to represent sets of integers of the form `stride * ℤ + offset`
    - An interval domain to constrain the lower and upper bounds of values in a set.

    The combination is reduced to eliminate redundancies and to ensure consistency.
    It supports operations such as join, meet, equality checks, and conversion between domains.

    The RIC abstraction is used in static analysis to represent value sets compactly and precisely,
    such as possible pointer offsets in memory. *)
module RIC = struct

  (** Congruence domain module.

      Represents sets of integers as arithmetic progressions: `stride * ℤ + offset`.
      Supports absolute and relative offsets. *)
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
      | Congruence c1, Congruence c2 when c1.stride = 0 && c2.stride = 0 -> true
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
        | stride, offset -> stride ^ " + " ^ offset

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

    (** [meet c1 c2] computes the greatest lower bound (intersection) of two congruences. *)
    let meet (c1 : t) (c2 : t) : t =
      match c1, c2 with
      | Top, c -> c
      | c, Top -> c 
      | Bottom, _ | _, Bottom -> Bottom 
      | Congruence c1, Congruence c2 ->
        let gcd_stride = gcd c1.stride c2.stride in 
        begin match c1.offset, c2.offset with 
          | (v1, o1), (v2, o2) when String.equal v1 v2 && (o1 - o2) mod gcd_stride = 0 -> 
            let new_stride = lcm c1.stride c2.stride in 
            let new_offset, _ = chinese_remainder c1.stride o1 c2.stride o2 in
            let new_offset = (v1, new_offset) in
            Congruence {stride = new_stride; offset = new_offset}
          | _ -> Bottom
        end
    
    let widen (c1 :t) (c2 : t) : t = join c1 c2
  end

  (** Interval domain module.

      Represents bounded or unbounded intervals over integers using an extended integer type. *)
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
      | Bottom, Interval {lower_bound = l; upper_bound = u} when ExtendedInt.less_than u l -> true
      | Interval {lower_bound = l; upper_bound = u}, Bottom when ExtendedInt.less_than u l -> true
      | Interval {lower_bound = l1; upper_bound = u1},
        Interval {lower_bound = l2; upper_bound = u2} when ExtendedInt.less_than u1 l1 && ExtendedInt.less_than u2 l2 -> true
      | Interval i1, Interval i2 -> 
        (ExtendedInt.equal i1.lower_bound i2.lower_bound && ExtendedInt.equal i1.upper_bound i2.upper_bound)
      | _ -> false

    let to_string (i : t) : string =
      match i with 
      | Top -> "ℤ"
      | Interval _ when equal Top i -> "ℤ"
      | Bottom -> "∅"
      | Interval _ when equal Bottom i -> "∅"
      | Interval i ->
        begin match i.lower_bound, i.upper_bound with 
          | Int l, Infinity -> "[" ^ string_of_int l ^ ", " ^ ExtendedInt.to_string Infinity ^ "]"
          | NegInfinity, Int u -> "[" ^ ExtendedInt.to_string NegInfinity ^ ", " ^ string_of_int u ^ "]"
          | Infinity, _ | _, NegInfinity -> assert false 
          | Int l, Int u when l <= u -> "[" ^ string_of_int l ^ ", " ^ string_of_int u ^ "]"
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

    (** [meet i1 i2] computes the greatest lower bound (intersection) of two intervals. *)
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
      
  end

  (** A Reduced Interval Congruence (RIC) is defined by a stride, interval bounds, and an offset. *)
  type ric = {
    stride : int;
    lower_bound : ExtendedInt.t;
    upper_bound : ExtendedInt.t;
    offset : string * int;
  }
  [@@deriving sexp, compare, equal]

  type t =
    | Top 
    | Bottom 
    | RIC of ric
  [@@deriving sexp, compare, equal]

  let ric (r : int * ExtendedInt.t * ExtendedInt.t * (string * int)) : t =
    match r with 
    | s, l, u, o -> RIC {stride = s; lower_bound = l; upper_bound = u; offset = o}

  (** [reduce r] canonicalizes the RIC representation to a standard form. *)
  let reduce (r : t) : t =
    match r with 
    | Top -> Top 
    | Bottom -> Bottom 
    | RIC {stride = s; lower_bound = l; upper_bound = u; offset = o} ->
      if ExtendedInt.less_than u l then Bottom else 
      if (s = 1 && ExtendedInt.equal NegInfinity l && ExtendedInt.equal Infinity u) then Top else
      if (s = 0) then RIC {stride = 0; lower_bound = Int 0; upper_bound = Int 0; offset = o} else
      let new_offset =
        match o, l with 
        | ("", o), Int l -> ("", (o + s * l))
        | (var, o), Int l -> (var, o + s * l)
        | (var, o), NegInfinity -> (var, o mod s)
        | _ -> assert false
      in
      let new_lower =
        match l with 
        | NegInfinity -> ExtendedInt.NegInfinity
        | Int _ -> ExtendedInt.Int 0
        | Infinity -> assert false 
      in 
      let new_upper =
        match o, l, u with
        | _, _, Infinity -> ExtendedInt.Infinity
        | _, Int l, Int u -> Int (u - l)
        | (_, o), NegInfinity, u -> ExtendedInt.plus u (ExtendedInt.divide (ExtendedInt.Int o) (ExtendedInt.Int s))
        | _ -> assert false
      in
      ric (s, new_lower, new_upper, new_offset)

  (** [of_congruence_and_interval c i] builds a RIC value from a congruence and interval pair. *)
  let of_congruence_and_interval (c : Congruence.t) (i : Interval.t) : t =
    match c with
    | Top ->
      begin match i with 
        | Top -> Top
        | Bottom -> Bottom
        | Interval {lower_bound = l; upper_bound = u} ->
          reduce (ric (1, l, u, ("", 0)))
      end
    | c when Congruence.equal Top c ->
      begin match i with 
        | Top -> Top
        | Bottom -> Bottom
        | Interval {lower_bound = l; upper_bound = u} ->
          reduce (ric (1, l, u, ("", 0)))
      end
    | Bottom -> Bottom
    | Congruence {stride = 0; offset = o} ->
      RIC {stride = 0; lower_bound = Int 0; upper_bound = Int 0; offset = o}
    | Congruence {stride = s; offset = ("", o)} ->
      begin match i with 
        | Top -> reduce (ric (s, ExtendedInt.NegInfinity, ExtendedInt.Infinity, ("", o)))
        | Bottom -> Bottom
        | Interval {lower_bound = l; upper_bound = u} ->
          let lower = ExtendedInt.divide (ExtendedInt.minus l (ExtendedInt.Int o)) (ExtendedInt.Int s) in
          let upper = ExtendedInt.divide (ExtendedInt.minus u (ExtendedInt.Int o)) (ExtendedInt.Int s) in
          reduce (ric (s, lower, upper, ("", o)))
        end
    | Congruence {stride = s; offset = (var, o)} ->
      begin match i with 
        | Top -> reduce (ric (s, ExtendedInt.NegInfinity, ExtendedInt.Infinity, (var, o)))
        | Bottom -> Bottom
        | Interval {lower_bound = l; upper_bound = u} ->
          let lower = ExtendedInt.divide (ExtendedInt.minus l (ExtendedInt.Int o)) (ExtendedInt.Int s) in
          let upper = ExtendedInt.divide (ExtendedInt.minus u (ExtendedInt.Int o)) (ExtendedInt.Int s) in
          reduce (ric (s, lower, upper, (var, o)))
      end

  (** [to_congruence_and_interval r] decomposes a RIC into its congruence and interval parts. *)
  let to_congruence_and_interval (r : t) : Congruence.t * Interval.t =
    (* let r = reduce r in *)
    match r with 
    | Top -> Top, Top
    | Bottom -> Bottom, Bottom 
    | RIC {stride = s; offset = ("", o); lower_bound = l; upper_bound = u} ->
      Congruence {stride = s; offset = ("", o)},
      Interval {lower_bound = ExtendedInt.plus (ExtendedInt.Int o) (ExtendedInt.times (ExtendedInt.Int s) l); 
                upper_bound = ExtendedInt.plus (ExtendedInt.Int o) (ExtendedInt.times (ExtendedInt.Int s) u)}
    | RIC {stride = s; offset = (var, o); lower_bound = l; upper_bound = u} ->
      Congruence {stride = s; offset = (var, o)},
      Interval {lower_bound = ExtendedInt.plus (ExtendedInt.Int o) (ExtendedInt.times (ExtendedInt.Int s) l); 
                upper_bound = ExtendedInt.plus (ExtendedInt.Int o) (ExtendedInt.times (ExtendedInt.Int s) u)}
      
    

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
            (if not (String.equal var "") then " + " else "") ^ var
          else if String.equal var "" then
            (if i > 0 then " + " else " - ") ^ string_of_int (abs i)
          else
            " + (" ^ var ^ ((if i > 0 then "+" else "-") ^ string_of_int (abs i)) ^ ")"
      in
      let lower = ExtendedInt.to_string l in
      let upper = ExtendedInt.to_string u in
      let interval = "[" ^ lower ^ ", " ^ upper ^ "]" in
      let stride = if s = 1 then "" else string_of_int s in
      match stride, interval, offset with 
      | "0", _, "" -> "⊥"
      | "0", _, o -> o
      | "", "[-∞, ∞]", _ -> "⊤"
      | _, _, "" ->  stride ^ interval
      | _ -> stride ^ interval ^ offset


  (** [of_list l] builds a RIC value representing exactly the elements in the given list of integers. *)
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

  let meet (ric1 : t) (ric2 : t) : t =
    let (c1, i1) = to_congruence_and_interval ric1 in
    let (c2, i2) = to_congruence_and_interval ric2 in
    let c = Congruence.meet c1 c2 in
    let i = Interval.meet i1 i2 in 
    of_congruence_and_interval c i

  let join (ric1 : t) (ric2 : t) : t =
    let (c1, i1) = to_congruence_and_interval ric1 in
    let (c2, i2) = to_congruence_and_interval ric2 in
    let c = Congruence.join c1 c2 in
    let i = Interval.join i1 i2 in 
    of_congruence_and_interval c i

  (* Checks that RIC1 is a subset of RIC2 *)
  let is_subset (ric1 : t) ~(of_ : t) : bool =
    let m = meet ric1 of_ in
    let j = join ric1 of_ in 
    match m, j with 
    | m, j when equal m ric1 && equal j of_ -> true 
    | m, _ when equal m ric1 -> assert false
    | _, j when equal j of_ -> assert false
    | _ -> false

  (* Checks that RIC1 over-approximates RIC2 : *)
  let subsumes (ric1 : t) (ric2 : t) : bool = 
    is_subset ric2 ~of_:ric1


  (* ⊞ *)
  let add_offset (r : t) (c : int) : t =
    match r with
    | Top -> Top 
    | Bottom -> Bottom
    | RIC {stride = s; lower_bound = l; upper_bound = u; offset = (v,o)} -> reduce (ric (s, l, u, (v, o + c)))

  let remove_lower_bound (r : t) : t =
    let r = reduce r in
    match r with 
    | Top -> Top 
    | Bottom -> Bottom 
    | RIC {stride = s; lower_bound = _; upper_bound = u; offset = o} -> ric (s, NegInfinity, u, o)

  let remove_upper_bound (r : t) : t =
    let r = reduce r in
    match r with 
    | Top -> Top 
    | Bottom -> Bottom 
    | RIC {stride = s; lower_bound = l; upper_bound = _; offset = o} -> ric (s, l, Infinity, o)

  let widen (ric1 : t) (ric2 : t) : t =
    let (c1, i1) = to_congruence_and_interval ric1 in
    let (c2, i2) = to_congruence_and_interval ric2 in
    let widened_c = Congruence.widen c1 c2 in
    let widened_i = Interval.widen i1 i2 in 
    of_congruence_and_interval widened_c widened_i 

  let element (i : string * int) (r : t) : bool =
    match i, r with 
    | _, Top -> true
    | (v, i), RIC {stride = s; lower_bound = l; upper_bound = u; offset = (var, o)} when String.equal v var ->
      begin match l, u with 
        | Int l, _ ->
          i >= (s * l + o) && not (ExtendedInt.less_than (ExtendedInt.plus (ExtendedInt.times u (Int s)) (Int o)) (Int i)) &&
          (i - (s * l + o)) mod s = 0
        | NegInfinity, Infinity ->
          (i - o) mod s = 0
        | NegInfinity, Int u ->
          i <= (s * u + o) && (i - (s * u + o)) mod s = 0
        | _ -> false
      end
    | _ -> false

  let fully_accessed (s : int) (r : t) (v : Variable.t) : bool =
    match v with 
    | Var _ -> false 
    | Mem {address = addr; size = size} -> 
      size = s && element addr r 

  let element_but_wrong_size (s : int) (r : t) (v : Variable.t) : bool =
    match v with 
    | Var _ -> false 
    | Mem {address = addr; size = size} -> 
      size <> s && element addr r 

  let memory_block_partially_in_RIC (s : int) (r : t) (v : Variable.t) : bool =
    match v with 
    | Var _ -> false 
    | Mem {address = addr; size = size} -> 
      let addresses = Memory_block.extract_addresses {address = addr; size = size} in
      let is_partially_in = List.fold ~init:false ~f:(fun acc x -> acc || element x r) addresses in 
      is_partially_in && not (fully_accessed s r v) && not (element_but_wrong_size s r v) 

  
  let partially_accessed (s : int) (r : t) (v : Variable.t) : bool =
    element_but_wrong_size s r v || memory_block_partially_in_RIC s r v 

  let fully_accessed_set (vars : Variable.Set.t) (s : int) (r : t) : Variable.Set.t =
    Variable.Set.filter vars ~f:(fully_accessed s r) 

  let partially_accessed_set (vars : Variable.Set.t) (s : int) (r : t) : Variable.Set.t =
    Variable.Set.filter vars ~f:(partially_accessed s r) 

  (* *(vs, s)*)
  let accessed (vars : Variable.Set.t) (s : int) (r : t) : Variable.Set.t * Variable.Set.t =
    let f = fully_accessed_set vars s r in
    let p = partially_accessed_set vars s r in
    f, p
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
        "2ℤ + 3"

    (* to_string of stride 4, relative offset ("x", 1) should be "4ℤ + (x + 1)". *)
    let%test "Congruence: to_string relative" =
      String.equal
        (Congruence.to_string (Congruence.Congruence {stride = 4; offset = ("x", 1)}))
        "4ℤ + (x+1)"
    
    (* to_string of stride 4, relative offset ("x", 1) should be "4ℤ + (x + 1)". *)
    let%test "Congruence: to_string relative offset0" =
      String.equal
        (Congruence.to_string (Congruence.Congruence {stride = 10; offset = ("x", 0)}))
        "10ℤ + x"

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
    let%test "Congruence: join (2ℤ + 3) and (2ℤ + 2) => Top" =
      let c1 = Congruence.Congruence {stride = 2; offset = ("", 3)} in
      let c2 = Congruence.Congruence {stride = 2; offset = ("", 2)} in 
      let j = Congruence.join c1 c2 in 
      print_endline ("(" ^ Congruence.to_string c1 ^ ") ⊔ (" ^ Congruence.to_string c2 ^ ") = " ^ Congruence.to_string j);
      Congruence.equal j Congruence.Top

    (* Joining congruences with different offsets. *)
    let%test "Congruence: join (10ℤ + 3) and (10ℤ + 8) => 5ℤ + 3" =
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
      print_endline ("[0, 5] → " ^ s);
      String.equal s "[0, 5]"

    (* to_string of [-∞, 10] should be "[-∞, 10]". *)
    let%test "to_string_neg_inf_10" =
      let s = Interval.to_string (Interval.Interval {lower_bound = NegInfinity; upper_bound = Int 10}) in
      print_endline ("[-∞, 10] → " ^ s);
      String.equal s "[-∞, 10]"

    (* to_string of [4, ∞] should be "[4, ∞]". *)
    let%test "to_string_4_pos_inf" =
      let s = Interval.to_string (Interval.Interval {lower_bound = Int 4; upper_bound = Infinity}) in
      print_endline ("[4, ∞] → " ^ s);
      String.equal s "[4, ∞]"

    (* to_string of [-∞, ∞] should be "ℤ". *)
    let%test "to_string_neg_inf_pos_inf" =
      let s = Interval.to_string (Interval.Interval {lower_bound = NegInfinity; upper_bound = Infinity}) in
      print_endline ("[-∞, ∞] → " ^ s);
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
      print_endline ("2[0, 4] + 5 → " ^ s);
      String.equal s "2[0, 4] + 5"

    let%test "to_string_relative" =
      let s = to_string (ric (3, Int 0, Int 3, ("x", 2))) in
      print_endline ("3[0, 3] + (x + 2) → " ^ s);
      String.equal s "3[0, 3] + (x+2)"

    let%test "to_string_relative_offset_0" =
      let s = to_string (ric (3, Int 0, Int 3, ("x", 0))) in
      print_endline ("3[0, 3] + x → " ^ s);
      String.equal s "3[0, 3] + x"

    let%test "to_string_stride_1" =
      let s = to_string (ric (1, Int 0, Int 3, ("", 0))) in
      print_endline ("1[0, 3] + 0 → " ^ s);
      String.equal s "[0, 3]"

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
      let w = widen Top r in
      print_endline ("widen ⊤ (" ^ to_string r ^ ") → " ^ to_string w);
      RIC.equal w Top

    let%test "widen_bottom_and_any" =
      let r = ric (3, Int 0, Int 4, ("", 5)) in
      let w = widen Bottom r in
      print_endline ("widen ⊥ (" ^ to_string r ^ ") → " ^ to_string w);
      RIC.equal w r

    let%test "widen_regular_to_infinite_upper" =
      let r1 = ric (4, Int 0, Int 1, ("", 0)) in
      let r2 = ric (4, Int 0, Int 2, ("", 0)) in
      let expected = ric (4, Int 0, Infinity, ("", 0)) in
      let result = widen r1 r2 in
      print_endline ("widen (" ^ to_string r1 ^ ") (" ^ to_string r2 ^ ") → " ^ to_string result);
      RIC.equal result expected

    let%test "widen_extend_lower_bound" =
      let r1 = ric (2, Int 1, Int 4, ("", 3)) in
      let r2 = ric (2, Int 0, Int 4, ("", 3)) in
      let expected = ric (2, NegInfinity, Int 4, ("", 3)) in
      let result = widen r1 r2 in
      print_endline ("widen (" ^ to_string r1 ^ ") (" ^ to_string r2 ^ ") → " ^ to_string result);
      RIC.equal result expected

    let%test "widen_shifted_relative_offsets" =
      let r1 = ric (3, Int 1, Int 3, ("x", 7)) in
      let r2 = ric (3, Int 1, Int 5, ("x", 7)) in
      let expected = ric (3, Int 1, Infinity, ("x", 7)) in
      let result = widen r1 r2 in
      print_endline ("widen (" ^ to_string r1 ^ ") (" ^ to_string r2 ^ ") → " ^ to_string result);
      RIC.equal result expected

    let%test "widen_self" =
      let r = ric (4, Int 0, Int 3, ("", 5)) in
      let result = widen r r in
      print_endline ("widen (" ^ to_string r ^ ") (" ^ to_string r ^ ") → " ^ to_string result);
      RIC.equal result r

    let%test "widen_different_offsets" =
      let r1 = ric (20, Int 0, Int 3, ("", 3)) in
      let r2 = ric (20, Int 0, Int 3, ("", 7)) in
      let result = widen r1 r2 in
      print_endline ("widen (" ^ to_string r1 ^ ") (" ^ to_string r2 ^ ") → " ^ to_string result);
      RIC.equal result (ric (4, Int 0, Infinity, ("", 3)))

    (* --- Additional tests for RIC.element --- *)
    let%test "element_in_absolute_ric" =
      let r = ric (3, Int 0, Int 4, ("", 2)) in
      let result = element ("", 11) r in
      print_endline ("Is 11 in " ^ to_string r ^ "? → " ^ string_of_bool result);
      result

    let%test "element_not_in_absolute_ric" =
      let r = ric (3, Int 0, Int 4, ("", 2)) in
      let result = element ("", 10) r in
      print_endline ("Is 10 in " ^ to_string r ^ "? → " ^ string_of_bool result);
      not result

    let%test "element_in_relative_ric" =
      let r = ric (2, Int 0, Int 3, ("x", 4)) in
      let result = element ("x", 6) r in
      print_endline ("Is (x, 6) in " ^ to_string r ^ "? → " ^ string_of_bool result);
      result

    let%test "element_not_in_relative_ric_wrong_var" =
      let r = ric (2, Int 0, Int 3, ("x", 4)) in
      let result = element ("y", 6) r in
      print_endline ("Is (y, 6) in " ^ to_string r ^ "? → " ^ string_of_bool result);
      not result

    let%test "element_top_always_true" =
      let result = element ("", 42) Top in
      print_endline ("Is 42 in ⊤? → " ^ string_of_bool result);
      result

    let%test "element_bottom_always_false" =
      let result = element ("", 42) Bottom in
      print_endline ("Is 42 in ⊥? → " ^ string_of_bool result);
      not result

    let%test "another element test" =
      let result = element ("a", 32) (ric (4,Int 0, Int 4, ("a", 0))) in
      print_endline ("Is a+17 in 4[0,4] + a? → " ^ string_of_bool result);
      not result


    (* --- Additional tests for RIC.fully_accessed --- *)
    let%test "fully_accessed_true" =
      let v = Variable.Mem {address = ("", 8); size = 4} in
      let r = ric (2, Int 0, Int 4, ("", 0)) in
      let result = fully_accessed 4 r v in
      print_endline ("fully_accessed " ^ Variable.to_string v ^ " in " ^ to_string r ^ " with expected size 4 → " ^ string_of_bool result);
      result

    let%test "fully_accessed_false_wrong_size" =
      let v = Variable.Mem {address = ("", 8); size = 2} in
      let r = ric (2, Int 0, Int 4, ("", 0)) in
      let result = fully_accessed 4 r v in
      print_endline ("fully_accessed " ^ Variable.to_string v ^ " in " ^ to_string r ^ " with expected size 4 → " ^ string_of_bool result);
      not result

    let%test "fully_accessed_false_not_in_ric" =
      let v = Variable.Mem {address = ("", 7); size = 4} in
      let r = ric (2, Int 0, Int 4, ("", 0)) in
      let result = fully_accessed 4 r v in
      print_endline ("fully_accessed (Mem(7, size=4)) in " ^ to_string r ^ " → " ^ string_of_bool result);
      not result

    let%test "fully_accessed_var_is_not_memory" =
      let v = Variable.Var (Var.Global 0) in
      let r = ric (1, Int 0, Int 4, ("", 0)) in
      let result = fully_accessed 4 r v in
      print_endline ("fully_accessed (Var x) in " ^ to_string r ^ " → " ^ string_of_bool result);
      not result


    (* let%test "accessed with 5 variables" =
      let blocks = [
        Variable.Mem (Memory_block.make ("a", 0) 4);  
        Variable.Mem (Memory_block.make ("a", 4) 4);  
        Variable.Mem (Memory_block.make ("c", 2) 3);  
        Variable.Mem (Memory_block.make ("a", 0) 5);  
        Variable.Mem (Memory_block.make ("a", 1) 4);
        Variable.Mem (Memory_block.make ("", 0) 5);  
        Variable.Mem (Memory_block.make ("a", 17) 5) (* falsly detected as partially accessed *)
      ] in
      let blocks = Variable.Set.of_list blocks in
      let r = (ric (4, Int 0, Int 4, ("a", 0))) in
      let actual = fully_accessed_set blocks 4 r in
      let actual = Variable.Set.to_list actual in
      let printed = Set.map actual ~f:Variable.to_string in
      print_endline ("accessed with 5 variables and RIC = " ^ (to_string r));
      print_endline "fully_accessed:";
      Set.iter printed ~f:print_endline;
      let expected = [
        "mem[a+0,a+3]";
        "mem[a+4,a+7]"
      ] in
      print_endline "expected:";
      List.iter expected ~f:print_endline;

      let actual = partially_accessed_set blocks 4 r in
      let printed = List.map actual ~f:Variable.to_string in
      print_endline "partially_accessed:";
      List.iter printed ~f:print_endline;
      let expected = [
        "mem[a+0,a+4]";
        "mem[a+1,a+4]"
      ] in
      print_endline "expected:";
      List.iter expected ~f:print_endline;
      List.equal String.equal (List.sort ~compare:String.compare printed) (List.sort ~compare:String.compare expected) *)
  end)
end)

    