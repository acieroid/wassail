(**
  Bitfield analysis utilities for abstract interpretation of 32‑bit integers.

  This module implements a *tri‑state per‑bit* abstraction. For each bit
  position [i ∈ 0..31], we track whether that bit is known to be 0, known to
  be 1, or may be either (unknown). The representation is:

  - [Bit { zeros; ones }] where [zeros] and [ones] are 32‑bit masks.
    For each bit position [i]:
      - if [zeros[i] = 1] and [ones[i] = 0] then bit i is known to be 0;
      - if [zeros[i] = 0] and [ones[i] = 1] then bit i is known to be 1;
      - if [zeros[i] = 1] and [ones[i] = 1] then bit i is **unknown** (may be 0 or 1);
      - [zeros[i] = 0] and [ones[i] = 0] is an *infeasible* state and is only
        used for the abstract value [Bottom].

  Special elements:
  - [Top]  : all bits are unknown (i.e. may be 0 or 1).
  - [Bottom]: no concrete value (empty set of integers).

  Invariants enforced by constructors/utilities:
  - [reduce] maps [Bit {0;0}] to [Bottom] and [Bit {z; o}] with [z = o] to [Top].
  - For any concrete [Bit {z; o}] distinct from [Bottom], bitwise [(z land o) <> 0]
    marks the unknown positions; [(z land o) = 0] indicates a singleton value.

  Notation used by [to_string]:
  - "0" means the bit is definitely 0; "1" means definitely 1;
  - ":" indicates an unknown bit (may be 0 or 1);
  - "#" is unreachable under the invariants and would signal an internal issue.

  Shifts:
  - The second operand is an abstract shift amount. We conservatively join all
    outcomes for any amount contained in that operand.
*)
open Core

(** Packed tri‑state bit information.
  [zeros] has 1s where bits may be 0; [ones] has 1s where bits may be 1. *)
type triStateInteger = {
  zeros : int32;
  ones : int32
}
[@@deriving sexp, compare, equal]

(** Abstract value for 32‑bit integers. *)
type t =
  | Top
  | Bottom
  | Bit of triStateInteger
[@@deriving sexp, compare, equal]

(** Pretty‑print an abstract bitfield using digits per bit from MSB→LSB.
  - "0"/"1" denote known bits; ":" unknown; "#" unreachable.
  @return a human‑readable string such as ["::1111"]. *)
let to_string (x : t) : string =
  match x with
  | Top -> "⊤"
  | Bottom -> "⊥"
  | Bit {zeros = z; ones = o} ->
    let rec aux acc z o : string =
      match z, o with
      | 0l, 0l -> acc
      | z, o ->
        let z_bit0 = Int32.(z % 2l = 1l) in
        let o_bit0 = Int32.(o % 2l = 1l) in
        if z_bit0 && o_bit0 then
          aux (":" ^ acc) (Int32.shift_right_logical z 1) (Int32.shift_right_logical o 1)
        else if z_bit0 then
          aux ("0" ^ acc) (Int32.shift_right_logical z 1) (Int32.shift_right_logical o 1)
        else if o_bit0 then
          aux ("1" ^ acc) (Int32.shift_right_logical z 1) (Int32.shift_right_logical o 1)
        else
          (* This state should not occur in real life *)
          aux ("#" ^ acc) (Int32.shift_right_logical z 1) (Int32.shift_right_logical o 1)
    in
    let rec cut_prefix (s : string) : string =
      let n = String.length s in
      if n > 1 && Char.equal s.[0] s.[1] then
        cut_prefix (String.drop_prefix s 1)
      else
        s
    in
    let s = aux "" z o in
    cut_prefix s

(** Test for the empty abstract value. *)
let is_bottom (bf : t) : bool =
  match bf with
  | Top -> false
  | Bottom -> true
  | Bit {ones = o; zeros = z} -> Int32.(not ((z lor o) = 0b11111111111111111111111111111111l))

(** Normalize a bitfield: map the degenerate masks to [Bottom] or [Top].
  Preserves other values unchanged. Idempotent. *)
let reduce (bf : t) : t =
  match bf with
  | bf when is_bottom bf -> Bottom
  | Bit {zeros = x; ones = y} when Int32.(x = y) -> Top
  | bf -> bf

(** Abstract a finite set of concrete 32‑bit integers.
    Each bit position is derived from the OR of possible zeros/ones across the set.
    @param set finite set of [int32] values
    @return the least upper bound bitfield containing exactly those values (and possibly more if necessary). *)
let of_set (set : Int32.Set.t) : t =
  reduce (
    Bit (
      Set.fold 
        set
        ~init:({zeros = 0l; ones = 0l}) 
        ~f:(fun acc n -> 
          {zeros = Int32.(acc.zeros lor (lnot n)); ones = Int32.(acc.ones lor n)}
        )
    )
  )

(** Singleton abstraction of a concrete integer. *)
let of_integer (n : int32) : t =
  Bit {zeros = Int32.(lnot n); ones = n}

(** Check whether [bf] denotes a single concrete integer.
  Equivalent to [(zeros land ones) = 0]. *)
let is_singleton (bf : t) : bool =
  match bf with
  | Bit {zeros = z; ones = o} -> Int32.(z land o = 0l)
  | _ -> false

(** Abstract value containing exactly the integer 1. *)
let one = Bit {zeros = (-2l); ones = 1l}

let zero = Bit {zeros = (-1l); ones = 0l}

(** [join bf1 bf2]
  Lattice join (least upper bound) of two abstract bitfields.
  Intuitively, this over-approximates the set union of the concrete integers
  represented by [bf1] and [bf2]. *)
let join (bf1 : t) (bf2 : t) : t =
  match bf1, bf2 with
  | Top, _ | _, Top -> Top
  | Bottom, x | x, Bottom -> x
  | Bit {zeros = z1; ones = o1}, Bit {zeros = z2; ones = o2} ->
    reduce (Bit {zeros = Int32.(z1 lor z2); ones = Int32.(o1 lor o2)})

let meet (bf1 : t) (bf2 : t) : t =
  match bf1, bf2 with
  | Top, x | x, Top -> x
  | Bottom, _ | _, Bottom -> Bottom
  | Bit {zeros = z1; ones = o1}, Bit {zeros = z2; ones = o2} ->
    reduce (Bit {zeros = Int32.(z1 land z2); ones = Int32.(o1 land o2)})

(** Bitwise NOT (two’s‑complement bitwise complement) on the abstraction. *)
let not_ (x : t) : t =
  match x with
  | Top -> Top
  | Bottom -> Bottom
  | Bit x -> Bit {zeros = x.ones; ones = x.zeros}

(** Bitwise AND on abstract values.
  [Top] is neutral; [Bottom] is absorbing. *)
let and_ (bf1 : t) (bf2 : t) : t =
  match bf1, bf2 with
  | Top, x | x, Top -> x
  | Bottom, _ | _, Bottom -> Bottom
  | Bit bf1, Bit bf2 ->
    Bit {zeros = Int32.(bf1.zeros lor bf2.zeros); ones = Int32.(bf1.ones land bf2.ones)}

(** Bitwise OR on abstract values.
  [Top] is absorbing; [Bottom] is neutral. *)
let or_ (bf1 : t) (bf2 : t) : t =
  match bf1, bf2 with
  | Top, _ | _, Top -> Top
  | Bottom, x | x, Bottom -> x
  | Bit bf1, Bit bf2 ->
    Bit {zeros = Int32.(bf1.zeros land bf2.zeros); 
          ones = Int32.(bf1.ones lor bf2.ones)}

(** Bitwise XOR on abstract values.
  Conservative join of per‑bit XOR possibilities. *)
let xor_ (bf1 : t) (bf2 : t) : t =
  match bf1, bf2 with
  | Top, _ | _, Top -> Top
  | Bottom, x | x, Bottom -> x
  | Bit bf1, Bit bf2 ->
    Bit {zeros = Int32.((bf1.zeros land bf2.zeros) lor (bf1.ones land bf2.ones)); 
          ones = Int32.((bf1.zeros land bf2.ones) lor (bf1.ones land bf2.zeros))}

(** Membership test for a concrete value [n].
  Returns [true] iff [n] is compatible with all per‑bit constraints in [bf]. *)
let contains (bf : t) (n : int32) : bool =
  match bf with
  | Top -> true
  | Bottom -> false
  | Bit {zeros = z; ones = o} ->
    Int32.(n land o = n) && Int32.((lnot n) land z = (lnot n))

(** Abstract left shift [bf1 << bf2].
  For each concrete shift amount contained in [bf2], shift and join results.
  Assumes shift amounts in [0..31]; other values may over‑approximate.
  @see shift_right for analogous behavior on right shifts. *)
let rec shift_left (bf1 : t) (bf2 : t) : t = 
  let bf2 =
    match bf2 with
    | Top | Bottom -> bf2
    | Bit {zeros = z; ones = o} -> 
      let zeros = Int32.(z lor (-32l))
      and ones = Int32.(o land 31l) in
      Bit {zeros; ones} in
  match bf1 with
  | Top -> shift_left (Bit {zeros = 0b11111111111111111111111111111111l; 
                              ones = 0b11111111111111111111111111111111l}) bf2
  | Bottom -> Bottom
  | Bit {zeros = z; ones = o} ->
    reduce (
      List.fold
        (List.init 32 ~f:(fun n -> n))
        ~init:Bottom
        ~f:(fun acc n ->
          if contains bf2 (Int32.of_int_exn n) then
            join acc (
              Bit {zeros = Int32.((shift_left z n) lor ((shift_left 1l n) - 1l)); 
                    ones = Int32.(shift_left o n)}
            )
          else
            acc)
    )

(** Abstract right shift. When [is_unsigned] is [true], performs logical shift;
  otherwise arithmetic shift. Joins results over all amounts in [bf2].
  Assumes shift amounts in [0..31]. *)
let rec shift_right ~(is_unsigned : bool) (bf1 : t) (bf2 : t) : t =
  let bf2 =
    match bf2 with
    | Top | Bottom -> bf2
    | Bit {zeros = z; ones = o} -> 
      let zeros = Int32.(z lor (-32l))
      and ones = Int32.(o land 31l) in
      Bit {zeros; ones} in
  match bf1 with
  | Top -> shift_right ~is_unsigned (Bit {zeros = 0b11111111111111111111111111111111l; 
                                            ones = 0b11111111111111111111111111111111l}) bf2
  | Bottom -> Bottom
  | Bit {zeros = z; ones = o} ->
    reduce (
      List.fold
        (List.init 32 ~f:(fun n -> n))
        ~init:Bottom
        ~f:(fun acc n ->
          if contains bf2 (Int32.of_int_exn n) then
            join acc (
              if is_unsigned then
                let n' = 32 - n in
                Bit {zeros = Int32.((shift_right_logical z n) lor (shift_left (-1l) n')); 
                      ones = Int32.(shift_right_logical o n)}
              else
                Bit {zeros = Int32.((shift_right z n)); 
                    ones = Int32.(shift_right o n)}
            )
          else
            acc)
    )
(** Aliases for logical and arithmetic right shifts. *)
let shift_right_unsigned = (shift_right ~is_unsigned:true)
let shift_right_signed = (shift_right ~is_unsigned:false)

(** Does [bf] contain at least one non‑negative value?  (Conservative test.) *)
let contains_positive_values (bf : t) : bool =
  match bf with
  | Top -> true
  | Bottom -> false
  | Bit {zeros = z; _} -> Int32.(z < 0l)

(** Does [bf] contain at least one negative value?  (Conservative test.) *)
let contains_negative_values (bf : t) : bool =
  match bf with
  | Top -> true
  | Bottom -> false
  | Bit {ones = o; _} -> Int32.(o < 0l)

(** Extract the mask of bits forced by the abstraction: [ones land lnot zeros].
  For [Top] this returns [0l]; for [Bottom] it raises [assert false]. *)
let constant_value (bf : t) : int32 =
  match bf with
  | Top -> 0l
  | Bottom -> assert false
  | Bit {zeros = z; ones = o} -> Int32.(o land (lnot z))

(** Describe the contiguous range induced by the least‑significant unknown bit.
  Returns [(k, lo, hi)] where [k] is the index of the least significant bit
  that is unknown (a power‑of‑two step), [lo] is the minimal value
  contributed by the unknown suffix, and [hi] the maximal. Intended for
  range‑like reasoning and quick splitting heuristics. Raises [assert false]
  on [Top] or [Bottom]. *)
let variable_values (bf : t) : int * int32 * int32 =
  match bf with
  | Top -> assert false
  | Bottom -> assert false
  | Bit {zeros = z; ones = o} ->
    let variable = Int32.(z land o) in
    if Int32.(variable = 0l) then
      0, 0l, 0l
    else
      let power_of_two = Maths.Binary.number_of_trailing_zeros variable in
      let lower_bound = 0l in
      let upper_bound = Int32.shift_right_logical variable power_of_two in
      (* let lower_bound = if Int32.(variable >= 0l) then 0l else Int32.shift_right 0b10000000000000000000000000000000l power_of_two in
      let upper_bound = Int32.(shift_right_logical) (Int32.(shift_left) variable 1) (1 + power_of_two) in *)
      power_of_two, lower_bound, upper_bound

(** [is_true bf]
  Conservative boolean test: returns [true] iff the abstraction admits at
  least one non-zero concrete value (i.e., there exists a model with some
  bit allowed to be [1]).  Implements three cases: [Top → true],
  [Bottom → false], and for [Bit b], checks whether [b.ones <> 0l]. *)
let is_true (bf : t) : bool =
  match bf with
  | Top -> true
  | Bottom -> false
  | Bit b -> Int32.(b.ones <> 0l)

(** [is_false bf]
  Conservative boolean test for zero: returns [true] iff the concrete value
  [0] is compatible with the per-bit constraints encoded by [bf].
  Implemented via [contains bf 0l]. *)
let is_false (bf : t) : bool =
  contains bf 0l


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

let%test_module "Bitfield tests" = (module struct
  (* open Bitfield *)

  let show = to_string

  let%test "Bitfield_header" =
    print_endline "\n_______ ____________________________ _______\n        BitField Module        \n------- ---------------------------- -------\n"; true

  (* ---------- Constructors & inspectors ---------- *)
  let%test "of_integer singleton & contains" =
    let n = 0x2Al in
    let bf = of_integer n in
    let got_is_singleton = is_singleton bf in
    let got_contains = contains bf n in
    Printf.printf "[of_integer]\t n=%ld -> %s; singleton=%b; (contains %ld)=%b\n" n (show bf) got_is_singleton n got_contains; Out_channel.flush stdout;
    got_is_singleton && got_contains

  let%test "of_set 2,3,6 basic" =
    let s = Set.of_list (module Int32) [2l; 3l; 6l] in
    let bf = of_set s in
    Printf.printf "[of_set]\t {2,3,6} -> %s\n" (show bf); Out_channel.flush stdout;
    String.equal (to_string bf) "0:1:"

  let%test "is_bottom Top/Bottom/Bit" =
    let a = Top and b = Bottom and c = of_integer 7l in
    let ga = is_bottom a and gb = is_bottom b and gc = is_bottom c in
    Printf.printf "[is_bottom]\t ⊤->%b ⊥->%b 7->%b\n" ga gb gc; Out_channel.flush stdout;
    (not ga) && gb && (not gc)

  (* ---------- Pretty printing ---------- *)
  let%test "to_string marker sanity" =
    let zeros = 0b11111111111111111111111111110000l in
    let ones  = 0b00000000000000000000000000111111l in
    let bf = Bit { zeros; ones } in
    Printf.printf "[to_string]\t z=%ld o=%ld -> %s\n" zeros ones (show bf); Out_channel.flush stdout;
    String.is_suffix ~suffix:"::1111" (show bf)

  (* ---------- Lattice-like specials ---------- *)
  let%test "not_ Top/Bottom/Bit" =
    let a = not_ Top and b = not_ Bottom and c = not_ (of_integer 0b1010l) in
    Printf.printf "[not]\t ⊤->%s ⊥->%s 0b1010->%s\n" (show a) (show b) (show c); Out_channel.flush stdout;
    String.equal (to_string c) "10101"

  (* ---------- Bitwise ops ---------- *)
  let%test "and_ neutral/absorbing" =
    let x = of_integer 0b101101l in
    let a = and_ Top x and b = and_ Bottom x in
    Printf.printf "[and]\t Top ∧ x -> %s  Bottom ∧ x -> %s\n" (show a) (show b); Out_channel.flush stdout;
    (* Top neutral, Bottom absorbing *) equal a x && equal b Bottom

  let%test "or_ absorbing/neutral" =
    let x = of_integer 0b1001l in
    let a = or_ Top x and b = or_ Bottom x in
    Printf.printf "[or]\t Top ∨ x -> %s  Bottom ∨ x -> %s\n" (show a) (show b); Out_channel.flush stdout;
    (* Top absorbing, Bottom neutral *) equal a Top && equal b x

  let%test "xor_ with singletons" =
    let x = of_integer 0b0101l and y = of_integer 0b0011l in
    let r = xor_ x y in
    Printf.printf "[xor]\t 0101 ⊕ 0011 -> %s\n" (show r); Out_channel.flush stdout;
    match r with Bit {zeros; ones} -> Int32.(zeros = lnot 0b0110l && ones = 0b0110l) | _ -> false

  let%test "contains yes/no" =
    let bf = of_set (Set.of_list (module Int32) [2l;3l;6l]) in
    let c1 = contains bf 3l and c2 = contains bf 4l in
    Printf.printf "[contains]\t [2;3;6] 3? %b ; 4? %b\n" c1 c2; Out_channel.flush stdout; c1 && (not c2)

  (* ---------- Positivity / Negativity ---------- *)
  let%test "contains_positive_values" =
    let a = of_set (Set.of_list (module Int32) [-2l; 2l; 3l; 6l]) in
    let b = of_set (Set.of_list (module Int32) [-2l; -3l; -6l]) in
    let ga = contains_positive_values a and gb = contains_positive_values b in
    Printf.printf "[contains_positive_values]\t [-2;2;3;6]->%b  [-2;-3;-6]->%b\n" ga gb; Out_channel.flush stdout; ga && (not gb)

  let%test "" =
    let a = of_set (Set.of_list (module Int32) [-1l; -2l]) in
    let b = of_set (Set.of_list (module Int32) [0l; 1l; 2l]) in
    let ga = contains_negative_values a and gb = contains_negative_values b in
    Printf.printf "[contains_negative_values]\t [-1;-2]->%b  [0;1;2]->%b\n" ga gb; Out_channel.flush stdout; ga && (not gb)

  (* ---------- constant_value / variable_values ---------- *)
  let%test "constant_value picks forced bits" =
    let bf = of_set (Set.of_list (module Int32) [0b001001010l; 0b011111010l]) in
    let c = constant_value bf in
    Printf.printf "[constant_value]\t %s -> %ld\n" (show bf) c; Out_channel.flush stdout; Int32.(c = 0b001001010l)

  let%test "variable_values examples" =
    let bf = of_set (Set.of_list (module Int32) [0b00001010l; 0b01111011l]) in
    let (k, lo, hi) = variable_values bf in
    Printf.printf "[variable_values]\t %s -> k=%d lo=%ld hi=%ld\n" (show bf) k lo hi; Out_channel.flush stdout;
    k >= 0 && Int32.(lo >= 0l) && Int32.(hi >= 0l)

  (* ---------- Shifts: left / right (unsigned & signed) ---------- *)
  let%test "shift_left with exact amount" =
    let x = of_integer 0b0000_0001l in
    let sh = of_integer 2l in
    let r = shift_left x sh in
    Printf.printf "[shl]\t %s << 2 -> %s\n" (show x) (show r); Out_channel.flush stdout;
    match r with Bit {zeros; ones} -> Int32.(ones = 0b0000_0100l && (ones land zeros) = 0l) | _ -> false

  let%test "shift_left with set of amounts (join)" =
    let x = of_integer 1l in
    let sh = of_set (Set.of_list (module Int32) [1l;3l]) in
    let r = shift_left x sh in
    Printf.printf "[shl]\t 1 << {1,3} -> %s\n" (show r); Out_channel.flush stdout; match r with Bit _ -> true | _ -> false

  let%test "shift_left clamps weird magnitudes" =
    let x = of_integer 2l in
    (* shift amounts outside 0..31 are masked internally; include -32 and 40 (as int32) *)
    let weird = of_set (Set.of_list (module Int32) [1l;16l;32l]) in
    let r = shift_left x weird in
    Printf.printf "[shl]\t 2 << {0,1,16,17,32,33,48,49} (masked=>{0,1,16,17}) -> %s\n" (show r); Out_channel.flush stdout;
    String.equal (show r) "0::00000000000000::0"

  let%test "shift_right_unsigned basic" =
    let neg_one = Bit {zeros = 0l; ones = (-1l)} in
    let result = shift_right_unsigned neg_one one in
    Printf.printf "[shr_u]\t %s >>u 1 -> %s\n" (show neg_one) (show result); Out_channel.flush stdout;
    equal result (Bit {zeros = 0b10000000000000000000000000000000l; ones = 0b01111111111111111111111111111111l})

  let%test "shift_right_signed retains sign" =
    let x = of_integer (-8l) in
    let r = shift_right_signed x (of_integer 1l) in
    Printf.printf "[shr_s]\t -8 >>s 1 -> %s\n" (show r); Out_channel.flush stdout; 
    equal (Bit {zeros = 0b11l; ones = 0b11111111111111111111111111111100l}) r

  (* ---------- Non-singleton tests for shifts and bitwise ops ---------- *)
  let%test "shift_left non_singleton membership" =
    let x = of_set (Set.of_list (module Int32) [1l; 3l]) in
    let sh = of_set (Set.of_list (module Int32) [1l; 2l]) in
    let r = shift_left x sh in
    Printf.printf "[shl ns]\t x=%s sh=%s -> %s\n" (show x) (show sh) (show r); Out_channel.flush stdout;
    (* results must include 1<<1, 1<<2, 3<<1, 3<<2 *)
    (not (is_singleton r))
    && contains r 2l  (* 1 << 1 *)
    && contains r 4l  (* 1 << 2 *)
    && contains r 6l  (* 3 << 1 *)
    && contains r 12l (* 3 << 2 *)

  let%test "shift_right_unsigned non_singleton membership" =
    let x = of_set (Set.of_list (module Int32) [8l; 12l]) in
    let sh = of_set (Set.of_list (module Int32) [1l; 2l]) in
    let r = shift_right_unsigned x sh in
    Printf.printf "[shr_u ns]\t x=%s sh=%s -> %s\n" (show x) (show sh) (show r); Out_channel.flush stdout;
    (* 8>>1=4, 8>>2=2, 12>>1=6, 12>>2=3 must be included *)
    (not (is_singleton r))
    && contains r 4l && contains r 2l && contains r 6l && contains r 3l

  let%test 
    "shift_right_signed non_singleton membership (negatives preserve sign)" =
    let x = of_set (Set.of_list (module Int32) [-8l; -4l]) in
    let sh = of_set (Set.of_list (module Int32) [1l; 2l]) in
    let r = shift_right_signed x sh in
    Printf.printf "[shr_s ns]\t x=%s sh=%s -> %s\n" (show x) (show sh) (show r); Out_channel.flush stdout;
    (* -8>>1=-4, -8>>2=-2, -4>>1=-2, -4>>2=-1 must be included *)
    (not (is_singleton r))
    && contains r (-4l) && contains r (-2l) && contains r (-1l)

  let%test "and_ non_singleton membership" =
    let x = of_set (Set.of_list (module Int32) [0b0101l; 0b0111l]) in
    let y = of_set (Set.of_list (module Int32) [0b0011l; 0b1111l]) in
    let r = and_ x y in
    Printf.printf "[and ns]\t x=%s y=%s -> %s\n" (show x) (show y) (show r); Out_channel.flush stdout;
    String.equal (show r) "0::1"

  let%test "or_ non_singleton membership" =
    let x = of_set (Set.of_list (module Int32) [0b0101l; 0b0111l]) in
    let y = of_set (Set.of_list (module Int32) [0b0011l; 0b1111l]) in
    let r = or_ x y in
    Printf.printf "[or ns]\t x=%s y=%s -> %s\n" (show x) (show y) (show r); Out_channel.flush stdout;
    String.equal (show r) "0:111"


  let%test "xor_ non_singleton membership" =
    let x = of_set (Set.of_list (module Int32) [0b0101l; 0b0111l]) in
    let y = of_set (Set.of_list (module Int32) [0b0011l; 0b1111l]) in
    let r = xor_ x y in
    Printf.printf "[xor ns]\t x=%s y=%s -> %s\n" (show x) (show y) (show r); Out_channel.flush stdout;
    String.equal (show r) "0:::0"

  (* ---------- Join (lattice union) ---------- *)
  let%test "join Top/Bottom" =
    let x = of_integer 0b1010l in
    let a = join Top x and b = join Bottom x and c = join x Bottom in
    Printf.printf "[join]\t x = %s ; Top ⊔ x -> %s ; Bottom ⊔ x -> %s ; x ⊔ Bottom -> %s\n" (show x) (show a) (show b) (show c); Out_channel.flush stdout;
    equal a Top && equal b x && equal c x

  let%test "join contains both singletons" =
    let x = of_integer 0b0101l and y = of_integer 0b0011l in
    let r = join x y in
    Printf.printf "[join]\t 0101 ⊔ 0011 -> %s\n" (show r); Out_channel.flush stdout;
    contains r 0b0101l && contains r 0b0011l && (not (is_singleton r))

  let%test "join commutative and associative" =
    let a = of_integer 0x0Fl and b = of_integer 0x33l and c = of_integer 0x55l in
    let ab = join a b and ba = join b a in
    let left = join (join a b) c and right = join a (join b c) in
    Printf.printf "[join props]\t a⊔b=%s b⊔a=%s ; (a⊔b)⊔c=%s ; a⊔(b⊔c)=%s\n" (show ab) (show ba) (show left) (show right); Out_channel.flush stdout;
    equal ab ba && equal left right

  let%test "join n with lnot n -> Top (reduce)" =
    let n = 0x0Fl in
    let r = join (of_integer n) (of_integer (Int32.lnot n)) in
    Printf.printf "[join reduce]\t n ⊔ ~n -> %s\n" (show r); Out_channel.flush stdout;
    equal r Top

  (* ---------- Algebraic properties sanity ---------- *)
  let%test "and_/or_ idempotent on singletons" =
    let x = of_integer 0x55l in
    let r_and = and_ x x and r_or = or_ x x in
    Printf.printf "[idem]\t x = %s ; x ∧ x = %s ; x ∨ x = %s\n" (show x) (show r_and) (show r_or); Out_channel.flush stdout;
    equal r_and x && equal r_or x

  let%test "xor_ self -> 0" =
    let x = of_integer 0x77l in
    let r = xor_ x x in
    Printf.printf "[xor self]\t x = %s ; x ⊕ x -> %s\n" (show x) (show r); Out_channel.flush stdout;
    match r with Bit {zeros; ones} -> Int32.(ones = 0l && zeros = lnot 0l) | _ -> false
end)