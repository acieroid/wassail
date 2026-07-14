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
  - [reduce] maps inconsistent masks to [Bottom] and fully unknown masks to [Top].
  - For any concrete [Bit {z; o}] distinct from [Bottom], bitwise [(z land o) = 1]
    marks the unknown positions; [(z land o) = 0] indicates a singleton value.
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

(** Test for the empty abstract value. *)
let is_bottom (bf : t) : bool =
  match bf with
  | Top -> false
  | Bottom -> true
  | Bit {ones; zeros} -> Int32.(zeros lor ones <> 0xFFFFFFFFl)


let is_top (bf : t) : bool =
  match bf with
  | Top -> true
  | Bottom -> false
  | Bit {ones; zeros} -> Int32.(zeros land ones = 0xFFFFFFFFl)

(** Normalize a bitfield.
    Maps inconsistent masks to [Bottom] and fully unknown masks to [Top]. *)
let reduce (bf : t) : t =
  if is_bottom bf then Bottom else if is_top bf then Top else bf

let equal (bf1 : t) (bf2 : t) : bool =
  equal (reduce bf1) (reduce bf2)
let (=) = equal
let (<>) (x : t) (y : t) : bool = not (x = y)

(** Pretty‑print an abstract bitfield using digits per bit from MSB→LSB.
  - "0"/"1" denote known bits; ":" unknown; "#" unreachable.
  @return a human‑readable string such as ["::1111"]. *)
let to_string (x : t) : string =
  match x with
  | Top -> "⊤"
  | Bottom -> "⊥"
  | Bit {zeros; ones} ->
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
    let s = aux "" zeros ones in
    "bf_" ^ cut_prefix s

let of_string (s : string) : t =
  let string_length = String.length s in
  assert (string_length <= 32);
  let nb_missing_bits = 32 - string_length in
  let s = String.to_list s in
  let s = 
    match s with
    | [] -> List.init 32 ~f:(fun _ -> ':')
    | x :: _ -> List.init nb_missing_bits ~f:(fun _ -> x) @ s
  in
  let ones =
    s |> List.map ~f:(fun x -> if not (Char.equal  x '0') then "1" else "0")
      |> String.concat
      |> String.append "0b"
      |> Int32.of_string
  in
  let zeros =
    s |> List.map ~f:(fun x -> if not (Char.equal  x '1') then "1" else "0")
      |> String.concat
      |> String.append "0b"
      |> Int32.of_string
  in
  Bit {zeros; ones}

(** Abstract a finite set of concrete 32‑bit integers.
    Each bit position is derived from the OR of possible zeros/ones across the set.
    @param set finite set of [int32] values
    @return the least upper bound bitfield containing exactly those values (and possibly more if necessary). *)
let of_set (set : Int32.Set.t) : t =
  Bit (
    Set.fold 
      set
      ~init:({zeros = 0l; ones = 0l}) 
      ~f:(fun acc n -> 
        {zeros = Int32.(acc.zeros lor (lnot n)); ones = Int32.(acc.ones lor n)}
      )
  )
  |> reduce

(** Singleton abstraction of a concrete integer. *)
let singleton (n : int32) : t =
  Bit {zeros = Int32.(lnot n); ones = n}

(** Return [true] iff [bf] is a singleton abstract value. *)
let is_singleton (bf : t) : bool =
  match bf with
  | Bit {zeros; ones} -> Int32.(zeros land ones = 0l)
  | _ -> false

(** Abstract value containing exactly the integer 1. *)
let one = Bit {zeros = 0xFFFFFFFEl; ones = 1l}

(** Abstract value containing exactly the integer 0. *)
let zero = Bit {zeros = 0xFFFFFFFFl; ones = 0l}

(** Least upper bound of two bitfields. *)
let join (bf1 : t) (bf2 : t) : t =
  match bf1, bf2 with
  | Top, _ | _, Top -> Top
  | Bottom, x | x, Bottom -> x
  | Bit {zeros = z1; ones = o1}, Bit {zeros = z2; ones = o2} ->
    reduce (Bit {zeros = Int32.(z1 lor z2); ones = Int32.(o1 lor o2)})

(** Greatest lower bound of two bitfields. *)
let meet (bf1 : t) (bf2 : t) : t =
  match bf1, bf2 with
  | Bottom, _ | _, Bottom -> Bottom
  | Top, x | x, Top -> x
  | Bit {zeros = z1; ones = o1}, Bit {zeros = z2; ones = o2} ->
    reduce (Bit {zeros = Int32.(z1 land z2); ones = Int32.(o1 land o2)})

(** Abstract bitwise AND. *)
let rec and_ (bf1 : t) (bf2 : t) : t =
  match bf1, bf2 with
  | Bottom, _ | _, Bottom -> Bottom
  | Top, Top -> Top
  | Top, bf | bf, Top -> and_ bf (Bit {ones=0xFFFFFFFFl; zeros=0xFFFFFFFFl})
  | Bit bf1, Bit bf2 ->
    Bit {zeros = Int32.(bf1.zeros lor bf2.zeros); ones = Int32.(bf1.ones land bf2.ones)}
let (&.) = and_

(** Abstract bitwise OR. *)
let rec or_ (bf1 : t) (bf2 : t) : t =
  match bf1, bf2 with
  |Bottom, _ | _, Bottom -> Bottom
  | Top, Top -> Top
  | Top, bf | bf, Top -> or_ bf (Bit {ones=0xFFFFFFFFl; zeros=0xFFFFFFFFl})
  | Bit bf1, Bit bf2 ->
    Bit {zeros = Int32.(bf1.zeros land bf2.zeros); 
          ones = Int32.(bf1.ones lor bf2.ones)}
let (|.) = or_

(** Bitwise XOR on abstract values.
  Conservative join of per‑bit XOR possibilities. *)
let rec xor_ (bf1 : t) (bf2 : t) : t =
  match bf1, bf2 with
  | Bottom, _ | _, Bottom -> Bottom
  | Top, Top -> Top
  | Top, bf | bf, Top -> xor_ bf (Bit {ones=0xFFFFFFFFl; zeros=0xFFFFFFFFl})
  | Bit bf1, Bit bf2 ->
    Bit {zeros = Int32.((bf1.zeros land bf2.zeros) lor (bf1.ones land bf2.ones)); 
          ones = Int32.((bf1.zeros land bf2.ones) lor (bf1.ones land bf2.zeros))}
let (<+>) = xor_

(** Membership test for a concrete value [n].
  Returns [true] iff [n] is compatible with all per‑bit constraints in [bf]. *)
let contains (bf : t) (n : int32) : bool =
  match bf with
  | Top -> true
  | Bottom -> false
  | Bit {zeros; ones} ->
    Int32.(n land ones = n) && Int32.((lnot n) land zeros = (lnot n))

(** Does [bf] contain at least one non‑negative value?  (Conservative test.) *)
let may_be_positive (bf : t) : bool =
  match bf with
  | Top -> true
  | Bottom -> false
  | Bit {zeros; _} -> Int32.(zeros < 0l)

(** Does [bf] contain at least one negative value?  (Conservative test.) *)
let may_be_negative (bf : t) : bool =
  match bf with
  | Top -> true
  | Bottom -> false
  | Bit {ones; _} -> Int32.(ones < 0l)

(** Restrict shift amounts to the WebAssembly-valid range [0,31]. *)
let valid_shift_values (shift : t) : t =
  (* only keep valid shift values (0 to 31) *)
  match shift with
  | Top -> of_string "0:::::"
  | Bottom -> shift
  | Bit {zeros; ones} -> 
    let zeros = Int32.(zeros lor 0xFFFFFFE0l)
    and ones = Int32.(ones land 0x0000001Fl) in
    Bit {zeros; ones}

(** Abstract left shift.
    Applies all shift amounts represented by [shift] and joins the results. *)
let rec shift_left (bf : t) (shift : t) : t = 
  let shift = valid_shift_values shift in
  match bf with
  | Top -> shift_left (Bit {zeros = 0xFFFFFFFFl; ones = 0xFFFFFFFFl}) shift
  | Bottom -> Bottom
  | Bit {zeros; ones} ->
    List.init 32 ~f:(fun n -> n)
    |> List.fold
      ~init:Bottom
      ~f:(fun acc n ->
        if contains shift (Int32.of_int_exn n) then
          join
            acc 
            (Bit {zeros = Int32.(shift_left zeros n + ((shift_left 1l n) - 1l)); 
                   ones = Int32.(shift_left ones n)})
        else
          acc)
    |> reduce
let (<<) = shift_left

(** Abstract right shift.
    Uses logical or arithmetic shifting depending on [is_unsigned]. *)
let rec shift_right ~(is_unsigned : bool) (bf : t) (shift : t) : t =
  let shift = valid_shift_values shift in
  match bf with
  | Top -> shift_right ~is_unsigned (Bit {zeros = 0xFFFFFFFFl; ones = 0xFFFFFFFFl}) shift
  | Bottom -> Bottom
  | Bit {zeros; ones} ->
    List.init 32 ~f:(fun n -> n)
    |> List.fold
      ~init:Bottom
      ~f:(fun acc n ->
        if contains shift (Int32.of_int_exn n) then
          join 
            acc
            (if is_unsigned then
              Bit {zeros = Int32.(shift_right_logical zeros n + shift_left 0xFFFFFFFFl Int.(32-n)); 
                    ones = Int32.(shift_right_logical ones n)}
            else
              Bit {zeros = Int32.(shift_right zeros n); 
                    ones = Int32.(shift_right ones n)})
        else
          acc)
    |> reduce
(** Aliases for logical and arithmetic right shifts. *)
let shift_right_unsigned = (shift_right ~is_unsigned:true)
let (>>.) = shift_right_unsigned
let shift_right_signed = (shift_right ~is_unsigned:false)
let (>>-) = shift_right_signed

(** Extract the mask of bits forced by the abstraction: [ones land lnot zeros].
  For [Top] this returns [0l]; for [Bottom] it raises [assert false]. *)
let constant_value (bf : t) : int32 =
  match bf with
  | Top -> 0l
  | Bottom -> assert false
  | Bit {zeros; ones} -> Int32.(ones land (lnot zeros))


type ric = {
  stride : int32;
  lower_bound : Maths.ExtendedInt.t;
  upper_bound : Maths.ExtendedInt.t;
  offset : string * int32;
}

(** Convert a bitfield into an over-approximating RIC.
    Fixed bits become the offset; varying bits determine the stride and bounds.
    [Top] becomes an unbounded RIC. [Bottom] is unsupported. *)
let to_RIC (bf : t) : ric =
  match bf with
  | Top -> {stride=1l; lower_bound=NegInfinity; upper_bound=Infinity; offset="",0l}
  | Bottom -> assert false
  | Bit {zeros; ones} ->
    let variable = Int32.(zeros land ones) in
    if Int32.(variable = 0l) then
      {stride=0l; lower_bound=Int 0l; upper_bound=Int 0l; offset="",constant_value bf}
    else
      let power_of_two = Int32.ctz variable in
      let stride = Int32.shift_left 1l power_of_two in
      let lower_bound = Maths.ExtendedInt.Int
        (if Int32.(variable >= 0l) then 0l else Int32.(shift_right min_value power_of_two)) in
      let upper_bound = Maths.ExtendedInt.Int
        Int32.(shift_right_logical (variable land 0x7FFFFFFFl) power_of_two) in
      {stride; lower_bound; upper_bound; offset="",constant_value bf}


(** Return [true] iff [bf] may represent a non-zero value. *)
let may_be_true (bf : t) : bool =
  match bf with
  | Top -> true
  | Bottom -> false
  | Bit b -> Int32.(b.ones <> 0l)

(** Return [true] iff [bf] may represent zero. *)
let may_be_false (bf : t) : bool =
  contains bf 0l

(** Reverse the order of the 32 bits in a bitfield.
    [Top] and [Bottom] are preserved. *)
let reverse_bit_order (bf : t) : t =
  let reverse (i : int32) : int32 =
    List.init 32 ~f:(fun x -> x)
    |> List.fold ~init:0l ~f:(fun acc bit_index ->
        let bit = Int32.(bit_and i (shift_left 1l bit_index)) in
        if Int32.equal bit 0l then 
          acc
        else 
          Int32.bit_or acc (Int32.shift_left 1l Int.(31 - bit_index)))
  in
  match bf with
  | Top -> Top
  | Bottom -> Bottom
  | Bit bf -> Bit {ones = reverse bf.ones; zeros = reverse bf.zeros}


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

  let%test "Bitfield_header" =
    print_endline "\n_______ ____________________________ _______\n        BitField Module        \n------- ---------------------------- -------\n"; true

  let%test "to_string marker sanity" =
    let zeros = 0b11111111111111111111111111110000l in
    let ones  = 0b00000000000000000000000000111111l in
    let bf = Bit { zeros; ones } in
    Printf.printf "[to_string]\t z=%ld o=%ld -> %s\n" zeros ones (to_string bf); Out_channel.flush stdout;
    String.is_suffix ~suffix:"0::1111" (to_string bf)

  let%test "of_integer singleton & contains" =
    let n = 0x2Al in
    let bf = singleton n in
    let got_is_singleton = is_singleton bf in
    let got_contains = contains bf n in
    Printf.printf "[of_integer]\t n=%ld -> %s; singleton=%b; (contains %ld)=%b\n" n (to_string bf) got_is_singleton n got_contains; Out_channel.flush stdout;
    got_is_singleton && got_contains

  let%test "of_set 2,3,6 basic" =
    let s = Set.of_list (module Int32) [2l; 3l; 6l] in
    let bf = of_set s in
    Printf.printf "[of_set]\t {2,3,6} -> %s\n" (to_string bf); Out_channel.flush stdout;
    String.equal (to_string bf) "bf_0:1:"

  let%test "is_bottom Top/Bottom/Bit" =
    let a = Top and b = Bottom and c = singleton 7l in
    let ga = is_bottom a and gb = is_bottom b and gc = is_bottom c in
    Printf.printf "[is_bottom]\t ⊤->%b ⊥->%b 7->%b\n" ga gb gc; Out_channel.flush stdout;
    (not ga) && gb && (not gc)

  let%test "join Top/Bottom" =
    let x = singleton 0b1010l in
    let a = join Top x and b = join Bottom x and c = join x Bottom in
    Printf.printf "[join]\t x = %s ; Top ⊔ x -> %s ; Bottom ⊔ x -> %s ; x ⊔ Bottom -> %s\n" (to_string x) (to_string a) (to_string b) (to_string c); Out_channel.flush stdout;
    equal a Top && equal b x && equal c x

  let%test "join contains both singletons" =
    let x = singleton 0b0101l and y = singleton 0b0011l in
    let r = join x y in
    Printf.printf "[join]\t 0101 ⊔ 0011 -> %s\n" (to_string r); Out_channel.flush stdout;
    contains r 0b0101l && contains r 0b0011l && (not (is_singleton r))

  let%test "join commutative and associative" =
    let a = singleton 0x0Fl and b = singleton 0x33l and c = singleton 0x55l in
    let ab = join a b and ba = join b a in
    let left = join (join a b) c and right = join a (join b c) in
    Printf.printf "[join props]\t a⊔b=%s b⊔a=%s ; (a⊔b)⊔c=%s ; a⊔(b⊔c)=%s\n" (to_string ab) (to_string ba) (to_string left) (to_string right); Out_channel.flush stdout;
    equal ab ba && equal left right

  let%test "join n with lnot n -> Top (reduce)" =
    let n = 0x0Fl in
    let r = join (singleton n) (singleton (Int32.lnot n)) in
    Printf.printf "[join reduce]\t n ⊔ ~n -> %s\n" (to_string r); Out_channel.flush stdout;
    r = Top

  let%test "meet1" =
    let a = of_string "0010100"
    and b = of_string "1111011" in
    let m = meet a b in
    Printf.printf "[meet]\t %s meet %s -> %s\n" (to_string a) (to_string b) (to_string m); Out_channel.flush stdout;
    m = Bottom

  let%test "meet2" =
    let a = of_string "0:10100"
    and b = of_string "0110::0" in
    let m = meet a b in
    Printf.printf "[meet]\t %s meet %s -> %s\n" (to_string a) (to_string b) (to_string m); Out_channel.flush stdout;
    m = of_string "0110100"

  (* ---------- Bitwise ops ---------- *)
  let%test "and_ neutral/absorbing" =
    let x = singleton 0b101101l in
    let a = Top &. x and b = Bottom &. x in
    Printf.printf "[and]\t Top ∧ x -> %s  Bottom ∧ x -> %s\n" (to_string a) (to_string b); Out_channel.flush stdout;
    a = of_string "0:0::0:" && b = Bottom

  let%test "and_ Top" =
    let x = of_string "01:01" in
    Printf.printf "[and]\t Top ∧ %s -> %s\n" (x |> to_string) (to_string (Top &. x)); Out_channel.flush stdout;
    Top &. x = of_string "0::0:"

  let%test "and_" =
    let x = of_string "10:10:10:" in
    let y = of_string "111000:::" in
    let a = x &. y in
    Printf.printf "[and]\t %s ∧ %s -> %s\n" (to_string x) (to_string y) (to_string a); Out_channel.flush stdout;
    a = of_string "10:000:0:"

  let%test "and_ non_singleton membership" =
    let x = of_set (Set.of_list (module Int32) [0b0101l; 0b0111l]) in
    let y = of_set (Set.of_list (module Int32) [0b0011l; 0b1111l]) in
    let r = x &. y in
    Printf.printf "[and]\t x=%s y=%s -> %s\n" (to_string x) (to_string y) (to_string r); Out_channel.flush stdout;
    String.equal (to_string r) "bf_0::1"

  let%test "or_ absorbing/neutral" =
    let x = of_string "010:1" in
    let a = Top |. x and b = Bottom |. x in
    Printf.printf "[or]\t Top ∨ 010:1 -> %s  Bottom ∨ 010:1 -> %s\n" (to_string a) (to_string b); Out_channel.flush stdout;
    a = of_string ":1::1" && b = Bottom

  let%test "or_" =
    let x = of_string "10:10:10:" in
    let y = of_string "111000:::" in
    let a = x |. y in
    Printf.printf "[or]\t %s ∨ %s -> %s\n" (to_string x) (to_string y) (to_string a); Out_channel.flush stdout;
    a = of_string "11110:1::"

  let%test "or_ non_singleton membership" =
    let x = of_set (Set.of_list (module Int32) [0b0101l; 0b0111l]) in
    let y = of_set (Set.of_list (module Int32) [0b0011l; 0b1111l]) in
    let r = x |. y in
    Printf.printf "[or]\t x=%s y=%s -> %s\n" (to_string x) (to_string y) (to_string r); Out_channel.flush stdout;
    String.equal (to_string r) "bf_0:111"
  

  (* ---------- Algebraic properties sanity ---------- *)
  let%test "and_/or_ idempotent on singletons" =
    let x = singleton 0x55l in
    let r_and = x &. x and r_or = x |. x in
    Printf.printf "[idem]\t x = %s ; x ∧ x = %s ; x ∨ x = %s\n" (to_string x) (to_string r_and) (to_string r_or); Out_channel.flush stdout;
    equal r_and x && equal r_or x

  let%test "xor_ self -> 0" =
    let x = singleton 0x77l in
    let r = x <+> x in
    Printf.printf "[xor self]\t x = %s ; x ⊕ x -> %s\n" (to_string x) (to_string r); Out_channel.flush stdout;
    match r with Bit {zeros; ones} -> Int32.(ones = 0l && zeros = lnot 0l) | _ -> false

  let%test "xor_ with singletons" =
    let x = singleton 0b0101l and y = singleton 0b0011l in
    let r = x <+> y in
    Printf.printf "[xor]\t 0101 ⊕ 0011 -> %s\n" (to_string r); Out_channel.flush stdout;
    match r with Bit {zeros; ones} -> Int32.(zeros = lnot 0b0110l && ones = 0b0110l) | _ -> false

  let%test "xor_" =
    let x = of_string "10:10:10:" in
    let y = of_string "111000:::" in
    let a = x <+> y in
    Printf.printf "[xor]\t %s ∨ %s -> %s\n" (to_string x) (to_string y) (to_string a); Out_channel.flush stdout;
    a = of_string "01:10::::"

  let%test "xor_ non_singleton membership" =
    let x = of_set (Set.of_list (module Int32) [0b0101l; 0b0111l]) in
    let y = of_set (Set.of_list (module Int32) [0b0011l; 0b1111l]) in
    let r = x <+> y in
    Printf.printf "[xor ns]\t x=%s y=%s -> %s\n" (to_string x) (to_string y) (to_string r); Out_channel.flush stdout;
    String.equal (to_string r) "bf_0:::0"

  let%test "contains yes/no" =
    let bf = of_set (Set.of_list (module Int32) [2l;3l;6l]) in
    let c1 = contains bf 3l and c2 = contains bf 4l in
    Printf.printf "[contains]\t [2;3;6] 3? %b ; 4? %b\n" c1 c2; Out_channel.flush stdout; c1 && (not c2)

  (* ---------- Positivity / Negativity ---------- *)
  let%test "contains_positive_values" =
    let a = of_set (Set.of_list (module Int32) [-2l; 2l; 3l; 6l]) in
    let b = of_set (Set.of_list (module Int32) [-2l; -3l; -6l]) in
    let ga = may_be_positive a and gb = may_be_positive b in
    Printf.printf "[may_be_positive]\t [-2;2;3;6]->%b  [-2;-3;-6]->%b\n" ga gb; Out_channel.flush stdout; ga && (not gb)

  let%test "" =
    let a = of_set (Set.of_list (module Int32) [-1l; -2l]) in
    let b = of_set (Set.of_list (module Int32) [0l; 1l; 2l]) in
    let ga = may_be_negative a and gb = may_be_negative b in
    Printf.printf "[may_be_negative]\t [-1;-2]->%b  [0;1;2]->%b\n" ga gb; Out_channel.flush stdout; ga && (not gb)

  (* ---------- Shifts: left / right (unsigned & signed) ---------- *)
  let%test "shift_left with exact amount" =
    let x = singleton 0b0000_0001l in
    let sh = singleton 2l in
    let r = x << sh in
    Printf.printf "[shl]\t %s << 2 -> %s\n" (to_string x) (to_string r); Out_channel.flush stdout;
    match r with Bit {zeros; ones} -> Int32.(ones = 0b0000_0100l && (ones land zeros) = 0l) | _ -> false

  let%test "shift_left with set of amounts (join)" =
    let x = singleton 1l in
    let sh = of_set (Set.of_list (module Int32) [1l;3l]) in
    let r = x << sh in
    Printf.printf "[shl]\t 1 << {1,3} -> %s\n" (to_string r); Out_channel.flush stdout; 
    r = of_string "0:0:0"

  let%test "shift_left clamps weird magnitudes" =
    let x = singleton 2l in
    (* shift amounts outside 0..31 are masked internally; include -32 and 40 (as int32) *)
    let weird = of_set (Set.of_list (module Int32) [1l;16l;32l]) in
    let r = x << weird in
    Printf.printf "[shl]\t 2 << {0,1,16,17,32,33,48,49} (masked=>{0,1,16,17}) -> %s\n" (to_string r); Out_channel.flush stdout;
    String.equal (to_string r) "bf_0::00000000000000::0"

  let%test "shift_left with shift value = top" =
    let x = singleton 1l in
    let sh = Top in
    let r = x << sh in
    Printf.printf "[shl]\t 1 << Top -> %s\n" (to_string r); Out_channel.flush stdout;
    r = Top

  let%test "shift_left with shift value = top2" =
    let x = singleton 2l in
    let sh = Top in
    let r = x << sh in
    Printf.printf "[shl]\t 1 << Top -> %s\n" (to_string r); Out_channel.flush stdout;
    r = of_string ":0"

  let%test "shift_right_unsigned basic" =
    let neg_one = Bit {zeros = 0l; ones = (-1l)} in
    let result = neg_one >>. one in
    Printf.printf "[shr_u]\t -%s >>u 1 -> %s\n" (to_string neg_one) (to_string result); Out_channel.flush stdout;
    equal result (Bit {zeros = 0b10000000000000000000000000000000l; ones = 0b01111111111111111111111111111111l})

  let%test "shift_right_unsigned basic 2" =
    let neg32 = singleton (-32l) in
    let result = neg32 >>. one >>. one in
    Printf.printf "[shr_u]\t -32 >>u 2 -> %s\n" (to_string result); Out_channel.flush stdout;
    result = of_string "00111111111111111111111111111000"

  let%test "shift_right_signed retains sign" =
    let x = singleton (-8l) in
    let r = x >>- (singleton 1l) in
    Printf.printf "[shr_s]\t -8 >>s 1 -> %s\n" (to_string r); Out_channel.flush stdout; 
    equal (Bit {zeros = 0b11l; ones = 0b11111111111111111111111111111100l}) r

  (* ---------- Non-singleton tests for shifts and bitwise ops ---------- *)
  let%test "shift_left non_singleton membership" =
    let x = of_set (Set.of_list (module Int32) [1l; 3l]) in
    let sh = of_set (Set.of_list (module Int32) [1l; 2l]) in
    let r = x << sh in
    Printf.printf "[shl ns]\t x=%s sh=%s -> %s\n" (to_string x) (to_string sh) (to_string r); Out_channel.flush stdout;
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
    Printf.printf "[shr_u ns]\t x=%s sh=%s -> %s\n" (to_string x) (to_string sh) (to_string r); Out_channel.flush stdout;
    (* 8>>1=4, 8>>2=2, 12>>1=6, 12>>2=3 must be included *)
    (not (is_singleton r))
    && contains r 4l && contains r 2l && contains r 6l && contains r 3l

  let%test 
    "shift_right_signed non_singleton membership (negatives preserve sign)" =
    let x = of_set (Set.of_list (module Int32) [-8l; -4l]) in
    let sh = of_set (Set.of_list (module Int32) [1l; 2l]) in
    let r = shift_right_signed x sh in
    Printf.printf "[shr_s ns]\t x=%s sh=%s -> %s\n" (to_string x) (to_string sh) (to_string r); Out_channel.flush stdout;
    (* -8>>1=-4, -8>>2=-2, -4>>1=-2, -4>>2=-1 must be included *)
    (not (is_singleton r))
    && contains r (-4l) && contains r (-2l) && contains r (-1l)


  let%test "constant_value picks forced bits" =
    let bf = of_set (Set.of_list (module Int32) [0b001001010l; 0b011111010l]) in
    let c = constant_value bf in
    Printf.printf "[constant_value]\t %s -> %s\n" (to_string bf) (c |> singleton |> to_string); Out_channel.flush stdout; Int32.(c = 0b001001010l)

  let%test "variable_content examples" =
    let bf = of_set (Set.of_list (module Int32) [0b00001010l; 0b01111011l]) in
    let vc = to_RIC bf in
    let lo, hi =
      match vc.lower_bound, vc.upper_bound with
      | Maths.ExtendedInt.Int lo, Maths.ExtendedInt.Int hi -> lo,hi
      | _ -> 0l, 0l in
    Printf.printf "[variable_content]\t %s -> stride=%ld lo=%ld hi=%s\n" (to_string bf) (vc.stride) lo (hi |> singleton |> to_string); Out_channel.flush stdout;
    Int32.(vc.stride = 1l) && Maths.ExtendedInt.(vc.lower_bound = Int 0l) && Maths.ExtendedInt.(vc.upper_bound = Int 0b01110001l)

  let%test "variable_values examples2" =
    let bf = of_string ":10:10:" in
    let vc = to_RIC bf in
    let lo, hi =
      match vc.lower_bound, vc.upper_bound with
      | Maths.ExtendedInt.Int lo, Maths.ExtendedInt.Int hi -> lo,hi
      | _ -> 0l, 0l in
    Printf.printf "[variable_content]\t %s -> stride=%ld lo=%s hi=%s\n" (to_string bf) (vc.stride) (lo |> singleton |> to_string) (hi |> singleton |> to_string);  Out_channel.flush stdout;
    (* k >= 0 && Int32.(lo = shift_right min_value k) && Int32.(hi = 0b01111111111111111111111111001001l) *)
    Int32.(vc.stride = 1l) && Maths.ExtendedInt.(vc.lower_bound = Int Int32.min_value) && Int32.(hi = 0b01111111111111111111111111001001l)

  let%test "variable_values examples3" =
    let bf = of_string ":01:01:0000101110011100001101100" in
    let vc = to_RIC bf in
    let lo, hi =
      match vc.lower_bound, vc.upper_bound with
      | Maths.ExtendedInt.Int lo, Maths.ExtendedInt.Int hi -> lo,hi
      | _ -> 0l, 0l in
    Printf.printf "[variable_content]\t %s -> stride=%ld lo=%s hi=%s\n" (to_string bf) (vc.stride) (lo |> singleton |> to_string) (hi |> singleton |> to_string); Out_channel.flush stdout;
    (* Int.(k = 25) && Int32.(lo = shift_right min_value k) && Int32.(hi = 0b01001l) *)
    Int32.(vc.stride = 0b10000000000000000000000000l) && Maths.ExtendedInt.(vc.lower_bound = Int (-64l)) && Int32.(hi = 0b01001l)

  let%test "reverse_bit_order singleton" =
    let x = singleton 0x00000005l in
    let r = reverse_bit_order x in
    Printf.printf "[reverse_bit_order]\t %s -> %s\n" (to_string x) (to_string r); Out_channel.flush stdout;
    r = singleton 0xA0000000l

  let%test "reverse_bit_order non_singleton" =
    let x = of_string "10:" in
    let r = reverse_bit_order x in
    Printf.printf "[reverse_bit_order]\t %s -> %s\n" (to_string x) (to_string r); Out_channel.flush stdout;
    r = of_string ":0111111111111111111111111111111"

  let%test "reverse_bit_order preserves Top and Bottom" =
    let top = reverse_bit_order Top in
    let bottom = reverse_bit_order Bottom in
    Printf.printf "[reverse_bit_order]\t Top -> %s ; Bottom -> %s\n" (to_string top) (to_string bottom); Out_channel.flush stdout;
    top = Top && bottom = Bottom

end)