open Core
open Maths

(* TODO: put this somewhere else *)
let (|>>) (c, i : 'a * 'b) (f : 'a -> 'b -> 'c) : 'c = f c i

(** {1 Congruence}

    Congruences model arithmetic progressions [stride * ℤ + offset]. Offsets are
    either absolute [("", c)] or relative [(var, c)]. We use [⊤] to denote all
    integers and [⊥] for the empty set.

    {b Normal forms}
    - [Top] ≡ [1ℤ + o] for any [o].
    - [Bottom] ≡ ∅.
    - [stride = 0] encodes a singleton with the given offset.
*)
  type congruence = {
    stride : int32;
    offset : string * int32
  }

  (** Abstract values for congruences.
      - [Top] : all integers (ℤ)
      - [Bottom] : empty set (∅)
      - [Congruence {stride; offset}] : the set [stride * ℤ + offset] *)
  type t =
    | Top
    | Bottom
    | Congruence of congruence

  let make (stride : int32) (offset : string * int32) : t =
    Congruence {stride; offset}

  (** [equal c1 c2]
      Set equivalence modulo standard congruence identities.
      Recognizes, e.g., [Top ≡ 1ℤ+o], singleton equality for [stride=0], and
      equivalence of offsets modulo the stride when bases match. *)
  let equal (c1 : t) (c2 : t) : bool =
    match c1, c2 with 
    | Top, Top | Bottom, Bottom -> true
    | Top, Congruence { stride = 1l; _ }
    | Congruence { stride = 1l; _ }, Top -> true 
    | Congruence { stride = 1l; _ }, Congruence { stride = 1l; _ } -> true
    | Congruence { stride = 0l; offset = (v1, o1) }, Congruence { stride = 0l; offset = (v2, o2) } ->
      String.(v1 = v2) && Int32.(o1 = o2)
    | Congruence { stride = s1; offset = (v1, o1) }, Congruence { stride = s2; offset = (v2, o2) } ->
      Int32.(s1 = s2) && String.(v1 = v2) && Int32.((o1 - o2) % s1 = 0l)
    | _ -> false

  let (=) = equal
  let (<>) (c1 : t) (c2 : t) : bool = not (c1 = c2)


  (** [to_string c]
      Pretty‑prints in compact math style, e.g. ["4ℤ+(x+1)"] or ["ℤ"/"∅"]. *)
  let to_string (c : t) : string =
    match c with 
    | Top -> "ℤ" 
    | Bottom -> "∅"
    | Congruence { stride = s; offset = offset } ->
      let stride = 
        if Int32.(s = 0l) then
          ""
        else
          (if Int32.(s = 1l) then "" else Int32.to_string s) ^ "ℤ" in
      let offset = 
        match offset with 
        | ("", offset) ->
          if Int32.(offset = 0l) then
            ""
          else
            Int32.to_string offset
        | (var, offset) ->
          if Int32.(offset = 0l) then
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

  (** [join c1 c2]
      Least upper bound (over‑approximate union).
      - [Top] absorbs; [Bottom] is neutral.
      - If bases match, uses gcd(strides, offset diff) to compute the resulting stride
        and keeps the smaller offset representative; otherwise returns [Top]. *)
  let join (c1 : t) (c2 : t) : t =
    match c1, c2 with 
    | Top, _ | _, Top -> Top 
    | Bottom, c | c, Bottom -> c 
    (* | Congruence { offset = (_, o1); _ }, Congruence { offset = (_, o2); _ } 
      when Int32.(o1 = (-2147483648l)) || Int32.(o2 = (-2147483648l)) -> Top *)
    | Congruence { stride = s1; offset = (v1, o1) }, Congruence { stride = s2; offset = (v2, o2) } ->
      if String.equal v1 v2 then
        let difference = Int64.abs Int64.(Int64.of_int32_exn o1 - Int64.of_int32_exn o2) in
        if Int64.(difference > Int64.of_int32_exn Int32.max_value) then
          Top
        else
          let new_stride = difference |> Int32.of_int64_exn |> gcd (gcd s1 s2) in
          let new_offset =
            if Int32.(new_stride = 0l) then
              (v1, o1)
            else if Int32.(o1 % new_stride < 0l) then
              (v1, Int32.((o1 % new_stride) + new_stride))
            else
              (v1, Int32.(o1 % new_stride)) in
          (new_stride, new_offset) |>> make
      else
        Top

  (** [meet c1 c2]
      Greatest lower bound (intersection).
      - Handles singleton/constant cases directly.
      - If bases match and CRT side‑conditions hold, uses lcm for the stride and the
        Chinese Remainder Theorem to select a consistent offset; returns [Bottom]
        when no solution exists. *)
  let meet (c1 : t) (c2 : t) : t =
    match c1, c2 with
    | Top, c | c, Top -> c 
    | Bottom, _ | _, Bottom -> Bottom 
    (* two singletons: *)
    | Congruence {stride = 0l; offset = (v1, o1)}, Congruence {stride = 0l; offset = (v2, o2)} ->
      if String.(v1 = v2) && Int32.(o1 = o2) then c1 else Bottom
    (* Singleton meet non-singleton: *)
    | Congruence {stride = 0l; offset = (v1, o1)}, Congruence {stride = s2; offset = (v2, o2)} 
    | Congruence {stride = s2; offset = (v2, o2)}, Congruence {stride = 0l; offset = (v1, o1)} ->
      if String.(v1 = v2) && Int32.(o1 % s2 = o2) then Congruence {stride = 0l; offset = (v1, o1)} else Bottom
    (* General case: *)
    | Congruence {stride = s1; offset = (v1, o1)}, Congruence {stride = s2; offset = (v2, o2)} ->
      let gcd_stride = gcd s1 s2 in 
      if String.(v1 = v2) && Int32.((o1 - o2) % gcd_stride = 0l) then 
        let new_stride = lcm s1 s2 in 
        let new_offset = v1, fst (chinese_remainder s1 o1 s2 o2) in
        (new_stride, new_offset) |>> make
      else
        Bottom
  
  (** [widen c1 c2]
      Widening for congruences. Here identical to [join]. *)
  let widen : t -> t -> t = join

  (** [sum c1 c2]
      Abstract addition of progressions: gcd of strides, offsets added.
      Relative bases are combined using [add_relative_offsets]. *)
  let plus (c1 : t) (c2 : t) : t =
    match c1, c2 with
    | Top, _ | _, Top -> Top
    | Bottom, c | c, Bottom -> c
    | Congruence {stride = s1; offset = (v1, o1)},
      Congruence {stride = s2; offset = (v2, o2)} ->
        let sum_of_offsets = Int64.(Int32.to_int64 o1 + Int32.to_int64 o2) in
        if Int64.(sum_of_offsets > Int32.to_int64 Int32.max_value) then
          Top
        else
          let new_stride = gcd s1 s2 in
          let new_offset = if Int32.(new_stride <> 0l) then Int32.((o1 + o2) % new_stride) else Int32.(o1 + o2) in
          let new_offset = if Int32.(new_offset < 0l) then Int32.(new_offset + new_stride) else new_offset in 
          (new_stride, (add_relative_offsets v1 v2, new_offset)) |>> make
  
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

let%test_module "Congruence tests" = (module struct

  let%test "RIC_tests" =
  print_endline "\n_______ ____________________________ _______\n        Congruence module        \n------- ---------------------------- -------\n"; true

  (* Top should be equal to Congruence with stride 1 and offset 0. *)
  let%test "Congruence: top equals stride1 offset0" =
    equal Top (Congruence {stride = 1l; offset = ("", 0l)})

  (* Top should be equal to Congruence with stride 1 and offset (g0, 10). *)
  let%test "Congruence: top equals stride1 offset(\"g0\", 10)" =
    equal Top (Congruence {stride = 1l; offset = ("g0", 10l)})

  (* Congruences with same stride and same absolute offset are equal. *)
  let%test "Congruence: same stride same absolute offset" =
    equal (Congruence {stride = 3l; offset = ("", 5l)})
                      (Congruence {stride = 3l; offset = ("", 5l)})

  (* Congruences with same stride and same absolute offset are not equal but equivalent. *)
  let%test "Congruence: same stride, equivalent absolute offsets" =
    equal (Congruence {stride = 3l; offset = ("", 5l)})
                      (Congruence {stride = 3l; offset = ("", 2l)})

  (* Congruences with same stride and same absolute offset are not equal but equivalent. *)
  let%test "Congruence: same stride, equivalent relative offsets" =
    equal (Congruence {stride = 3l; offset = ("v", 5l)})
                      (Congruence {stride = 3l; offset = ("v", 2l)})

  (* Congruences with different strides are not equal. *)
  let%test "Congruence: different strides" =
    not (equal (Congruence {stride = 3l; offset = ("", 5l)})
                          (Congruence {stride = 4l; offset = ("", 5l)}))

  (* Congruences with different absolute offsets are not equal. *)
  let%test "Congruence: different absolute offsets that are not equivalent" =
    not (equal (Congruence {stride = 3l; offset = ("", 5l)})
                          (Congruence {stride = 3l; offset = ("", 6l)}))

  (* Bottom is equal to Bottom. *)
  let%test "Congruence: bottom equals bottom" =
    equal Bottom Bottom

  (* Bottom is not equal to Top. *)
  let%test "Congruence: bottom not equal top" =
    not (equal Bottom Top)

  (* Relative congruences with same var and offset are equal. *)
  let%test "Congruence: same relative var and offset" =
    equal
      (Congruence {stride = 2l; offset = ("x", 4l)})
      (Congruence {stride = 2l; offset = ("x", 4l)})

  (* Relative congruences with same var but non equivalent offsets are not equal. *)
  let%test "Congruence: same var, non equivalent offsets" =
    not (equal
            (Congruence {stride = 2l; offset = ("x", 4l)})
            (Congruence {stride = 2l; offset = ("x", 5l)}))

  (* Relative congruences with different vars are not equal. *)
  let%test "Congruence: different vars" =
    not (equal
            (Congruence {stride = 2l; offset = ("x", 4l)})
            (Congruence {stride = 2l; offset = ("y", 4l)}))

  (* Congruences with same offset but different strides are not equal. *)
  let%test "Congruence: same offset different strides" =
    not (equal
            (Congruence {stride = 2l; offset = ("x", 4l)})
            (Congruence {stride = 3l; offset = ("x", 4l)}))

  (* Top is not equal to Bottom. *)
  let%test "Congruence: top not equal bottom" =
    not (equal Top Bottom)

  (* Top is not equal to Congruence with stride 2 and offset 0. *)
  let%test "Congruence: top not equal stride2 offset0" =
    not (equal Top (Congruence {stride = 2l; offset = ("", 0l)}))

  (* Stride is equal to 1, one relative offset, one absolute offset *)
  let%test "Congruence: both strides are equal to 1" =
    equal (Congruence {stride = 1l; offset = ("", 0l)}) 
                      (Congruence {stride = 1l; offset = ("v", 7l)})

  (* to string of Top should be "ℤ". *)
  let%test "Congruence: to string top" =
    String.equal (to_string Top) "ℤ"

  (* to_string of Bottom should be "⊥". *)
  let%test "Congruence: to_string_bottom" =
    String.equal (to_string Bottom) "∅"

  (* to_string of stride 1, absolute offset 0 should be "ℤ". *)
  let%test "Congruence: to_string stride1 offset0" =
    String.equal
      (to_string (Congruence {stride = 1l; offset = ("", 0l)}))
      "ℤ"

  (* to_string of stride 2, absolute offset 3 should be "2ℤ + 3". *)
  let%test "Congruence: to_string stride2 offset3" =
    String.equal
      (to_string (Congruence {stride = 2l; offset = ("", 3l)}))
      "2ℤ+3"

  (* to_string of stride 4, relative offset ("x", 1) should be "4ℤ + (x + 1)". *)
  let%test "Congruence: to_string relative" =
    String.equal
      (to_string (Congruence {stride = 4l; offset = ("x", 1l)}))
      "4ℤ+(x+1)"
  
  (* to_string of stride 4, relative offset ("x", 1) should be "4ℤ + (x + 1)". *)
  let%test "Congruence: to_string relative offset0" =
    String.equal
      (to_string (Congruence {stride = 10l; offset = ("x", 0l)}))
      "10ℤ+x"

  (* Joining Top with anything should return Top. *)
  let%test "Congruence: join_top_left" =
    equal
      (join Top (Congruence {stride = 2l; offset = ("", 0l)}))
      Top

  let%test "Congruence: join_top_right" =
    equal
      (join (Congruence {stride = 2l; offset = ("v", 3l)}) Top)
      Top

  (* Joining Bottom with a congruence should return that  *)
  let%test "Congruence: join_bottom_left" =
    equal
      (join Bottom (Congruence {stride = 27l; offset = ("", 36l)}))
      (Congruence {stride = 27l; offset = ("", 36l)})

  let%test "Congruence: join_bottom_right" =
    equal
      (join (Congruence {stride = 2l; offset = ("", 3l)}) Bottom)
      (Congruence {stride = 2l; offset = ("", 3l)})

  (* Joining two equal congruences returns the same  *)
  let%test "Congruence: join_equal_congruences" =
    let c = Congruence {stride = 4l; offset = ("", 1l)} in
    equal (join c c) c

  (* Joining congruences with same offset but different stride returns least common multiple stride. *)
  let%test "Congruence: join_same_offset_diff_stride" =
    equal
      (join
        (Congruence {stride = 2l; offset = ("", 3l)})
        (Congruence {stride = 4l; offset = ("", 3l)}))
      (Congruence {stride = 2l; offset = ("", 3l)})  (* gcd(2,4)=2; offset=3 *)

  (* Joining congruences with different offsets. *)
  let%test "Congruence: join (2ℤ+3) and (2ℤ+2) => Top" =
    let c1 = Congruence {stride = 2l; offset = ("", 3l)} in
    let c2 = Congruence {stride = 2l; offset = ("", 2l)} in 
    let j = join c1 c2 in 
    print_endline ("[JOIN of congruences]     (" ^ to_string c1 ^ ") ⊔ (" ^ to_string c2 ^ ") = " ^ to_string j);
    equal j Top

  (* Joining congruences with different offsets. *)
  let%test "Congruence: join (10ℤ+3) and (10ℤ+8) => 5ℤ+3" =
    let c1 = Congruence {stride = 10l; offset = ("", 3l)} in
    let c2 = Congruence {stride = 10l; offset = ("", 8l)} in 
    let j = join c1 c2 in 
    print_endline ("[JOIN of congruences]     (" ^ to_string c1 ^ ") ⊔ (" ^ to_string c2 ^ ") = " ^ to_string j);
    equal j (Congruence {stride = 5l; offset = ("", 3l)})

  (* Joining congruences with different types of offsets should return Top. *)
  let%test "Congruence: join_relative_and_absolute" =
    let c1 = (Congruence {stride = 2l; offset = ("", 3l)}) in
    let c2 = (Congruence {stride = 2l; offset = ("x", 3l)}) in
    let j = join c1 c2 in
    print_endline ("[JOIN of congruences]     (" ^ to_string c1 ^ ") ⊔ (" ^ to_string c2 ^ ") = " ^ to_string j);
    equal j Top

  (* Joining congruences with equivalent relative offsets. *)
  let%test "Congruence: join_equivalent_relative_offsets" =
    let c1 = Congruence {stride = 20l; offset = ("v", 3l)} in
    let c2 = Congruence {stride = 20l; offset = ("v", 23l)} in 
    let j = join c1 c2 in 
    print_endline ("[JOIN of congruences]     (" ^ to_string c1 ^ ") ⊔ (" ^ to_string c2 ^ ") = " ^ to_string j);
    equal j c1

  (* Joining congruences with equivalent relative offsets. *)
  let%test "Congruence: join_non_equivalent_relative_offsets" =
    let c1 = Congruence {stride = 20l; offset = ("v", 3l)} in
    let c2 = Congruence {stride = 20l; offset = ("v", 21l)} in 
    let j = join c1 c2 in 
    print_endline ("[JOIN of congruences]     (" ^ to_string c1 ^ ") ⊔ (" ^ to_string c2 ^ ") = " ^ to_string j);
    equal j (Congruence {stride = 2l; offset = ("v", 1l)}
    )

  (* Joining congruences with different relative variables returns Top. *)
  let%test "Congruence: join_diff_relative_vars" =
    let c1 = Congruence {stride = 20l; offset = ("v", 3l)} in
    let c2 = Congruence {stride = 20l; offset = ("x", 23l)} in 
    let j = join c1 c2 in 
    print_endline ("[JOIN of congruences]     (" ^ to_string c1 ^ ") ⊔ (" ^ to_string c2 ^ ") = " ^ to_string j);
    equal j Top

  let%test "Congruence: join normalizes negative offset" =
    let c1 = Congruence {stride = 0l; offset = ("", -3l)} in
    let c2 = Congruence {stride = 4l; offset = ("", 1l)} in
    let j = join c1 c2 in
    print_endline
      ("[JOIN of congruences]     (" ^ to_string c1 ^
      ") ⊔ (" ^ to_string c2 ^
      ") = " ^ to_string j);
    (* equal j (Congruence {stride = 4l; offset = ("", 1l)}) *)
    match j with
    | Congruence {stride = 4l; offset = ("", 1l)} -> true
    | _ -> false

  let%test "Congruence: join negative offset divisible by stride normalizes to zero" =
    let c1 = Congruence {stride = 0l; offset = ("", -4l)} in
    let c2 = Congruence {stride = 4l; offset = ("", 0l)} in
    let j = join c1 c2 in
    print_endline
      ("[JOIN of congruences]     (" ^ to_string c1 ^
      ") ⊔ (" ^ to_string c2 ^
      ") = " ^ to_string j);
    equal j (Congruence {stride = 4l; offset = ("", 0l)})

  let%test "Congruence: join avoids Int32.abs min_value overflow" =
    let c1 = Congruence {stride = 4l; offset = ("", 0l)} in
    let c2 = Congruence {stride = 4l; offset = ("", Int32.min_value)} in
    let j = join c1 c2 in
    print_endline
      ("[JOIN of congruences]     (" ^ to_string c1 ^
      ") ⊔ (" ^ to_string c2 ^
      ") = " ^ to_string j);
    equal j Top

  let%test "Congruence: join avoids subtraction overflow near bounds" =
    let c1 = Congruence {stride = 4l; offset = ("", Int32.max_value)} in
    let c2 = Congruence {stride = 4l; offset = ("", Int32.min_value)} in
    let j = join c1 c2 in
    print_endline
      ("[JOIN of congruences]     (" ^ to_string c1 ^
      ") ⊔ (" ^ to_string c2 ^
      ") = " ^ to_string j);
    equal j Top

  let%test "Congruence: join handles large safe offset difference" =
    let c1 = Congruence {stride = 8l; offset = ("", Int32.max_value)} in
    let c2 = Congruence {stride = 8l; offset = ("", 0l)} in
    let j = join c1 c2 in
    print_endline
      ("[JOIN of congruences]     (" ^ to_string c1 ^
      ") ⊔ (" ^ to_string c2 ^
      ") = " ^ to_string j);
    equal j (Congruence {stride = 1l; offset = ("", 0l)})

  (* Meeting Bottom with anything should return Bottom. *)
  let%test "Congruence: meet_bottom_left" =
    equal
      (meet Bottom (Congruence {stride = 2l; offset = ("", 0l)}))
      Bottom

  let%test "Congruence: meet_bottom_right" =
    equal
      (meet (Congruence {stride = 2l; offset = ("v", 3l)}) Bottom)
      Bottom

  (* Meeting Top with a congruence should return that  *)
  let%test "Congruence: meet_top_left" =
    equal
      (meet Top (Congruence {stride = 27l; offset = ("", 36l)}))
      (Congruence {stride = 27l; offset = ("", 36l)})

  let%test "Congruence: meet_top_right" =
    equal
      (meet (Congruence {stride = 2l; offset = ("", 3l)}) Top)
      (Congruence {stride = 2l; offset = ("", 3l)})

  (* Meeting two equal congruences returns the same  *)
  let%test "Congruence: meet_equal_congruences" =
    let c = Congruence {stride = 4l; offset = ("", 1l)} in
    equal (meet c c) c

  (* Meeting congruences with same offset but different stride. *)
  let%test "Congruence: meet_same_offset_diff_stride" =
    let c1 = (Congruence {stride = 2l; offset = ("", 3l)}) in
    let c2 = (Congruence {stride = 4l; offset = ("", 3l)}) in
    let m = meet c1 c2 in
    print_endline ("[MEET of congruences]     (" ^ to_string c1 ^ ") ⊓ (" ^ to_string c2 ^ ") = " ^ to_string m);
    equal m c2

    (* Meeting congruences with same offset but different stride. *)
  let%test "Congruence: meet_same_offset_diff_stride2" =
    let c1 = (Congruence {stride = 2l; offset = ("", 3l)}) in
    let c2 = (Congruence {stride = 5l; offset = ("", 3l)}) in
    let m = meet c1 c2 in
    print_endline ("[MEET of congruences]     (" ^ to_string c1 ^ ") ⊓ (" ^ to_string c2 ^ ") = " ^ to_string m);
    equal m (Congruence {stride = 10l; offset = ("", 3l)}) (* stride = lcm 2 5*)

  (* Meeting congruences with different offsets. *)
  let%test "Congruence: meet (2ℤ + 3) and (2ℤ + 2) => Top" =
    let c1 = Congruence {stride = 2l; offset = ("", 3l)} in
    let c2 = Congruence {stride = 2l; offset = ("", 2l)} in 
    let m = meet c1 c2 in 
    print_endline ("[MEET of congruences]     (" ^ to_string c1 ^ ") ⊓ (" ^ to_string c2 ^ ") = " ^ to_string m);
    equal m Bottom

  (* Meeting congruences with different offsets. *)
  let%test "Congruence: meet (10ℤ + 3) and (10ℤ + 8) => Bottom" =
    let c1 = Congruence {stride = 10l; offset = ("", 3l)} in
    let c2 = Congruence {stride = 10l; offset = ("", 8l)} in 
    let m = meet c1 c2 in 
    print_endline ("[MEET of congruences]     (" ^ to_string c1 ^ ") ⊓ (" ^ to_string c2 ^ ") = " ^ to_string m);
    equal m Bottom

  (* Joining congruences with different types of offsets should return Top. *)
  let%test "Congruence: meet_relative_and_absolute" =
    let c1 = (Congruence {stride = 2l; offset = ("", 3l)}) in
    let c2 = (Congruence {stride = 2l; offset = ("x", 3l)}) in
    let m = meet c1 c2 in
    print_endline ("[MEET of congruences]     (" ^ to_string c1 ^ ") ⊓ (" ^ to_string c2 ^ ") = " ^ to_string m);
    equal m Bottom

  (* Meeting congruences with equivalent relative offsets. *)
  let%test "Congruence: meet_equivalent_relative_offsets" =
    let c1 = Congruence {stride = 20l; offset = ("v", 3l)} in
    let c2 = Congruence {stride = 20l; offset = ("v", 23l)} in 
    let m = meet c1 c2 in 
    print_endline ("[MEET of congruences]     (" ^ to_string c1 ^ ") ⊓ (" ^ to_string c2 ^ ") = " ^ to_string m);
    equal m c1

  let%test "Congruence: meet with singleton congruence" =
    let c1 = Congruence {stride = 0l; offset = ("v", 3l)} in
    let c2 = Congruence {stride = 20l; offset = ("v", 3l)} in
    let  m = meet c1 c2 in 
    print_endline ("[MEET of congruences]     (" ^ to_string c1 ^ ") ⊓ (" ^ to_string c2 ^ ") = " ^ to_string m);
    equal m (Congruence {stride = 0l; offset = ("v", 3l)})

  (* Meeting congruences with equivalent relative offsets. *)
  let%test "Congruence: join_non_equivalent_relative_offsets" =
    let c1 = Congruence {stride = 20l; offset = ("v", 3l)} in
    let c2 = Congruence {stride = 20l; offset = ("v", 21l)} in 
    let m = meet c1 c2 in 
    print_endline ("[MEET of congruences]     (" ^ to_string c1 ^ ") ⊓ (" ^ to_string c2 ^ ") = " ^ to_string m);
    equal m Bottom

  (* Meeting congruences with different relative variables returns Bottom. *)
  let%test "Congruence: join_diff_relative_vars" =
    let c1 = Congruence {stride = 20l; offset = ("v", 3l)} in
    let c2 = Congruence {stride = 20l; offset = ("x", 23l)} in 
    let m = meet c1 c2 in 
    print_endline ("[MEET of congruences]     (" ^ to_string c1 ^ ") ⊓ (" ^ to_string c2 ^ ") = " ^ to_string m);
    equal m Bottom

  let%test "Congruence: meet singleton contained in congruence" =
    let c1 = Congruence {stride = 0l; offset = ("", 5l)} in
    let c2 = Congruence {stride = 4l; offset = ("", 1l)} in
    let m = meet c1 c2 in
    print_endline
      ("[MEET of congruences]     (" ^ to_string c1 ^
      ") ⊓ (" ^ to_string c2 ^
      ") = " ^ to_string m);
    equal m c1

  let%test "Congruence: meet singleton not contained in congruence" =
    let c1 = Congruence {stride = 0l; offset = ("", 6l)} in
    let c2 = Congruence {stride = 4l; offset = ("", 1l)} in
    let m = meet c1 c2 in
    print_endline
      ("[MEET of congruences]     (" ^ to_string c1 ^
      ") ⊓ (" ^ to_string c2 ^
      ") = " ^ to_string m);
    equal m Bottom

  let%test "Congruence: meet congruence with contained singleton" =
    let c1 = Congruence {stride = 4l; offset = ("", 1l)} in
    let c2 = Congruence {stride = 0l; offset = ("", 5l)} in
    let m = meet c1 c2 in
    print_endline
      ("[MEET of congruences]     (" ^ to_string c1 ^
      ") ⊓ (" ^ to_string c2 ^
      ") = " ^ to_string m);
    equal m c2

  let%test "Congruence: meet congruence with non-contained singleton" =
    let c1 = Congruence {stride = 4l; offset = ("", 1l)} in
    let c2 = Congruence {stride = 0l; offset = ("", 6l)} in
    let m = meet c1 c2 in
    print_endline
      ("[MEET of congruences]     (" ^ to_string c1 ^
      ") ⊓ (" ^ to_string c2 ^
      ") = " ^ to_string m);
    equal m Bottom

  let%test "Congruence: sum top left" =
    let c = Congruence {stride = 4l; offset = ("", 1l)} in
    let s = plus Top c in
    print_endline
      ("[SUM of congruences]      (" ^ to_string Top ^
      ") + (" ^ to_string c ^
      ") = " ^ to_string s);
    equal s Top

  let%test "Congruence: sum top right" =
    let c = Congruence {stride = 4l; offset = ("", 1l)} in
    let s = plus c Top in
    print_endline
      ("[SUM of congruences]      (" ^ to_string c ^
      ") + (" ^ to_string Top ^
      ") = " ^ to_string s);
    equal s Top

  let%test "Congruence: sum bottom left" =
    let c = Congruence {stride = 4l; offset = ("", 1l)} in
    let s = plus Bottom c in
    print_endline
      ("[SUM of congruences]      (" ^ to_string Bottom ^
      ") + (" ^ to_string c ^
      ") = " ^ to_string s);
    equal s c

  let%test "Congruence: sum bottom right" =
    let c = Congruence {stride = 4l; offset = ("", 1l)} in
    let s = plus c Bottom in
    print_endline
      ("[SUM of congruences]      (" ^ to_string c ^
      ") + (" ^ to_string Bottom ^
      ") = " ^ to_string s);
    equal s c

  let%test "Congruence: sum two absolute congruences" =
    let c1 = Congruence {stride = 6l; offset = ("", 2l)} in
    let c2 = Congruence {stride = 10l; offset = ("", 3l)} in
    let s = plus c1 c2 in
    print_endline
      ("[SUM of congruences]      (" ^ to_string c1 ^
      ") + (" ^ to_string c2 ^
      ") = " ^ to_string s);
    equal s (Congruence {stride = 2l; offset = ("", 5l)})

  let%test "Congruence: sum singleton and congruence" =
    let c1 = Congruence {stride = 0l; offset = ("", 7l)} in
    let c2 = Congruence {stride = 4l; offset = ("", 1l)} in
    let s = plus c1 c2 in
    print_endline
      ("[SUM of congruences]      (" ^ to_string c1 ^
      ") + (" ^ to_string c2 ^
      ") = " ^ to_string s);
    equal s (Congruence {stride = 4l; offset = ("", 8l)})

  let%test "Congruence: sum two singletons" =
    let c1 = Congruence {stride = 0l; offset = ("", 7l)} in
    let c2 = Congruence {stride = 0l; offset = ("", 5l)} in
    let s = plus c1 c2 in
    print_endline
      ("[SUM of congruences]      (" ^ to_string c1 ^
      ") + (" ^ to_string c2 ^
      ") = " ^ to_string s);
    equal s (Congruence {stride = 0l; offset = ("", 12l)})

  let%test "Congruence: sum absolute and relative congruence" =
    let c1 = Congruence {stride = 4l; offset = ("", 2l)} in
    let c2 = Congruence {stride = 6l; offset = ("x", 3l)} in
    let s = plus c1 c2 in
    print_endline
      ("[SUM of congruences]      (" ^ to_string c1 ^
      ") + (" ^ to_string c2 ^
      ") = " ^ to_string s);
    equal s (Congruence {stride = 2l; offset = ("x", 5l)})

  let%test "Congruence: sum two same relative congruences" =
    let c1 = Congruence {stride = 4l; offset = ("x", 2l)} in
    let c2 = Congruence {stride = 6l; offset = ("x", 3l)} in
    let s = plus c1 c2 in
    print_endline
      ("[SUM of congruences]      (" ^ to_string c1 ^
      ") + (" ^ to_string c2 ^
      ") = " ^ to_string s);
    equal s (Congruence {stride = 2l; offset = ("x+x", 5l)})

  let%test "Congruence: sum relative congruences with different variables" =
    let c1 = Congruence {stride = 4l; offset = ("x", 2l)} in
    let c2 = Congruence {stride = 6l; offset = ("y", 3l)} in
    let s = plus c1 c2 in
    print_endline
      ("[SUM of congruences]      (" ^ to_string c1 ^
      ") + (" ^ to_string c2 ^
      ") = " ^ to_string s);
    equal s (Congruence {stride = 2l; offset = ("x+y", 5l)})
end)