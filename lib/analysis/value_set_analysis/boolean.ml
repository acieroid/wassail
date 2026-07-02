open Reduced_interval_congruence

open Core

(** Boolean abstraction with conditional refinements.

    A boolean value keeps a numeric RIC, which may represent any integer value,
    together with refinements that become available when the value is used as a
    condition. For each tracked variable, the refinement map records what the
    variable can be when the condition is true and when it is false. *)
module True_or_false = struct
  (** Abstract values associated with the two outcomes of a boolean expression.

      [true_] is the value inferred for a variable when the expression is true;
      [false_] is the value inferred when the expression is false. *)
  type true_or_false = {
    true_ : RIC.t;
    false_ : RIC.t
  }
  [@@deriving sexp, compare, equal]

  (** Map from variables to their true/false refinements. *)
  type t = true_or_false Variable.Map.t
  [@@deriving sexp, compare, equal]

  let get (tf : t) ~(var : Variable.t) : true_or_false = Variable.Map.find_exn tf var

  let set (tf : t) ~(var : Variable.t) ~data : t = Variable.Map.set tf ~key:var ~data

  let true_or_false_to_string (tf : true_or_false) : string =
    "T(" ^ RIC.to_string tf.true_ ^ "), F(" ^ RIC.to_string tf.false_ ^ ")"

  let to_string (tf : t) : string =
    "[" ^
    (tf |> Variable.Map.to_alist
        |> List.map ~f:(fun (k, t) ->
          Printf.sprintf "%s : %s"
            (Variable.to_string k)
            (true_or_false_to_string t))
        |> String.concat ~sep:"; ")
    ^ "]"

  (** Weaken false-branch refinements.

      When the surrounding expression is known to be false, the original
      condition may have been either true or false. The false branch is
      therefore joined with the true branch. *)
  let weaken_false_branch (x : t) : t =
    x |> Variable.Map.map ~f:(fun tf -> {true_ = tf.true_; false_ = RIC.join tf.false_ tf.true_})

  (** Weaken true-branch refinements.

      When the surrounding expression is known to be true, the original
      condition may have been either true or false. The true branch is
      therefore joined with the false branch. *)
  let weaken_true_branch (x : t) : t =
    x |> Variable.Map.map ~f:(fun tf -> {false_ = tf.false_; true_ = RIC.join tf.false_ tf.true_})

  
end

(** Abstract boolean value.

    [numeric_value] is the RIC abstraction of the boolean as an integer value.
    [true_or_false] stores the refinements that hold for variables occurring in
    the condition when the boolean is respectively true or false. *)
type t = {
  true_or_false : True_or_false.t;
  numeric_value : RIC.t
}
[@@deriving sexp, compare, equal]

let (=) = equal
let (<>) (x : t) (y : t) : bool = not (x = y)

let top = {true_or_false = Variable.Map.empty; numeric_value = RIC.Top}

let to_string (boolean : t) : string =
  Printf.sprintf "(%s)--%s" 
    (RIC.to_string boolean.numeric_value) 
    (True_or_false.to_string boolean.true_or_false)

(** Return the true/false refinements associated with [var].

    Raises if [var] has no entry in the refinement map. *)
let get (boolean : t) ~(var : Variable.t) : True_or_false.true_or_false =
  True_or_false.get boolean.true_or_false ~var

(** Return the refinement for [var] when the boolean is true. *)
let get_true (boolean : t) ~(var : Variable.t) : RIC.t =
  (get boolean ~var).true_

(** Return the refinement for [var] when the boolean is false. *)
let get_false (boolean : t) ~(var : Variable.t) : RIC.t =
  (get boolean ~var).false_


(** Whether the boolean numeric value denotes a single integer. *)
let is_singleton (b : t) : bool =
  RIC.is_singleton b.numeric_value

(** Abstract conjunction.

    When the result is true, both operands are true, so true-branch refinements
    are intersected. When the result is false, either operand may be false, so
    refinements are joined conservatively. *)
let and_ (boolean1 : t) (boolean2 : t) : t =
  { true_or_false =
      Variable.Map.merge boolean1.true_or_false boolean2.true_or_false ~f:(fun ~key:_ v ->
        match v with
        | `Both (x, y) -> 
          Some {True_or_false.true_ = RIC.meet x.true_ y.true_; 
                             false_ = RIC.(join (join y.false_ y.true_) (join x.false_ x.true_))} (* when false, we don't know which branch was false *)
        | `Right {true_ = t; false_ = f} 
        | `Left  {true_ = t; false_ = f} -> 
          Some {true_ = t; 
               false_ = RIC.join t f}); (* This boolean being false doesn't imply that the individual branch was false, hense the join *)
    numeric_value = RIC.(boolean1.numeric_value &. boolean2.numeric_value)}
let (&.) = and_

(** Abstract exclusive-or.

    True refinements come from the cases where exactly one operand is true;
    false refinements come from the cases where both operands have the same
    truth value. Variables present in only one operand are conservatively joined. *)
let xor_ (boolean1 : t) (boolean2 : t) : t =
  { true_or_false =
      Variable.Map.merge boolean1.true_or_false boolean2.true_or_false ~f:(fun ~key:_ v ->
        match v with
        | `Both (x, y) -> 
          Some {True_or_false.true_ = RIC.(join (meet x.false_ y.true_) (meet x.true_ y.false_)); 
                             false_ = RIC.(join (meet x.true_ y.true_) (meet x.false_ y.false_))}
        | `Right {true_ = t; false_ = f} 
        | `Left  {true_ = t; false_ = f} -> 
          let vs = RIC.join t f in (* we must join true and false together, because we don't know which branch is true *)
          Some {true_ = vs; false_ = vs});
    numeric_value = RIC.(boolean1.numeric_value <+> boolean2.numeric_value)}
let (<+>) = xor_

(** Abstract disjunction.

    When the result is false, both operands are false, so false-branch
    refinements are intersected. When the result is true, either operand may be
    true, so refinements are joined conservatively. *)
let or_ (boolean1 : t) (boolean2 : t) : t =
  { true_or_false =
      Variable.Map.merge boolean1.true_or_false boolean2.true_or_false ~f:(fun ~key:_ v ->
        match v with
        | `Both (x, y) -> 
          Some {True_or_false.true_ = RIC.join (RIC.join y.false_ y.true_) (RIC.join x.false_ x.true_); 
                             false_ = RIC.meet x.false_ y.false_}
        | `Right {true_ = t; false_ = f} 
        | `Left  {true_ = t; false_ = f} -> 
          Some {true_ = RIC.join t f; (* This boolean being true doesn't imply that the individual branch was true, hense the join *)
               false_ = f});
    numeric_value = RIC.or_ boolean1.numeric_value boolean2.numeric_value}
let (|.) = or_

(** Abstract [eqz].

    Swaps true and false refinements and computes the numeric value of testing
    whether the input may be zero. *)
let eqz (boolean : t) : t =
    { true_or_false =
      Variable.Map.fold 
        ~init:Variable.Map.empty 
        ~f:(fun ~key ~data:{true_ = t; false_ = f} acc -> Variable.Map.set acc ~key ~data:{True_or_false.true_ = f; false_ = t}) 
        boolean.true_or_false;
    numeric_value = 
      RIC.join 
        (if RIC.may_be_false boolean.numeric_value then RIC.one else RIC.Bottom) 
        (if RIC.may_be_true boolean.numeric_value then RIC.zero else RIC.Bottom)
    }

(** Least upper bound of two abstract booleans.

    Both operands are first made compatible so that variables missing from one
    side are interpreted using that side's current knowledge. Shared
    refinements are joined pointwise. *)
let join (boolean1 : t) (boolean2 : t) : t = 
  let boolean1 = { boolean1 with 
                   true_or_false = Variable.Map.make_compatible ~this:boolean1.true_or_false ~relative_to:boolean2.true_or_false ~get:True_or_false.get;} in
  let boolean2 = { boolean2 with
                   true_or_false = Variable.Map.make_compatible ~this:boolean2.true_or_false ~relative_to:boolean1.true_or_false ~get:True_or_false.get;} in
  { true_or_false =
      Variable.Map.merge boolean1.true_or_false boolean2.true_or_false ~f:(fun ~key:_ v ->
        match v with
        | `Both (x, y) -> Some {True_or_false.true_ = RIC.join x.true_ y.true_; 
                                             false_ = RIC.join x.false_ y.false_}
        | `Right _ | `Left _ -> None); (* Nothing can be inferred from this condition *)
    numeric_value = RIC.join boolean1.numeric_value boolean2.numeric_value }

(** Intersection of two abstract booleans.

    Both operands are first made compatible, then shared refinements and numeric
    values are intersected pointwise. *)
let meet (boolean1 : t) (boolean2 : t) : t = Log.warn (fun () -> "I'm not sure about the meet of two boolean conditions"); 
  let boolean1 = { boolean1 with 
                   true_or_false = Variable.Map.make_compatible ~this:boolean1.true_or_false ~relative_to:boolean2.true_or_false ~get:True_or_false.get;} in
  let boolean2 = { boolean2 with
                   true_or_false = Variable.Map.make_compatible ~this:boolean2.true_or_false ~relative_to:boolean1.true_or_false ~get:True_or_false.get;} in
  { true_or_false =
      Variable.Map.merge boolean1.true_or_false boolean2.true_or_false ~f:(fun ~key:_ v ->
        match v with
        | `Both (x, y) -> Some {True_or_false.true_ = RIC.meet x.true_ y.true_; 
                                             false_ = RIC.meet x.false_ y.false_}
        | `Right _ | `Left _ -> None); (* Nothing can be inferred from this condition *)
    numeric_value = RIC.meet boolean1.numeric_value boolean2.numeric_value }


(** Whether the refinement map allows the boolean to be true. *)
let may_be_true (boolean : t) : bool =
  boolean.true_or_false 
  |> Variable.Map.exists ~f:(fun {true_; _} -> RIC.(true_ = Bottom))
  |> not

(** Whether the refinement map allows the boolean to be false. *)
let may_be_false (boolean : t) : bool =
  boolean.true_or_false
  |> Variable.Map.exists ~f:(fun {false_; _} -> RIC.(false_ = Bottom))
  |> not


(** Build a boolean abstraction from a numeric RIC value.

    The false branch keeps only zero when [ric] may be false. The true branch
    keeps the non-zero part of [ric] when [ric] may be true. *)
let of_RIC ~(var : Variable.t) (ric : RIC.t) : t =
  let false_ = 
    if RIC.may_be_false ric then 
      RIC.zero 
    else 
      RIC.Bottom in
  let true_ = 
    if RIC.may_be_true ric then 
      if ric |> RIC.extract_relative_offset |> String.is_empty then
        RIC.remove ~this:RIC.zero ~from:ric |> List.fold ~init:RIC.Bottom ~f:(fun acc vs -> RIC.join acc vs) 
      else
        ric
    else 
      RIC.Bottom in
  { true_or_false = Variable.Map.empty |> True_or_false.set ~var ~data:{ true_; false_ }; 
    numeric_value = ric }

(** Build a boolean abstraction from a bitfield by first converting it to a RIC. *)
let of_bitfield ~(var : Variable.t) (bf : Bitfield.t)  : t =
  of_RIC ~var (RIC.of_bitfield bf)


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

let%test_module "Boolean tests" = (module struct

let x = Variable.Var (Var.Other "x")
let y = Variable.Var (Var.Other "y")

  let print_boolean_test_result ~(op : string) ~(test_name : string) ~(expected : t) ~(actual : t) : unit =
    Printf.printf
      "[%s]\n%s\texpected: %s\n\tactual:   %s\n\n"
      test_name
      op
      (to_string expected)
      (to_string actual)

  let expect_equal_boolean ?(op : string = "") ~(test_name : string) ~(expected : t) ~(actual : t) (_ : unit) : bool =
    print_boolean_test_result ~op ~test_name ~expected ~actual;
    expected = actual

  let%test "Tests on Boolean module" =
    print_endline "_______ ______________ _______\n        Boolean module        \n------- -------------- -------\n";
    true

  let%test "of_RIC: definitely false" =
    let expected =
      { true_or_false =
          Variable.Map.empty
          |> True_or_false.set
               ~var:x
               ~data:{ true_ = RIC.Bottom; false_ = RIC.zero };
        numeric_value = RIC.zero }
    in
    let actual = of_RIC RIC.zero ~var:x in
    expect_equal_boolean ~test_name:"of_RIC: definitely false" ~expected ~actual ()

  let%test "of_RIC: definitely true" =
    let expected =
      { true_or_false =
          Variable.Map.empty
          |> True_or_false.set
               ~var:x
               ~data:{ true_ = RIC.one; false_ = RIC.Bottom };
        numeric_value = RIC.one }
    in
    let actual = of_RIC ~var:x RIC.one in
    expect_equal_boolean ~test_name:"of_RIC: definitely true" ~expected ~actual ()

  let%test "of_RIC: true or false" =
    let ric = RIC.join RIC.zero RIC.one in
    let expected =
      { true_or_false =
          Variable.Map.empty
          |> True_or_false.set
               ~var:x
               ~data:{ true_ = RIC.one; false_ = RIC.zero };
        numeric_value = ric }
    in
    let actual = of_RIC ~var:x ric in
    expect_equal_boolean ~test_name:"of_RIC: true or false" ~expected ~actual ()

  let%test "of_RIC: zero or non-boolean true value" =
    let four = RIC.of_int32 4l in
    let ric = RIC.join RIC.zero four in
    let expected =
      { true_or_false =
          Variable.Map.empty
          |> True_or_false.set
               ~var:x
               ~data:{ true_ = four; false_ = RIC.zero };
        numeric_value = ric }
    in
    let actual = of_RIC ~var:x ric in
    expect_equal_boolean
      ~test_name:"of_RIC: zero or non-boolean true value"
      ~expected
      ~actual
      ()

  let%test "of_RIC: non-zero interval" =
    let ric = RIC.ric (1l, Int 2l, Int 5l, ("",0l)) in
    let expected =
      { true_or_false =
          Variable.Map.empty
          |> True_or_false.set
               ~var:x
               ~data:{ true_ = ric; false_ = RIC.Bottom };
        numeric_value = ric }
    in
    let actual = of_RIC ~var:x ric in
    expect_equal_boolean ~test_name:"of_RIC: non-zero interval" ~expected ~actual ()

  let%test "of_RIC: interval containing zero" =
    let ric = RIC.ric (1l, Int 0l, Int 5l, ("", 0l)) in
    let true_ = RIC.ric (1l, Int 1l, Int 5l, ("", 0l)) in
    let expected =
      { true_or_false =
          Variable.Map.empty
          |> True_or_false.set
               ~var:x
               ~data:{ true_; false_ = RIC.zero };
        numeric_value = ric }
    in
    let actual = of_RIC ~var:x ric in
    expect_equal_boolean ~test_name:"of_RIC: interval containing zero" ~expected ~actual ()


  let%test "of_RIC: negative interval containing zero" =
    let ric = RIC.ric (1l, Int (-1l), Int 1l, ("", 0l)) in
    let true_ = RIC.ric (2l, Int 0l, Int 1l, ("", -1l)) in
    let expected =
      { true_or_false =
          Variable.Map.empty
          |> True_or_false.set
               ~var:x
               ~data:{ true_; false_ = RIC.zero };
        numeric_value = ric }
    in
    let actual = of_RIC ~var:x ric in
    expect_equal_boolean
      ~test_name:"of_RIC: negative interval containing zero"
      ~expected
      ~actual
      ()


  let%test "of_bitfield: definitely true" =
    let bf = Bitfield.of_string "01" in
    let expected = of_RIC ~var:x RIC.one in
    let actual = of_bitfield ~var:x bf in
    expect_equal_boolean ~test_name:"of_bitfield: definitely true" ~expected ~actual ()

  let%test "eqz: definitely false becomes definitely true" =
    let boolean = of_RIC ~var:x RIC.zero in
    let expected =
      { true_or_false =
          Variable.Map.empty
          |> True_or_false.set
               ~var:x
               ~data:{ true_ = RIC.zero; false_ = RIC.Bottom };
        numeric_value = RIC.one }
    in
    let actual = eqz boolean in
    expect_equal_boolean
      ~test_name:"eqz: definitely false becomes definitely true"
      ~expected
      ~actual
      ()

  let%test "eqz: definitely true becomes definitely false" =
    let boolean = of_RIC RIC.(one + one) ~var:x in
    let expected =
      { true_or_false =
          Variable.Map.empty
          |> True_or_false.set
               ~var:x
               ~data:{ true_ = RIC.Bottom; false_ = RIC.(one + one) };
        numeric_value = RIC.zero }
    in
    let actual = eqz boolean in
    expect_equal_boolean
      ~test_name:"eqz: definitely true becomes definitely false"
      ~expected
      ~actual
      ()

  let%test "eqz: true or false remains true or false" =
    let ric = RIC.join RIC.zero RIC.(one + one + one) in
    let boolean = of_RIC ric ~var:x in
    let expected =
      { true_or_false =
          Variable.Map.empty
          |> True_or_false.set
               ~var:x
               ~data:{ true_ = RIC.zero; false_ = RIC.(one + one + one) };
        numeric_value = RIC.(ric (1l, Int 0l, Int 1l, ("", 0l))) }
    in
    let actual = eqz boolean in
    expect_equal_boolean
      ~test_name:"eqz: true or false remains true or false"
      ~expected
      ~actual
      ()

  let%test "and_: definitely true and definitely true" =
    let boolean1 = of_RIC RIC.one ~var:x in
    let boolean2 = of_RIC RIC.(one + one) ~var:y in
    let expected =
      { true_or_false =
          Variable.Map.empty
          |> True_or_false.set
               ~var:x
               ~data:{ true_ = RIC.one; false_ = RIC.one }
          |> True_or_false.set
               ~var:y
               ~data:{ true_ = RIC.(one + one); false_ = RIC.(one + one) };
        numeric_value = RIC.zero }
    in
    let actual = and_ boolean1 boolean2 in
    expect_equal_boolean
      ~op:("\t" ^to_string boolean1 ^ "    and    " ^ to_string boolean2 ^ "\n")
      ~test_name:"and_: definitely true and definitely true"
      ~expected
      ~actual
      ()

  let%test "and_: definitely false and definitely true" =
    let boolean1 = of_RIC RIC.zero ~var:x in
    let boolean2 = of_RIC RIC.one ~var:y in
    let expected =
      { true_or_false =
          Variable.Map.empty
          |> True_or_false.set
              ~var:x
              ~data:{ true_ = RIC.Bottom; false_ = RIC.zero }
          |> True_or_false.set
              ~var:y
              ~data:{ true_ = RIC.one; false_ = RIC.one };
        numeric_value = RIC.zero }
    in
    let actual = and_ boolean1 boolean2 in
    expect_equal_boolean
      ~op:("\t" ^ to_string boolean1 ^ "    and    " ^ to_string boolean2 ^ "\n")
      ~test_name:"and_: definitely false and definitely true"
      ~expected
      ~actual
      ()

  let%test "and_: true or false and definitely true" =
    let boolean1 = of_RIC (RIC.join RIC.zero RIC.one) ~var:x in
    let boolean2 = of_RIC RIC.one ~var:y in
    let expected =
      { true_or_false =
          Variable.Map.empty
          |> True_or_false.set
              ~var:x
              ~data:{ true_ = RIC.one; false_ = RIC.(join zero one) }
          |> True_or_false.set
              ~var:y
              ~data:{ true_ = RIC.one; false_ = RIC.one };
        numeric_value = RIC.(ric (1l, Int 0l, Int 1l, ("", 0l))) }
    in
    let actual = and_ boolean1 boolean2 in
    expect_equal_boolean
      ~op:("\t" ^ to_string boolean1 ^ "    and    " ^ to_string boolean2 ^ "\n")
      ~test_name:"and_: true or false and definitely true"
      ~expected
      ~actual
      ()

  let%test "and_: true or false and true or false" =
    let boolean1 = of_RIC (RIC.join RIC.zero RIC.one) ~var:x in
    let boolean2 = of_RIC (RIC.join RIC.zero RIC.one) ~var:y in
    let expected =
      { true_or_false =
          Variable.Map.empty
          |> True_or_false.set
              ~var:x
              ~data:{ true_ = RIC.one; false_ = RIC.(join zero one) }
          |> True_or_false.set
              ~var:y
              ~data:{ true_ = RIC.one; false_ = RIC.(join zero one) };
        numeric_value = RIC.(ric (1l, Int 0l, Int 1l, ("", 0l))) }
    in
    let actual = and_ boolean1 boolean2 in
    expect_equal_boolean
      ~op:("\t" ^ to_string boolean1 ^ "    and    " ^ to_string boolean2 ^ "\n")
      ~test_name:"and_: true or false and true or false"
      ~expected
      ~actual
      ()

  let%test "and_: definitely true and definitely false" =
    let boolean1 = of_RIC RIC.one ~var:x in
    let boolean2 = of_RIC RIC.zero ~var:y in
    let expected =
      { true_or_false =
          Variable.Map.empty
          |> True_or_false.set
              ~var:x
              ~data:{ true_ = RIC.one; false_ = RIC.one }
          |> True_or_false.set
              ~var:y
              ~data:{ true_ = RIC.Bottom; false_ = RIC.zero };
        numeric_value = RIC.zero }
    in
    let actual = and_ boolean1 boolean2 in
    expect_equal_boolean
      ~op:("\t" ^ to_string boolean1 ^ "    and    " ^ to_string boolean2 ^ "\n")
      ~test_name:"and_: definitely true and definitely false"
      ~expected
      ~actual
      ()

  let%test "and_: definitely true and true or false" =
    let boolean1 = of_RIC RIC.one ~var:x in
    let boolean2 = of_RIC (RIC.join RIC.zero RIC.one) ~var:y in
    let expected =
      { true_or_false =
          Variable.Map.empty
          |> True_or_false.set
              ~var:x
              ~data:{ true_ = RIC.one; false_ = RIC.one }
          |> True_or_false.set
              ~var:y
              ~data:{ true_ = RIC.one; false_ = RIC.(join zero one) };
        numeric_value = RIC.(ric (1l, Int 0l, Int 1l, ("", 0l))) }
    in
    let actual = and_ boolean1 boolean2 in
    expect_equal_boolean
      ~op:("\t" ^ to_string boolean1 ^ "    and    " ^ to_string boolean2 ^ "\n")
      ~test_name:"and_: definitely true and true or false"
      ~expected
      ~actual
      ()

  let%test "and_: definitely false and definitely false" =
    let boolean1 = of_RIC RIC.zero ~var:x in
    let boolean2 = of_RIC RIC.zero ~var:y in
    let expected =
      { true_or_false =
          Variable.Map.empty
          |> True_or_false.set
              ~var:x
              ~data:{ true_ = RIC.Bottom; false_ = RIC.zero }
          |> True_or_false.set
              ~var:y
              ~data:{ true_ = RIC.Bottom; false_ = RIC.zero };
        numeric_value = RIC.zero }
    in
    let actual = and_ boolean1 boolean2 in
    expect_equal_boolean
      ~op:("\t" ^ to_string boolean1 ^ "    and    " ^ to_string boolean2 ^ "\n")
      ~test_name:"and_: definitely false and definitely false"
      ~expected
      ~actual
      ()

  let%test "and_: definitely false and true or false" =
    let boolean1 = of_RIC RIC.zero ~var:x in
    let boolean2 = of_RIC (RIC.join RIC.zero RIC.one) ~var:y in
    let expected =
      { true_or_false =
          Variable.Map.empty
          |> True_or_false.set
              ~var:x
              ~data:{ true_ = RIC.Bottom; false_ = RIC.zero }
          |> True_or_false.set
              ~var:y
              ~data:{ true_ = RIC.one; false_ = RIC.(join zero one) };
        numeric_value = RIC.zero }
    in
    let actual = and_ boolean1 boolean2 in
    expect_equal_boolean
      ~op:("\t" ^ to_string boolean1 ^ "    and    " ^ to_string boolean2 ^ "\n")
      ~test_name:"and_: definitely false and true or false"
      ~expected
      ~actual
      ()

  let%test "and_: true or false and definitely false" =
    let boolean1 = of_RIC (RIC.join RIC.zero RIC.one) ~var:x in
    let boolean2 = of_RIC RIC.zero ~var:y in
    let expected =
      { true_or_false =
          Variable.Map.empty
          |> True_or_false.set
              ~var:x
              ~data:{ true_ = RIC.one; false_ = RIC.(join zero one) }
          |> True_or_false.set
              ~var:y
              ~data:{ true_ = RIC.Bottom; false_ = RIC.zero };
        numeric_value = RIC.zero }
    in
    let actual = and_ boolean1 boolean2 in
    expect_equal_boolean
      ~op:("\t" ^ to_string boolean1 ^ "    and    " ^ to_string boolean2 ^ "\n")
      ~test_name:"and_: true or false and definitely false"
      ~expected
      ~actual
      ()

  let%test "xor_: definitely true xor definitely true" =
    let boolean1 = of_RIC (RIC.constant 3l) ~var:x in
    let boolean2 = of_RIC (RIC.constant 2l) ~var:y in
    let expected =
      { true_or_false =
          Variable.Map.empty
          |> True_or_false.set
              ~var:x
              ~data:{ true_ = RIC.constant 3l; false_ = RIC.constant 3l }
          |> True_or_false.set
              ~var:y
              ~data:{ true_ = RIC.constant 2l; false_ = RIC.constant 2l };
        numeric_value = RIC.one }
    in
    let actual = xor_ boolean1 boolean2 in
    expect_equal_boolean
      ~op:("\t" ^ to_string boolean1 ^ "    xor    " ^ to_string boolean2 ^ "\n")
      ~test_name:"xor_: definitely true xor definitely true"
      ~expected
      ~actual
      ()

  let%test "xor_: definitely true xor definitely false" =
    let boolean1 = of_RIC (RIC.constant 5l) ~var:x in
    let boolean2 = of_RIC RIC.zero ~var:y in
    let expected =
      { true_or_false =
          Variable.Map.empty
          |> True_or_false.set
              ~var:x
              ~data:{ true_ = RIC.constant 5l; false_ = RIC.constant 5l }
          |> True_or_false.set
              ~var:y
              ~data:{ true_ = RIC.zero; false_ = RIC.zero };
        numeric_value = RIC.constant 5l }
    in
    let actual = xor_ boolean1 boolean2 in
    expect_equal_boolean
      ~op:("\t" ^ to_string boolean1 ^ "    xor    " ^ to_string boolean2 ^ "\n")
      ~test_name:"xor_: definitely true xor definitely false"
      ~expected
      ~actual
      ()

  let%test "xor_: definitely false xor definitely false" =
    let boolean1 = of_RIC RIC.zero ~var:x in
    let boolean2 = of_RIC RIC.zero ~var:y in
    let expected =
      { true_or_false =
          Variable.Map.empty
          |> True_or_false.set
              ~var:x
              ~data:{ true_ = RIC.zero; false_ = RIC.zero }
          |> True_or_false.set
              ~var:y
              ~data:{ true_ = RIC.zero; false_ = RIC.zero };
        numeric_value = RIC.zero }
    in
    let actual = xor_ boolean1 boolean2 in
    expect_equal_boolean
      ~op:("\t" ^ to_string boolean1 ^ "    xor    " ^ to_string boolean2 ^ "\n")
      ~test_name:"xor_: definitely false xor definitely false"
      ~expected
      ~actual
      ()

  let%test "xor_: true or false xor definitely true" =
    let ric1 = RIC.join RIC.zero (RIC.constant 5l) in
    let boolean1 = of_RIC ric1 ~var:x in
    let boolean2 = of_RIC (RIC.constant 2l) ~var:y in
    let expected =
      { true_or_false =
          Variable.Map.empty
          |> True_or_false.set
              ~var:x
              ~data:{ true_ = ric1; false_ = ric1 }
          |> True_or_false.set
              ~var:y
              ~data:{ true_ = RIC.constant 2l; false_ = RIC.constant 2l };
        numeric_value = RIC.(ric (1l, Int 0l, Int 5l, ("", 2l))) }
    in
    let actual = xor_ boolean1 boolean2 in
    expect_equal_boolean
      ~op:("\t" ^ to_string boolean1 ^ "    xor    " ^ to_string boolean2 ^ "\n")
      ~test_name:"xor_: true or false xor definitely true"
      ~expected
      ~actual
      ()

  let%test "xor_: true or false xor definitely false" =
    let ric1 = RIC.join RIC.zero (RIC.constant 6l) in
    let boolean1 = of_RIC ric1 ~var:x in
    let boolean2 = of_RIC RIC.zero ~var:y in
    let expected =
      { true_or_false =
          Variable.Map.empty
          |> True_or_false.set
              ~var:x
              ~data:{ true_ = ric1; false_ = ric1 }
          |> True_or_false.set
              ~var:y
              ~data:{ true_ = RIC.zero; false_ = RIC.zero };
        numeric_value = RIC.(ric (2l, Int 0l, Int 3l, ("", 0l))) }
    in
    let actual = xor_ boolean1 boolean2 in
    expect_equal_boolean
      ~op:("\t" ^ to_string boolean1 ^ "    xor    " ^ to_string boolean2 ^ "\n")
      ~test_name:"xor_: true or false xor definitely false"
      ~expected
      ~actual
      ()

  let%test "xor_: true or false xor true or false" =
    let ric1 = RIC.join RIC.zero (RIC.constant 5l) in
    let ric2 = RIC.join RIC.zero (RIC.constant 3l) in
    let boolean1 = of_RIC ric1 ~var:x in
    let boolean2 = of_RIC ric2 ~var:y in
    let expected =
      { true_or_false =
          Variable.Map.empty
          |> True_or_false.set
              ~var:x
              ~data:{ true_ = ric1; false_ = ric1 }
          |> True_or_false.set
              ~var:y
              ~data:{ true_ = ric2; false_ = ric2 };
        numeric_value =
          RIC.(ric (1l, Int 0l, Int 7l, ("", 0l))) }
    in
    let actual = xor_ boolean1 boolean2 in
    expect_equal_boolean
      ~op:("\t" ^ to_string boolean1 ^ "    xor    " ^ to_string boolean2 ^ "\n")
      ~test_name:"xor_: true or false xor true or false"
      ~expected
      ~actual
      ()

    let%test "or_: definitely true or definitely true" =
    let boolean1 = of_RIC (RIC.constant 2l) ~var:x in
    let boolean2 = of_RIC (RIC.constant 5l) ~var:y in
    let expected =
      { true_or_false =
          Variable.Map.empty
          |> True_or_false.set ~var:x ~data:{ true_ = RIC.constant 2l; false_ = RIC.Bottom }
          |> True_or_false.set ~var:y ~data:{ true_ = RIC.constant 5l; false_ = RIC.Bottom };
        numeric_value = RIC.constant 7l }
    in
    let actual = or_ boolean1 boolean2 in
    expect_equal_boolean ~op:("\t" ^ to_string boolean1 ^ "    or    " ^ to_string boolean2 ^ "\n")
      ~test_name:"or_: definitely true or definitely true" ~expected ~actual ()

  let%test "or_: definitely true or definitely false" =
    let boolean1 = of_RIC (RIC.constant 6l) ~var:x in
    let boolean2 = of_RIC RIC.zero ~var:y in
    let expected =
      { true_or_false =
          Variable.Map.empty
          |> True_or_false.set ~var:x ~data:{ true_ = RIC.constant 6l; false_ = RIC.Bottom }
          |> True_or_false.set ~var:y ~data:{ true_ = RIC.zero; false_ = RIC.zero };
        numeric_value = RIC.constant 6l }
    in
    let actual = or_ boolean1 boolean2 in
    expect_equal_boolean ~op:("\t" ^ to_string boolean1 ^ "    or    " ^ to_string boolean2 ^ "\n")
      ~test_name:"or_: definitely true or definitely false" ~expected ~actual ()

  let%test "or_: definitely false or definitely false" =
    let boolean1 = of_RIC RIC.zero ~var:x in
    let boolean2 = of_RIC RIC.zero ~var:y in
    let expected =
      { true_or_false =
          Variable.Map.empty
          |> True_or_false.set ~var:x ~data:{ true_ = RIC.zero; false_ = RIC.zero }
          |> True_or_false.set ~var:y ~data:{ true_ = RIC.zero; false_ = RIC.zero };
        numeric_value = RIC.zero }
    in
    let actual = or_ boolean1 boolean2 in
    expect_equal_boolean ~op:("\t" ^ to_string boolean1 ^ "    or    " ^ to_string boolean2 ^ "\n")
      ~test_name:"or_: definitely false or definitely false" ~expected ~actual ()

  let%test "or_: true or false or definitely true" =
    let ric1 = RIC.join RIC.zero (RIC.constant 5l) in
    let boolean1 = of_RIC ric1 ~var:x in
    let boolean2 = of_RIC (RIC.constant 3l) ~var:y in
    let expected =
      { true_or_false =
          Variable.Map.empty
          |> True_or_false.set ~var:x ~data:{ true_ = ric1; false_ = RIC.zero }
          |> True_or_false.set ~var:y ~data:{ true_ = RIC.constant 3l; false_ = RIC.Bottom };
        numeric_value = RIC.join (RIC.constant 3l) (RIC.constant 7l) }
    in
    let actual = or_ boolean1 boolean2 in
    expect_equal_boolean ~op:("\t" ^ to_string boolean1 ^ "    or    " ^ to_string boolean2 ^ "\n")
      ~test_name:"or_: true or false or definitely true" ~expected ~actual ()

  let%test "or_: true or false or definitely false" =
    let ric1 = RIC.join RIC.zero (RIC.constant 6l) in
    let boolean1 = of_RIC ric1 ~var:x in
    let boolean2 = of_RIC RIC.zero ~var:y in
    let expected =
      { true_or_false =
          Variable.Map.empty
          |> True_or_false.set ~var:x ~data:{ true_ = ric1; false_ = RIC.zero }
          |> True_or_false.set ~var:y ~data:{ true_ = RIC.zero; false_ = RIC.zero };
        numeric_value = RIC.(ric (2l, Int 0l, Int 3l, ("", 0l))) }
    in
    let actual = or_ boolean1 boolean2 in
    expect_equal_boolean ~op:("\t" ^ to_string boolean1 ^ "    or    " ^ to_string boolean2 ^ "\n")
      ~test_name:"or_: true or false or definitely false" ~expected ~actual ()

  let%test "or_: true or false or true or false" =
    let ric1 = RIC.join RIC.zero (RIC.constant 5l) in
    let ric2 = RIC.join RIC.zero (RIC.constant 3l) in
    let boolean1 = of_RIC ric1 ~var:x in
    let boolean2 = of_RIC ric2 ~var:y in
    let expected =
      { true_or_false =
          Variable.Map.empty
          |> True_or_false.set ~var:x ~data:{ true_ = ric1; false_ = RIC.zero }
          |> True_or_false.set ~var:y ~data:{ true_ = ric2; false_ = RIC.zero };
        numeric_value =
          RIC.(join zero (join (constant 3l) (join (constant 5l) (constant 7l)))) }
    in
    let actual = or_ boolean1 boolean2 in
    expect_equal_boolean ~op:("\t" ^ to_string boolean1 ^ "    or    " ^ to_string boolean2 ^ "\n")
      ~test_name:"or_: true or false or true or false" ~expected ~actual ()


    let%test "and_: same variable in both operands" =
    let ric1 = RIC.join RIC.zero (RIC.constant 5l) in
    let ric2 = RIC.join RIC.zero (RIC.constant 3l) in
    let boolean1 = of_RIC ric1 ~var:x in
    let boolean2 = of_RIC ric2 ~var:x in
    let expected =
      { true_or_false =
          Variable.Map.empty
          |> True_or_false.set
              ~var:x
              ~data:{ true_ = RIC.Bottom; false_ = RIC.(join ric1 ric2) };
        numeric_value = RIC.join RIC.zero RIC.one }
    in
    let actual = and_ boolean1 boolean2 in
    expect_equal_boolean
      ~op:("\t" ^ to_string boolean1 ^ "    and    " ^ to_string boolean2 ^ "\n")
      ~test_name:"and_: same variable in both operands"
      ~expected
      ~actual
      ()

  let%test "or_: same variable in both operands" =
    let ric1 = RIC.join RIC.zero (RIC.constant 5l) in
    let ric2 = RIC.join RIC.zero (RIC.constant 3l) in
    let boolean1 = of_RIC ric1 ~var:x in
    let boolean2 = of_RIC ric2 ~var:x in
    let expected =
      { true_or_false =
          Variable.Map.empty
          |> True_or_false.set
              ~var:x
              ~data:{ true_ = RIC.(join ric1 ric2); false_ = RIC.zero };
        numeric_value =
          RIC.(join zero (join (constant 3l) (join (constant 5l) (constant 7l)))) }
    in
    let actual = or_ boolean1 boolean2 in
    expect_equal_boolean
      ~op:("\t" ^ to_string boolean1 ^ "    or    " ^ to_string boolean2 ^ "\n")
      ~test_name:"or_: same variable in both operands"
      ~expected
      ~actual
      ()

  let%test "xor_: same variable in both operands" =
    let ric1 = RIC.join RIC.zero (RIC.constant 5l) in
    let ric2 = RIC.join RIC.zero (RIC.constant 3l) in
    let boolean1 = of_RIC ric1 ~var:x in
    let boolean2 = of_RIC ric2 ~var:x in
    let expected =
      { true_or_false =
          Variable.Map.empty
          |> True_or_false.set
              ~var:x
              ~data:{ true_ = RIC.Bottom; false_ = RIC.zero };
        numeric_value =
          RIC.(ric (1l, Int 0l, Int 7l, ("", 0l))) }
    in
    let actual = xor_ boolean1 boolean2 in
    expect_equal_boolean
      ~op:("\t" ^ to_string boolean1 ^ "    xor    " ^ to_string boolean2 ^ "\n")
      ~test_name:"xor_: same variable in both operands"
      ~expected
      ~actual
      ()

  let%test "join: compatible boolean refinements" =
    let boolean1 = of_RIC (RIC.join RIC.zero (RIC.constant 5l)) ~var:x in
    let boolean2 = of_RIC (RIC.join RIC.zero (RIC.constant 3l)) ~var:x in
    let expected =
      { true_or_false =
          Variable.Map.empty
          |> True_or_false.set
              ~var:x
              ~data:{ true_ = RIC.join (RIC.constant 5l) (RIC.constant 3l);
                      false_ = RIC.zero };
        numeric_value =
          RIC.join boolean1.numeric_value boolean2.numeric_value }
    in
    let actual = join boolean1 boolean2 in
    expect_equal_boolean 
    ~op:("\t" ^ to_string boolean1 ^ "    join    " ^ to_string boolean2 ^ "\n")
    ~test_name:"join: compatible boolean refinements" ~expected ~actual ()

  let%test "meet: compatible boolean refinements" =
    let boolean1 = of_RIC (RIC.join RIC.zero (RIC.constant 5l)) ~var:x in
    let boolean2 = of_RIC (RIC.join RIC.zero (RIC.constant 3l)) ~var:x in
    let expected =
      { true_or_false =
          Variable.Map.empty
          |> True_or_false.set
              ~var:x
              ~data:{ true_ = RIC.Bottom; false_ = RIC.zero };
        numeric_value = RIC.meet boolean1.numeric_value boolean2.numeric_value }
    in
    let actual = meet boolean1 boolean2 in
    expect_equal_boolean 
    ~op:("\t" ^ to_string boolean1 ^ "    meet    " ^ to_string boolean2 ^ "\n")
    ~test_name:"meet: compatible boolean refinements" ~expected ~actual ()

  let%test "may_be_true: definitely false" =
    let boolean = of_RIC RIC.zero ~var:x in
    Bool.equal (may_be_true boolean) false

  let%test "may_be_true: possibly true" =
    let boolean = of_RIC (RIC.join RIC.zero (RIC.constant 5l)) ~var:x in
    Bool.equal (may_be_true boolean) true

  let%test "may_be_false: definitely true" =
    let boolean = of_RIC (RIC.constant 5l) ~var:x in
    Bool.equal (may_be_false boolean) false

  let%test "may_be_false: possibly false" =
    let boolean = of_RIC (RIC.join RIC.zero (RIC.constant 5l)) ~var:x in
    Bool.equal (may_be_false boolean) true

  let%test "is_singleton: singleton" =
    let boolean = of_RIC (RIC.constant 5l) ~var:x in
    Bool.equal (is_singleton boolean) true

  let%test "is_singleton: non-singleton" =
    let boolean = of_RIC (RIC.join RIC.zero (RIC.constant 5l)) ~var:x in
    Bool.equal (is_singleton boolean) false

  let%test "of_RIC: relative RIC" =
    let ric = RIC.relative_ric "l0" in
    let expected =
      { true_or_false =
          Variable.Map.empty
          |> True_or_false.set
               ~var:x
               ~data:{ true_=RIC.relative_ric "l0"; false_ = RIC.zero };
        numeric_value = ric }
    in
    let actual = of_RIC ~var:x ric in
    expect_equal_boolean
      ~test_name:"of_RIC: relative RIC"
      ~expected
      ~actual
      ()

end)
