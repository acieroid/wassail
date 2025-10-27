open Core

module Bitfield = struct

  type bitField = { (* TODO: get a better name for this *)
    zeros : int32;
    ones : int32
  }

  (* let is_bottom_ (b : bitField) : bool =
    Int32.(b.zeros = 0l && b.ones = 0l)
  
  let is_singleton_ (b : bitField) : bool =
    Int32.(b.ones land b.zeros = 0l) *)

  type t =
    | Top
    | Bottom
    | Bit of bitField

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
            aux ("#" ^ acc) (Int32.shift_right_logical z 1) (Int32.shift_right_logical o 1)
    in
    aux "" z o

    let reduce (bf : t) : t =
      match bf with
      | Bit {zeros = 0l; ones = 0l} -> Bottom
      | Bit {zeros = x; ones = y} when Int32.(x = y) -> Top
      | bf -> bf

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

    let of_integer (n : int32) : t =
      Bit {zeros = Int32.(lnot n); ones = n}

    let is_singleton (bf : t) : bool =
      match bf with
      | Bit {zeros = z; ones = o} ->
        Int32.(z land o = 0l)
      | _ -> false

    let is_bottom (bf : t) : bool =
      match bf with
      | Top -> false
      | Bottom -> true
      | Bit {ones = o; zeros = z} -> Int32.(o = 0l && z = 0l)

    let one = Bit {zeros = (-2l); ones = 1l}

    let not_ (x : t) : t =
      match x with
      | Top -> Top
      | Bottom -> Bottom
      | Bit x -> Bit {zeros = x.ones; ones = x.zeros}

    let and_ (bf1 : t) (bf2 : t) : t =
      match bf1, bf2 with
      | Top, x | x, Top -> x
      | Bottom, _ | _, Bottom -> Bottom
      | Bit bf1, Bit bf2 ->
        Bit {zeros = Int32.(bf1.zeros lor bf2.zeros); ones = Int32.(bf1.ones land bf2.ones)}

    let or_ (bf1 : t) (bf2 : t) : t =
      match bf1, bf2 with
      | Top, _ | _, Top -> Top
      | Bottom, x | x, Bottom -> x
      | Bit bf1, Bit bf2 ->
        Bit {zeros = Int32.(bf1.zeros land bf2.zeros); 
              ones = Int32.(bf1.ones lor bf2.ones)}

    let xor_ (bf1 : t) (bf2 : t) : t =
      match bf1, bf2 with
      | Top, _ | _, Top -> Top
      | Bottom, x | x, Bottom -> x
      | Bit bf1, Bit bf2 ->
        Bit {zeros = Int32.((bf1.zeros land bf2.zeros) lor (bf1.ones land bf2.ones)); 
              ones = Int32.((bf1.zeros land bf2.ones) lor (bf1.ones land bf2.zeros))}

    let contains (bf : t) (n : int32) : bool =
      match bf with
      | Top -> true
      | Bottom -> false
      | Bit {zeros = z; ones = o} ->
        Int32.(n land o = n) && Int32.((lnot n) land z = (lnot n))
    
    let rec shift_left (bf1 : t) (bf2 : t)  : t = (* TODO: manage case where bf2 doesn't contain [0,32] *)
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
                or_ acc (
                  Bit {zeros = Int32.((shift_left z n) lor (shift_left 1l n - 1l)); 
                        ones = Int32.(shift_left o n)}
                )
              else
                acc)
        )


    let rec shift_right_unsigned (bf1 : t) (bf2 : t)  : t =
      match bf1 with
      | Top -> shift_right_unsigned (Bit {zeros = 0b11111111111111111111111111111111l; 
                                           ones = 0b11111111111111111111111111111111l}) bf2
      | Bottom -> Bottom
      | Bit {zeros = z; ones = o} ->
        reduce (
          List.fold
            (List.init 32 ~f:(fun n -> n))
            ~init:Bottom
            ~f:(fun acc n ->
              if contains bf2 (Int32.of_int_exn n) then
                or_ acc (
                  let n' = 32 - n in
                  Bit {zeros = Int32.((shift_right_logical z n) lor (shift_left (-1l) n')); 
                        ones = Int32.(shift_right_logical o n)}
                )
              else
                acc)
        )

    let rec shift_right_signed (bf1 : t) (bf2 : t)  : t = (* TODO: make a single shift right function that takes a shift_right function as argument. *)
      match bf1 with
      | Top -> shift_right_signed (Bit {zeros = 0b11111111111111111111111111111111l; 
                                         ones = 0b11111111111111111111111111111111l}) bf2
      | Bottom -> Bottom
      | Bit {zeros = z; ones = o} ->
        reduce (
          List.fold
            (List.init 32 ~f:(fun n -> n))
            ~init:Bottom
            ~f:(fun acc n ->
              if contains bf2 (Int32.of_int_exn n) then
                or_ acc (
                  Bit {zeros = Int32.((shift_right z n)); 
                        ones = Int32.(shift_right o n)}
                )
              else
                acc)
        )

    let contains_positive_values (bf : t) : bool =
      match bf with
      | Top -> true
      | Bottom -> false
      | Bit {zeros = z; _} -> Int32.(z < 0l)

    let contains_negative_values (bf : t) : bool =
      match bf with
      | Top -> true
      | Bottom -> false
      | Bit {ones = o; _} -> Int32.(o < 0l)

    (* let number_of_trailing_zeros (x : int32) : int =
      let rec aux x n =
        if Int32.(x % 2l = 1l) then
          n
        else
          aux (Int32.(shift_right x 1)) (n + 1) 
      in
      aux x 0 *)

    let constant_value (bf : t) : int32 =
      match bf with
      | Top -> 0l
      | Bottom -> assert false
      | Bit {zeros = z; ones = o} -> Int32.(o land (lnot z))

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
          let lower_bound = if Int32.(variable >= 0l) then 0l else Int32.shift_right 0b10000000000000000000000000000000l power_of_two in
          let upper_bound = Int32.(shift_right_logical) (Int32.(shift_left) variable 1) (1 + power_of_two) in
          power_of_two, lower_bound, upper_bound



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

let%test_module "Bitfield tests" = (module struct
  open Bitfield

  let%test "Bitfield_tests" =
    print_endline "\n_______ ____________________________ _______\n        BitField Module        \n------- ---------------------------- -------\n"; true
  let%test "print bitfield with specific zeros and ones" =
    (* zeros everywhere except lowest 4 bits: bits 0-3 can be 0, others must be 1.
       So zeros mask has bits 0-3 set to 1. *)
    let zeros = 0b11111111111111111111111111110000l in
    (* ones on the lowest 6 bits: bits 0-5 can be 1, so ones mask bits 0-5 set to 1. *)
    let ones  = 0b00000000000000000000000000111111l in
    let bf = Bit { zeros; ones } in
    Printf.printf "(%s, %s): %s\n" (Int32.to_string zeros) (Int32.to_string ones) (to_string bf);
    String.is_suffix ~suffix:"::1111" (to_string bf)

  let%test "binSet.of_set" =
    let int_set = Set.of_list (module Int32) [2l; 3l; 6l] in
    let bf = of_set int_set in
    Printf.printf "of_set [2l; 3l; 6l] %s\n" (to_string bf);
    true

  let%test "contains returns true when value matches the bitfield" =
    let bf = of_set (Set.of_list (module Int32) [2l; 3l; 6l]) in
    contains bf 3l

  let%test "contains returns false when value does not match the bitfield" =
    let bf = of_set (Set.of_list (module Int32) [2l; 3l; 6l]) in
    not (contains bf 4l)

  let%test "contains positive values" =
    let bf = of_set (Set.of_list (module Int32) [-2l; 2l; 3l; 6l]) in
    contains_positive_values bf
  
  let%test "contains no positive values" =
    let bf = of_set (Set.of_list (module Int32) [-2l; -3l; -6l]) in
    not (contains_positive_values bf)

  (* let%test "Top contains all positives" =
    Printf.printf "positives Top = %s" (to_string (positives Top));
    true *)

  let%test "constant value of ::1::1010 = 001001010" =
    let bf = of_set (Set.of_list (module Int32) [0b001001010l;
                                                 0b011111010l]) in
    print_endline (to_string bf);
    Int32.(constant_value bf = 0b001001010l)

  let%test "constant value of :::101: = 1010" =
    let bf = of_set (Set.of_list (module Int32) [0b00001010l;
                                                 0b01111011l]) in
    print_endline (to_string bf);
    Int32.(constant_value bf = 0b1010l)

  let%test "variable values of :1::1010 = 2^4[0, 1011]" =
    let bf = of_set (Set.of_list (module Int32) [0b001001010l;
                                                 0b011111010l]) in
    print_endline (to_string bf);
    let n, l, u = variable_values bf in
    print_endline ("low: " ^ Int32.to_string l ^ " high: " ^ Int32.to_string u);
    n = 4 && Int32.(l = 0l) && Int32.(u =  0b1011l)

  let%test "Variable values of :::101: = 2^0[0, 1110001]" =
    let bf = of_set (Set.of_list (module Int32) [0b00001010l;
                                                 0b01111011l]) in
    print_endline (to_string bf);
    let n, l, u = variable_values bf in
    n = 0 && Int32.(l = 0l) && Int32.(u = 0b1110001l)

  let%test "shift left Top" = 
    print_endline "shift_left Top";
    let x = shift_left Top (of_integer 2l) in
    match x with
    | Top | Bottom -> assert false
    | Bit {zeros = z; ones = o} -> 
      print_endline (Int32.to_string z ^ "  " ^ Int32.to_string o);
    print_endline ("shift left Top: " ^ to_string x ^ ".");
    true


  let%test "shift_right_unsigned (-1l)" =
    let neg_one = Bit {zeros = 0l; ones = (-1l)} in
    let result = shift_right_unsigned neg_one one in
    print_endline (to_string neg_one ^ "\n" ^ to_string result);
    match result with
    | Bit {zeros = 0b10000000000000000000000000000000l; ones = 0b1111111111111111111111111111111l} -> true
    | _ -> false
end)