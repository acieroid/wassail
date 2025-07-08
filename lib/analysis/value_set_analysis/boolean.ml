open Reduced_interval_congruence
(* open Maths *)
open Core

type true_or_false = {
  true_ : RIC.t;
  false_ : RIC.t
}
[@@deriving sexp, compare, equal]

let true_or_false_to_string (tf : true_or_false) : string =
  "true(" ^ RIC.to_string tf.true_ ^ "), false(" ^ RIC.to_string tf.false_ ^ ")"

type t = true_or_false Variable.Map.t
[@@deriving sexp, compare, equal]

let to_string (boolean : t) : string =
  Printf.sprintf "[%s]" (String.concat ~sep:"; "
                          (List.map (Variable.Map.to_alist boolean)
                            ~f:(fun (k, t) ->
                                Printf.sprintf "%s : %s"
                                  (Variable.to_string k)
                                  (true_or_false_to_string t))))

let remove_non_memory_variable (boolean : t) ~(var : Variable.t) : t =
  assert (not (Variable.is_linear_memory var));
  Variable.Map.remove boolean var

let get (boolean : t) ~(var : Variable.t) : true_or_false =
  match Variable.Map.find boolean var with
  | None -> assert false
  | Some tf -> tf

let get_true (boolean : t) ~(var : Variable.t) : RIC.t =
  (get boolean ~var).true_

let get_false (boolean : t) ~(var : Variable.t) : RIC.t =
  (get boolean ~var).false_

let remove_memory_variables (boolean : t) ~(accessed : RIC.accessed) : t =
  let accessed = accessed.fully :: accessed.partially in
  let vars = Variable.Map.keys boolean in
  let value_sets = List.map vars ~f:(fun var -> Option.value_exn (Variable.Map.find boolean var)) in
  let truncated_variables = List.map vars ~f:(fun v -> Variable.remove_all ~these_addresses_list:accessed ~from:v) in
  assert (List.length value_sets = List.length truncated_variables);
  List.fold2_exn 
    ~init:Variable.Map.empty 
    ~f:(fun acc vars vs -> Variable.Map.update_all acc (Variable.Set.of_list vars) vs) 
    truncated_variables 
    value_sets

let and_ (boolean1 : t) (boolean2 : t) : t =
  let boolean1 = Variable.Map.make_compatible ~this:boolean1 ~relative_to:boolean2 ~get in
  let boolean2 = Variable.Map.make_compatible ~this:boolean2 ~relative_to:boolean1 ~get in
  Variable.Map.merge boolean1 boolean2 ~f:(fun ~key:k v ->
      match k, v with
      | _, `Both (x, y) -> 
        Some {true_ = RIC.meet x.true_ y.true_; false_ = RIC.join (RIC.join y.false_ y.true_) (RIC.join x.false_ x.true_)}
      | _, `Right _ | _, `Left _ -> None) (* If one of them is absent, it might have been deleted by a new variable assignment in one of the branches *)

let xor_ (boolean1 : t) (boolean2 : t) : t =
  let boolean1 = Variable.Map.make_compatible ~this:boolean1 ~relative_to:boolean2 ~get in
  let boolean2 = Variable.Map.make_compatible ~this:boolean2 ~relative_to:boolean1 ~get in
  Variable.Map.merge boolean1 boolean2 ~f:(fun ~key:k v ->
      match k, v with
      | _, `Both (x, y) -> Some {true_ = RIC.join (RIC.meet x.false_ y.true_) (RIC.meet x.true_ y.false_); 
                                false_ = RIC.join (RIC.meet x.true_ y.true_) (RIC.meet x.false_ y.false_)}
      | _, `Right _ | _, `Left _ -> None) (* Nothing can be inferred from this condition *)

let or_ (boolean1 : t) (boolean2 : t) : t =
  let boolean1 = Variable.Map.make_compatible ~this:boolean1 ~relative_to:boolean2 ~get in
  let boolean2 = Variable.Map.make_compatible ~this:boolean2 ~relative_to:boolean1 ~get in
  Variable.Map.merge boolean1 boolean2 ~f:(fun ~key:k v ->
      match k, v with
      | _, `Both (x, y) -> Some {true_ = RIC.join (RIC.join y.false_ y.true_) (RIC.join x.false_ x.true_); false_ = RIC.meet x.false_ y.false_}
      | _, `Right _ | _, `Left _ -> None) (* Nothing can be inferred from this condition *)

let not_ (boolean : t) : t =
  Variable.Map.fold 
    ~init:Variable.Map.empty 
    ~f:(fun ~key ~data:{true_ = t; false_ = f} acc -> Variable.Map.set acc ~key ~data:{true_ = f; false_ = t}) 
    boolean

let join (boolean1 : t) (boolean2 : t) : t = 
  let boolean1 = Variable.Map.make_compatible ~this:boolean1 ~relative_to:boolean2 ~get in
  let boolean2 = Variable.Map.make_compatible ~this:boolean2 ~relative_to:boolean1 ~get in
  Variable.Map.merge boolean1 boolean2 ~f:(fun ~key:k v ->
      match k, v with
      | _, `Both (x, y) -> Some {true_ = RIC.join x.true_ y.true_; false_ = RIC.join x.false_ y.false_}
      | _, `Right _ | _, `Left _ -> None) (* Nothing can be inferred from this condition *)

let meet (boolean1 : t) (boolean2 : t) : t = Log.warn "I'm not sure about the meet of two boolean conditions"; and_ boolean1 boolean2





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

let%test_module "Variable tests" = (module struct

  let%test "Tests on variable module" =
    print_endline "_______ ______________ _______\n        Boolean module        \n------- -------------- -------\n";
    true

  let%test "remove memory variables" =
    let var1 = Variable.mem (4, Int 0, Int 10, ("", 0)) in
    let vs_true = RIC.ric (0, Int 0, Int 0, ("", 42)) in
    let vs_false = RIC.ric (0, Int 0, Int 0, ("", 36)) in
    let b = Variable.Map.set Variable.Map.empty ~key:var1 ~data:{true_ = vs_true; false_ = vs_false} in
    let var2 = RIC.ric (2, Int 3, Int 6, ("", 0)) in
    let accessed = RIC.accessed ~value_set:var2 ~size:4 in
    let b2 = remove_memory_variables b ~accessed in
    print_endline (to_string b ^ "\n" ^ to_string b2);
    true (* TODO: complete test automation *)

  let%test "boolean1 and boolean2" =
    let var1 = Variable.mem (1, Int 0, Int 1, ("", 0)) in
    let vs_true = RIC.ric (1, Int 0, Int 1, ("", 42)) in
    let vs_false = RIC.ric (1, Int 0, Int 1, ("", 43)) in
    let bool1 = Variable.Map.set Variable.Map.empty ~key:var1 ~data:{true_ = vs_true; false_ = vs_false} in
    let var2 = Variable.mem (1, Int 1, Int 2, ("", 0)) in
    let bool2 = Variable.Map.set Variable.Map.empty ~key:var2 ~data:{true_ = vs_false; false_ = vs_true} in
    let bool3 = and_ bool1 bool2 in
    print_endline (to_string bool1 ^ "\n" ^ to_string bool2 ^ "\n" ^ to_string bool3);
    true (* TODO: complete test automation *)
    
end)
