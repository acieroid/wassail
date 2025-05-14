(** This module defines a representation of memory blocks and operations over them
    for static analysis of WebAssembly (or similar) programs.

    A memory block is modeled as a base address (absolute or symbolic/relative)
    along with lower and upper bounds that describe a range of offsets.

    The module provides utilities to:
    - Convert memory blocks to string representation.
    - Determine whether two memory blocks "touch" (overlap or are adjacent).
    - Merge two or more memory blocks if they touch.
*)

open Core 

(** A base address represents the origin of a memory block:
    - [Absolute int] refers to a fixed address in linear memory.
    - [Relative string] refers to a symbolic base (e.g., a stack frame name). *)
type base_address =
  | Absolute of int
  | Relative of string
[@@deriving sexp, compare, equal]

(** An integer that can be either a concrete int or infinity.
    Used to express unbounded memory regions. *)
module ExtendedInt = struct
  type t = 
    | Int of int
    | Infinity
  [@@deriving sexp, compare, equal]

  (** Converts an [Int2.t] value to its string representation. *)
  let to_string (i : t) : string =
    match i with 
    | Int i -> string_of_int i
    | Infinity -> "∞"

  (** Computes the maximum of two [Int2.t] values. *)
  let maximum (x : t) (y : t) : t =
    match x, y with 
    | Infinity, _ | _, Infinity -> Infinity
    | Int x, Int y -> Int (max x y)

  (** Computes the minimum of two [Int2.t] values. *)
  let minimum (x : t) (y : t) : t =
    match x, y with 
    | Infinity, _ | _, Infinity -> Infinity
    | Int x, Int y -> Int (min x y)
end


type lower_bound = ExtendedInt.t
[@@deriving sexp, compare, equal]

type upper_bound = ExtendedInt.t
[@@deriving sexp, compare, equal]

(** A memory block represented as a triple:
    - base address (absolute or relative),
    - lower bound (inclusive),
    - upper bound (inclusive or infinite). *)
type t = base_address * lower_bound * upper_bound
[@@deriving sexp, compare]

(** [subsetOf mem1 mem2] returns [true] if memory block [mem1] is entirely
    contained within memory block [mem2], and [false] otherwise.

    For absolute addresses, containment is tested by comparing the concrete
    ranges (base + offset) of both blocks.

    For relative addresses, containment is tested only if both blocks share
    the same symbolic base; bounds are compared using extended integer logic.

    Note: If base addresses differ or include symbolic/absolute mismatches,
    the function conservatively returns [false], even though actual inclusion
    might be possible.

    @param mem1 the potentially smaller memory block
    @param mem2 the potentially larger memory block
    @return [true] if [mem1] ⊆ [mem2]; [false] otherwise
*)
let subsetOf (mem1 : t) (mem2 : t) : bool =
  match mem1, mem2 with 
  | (Absolute b1, Int l1, Int u1), (Absolute b2, Int l2, Int u2) ->
    (b1 + l1) >= (b2 + l2) && (b1 + u1) <= (b2 + u2)
  | (Absolute b1, Int l1, _), (Absolute b2, Int l2, Infinity) ->
    (b1 + l1) >= (b2 + l2)
  | (Absolute b1, _, Int u1), (Absolute b2, Infinity, Int u2) ->
    (b1 + u1) <= (b2 + u2)
  | _, (_, Infinity, Infinity) ->
    true
  | (Relative b1, l1, u1), (Relative b2, l2, u2) when String.equal b1 b2 ->
    ExtendedInt.equal (ExtendedInt.minimum l1 l2) l2 && ExtendedInt.equal (ExtendedInt.maximum u1 u2) u2
  | _ -> false 
  (* Warning: if two memory blocks have non matching base addresses (one Relative and one Absolute, or two different Relatives), there is no way to know whether mem1 is included in mem2 or not. The result should be "May" to signify that inclusion is possible.
    TODO: see if we can manage that *)

let equal (t1 : t) (t2 : t) : bool =
  match t1, t2 with 
  | (Absolute b1, Int l1, Int u1), (Absolute b2, Int l2, Int u2) ->
    (b1 + l1 = b2 + l2) && (b1 + u1 = b2 + u2)
  | (Absolute b1, Infinity, Int u1), (Absolute b2, Infinity, Int u2) ->
    b1 + u1 = b2 + u2
  | (_, Infinity, Infinity), (_, Infinity, Infinity) ->
    true
  | (Absolute b1, Int l1, Infinity), (Absolute b2, Int l2, Infinity) ->
    b1 + l1 = b2 + l2
  | (Relative b1, Int l1, Int u1), (Relative b2, Int l2, Int u2) ->
    String.equal b1 b2 && l1 = l2 && u1 = u2
  | (Relative b1, Infinity, Int u1), (Relative b2, Infinity, Int u2) ->
    String.equal b1 b2 && u1 = u2
  | (Relative b1, Int l1, Infinity), (Relative b2, Int l2, Infinity) ->
    String.equal b1 b2 && l1 = l2
  | _ -> false

(** Returns a string representation of a memory block [t], including
    base and offset range in readable format. *)
let to_string (block : t) : string =
  "[" ^
  (match block with 
  | (Absolute base, Int lower, Int upper) ->
    let x = base + lower in
    let y = base + upper in 
    string_of_int x ^ ".." ^ string_of_int y
  | (Absolute base, Infinity, Int upper) ->
    let y =  base + upper in 
    "0.." ^ string_of_int y
  | (Absolute base, Int lower, Infinity) ->
    let x =  base + lower in 
    string_of_int x ^ "..∞"
  | (Absolute _, Infinity, Infinity) ->
    "0..∞"
  | (Relative base, Int lower, upper) ->
    base ^ " + (" ^ string_of_int lower ^ ".." ^ (ExtendedInt.to_string upper) ^ ")"
  | (Relative base, Infinity, upper) ->
    base ^ " + (-∞.." ^ (ExtendedInt.to_string upper) ^ ")")
  ^ "]"
  

(** Determines whether two memory blocks [block1] and [block2] touch,
    meaning they either overlap or are adjacent in memory.
    Returns [true] if they touch, [false] otherwise. *)
let touching (block1 : t option) (block2 : t option) : bool =
  match block1, block2 with 
  | Some (Absolute base1, lower1, upper1), Some (Absolute base2, lower2, upper2) ->
    (match lower1, upper1, lower2, upper2 with 
    | Infinity, Infinity, _, _ | _, _, Infinity, Infinity -> true
    | Infinity, _, Infinity, _ | _, Infinity, _, Infinity -> true 
    | Infinity, Int u1, Int l2, _ -> (base2 + l2) <= (base1 + u1 + 1)
    | Int l1, _, Infinity, Int u2 -> (base1 + l1) <= (base2 + u2 + 1)
    | Int l1, Infinity, _, Int u2 -> (base2 + u2) <= (base1 + l1 - 1)
    | _, Int u1, Int l2, Infinity -> (base1 + u1) <= (base2 + l2 - 1) 
    | Int l1, Int u1, Int l2, Int u2 -> 
      (let x1 = base1 + l1 in
      let y1 = base1 + u1 in
      let x2 = base2 + l2 in
      let y2 = base2 + u2 in
      (x1 <= x2 && y1 >= x2 - 1) || (x2 <= x1 && y2 >= x1 - 1))
    )
  | Some (Relative base1, lower1, upper1), Some (Relative base2, lower2, upper2) when String.equal base1 base2 ->
    (match lower1, upper1, lower2, upper2 with 
    | Infinity, Infinity, _, _ | _, _, Infinity, Infinity -> true
    | Infinity, _, Infinity, _ | _, Infinity, _, Infinity -> true 
    | Infinity, Int u1, Int l2, _ -> (l2) <= (u1 + 1)
    | Int l1, _, Infinity, Int u2 -> (l1) <= (u2 + 1)
    | Int l1, Infinity, _, Int u2 -> (u2) <= (l1 - 1)
    | _, Int u1, Int l2, Infinity -> (u1) <= (l2 - 1) 
    | Int l1, Int u1, Int l2, Int u2 -> 
      (l1 <= l2 && u1 >= l2 - 1) || (l2 <= l1 && u2 >= u1 - 1)
    )
  | None, _ | _, None -> false
  | _ -> failwith "impossible to determine whether two memory blocks are touching if their base addresses are undetermined"

(** Attempts to merge two memory blocks if they touch.
    Returns the merged block, or raises an error if they do not touch. *)
let merge (block1 : t option) (block2 : t option) : t option =
  match block1, block2 with 
  | Some block1, Some block2 ->
    if (touching (Some block1) (Some block2)) then
      match block1, block2 with 
      | (b1, l1, u1), (b2, l2, u2) -> (
        match b1 with 
        | Relative _ -> (
          let x = ExtendedInt.minimum l1 l2 in
          let y = ExtendedInt.maximum u1 u2 in
          Some (b1, x, y)
        )
        | Absolute base1 -> (
          match b2 with 
          | Absolute base2 ->
            let x = if (ExtendedInt.equal l1 Infinity || ExtendedInt.equal l2 Infinity) then ExtendedInt.Infinity else (
              match l1, l2 with 
              | Int l1, Int l2 -> ExtendedInt.minimum (Int (base1 + l1)) (Int (base2 + l2))
              | _ -> failwith "unreachable"
            ) in
            let y = if (ExtendedInt.equal u1 Infinity || ExtendedInt.equal u2 Infinity) then ExtendedInt.Infinity else (
              match u1, u2 with 
              | Int u1, Int u2 -> ExtendedInt.maximum (Int (base1 + u1)) (Int (base2 + u2))
              | _ -> failwith "unreachable"
            ) in
            Some (Absolute 0, x, y)
          | _ -> failwith "unreachable"
        )
      )
    else
      failwith "memory blocks need to touch in order to be merged"
  | Some block, None | None, Some block -> Some block
  | None, None -> None

(** Attempts to merge a list of memory blocks, pairwise.
    Returns [None] for an empty list, or [Some block] for the merged result. *)
let merge_all (blocks : t list) : t option =
  match blocks with 
  | [] -> None
  | x :: xs -> (List.fold ~init:(Some x) ~f:(fun acc b -> merge acc (Some b)) xs)








































(* Unit tests for memory_block.ml *)
let%test_module "memory_block tests" = (module struct
  (* [0..3] *)
  let b1 = (Absolute 0, ExtendedInt.Int 0, ExtendedInt.Int 3)
  (* [4..7] *)
  let b2 = (Absolute 0, ExtendedInt.Int 4, ExtendedInt.Int 7)
  (* [5..9] *)
  let b3 = (Absolute 0, ExtendedInt.Int 5, ExtendedInt.Int 9)
  (* stack + [0..3] *)
  let b4 = (Relative "stack", ExtendedInt.Int 0, ExtendedInt.Int 3)
  (* stack + [3..6] *)
  let b5 = (Relative "stack", ExtendedInt.Int 3, ExtendedInt.Int 6)
  

  let%expect_test "touching_absolute" =
    print_endline (Bool.to_string (touching (Some b1) (Some b2)));
    print_endline (Bool.to_string (touching (Some b1) (Some b3)));
    [%expect {|
      true
      false
    |}]

  let%expect_test "touching_relative_success" =
    print_endline (Bool.to_string (touching (Some b4) (Some b5)));
    [%expect {|
      true
    |}]

  let%test "merging [0..3] with [4..7] yields [0..7]" =
    let x = merge (Some b1) (Some b2) in
    match x with
    | None -> false
    | Some x ->
      print_endline ("merging " ^ to_string b1 ^ " with " ^ to_string b2 ^ " -> " ^ to_string x);
      equal x (Absolute 1, Int (-1), Int 6)
    

  (* D'autres tests viendront *)
end)
