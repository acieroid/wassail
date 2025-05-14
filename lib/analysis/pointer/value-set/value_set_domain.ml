(** This module defines the domain used in value-set analysis for pointer analysis.

    Each variable is mapped to a set of abstract memory locations, where memory locations
    are either:
      - TopMemory: indicating potential alias with any memory block,
      - BottomMemory: indicating no known memory location,
      - Mem block: a specific memory block (with offset and size).

    The domain is used to track possible memory locations that variables or expressions may refer to.
*)

open Core

module Value_sets = struct
  (** Representation of abstract memory values used in the value-set domain.
      These values represent the possible memory blocks a variable may point to.
  *)
  module Abstract_memory = struct
    type t =
      | TopMemory (* May refer to any memory block *)
      | BottomMemory (* Refers to no memory block *)
      | Mem of Memory_block.t
    [@@deriving sexp, compare, equal]

    let to_string (abs_mem : t) : string =
      match abs_mem with 
      | TopMemory -> "TopMemory"
      | BottomMemory -> "BottomMemory"
      | Mem mem_block -> Memory_block.to_string mem_block
  end

  (** A set of abstract memory values representing the value-set of a variable. *)
  module ValueSet = struct
    include Set
    include Set.Make(Abstract_memory)
    let to_string (set : t) : string =
      "{" ^
      (Set.to_list set
             |> List.map ~f:Abstract_memory.to_string
             |> String.concat ~sep:", ")
      ^ "}"

    (** A set containing only TopMemory, representing complete uncertainty. *)
    let top_set = singleton TopMemory
    (** An empty set, representing no points-to information (Bottom). *)
    let bottom_set = empty
  end

  type set = ValueSet.t

  (** Adds a memory block to an existing set of abstract memory locations.
      If TopMemory is added, the result is Top.
      If BottomMemory is added, the original set is returned unchanged.
      If the memory block overlaps or touches others in the set, they are merged.
  *)
  let add_memory_block (mem : Abstract_memory.t) (value_set : set) : set =
    match mem with 
    | TopMemory -> ValueSet.top_set
    | BottomMemory -> value_set (* nothing to add *)
    | Mem block ->
      if (ValueSet.equal value_set ValueSet.top_set) then (
        value_set
      ) else (
        if (ValueSet.equal value_set ValueSet.bottom_set) then (
          ValueSet.singleton mem
        ) else (
          let (overlapping_set, non_overlapping_set) =
            Set.partition_tf value_set ~f:(fun x ->
              match x with
              | Mem b -> Memory_block.touching (Some b) (Some block)
              | _ -> false) in
          let overlapping  =
            List.map ~f:(fun x -> 
              match x with 
              | Mem b -> b
              | _ -> failwith "unreachable")
            (Set.to_list overlapping_set) in 
          let merged_blocks = Memory_block.merge (Some block) (Memory_block.merge_all overlapping) in
          let non_overlapping = (Set.to_list non_overlapping_set) in
          match merged_blocks with 
          | None -> value_set 
          | Some merged_blocks ->
            ValueSet.of_list (Mem merged_blocks :: non_overlapping) (* Je n'ai toujours pas réussi à utiliser ValueSet.add *)
        )
      )

  (** [value_set_join s1 s2] computes the union of two abstract memory sets [s1] and [s2],
      taking into account potential merging of overlapping memory blocks. The resulting
      set represents all memory locations that may be referred to by either [s1] or [s2].
  *)
  let value_set_join (s1 :set) (s2 : set) : set =
    let s = Set.to_list s1 in
    let s = (Set.to_list s2) @ s in
    let add = fun mem_set mem  ->
      add_memory_block mem mem_set in
    match s with 
    | [] -> ValueSet.empty
    | x :: xs ->
      List.fold ~init:(add ValueSet.empty x) ~f:add xs
    


  (** The abstract store mapping variables to their value-sets. *)
  type t = set Variable.Map.t

  (** An empty value-set store (bottom element of the domain lattice). *)
  let bottom_value_sets = Variable.Map.empty

  (** Pretty-prints the abstract store as a string. *)
  let to_string (value_sets : t) : string =
    "Value_sets = [\t" ^
    (Map.to_alist value_sets
    |> List.map ~f:(fun (var, mem_set) ->
         let mems =
           ValueSet.to_string mem_set
         in
         Variable.to_string var ^ " ↦ " ^ mems ^ "")
    |> String.concat ~sep:",\n\t\t")
    ^ "\t]"

  (* let join RENDU ICI! *)










































































  (** Unit tests for the value-set domain operations. *)
  module Test = struct
    let%test_module "add_memory_block tests" = (module struct
      (* [0..4] *)
      let block1 = Memory_block.Absolute 0, Memory_block.ExtendedInt.Int 0, Memory_block.ExtendedInt.Int 4
      (* [3..6] *)
      let block2 = Memory_block.Absolute 0, Memory_block.ExtendedInt.Int 3, Memory_block.ExtendedInt.Int 6
      (* [4..7] *)
      let block3 = Memory_block.Absolute 0, Memory_block.ExtendedInt.Int 4, Memory_block.ExtendedInt.Int 7
      (* [5..8] *)
      let block4 = Memory_block.Absolute 5, Memory_block.ExtendedInt.Int 0, Memory_block.ExtendedInt.Int 4
      (* [6..9] *)
      let block5 = Memory_block.Absolute 6, Memory_block.ExtendedInt.Int 0, Memory_block.ExtendedInt.Int 4

      let mem1 = Abstract_memory.Mem block1
      let mem2 = Abstract_memory.Mem block2
      let mem3 = Abstract_memory.Mem block3
      let mem4 = Abstract_memory.Mem block4
      let mem5 = Abstract_memory.Mem block5

      let x = Variable.Var (Var.Global 3)
      let y = Variable.Var (Var.Local 4)
      let z = Variable.Mem (block1)

      let%test "testing value sets" =
        let xPointsTo = ValueSet.bottom_set |> add_memory_block mem3 |> add_memory_block mem4 in
        let yPointsTo = ValueSet.bottom_set |> add_memory_block mem1 in
        let zPointsTo = ValueSet.bottom_set |> add_memory_block mem1 |> add_memory_block mem4 in
        let value_sets = Map.set bottom_value_sets ~key:x ~data:xPointsTo in
        let value_sets = Map.set value_sets ~key:y ~data:yPointsTo in
        let value_sets = Map.set value_sets ~key:z ~data:zPointsTo in
        print_endline (to_string value_sets);
        true 

      let%test "adding to bottom set" =
        (* print_endline "test1"; *)
        let s = add_memory_block mem1 ValueSet.bottom_set in
        print_endline ("Adding " ^
        Abstract_memory.to_string mem1 ^
          " to " ^
          ValueSet.to_string ValueSet.bottom_set ^
          " => " ^
          ValueSet.to_string s);
        Set.mem s mem1

      let%test "adding overlapping blocks merges them" =
        (* print_endline "test2"; *)
        let s1 = add_memory_block mem1 ValueSet.bottom_set in
        let s = add_memory_block mem2 s1 in
        print_endline ("Adding " ^
        Abstract_memory.to_string mem2 ^
          " to " ^
          ValueSet.to_string s1 ^ 
          " => " ^
          ValueSet.to_string s);
        match Set.to_list s with
        | [Mem (Memory_block.Absolute 0, Memory_block.ExtendedInt.Int 0, Memory_block.ExtendedInt.Int 6)] -> true
        | _ -> false

      let%test "adding adjacent blocks merges them" =
        (* print_endline "test3"; *)
        let s1 = add_memory_block mem3 ValueSet.bottom_set in
        let s = add_memory_block mem1 s1 in
        print_endline ("Adding " ^
        Abstract_memory.to_string mem1 ^
          " to " ^
          ValueSet.to_string s1 ^
          " => " ^
          ValueSet.to_string s);
        match Set.to_list s with
        | [Mem (Memory_block.Absolute 0, Memory_block.ExtendedInt.Int 0, Memory_block.ExtendedInt.Int 7)] -> true
        | _ -> false

      let%test "adding TopMemory" =
        (* print_endline "test3"; *)
        let s1 = add_memory_block mem3 ValueSet.bottom_set in
        let s = add_memory_block TopMemory s1 in
        print_endline ("Adding " ^
        Abstract_memory.to_string TopMemory ^
          " to " ^
          ValueSet.to_string s1 ^
          " => " ^
          ValueSet.to_string s);
        match Set.to_list s with
        | [TopMemory] -> true
        | _ -> false

      let%test "adding BottomMemory" =
        (* print_endline "test3"; *)
        let s1 = add_memory_block mem3 ValueSet.bottom_set in
        let s = add_memory_block BottomMemory s1 in
        print_endline ("Adding " ^
        Abstract_memory.to_string BottomMemory ^
          " to " ^
          ValueSet.to_string s1 ^
          " => " ^
          ValueSet.to_string s);
        match Set.to_list s with
        | [Mem (Memory_block.Absolute 0, Memory_block.ExtendedInt.Int 4, Memory_block.ExtendedInt.Int 7)] -> true
        | _ -> false

      let%test "adding non touching blocks doesn't merge them" =
        let s1 = add_memory_block mem5 ValueSet.bottom_set in
        (* let s = add_memory_block s1 mem1 in *)
        let s = ValueSet.bottom_set |> add_memory_block mem1 |> add_memory_block mem5 in 
        print_endline ("Adding " ^
        Abstract_memory.to_string mem1 ^
          " to " ^
          ValueSet.to_string s1 ^
          " => " ^
          ValueSet.to_string s);
        match Set.to_list s with
        | [Mem (Memory_block.Absolute 0, Memory_block.ExtendedInt.Int 0, Memory_block.ExtendedInt.Int 4); Mem (Memory_block.Absolute 6, Memory_block.ExtendedInt.Int 0, Memory_block.ExtendedInt.Int 4)] -> true
        | _ -> false
    end)

    let%test_module "Value_set join" = (module struct
      let block1 = Memory_block.Absolute 0, Memory_block.ExtendedInt.Int 0, Memory_block.ExtendedInt.Int 4
      let block2 = Memory_block.Absolute 0, Memory_block.ExtendedInt.Int 4, Memory_block.ExtendedInt.Int 7
      let block3 = Memory_block.Absolute 0, Memory_block.ExtendedInt.Int 8, Memory_block.ExtendedInt.Int 11
      let block4 = Memory_block.Absolute 100, Memory_block.ExtendedInt.Int 0, Memory_block.ExtendedInt.Int 4

      let mem1 = Abstract_memory.Mem block1
      let mem2 = Abstract_memory.Mem block2
      let mem3 = Abstract_memory.Mem block3
      let mem4 = Abstract_memory.Mem block4

      let%test "value_set_join merges sets correctly" =
        let s1 = ValueSet.bottom_set |> add_memory_block mem1 |> add_memory_block mem2 in
        let s2 = ValueSet.bottom_set |> add_memory_block mem3 in
        let joined = value_set_join s1 s2 in
        print_endline ("Join of " ^ ValueSet.to_string s1 ^ " and " ^ ValueSet.to_string s2 ^ " = " ^ ValueSet.to_string joined);
        match Set.to_list joined with
        | [Mem (Memory_block.Absolute 0, Memory_block.ExtendedInt.Int 0, Memory_block.ExtendedInt.Int 11)] -> true
        | _ -> false

      let%test "value_set_join with TopMemory results in TopMemory" =
        let s1 = ValueSet.bottom_set |> add_memory_block mem1 in
        let s2 = ValueSet.top_set in
        let joined = value_set_join s1 s2 in
        print_endline ("Join of " ^ ValueSet.to_string s1 ^ " and TopMemory = " ^ ValueSet.to_string joined);
        match Set.to_list joined with
        | [TopMemory] -> true
        | _ -> false

      let%test "value_set_join of two disjoint sets keeps both" =
        let s1 = ValueSet.bottom_set |> add_memory_block mem1 in
        let s2 = ValueSet.bottom_set |> add_memory_block mem4 in
        let joined = value_set_join s1 s2 in
        print_endline ("Join of " ^ ValueSet.to_string s1 ^ " and " ^ ValueSet.to_string s2 ^ " = " ^ ValueSet.to_string joined);
        match Set.to_list joined with
        | [Mem (Memory_block.Absolute 0, Memory_block.ExtendedInt.Int 0, Memory_block.ExtendedInt.Int 4); Mem (Memory_block.Absolute 100, Memory_block.ExtendedInt.Int 0, Memory_block.ExtendedInt.Int 4)]
        | [Mem (Memory_block.Absolute 100, Memory_block.ExtendedInt.Int 0, Memory_block.ExtendedInt.Int 4); Mem (Memory_block.Absolute 0, Memory_block.ExtendedInt.Int 0, Memory_block.ExtendedInt.Int 4)] -> true
        | _ -> false
    end)
  end


  (* D'autres tests suivront *)
end
