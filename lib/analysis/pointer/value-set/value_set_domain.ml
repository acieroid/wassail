(** This module defines the domain used in value-set analysis for pointer analysis.

    Each variable is mapped to a set of abstract memory locations, where memory locations
    are either:
      - TopMemory: indicating potential alias with any memory block,
      - BottomMemory: indicating no known memory location,
      - Mem block: a specific memory block (with offset and size).

    The domain is used to track possible memory locations that variables or expressions may refer to.
*)

open Core

module Abstract_store = struct
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

  (** [to_string set] converts the value-set [set] into a human-readable string representation.

        Useful for debugging and printing analysis results.
    *)
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

    (** Adds a memory block to an existing set of abstract memory locations.
        If TopMemory is added, the result is Top.
        If BottomMemory is added, the original set is returned unchanged.
        If the memory block overlaps or touches others in the set, they are merged.
    *)
    let add_memory_block (memory : Abstract_memory.t) (value_set : t) : t =
      match memory with 
      | TopMemory -> top_set
      | BottomMemory -> value_set (* nothing to add *)
      | Mem block ->
        if (equal value_set top_set) then
          value_set
        else if (equal value_set bottom_set) then
          singleton memory
        else
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
          let merged_blocks = (List.fold ~init:block ~f:(
            fun acc x -> 
              Option.value_exn ~here:[%here] ~error:(Error.of_string "Expected Some from merge") 
              (Memory_block.merge (Some acc) (Some x))) 
            overlapping) in
          add non_overlapping_set (Mem merged_blocks)

    let add_block = fun mem_set mem  ->
      add_memory_block mem mem_set
    
    (** [join s1 s2] computes the union of two abstract memory sets [s1] and [s2],
        merging overlapping memory blocks when necessary.

        The resulting set represents all memory locations that may be referred to by either input.
    *)
    let join (s1 : t) (s2 : t) : t =
      let s = Set.to_list s1 in
      let s = (Set.to_list s2) @ s in
      List.fold ~init:empty ~f:add_block s

    (** [member m vs] checks if the abstract memory [m] is an element of the value-set [vs].

        This function is used to determine whether a given memory block or memory abstraction
        is part of the set of possible memory locations associated with a variable.
    *)
    let member (m : Abstract_memory.t) (vs : t) : bool =
      List.mem (Set.to_list vs) m ~equal:Abstract_memory.equal

      
    let find_intersections (m : Abstract_memory.t) (s : t) : t =
      match m with 
      | TopMemory -> s
      | BottomMemory -> bottom_set
      | Mem block ->
        (let s = Set.to_list s in 
        List.fold ~init:empty ~f:(
          fun acc x -> 
            match x with 
            | Mem x ->
              (let intersection = Memory_block.intersection (Some block) (Some x) in
              match intersection with 
              | None -> acc
              | Some intersection -> add_memory_block (Mem intersection) acc)
            | TopMemory -> add_memory_block m acc  
            | BottomMemory -> acc
          ) s)

    
    (** [meet s1 s2] computes the intersection of two abstract memory sets [s1] and [s2].

        The result is a new set containing only those memory blocks that are present in both sets.
        This operation is used to refine the set of possible memory locations shared between paths.
    *)
    let meet (s1 : t) (s2 : t) : t =
      if (member TopMemory s1) then
        s2
      else if (member TopMemory s2) then
        s1
      else if (equal empty s1 || equal empty s2) then
        empty
      else 
        let s1 = Set.to_list s1 in
        List.fold ~init:empty ~f:(
          fun acc x -> 
            let intersections = find_intersections x s2 in
            (List.fold ~init:acc ~f:add_block (Set.to_list intersections))
        ) s1

  end

  (** Alias for the value set type representing sets of abstract memory blocks. *)
  type valueSet = ValueSet.t
    
  (** The abstract store mapping variables to their value-sets. *)
  type t = valueSet Variable.Map.t

  (** An empty value-set store (bottom element of the domain lattice). *)
  let bottom = Variable.Map.empty

  (** [to_string value_sets] returns a string representation of the entire abstract store,
      listing each variable and the value-set it maps to.
  *)
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

  (** [join vs1 vs2] computes the least upper bound (join) of two abstract stores [vs1] and [vs2].
      
      Each abstract store maps variables to sets of abstract memory locations (value-sets).
      The join operation merges these mappings variable-wise: for each variable present
      in either store, it joins their corresponding value-sets using [value_set_join].

      This operation is used to combine analysis information from two different control-flow paths.
  *)
  let join (store1 : t) (store2 : t) : t =
    let vars =
      List.concat [Map.keys store1; Map.keys store2]
      |> Variable.Set.of_list
      |> Set.to_list
    in
    List.fold ~init:bottom ~f:(
      fun acc var ->
        let set1 = Map.find store1 var |> Option.value ~default:ValueSet.bottom_set in
        let set2 = Map.find store2 var |> Option.value ~default:ValueSet.bottom_set in
        let joined_value_sets = ValueSet.join set1 set2 in
        Map.set ~key:var ~data:joined_value_sets acc
      ) vars
  
  (** [join_all stores] computes the least upper bound (join) of a list of abstract stores.

      It successively joins each store in the list using the [join] function, starting from the
      bottom element of the domain. The result is an abstract store that conservatively approximates
      all memory location information from the input stores.

      This is typically used when merging analysis information from multiple control-flow predecessors.

      @param stores the list of abstract stores to join
      @return a single abstract store that is the join of all input stores
  *)
  let join_all (stores : t list) : t =
    List.fold ~init:bottom ~f:join stores

  (** [meet store1 store2] computes the greatest lower bound (meet) of two abstract stores.

      The meet operation intersects the value-sets associated with each variable in both stores,
      yielding a new store that conservatively captures only shared memory location information.

      This is typically used in backward analyses or in narrowing operations.
  *)
  let meet (store1 : t) (store2 : t) : t =
    let vars =
      List.concat [Map.keys store1; Map.keys store2]
      |> Variable.Set.of_list
      |> Set.to_list
    in
    List.fold ~init:bottom ~f:(
      fun acc var ->
        let set1 = Map.find store1 var |> Option.value ~default:ValueSet.bottom_set in
        let set2 = Map.find store2 var |> Option.value ~default:ValueSet.bottom_set in
        let met_value_sets = ValueSet.meet set1 set2 in
        Map.set ~key:var ~data:met_value_sets acc
      ) vars





































































  (** Unit tests for the value-set domain operations. *)
  module Test = struct
    let%test_module "add_memory_block tests" = (module struct
      (* [0..4] *)
      let block1 = Memory_block.Absolute 0, Memory_block.ExtendedInt.Int 0, Memory_block.ExtendedInt.Int 4
      (* [3..6] *)
      let block2 = Memory_block.Absolute 0, Memory_block.ExtendedInt.Int 3, Memory_block.ExtendedInt.Int 6
      (* [4..7] *)
      let block3 = Memory_block.Absolute 0, Memory_block.ExtendedInt.Int 4, Memory_block.ExtendedInt.Int 7
      (* [5..9] *)
      let block4 = Memory_block.Absolute 5, Memory_block.ExtendedInt.Int 0, Memory_block.ExtendedInt.Int 4
      (* [6..10] *)
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
        let xPointsTo = ValueSet.bottom_set |> ValueSet.add_memory_block mem3 |> ValueSet.add_memory_block mem4 in
        let yPointsTo = ValueSet.bottom_set |> ValueSet.add_memory_block mem1 in
        let zPointsTo = ValueSet.bottom_set |> ValueSet.add_memory_block mem1 |> ValueSet.add_memory_block mem4 in
        let value_sets = Map.set bottom ~key:x ~data:xPointsTo in
        let value_sets = Map.set value_sets ~key:y ~data:yPointsTo in
        let value_sets = Map.set value_sets ~key:z ~data:zPointsTo in
        print_endline (to_string value_sets);
        true 

      let%test "adding to bottom set" =
        (* print_endline "test1"; *)
        let s = ValueSet.add_memory_block mem1 ValueSet.bottom_set in
        print_endline ("Adding " ^
        Abstract_memory.to_string mem1 ^
          " to " ^
          ValueSet.to_string ValueSet.bottom_set ^
          " => " ^
          ValueSet.to_string s);
        Set.mem s mem1

      let%test "adding overlapping blocks merges them" =
        (* print_endline "test2"; *)
        let s1 = ValueSet.add_memory_block mem1 ValueSet.bottom_set in
        let s = ValueSet.add_memory_block mem2 s1 in
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
        let s1 = ValueSet.add_memory_block mem3 ValueSet.bottom_set in
        let s = ValueSet.add_memory_block mem1 s1 in
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
        let s1 = ValueSet.add_memory_block mem3 ValueSet.bottom_set in
        let s = ValueSet.add_memory_block TopMemory s1 in
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
        let s1 = ValueSet.add_memory_block mem3 ValueSet.bottom_set in
        let s = ValueSet.add_memory_block BottomMemory s1 in
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
        let s1 = ValueSet.add_memory_block mem5 ValueSet.bottom_set in
        (* let s = add_memory_block s1 mem1 in *)
        let s = ValueSet.bottom_set |> ValueSet.add_memory_block mem1 |> ValueSet.add_memory_block mem5 in 
        print_endline ("Adding " ^
        Abstract_memory.to_string mem1 ^
          " to " ^
          ValueSet.to_string s1 ^
          " => " ^
          ValueSet.to_string s);
        match Set.to_list s with
        | [Mem (Memory_block.Absolute 0, Memory_block.ExtendedInt.Int 0, Memory_block.ExtendedInt.Int 4); Mem (Memory_block.Absolute 6, Memory_block.ExtendedInt.Int 0, Memory_block.ExtendedInt.Int 4)] -> true
        | _ -> false

      let%test "adding a block that touches two non touching blocks merges them all" =
        let s1 = ValueSet.bottom_set |> ValueSet.add_memory_block mem1 |> ValueSet.add_memory_block mem5 in 
        let s = s1 |> ValueSet.add_memory_block mem3 in
        print_endline ("Adding " ^ Abstract_memory.to_string mem3 ^ " to " ^ ValueSet.to_string s1 ^ " => " ^ ValueSet.to_string s);
        match Set.to_list s with
        | [Mem (Memory_block.Absolute 0, Memory_block.ExtendedInt.Int 0, Memory_block.ExtendedInt.Int 10)] -> true
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
        let s1 = ValueSet.bottom_set |> ValueSet.add_memory_block mem1 |> ValueSet.add_memory_block mem2 in
        let s2 = ValueSet.bottom_set |> ValueSet.add_memory_block mem3 in
        let joined = ValueSet.join s1 s2 in
        print_endline ("Join of " ^ ValueSet.to_string s1 ^ " and " ^ ValueSet.to_string s2 ^ " = " ^ ValueSet.to_string joined);
        match Set.to_list joined with
        | [Mem (Memory_block.Absolute 0, Memory_block.ExtendedInt.Int 0, Memory_block.ExtendedInt.Int 11)] -> true
        | _ -> false

      let%test "value_set_join with TopMemory results in TopMemory" =
        let s1 = ValueSet.bottom_set |> ValueSet.add_memory_block mem1 in
        let s2 = ValueSet.top_set in
        let joined = ValueSet.join s1 s2 in
        print_endline ("Join of " ^ ValueSet.to_string s1 ^ " and TopMemory = " ^ ValueSet.to_string joined);
        match Set.to_list joined with
        | [TopMemory] -> true
        | _ -> false

      let%test "value_set_join of two disjoint sets keeps both" =
        let s1 = ValueSet.bottom_set |> ValueSet.add_memory_block mem1 in
        let s2 = ValueSet.bottom_set |> ValueSet.add_memory_block mem4 in
        let joined = ValueSet.join s1 s2 in
        print_endline ("Join of " ^ ValueSet.to_string s1 ^ " and " ^ ValueSet.to_string s2 ^ " = " ^ ValueSet.to_string joined);
        match Set.to_list joined with
        | [Mem (Memory_block.Absolute 0, Memory_block.ExtendedInt.Int 0, Memory_block.ExtendedInt.Int 4); Mem (Memory_block.Absolute 100, Memory_block.ExtendedInt.Int 0, Memory_block.ExtendedInt.Int 4)]
        | [Mem (Memory_block.Absolute 100, Memory_block.ExtendedInt.Int 0, Memory_block.ExtendedInt.Int 4); Mem (Memory_block.Absolute 0, Memory_block.ExtendedInt.Int 0, Memory_block.ExtendedInt.Int 4)] -> true
        | _ -> false
    end)

    let%test_module "Value_set meet" = (module struct
      let block1 = Memory_block.Absolute 0, Memory_block.ExtendedInt.Int 0, Memory_block.ExtendedInt.Int 4
      let block2 = Memory_block.Absolute 0, Memory_block.ExtendedInt.Int 4, Memory_block.ExtendedInt.Int 7
      let block3 = Memory_block.Absolute 0, Memory_block.ExtendedInt.Int 8, Memory_block.ExtendedInt.Int 11
      let block4 = Memory_block.Absolute 0, Memory_block.ExtendedInt.Int 0, Memory_block.ExtendedInt.Int 7

      let mem1 = Abstract_memory.Mem block1
      let mem2 = Abstract_memory.Mem block2
      let mem3 = Abstract_memory.Mem block3
      let mem4 = Abstract_memory.Mem block4

      let%test "meet of overlapping sets keeps common blocks" =
        let s1 = ValueSet.bottom_set |> ValueSet.add_memory_block mem1 |> ValueSet.add_memory_block mem2 in
        let s2 = ValueSet.bottom_set |> ValueSet.add_memory_block mem2 |> ValueSet.add_memory_block mem3 in
        let met = ValueSet.meet s1 s2 in
        print_endline ("Meet of " ^ ValueSet.to_string s1 ^ " and " ^ ValueSet.to_string s2 ^ " = " ^ ValueSet.to_string met);
        match Set.to_list met with
        | [Mem _] when List.mem (Set.to_list met) mem2 ~equal:Abstract_memory.equal -> true
        | _ -> false

      let%test "meet of disjoint sets is empty" =
        let s1 = ValueSet.bottom_set |> ValueSet.add_memory_block mem1 in
        let s2 = ValueSet.bottom_set |> ValueSet.add_memory_block mem3 in
        let met = ValueSet.meet s1 s2 in
        print_endline ("Meet of " ^ ValueSet.to_string s1 ^ " and " ^ ValueSet.to_string s2 ^ " = " ^ ValueSet.to_string met);
        Set.is_empty met

      let%test "meet of equal sets is the same set" =
        let s1 = ValueSet.bottom_set |> ValueSet.add_memory_block mem4 in
        let s2 = ValueSet.bottom_set |> ValueSet.add_memory_block mem4 in
        let met = ValueSet.meet s1 s2 in
        print_endline ("Meet of " ^ ValueSet.to_string s1 ^ " and " ^ ValueSet.to_string s2 ^ " = " ^ ValueSet.to_string met);
        String.equal (ValueSet.to_string met) (ValueSet.to_string s1)
    end)
  end


  (* D'autres tests suivront *)
end
