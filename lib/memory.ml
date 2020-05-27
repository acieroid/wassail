open Core_kernel

module Map = Map.Make(Value.ValueT)

(** The memory is a map from values (without types) to values *)
type t = Value.t Map.t
[@@deriving sexp, compare]

let to_string (m : t) : string =
  Printf.sprintf "M%s" (String.concat ~sep:"" (List.map (Map.to_alist m) ~f:(fun (a, v) ->
      Printf.sprintf "[%s: %s]" (Value.value_to_string a) (Value.to_string v))))

(** The initial memory is empty *)
let initial = Map.empty

(** Look up a value in the memory at effective address ea. Returns either the value (Some v), or None if the value is not directly found in the memory (meaning it could be any value) *)
let find (m : t) (ea : Value.value) : Value.t option =
  (* Step 1. Find all values that subsume ea, or are subsumed by ea
     Example: find M[[[0,5]: 3][[1,3]: 0] 4 returns M[[0,5]: 3]
              find M[1: 2] [0,5] returns M[1: 2]
     Step 2. Join all the values.
     Step 3. If there are addresses subsumed by ea, but that do not subsume ea (e.g., second example above): join with top *)
  let m' = Map.filteri m ~f:(fun ~key:a ~data:_ -> Value.value_subsumes a ea || Value.value_subsumes ea a) in
  Logging.info (Printf.sprintf "Memory.find %s %s" (to_string m) (Value.value_to_string ea));
  let vopt = Map.fold m' ~init:None ~f:(fun ~key:_ ~data:v acc ->
      match acc with
      | Some v' -> Some (Value.join v v')
      | None -> Some v) in
  match vopt with
  | Some v ->
    if (Map.existsi m' ~f:(fun ~key:a ~data:_ -> Stdlib.(ea <> a) && Value.value_subsumes ea a)) then
      Some (Value.join v (Value.top (Printf.sprintf "Top created at memory.find %s %s" (to_string m) (Value.value_to_string ea))))
    else
      Some v
  | None ->
    Logging.warn "ValueNotFoundInMemory" (Printf.sprintf "value %s at address %s" (to_string m) (Value.value_to_string ea));
    None

let rec resolve_value (m : t) (v : Value.value) : Value.value =
  Printf.printf "Trying to resolve %s\n" (Value.value_to_string v);
  match v with
  | Interval (a, b) -> Value.Interval (resolve_symbolic m a, resolve_symbolic m b)
  | LeftOpenInterval a -> Value.LeftOpenInterval (resolve_symbolic m a)
  | RightOpenInterval b -> Value.RightOpenInterval (resolve_symbolic m b)
  | Symbolic (Deref a) -> begin match find m a with
      | Some v' -> v'.value
      | None -> (Symbolic (Deref (resolve_value m a)))
    end
  | Symbolic sym -> Symbolic (resolve_symbolic m sym)
  | _ -> v
and resolve_symbolic (m : t) (sym : Value.symbolic) : Value.symbolic =
  Printf.printf "Trying to resolve symbolic: %s\n" (Value.symbolic_to_string sym);
  match sym with
  | Op (op, left, right) -> Op (op, resolve_value m left, resolve_value m right)
  | Deref _ -> failwith "Memory.resolve_symbolic unsupported for Deref"
  | _ -> sym

(** Resolves the pointers that are seen in v, if possible (if they are valid addresses in the memory) *)
let resolve (m : t) (v : Value.t) : Value.t =
  { value = resolve_value m v.value; typ = v.typ }

(** Update an address in the memory, joining it with the previous value if there was already one stored at the same address *)
let update (m : t) (ea : Value.value) (v : Value.t) : t =
  (* TODO: what about overlapping values? e.g., M[0: 0][[0,1]: 1].
     Possible solution: find the most general key, join the values
     Hence, M[0: 0][[0,1]: 1] becomes M[[0,1]: [0,1]]
     Other: split when possible:
     M[[[0,3]: 1][[1,5]: 2] becomes M[[0,0]: 1][[1,3]: [1,2]][[4,5]: 2]
  *)
  let resolved_v = Value.{ value = resolve_value m v.value; typ = v.typ } in
  begin if Stdlib.(resolved_v <> v) then
    Printf.printf "Memory.update using %s instead of %s [ea: %s]\n" (Value.to_string resolved_v) (Value.to_string v) (Value.value_to_string ea)
  end;
  Map.update m ea ~f:(function
      | None -> v
      | Some v' -> Value.join resolved_v v')

(** Load a byte from memory, at effective address ea *)
let load_byte (m : t) (ea : Value.value) : Value.value =
  match find m ea with
  | Some v -> v.value
  | None -> (Value.deref ea).value

(** Load a value from the memory m, stored at address addr. Argument op
   indicates, among others, the size of the value loaded.
 *)
let load (m : t) (addr : Value.value) (op : Memoryop.t) : Value.t =
  Printf.printf "load %s %s\n" (Value.value_to_string addr) (Memoryop.to_string op);
  let ea = Value.add_offset addr op.offset in (* effective address *)
  match op.sz with
  | None -> (* load instruction *)
    (* N unspecified, loads 4 bytes for an i32, 8 for i64
       Reference interpreter: memory.ml/load_value *)
    begin match op.typ with
      | I32 ->
        Value.symbolic op.typ
          (* Put all bytes together, the rightmost byte to the right of Bytes4 *)
          (Value.Bytes4 (load_byte m (Value.add_offset ea 3), load_byte m (Value.add_offset ea 2),
                         load_byte m (Value.add_offset ea 1), load_byte m ea))
      | _ -> failwith "unsupported: load with non-i32"
    end
  | Some Memoryop.(Pack8, ZX) -> (* load8_u *)
    { value = load_byte m ea; typ = op.typ }
  | Some Memoryop.(Pack8, SX) -> (* load8_s *)
    (* Just like ZX, but does extend the value through two shifts, see reference implementation: memory.ml/extend *)
    let v = load_byte m ea in
    Logging.warn "Unsigned" (Printf.sprintf "unsigned to signed conversion not performed for value %s" (Value.value_to_string v));
    { value = v; typ = op.typ }
  | Some Memoryop.(Pack16, SX) -> (* load16_s *)
    failwith "NYI: load16_s"
  | Some Memoryop.(Pack16, ZX) -> (* load16_u *)
    failwith "NYI: load16_u"
  | Some Memoryop.(Pack32, SX) -> (* load32_s *)
    failwith "NYI: load32_s"
  | Some Memoryop.(Pack32, ZX) -> (* load32_u *)
    failwith "NYI: load32_u"

let store (m : t) (addr : Value.value) (value : Value.t) (op : Memoryop.t) : t =
  let ea = Value.add_offset addr op.offset in
  match op.sz with
  | None -> (* store instruction *)
    begin match op.typ with
      | I32 ->
        update
          (update
            (update
               (update m ea (Value.symbolic op.typ (Value.Byte (value.value, 0))))
               (Value.add_offset ea 1) (Value.symbolic op.typ (Value.Byte (value.value, 1))))
            (Value.add_offset ea 2) (Value.symbolic op.typ (Value.Byte (value.value, 2))))
          (Value.add_offset ea 3) (Value.symbolic op.typ (Value.Byte (value.value, 3)))
      | I64 ->
        update
          (update
             (update
                (update
                   (update
                      (update
                         (update
                            (update m ea (Value.symbolic op.typ (Value.Byte (value.value, 0))))
                            (Value.add_offset ea 1) (Value.symbolic op.typ (Value.Byte (value.value, 1))))
                         (Value.add_offset ea 2) (Value.symbolic op.typ (Value.Byte (value.value, 2))))
                      (Value.add_offset ea 3) (Value.symbolic op.typ (Value.Byte (value.value, 3))))
                   (Value.add_offset ea 4) (Value.symbolic op.typ (Value.Byte (value.value, 4))))
                (Value.add_offset ea 5) (Value.symbolic op.typ (Value.Byte (value.value, 5))))
             (Value.add_offset ea 6) (Value.symbolic op.typ (Value.Byte (value.value, 6))))
          (Value.add_offset ea 7) (Value.symbolic op.typ (Value.Byte (value.value, 7)))
      | _ -> failwith "unsupported: store with floats"
    end
  | Some Memoryop.(Pack8, _) -> (* store8 *)
    (* We should in practice only keep the first byte, but we ignore that *)
    update m ea value
  | _ -> failwith (Printf.sprintf "NYI: store with op as %s" (Memoryop.to_string op))

let join (m1 : t) (m2 : t) : t =
  (* M[a: b] joined with M[c: d] should be just like doing M[a: b][c: d] *)
  let m = Map.fold m2 ~init:m1 ~f:(fun ~key:addr ~data:value m -> update m addr value) in
  Logging.warn "Join" (Printf.sprintf "join %s with %s gives %s" (to_string m1) (to_string m2) (to_string m));
  m

(** Adapt (both in the addresses and in the values), using the map given as argument
    1. globals, e.g., M[g0: a], with map: [g0: X] becomes M[X: a]
    2. parameters are mapped, e.g., M[A: p0] with map: [p0: X] becomes M[A: X]
    3. Is it enough or do we need anything else? (TODO)
*)
let adapt (m : t) (map : Value.ValueValueMap.t) : t =
  Map.of_alist_exn (List.map (Map.to_alist m)
                  ~f:(fun (a, v) -> (Value.adapt_value a map, Value.adapt v map)))

(** Refines the value at M[ea], by meeting it with v *)
let refine_value_at (m : t) (ea : Value.value) (v : Value.t) =
  Map.update m ea ~f:(function
      | None ->
        Logging.warn "NotFoundInMemory" (Printf.sprintf "refine_value_at should have found a value at address %s, none found instead, in memory %s" (Value.value_to_string ea) (to_string m));
        v
      | Some v' -> Value.meet v v')

(** Refine the memory based on a condition cond (represented as a value), that either holds or not, according to holds *)
let refine (m : t) (cond : Value.t) (holds : bool) : t=
  match (cond.value, holds) with
  | Symbolic (Op (GtE, Symbolic (Deref ea), Symbolic b)), true ->
    (* M[*ea: X] with cond *ea >= b refines X with [b,+inf] *)
    refine_value_at m ea Value.{ value = (RightOpenInterval b); typ = I32 } (* TODO: typ *)
  | Symbolic (Op (GtE, Symbolic (Deref ea), Symbolic b)), false ->
    (* M[*ea: X] with cond *ea >= b (false -> *ea < b) refines X with ]-inf,b-1] *)
    refine_value_at m ea Value.{ value = LeftOpenInterval (Value.simplify_symbolic (Op (Minus, Symbolic b, (i32_const 1l).value))); typ = I32 } (* TODO: typ *)
  | _ ->
    Logging.warn "ImpreciseOperation" (Printf.sprintf "cannot refine memory %s based on condition %s" (to_string m) (Value.to_string cond));
    m
