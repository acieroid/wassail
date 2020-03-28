open Core_kernel

module Map = Map.Make(Value)
type t = Value.t Map.t
[@@deriving sexp, compare]


let to_string (m : t) : string =
  Printf.sprintf "M%s" (String.concat ~sep:"" (List.map (Map.to_alist m) ~f:(fun (a, v) ->
      Printf.sprintf "[%s: %s]" (Value.to_string a) (Value.to_string v))))

let to_yojson (m : t) = Map.to_alist m
                        |> [%to_yojson: (Value.t * Value.t) list]
let of_yojson json = match [%of_yojson: (Value.t * Value.t) list] json with
  | Ok a -> begin match Map.of_alist a with
      | `Duplicate_key v -> Error (Printf.sprintf "Memory.of_yojson: duplicate key %s" (Value.to_string v))
      | `Ok v -> Ok v
    end
  | Error err -> Error err


let initial = Map.empty

let update (m : t) (ea : Value.t) (v : Value.t) : t =
  (* TODO: what about overlapping values? e.g., M[0: 0][[0,1]: 1].
     Possible solution: find the most general key, join the values
     Hence, M[0: 0][[0,1]: 1] becomes M[[0,1]: [0,1]]
     Other: split when possible:
     M[[[0,3]: 1][[1,5]: 2] becomes M[[0,0]: 1][[1,3]: [1,2]][[4,5]: 2]
  *)
  Map.update m ea ~f:(function
      | None -> v
      | Some v' -> Value.join v v')

let find (m : t) (ea : Value.t) : Value.t =
  (* Step 1. Find all values that subsume ea, or are subsumed by ea
     Example: find M[[[0,5]: 3][[1,3]: 0] 4 returns M[[0,5]: 3]
              find M[1: 2] [0,5] returns M[1: 2]
     Step 2. Join all the values.
     Step 3. If there are addresses subsumed by ea, but that do not subsume ea (e.g., second example above): join with top *)
  let m' = Map.filter m ~f:(fun a -> Value.subsumes a ea || Value.subsumes ea a) in
  let v = Map.fold ~init:Value.bottom ~f:(fun ~key:_ ~data:v acc -> Value.join acc v) m' in
  if (Map.exists m' ~f:(fun a -> Value.subsumes ea a)) then
    Value.join v (Value.top (Printf.sprintf "Memory.find %s %s" (to_string m) (Value.to_string ea)))
  else
    v

let load (m : t) (addr : Value.t) (op : Memoryop.t) : Value.t =
  assert (op.sz = None); (* We only support N = 32 for now. *)
  let ea = Value.add_offset addr op.offset in (* effective address *)
  let v = find m ea in (* TODO: find should return an option, rather than bottom, if value not found *)
  if v = Value.bottom then
    Value.deref addr
  else
    v

let store (m : t) (addr : Value.t) (value : Value.t) (op : Memoryop.t) : t =
  assert (op.sz = None); (* We only support N = 32 for now. *)
  let ea = Value.add_offset addr op.offset in
  update m ea value

let join (m1 : t) (m2 : t) : t =
  (* M[a: b] joined with M[c: d] should be just like doing M[a: b][c: d] *)
  Logging.warn WarnMemJoin (fun () -> Printf.sprintf "join %s with %s" (to_string m1) (to_string m2));
  Map.fold m2 ~init:m1 ~f:(fun ~key:addr ~data:value m -> update m addr value)
