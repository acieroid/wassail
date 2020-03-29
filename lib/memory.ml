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

(** Look up a value in the store at effective address ea. Returns either the value (Some v), or None if the value is not directly found in the store (meaning it could be any value) *)
let find (m : t) (ea : Value.t) : Value.t option =
  (* Step 1. Find all values that subsume ea, or are subsumed by ea
     Example: find M[[[0,5]: 3][[1,3]: 0] 4 returns M[[0,5]: 3]
              find M[1: 2] [0,5] returns M[1: 2]
     Step 2. Join all the values.
     Step 3. If there are addresses subsumed by ea, but that do not subsume ea (e.g., second example above): join with top *)
  let m' = Map.filteri m ~f:(fun ~key:a ~data:_ -> Value.subsumes a ea || Value.subsumes ea a) in
  Logging.info (Printf.sprintf "Memory.find %s %s" (to_string m) (Value.to_string ea));
  let vopt = Map.fold m' ~init:None ~f:(fun ~key:_ ~data:v acc ->
      match acc with
      | Some v' -> Some (Value.join v v')
      | None -> Some v) in
  match vopt with
  | Some v ->
    if (Map.existsi m' ~f:(fun ~key:a ~data:_ -> ea <> a && Value.subsumes ea a)) then
      Some (Value.join v (Value.top (Printf.sprintf "Top created at memory.find %s %s" (to_string m) (Value.to_string ea))))
    else
      Some v
  | None ->
    Logging.warn WarnNotFoundInMem (fun () -> Printf.sprintf "value not found in store (%s) at address %s" (to_string m) (Value.to_string ea));
    None

let load (m : t) (addr : Value.t) (op : Memoryop.t) : Value.t =
  assert (op.sz = None); (* We only support N = 32 for now. *)
  let ea = Value.add_offset addr op.offset in (* effective address *)
  match find m ea with
  | Some v -> v
  | None -> Value.deref addr

let store (m : t) (addr : Value.t) (value : Value.t) (op : Memoryop.t) : t =
  assert (op.sz = None); (* We only support N = 32 for now. *)
  let ea = Value.add_offset addr op.offset in
  update m ea value

let join (m1 : t) (m2 : t) : t =
  (* M[a: b] joined with M[c: d] should be just like doing M[a: b][c: d] *)
  Logging.warn WarnMemJoin (fun () -> Printf.sprintf "join %s with %s" (to_string m1) (to_string m2));
  Map.fold m2 ~init:m1 ~f:(fun ~key:addr ~data:value m -> update m addr value)
