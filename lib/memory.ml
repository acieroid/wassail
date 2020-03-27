open Core_kernel

module Map = Map.Make(Value)
type t = Value.t Map.t
[@@deriving sexp, compare]

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
  Map.update m ea ~f:(function
      | None -> v
      | Some v' -> Value.join v v')

let load (m : t) (addr : Value.t) (op : Memoryop.t) : Value.t =
  assert (op.sz = None); (* We only support N = 32 for now. *)
  let ea = Value.add_offset addr op.offset in (* effective address *)
  (* v is the join of all values that could be stored at ea *)
  let v = List.fold_left ~init:Value.bottom ~f:(fun v1 (_, v2) -> Value.join v1 v2)
      (Map.to_alist (Map.filter m ~f:(fun a -> Value.compare a ea = 0 (* TODO: subsumes, not compare *)))) in
  if v = Value.bottom then
    Value.deref addr
  else
    v

let store (m : t) (addr : Value.t) (value : Value.t) (op : Memoryop.t) : t =
  assert (op.sz = None); (* We only support N = 32 for now. *)
  let ea = Value.add_offset addr op.offset in
  (* TODO: what about overlapping values? e.g., M[0: 0][[0,1]: 1].
     Possible solution: find the most general key, join the values
     Hence, M[0: 0][[0,1]: 1] becomes M[[0,1]: [0,1]]
  *)
  update m ea value

let to_string (m : t) : string =
  Printf.sprintf "M%s" (String.concat ~sep:"" (List.map (Map.to_alist m) ~f:(fun (a, v) ->
      Printf.sprintf "[%s: %s]" (Value.to_string a) (Value.to_string v))))

let join (m1 : t) (m2 : t) : t =
  (* M[a: b] joined with M[c: d] should be just like doing M[a: b][c: d] *)
  Printf.printf "STORE JOINING\n";
  Map.fold m2 ~init:m1 ~f:(fun ~key:addr ~data:value m -> update m addr value)
