open Core_kernel

module Map = Map.Make(Value)

(** The memory is a map from values to bytes *)
type t = Value.byte Map.t
[@@deriving sexp, compare]

let to_string (m : t) : string =
  Printf.sprintf "M%s" (String.concat ~sep:"" (List.map (Map.to_alist m) ~f:(fun (a, b) ->
      Printf.sprintf "[%s: %s]" (Value.to_string a) (Value.byte_to_string b))))

(** The initial memory is empty *)
let empty = Map.empty

(** Stores one byte in memory, at address ea *)
let store8 (m : t) (ea : Value.t) (b : Value.byte) : t =
  Map.update m ea ~f:(function
      | None -> b
      | Some _b' -> (* strong update. TODO: is it safe? *) b)

(** Stores multiple bytes in memory
    @param vs is the list of (address, byte) to store *)
let store (m : t) (vs : (Value.t * Value.byte) list) : t =
  (* TODO: should check for precise equality: what happens if we store twice at the same location? *)
  (* TODO: or rather, we should follow an approach similar to what is described in notes/VMCAI2019.md *)
  List.fold_left vs ~init:m ~f:(fun m (ea, b) -> store8 m ea b)

let load8 (m : t) (ea : Value.t) (precisely_eq : Value.t -> Value.t -> bool) : Value.byte option =
  Map.fold m ~init:None ~f:(fun ~key:addr ~data:v res ->
      if precisely_eq addr ea then begin
        assert Stdlib.(res = None);
        Some v
      end else
        res)

(** Joins two memories by returning the "most general memory"
    For example, consider M[x: y] joined with M: we could return M[x: y] where y gets assigned top.
     That's definitely sound: M's interpretation is that M maps everything to top, M[x: y] maps x to y.
     Hence, joining M with M[x: y] will have to join y with top.
     However, another way of doing that is to return M ! M itself is the most general memory between M[x: y] and M *)
let join (m1 : t) (m2 : t) : t =
  (* The most general memory is found by keeping only the set of keys that are present in both memory maps. *)
  let keys1 = Map.key_set m1 in
  let keys2 = Map.key_set m2 in
  let intersection = Base.Set.inter keys1 keys2 in
  Base.Set.fold intersection ~init:empty ~f:(fun m k ->
      match Map.find m1 k, Map.find m2 k with
      | None, None
      | Some _, None
      | None, Some _ -> m (* present in none, or only one of the memories: don't keep it *)
      | Some v1, Some v2 ->
        (* present in both memories, only keep it if it is the same value *)
        if Stdlib.(v1 = v2) then
          Map.set m ~key:k ~data:v1
        else
          m)
