open Core_kernel

type t = (Value.t * Value.t) list
[@@deriving sexp, compare, yojson]

let initial = []

let load (m : t) (addr : Value.t) (op : Memoryop.t) : Value.t =
  assert (op.sz = None); (* We only support N = 32 for now. *)
  let ea = Value.add_offset addr op.offset in
  let v = List.fold_left ~init:Value.bottom ~f:(fun v1 (_, v2) -> Value.join v1 v2)
      (List.filter m ~f:(fun (a, _) -> Value.compare a ea = 0 (* TODO: subsumes, not compare *))) in
  if v = Value.bottom then
    Value.deref addr
  else
    v

let store (m : t) (addr : Value.t) (value : Value.t) (op : Memoryop.t) : t =
  assert (op.sz = None); (* We only support N = 32 for now. *)
  let ea = Value.add_offset addr op.offset in
  (ea, value) :: m (* TODO: overlapping values *)

let to_string (m : t) : string =
  Printf.sprintf "M%s" (String.concat ~sep:"" (List.map m ~f:(fun (a, v) ->
      Printf.sprintf "[%s: %s]" (Value.to_string a) (Value.to_string v))))

let join (m1 : t) (m2 : t) : t = m1 @ m2 (* TODO: what about overlapping values? *)
