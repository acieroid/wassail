open Core_kernel

(* Representation of the memory using separation logic *)
type byte =
  | ByteInValue of (Value.t * int) (* (val, byte_position) *)
[@@deriving sexp, compare]
let byte_to_string (b : byte) : string = match b with
  | ByteInValue (v, b) -> begin match v.value with
      | Value.Bottom -> "Bottom"
      | Value.Const n -> Printf.sprintf "%d[%d]" (Option.value_exn (Int32.to_int n)) b
      | Value.Int -> Printf.sprintf "%s[%d]" (Value.sources_to_string v.sources) b
    end

type formula = (byte * byte) list
[@@deriving sexp, compare]
let formula_to_string (f : formula) : string = match f with
  | [] -> "emp"
  | _ -> String.concat ~sep:" * " (List.map ~f:(fun (b1, b2) -> Printf.sprintf "%s -> %s" (byte_to_string b1) (byte_to_string b2)) f)

let maps_to (f : formula) (b : byte) = Option.map ~f:(fun (_, b2) -> b2) (List.find f ~f:(fun (b1, _) -> compare_byte b b1 = 0))

let formula_mapsto_4bytes (f : formula) (i : Value.t) (offset : int) : Value.t option =
  match (maps_to f (ByteInValue (i, offset)),
         maps_to f (ByteInValue (i, offset + 1)),
         maps_to f (ByteInValue (i, offset + 2)),
         maps_to f (ByteInValue (i, offset + 3))) with
  | Some (ByteInValue (c0, _)), Some (ByteInValue (c1, _)), Some (ByteInValue (c2, _)), Some (ByteInValue (c3, _))
    when Value.compare c0 c1 = 0 && Value.compare c1 c2 = 0 && Value.compare c2 c3 = 0 -> Some c0
  | None, None, None, None -> None
  | _ -> failwith "TODO: formula_mapsto_4bytes"
let add_mapsto (f : formula) (b1 : byte) (b2 : byte) = (b1, b2) :: (List.filter ~f:(fun (b1', _) -> compare_byte b1 b1' <> 0) f)


type t = formula
[@@deriving sexp, compare]
let to_string (m : t) : string = formula_to_string m


let join (m1 : t) (m2 : t) : t =
  (* TODO: if m2 redefines elements from m1, fail? or do what? *)
  List.fold_left ~init:m1 ~f:(fun f (b1, b2) -> add_mapsto f b1 b2) m2
