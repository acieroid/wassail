open Core_kernel

module T = struct
  type funcelem = Var.t option
  [@@deriving sexp, compare]

  type t = {
    elems : funcelem Array.t; (** The elements in the table *)
    max : Int32.t option; (** Maximal size of the table. If None, it is unlimited *)
  }
  [@@deriving sexp, compare]
end
include T

let init (t : Table.t) (elems : Elem.t list) : t =
  let table = { max = snd t.limits;
                elems = Wasm.Lib.Array32.make (fst t.limits) None } in
  List.iter elems ~f:(fun e ->
      assert (e.index = 0);
      match e.offset with
      | { instr = Data (Const ({ value = Symbolic (Const (I32 offset)); _ })); _ } :: [] ->
        List.iteri e.init ~f:(fun idx addr ->
            Wasm.Lib.Array32.set table.elems Int32.((of_int_exn idx) + offset) (Some addr))
      | _ -> failwith "Unsupported elems for table initialization"
    );
  table

let get (t : t) (idx : Int32.t) : funcelem =
  Wasm.Lib.Array32.get t.elems idx

let get_subsumed_by_index (t : t) (idx : Value.t) : funcelem list =
  let rec loop (i : Int32.t) (acc : Int32.t list) : Int32.t list =
    Printf.printf "index: %d\n" (Int32.to_int_exn i);
    if Int32.(i >= Wasm.Lib.Array32.length t.elems) then
      acc
    else begin
      Printf.printf "%s subsumes %s? %b\n" (Value.to_string idx) (Value.to_string (Value.i32_const i)) (Value.subsumes idx (Value.i32_const i));
      loop Int32.(i + 1l) (if Value.subsumes idx (Value.i32_const i) then i :: acc else acc)
    end
  in
  let indices = loop 0l [] in
  List.map indices ~f:(get t)

let to_string (t : t) : string =
  Printf.sprintf "elems: %s\n" (String.concat ~sep:","
                                  (List.map (Array.to_list t.elems)
                                     ~f:(function
                                         | None -> "/"
                                         | Some v -> string_of_int v)))
