open Core_kernel

module T = struct
  type funcelem = Int32.t option
  [@@deriving sexp, compare, equal]

  type t = {
    elems : funcelem Array.t; (** The elements in the table *)
    max : Int32.t option; (** Maximal size of the table. If None, it is unlimited *)
  }
  [@@deriving sexp, compare, equal]
end
include T

let init (t : Table.t) (elems : Elem.t list) : t =
  let table = { max = Limits.max t.ttype;
                elems = Wasm.Lib.Array32.make (Limits.min t.ttype) None } in
  List.iter elems ~f:(fun e ->
      assert Int32.(e.index = 0l);
      match e.offset with
      | Data { instr = Const (I32 offset); _} :: [] ->
        List.iteri e.init ~f:(fun idx addr ->
            Wasm.Lib.Array32.set table.elems Int32.((of_int_exn idx) + offset) (Some addr))
      | _ -> failwith "Unsupported elems for table initialization"
    );
  table

let indices (t : t) : Int32.t list =
  List.init (Array.length t.elems) ~f:(fun i -> Int32.of_int_exn i)

let get (t : t) (idx : Int32.t) : funcelem =
  Array.get t.elems (Int32.to_int_exn idx)

let to_string (t : t) : string =
  Printf.sprintf "elems: %s\n" (String.concat ~sep:","
                                  (List.map (Array.to_list t.elems)
                                     ~f:(function
                                         | None -> "/"
                                         | Some v -> Int32.to_string v)))
