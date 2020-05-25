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
      | Data (Const ({ value = Symbolic (Const (I32 offset)); typ = _ })) :: [] ->
        List.iteri e.init ~f:(fun idx addr ->
            Wasm.Lib.Array32.set table.elems Int32.((of_int_exn idx) + offset) (Some addr))
      | _ -> failwith "Unsupported elems for table initialization"
    );
  table

let get (t : t) (idx : Int32.t) : funcelem =
  Wasm.Lib.Array32.get t.elems idx
