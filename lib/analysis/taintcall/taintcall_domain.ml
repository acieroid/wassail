open Core
open Helpers

module Call = struct
  (** The calls taints, as a map of functions to the taints with which they are called.
      The domain is a list of taints, to match the arguments passed to the function. *)
  type t = Taint_domain.Taint.t list Int32Map.t
  [@@deriving sexp, compare, equal]

  let bottom : t = Int32Map.empty

  let to_string (t : t) : string =
    Int32Map.to_string t (fun l -> String.concat ~sep:";" (List.map l ~f:Taint_domain.Taint.to_string))

  let join (t1 : t) (t2 : t) : t =
    Int32Map.merge t1 t2 ~f:(fun ~key:_ -> function
        | `Left a -> Some a
        | `Right b -> Some b
        | `Both (a, b) -> Some (List.map2_exn a b ~f:Taint_domain.Taint.join))

  let widen (_t1 : t) (t2 : t) : t = t2

end

include Product_domain.Make(Call)(Taint_domain)
