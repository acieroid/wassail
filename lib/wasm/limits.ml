open Core

type t = Int32.t * (Int32.t option)
[@@deriving sexp, compare, equal]

let of_wasm (l : Int32.t Wasm.Types.limits) : t =
  (l.min, l.max)

let min (l : t) : Int32.t = fst l

let max (l : t) : Int32.t option = snd l
