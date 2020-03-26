open Core_kernel

type t = unit
[@@deriving sexp, compare]

let initial = ()

let load (_m : t) (_addr : Value.t)  = failwith "TODO"

let store (_m : t) (_addr : Value.t) (_value : Value.t) = failwith "TODO"

let to_string (_m : t) : string = "M"

let join (m1 : t) (_m2 : t) : t = m1

