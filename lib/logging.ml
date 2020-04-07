open Core_kernel

type option =
  | Info
  | Warn of string
[@@deriving sexp, compare]

let option_to_string (opt : option) =
  match opt with
  | Info -> "INFO"
  | Warn s -> (Printf.sprintf "WARN:%s" s)

type callback = option -> string -> unit

let callbacks : (callback list) ref = ref []

let add_callback (cb : option -> string -> unit) =
  callbacks := cb :: !callbacks

let log (opt : option) (msg : string) : unit =
  List.iter !callbacks ~f:(fun cb -> cb opt (Printf.sprintf "%s\n" msg))

let info (msg : string) : unit = log Info msg

let warn (kind : string) (msg : string) : unit = log (Warn kind) msg

