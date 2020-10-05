open Core_kernel

(** The types of logged items *)
type option =
  | Info
  | Warn of string
[@@deriving sexp, compare, equal]

let option_to_string (opt : option) =
  match opt with
  | Info -> "INFO"
  | Warn s -> (Printf.sprintf "WARN:%s" s)

type callback = option -> string -> unit

let callbacks : (callback list) ref = ref []

let log (opt : option) (enabled: bool) (msg : string) : unit =
  if enabled then
    List.iter !callbacks ~f:(fun cb -> cb opt (Printf.sprintf "%s\n" msg))

(** Adds a callback that will be called when something is logged *)
let add_callback (cb : option -> string -> unit) =
  callbacks := cb :: !callbacks

(** Logs some information *)
let info (enabled : bool) (msg : string) : unit = log Info enabled msg

(** Logs a warning *)
let warn (kind : string) (enabled : bool) (msg : string) : unit = log (Warn kind) enabled msg
