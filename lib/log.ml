let warn (message : unit -> string) : unit =
  Printf.printf "[WARNING] %s\n%!" (message ())

let error (message : unit -> string) : unit =
  Printf.printf "[ERROR] %s\n%!" (message ())

let result (message : unit -> string) : unit =
  Printf.printf "[RESULT] %s\n%!" (message ())

let imprecision (message : unit -> string) : unit =
  Printf.printf "[IMPRECISION] %s\n%!" (message ())

let info_enabled = ref false
let info (message : unit -> string) : unit =
  if !info_enabled then
    Printf.printf "[INFO] %s\n%!" (message ())
let enable_info () = info_enabled := true

let debug_enabled = ref false
let enable_debug () = debug_enabled := true
let debug (message : unit -> string) : unit =
  if !debug_enabled then
    Printf.printf "[DEBUG] %s\n%!" (message ())
