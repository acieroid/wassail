type message = unit -> string

let warn (message : message) : unit =
  Printf.printf "[WARNING] %s\n%!" (message ())

let error (message : message) : unit =
  Printf.printf "[ERROR] %s\n%!" (message ())

let result (message : message) : unit =
  Printf.printf "[RESULT] %s\n%!" (message ())

let imprecision (message : message) : unit =
  Printf.printf "[IMPRECISION] %s\n%!" (message ())

let info_enabled = ref false
let info (message : message) : unit =
  if !info_enabled then
    Printf.printf "[INFO] %s%!\n" (message ())
let enable_info () = info_enabled := true

let debug_enabled = ref false
let enable_debug () = debug_enabled := true
let debug (message : message) : unit =
  if !debug_enabled then
    Printf.printf "[DEBUG] %s\n%!" (message ())
