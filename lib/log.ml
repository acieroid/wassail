let warn (message : string) : unit =
  Printf.printf "[WARNING]: %s\n" message

let info_enabled = ref false
let info (message : string) : unit =
  if !info_enabled then
    Printf.printf "[INFO]: %s\n" message

let debug_enabled = ref false
let debug (message : string) : unit =
  if !debug_enabled then
    Printf.printf "[DEBUG]: %s\n" message
