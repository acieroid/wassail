let warn (message : string) : unit =
  Printf.printf "[WARNING]: %s" message

let info_enabled = ref false
let info (message : string) : unit =
  if !info_enabled then
    Printf.printf "[INFO]: %s" message
