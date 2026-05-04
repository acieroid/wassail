

let print fmt =
  if !Value_set_options.print_trace then
    Printf.printf fmt
  else
    Printf.ifprintf stdout fmt