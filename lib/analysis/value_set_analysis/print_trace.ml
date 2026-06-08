(** [print fmt] prints [fmt] to stdout when tracing is enabled.
    Otherwise, the output is discarded using [Printf.ifprintf]. *)
let print fmt =
  if !Value_set_options.print_trace then
    Printf.printf fmt
  else
    Printf.ifprintf stdout fmt