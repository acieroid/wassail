open Core_kernel

let warn_imprecise (source : string) : unit =
  Printf.printf "[WARNING] Imprecise operation: %s\n" source
