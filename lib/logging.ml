open Core_kernel

type option =
  | WarnMemJoin
  | WarnImpreciseOp
  | WarnSubsumesIncorrect
  | WarnNotImplemented

let enabled_options = ref [WarnMemJoin; WarnImpreciseOp; WarnSubsumesIncorrect; WarnNotImplemented]

let info (msg : string) : unit =
  Printf.printf "[INFO] %s\n" msg

let warn (opt : option) (msg : unit -> string) : unit =
  if List.mem !enabled_options opt ~equal:Pervasives.(=) then
    Printf.printf "[WARNING] %s\n" (msg ())
  else
    ()
