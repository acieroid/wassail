open Core_kernel

type option =
  | WarnMemJoin
  | WarnImpreciseOp
  | WarnSubsumesIncorrect

let enabled_options = ref [WarnMemJoin; WarnImpreciseOp; WarnSubsumesIncorrect]

let warn (opt : option) (msg : unit -> string) : unit =
  if List.mem !enabled_options opt ~equal:Pervasives.(=) then
    Printf.printf "[WARNING] %s\n" (msg ())
  else
    ()
