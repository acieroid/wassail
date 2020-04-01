open Core_kernel

type option =
  | WarnMemJoin
  | WarnImpreciseOp
  | WarnSubsumesIncorrect
  | WarnNotFoundInMem
  | WarnNotImplemented
  | WarnCannotSimplify
  | WarnUnsoundAssumption

let enabled_options = ref [WarnMemJoin; WarnImpreciseOp; WarnSubsumesIncorrect; WarnNotImplemented; WarnNotFoundInMem; WarnCannotSimplify; WarnUnsoundAssumption]

let info (msg : string) : unit =
  Printf.printf "[INFO] %s\n" msg

let warn (opt : option) (msg : unit -> string) : unit =
  if List.mem !enabled_options opt ~equal:Pervasives.(=) then
    Printf.printf "[WARNING] %s\n" (msg ())
  else
    ()
