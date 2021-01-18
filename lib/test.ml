open Core_kernel
module type EqualT = sig
  type t
  [@@deriving equal]
  val to_string : t -> string
end

module type SetT = sig
  include Set.S
  val to_string : t -> string
end

(** Module that provides helper functions to use within tests *)
module Helpers(T : EqualT) = struct
  (** Check equality and print both elements if they are not equal. *)
  let check_equality ~expected:(expected : T.t) ~actual:(actual : T.t) : bool =
    let eq = T.equal actual expected in
    begin if not eq then
        Printf.printf "not equal:\n\tactual: %s\n\texpected: %s\n" (T.to_string actual) (T.to_string expected)
    end;
    eq
end

module HelpersForSet(T : SetT) = struct
  let check_equality ~expected:(expected : T.t) ~actual:(actual : T.t) : bool =
    let eq = T.equal actual expected in
    begin if not eq then
        let diff = T.diff actual expected in
        Printf.printf "not equal:\n\tactual: %s\n\texpected: %s\ndiff:\t%s\n" (T.to_string actual) (T.to_string expected) (T.to_string diff)
    end;
    eq
end
