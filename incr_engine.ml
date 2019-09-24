open Core
module Incr = Incremental_kernel.Incremental.Make ()
module Let_syntax = Incr.Let_syntax
(* module IncrMap = Incr_map.Make(Incr) *)

type 'a t = 'a Incr.t

  module Stats : sig
    val reporter : unit -> (unit -> unit) Staged.t
  end = struct
    type t =
      { created : int
      ; recomputed : int
      ; changed : int
      }
        [@@deriving sexp]


    let diff t1 t2 =
      { created = t1.created - t2.created
      ; recomputed = t1.recomputed - t2.recomputed
      ; changed = t1.changed - t2.changed
      }
    ;;

    let snap () =
      { created = Incr.State.num_nodes_created Incr.State.t
      ; recomputed = Incr.State.num_nodes_recomputed Incr.State.t
      ; changed = Incr.State.num_nodes_changed Incr.State.t
      }
    ;;

    let reporter () =
      let old_stats = ref (snap ()) in
      let report () =
        let stats = snap () in
        print_s [%sexp (diff stats !old_stats : t)];
        old_stats := stats
      in
      stage report
    ;;
  end

  let report = unstage (Stats.reporter ())

  let (:=) = Incr.Var.set
  let (!) = Incr.Var.value
