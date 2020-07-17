open Core_kernel
open Helpers

(** A taint summary *)
type t = {
  ret : Taint_domain.taint option; (** The taint of the (optional) return value *)
  globals : Taint_domain.taint list; (** The taint of the globals after applying the function *)
  (* TODO: mem *)
}
[@@deriving sexp, compare, equal]

type state = Taint_domain.t

let to_string (s : t) : string =
  Printf.sprintf "stack: [%s], globals: [%s]"
    (String.concat ~sep:";"
       (List.map ~f:Taint_domain.taint_to_string
          (Option.to_list s.ret)))
    (String.concat ~sep:";"
       (List.map ~f:Taint_domain.taint_to_string s.globals))

let bottom (cfg : Cfg.t) (_vars : Var.t list) : t =
  let globals = List.map cfg.global_types ~f:(fun _ -> Taint_domain.taint_bottom) in
  let ret = match cfg.return_types with
      | [] -> None
      | _ :: [] -> Some Taint_domain.taint_bottom
      | _ -> failwith "more than one return value" in
  { ret; globals }

let top (cfg : Cfg.t) (_vars : Var.t list) : t =
  let globals = List.map cfg.global_types ~f:(fun _ -> Taint_domain.taint_top) in
  let ret = match cfg.return_types with
      | [] -> None
      | _ :: [] -> Some Taint_domain.taint_top
      | _ -> failwith "more than one return value" in
  { globals; ret }

let of_import (_idx : int) (name : string) (nglobals : int) (_args : Type.t list) (ret : Type.t list) : t =
  match name with
  | "fd_write" | "fd_close" | "fd_seek" | "fd_fdstat_get" | "proc_exit" ->
    (* Globals are unchanged *)
    let globals = List.init nglobals ~f:(fun i -> Taint_domain.taint (Var.Global i)) in
    (* Ret is untainted *)
    let ret = match ret with
      | [] -> None
      | _ :: [] -> Some Taint_domain.taint_bottom
      | _ -> failwith "more than one return value" in
    { globals; ret }
  | _ ->
    (* XXX: We assume globals are unchanged, might not always be the case! *)
    Logging.info (Printf.sprintf "Imported function is not modelled: %s" name);
    let globals = List.init nglobals ~f:(fun i -> Taint_domain.taint (Var.Global i)) in
    (* Ret is tainted *)
    let ret = match ret with
      | [] -> None
      | _ :: [] -> Some Taint_domain.taint_top
      | _ -> failwith "more than one return value" in
    { globals; ret }

let initial_summaries (cfgs : Cfg.t IntMap.t) (module_ : Wasm_module.t) (typ : [`Bottom | `Top]) : t IntMap.t =
  List.fold_left module_.imported_funcs
    ~init:(IntMap.map cfgs ~f:(fun cfg ->
        (match typ with
         | `Bottom -> bottom
         | `Top -> top) cfg []))
    ~f:(fun summaries (idx, name, (args, ret)) ->
        IntMap.set summaries ~key:idx ~data:(of_import idx name module_.nglobals args ret))

let make (_cfg : Cfg.t) (state : Taint_domain.t)
    (ret : Var.t option) (globals_post : Var.t list)
    (_mem_post : Var.t list)
  : t =
  let globals = List.map globals_post ~f:(fun g -> Taint_domain.get_taint state g) in
  let ret = Option.map ret ~f:(fun r -> Taint_domain.get_taint state r) in
  { globals; ret; }


(** Apply a summary to a given state, where args are the arguments to apply the summary, globals are the globals after the summary, and ret is the optional return value.
    For example, if the summary states that ret is tainted with {l0, l1}::
    apply [ret: {l0, l1}] (* <- summary *) [r: bot] (* <- state *) [a, b] (* <- args *) [] (* <- globals *) (Some r) (* <- ret *)
    should result in:
    [r: {l0, l1}].
    The process is similar for globals.
 *)
let apply (summary : t) (state : Taint_domain.t) (args : Var.t list) (globals_pre : Var.t list) (globals_post : Var.t list) (ret : Var.t option) : Taint_domain.t =
  (* Printf.printf "apply summary, args are %s, globals_pre are: %s, globals_post are: %s, ret is: %s\n"
    (String.concat ~sep:"," (List.map args ~f:Var.to_string))
    (String.concat ~sep:"," (List.map globals_pre ~f:Var.to_string))
    (String.concat ~sep:"," (List.map globals_post ~f:Var.to_string))
    (match ret with
     | Some v -> Var.to_string v
     | None -> "__none__"); *)
  (* The arguments of the summary are l0, l1, etc. *)
  let summary_args = List.init (List.length args) ~f:(fun i -> Var.Local i) in
  (* The globals of the summary are g0, g1, etc. *)
  let summary_globals = List.init (List.length globals_pre) ~f:(fun i -> Var.Global i) in
  let subst (t : Taint_domain.taint) : Taint_domain.taint =
    (* We substitute in the taint: l0 by the actual argument (a in the example.
       Similarly for globals: g0 becomes the corresponding global in globals_pre *)
    (Taint_domain.taint_substitute t
           ((List.map2_exn summary_args args ~f:(fun x y -> (x, Taint_domain.get_taint state y))) @
            (List.map2_exn summary_globals globals_pre ~f:(fun x y -> (x, Taint_domain.get_taint state y))))) in
  (* First, propagate the taint for the return value in the given state *)
  let with_ret = match summary.ret, ret with
    | Some r, Some r' ->
      (* r' (the return value from the point of view of the caller) is tainted with the taint of r (after performing the substitutions). *)
      Taint_domain.add_taint state r' (subst r)
    | None, None -> state
    | _ -> failwith "Taint_summary.apply: incompatible return value for summary"
  in
  (* Then, propagate the taint for the globals in a similar way*)
  let with_globals = List.fold_left (List.map2_exn summary.globals globals_post ~f:(fun x y -> (x, y)))
      ~init:with_ret
      ~f:(fun acc (g, g') ->
          (* g' (the global after the call) is tainted with the taint of g (the global of the summary, representing the global at the end of the execution of the summary *)
          Taint_domain.add_taint acc g' (subst g))
  in
  (* TODO: memory *)
  with_globals
