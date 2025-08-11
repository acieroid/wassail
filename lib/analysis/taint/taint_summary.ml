open Core
open Helpers

(** A taint summary *)
type t = {
  ret : Taint_domain.Taint.t option; (** The taint of the (optional) return value *)
  globals : Taint_domain.Taint.t list; (** The taint of the globals after applying the function *)
  mem: Taint_domain.Taint.t; (* XXX: mem is now a single taint, should be one per var maybe *)
}
[@@deriving sexp, compare, equal]

(** Check if s1 subsumes s2 *)
let subsumes (s1 : t) (s2 : t) : bool =
  if List.length s1.globals = List.length s2.globals then
    let ret_subsumes = match s1.ret, s2.ret with
      | Some v1, Some v2 -> Taint_domain.Taint.subsumes v1 v2
      | None, None -> true
      | _ -> false in
    let globals_subsumes = ret_subsumes && (List.fold2_exn s1.globals s2.globals ~init:true ~f:(fun acc t1 t2 -> acc && Taint_domain.Taint.subsumes t1 t2)) in
    let mem_subsumes = globals_subsumes && (Taint_domain.Taint.subsumes s1.mem s2.mem) in
    mem_subsumes
  else
    false

type state = Taint_domain.t

let to_string (s : t) : string =
  Printf.sprintf "stack: [%s], globals: [%s], mem: %s"
    (String.concat ~sep:";"
       (List.map ~f:Taint_domain.Taint.to_string
          (Option.to_list s.ret)))
    (String.concat ~sep:";"
       (List.map ~f:Taint_domain.Taint.to_string s.globals))
    (Taint_domain.Taint.to_string s.mem)

let of_string (s : string) : t =
  let stack, globals, mem = Scanf.sscanf s "stack: [%s], globals: [%s], mem: %s" (fun x y z -> (x, y, z)) in
  let ret = match String.split stack ~on:';' with
    | [] -> None
    | taint :: [] -> Some (Taint_domain.Taint.of_string taint)
    | _ -> failwith "invalid taint summary"
  in
  let globals = List.map (String.split globals ~on:';') ~f:Taint_domain.Taint.of_string in
  let mem = Taint_domain.Taint.of_string mem in
  { ret; globals; mem }

let bottom (cfg : 'a Cfg.t) (_vars : Var.Set.t) : t =
  let globals = List.map cfg.global_types ~f:(fun _ -> Taint_domain.Taint.bottom) in
  let ret = match cfg.return_types with
      | [] -> None
      | _ :: [] -> Some Taint_domain.Taint.bottom
      | _ -> failwith "more than one return value" in
  let mem = Taint_domain.Taint.bottom in
  { ret; globals; mem }

let top (cfg : 'a Cfg.t) (_vars : Var.Set.t) : t =
  let globals = List.map cfg.global_types ~f:(fun _ -> Taint_domain.Taint.top) in
  let ret = match cfg.return_types with
      | [] -> None
      | _ :: [] -> Some Taint_domain.Taint.top
      | _ -> failwith "more than one return value" in
  let mem = Taint_domain.Taint.top in
  { globals; ret; mem }

let of_import (name : string) (nglobals : Int32.t) (_args : Type.t list) (ret : Type.t list) : t =
  match name with
  | "fd_write" | "fd_close" | "fd_seek" | "fd_fdstat_get" | "proc_exit" ->
    (* Globals are unchanged *)
    let globals = List.init (Int32.to_int_exn nglobals) ~f:(fun i -> Taint_domain.Taint.taint (Var.Global i)) in
    (* Ret is untainted *)
    let ret = match ret with
      | [] -> None
      | _ :: [] -> Some Taint_domain.Taint.bottom
      | _ -> failwith "more than one return value" in
    { globals; ret; mem = Taint_domain.Taint.top } (* XXX: We assume memory does not get tainted, which is definitely not always true (e.g., fd_write) *)
  | _ ->
    (* XXX: We assume globals are unchanged, might not always be the case! *)
    Log.warn (Printf.sprintf "Imported function is not modelled: %s" name);
    let globals = List.init (Int32.to_int_exn nglobals) ~f:(fun i -> Taint_domain.Taint.taint (Var.Global i)) in
    (* Ret is tainted *)
    let ret = match ret with
      | [] -> None
      | _ :: [] -> Some Taint_domain.Taint.top
      | _ -> failwith "more than one return value" in
    { globals; ret; mem = Taint_domain.Taint.top }

let initial_summaries (cfgs : 'a Cfg.t Int32Map.t) (module_ : Wasm_module.t) (typ : [`Bottom | `Top]) : t Int32Map.t =
  List.fold_left module_.imported_funcs
    ~init:(Int32Map.map cfgs ~f:(fun cfg ->
        (match typ with
         | `Bottom -> bottom
         | `Top -> top) cfg Var.Set.empty))
    ~f:(fun summaries desc ->
        Int32Map.set summaries ~key:desc.idx ~data:(of_import desc.name module_.nglobals desc.arguments desc.returns))

let make (_cfg : 'a Cfg.t) (state : Taint_domain.t)
    (ret : Var.t option) (globals_post : Var.t list)
    (mem_post : Var.t list)
  : t =
  let globals = List.map globals_post ~f:(fun g -> Taint_domain.get_taint state g) in
  let ret = Option.map ret ~f:(fun r -> Taint_domain.get_taint state r) in
  { globals; ret;
    (* The taint of the memory is the join of all taints of memory variables *)
    mem = List.fold_left (List.map mem_post ~f:(Taint_domain.get_taint state))
        ~init:Taint_domain.Taint.bottom ~f:Taint_domain.Taint.join }


(** Apply a summary to a given state, where args are the arguments to apply the summary, globals are the globals after the summary, and ret is the optional return value.
    For example, if the summary states that ret is tainted with {l0, l1}::
    apply [ret: {l0, l1}] (* <- summary *) [r: bot] (* <- state *) [a, b] (* <- args *) [] (* <- globals *) (Some r) (* <- ret *)
    should result in:
    [r: {l0, l1}].
    The process is similar for globals.
 *)
let apply (summary : t) (state : Taint_domain.t) (args : Var.t list) (globals_pre : Var.t list) (globals_post : Var.t list) (mem_post : Var.t list) (ret : Var.t option) : Taint_domain.t =
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
  let subst (t : Taint_domain.Taint.t) : Taint_domain.Taint.t =
    (* We substitute in the taint: l0 by the actual argument (a in the example.
       Similarly for globals: g0 becomes the corresponding global in globals_pre *)
    assert (List.length summary_args = List.length args);
    assert (List.length summary_globals = List.length globals_pre);
    (Taint_domain.Taint.substitute t
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
  assert (List.length summary.globals = List.length globals_post);
  let with_globals = List.fold_left (List.map2_exn summary.globals globals_post ~f:(fun x y -> (x, y)))
      ~init:with_ret
      ~f:(fun acc (g, g') ->
          (* g' (the global after the call) is tainted with the taint of g (the global of the summary, representing the global at the end of the execution of the summary *)
          Taint_domain.add_taint acc g' (subst g))
  in
  (* Memory: propagate the join memory taint from the summary to all memory locs in the state *)
  let with_mem = if !Taint_options.keep_memory then
      List.fold_left mem_post ~init:with_globals ~f:(fun acc v ->
          Taint_domain.add_taint acc v (subst summary.mem))
    else
      with_globals in
  with_mem


let summary_of (cfg : Spec_domain.t Cfg.t) (out_state : Taint_domain.t) : t =
  match Cfg.state_after_block cfg cfg.exit_block with
  | Bottom ->
    (* The function exit is likely unreachable, so we use a bottom summary *)
      { ret = None;
        globals = List.init (List.length cfg.global_types) ~f:(fun _ -> Taint_domain.Taint.bottom);
        mem = Taint_domain.Taint.bottom; }
    | NotBottom exit_spec ->
      make cfg out_state
        (if List.length cfg.return_types = 1 then List.hd exit_spec.vstack else None)
        exit_spec.globals
        (List.concat_map (Var.OffsetMap.to_alist exit_spec.memory)
           ~f:(fun ((a, _), b) ->
               (* Log.warn (Printf.sprintf "ignoring offset"); *)
               [a; b]))
