open Core
open Helpers

(** A value-set summary *)
type t = {
  ret : Abstract_store_domain.Value.t option; (** the value-set of the (optional) return value *)
  globals : Abstract_store_domain.Value.t list; (** the value-sets of the globals after applying the function *)
  mem : Abstract_store_domain.t; (** the value-sets of all pointer variables on the memory *)
}
[@@deriving sexp, compare, equal]

(** Check if s1 subsumes s2 *)
let subsumes (s1 : t) (s2 : t) : bool =
  if List.length s1.globals = List.length s2.globals then (* Why do they need to be of same length ?*)
    let ret_subsumes = match s1.ret, s2.ret with
      | Some _v1, Some _v2 -> true (* TODO: rethink this function Reduced_interval_congruence.RIC.subsumes v1 v2*)
      | None, None -> true
      | _ -> false in
    let globals_subsumes = 
      ret_subsumes && 
      (List.fold2_exn s1.globals s2.globals 
        ~init:true 
        ~f:(fun acc _ric1 _ric2 -> acc (* TODO: Rethink this function && Reduced_interval_congruence.RIC.subsumes ric1 ric2)) in *) )) in
    let mem_subsumes = 
      globals_subsumes && 
      (Abstract_store_domain.subsumes s1.mem s2.mem) in
    mem_subsumes
  else
    false

type state = Abstract_store_domain.t

let to_string (s : t) : string =
  Printf.sprintf "stack: [%s]\nglobals:[%s]\nmem: %s"
    (String.concat ~sep:";"
      (List.map ~f:Abstract_store_domain.Value.to_string (Option.to_list s.ret)))
    (String.concat ~sep:";"
      (List.map ~f:Abstract_store_domain.Value.to_string s.globals))
    (Abstract_store_domain.to_string s.mem)

(* TODO: of_string, if necessary *)

let bottom (cfg : 'a Cfg.t) (vars : Var.Set.t) : t =
  (* let globals = List.map (Var.Set.to_list vars) ~f:(fun v -> Reduced_interval_congruence.RIC.ric (0, Int 0, Int 0, (Var.to_string v, 0))) in
  print_endline ("initial globals: " ^ (List.to_string ~f:Reduced_interval_congruence.RIC.to_string globals)); *)
  (* let globals = List.map cfg.global_types ~f:(fun _ -> Reduced_interval_congruence.RIC.Bottom) in *)
  let globals = List.map (Var.Set.to_list vars) ~f:(fun g -> Abstract_store_domain.Value.ValueSet (Reduced_interval_congruence.RIC.ric (0, Int 0, Int 0, (Var.to_string g, 0)))) in
  let ret = match cfg.return_types with
      | [] -> None
      | _ :: [] -> Some (Abstract_store_domain.Value.ValueSet Reduced_interval_congruence.RIC.Bottom)
      | _ -> failwith "more than one return value" in
  let mem = Abstract_store_domain.bottom in
  { ret; globals; mem }

let top (cfg : 'a Cfg.t) (_vars : Var.Set.t) : t =
  let globals = List.map cfg.global_types ~f:(fun _ -> Abstract_store_domain.Value.ValueSet Reduced_interval_congruence.RIC.Top) in
  let ret = match cfg.return_types with
      | [] -> None
      | _ :: [] -> Some (Abstract_store_domain.Value.ValueSet Reduced_interval_congruence.RIC.Top)
      | _ -> failwith "more than one return value" in
  let mem = Abstract_store_domain.top in
  { ret; globals; mem }

let of_import (name : string) (nglobals : Int32.t) (_args : Type.t list) (ret : Type.t list) : t =
  print_endline "making summary from imports";
  match name with
  | "fd_write" | "fd_close" | "fd_seek" | "fd_fdstat_get" | "proc_exit" ->
    (* Globals are unchanged *)
    let globals = 
      List.init (Int32.to_int_exn nglobals) 
        ~f:(fun i -> Abstract_store_domain.Value.ValueSet (Reduced_interval_congruence.RIC.ric (0, Int 0, Int 0, (Var.to_string (Var.Global i), 0)))) in
    (* Ret points to nowhere *)
    let ret = match ret with
      | [] -> None
      | _ :: [] -> Some (Abstract_store_domain.Value.ValueSet Reduced_interval_congruence.RIC.Bottom) (* Is that right ??? *)
      | _ -> failwith "more than one return value" in
    { globals; ret; mem = Abstract_store_domain.top } 
  | _ ->
    (* XXX: We assume globals are unchanged, might not always be the case! *)
    Log.warn (Printf.sprintf "Imported function is not modelled: %s" name);
    let globals = 
      List.init (Int32.to_int_exn nglobals) 
        ~f:(fun i -> Abstract_store_domain.Value.ValueSet (Reduced_interval_congruence.RIC.ric (0, Int 0, Int 0, (Var.to_string (Var.Global i), 0)))) in
    (* Ret can point anywhere *)
    let ret = match ret with
      | [] -> None
      | _ :: [] -> Some (Abstract_store_domain.Value.ValueSet Reduced_interval_congruence.RIC.Top)
      | _ -> failwith "more than one return value" in
    { globals; ret; mem = Abstract_store_domain.top }

let initial_summaries (cfgs : 'a Cfg.t Int32Map.t) (module_ : Wasm_module.t) (typ : [`Bottom | `Top]) : t Int32Map.t =
  List.fold_left module_.imported_funcs
    ~init:(Int32Map.map cfgs ~f:(fun cfg ->
        let globals = Var.Set.of_list (List.init (List.length cfg.global_types) ~f:(fun i -> Var.Global i)) in
        (match typ with
         | `Bottom -> bottom
         | `Top -> top) cfg globals))
    ~f:(fun summaries (idx, name, (args, ret)) ->
        print_endline "of_import";
        Int32Map.set summaries ~key:idx ~data:(of_import name module_.nglobals args ret))

let make (_cfg : 'a Cfg.t) (state : Abstract_store_domain.t)
    (ret : Variable.t option) (globals_post : Variable.t list)
    (mem_post : (Variable.t * Reduced_interval_congruence.RIC.t) list)
  : t =
  let globals = List.map globals_post ~f:(fun g -> Abstract_store_domain.get state ~var:g) in
  let ret = Option.map ret ~f:(fun r -> Abstract_store_domain.get state ~var:r) in
  let mem = List.fold mem_post 
    ~init:Abstract_store_domain.bottom
    ~f:(fun store (x, r) -> Abstract_store_domain.set store ~var:x ~vs:(Abstract_store_domain.Value.ValueSet r)) in
  { globals; ret; mem }

(** Apply a summary to a given state, where args are the arguments to apply the summary, globals are the globals after the summary, and ret is the optional return value.
    For example, if the summary states that ret is tainted with {l0, l1}::
    apply [ret: {l0, l1}] (* <- summary *) [r: bot] (* <- state *) [a, b] (* <- args *) [] (* <- globals *) (Some r) (* <- ret *)
    should result in:
    [r: {l0, l1}].
    The process is similar for globals.
 *)
let apply 
    (_summary : t) 
    (state : Abstract_store_domain.t) 
    (_args : Variable.t list) 
    (_globals_pre : Variable.t list) 
    (_globals_post : Variable.t list) 
    (_mem_post : Variable.t list) 
    (_ret : Variable.t option) 
  : Abstract_store_domain.t = state
  (* TODO: complete this function *)