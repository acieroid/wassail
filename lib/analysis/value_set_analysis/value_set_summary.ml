open Core
open Helpers
open Reduced_interval_congruence

(** A value set summary *)
type t = Abstract_store_domain.t

(* let subsumes = Abstract_store_domain.subsumes *)

let to_string (s : t) : string =
  let ret = Variable.Map.find s (Variable.Var Var.Return) in
  let affected = Variable.Map.find s Variable.Affected in
  let globals = 
    Variable.Map.filter_keys s 
      ~f:(fun key -> 
        match key with
        | Variable.Var Var.Global _ -> true
        | _ -> false) in
  let memory =
    Variable.Map.filter_keys s
      ~f:(fun key -> Variable.is_linear_memory key) in
  let stack =
    Variable.Map.filter_keys s
      ~f:(fun key -> Variable.is_stack key) in
  let string_list =
    begin match ret with
    | None -> []
    | Some Abstract_store_domain.Value.Boolean _ -> ["RETURN: ⊤"]
    | Some value -> ["RETURN:" ^ Abstract_store_domain.Value.to_string value]
    end
    @
    (if Variable.Map.is_empty globals then
      []
    else
      ["GLOBALS:" ^ Abstract_store_domain.to_string globals])
    @
    begin match affected with
    | None | Some (Abstract_store_domain.Value.ValueSet RIC.Bottom) -> []
    | Some Abstract_store_domain.Value.Boolean _ -> assert false
    | Some (Abstract_store_domain.Value.ValueSet addresses) ->
      ["AFFECTED MEMORY:" ^ RIC.to_string addresses ^ ", " ^
      "LINEAR MEMORY:" ^ Abstract_store_domain.to_string memory]
    end
    @ if !Value_set_options.disjoint_stack then ["Stack:" ^ Abstract_store_domain.to_string stack] else []
  in String.concat ~sep:", " string_list
  

(* TODO: do I need to mention stack and memory? *)
let bottom (cfg : 'a Cfg.t) (_vars : Var.Set.t) : t =
  let nb_of_globals = List.length (cfg.global_types) in
  let global_indices = List.init nb_of_globals ~f:(fun i -> i) in
  let summary = 
    List.fold global_indices
      ~init:Variable.Map.empty
      ~f:(fun store idx -> Variable.Map.set store ~key:(Variable.Var (Var.Global idx)) ~data:(Abstract_store_domain.Value.ValueSet RIC.Bottom)) in
  let summary =
    match cfg.return_types with
    | [] -> summary
    | _ :: [] -> Variable.Map.set summary ~key:(Variable.Var Var.Return) ~data:(Abstract_store_domain.Value.ValueSet RIC.Bottom)
    | _ -> failwith "more than one return value" in
  Variable.Map.set summary ~key:(Variable.Affected) ~data:(Abstract_store_domain.Value.ValueSet RIC.Bottom)

(* TODO: set value depending on type (e.g. f64 is always bottom) *)
let top (cfg : 'a Cfg.t) (_vars : Var.Set.t) : t =
  let nb_of_globals = List.length (cfg.global_types) in
  let global_indices = List.init nb_of_globals ~f:(fun i -> i) in
  let summary = 
    List.fold global_indices
      ~init:Variable.Map.empty
      ~f:(fun store idx -> Variable.Map.set store ~key:(Variable.Var (Var.Global idx)) ~data:(Abstract_store_domain.Value.ValueSet RIC.Top)) in
  let summary =
    match cfg.return_types with
    | [] -> summary
    | _ :: [] -> Variable.Map.set summary ~key:(Variable.Var Var.Return) ~data:(Abstract_store_domain.Value.ValueSet RIC.Top)
    | _ -> failwith "more than one return value" in
  Variable.Map.set summary ~key:(Variable.Affected) ~data:(Abstract_store_domain.Value.ValueSet RIC.Top)



let of_import (name : string) (nglobals : Int32.t) (_args : Type.t list) (ret : Type.t list) : t =
  let summary = Variable.Map.empty in
  match name with
  | "fd_write" | "fd_seek" | "fd_fdstat_get" ->
    (* Globals are unchanged *)
    let globals = List.init (Int32.to_int_exn nglobals) ~f:(fun i -> Variable.Var (Var.Global i)) in
    let summary = List.fold globals ~init:summary ~f:(fun acc var -> Variable.Map.set acc ~key:var ~data:(Abstract_store_domain.Value.ValueSet (RIC.relative_ric (Variable.to_string var)))) in
    (* If present, return value is unknown: *)
    let summary =
      begin match ret with
      | [] -> summary
      | _ :: [] -> Variable.Map.set summary ~key:(Variable.Var Var.Return) ~data:(Abstract_store_domain.Value.ValueSet RIC.Top)
      | _ -> failwith "more than one return value"
      end in
    (* Linear memory has been modified, but we don't know how: *)
    let summary = Variable.Map.set summary ~key:(Variable.Affected) ~data:(Abstract_store_domain.Value.ValueSet RIC.Top) in
    let summary = Variable.Map.set summary ~key:(Variable.entire_memory) ~data:(Abstract_store_domain.Value.ValueSet RIC.Top) in
    (* Stack portion of the linear memory is assumed to not have been changed *)
    Variable.Map.set summary ~key:(Variable.entire_stack) ~data:(Abstract_store_domain.Value.ValueSet RIC.Bottom)
  | "fd_close" | "proc_exit" ->
    (* Globals are unchanged *)
    let globals = List.init (Int32.to_int_exn nglobals) ~f:(fun i -> Variable.Var (Var.Global i)) in
    let summary = List.fold globals ~init:summary ~f:(fun acc var -> Variable.Map.set acc ~key:var ~data:(Abstract_store_domain.Value.ValueSet (RIC.relative_ric (Variable.to_string var)))) in
    (* If present, return value is unknown: *)
    let summary =
      begin match ret with
      | [] -> summary
      | _ :: [] -> Variable.Map.set summary ~key:(Variable.Var Var.Return) ~data:(Abstract_store_domain.Value.ValueSet RIC.Top)
      | _ -> failwith "more than one return value"
      end in
    (* Linear memory is unchanged: *)
    let summary = Variable.Map.set summary ~key:(Variable.Affected) ~data:(Abstract_store_domain.Value.ValueSet RIC.Bottom) in
    (* Stack portion of the linear memory is assumed to not have been changed *)
    Variable.Map.set summary ~key:(Variable.entire_stack) ~data:(Abstract_store_domain.Value.ValueSet RIC.Bottom)
  | _ ->
    (* There is no way to know if global variables have been changed *)
    Log.warn (Printf.sprintf "Imported function is not modelled: %s" name);
    (* Globals can point anywhere *)
    let globals = List.init (Int32.to_int_exn nglobals) ~f:(fun i -> Variable.Var (Var.Global i)) in
    let summary = List.fold globals ~init:summary ~f:(fun acc var -> Variable.Map.set acc ~key:var ~data:(Abstract_store_domain.Value.ValueSet (RIC.Top))) in
    (* If present, return value is unknown: *)
    let summary =
      begin match ret with
      | [] -> summary
      | _ :: [] -> Variable.Map.set summary ~key:(Variable.Var Var.Return) ~data:(Abstract_store_domain.Value.ValueSet RIC.Top)
      | _ -> failwith "more than one return value"
      end in
    (* Linear memory may have been modified, but we don't know how: *)
    let summary = Variable.Map.set summary ~key:(Variable.Affected) ~data:(Abstract_store_domain.Value.ValueSet RIC.Top) in
    let summary = Variable.Map.set summary ~key:(Variable.entire_memory) ~data:(Abstract_store_domain.Value.ValueSet RIC.Top) in
    (* Stack portion of the linear memory is assumed to not have been changed *)
    Variable.Map.set summary ~key:(Variable.entire_stack) ~data:(Abstract_store_domain.Value.ValueSet RIC.Bottom)

let initial_summaries 
    (cfgs : 'a Cfg.t Int32Map.t) 
    (module_ : Wasm_module.t)
    (typ : [`Bottom | `Top]) 
  : t Int32Map.t =
  List.fold_left module_.imported_funcs
    ~init:(Int32Map.map cfgs ~f:(fun cfg ->
        (match typ with
         | `Bottom -> bottom
         | `Top -> top) cfg Var.Set.empty))
    ~f:(fun summaries (idx, name, (args, ret)) ->
        Int32Map.set summaries ~key:idx ~data:(of_import name module_.nglobals args ret))

let make (state : Abstract_store_domain.t) : t =
  Variable.Map.filter_keys state
    ~f:(fun key -> 
      match key with
      | Variable.Var Var.Global _
      | Variable.Var Var.Return
      | Variable.Mem _
      | Variable.Stack _
      | Variable.Affected -> true
      | _ -> false)

let update_relative_offsets (summary : t) ~(actual_values : RIC.t String.Map.t) : t =
  Variable.Map.fold summary 
    ~init:Variable.Map.empty 
    ~f:(fun ~key:var ~data:vs acc -> 
      Variable.Map.set acc
        ~key:(Variable.update_relative_offset ~var ~actual_values)
        ~data:(Abstract_store_domain.Value.update_relative_offset vs actual_values))

let apply 
    ~(summary : t) 
    ~(state : Abstract_store_domain.t) 
    ~(args : Var.t list)
    ~(return_variable : Var.t option)
  : Abstract_store_domain.t =
  let globals = Abstract_store_domain.extract_global_values state in
  let arguments = Abstract_store_domain.extract_argument_values state ~args in
  let actual_values =
    Map.merge globals arguments
      ~f:(fun ~key:_ vs ->
        match vs with
        | `Left vs | `Right vs -> Some vs
        | `Both _ -> assert false) in
  let summary = update_relative_offsets summary ~actual_values in
  let affected_memory = Abstract_store_domain.get summary ~var:(Variable.Affected) in
  let state = Abstract_store_domain.affect_memory state ~addresses:affected_memory in
  let affected_mem_var =
    match affected_memory with
    | Boolean _ -> assert false
    | ValueSet r -> Variable.Mem r in
  let affected_state = 
    Variable.Map.set 
      Variable.Map.empty 
      ~key:affected_mem_var 
      ~data:(Abstract_store_domain.Value.ValueSet RIC.Bottom) in
  let state = Abstract_store_domain.make_compatible ~this_store:state ~relative_to:affected_state in
  let state = 
    Variable.Map.filter_keys state 
      ~f:(fun var -> 
        match var with
        | Var _ | Affected | Stack _-> true
        | Mem _ -> Variable.comparable_offsets var affected_mem_var && (not (Variable.share_addresses var affected_mem_var))) in
  (* Up to here, the affected memory areas have been earased and the list of affected addresses has been updated *)
  (* Update globals: *)
  let summary_globals = Variable.Map.filter_keys summary ~f:Variable.is_global in
  let state = 
    Variable.Map.fold summary_globals 
      ~init:state 
      ~f:(fun ~key ~data acc -> Abstract_store_domain.set acc ~var:key ~vs:data) in
  (* Update memory variables: *)
  let state =
    match Abstract_store_domain.get summary ~var:Variable.Affected with
    | Abstract_store_domain.Value.ValueSet RIC.Bottom -> state
    | _ ->
      let summary_mems = Variable.Map.filter_keys summary ~f:(fun key -> Variable.is_linear_memory key && not (Variable.equal key Variable.entire_memory)) in
      Variable.Map.fold summary_mems 
        ~init:state 
        ~f:(fun ~key ~data acc -> 
          Abstract_store_domain.set acc ~var:key ~vs:data) 
  in
  (* Update stack variables: *)
  let state = 
    let summery_stack = Variable.Map.filter_keys summary ~f:(fun key -> Variable.is_stack key && not (Variable.equal key Variable.entire_stack)) in
    Variable.Map.fold summery_stack 
      ~init:state 
      ~f:(fun ~key ~data acc -> 
        Abstract_store_domain.set acc ~var:key ~vs:data) 
  in
  let state = Abstract_store_domain.remove_pointers_to_top state in
  (* Set return value: *)
  match Variable.Map.find summary (Variable.Var Var.Return), return_variable with
  | Some vs, Some return_variable -> Abstract_store_domain.set state ~var:(Variable.Var return_variable) ~vs
  | None, None -> state
  | _ -> assert false (* TODO: error message? *)