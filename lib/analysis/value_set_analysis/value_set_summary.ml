open Core
open Helpers
open Reduced_interval_congruence

(** A value set summary *)
type t = Abstract_store_domain.t

(* let subsumes = Abstract_store_domain.subsumes *)

let to_string (s : t) : string =
  let ret = Variable.Map.find s.abstract_store (Variable.Var Var.Return) in
  (* let affected = Variable.Map.find s.abstract_store Variable.Affected in *)
  let affected_memory = s.store_operations in
  let accessed = Variable.Map.find s.abstract_store Variable.Accessed in
  let globals = 
    Variable.Map.filter_keys s.abstract_store 
      ~f:(fun key -> 
        match key with
        | Variable.Var Var.Global _ -> true
        | _ -> false) in
  let memory =
    Variable.Map.filter_keys s.abstract_store
      ~f:(fun key -> Variable.is_linear_memory key) in
  let stack =
    Variable.Map.filter_keys s.abstract_store
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
      ["GLOBALS:" ^ Abstract_store_domain.to_string {abstract_store = globals; store_operations = RICSet.empty}])
    @
    if Set.is_empty affected_memory then
      []
    else
      (["AFFECTED MEMORY: " ^ RICSet.to_string (
        if !Value_set_options.disjoint_stack then 
          RICSet.filter ~f:(fun r -> not (RIC.is_stack r)) affected_memory
        else 
          affected_memory)]
      @
      ["LINEAR MEMORY:" 
      ^ Abstract_store_domain.to_string {abstract_store = memory; store_operations = RICSet.empty}])
    @
    if !Value_set_options.disjoint_stack then
      let affected_stack = RICSet.filter ~f:(fun r -> RIC.is_stack r) affected_memory in
      if RICSet.is_empty affected_stack then
        []
      else
        (["AFFECTED STACK: " ^ RICSet.to_string affected_stack]
        @
        ["STACK: " ^ Abstract_store_domain.to_string {abstract_store = stack; store_operations = RICSet.empty}])
    else
      []
    @ 
    begin match accessed with
    | None | Some (Abstract_store_domain.Value.ValueSet RIC.Bottom) -> []
    | Some Abstract_store_domain.Value.Boolean _ -> assert false
    | Some (Abstract_store_domain.Value.ValueSet addresses) ->
      ["ACCESSED MEMORY:" ^ RIC.to_string addresses]
    end
  in "\t" ^ String.concat ~sep:"\n\t" string_list
  

(* TODO: do I need to mention stack and memory? *)
let bottom (cfg : 'a Cfg.t) (_vars : Var.Set.t) : t =
  let nb_of_globals = List.length (cfg.global_types) in
  let global_indices = List.init nb_of_globals ~f:(fun i -> i) in
  let summary = 
    { Abstract_store_domain.abstract_store =
        List.fold global_indices
          ~init:Variable.Map.empty
          ~f:(fun store idx -> Variable.Map.set store ~key:(Variable.Var (Var.Global idx)) ~data:(Abstract_store_domain.Value.ValueSet RIC.Bottom));
      store_operations = RICSet.empty } in
  let summary =
    match cfg.return_types with
    | [] -> summary
    | _ :: [] -> Abstract_store_domain.set summary ~var:(Variable.Var Var.Return) ~vs:(Abstract_store_domain.Value.ValueSet RIC.Bottom)
    | _ -> failwith "more than one return value" in
  Abstract_store_domain.set summary ~var:(Variable.Accessed) ~vs:(Abstract_store_domain.Value.ValueSet RIC.Bottom)

(* TODO: set value depending on type (e.g. f64 is always bottom) *)
let top (cfg : 'a Cfg.t) (_vars : Var.Set.t) : t =
  let nb_of_globals = List.length (cfg.global_types) in
  let global_indices = List.init nb_of_globals ~f:(fun i -> i) in
  let summary = 
    { Abstract_store_domain.abstract_store = 
        List.fold global_indices
          ~init:Variable.Map.empty
          ~f:(fun store idx -> Variable.Map.set store ~key:(Variable.Var (Var.Global idx)) ~data:(Abstract_store_domain.Value.ValueSet RIC.Top));
      store_operations = RICSet.singleton RIC.Top } in
  let summary =
    match cfg.return_types with
    | [] -> summary
    | _ :: [] -> Abstract_store_domain.set summary ~var:(Variable.Var Var.Return) ~vs:(Abstract_store_domain.Value.ValueSet RIC.Top)
    | _ -> failwith "more than one return value" in
  Abstract_store_domain.set summary ~var:(Variable.Accessed) ~vs:(Abstract_store_domain.Value.ValueSet RIC.Top)



let of_import (name : string) (nglobals : Int32.t) (_args : Type.t list) (ret : Type.t list) : t =
  let summary = {Abstract_store_domain.abstract_store = Variable.Map.empty; store_operations = RICSet.empty} in
  match name with
  | "fd_write" | "fd_seek" | "fd_fdstat_get" ->
    (* Globals are unchanged *)
    let globals = List.init (Int32.to_int_exn nglobals) ~f:(fun i -> Variable.Var (Var.Global i)) in
    let summary = List.fold globals ~init:summary ~f:(fun acc var -> Abstract_store_domain.set acc ~var:var ~vs:(Abstract_store_domain.Value.ValueSet (RIC.relative_ric (Variable.to_string var)))) in
    (* If present, return value is unknown: *)
    let summary =
      begin match ret with
      | [] -> summary
      | _ :: [] -> Abstract_store_domain.set summary ~var:(Variable.Var Var.Return) ~vs:(Abstract_store_domain.Value.ValueSet RIC.Top)
      | _ -> failwith "more than one return value"
      end in
    if !Value_set_options.ignore_imports then 
      ((* Linear memory is considered to be unchanged *)
      let summary = Abstract_store_domain.set summary ~var:(Variable.Accessed) ~vs:(Abstract_store_domain.Value.ValueSet RIC.Bottom) in
      let summary = Abstract_store_domain.set summary ~var:(Variable.entire_memory) ~vs:(Abstract_store_domain.Value.ValueSet RIC.Bottom) in
      (* Stack portion of the linear memory is assumed to not have been changed *)
      let summary = 
        if !Value_set_options.disjoint_stack then
          Abstract_store_domain.set summary ~var:(Variable.entire_stack) ~vs:(Abstract_store_domain.Value.ValueSet RIC.Bottom)
        else
          summary
      in
        {Abstract_store_domain.abstract_store = summary.abstract_store; store_operations = RICSet.empty})
    else
      ((* Linear memory has been modified/accessed, but we don't know how: *)
      let summary = Abstract_store_domain.set summary ~var:(Variable.Accessed) ~vs:(Abstract_store_domain.Value.ValueSet RIC.Top) in
      let summary = Abstract_store_domain.set summary ~var:(Variable.entire_memory) ~vs:(Abstract_store_domain.Value.ValueSet RIC.Top) in
      (* Stack portion of the linear memory is assumed to not have been changed *)
      let summary = 
        if !Value_set_options.disjoint_stack then
          Abstract_store_domain.set summary ~var:(Variable.entire_stack) ~vs:(Abstract_store_domain.Value.ValueSet RIC.Bottom)
        else
          summary
      in
        {Abstract_store_domain.abstract_store = summary.abstract_store; store_operations = RICSet.singleton RIC.Top})
  | "fd_close" | "proc_exit" ->
    (* Globals are unchanged *)
    let globals = List.init (Int32.to_int_exn nglobals) ~f:(fun i -> Variable.Var (Var.Global i)) in
    let summary = List.fold globals ~init:summary ~f:(fun acc var -> Abstract_store_domain.set acc ~var ~vs:(Abstract_store_domain.Value.ValueSet (RIC.relative_ric (Variable.to_string var)))) in
    (* If present, return value is unknown: *)
    let summary =
      begin match ret with
      | [] -> summary
      | _ :: [] -> Abstract_store_domain.set summary ~var:(Variable.Var Var.Return) ~vs:(Abstract_store_domain.Value.ValueSet RIC.Top)
      | _ -> failwith "more than one return value"
      end in
    (* Linear memory is unchanged, but we may have accessed all of it: *)
    let summary = Abstract_store_domain.set summary ~var:(Variable.Accessed) ~vs:(Abstract_store_domain.Value.ValueSet RIC.Top) in
    (* Stack portion of the linear memory is assumed to not have been changed *)
    if !Value_set_options.disjoint_stack then
      Abstract_store_domain.set summary ~var:(Variable.entire_stack) ~vs:(Abstract_store_domain.Value.ValueSet RIC.Bottom)
    else
      summary
  | _ ->
    (* There is no way to know if global variables have been changed *)
    Log.warn (Printf.sprintf "Imported function is not modelled: %s" name);
    (* Globals can point anywhere *)
    let globals = List.init (Int32.to_int_exn nglobals) ~f:(fun i -> Variable.Var (Var.Global i)) in
    let summary = 
      if !Value_set_options.ignore_imports then
        List.fold globals ~init:summary ~f:(fun acc var -> Abstract_store_domain.set acc ~var ~vs:(Abstract_store_domain.Value.ValueSet (RIC.relative_ric (Variable.to_string var))))
      else
        List.fold globals ~init:summary ~f:(fun acc var -> Abstract_store_domain.set acc ~var ~vs:(Abstract_store_domain.Value.ValueSet (RIC.Top))) in
    (* If present, return value is unknown: *)
    let summary =
      begin match ret with
      | [] -> summary
      | _ :: [] -> Abstract_store_domain.set summary ~var:(Variable.Var Var.Return) ~vs:(Abstract_store_domain.Value.ValueSet RIC.Top)
      | _ -> failwith "more than one return value"
      end in
    if !Value_set_options.ignore_imports then 
      ((* Linear memory is considered to be unchanged *)
      let summary = Abstract_store_domain.set summary ~var:(Variable.Accessed) ~vs:(Abstract_store_domain.Value.ValueSet RIC.Bottom) in
      (* Stack portion of the linear memory is assumed to not have been changed *)
      if !Value_set_options.disjoint_stack then
          Abstract_store_domain.set summary ~var:(Variable.entire_stack) ~vs:(Abstract_store_domain.Value.ValueSet RIC.Bottom)
        else
          summary)
    else
      ((* Linear memory may have been modified, but we don't know how: *)
      let summary = Abstract_store_domain.set summary ~var:(Variable.Accessed) ~vs:(Abstract_store_domain.Value.ValueSet RIC.Bottom) in
      (* let summary = Abstract_store_domain.set summary ~var:(Variable.entire_memory) ~vs:(Abstract_store_domain.Value.ValueSet RIC.Top) in *)
      (* Stack portion of the linear memory is assumed to not have been changed *)
      let summary = 
        if !Value_set_options.disjoint_stack then
          Abstract_store_domain.set summary ~var:(Variable.entire_stack) ~vs:(Abstract_store_domain.Value.ValueSet RIC.Bottom)
        else
          summary in
      {abstract_store = summary.abstract_store; store_operations = RICSet.singleton RIC.Top})

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
  { abstract_store =
      Variable.Map.filter_keys state.abstract_store
        ~f:(fun key -> 
          match key with
          | Variable.Var Var.Global _
          | Variable.Var Var.Return
          | Variable.Mem _
          | Variable.Stack _
          | Variable.Accessed -> true
          | _ -> false);
    store_operations = state.store_operations }

let update_relative_offsets (summary : t) ~(actual_values : RIC.t String.Map.t) : t =
  { abstract_store =
      Variable.Map.fold summary.abstract_store 
        ~init:Variable.Map.empty 
        ~f:(fun ~key:var ~data:vs acc -> 
          Variable.Map.set acc
            ~key:(Variable.update_relative_offset ~var ~actual_values)
            ~data:(Abstract_store_domain.Value.update_relative_offset vs actual_values));
    store_operations = RICSet.map ~f:(fun ric_ -> RIC.update_relative_offset ~ric_ ~actual_values) summary.store_operations}

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
  let accessed_memory = Abstract_store_domain.get summary ~var:(Variable.Accessed) in
  let state = Abstract_store_domain.access_memory state ~addresses:accessed_memory in
  let affected_mem_vars =
    List.map
      ~f:(fun addr ->
        if !Value_set_options.disjoint_stack && RIC.is_stack addr then Variable.Stack addr else Variable.Mem addr)
      (RICSet.to_list summary.store_operations)
  in
  let affected_state =
    { Abstract_store_domain.abstract_store = List.fold ~init:Variable.Map.empty ~f:(fun acc var -> Variable.Map.set ~key:var ~data:(Abstract_store_domain.Value.ValueSet RIC.Bottom) acc) affected_mem_vars;
      store_operations = RICSet.empty } in
  let state = Abstract_store_domain.make_compatible ~this_store:state ~relative_to:affected_state in
  let store = 
    Variable.Map.fold
      state.abstract_store
      ~init:Variable.Map.empty
      ~f:(fun ~key:var ~data:vs acc ->
        match var with
        | Variable.Var _
        | Variable.Accessed -> Variable.Map.set acc ~key:var ~data:vs
        | Variable.Stack _
        | Variable.Mem _ ->
          let is_safe =
            List.fold 
              affected_mem_vars 
              ~init:true 
              ~f:(fun acc v -> 
                acc 
                && (if !Value_set_options.disjoint_memory_spaces then true else Variable.comparable_offsets var v)
                && not (Variable.share_addresses var v))
          in
          if is_safe then
            Variable.Map.set acc ~key:var ~data:vs
          else
            acc)
  in
  let state = { Abstract_store_domain.abstract_store = store; 
                store_operations = RICSet.union summary.store_operations state.store_operations} in
  (* Up to here, the affected memory areas have been earased and the lists of affected/accessed addresses have been updated *)
  (* Update globals: *)
  let summary_globals = Variable.Map.filter_keys summary.abstract_store ~f:Variable.is_global in
  let state = 
    Variable.Map.fold summary_globals 
      ~init:state 
      ~f:(fun ~key ~data acc -> Abstract_store_domain.set acc ~var:key ~vs:data) in
  (* Update memory variables: *)
  let state =
    let summary_mems = Variable.Map.filter_keys summary.abstract_store ~f:(fun key -> Variable.is_linear_memory key && not (Variable.equal key Variable.entire_memory)) in
    Variable.Map.fold summary_mems 
      ~init:state 
      ~f:(fun ~key ~data acc -> 
        Abstract_store_domain.set acc ~var:key ~vs:data)
  in
  (* Update stack variables: *)
  let state = 
    let summary_stack = Variable.Map.filter_keys summary.abstract_store ~f:(fun key -> Variable.is_stack key && not (Variable.equal key Variable.entire_stack)) in
    Variable.Map.fold summary_stack 
      ~init:state 
      ~f:(fun ~key ~data acc -> 
        Abstract_store_domain.set acc ~var:key ~vs:data) 
  in
  let state = Abstract_store_domain.remove_pointers_to_top state in
  (* Set return value: *)
  match Variable.Map.find summary.abstract_store (Variable.Var Var.Return), return_variable with
  | Some vs, Some return_variable -> Abstract_store_domain.set state ~var:(Variable.Var return_variable) ~vs
  | None, None -> state
  | _ -> assert false (* TODO: error message? *)