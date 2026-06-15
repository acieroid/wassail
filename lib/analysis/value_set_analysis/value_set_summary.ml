open Core
open Helpers
open Reduced_interval_congruence

(* TODO: put this somewhere else *)
let (|>>) (c, i : 'a * 'b) (f : 'a -> 'b -> 'c) : 'c = f c i

(** Function summaries for the value-set analysis.

    A summary keeps only the effects that can be observed by callers: globals,
    return values, linear-memory cells, accessed memory, memory size, and the set
    of memory areas written by the function. *)
type t = Abstract_store_domain.t

(** Pretty-print the caller-observable parts of a summary. *)
let to_string (s : t) : string =
  let affected_memory = s.store_operations in
  let accessed = Variable.Map.find s.abstract_store Variable.Accessed in
  let memory_size = Variable.Map.find s.abstract_store Variable.MemorySize in
  let returns = 
    s.abstract_store |> Variable.Map.filter_keys ~f:(function
                                                     | Variable.Var Var.Return _ -> true
                                                     | _ -> false) in
  let globals = 
    s.abstract_store |> Variable.Map.filter_keys ~f:(function
                                                     | Variable.Var Var.Global _ -> true
                                                     | _ -> false) in
  let memory =
    s.abstract_store |> Variable.Map.filter_keys ~f:(fun key -> Variable.is_linear_memory key) in
  ["\t\t"]
  @
  (if Variable.Map.is_empty returns then
    []
  else
    ["RETURNS: " ^ Abstract_store_domain.to_string {abstract_store = returns; store_operations = RICSet.empty; unreachable = false}])
  @
  (if Variable.Map.is_empty globals then
    []
  else
    ["GLOBALS: " ^ Abstract_store_domain.to_string {abstract_store = globals; store_operations = RICSet.empty; unreachable = false}])
  @
  (if Set.is_empty affected_memory then
    []
  else
    (["AFFECTED MEMORY: " ^ RICSet.to_string affected_memory]))
  @
  (["LINEAR MEMORY: " ^ Abstract_store_domain.to_string {abstract_store = memory; store_operations = RICSet.empty; unreachable = false}])
  @ 
  (match accessed with
  | None | Some (Abstract_store_domain.Value.ValueSet RIC.Bottom) -> 
    []
  | Some addresses -> ["ACCESSED MEMORY:" ^ Value_set_abstraction.to_string addresses])
  @
  (match memory_size with
  | None | Some (Abstract_store_domain.Value.ValueSet RIC.Bottom) -> 
    []
  | Some size -> ["MEMORY SIZE (nb of pages):" ^ Value_set_abstraction.to_string size])
  |> String.concat ~sep:"\n\t\t"
  
(** Initial bottom summary for a defined function. *)
let bottom (cfg : 'a Cfg.t) (_vars : Var.Set.t) : t =
  let state =
    { Abstract_store_domain.abstract_store =
        List.foldi cfg.global_types
          ~init:Variable.Map.empty
          ~f:(fun idx store _ -> Variable.Map.set store ~key:(Variable.Var (Var.Global idx)) ~data:Abstract_store_domain.Value.bottom);
      store_operations = RICSet.empty;
      unreachable = false }
  in
  cfg.return_types
  |> List.foldi ~init:state 
    ~f:(fun idx state _ -> 
      state |> Abstract_store_domain.set 
                ~var:(Variable.Var (Var.Return (cfg.idx, Int32.of_int_exn idx))) 
                ~vs:Abstract_store_domain.Value.bottom)
  |> Abstract_store_domain.set ~var:(Variable.Accessed) ~vs:(Abstract_store_domain.Value.bottom)
  |> Abstract_store_domain.set ~var:(Variable.MemorySize) ~vs:(Abstract_store_domain.Value.bottom)

(** Initial top summary for a defined function.

    Globals and return values are unknown, all memory may have been written, all
    memory may have been accessed, and memory size is any positive number of
    pages. *)
let top (cfg : 'a Cfg.t) (_vars : Var.Set.t) : t =
  let state =
    { Abstract_store_domain.abstract_store = 
        List.foldi cfg.global_types
          ~init:Variable.Map.empty
          ~f:(fun idx store _ -> Variable.Map.set store ~key:(Variable.Var (Var.Global idx)) ~data:(Abstract_store_domain.Value.top));
      store_operations = RICSet.singleton RIC.Top;
      unreachable = false }
  in
  cfg.return_types
  |> List.foldi ~init:state 
    ~f:(fun idx state _ -> 
      state |> Abstract_store_domain.set 
                ~var:(Variable.Var (Var.Return (cfg.idx, Int32.of_int_exn idx))) 
                ~vs:Abstract_store_domain.Value.top)
  |> Abstract_store_domain.set ~var:(Variable.Accessed) ~vs:(Abstract_store_domain.Value.top)
  |> Abstract_store_domain.set ~var:(Variable.MemorySize) ~vs:(Abstract_store_domain.Value.ValueSet RIC.positive_integers)


(** Summary used for an imported function.

    Modelled WASI imports preserve globals and either preserve or havoc memory
    according to [Value_set_options.ignore_imports]. Unknown imports preserve
    globals only when imports are ignored; otherwise globals and memory are
    treated as unknown. *)
let of_import (fct_idx : int32) (name : string) (nglobals : Int32.t) (_args : Type.t list) (ret : Type.t list) : t =
  let globals = List.init (Int32.to_int_exn nglobals) ~f:(fun i -> Variable.Var (Var.Global i)) in
  let summary = 
    {Abstract_store_domain.abstract_store = Variable.Map.empty; store_operations = RICSet.empty; unreachable = false}
    |> Abstract_store_domain.set ~var:Variable.MemorySize ~vs:(Abstract_store_domain.Value.ValueSet RIC.positive_integers) in
  match name with
  | "fd_write" | "fd_seek" | "fd_fdstat_get" ->
    let summary =
      (* Globals are unchanged *)
      globals |> List.fold 
        ~init:summary 
        ~f:(fun acc var -> Abstract_store_domain.set acc ~var:var ~vs:(Abstract_store_domain.Value.ValueSet (RIC.relative_ric (Variable.to_string var))))
    in
    let summary =
      (* Return values are unknown *)
      ret |> List.foldi ~init:summary
              ~f:(fun idx state _ -> state |> Abstract_store_domain.set ~var:(Variable.Var (Var.Return (fct_idx, Int32.of_int_exn idx))) ~vs:Abstract_store_domain.Value.top)
    in
    if !Value_set_options.ignore_imports then 
      (* Linear memory is considered to be unchanged *)
      let summary = 
        summary 
        |> Abstract_store_domain.set ~var:(Variable.Accessed) ~vs:(Abstract_store_domain.Value.bottom)
        |> Abstract_store_domain.set ~var:(Variable.entire_memory) ~vs:(Abstract_store_domain.Value.bottom) in
      { summary with store_operations = RICSet.empty }
    else
      (* Linear memory has been modified/accessed, but we don't know how: *)
      let summary = 
        summary
        |> Abstract_store_domain.set ~var:(Variable.Accessed) ~vs:(Abstract_store_domain.Value.top)
        |> Abstract_store_domain.set ~var:(Variable.entire_memory) ~vs:(Abstract_store_domain.Value.top)
      in
      { summary with store_operations = RICSet.singleton RIC.Top }
  | "fd_close" | "proc_exit" ->
    (* Globals are unchanged *)
    let summary =
      List.fold globals 
        ~init:summary 
        ~f:(fun acc var -> Abstract_store_domain.set acc ~var ~vs:(Abstract_store_domain.Value.ValueSet (RIC.relative_ric (Variable.to_string var))))
    in
    (* If present, return value is unknown: *)
    ret |> List.foldi ~init:summary
            ~f:(fun idx state _ -> state |> Abstract_store_domain.set ~var:(Variable.Var (Var.Return (fct_idx, Int32.of_int_exn idx))) ~vs:Abstract_store_domain.Value.top)
    (* Linear memory is unchanged, but we may have accessed all of it: *)
    |> Abstract_store_domain.set ~var:(Variable.Accessed) ~vs:(Abstract_store_domain.Value.top)
  | _ ->
    (* There is no way to know if global variables have been changed *)
    Log.warn (Printf.sprintf "Imported function is not modelled: %s" name);
    let summary =
      (* Globals: *)
      (if !Value_set_options.ignore_imports then
        List.fold globals 
          ~init:summary 
          ~f:(fun acc var -> Abstract_store_domain.set acc ~var ~vs:(Abstract_store_domain.Value.ValueSet (RIC.relative_ric (Variable.to_string var))))
      else
        (* Globals can point anywhere *)
        List.fold globals 
          ~init:summary 
          ~f:(fun acc var -> Abstract_store_domain.set acc ~var ~vs:(Abstract_store_domain.Value.top)))
    in
    let summary =
      (* If present, return value is unknown: *)
      ret |> List.foldi ~init:summary
            ~f:(fun idx state _ -> state |> Abstract_store_domain.set ~var:(Variable.Var (Var.Return (fct_idx, Int32.of_int_exn idx))) ~vs:Abstract_store_domain.Value.top)
    in
    (if !Value_set_options.ignore_imports then 
      ((* Linear memory is considered to be unchanged *)
      Abstract_store_domain.set summary ~var:(Variable.Accessed) ~vs:(Abstract_store_domain.Value.bottom))
    else
      ((* Linear memory may have been modified, but we don't know how: *)
      let summary = Abstract_store_domain.set summary ~var:(Variable.Accessed) ~vs:(Abstract_store_domain.Value.top) in
      { summary with store_operations = RICSet.singleton RIC.Top }))

(** Build the initial summary map for all functions.

    Defined functions receive either [bottom] or [top], while imported functions
    receive their import model. *)
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
    ~f:(fun summaries desc ->
        Int32Map.set summaries ~key:desc.idx ~data:(of_import desc.idx desc.name module_.nglobals desc.arguments desc.returns))

(** Extract the caller-visible part of an intraprocedural state. *)
let make (state : Abstract_store_domain.t) : t =
  { abstract_store =
      state.abstract_store
      |> Variable.Map.filter_keys 
          ~f:(fun key -> 
            match key with
            | Variable.Var Var.Global _
            | Variable.Var Var.Return _
            | Variable.Mem _
            | Variable.Accessed 
            | Variable.MemorySize -> true
            | _ -> false);
    store_operations = state.store_operations;
    unreachable = state.unreachable }

(** Substitute callee-relative origins with caller values.

    Relative globals, arguments, memory variables, and written-memory addresses
    are rewritten using [actual_values]. If rewriting a variable makes its
    address invalid, the associated value is set to top. *)
let update_relative_offsets (summary : t) ~(actual_values : RIC.t String.Map.t) : t =
  { abstract_store =
      Variable.Map.fold summary.abstract_store 
        ~init:Variable.Map.empty 
        ~f:(fun ~key:var ~data:vs acc -> 
          let key, invalid = Variable.update_relative_offset ~var ~actual_values in
          let data =
            if invalid then
              Abstract_store_domain.Value.top
            else
              Abstract_store_domain.Value.update_relative_offset vs actual_values
          in
          Variable.Map.set acc
            ~key
            ~data);
    store_operations = 
      summary.store_operations |> RICSet.map ~f:(fun ric_ -> RIC.update_relative_offset ~ric_ ~actual_values);
    unreachable = summary.unreachable }


(** Return the unique return value stored in a summary, if any. *)
let extract_return_values 
    (store : Value_set_abstraction.t Variable.Map.t) 
  : Value_set_abstraction.t list =
  store
  |> Map.keys
  |> List.filter_map 
      ~f:(fun v -> match v with
          | Variable.Var Var.Return _ -> Variable.Map.find store v
          | _ -> None)
        
  
  (* match ret_vars with
  | [] -> None
  | [ret] -> Variable.Map.find store ret
  | _ -> Log.error "Functions with multiple returns not yet implemented"; failwith "" *)

(** Apply a callee summary to a caller state.

    The summary is first rewritten with the caller's actual globals and
    arguments. Accessed memory is recorded, memory areas written by the summary
    are removed from the caller state, then summary globals, memory cells,
    return value, and memory size are merged into the caller state. *)
let apply 
    ~(summary : t) 
    ~(state : Abstract_store_domain.t) 
    ~(args : Var.t list)
    ~(return_variables : Var.t list)
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
    summary.store_operations
    |> RICSet.to_list
    |> List.map ~f:(fun addr -> Variable.Mem addr)
  in
  let affected_state =
    { Abstract_store_domain.abstract_store = 
        affected_mem_vars
        |> List.fold 
          ~init:Variable.Map.empty 
          ~f:(fun acc var -> acc |> Variable.Map.set ~key:var ~data:(Abstract_store_domain.Value.bottom));
      store_operations = RICSet.empty;
      unreachable = false } in
  let state = Abstract_store_domain.make_compatible ~this_store:state ~relative_to:affected_state in
  let store = 
    Variable.Map.fold
      state.abstract_store
      ~init:Variable.Map.empty
      ~f:(fun ~key:var ~data:vs acc ->
        match var with
        | Variable.Var _
        | Variable.Accessed
        | Variable.MemorySize -> Variable.Map.set acc ~key:var ~data:vs
        | Variable.Mem _ ->
          let is_safe =
            List.fold 
              affected_mem_vars 
              ~init:true 
              ~f:(fun acc v -> 
                acc 
                && Variable.comparable_offsets var v
                && not (Variable.share_addresses var v))
          in
          if is_safe then
            Variable.Map.set acc ~key:var ~data:vs
          else
            acc)
  in
  let state = { Abstract_store_domain.abstract_store = store; 
                store_operations = RICSet.union summary.store_operations state.store_operations;
                unreachable = state.unreachable || summary.unreachable } in
  (* Up to this point, the affected memory areas have been earased and the lists of affected/accessed addresses have been updated *)
  (* Update globals: *)
  let state = 
    summary.abstract_store 
    |> Variable.Map.filter_keys ~f:Variable.is_global
    |> Variable.Map.fold  
        ~init:state 
        ~f:(fun ~key ~data acc -> Abstract_store_domain.set acc ~var:key ~vs:data) in
  (* Update memory variables: *)
  let state =
    summary.abstract_store
    |> Variable.Map.filter_keys ~f:(fun key -> Variable.is_linear_memory key && not (Variable.equal key Variable.entire_memory))
    |> Variable.Map.fold 
      ~init:state
      ~f:(fun ~key ~data acc -> 
        Abstract_store_domain.set acc ~var:key ~vs:data)
    |> Abstract_store_domain.remove_pointers_to_top in
  (* Set return values: *)
  (* (match extract_return_value summary.abstract_store, return_variable with
  | Some vs, Some return_variable -> Abstract_store_domain.set state ~var:(Variable.Var return_variable) ~vs
  | None, None -> state
  | None, Some _ -> (Log.error "Summary is providing no return variable, but the calling state is expecting one."; assert false)
  | Some _, None -> (Log.error "Summary is providing a return variable, but the calling state is expecting none."; assert false)) *)
  (return_variables,
  summary.abstract_store |> extract_return_values)
  |>> List.fold2_exn
    ~init:state 
    ~f:(fun state ret value -> state |> Abstract_store_domain.set ~var:(Variable.Var ret) ~vs:value)
  (* let test = (summary.abstract_store |> extract_return_values,
  return_variables) in
  test
  |>>   *)
  (* | _ -> assert false TODO: error message? *)
  |> Abstract_store_domain.update_memory_size ~summary



(*
TTTTTTTTTTTTTTTTTTTTTTTEEEEEEEEEEEEEEEEEEEEEE   SSSSSSSSSSSSSSS TTTTTTTTTTTTTTTTTTTTTTT   SSSSSSSSSSSSSSS 
T:::::::::::::::::::::TE::::::::::::::::::::E SS:::::::::::::::ST:::::::::::::::::::::T SS:::::::::::::::S
T:::::::::::::::::::::TE::::::::::::::::::::ES:::::SSSSSS::::::ST:::::::::::::::::::::TS:::::SSSSSS::::::S
T:::::TT:::::::TT:::::TEE::::::EEEEEEEEE::::ES:::::S     SSSSSSST:::::TT:::::::TT:::::TS:::::S     SSSSSSS
TTTTTT  T:::::T  TTTTTT  E:::::E       EEEEEES:::::S            TTTTTT  T:::::T  TTTTTTS:::::S            
        T:::::T          E:::::E             S:::::S                    T:::::T        S:::::S            
        T:::::T          E::::::EEEEEEEEEE    S::::SSSS                 T:::::T         S::::SSSS         
        T:::::T          E:::::::::::::::E     SS::::::SSSSS            T:::::T          SS::::::SSSSS    
        T:::::T          E:::::::::::::::E       SSS::::::::SS          T:::::T            SSS::::::::SS  
        T:::::T          E::::::EEEEEEEEEE          SSSSSS::::S         T:::::T               SSSSSS::::S 
        T:::::T          E:::::E                         S:::::S        T:::::T                    S:::::S
        T:::::T          E:::::E       EEEEEE            S:::::S        T:::::T                    S:::::S
      TT:::::::TT      EE::::::EEEEEEEE:::::ESSSSSSS     S:::::S      TT:::::::TT      SSSSSSS     S:::::S
      T:::::::::T      E::::::::::::::::::::ES::::::SSSSSS:::::S      T:::::::::T      S::::::SSSSSS:::::S
      T:::::::::T      E::::::::::::::::::::ES:::::::::::::::SS       T:::::::::T      S:::::::::::::::SS 
      TTTTTTTTTTT      EEEEEEEEEEEEEEEEEEEEEE SSSSSSSSSSSSSSS         TTTTTTTTTTT       SSSSSSSSSSSSSSS   
*)

let%test_module "value-set summary tests" = (module struct


  let%test "value-set summary tests" =
    print_endline "\n_______ _________________ _______\n        Value-set summary        \n------- ----------------- -------\n"; true

  let test_label name = Printf.sprintf "%-45s" name

  let () = Value_set_options.show_intermediates := true


  let%test "update_relative_offsets: rewrites memory variable relative to argument" =
    let summary =
      { Abstract_store_domain.abstract_store =
          Variable.Map.empty
          |> Variable.Map.set
              ~key:(Variable.Mem (RIC.relative_ric "a"))
              ~data:(Abstract_store_domain.Value.ValueSet (RIC.(constant 42l + relative_ric "a")));
        store_operations = RICSet.empty;
        unreachable = false }
    in
    let actual_values =
      String.Map.empty
      |> Map.set ~key:"a" ~data:(RIC.of_int32 4l)
    in
    let actual = update_relative_offsets summary ~actual_values in
    let expected =
      { Abstract_store_domain.abstract_store =
          Variable.Map.empty
          |> Variable.Map.set
              ~key:(Variable.Mem (RIC.of_int32 4l))
              ~data:(Abstract_store_domain.Value.ValueSet (RIC.of_int32 46l));
        store_operations = RICSet.empty;
        unreachable = false }
    in
    let ok = Abstract_store_domain.equal actual expected in
    print_endline
      (Printf.sprintf "%s mem[%s] := %s with a = %s   ->   %s%s"
        (test_label "[update_relative_offsets: memory arg]")
        (RIC.to_string (RIC.relative_ric "a"))
        (Value_set_abstraction.to_string 
          (Abstract_store_domain.Value.ValueSet (RIC.(constant 42l + relative_ric "a"))))
        (RIC.to_string (RIC.of_int32 4l))
        (Abstract_store_domain.to_string actual)
        (if ok then "" else Printf.sprintf " (expected %s)" (to_string expected)));
    ok

  let%test "update_relative_offsets: rewrites store operations relative to argument" =
    let summary =
      { Abstract_store_domain.abstract_store = Variable.Map.empty;
        store_operations = RICSet.singleton (RIC.relative_ric "a");
        unreachable = false }
    in
    let actual_values =
      String.Map.empty
      |> Map.set ~key:"a" ~data:(RIC.of_int32 8l)
    in
    let actual = update_relative_offsets summary ~actual_values in
    let expected =
      { Abstract_store_domain.abstract_store = Variable.Map.empty;
        store_operations = RICSet.singleton (RIC.of_int32 8l);
        unreachable = false }
    in
    let ok = Abstract_store_domain.equal actual expected in
    print_endline
      (Printf.sprintf "%s store_operations {%s} with a = %s   ->   {%s}%s"
        (test_label "[update_relative_offsets: store ops]")
        (Abstract_store_domain.store_operations_to_string summary)
        (RIC.to_string (RIC.of_int32 8l))
        (Abstract_store_domain.store_operations_to_string actual)
        (if ok then "" else Printf.sprintf " (expected %s)" (to_string expected)));
    ok

  let%test "update_relative_offsets: non-singleton memory address stores top" =
    let summary =
      { Abstract_store_domain.abstract_store =
          Variable.Map.empty
          |> Variable.Map.set
              ~key:(Variable.Mem (RIC.relative_ric "a"))
              ~data:(Abstract_store_domain.Value.ValueSet (RIC.of_int32 42l));
        store_operations = RICSet.empty;
        unreachable = false }
    in
    let actual_values =
      String.Map.empty
      |> Map.set ~key:"a" ~data:(RIC.ric (1l, Int 0l, Int 1l, ("", 0l)))
    in
    let actual = update_relative_offsets summary ~actual_values in
    let expected =
      { Abstract_store_domain.abstract_store =
          Variable.Map.empty
          |> Variable.Map.set
              ~key:(Variable.Mem (RIC.ric (1l, Int 0l, Int 1l, ("", 0l))))
              ~data:Abstract_store_domain.Value.top;
        store_operations = RICSet.empty;
        unreachable = false }
    in
    let ok = Abstract_store_domain.equal actual expected in
    print_endline
      (Printf.sprintf "%s mem[%s] := %s with a = %s   ->   %s%s"
        (test_label "[update_relative_offsets: non-singleton mem]")
        (RIC.to_string (RIC.relative_ric "a"))
        (Value_set_abstraction.to_string (Abstract_store_domain.Value.ValueSet (RIC.of_int32 42l)))
        (RIC.to_string (RIC.ric (1l, Int 0l, Int 1l, ("", 0l))))
        (Abstract_store_domain.to_string actual)
        (if ok then "" else Printf.sprintf " (expected %s)" (to_string expected)));
    ok

  let%test "apply: written memory replaces caller value" =
    let state =
      { Abstract_store_domain.abstract_store =
          Variable.Map.empty
          |> Variable.Map.set
              ~key:(Variable.Mem (RIC.of_int32 4l))
              ~data:(Abstract_store_domain.Value.ValueSet (RIC.of_int32 7l))
          |> Variable.Map.set
              ~key:(Variable.Mem (RIC.of_int32 8l))
              ~data:(Abstract_store_domain.Value.ValueSet (RIC.of_int32 9l));
        store_operations = RICSet.empty;
        unreachable = false }
    in
    let summary =
      { Abstract_store_domain.abstract_store =
          Variable.Map.empty
          |> Variable.Map.set
              ~key:Variable.Accessed
              ~data:Abstract_store_domain.Value.bottom
          |> Variable.Map.set
              ~key:Variable.MemorySize
              ~data:Abstract_store_domain.Value.bottom
          |> Variable.Map.set
              ~key:(Variable.Mem (RIC.of_int32 4l))
              ~data:(Abstract_store_domain.Value.ValueSet (RIC.of_int32 42l));
        store_operations = RICSet.singleton (RIC.of_int32 4l);
        unreachable = false }
    in
    let actual = apply ~summary ~state ~args:[] ~return_variables:[] in
    let mem4 = Abstract_store_domain.get actual ~var:(Variable.Mem (RIC.of_int32 4l)) in
    let mem8 = Abstract_store_domain.get actual ~var:(Variable.Mem (RIC.of_int32 8l)) in
    let ok =
      Value_set_abstraction.equal mem4 (Abstract_store_domain.Value.ValueSet (RIC.of_int32 42l))
      && Value_set_abstraction.equal mem8 (Abstract_store_domain.Value.ValueSet (RIC.of_int32 9l))
      && RICSet.equal actual.store_operations (RICSet.singleton (RIC.of_int32 4l))
    in
    print_endline
      (Printf.sprintf "%s caller mem[4] = %s, summary mem[4] = %s, caller mem[8] = %s   ->   %s%s"
        (test_label "[apply: memory replacement]")
        (Value_set_abstraction.to_string (Abstract_store_domain.Value.ValueSet (RIC.of_int32 7l)))
        (Value_set_abstraction.to_string (Abstract_store_domain.Value.ValueSet (RIC.of_int32 42l)))
        (Value_set_abstraction.to_string (Abstract_store_domain.Value.ValueSet (RIC.of_int32 9l)))
        (Abstract_store_domain.to_string actual)
        (if ok then "" else " (expected mem[4] = 42, mem[8] = 9, store_operations = {4})"));
    ok

  let%test "apply: written memory removes overlapping caller value" =
    let overlapping_address = RIC.ric (1l, Int 3l, Int 6l, ("", 0l)) in
    let state =
      { Abstract_store_domain.abstract_store =
          Variable.Map.empty
          |> Variable.Map.set
              ~key:(Variable.Mem overlapping_address)
              ~data:(Abstract_store_domain.Value.ValueSet (RIC.of_int32 7l))
          |> Variable.Map.set
              ~key:(Variable.Mem (RIC.of_int32 8l))
              ~data:(Abstract_store_domain.Value.ValueSet (RIC.of_int32 9l));
        store_operations = RICSet.empty;
        unreachable = false }
    in
    let summary =
      { Abstract_store_domain.abstract_store =
          Variable.Map.empty
          |> Variable.Map.set
              ~key:Variable.Accessed
              ~data:Abstract_store_domain.Value.bottom
          |> Variable.Map.set
              ~key:Variable.MemorySize
              ~data:Abstract_store_domain.Value.bottom
          |> Variable.Map.set
              ~key:(Variable.Mem (RIC.of_int32 4l))
              ~data:(Abstract_store_domain.Value.ValueSet (RIC.of_int32 42l));
        store_operations = RICSet.singleton (RIC.ric (1l, Int 1l, Int 7l, ("", 0l)));
        unreachable = false }
    in
    let expected =
      { Abstract_store_domain.abstract_store =
          Variable.Map.empty
          |> Variable.Map.set
              ~key:Variable.Accessed
              ~data:Abstract_store_domain.Value.bottom
          |> Variable.Map.set
              ~key:(Variable.Mem (RIC.of_int32 4l))
              ~data:(Abstract_store_domain.Value.ValueSet (RIC.of_int32 42l))
          |> Variable.Map.set
              ~key:(Variable.Mem (RIC.of_int32 8l))
              ~data:(Abstract_store_domain.Value.ValueSet (RIC.of_int32 9l));
        store_operations = RICSet.singleton (RIC.ric (1l, Int 1l, Int 7l, ("", 0l)));
        unreachable = false }
    in
    let actual = apply ~summary ~state ~args:[] ~return_variables:[] in
    let ok = Abstract_store_domain.equal actual expected in
    print_endline
      (Printf.sprintf "%s caller mem[%s] overlaps summary write at %s   ->   %s%s"
        (test_label "[apply: overlapping memory]")
        (RIC.to_string overlapping_address)
        (RIC.to_string (RIC.of_int32 4l))
        (Abstract_store_domain.to_string actual)
        (if ok then "" else " (expected overlapping caller memory removed, mem[4] = 42, mem[8] = 9)"));
    ok

  let%test "apply: rewrites summary using caller argument" =
    let state =
      { Abstract_store_domain.abstract_store =
          Variable.Map.empty
          |> Variable.Map.set
              ~key:(Variable.Var (Var.Other "arg"))
              ~data:(Abstract_store_domain.Value.ValueSet (RIC.constant 4l))
          |> Variable.Map.set
              ~key:(Variable.Mem (RIC.of_int32 4l))
              ~data:(Abstract_store_domain.Value.ValueSet (RIC.of_int32 7l))
          |> Variable.Map.set
              ~key:(Variable.Mem (RIC.of_int32 8l))
              ~data:(Abstract_store_domain.Value.ValueSet (RIC.of_int32 9l));
        store_operations = RICSet.empty;
        unreachable = false }
    in
    let summary =
      { Abstract_store_domain.abstract_store =
          Variable.Map.empty
          |> Variable.Map.set
              ~key:Variable.Accessed
              ~data:Abstract_store_domain.Value.bottom
          |> Variable.Map.set
              ~key:Variable.MemorySize
              ~data:Abstract_store_domain.Value.bottom
          |> Variable.Map.set
              ~key:(Variable.Mem (RIC.relative_ric (Variable.to_string (Variable.Var (Var.Local 0)))))
              ~data:(Abstract_store_domain.Value.ValueSet (RIC.(constant 42l + relative_ric (Variable.to_string (Variable.Var (Var.Local 0))))));
        store_operations = RICSet.singleton (RIC.relative_ric (Variable.to_string (Variable.Var (Var.Local 0))));
        unreachable = false }
    in
    let expected =
      { Abstract_store_domain.abstract_store =
          Variable.Map.empty
          |> Variable.Map.set
              ~key:(Variable.Var (Var.Other "arg"))
              ~data:(Abstract_store_domain.Value.ValueSet (RIC.constant 4l))
          |> Variable.Map.set
              ~key:Variable.Accessed
              ~data:Abstract_store_domain.Value.bottom
          |> Variable.Map.set
              ~key:(Variable.Mem (RIC.of_int32 4l))
              ~data:(Abstract_store_domain.Value.ValueSet (RIC.of_int32 46l))
          |> Variable.Map.set
              ~key:(Variable.Mem (RIC.of_int32 8l))
              ~data:(Abstract_store_domain.Value.ValueSet (RIC.of_int32 9l));
        store_operations = RICSet.singleton (RIC.of_int32 4l);
        unreachable = false }
    in
    let actual = apply ~summary ~state ~args:[Var.Other "arg"] ~return_variables:[] in
    let ok = Abstract_store_domain.equal actual expected in
    print_endline
      (Printf.sprintf "%s initial state:%s,    arg0 = %s, summary: %s   ->   %s%s"
        (test_label "[apply: caller argument]")
        (Abstract_store_domain.to_string state)
        (RIC.to_string (RIC.of_int32 4l))
        (Abstract_store_domain.to_string summary)
        (Abstract_store_domain.to_string actual)
        (if ok then "" else Printf.sprintf " (expected %s)" (to_string expected)));
    ok

  let%test "apply: assigns summary return value to caller variable" =
    let state =
      { Abstract_store_domain.abstract_store =
          Variable.Map.empty
          |> Variable.Map.set
              ~key:(Variable.Var (Var.Other "ret"))
              ~data:(Abstract_store_domain.Value.ValueSet (RIC.of_int32 7l));
        store_operations = RICSet.empty;
        unreachable = false }
    in
    let summary =
      { Abstract_store_domain.abstract_store =
          Variable.Map.empty
          |> Variable.Map.set
              ~key:Variable.Accessed
              ~data:Abstract_store_domain.Value.bottom
          |> Variable.Map.set
              ~key:Variable.MemorySize
              ~data:Abstract_store_domain.Value.bottom
          |> Variable.Map.set
              ~key:(Variable.Var (Var.Return (0l,0l)))
              ~data:(Abstract_store_domain.Value.ValueSet (RIC.of_int32 42l));
        store_operations = RICSet.empty;
        unreachable = false }
    in
    let expected =
      { Abstract_store_domain.abstract_store =
          Variable.Map.empty
          |> Variable.Map.set
              ~key:(Variable.Var (Var.Other "ret"))
              ~data:(Abstract_store_domain.Value.ValueSet (RIC.of_int32 42l))
          |> Variable.Map.set
              ~key:Variable.Accessed
              ~data:Abstract_store_domain.Value.bottom
          |> Variable.Map.set
              ~key:(Variable.entire_memory)
              ~data:(Abstract_store_domain.Value.top);
        store_operations = RICSet.empty;
        unreachable = false }
    in
    let actual = apply ~summary ~state ~args:[] ~return_variables:[Var.Other "ret"] in
    let ok = Abstract_store_domain.equal actual expected in
    print_endline
      (Printf.sprintf "%s caller ret = %s, summary return = %s   ->   %s%s"
        (test_label "[apply: return value]")
        (Value_set_abstraction.to_string (Abstract_store_domain.Value.ValueSet (RIC.of_int32 7l)))
        (Value_set_abstraction.to_string (Abstract_store_domain.Value.ValueSet (RIC.of_int32 42l)))
        (Abstract_store_domain.to_string actual)
        (if ok then "" else Printf.sprintf " (expected %s)" (Abstract_store_domain.to_string expected)));
    ok

  let%test "apply: records accessed memory from summary" =
    let accessed_addresses = RIC.ric (1l, Int 4l, Int 8l, ("", 0l)) in
    let state =
      { Abstract_store_domain.abstract_store =
          Variable.Map.empty
          |> Variable.Map.set
              ~key:Variable.Accessed
              ~data:Abstract_store_domain.Value.bottom
          |> Variable.Map.set
              ~key:(Variable.Mem (RIC.of_int32 4l))
              ~data:(Abstract_store_domain.Value.ValueSet (RIC.of_int32 7l));
        store_operations = RICSet.empty;
        unreachable = false }
    in
    let summary =
      { Abstract_store_domain.abstract_store =
          Variable.Map.empty
          |> Variable.Map.set
              ~key:Variable.Accessed
              ~data:(Abstract_store_domain.Value.ValueSet accessed_addresses)
          |> Variable.Map.set
              ~key:Variable.MemorySize
              ~data:Abstract_store_domain.Value.bottom;
        store_operations = RICSet.empty;
        unreachable = false }
    in
    let actual = apply ~summary ~state ~args:[] ~return_variables:[] in
    let actual_accessed = Abstract_store_domain.get actual ~var:Variable.Accessed in
    let mem4 = Abstract_store_domain.get actual ~var:(Variable.Mem (RIC.of_int32 4l)) in
    let ok =
      Value_set_abstraction.equal actual_accessed (Abstract_store_domain.Value.ValueSet accessed_addresses)
      && Value_set_abstraction.equal mem4 (Abstract_store_domain.Value.ValueSet (RIC.of_int32 7l))
      && RICSet.is_empty actual.store_operations
    in
    print_endline
      (Printf.sprintf "%s summary accessed %s, caller mem[4] = %s   ->   %s%s"
        (test_label "[apply: accessed memory]")
        (RIC.to_string accessed_addresses)
        (Value_set_abstraction.to_string (Abstract_store_domain.Value.ValueSet (RIC.of_int32 7l)))
        (Abstract_store_domain.to_string actual)
        (if ok then "" else " (expected accessed memory recorded, mem[4] unchanged, no store operation)"));
    ok

  let%test "apply: joins summary accessed memory with caller accessed memory" =
    let caller_accessed = RIC.of_int32 2l in
    let summary_accessed = RIC.of_int32 4l in
    let expected_accessed = RIC.join caller_accessed summary_accessed in
    let state =
      { Abstract_store_domain.abstract_store =
          Variable.Map.empty
          |> Variable.Map.set
              ~key:Variable.Accessed
              ~data:(Abstract_store_domain.Value.ValueSet caller_accessed)
          |> Variable.Map.set
              ~key:(Variable.Mem (RIC.of_int32 4l))
              ~data:(Abstract_store_domain.Value.ValueSet (RIC.of_int32 7l));
        store_operations = RICSet.empty;
        unreachable = false }
    in
    let summary =
      { Abstract_store_domain.abstract_store =
          Variable.Map.empty
          |> Variable.Map.set
              ~key:Variable.Accessed
              ~data:(Abstract_store_domain.Value.ValueSet summary_accessed)
          |> Variable.Map.set
              ~key:Variable.MemorySize
              ~data:Abstract_store_domain.Value.bottom;
        store_operations = RICSet.empty;
        unreachable = false }
    in
    let actual = apply ~summary ~state ~args:[] ~return_variables:[] in
    let actual_accessed = Abstract_store_domain.get actual ~var:Variable.Accessed in
    let mem4 = Abstract_store_domain.get actual ~var:(Variable.Mem (RIC.of_int32 4l)) in
    let ok =
      Value_set_abstraction.equal actual_accessed (Abstract_store_domain.Value.ValueSet expected_accessed)
      && Value_set_abstraction.equal mem4 (Abstract_store_domain.Value.ValueSet (RIC.of_int32 7l))
      && RICSet.is_empty actual.store_operations
    in
    print_endline
      (Printf.sprintf "%s caller accessed %s, summary accessed %s, caller mem[4] = %s   ->   %s%s"
        (test_label "[apply: joined accessed memory]")
        (RIC.to_string caller_accessed)
        (RIC.to_string summary_accessed)
        (Value_set_abstraction.to_string (Abstract_store_domain.Value.ValueSet (RIC.of_int32 7l)))
        (Abstract_store_domain.to_string actual)
        (if ok then "" else Printf.sprintf " (expected accessed memory = %s, mem[4] unchanged, no store operation)" (RIC.to_string expected_accessed)));
    ok

  let%test "apply: updates memory size from summary" =
    let caller_memory_size = RIC.(positive_integers + constant 1l) in
    let summary_memory_size = RIC.(positive_integers + constant 3l) in
    let expected_memory_size = RIC.(positive_integers + constant 4l) in
    let state =
      { Abstract_store_domain.abstract_store =
          Variable.Map.empty
          |> Variable.Map.set
              ~key:Variable.Accessed
              ~data:Abstract_store_domain.Value.bottom
          |> Variable.Map.set
              ~key:Variable.MemorySize
              ~data:(Abstract_store_domain.Value.ValueSet caller_memory_size)
          |> Variable.Map.set
              ~key:(Variable.Mem (RIC.of_int32 4l))
              ~data:(Abstract_store_domain.Value.ValueSet (RIC.of_int32 7l));
        store_operations = RICSet.empty;
        unreachable = false }
    in
    let summary =
      { Abstract_store_domain.abstract_store =
          Variable.Map.empty
          |> Variable.Map.set
              ~key:Variable.Accessed
              ~data:Abstract_store_domain.Value.bottom
          |> Variable.Map.set
              ~key:Variable.MemorySize
              ~data:(Abstract_store_domain.Value.ValueSet summary_memory_size);
        store_operations = RICSet.empty;
        unreachable = false }
    in
    let actual = apply ~summary ~state ~args:[] ~return_variables:[] in
    let actual_memory_size = Abstract_store_domain.get actual ~var:Variable.MemorySize in
    let mem4 = Abstract_store_domain.get actual ~var:(Variable.Mem (RIC.of_int32 4l)) in
    let ok =
      Value_set_abstraction.equal actual_memory_size (Abstract_store_domain.Value.ValueSet expected_memory_size)
      && Value_set_abstraction.equal mem4 (Abstract_store_domain.Value.ValueSet (RIC.of_int32 7l))
      && RICSet.is_empty actual.store_operations
    in
    print_endline
      (Printf.sprintf "%s caller memory size %s, summary memory size %s, caller mem[4] = %s   ->   %s%s"
        (test_label "[apply: memory size]")
        (RIC.to_string caller_memory_size)
        (RIC.to_string summary_memory_size)
        (Value_set_abstraction.to_string (Abstract_store_domain.Value.ValueSet (RIC.of_int32 7l)))
        (Abstract_store_domain.to_string actual)
        (if ok then "" else Printf.sprintf " (expected memory size = %s, mem[4] unchanged, no store operation)" (RIC.to_string expected_memory_size)));
    ok

  
  let%test "apply: updates global from summary" =
    let global = Variable.Var (Var.Global 0) in
    let state =
      { Abstract_store_domain.abstract_store =
          Variable.Map.empty
          |> Variable.Map.set
              ~key:global
              ~data:(Abstract_store_domain.Value.ValueSet (RIC.of_int32 7l))
          |> Variable.Map.set
              ~key:Variable.Accessed
              ~data:Abstract_store_domain.Value.bottom
          |> Variable.Map.set
              ~key:Variable.MemorySize
              ~data:Abstract_store_domain.Value.bottom
          |> Variable.Map.set
              ~key:(Variable.Mem (RIC.of_int32 4l))
              ~data:(Abstract_store_domain.Value.ValueSet (RIC.of_int32 9l));
        store_operations = RICSet.empty;
        unreachable = false }
    in
    let summary =
      { Abstract_store_domain.abstract_store =
          Variable.Map.empty
          |> Variable.Map.set
              ~key:global
              ~data:(Abstract_store_domain.Value.ValueSet (RIC.of_int32 42l))
          |> Variable.Map.set
              ~key:Variable.Accessed
              ~data:Abstract_store_domain.Value.bottom
          |> Variable.Map.set
              ~key:Variable.MemorySize
              ~data:Abstract_store_domain.Value.bottom;
        store_operations = RICSet.empty;
        unreachable = false }
    in
    let actual = apply ~summary ~state ~args:[] ~return_variables:[] in
    let actual_global = Abstract_store_domain.get actual ~var:global in
    let mem4 = Abstract_store_domain.get actual ~var:(Variable.Mem (RIC.of_int32 4l)) in
    let ok =
      Value_set_abstraction.equal actual_global (Abstract_store_domain.Value.ValueSet (RIC.of_int32 42l))
      && Value_set_abstraction.equal mem4 (Abstract_store_domain.Value.ValueSet (RIC.of_int32 9l))
      && RICSet.is_empty actual.store_operations
    in
    print_endline
      (Printf.sprintf "%s caller global = %s, summary global = %s, caller mem[4] = %s   ->   %s%s"
        (test_label "[apply: global update]")
        (Value_set_abstraction.to_string (Abstract_store_domain.Value.ValueSet (RIC.of_int32 7l)))
        (Value_set_abstraction.to_string (Abstract_store_domain.Value.ValueSet (RIC.of_int32 42l)))
        (Value_set_abstraction.to_string (Abstract_store_domain.Value.ValueSet (RIC.of_int32 9l)))
        (Abstract_store_domain.to_string actual)
        (if ok then "" else " (expected global = 42, mem[4] unchanged, no store operation)"));
    ok

  let%test "apply: rewrites summary using caller global" =
    let global = Variable.Var (Var.Global 0) in
    let global_origin = Variable.to_string global in
    let state =
      { Abstract_store_domain.abstract_store =
          Variable.Map.empty
          |> Variable.Map.set
              ~key:global
              ~data:(Abstract_store_domain.Value.ValueSet (RIC.of_int32 10l))
          |> Variable.Map.set
              ~key:Variable.Accessed
              ~data:Abstract_store_domain.Value.bottom
          |> Variable.Map.set
              ~key:Variable.MemorySize
              ~data:Abstract_store_domain.Value.bottom
          |> Variable.Map.set
              ~key:(Variable.Mem (RIC.of_int32 4l))
              ~data:(Abstract_store_domain.Value.ValueSet (RIC.of_int32 9l));
        store_operations = RICSet.empty;
        unreachable = false }
    in
    let summary_global_value = RIC.(relative_ric global_origin + constant 5l) in
    let summary =
      { Abstract_store_domain.abstract_store =
          Variable.Map.empty
          |> Variable.Map.set
              ~key:global
              ~data:(Abstract_store_domain.Value.ValueSet summary_global_value)
          |> Variable.Map.set
              ~key:Variable.Accessed
              ~data:Abstract_store_domain.Value.bottom
          |> Variable.Map.set
              ~key:Variable.MemorySize
              ~data:Abstract_store_domain.Value.bottom;
        store_operations = RICSet.empty;
        unreachable = false }
    in
    let actual = apply ~summary ~state ~args:[] ~return_variables:[] in
    let actual_global = Abstract_store_domain.get actual ~var:global in
    let mem4 = Abstract_store_domain.get actual ~var:(Variable.Mem (RIC.of_int32 4l)) in
    let ok =
      Value_set_abstraction.equal actual_global (Abstract_store_domain.Value.ValueSet (RIC.of_int32 15l))
      && Value_set_abstraction.equal mem4 (Abstract_store_domain.Value.ValueSet (RIC.of_int32 9l))
      && RICSet.is_empty actual.store_operations
    in
    print_endline
      (Printf.sprintf "%s caller global = %s, summary global = %s, caller mem[4] = %s   ->   %s%s"
        (test_label "[apply: caller global]")
        (Value_set_abstraction.to_string (Abstract_store_domain.Value.ValueSet (RIC.of_int32 10l)))
        (Value_set_abstraction.to_string (Abstract_store_domain.Value.ValueSet summary_global_value))
        (Value_set_abstraction.to_string (Abstract_store_domain.Value.ValueSet (RIC.of_int32 9l)))
        (Abstract_store_domain.to_string actual)
        (if ok then "" else " (expected global = 15, mem[4] unchanged, no store operation)"));
    ok

  let%test "apply: rewrites memory write using caller global" =
    let global = Variable.Var (Var.Global 0) in
    let global_origin = Variable.to_string global in
    let summary_address = RIC.(relative_ric global_origin + constant 4l) in
    let expected_address = RIC.of_int32 14l in
    let state =
      { Abstract_store_domain.abstract_store =
          Variable.Map.empty
          |> Variable.Map.set
              ~key:global
              ~data:(Abstract_store_domain.Value.ValueSet (RIC.of_int32 10l))
          |> Variable.Map.set
              ~key:Variable.Accessed
              ~data:Abstract_store_domain.Value.bottom
          |> Variable.Map.set
              ~key:Variable.MemorySize
              ~data:Abstract_store_domain.Value.bottom
          |> Variable.Map.set
              ~key:(Variable.Mem expected_address)
              ~data:(Abstract_store_domain.Value.ValueSet (RIC.of_int32 7l))
          |> Variable.Map.set
              ~key:(Variable.Mem (RIC.of_int32 20l))
              ~data:(Abstract_store_domain.Value.ValueSet (RIC.of_int32 9l))
          |> Variable.Map.set
              ~key:(Variable.Mem (RIC.of_int32 16l))
              ~data:(Abstract_store_domain.Value.ValueSet (RIC.of_int32 999l));
        store_operations = RICSet.empty;
        unreachable = false }
    in
    let summary =
      { Abstract_store_domain.abstract_store =
          Variable.Map.empty
          |> Variable.Map.set
              ~key:(Variable.Mem summary_address)
              ~data:(Abstract_store_domain.Value.ValueSet (RIC.of_int32 42l))
          |> Variable.Map.set
              ~key:Variable.Accessed
              ~data:Abstract_store_domain.Value.bottom
          |> Variable.Map.set
              ~key:Variable.MemorySize
              ~data:Abstract_store_domain.Value.bottom;
        store_operations = RICSet.singleton (RIC.ric (1l, Int 0l, Int 6l, ("g0", 1l)));
        unreachable = false }
    in
    let expected =
      { Abstract_store_domain.abstract_store =
          Variable.Map.empty
          |> Variable.Map.set
              ~key:global
              ~data:(Abstract_store_domain.Value.ValueSet (RIC.of_int32 10l))
          |> Variable.Map.set
              ~key:Variable.Accessed
              ~data:Abstract_store_domain.Value.bottom
          |> Variable.Map.set
              ~key:Variable.MemorySize
              ~data:(Abstract_store_domain.Value.ValueSet RIC.positive_integers)
          |> Variable.Map.set
              ~key:(Variable.Mem expected_address)
              ~data:(Abstract_store_domain.Value.ValueSet (RIC.of_int32 42l))
          |> Variable.Map.set
              ~key:(Variable.Mem (RIC.of_int32 20l))
              ~data:(Abstract_store_domain.Value.ValueSet (RIC.of_int32 9l));
        store_operations = RICSet.singleton (RIC.ric (1l, Int 0l, Int 6l, ("", 11l)));
        unreachable = false }
    in
    let actual = apply ~summary ~state ~args:[] ~return_variables:[] in
    let ok = Abstract_store_domain.equal actual expected in
    print_endline
      (Printf.sprintf "%s caller: %s, \n\t\t\t\t\t      summary: %s   ->   %s%s"
        (test_label "[apply: global memory write]")
        (Abstract_store_domain.to_string state)
        (Abstract_store_domain.to_string summary)
        (Abstract_store_domain.to_string actual)
        (if ok then "" else "\n\t\t\t\t\texpected: " ^ Abstract_store_domain.to_string expected ^ " store_operations = {14})"));
    ok

  let%test "apply: rewrites memory write using caller global (relative offset)" =
    let global = Variable.Var (Var.Global 0) in
    let global_origin = Variable.to_string global in
    let summary_address = RIC.(relative_ric global_origin + constant 4l) in
    let expected_address = RIC.(constant 4l + relative_ric "x") in
    let state =
      { Abstract_store_domain.abstract_store =
          Variable.Map.empty
          |> Variable.Map.set
              ~key:global
              ~data:(Abstract_store_domain.Value.ValueSet (RIC.relative_ric "x"))
          |> Variable.Map.set
              ~key:Variable.Accessed
              ~data:Abstract_store_domain.Value.bottom
          |> Variable.Map.set
              ~key:Variable.MemorySize
              ~data:Abstract_store_domain.Value.bottom
          |> Variable.Map.set
              ~key:(Variable.Mem expected_address)
              ~data:(Abstract_store_domain.Value.ValueSet (RIC.of_int32 7l))
          |> Variable.Map.set
              ~key:(Variable.Mem (RIC.of_int32 20l))
              ~data:(Abstract_store_domain.Value.ValueSet (RIC.of_int32 9l))
          |> Variable.Map.set
              ~key:(Variable.Mem (RIC.of_int32 16l))
              ~data:(Abstract_store_domain.Value.ValueSet (RIC.of_int32 999l));
        store_operations = RICSet.empty;
        unreachable = false }
    in
    let summary =
      { Abstract_store_domain.abstract_store =
          Variable.Map.empty
          |> Variable.Map.set
              ~key:(Variable.Mem summary_address)
              ~data:(Abstract_store_domain.Value.ValueSet (RIC.of_int32 42l))
          |> Variable.Map.set
              ~key:Variable.Accessed
              ~data:Abstract_store_domain.Value.bottom
          |> Variable.Map.set
              ~key:Variable.MemorySize
              ~data:Abstract_store_domain.Value.bottom;
        store_operations = RICSet.singleton (RIC.ric (1l, Int 0l, Int 6l, ("g0", 1l)));
        unreachable = false }
    in
    let expected =
      { Abstract_store_domain.abstract_store =
          Variable.Map.empty
          |> Variable.Map.set
              ~key:global
              ~data:(Abstract_store_domain.Value.ValueSet (RIC.relative_ric "x"))
          |> Variable.Map.set
              ~key:Variable.Accessed
              ~data:Abstract_store_domain.Value.bottom
          |> Variable.Map.set
              ~key:Variable.MemorySize
              ~data:(Abstract_store_domain.Value.ValueSet RIC.positive_integers)
          |> Variable.Map.set
              ~key:(Variable.Mem expected_address)
              ~data:(Abstract_store_domain.Value.ValueSet (RIC.of_int32 42l));
        store_operations = RICSet.singleton (RIC.ric (1l, Int 0l, Int 6l, ("x", 1l)));
        unreachable = false }
    in
    let actual = apply ~summary ~state ~args:[] ~return_variables:[] in
    let ok = Abstract_store_domain.equal actual expected in
    print_endline
      (Printf.sprintf "%s caller: %s, \n\t\t\t\t\t      summary: %s   ->   %s%s"
        (test_label "[apply: global memory write (relative offset)]")
        (Abstract_store_domain.to_string state)
        (Abstract_store_domain.to_string summary)
        (Abstract_store_domain.to_string actual)
        (if ok then "" else "\n\t\t\t\t\texpected: " ^ Abstract_store_domain.to_string expected));
    ok

  let%test "apply: rewrites return value using caller argument" =
    let caller_arg = Var.Other "arg" in
    let callee_arg = Variable.Var (Var.Local 0) in
    let callee_arg_origin = Variable.to_string callee_arg in
    let summary_return = RIC.(relative_ric callee_arg_origin + constant 5l) in
    let state =
      { Abstract_store_domain.abstract_store =
          Variable.Map.empty
          |> Variable.Map.set
              ~key:(Variable.Var caller_arg)
              ~data:(Abstract_store_domain.Value.ValueSet (RIC.of_int32 10l))
          |> Variable.Map.set
              ~key:Variable.Accessed
              ~data:Abstract_store_domain.Value.bottom
          |> Variable.Map.set
              ~key:Variable.MemorySize
              ~data:Abstract_store_domain.Value.bottom;
        store_operations = RICSet.empty;
        unreachable = false }
    in
    let summary =
      { Abstract_store_domain.abstract_store =
          Variable.Map.empty
          |> Variable.Map.set
              ~key:(Variable.Var (Var.Return (0l,0l)))
              ~data:(Abstract_store_domain.Value.ValueSet summary_return)
          |> Variable.Map.set
              ~key:Variable.Accessed
              ~data:Abstract_store_domain.Value.bottom
          |> Variable.Map.set
              ~key:Variable.MemorySize
              ~data:Abstract_store_domain.Value.bottom;
        store_operations = RICSet.empty;
        unreachable = false }
    in
    let actual =
      apply ~summary ~state ~args:[caller_arg] ~return_variables:[Var.Return (0l,0l)]
    in
    let actual_ret = Abstract_store_domain.get actual ~var:(Variable.Var (Var.Return (0l,0l))) in
    let ok =
      Value_set_abstraction.equal actual_ret (Abstract_store_domain.Value.ValueSet (RIC.of_int32 15l))
      && RICSet.is_empty actual.store_operations
    in
    print_endline
      (Printf.sprintf "%s caller: %s, \n\t\t\t\t\t      summary: %s   ->   %s%s"
        (test_label "[apply: caller arg return]")
        (Abstract_store_domain.to_string state)
        (Abstract_store_domain.to_string summary)
        (Abstract_store_domain.to_string actual)
        (if ok then "" else " (expected ret = 15, no store operation)"));
    ok

  let%test "apply: rewrites return value using caller global" =
    let global = Variable.Var (Var.Global 0) in
    let global_origin = Variable.to_string global in
    let summary_return = RIC.(relative_ric global_origin + constant 5l) in
    let state =
      { Abstract_store_domain.abstract_store =
          Variable.Map.empty
          |> Variable.Map.set
              ~key:global
              ~data:(Abstract_store_domain.Value.ValueSet (RIC.of_int32 10l))
          |> Variable.Map.set
              ~key:Variable.Accessed
              ~data:Abstract_store_domain.Value.bottom
          |> Variable.Map.set
              ~key:Variable.MemorySize
              ~data:Abstract_store_domain.Value.bottom;
        store_operations = RICSet.empty;
        unreachable = false }
    in
    let summary =
      { Abstract_store_domain.abstract_store =
          Variable.Map.empty
          |> Variable.Map.set
              ~key:(Variable.Var (Var.Return (0l,0l)))
              ~data:(Abstract_store_domain.Value.ValueSet summary_return)
          |> Variable.Map.set
              ~key:Variable.Accessed
              ~data:Abstract_store_domain.Value.bottom
          |> Variable.Map.set
              ~key:Variable.MemorySize
              ~data:Abstract_store_domain.Value.bottom;
        store_operations = RICSet.empty;
        unreachable = false }
    in
    let expected =
      { Abstract_store_domain.abstract_store =
          Variable.Map.empty
          |> Variable.Map.set
              ~key:global
              ~data:(Abstract_store_domain.Value.ValueSet (RIC.of_int32 10l))
          |> Variable.Map.set
              ~key:Variable.Accessed
              ~data:Abstract_store_domain.Value.bottom
          |> Variable.Map.set
              ~key:Variable.MemorySize
              ~data:(Abstract_store_domain.Value.ValueSet RIC.positive_integers)
          |> Variable.Map.set
              ~key:(Variable.Var (Var.Return (0l,0l)))
              ~data:(Abstract_store_domain.Value.ValueSet (RIC.of_int32 15l));
        store_operations = RICSet.empty;
        unreachable = false }
    in
    let actual = apply ~summary ~state ~args:[] ~return_variables:[Var.Return (0l,0l)] in
    let ok = Abstract_store_domain.equal actual expected in
    print_endline
      (Printf.sprintf "%s caller: %s, \n\t\t\t\t\t      summary: %s   ->   %s%s"
        (test_label "[apply: caller global return]")
        (Abstract_store_domain.to_string state)
        (Abstract_store_domain.to_string summary)
        (Abstract_store_domain.to_string actual)
        (if ok then "" else "\n\t\t\t\t\texpected: " ^ Abstract_store_domain.to_string expected));
    ok

  let%test "apply: rewrites accessed memory using caller argument" =
    let caller_arg = Var.Other "arg" in
    let callee_arg = Variable.Var (Var.Local 0) in
    let callee_arg_origin = Variable.to_string callee_arg in
    let summary_accessed = RIC.(relative_ric callee_arg_origin + constant 4l) in
    let expected_accessed = RIC.of_int32 14l in
    let state =
      { Abstract_store_domain.abstract_store =
          Variable.Map.empty
          |> Variable.Map.set
              ~key:(Variable.Var caller_arg)
              ~data:(Abstract_store_domain.Value.ValueSet (RIC.of_int32 10l))
          |> Variable.Map.set
              ~key:Variable.Accessed
              ~data:Abstract_store_domain.Value.bottom
          |> Variable.Map.set
              ~key:Variable.MemorySize
              ~data:Abstract_store_domain.Value.bottom;
        store_operations = RICSet.empty;
        unreachable = false }
    in
    let summary =
      { Abstract_store_domain.abstract_store =
          Variable.Map.empty
          |> Variable.Map.set
              ~key:Variable.Accessed
              ~data:(Abstract_store_domain.Value.ValueSet summary_accessed)
          |> Variable.Map.set
              ~key:Variable.MemorySize
              ~data:Abstract_store_domain.Value.bottom;
        store_operations = RICSet.empty;
        unreachable = false }
    in
    let expected =
      { Abstract_store_domain.abstract_store =
          Variable.Map.empty
          |> Variable.Map.set
              ~key:(Variable.Var caller_arg)
              ~data:(Abstract_store_domain.Value.ValueSet (RIC.of_int32 10l))
          |> Variable.Map.set
              ~key:Variable.Accessed
              ~data:(Abstract_store_domain.Value.ValueSet expected_accessed)
          |> Variable.Map.set
              ~key:Variable.MemorySize
              ~data:(Abstract_store_domain.Value.ValueSet RIC.positive_integers);
        store_operations = RICSet.empty;
        unreachable = false }
    in
    let actual = apply ~summary ~state ~args:[caller_arg] ~return_variables:[] in
    let ok = Abstract_store_domain.equal actual expected in
    print_endline
      (Printf.sprintf "%s caller: %s, \n\t\t\t\t\t      summary: %s   ->   %s%s"
        (test_label "[apply: caller arg accessed]")
        (Abstract_store_domain.to_string state)
        (Abstract_store_domain.to_string summary)
        (Abstract_store_domain.to_string actual)
        (if ok then "" else "\n\t\t\t\t\texpected: " ^ Abstract_store_domain.to_string expected));
    ok

  let%test "apply: rewrites accessed memory using caller global" =
    let global = Variable.Var (Var.Global 0) in
    let global_origin = Variable.to_string global in
    let summary_accessed = RIC.(relative_ric global_origin + constant 4l) in
    let expected_accessed = RIC.of_int32 14l in
    let state =
      { Abstract_store_domain.abstract_store =
          Variable.Map.empty
          |> Variable.Map.set
              ~key:global
              ~data:(Abstract_store_domain.Value.ValueSet (RIC.of_int32 10l))
          |> Variable.Map.set
              ~key:Variable.Accessed
              ~data:Abstract_store_domain.Value.bottom
          |> Variable.Map.set
              ~key:Variable.MemorySize
              ~data:Abstract_store_domain.Value.bottom;
        store_operations = RICSet.empty;
        unreachable = false }
    in
    let summary =
      { Abstract_store_domain.abstract_store =
          Variable.Map.empty
          |> Variable.Map.set
              ~key:Variable.Accessed
              ~data:(Abstract_store_domain.Value.ValueSet summary_accessed)
          |> Variable.Map.set
              ~key:Variable.MemorySize
              ~data:Abstract_store_domain.Value.bottom;
        store_operations = RICSet.empty;
        unreachable = false }
    in
    let expected =
      { Abstract_store_domain.abstract_store =
          Variable.Map.empty
          |> Variable.Map.set
              ~key:global
              ~data:(Abstract_store_domain.Value.ValueSet (RIC.of_int32 10l))
          |> Variable.Map.set
              ~key:Variable.Accessed
              ~data:(Abstract_store_domain.Value.ValueSet expected_accessed)
          |> Variable.Map.set
              ~key:Variable.MemorySize
              ~data:(Abstract_store_domain.Value.ValueSet RIC.positive_integers);
        store_operations = RICSet.empty;
        unreachable = false }
    in
    let actual = apply ~summary ~state ~args:[] ~return_variables:[] in
    let ok = Abstract_store_domain.equal actual expected in
    print_endline
      (Printf.sprintf "%s caller: %s, \n\t\t\t\t\t      summary: %s   ->   %s%s"
        (test_label "[apply: caller global accessed]")
        (Abstract_store_domain.to_string state)
        (Abstract_store_domain.to_string summary)
        (Abstract_store_domain.to_string actual)
        (if ok then "" else "\n\t\t\t\t\texpected: " ^ Abstract_store_domain.to_string expected));
    ok

  let%test "apply: relative store operation only removes memory with same relative offset" =
    let written = RIC.ric (1l, Int 0l, Int 8l, ("x", 0l)) in
    let same_relative_offset = RIC.ric (1l, Int 4l, Int 4l, ("x", 30l)) in
    let different_relative_offset = RIC.ric (1l, Int 4l, Int 4l, ("y", 0l)) in
    let concrete_address = RIC.of_int32 4l in
    let state =
      { Abstract_store_domain.abstract_store =
          Variable.Map.empty
          |> Variable.Map.set
              ~key:(Variable.Mem same_relative_offset)
              ~data:(Abstract_store_domain.Value.ValueSet (RIC.of_int32 11l))
          |> Variable.Map.set
              ~key:(Variable.Mem written)
              ~data:(Abstract_store_domain.Value.ValueSet (RIC.of_int32 999l))
          |> Variable.Map.set
              ~key:(Variable.Mem different_relative_offset)
              ~data:(Abstract_store_domain.Value.ValueSet (RIC.of_int32 22l))
          |> Variable.Map.set
              ~key:(Variable.Mem concrete_address)
              ~data:(Abstract_store_domain.Value.ValueSet (RIC.of_int32 33l))
          |> Variable.Map.set
              ~key:Variable.Accessed
              ~data:Abstract_store_domain.Value.bottom
          |> Variable.Map.set
              ~key:Variable.MemorySize
              ~data:Abstract_store_domain.Value.bottom;
        store_operations = RICSet.empty;
        unreachable = false }
    in
    let summary =
      { Abstract_store_domain.abstract_store =
          Variable.Map.empty
          |> Variable.Map.set
              ~key:Variable.Accessed
              ~data:Abstract_store_domain.Value.bottom
          |> Variable.Map.set
              ~key:Variable.MemorySize
              ~data:Abstract_store_domain.Value.bottom;
        store_operations = RICSet.singleton written;
        unreachable = false }
    in
    let expected =
      { Abstract_store_domain.abstract_store =
          Variable.Map.empty
          |> Variable.Map.set
              ~key:(Variable.Mem same_relative_offset)
              ~data:(Abstract_store_domain.Value.ValueSet (RIC.of_int32 11l))
          |> Variable.Map.set
              ~key:Variable.Accessed
              ~data:Abstract_store_domain.Value.bottom
          |> Variable.Map.set
              ~key:Variable.MemorySize
              ~data:(Abstract_store_domain.Value.ValueSet RIC.positive_integers);
        store_operations = RICSet.singleton written;
        unreachable = false }
    in
    let actual = apply ~summary ~state ~args:[] ~return_variables:[] in
    let ok = Abstract_store_domain.equal actual expected in
    print_endline
      (Printf.sprintf "%s caller: %s, \n\t\t\t\t\t      summary's store_operations: %s   ->   %s%s"
        (test_label "[apply: relative write filtering]")
        (Abstract_store_domain.to_string state)
        (Abstract_store_domain.store_operations_to_string summary)
        (Abstract_store_domain.to_string actual)
        (if ok then "" else "\n\t\t\t\t\texpected: " ^ Abstract_store_domain.to_string expected));
    ok

  let%test "apply: relative store operation removes overlapping same-offset memory" =
    let written = RIC.ric (1l, Int 0l, Int 8l, ("x", 0l)) in
    let overlapping_same_offset = RIC.ric (1l, Int 4l, Int 4l, ("x", 0l)) in
    let disjoint_same_offset = RIC.ric (1l, Int 4l, Int 4l, ("x", 30l)) in
    let state =
      { Abstract_store_domain.abstract_store =
          Variable.Map.empty
          |> Variable.Map.set
              ~key:(Variable.Mem overlapping_same_offset)
              ~data:(Abstract_store_domain.Value.ValueSet (RIC.of_int32 11l))
          |> Variable.Map.set
              ~key:(Variable.Mem disjoint_same_offset)
              ~data:(Abstract_store_domain.Value.ValueSet (RIC.of_int32 22l))
          |> Variable.Map.set
              ~key:Variable.Accessed
              ~data:Abstract_store_domain.Value.bottom
          |> Variable.Map.set
              ~key:Variable.MemorySize
              ~data:Abstract_store_domain.Value.bottom;
        store_operations = RICSet.empty;
        unreachable = false }
    in
    let summary =
      { Abstract_store_domain.abstract_store =
          Variable.Map.empty
          |> Variable.Map.set
              ~key:Variable.Accessed
              ~data:Abstract_store_domain.Value.bottom
          |> Variable.Map.set
              ~key:Variable.MemorySize
              ~data:Abstract_store_domain.Value.bottom;
        store_operations = RICSet.singleton written;
        unreachable = false }
    in
    let expected =
      { Abstract_store_domain.abstract_store =
          Variable.Map.empty
          |> Variable.Map.set
              ~key:(Variable.Mem disjoint_same_offset)
              ~data:(Abstract_store_domain.Value.ValueSet (RIC.of_int32 22l))
          |> Variable.Map.set
              ~key:Variable.Accessed
              ~data:Abstract_store_domain.Value.bottom
          |> Variable.Map.set
              ~key:Variable.MemorySize
              ~data:(Abstract_store_domain.Value.ValueSet RIC.positive_integers);
        store_operations = RICSet.singleton written;
        unreachable = false }
    in
    let actual = apply ~summary ~state ~args:[] ~return_variables:[] in
    let ok = Abstract_store_domain.equal actual expected in
    print_endline
      (Printf.sprintf "%s caller: %s, \n\t\t\t\t\t      summary's store_operations: %s   ->   %s%s"
        (test_label "[apply: overlapping relative write]")
        (Abstract_store_domain.to_string state)
        (Abstract_store_domain.store_operations_to_string summary)
        (Abstract_store_domain.to_string actual)
        (if ok then "" else "\n\t\t\t\t\texpected: " ^ Abstract_store_domain.to_string expected));
    ok

  let%test "apply: top store operation removes all caller memory" =
    let state =
      { Abstract_store_domain.abstract_store =
          Variable.Map.empty
          |> Variable.Map.set
              ~key:(Variable.Mem (RIC.of_int32 4l))
              ~data:(Abstract_store_domain.Value.ValueSet (RIC.of_int32 11l))
          |> Variable.Map.set
              ~key:(Variable.Mem (RIC.ric (1l, Int 0l, Int 8l, ("x", 0l))))
              ~data:(Abstract_store_domain.Value.ValueSet (RIC.of_int32 22l))
          |> Variable.Map.set
              ~key:Variable.Accessed
              ~data:Abstract_store_domain.Value.bottom
          |> Variable.Map.set
              ~key:Variable.MemorySize
              ~data:Abstract_store_domain.Value.bottom;
        store_operations = RICSet.empty;
        unreachable = false }
    in
    let summary =
      { Abstract_store_domain.abstract_store =
          Variable.Map.empty
          |> Variable.Map.set
              ~key:Variable.Accessed
              ~data:Abstract_store_domain.Value.bottom
          |> Variable.Map.set
              ~key:Variable.MemorySize
              ~data:Abstract_store_domain.Value.bottom;
        store_operations = RICSet.singleton RIC.Top;
        unreachable = false }
    in
    let expected =
      { Abstract_store_domain.abstract_store =
          Variable.Map.empty
          |> Variable.Map.set
              ~key:Variable.Accessed
              ~data:Abstract_store_domain.Value.bottom
          |> Variable.Map.set
              ~key:Variable.MemorySize
              ~data:(Abstract_store_domain.Value.ValueSet RIC.positive_integers);
        store_operations = RICSet.singleton RIC.Top;
        unreachable = false }
    in
    let actual = apply ~summary ~state ~args:[] ~return_variables:[] in
    let ok = Abstract_store_domain.equal actual expected in
    print_endline
      (Printf.sprintf "%s caller: %s, \n\t\t\t\t\t      summary's store_operations: %s   ->   %s%s"
        (test_label "[apply: top write]")
        (Abstract_store_domain.to_string state)
        (Abstract_store_domain.store_operations_to_string summary)
        (Abstract_store_domain.to_string actual)
        (if ok then "" else "\n\t\t\t\t\texpected: " ^ Abstract_store_domain.to_string expected));
    ok

  let%test "apply: non-singleton rewritten memory write stores top" =
    let caller_arg = Var.Other "arg" in
    let callee_arg = Variable.Var (Var.Local 0) in
    let callee_arg_origin = Variable.to_string callee_arg in
    let caller_arg_value = RIC.ric (1l, Int 0l, Int 1l, ("", 0l)) in
    let summary_address = RIC.relative_ric callee_arg_origin in
    let expected_address = caller_arg_value in
    let state =
      { Abstract_store_domain.abstract_store =
          Variable.Map.empty
          |> Variable.Map.set
              ~key:(Variable.Var caller_arg)
              ~data:(Abstract_store_domain.Value.ValueSet caller_arg_value)
          |> Variable.Map.set
              ~key:Variable.Accessed
              ~data:Abstract_store_domain.Value.bottom
          |> Variable.Map.set
              ~key:Variable.MemorySize
              ~data:Abstract_store_domain.Value.bottom;
        store_operations = RICSet.empty;
        unreachable = false }
    in
    let summary =
      { Abstract_store_domain.abstract_store =
          Variable.Map.empty
          |> Variable.Map.set
              ~key:(Variable.Mem summary_address)
              ~data:(Abstract_store_domain.Value.ValueSet (RIC.of_int32 42l))
          |> Variable.Map.set
              ~key:Variable.Accessed
              ~data:Abstract_store_domain.Value.bottom
          |> Variable.Map.set
              ~key:Variable.MemorySize
              ~data:Abstract_store_domain.Value.bottom;
        store_operations = RICSet.singleton summary_address;
        unreachable = false }
    in
    let expected =
      { Abstract_store_domain.abstract_store =
          Variable.Map.empty
          |> Variable.Map.set
              ~key:(Variable.Var caller_arg)
              ~data:(Abstract_store_domain.Value.ValueSet caller_arg_value)
          |> Variable.Map.set
              ~key:Variable.Accessed
              ~data:Abstract_store_domain.Value.bottom
          |> Variable.Map.set
              ~key:Variable.MemorySize
              ~data:(Abstract_store_domain.Value.ValueSet RIC.positive_integers)
          |> Variable.Map.set
              ~key:(Variable.Mem expected_address)
              ~data:Abstract_store_domain.Value.top;
        store_operations = RICSet.singleton expected_address;
        unreachable = false }
    in
    let actual = apply ~summary ~state ~args:[caller_arg] ~return_variables:[] in
    let ok = Abstract_store_domain.equal actual expected in
    print_endline
      (Printf.sprintf "%s caller: %s, \n\t\t\t\t\t      summary: %s   ->   %s%s"
        (test_label "[apply: non-singleton write]")
        (Abstract_store_domain.to_string state)
        (Abstract_store_domain.to_string summary)
        (Abstract_store_domain.to_string actual)
        (if ok then "" else "\n\t\t\t\t\texpected: " ^ Abstract_store_domain.to_string expected));
    ok

  let%test "apply: unions caller and summary store operations" =
    let caller_written = RIC.of_int32 4l in
    let summary_written = RIC.of_int32 8l in
    let state =
      { Abstract_store_domain.abstract_store =
          Variable.Map.empty
          |> Variable.Map.set
              ~key:Variable.Accessed
              ~data:Abstract_store_domain.Value.bottom
          |> Variable.Map.set
              ~key:Variable.MemorySize
              ~data:Abstract_store_domain.Value.bottom;
        store_operations = RICSet.singleton caller_written;
        unreachable = false }
    in
    let summary =
      { Abstract_store_domain.abstract_store =
          Variable.Map.empty
          |> Variable.Map.set
              ~key:Variable.Accessed
              ~data:Abstract_store_domain.Value.bottom
          |> Variable.Map.set
              ~key:Variable.MemorySize
              ~data:Abstract_store_domain.Value.bottom;
        store_operations = RICSet.singleton summary_written;
        unreachable = false }
    in
    let expected =
      { Abstract_store_domain.abstract_store =
          Variable.Map.empty
          |> Variable.Map.set
              ~key:Variable.Accessed
              ~data:Abstract_store_domain.Value.bottom
          |> Variable.Map.set
              ~key:Variable.MemorySize
              ~data:(Abstract_store_domain.Value.ValueSet RIC.positive_integers);
        store_operations =
          RICSet.empty
          |> RICSet.add ~ric:caller_written
          |> RICSet.add ~ric:summary_written;
        unreachable = false }
    in
    let actual = apply ~summary ~state ~args:[] ~return_variables:[] in
    let ok = Abstract_store_domain.equal actual expected in
    print_endline
      (Printf.sprintf "%s caller's store_operations: %s, summary's store_operations: %s   ->   %s%s"
        (test_label "[apply: store operation union]")
        (Abstract_store_domain.store_operations_to_string state)
        (Abstract_store_domain.store_operations_to_string summary)
        (Abstract_store_domain.store_operations_to_string actual)
        (if ok then "" else "\n\t\t\t\t\texpected: " ^ Abstract_store_domain.to_string expected));
    ok
end)