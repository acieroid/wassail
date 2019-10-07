open Core

module Symbol = String

module Exp = struct
  module T = struct
    type binary =
      | Plus | Minus | Times | Divides
      | Equals (* comparisons are treated as bin ops that return 0 (false) or 1 (true) *)
    [@@deriving sexp, compare]
    type unary =
      | UnaryMinus
      | Source | Sink (* needed for taint analysis *)
    [@@deriving sexp, compare]
    type t = exp
    and exp =
      | Variable of Symbol.t (* x *)
      | UnOp of unary * t (* - e *)
      | BinOp of t * binary * t (* e + e *)
      | Const of int (* 5 *)
      | Let of Symbol.t * t * t (* let x = 5 in e *)
      | LetRec of Symbol.t * Symbol.t list * t * t (* let f x ... = e in e' *)
      | If of t * t * t (* if e e' e'' *)
      | Lambda of Symbol.t list * t (* lambda x ... . e *)
      | Call of int * t * t list (* f^i x ... where i is a unique idientifer for that call*)
    [@@deriving sexp, compare]
  end
  include T
  include Comparator.Make(T)
  let rec contains_source (e : t) : bool =
    match e with
    | Variable _ -> false
    | UnOp (Source, _) -> true
    | UnOp (_, e) -> contains_source e
    | BinOp (left, _, right) -> contains_source left || contains_source right
    | Const _ -> false
    | Let (_, binding, body) -> contains_source binding || contains_source body
    | LetRec (_, _, _, body) -> (* We don't have to check the binding because it's a lambda *) contains_source body
    | If (cond, cons, alt) -> contains_source cond || contains_source cons || contains_source alt
    | Lambda _ -> false (* We're not actually looking *inside* lambdas *)
    | Call (_, f, args) -> contains_source f || (List.fold args ~init:false ~f:(fun acc e -> acc || contains_source e))
end

module ExpExamples = struct
  open Exp
  let fact body =
    LetRec
      ("fact", ["n"],
       (If (BinOp (Variable "n", Equals, Const 0),
            Const 1,
            BinOp (Variable "n", Times,
                   Call (0, (Variable "fact"),
                         [BinOp ((Variable "n"), Minus, Const 1)])))),
       body)

  (* Program 1: let fact (n) = ... in fact(5) *)
  let program1 =
    fact (Call (1, Variable "fact", [Const 5]))

  (* Program 2: let fact (n) = ... and f(x,y) = x+y in f(fact(5), 1) *)
  let program2 =
    (fact (Let ("f", Lambda (["x"; "y"],
                             BinOp (Variable "x", Plus, Variable "y")),
                Call (1, Variable "f", [Call (2, Variable "fact", [Const 5]); Const 1]))))

  (* Taint 1:
     let f x = source 1 in
     let g x = sink x in
     let h a b = a (b 0) in
     h g f
  *)
  let taint1 =
    (LetRec ("f", ["x"], UnOp (Source, Const 1),
             LetRec ("g", ["x"], UnOp (Sink, Variable "x"),
                     LetRec ("h", ["a"; "b"], Call (0, Variable "a", [Call (1, Variable "b", [Const 0])]),
                            Call (2, Variable "h", [Variable "g"; Variable "f"])))))
end

module Addr = struct
  module T = struct
    type t = Exp.t * string
    [@@deriving sexp, compare]
  end
  include T
  include Comparator.Make(T)
  let alloc (e : Exp.t) (name : string) = (e, name)
  module Set = Set.Make(T)
  module Map = Map.Make(T)
end

module Env = struct
  module T = struct
    type t = Addr.t String.Map.t
    [@@deriving sexp, compare]
  end
  include T
  include Comparator.Make(T)

  let empty = String.Map.empty
  let lookup_exn (env : t) (x : string) : Addr.t =
    match Map.find env x with
    | Some a -> a
    | None -> failwith (Printf.sprintf "Unbound variable: %s" x)
end

module Value = struct
  module T = struct
    type t =
      | Const
      | Closure of Symbol.t list * Exp.t * Symbol.t option * Env.t
    [@@deriving sexp, compare]
  end
  include T
  include Comparator.Make(T)
end

module Lattice = struct
  module ValueSet = Set.Make(Value)
  include ValueSet
  let join (x : t) (y : t) : t = union x y
  let bottom : t = empty
  let is_bottom : t -> bool = is_empty
  let inject (v : Value.t) = singleton v
  let apply_unary (op : Exp.unary) (v : t) : t =
    match op with
    | UnaryMinus ->
      fold v ~init:bottom ~f:(fun acc value ->
          match value with
          | Value.Const -> add acc Value.Const
          | _ -> acc)
    | Source -> v
    | Sink -> v
  let apply_binary (op : Exp.binary) (left : t) (right : t) : t =
    let apply_to_val (acc : t) (left : Value.t) (right : Value.t) : t =
      match (left, op, right) with
      | (Value.Const, Exp.Plus, Value.Const) -> add acc Value.Const
      | (Value.Const, Exp.Minus, Value.Const) -> add acc Value.Const
      | (Value.Const, Exp.Times, Value.Const) -> add acc Value.Const
      | (Value.Const, Exp.Divides, Value.Const) -> add acc Value.Const
      | (_, Exp.Equals, _) -> add acc Value.Const
      | _ -> acc
    in
    fold left ~init:bottom ~f:(fun acc v1 ->
        fold right ~init:acc ~f:(fun acc v2 ->
            apply_to_val acc v1 v2))
  let is_true (v : t) : bool =
    (* this may seem equal to simply "true", but it's not if v is bottom *)
    fold v ~init:false ~f:(fun _acc v ->
        match v with
        | Value.Const -> true
        | _ -> true) (* closures are considered true *)
  let is_false (v : t) : bool =
    fold v ~init:false ~f:(fun acc v ->
        match v with
        | Value.Const -> true
        | _ -> acc)
end

module Store = struct
  type t = Lattice.t Addr.Map.t
  let empty : t = Addr.Map.empty
  let lookup_bot (store : t) (a : Addr.t) : Lattice.t =
    match Map.find store a with
    | Some vs -> vs
    | None -> Lattice.bottom
  let extend (store : t) (a : Addr.t) (v : Lattice.t) : t =
    Map.update store a ~f:(function
        | Some oldv -> Lattice.join oldv v
        | None -> v)
end

module Component = struct
  module T = struct
    type t =
      | Main of Exp.t (* full program *)
      | Call of string list (* args *) * Exp.t (* body *) * Env.t (* extended env *) * string option (* function name *)
    [@@deriving sexp, compare]
  end
  include T
  include Comparator.Make(T)
  module Set = Set.Make(T)
  module Map = Map.Make(T)
  let name (component : t) : string =
    match component with
    | Main _ -> "main"
    | Call (_, _, _, Some name) -> name
    | Call (_, _, _, None) -> "anonymous"
  let env (component : t) : Env.t =
    match component with
    | Main _ -> Env.empty
    | Call (_, _, env, _) -> env
  let exp (component : t) : Exp.t =
    match component with
    | Main e -> e
    | Call (_, e, _, _) -> e
  let args (component : t) : string list =
    match component with
    | Main _ -> []
    | Call (args, _, _, _) -> args
end

module Deps = struct
  module T = struct
    type t = {
      reads: Component.Set.t Addr.Map.t ref; (* address -> components reading it *)
      write: Component.Set.t Addr.Map.t ref; (* address -> components writing to it *)
      calls: Component.Set.t Component.Map.t ref; (* callee -> caller map *)
      returns: Lattice.t Component.Map.t ref; (* component -> result map *)
      to_analyze : Component.Set.t ref; (* list of components to analyze *)
      cfa : Component.Set.t Int.Map.t ref (* call id -> closures *)
    }
    [@@deriving sexp, compare]
  end
  include T
  include Comparator.Make(T)
  let empty = {
    reads = ref Addr.Map.empty;
    write = ref Addr.Map.empty;
    calls = ref Component.Map.empty;
    returns = ref Component.Map.empty;
    to_analyze = ref Component.Set.empty;
    cfa = ref Int.Map.empty;
  }
  let register_read (deps : t) (component : Component.t) (a : Addr.t) : unit =
    deps.reads := Addr.Map.update !(deps.reads) a ~f:(function
        | Some comps -> Component.Set.add comps component
        | None -> Component.Set.singleton component)
  let register_alloc (_deps : t) (_component : Component.t) (_a : Addr.t) : unit =
    () (* we ignore allocs for now *)
  let register_call (deps : t) (id : int) (caller : Component.t) (callee : Component.t) : unit =
      deps.calls := Component.Map.update !(deps.calls) callee ~f:(function
        | Some callers ->
          (* It's already been called, add the dependency *)
          (* Printf.printf "[%s calls %s] no new component\n" (Component.name caller) (Component.name callee); *)
          Component.Set.add callers caller
        | None ->
          (* It's a new call, add the dependency and track the new component *)
          (* Printf.printf "[%s calls %s] callee added as new component\n" (Component.name caller) (Component.name callee); *)
          deps.to_analyze := Component.Set.add !(deps.to_analyze) callee;
          Component.Set.singleton caller
      );
      deps.cfa := Int.Map.update !(deps.cfa) id ~f:(function
          | Some comps -> Component.Set.add comps callee
          | None -> Component.Set.singleton callee)
  let register_result (deps : t) (component : Component.t) (result : Lattice.t) : unit =
    let retrigger () =
      deps.to_analyze := Component.Set.union !(deps.to_analyze)
          begin match Component.Map.find !(deps.calls) component with
            | Some callers -> callers
            | None -> Component.Set.empty
          end;
    in
    deps.returns := Component.Map.update !(deps.returns) component ~f:(function
        | Some r ->
          let new_result = Lattice.join r result in
          if Lattice.equal new_result r then
            (* New result is not different, safely do nothing *)
            r
          else begin
            (* New result differs, triggers all components that depend on it *)
            (* Printf.printf "[result of %s: %s -> %s] retriggers\n" (Component.name component) (Sexp.to_string [%sexp (r : Lattice.t)]) (Sexp.to_string [%sexp (new_result : Lattice.t)]); *)
            retrigger ();
            new_result
          end
        | None ->
          if Lattice.is_bottom result then
            Lattice.bottom (* also safely ignored *)
          else begin
            (* same as above *)
            (* Printf.printf "[result of %s: no result -> %s] retriggers\n" (Component.name component) (Sexp.to_string [%sexp (result : Lattice.t)]); *)
            retrigger ();
            result
          end)
  let result_of (deps : t) (component : Component.t) : Lattice.t =
    match Component.Map.find !(deps.returns) component with
    | Some v -> v
    | None -> Lattice.bottom
end

module IntraAnalysis = struct
  let analyze (component : Component.t) (store : Store.t ref) (deps : Deps.t) =
    let rec eval (env : Env.t) (e : Exp.t) : Lattice.t =
      (* print_s [%sexp (e : Exp.t)]; *)
      match e with
      | Exp.Variable x ->
        let addr = (Env.lookup_exn env x) in
        Deps.register_read deps component addr;
        Store.lookup_bot !store addr
      | Exp.UnOp (unary, e) ->
        let v = eval env e in
        Lattice.apply_unary unary v
      | Exp.BinOp (left, bin, right) ->
        let (v1, v2) = (eval env left, eval env right) in
        Lattice.apply_binary bin v1 v2
      | Exp.Const _n -> Lattice.inject Value.Const
      | Exp.Let (x, binding, body) as e ->
        let v = eval env binding in
        let addr = Addr.alloc e x in
        Deps.register_alloc deps component addr;
      store := Store.extend !store addr v;
        eval (Map.update env x ~f:(fun _ -> addr)) body
      | Exp.LetRec (name, args, fbody, body) as e ->
        let addr = Addr.alloc e name in
        (* We have to extend the env before constructing the closure, to make
           sure the recursive function can refer to itself *)
        let env' = (Map.update env name ~f:(fun _ -> addr)) in
        let v = Lattice.inject (Value.Closure (args, fbody, Some name, env')) in
        Deps.register_alloc deps component addr;
        store := Store.extend !store addr v;
        eval env' body
      | Exp.If (condition, consequence, alternative) ->
        let v = eval env condition in
        Lattice.join
          (if Lattice.is_true v then eval env consequence else Lattice.bottom)
          (if Lattice.is_false v then eval env alternative else Lattice.bottom)
      | Exp.Lambda (args, body) ->
        Lattice.inject (Value.Closure (args, body, None, env))
      | Exp.Call (id, f, args) as e ->
        let operator = eval env f in
        let operands = List.map ~f:(eval env) args in
        Lattice.fold operator ~init:Lattice.bottom ~f:(fun acc vrator ->
            match vrator with
            | Value.Closure (args, body, fname, env') ->
              begin match (List.zip args operands) with
                | Some argsops ->
                  let env'' = List.fold argsops ~init:env' ~f:(fun accenv (name, v) ->
                      let addr = Addr.alloc e name in
                      Deps.register_alloc deps component addr;
                      store := Store.extend !store addr v;
                      Map.update accenv name ~f:(function _ -> addr)) in
                  let component' = Component.Call (args, body, env'', fname) in
                  Deps.register_call deps id component component';
                  let result = Deps.result_of deps component' in
                  Lattice.join acc result
                | None -> acc
              end
            | _ -> acc)
    in
    let res = eval (Component.env component) (Component.exp component) in
    Deps.register_result deps component res
end

module ModularAnalysis = struct
  let analyze e =
    let store = ref Store.empty in
    let deps = Deps.empty in
    let visited = ref Component.Set.empty in
    let worklist = Queue.create () in
    let initial = Component.Main e in
    Queue.enqueue worklist initial;
    let rec loop () =
      match Queue.dequeue worklist with
      | Some component ->
        Printf.printf "Analysis of %s\n" (Component.name component);
        if not (Component.Set.mem !visited component) then begin
          IntraAnalysis.analyze component store deps;
          Component.Set.iter !(deps.to_analyze) ~f:(fun comp ->
              visited := Component.Set.remove !visited comp;
              Queue.enqueue worklist comp);
          deps.to_analyze := Component.Set.empty;
          loop ()
        end
        else ()
      | None -> () (* empty worklist, finished *)
    in
    loop ();
    (deps, store)
end

module TaintAnalysis = struct
  module Taint = struct
    module T = struct
      type t = Tainted | Untainted
      [@@deriving sexp, compare]
    end
    include T
    include Comparator.Make(T)
    let join (x : t) (y : t) : t =
      match x with
      | Tainted -> Tainted
      | Untainted -> y
  end
  module TaintEnv = struct
    module T = struct
      type t = Taint.t String.Map.t
      [@@deriving sexp, compare]
    end
    include T
    include Comparator.Make(T)
    let lookup_exn (env : t) (x : string) =
      match Map.find env x with
      | Some v -> v
      | None ->
        Printf.printf "Unbound taint variable %s\n" x;
        Untainted
    let empty : t = String.Map.empty
  end

  module CompEnvPair = struct
    module T = struct
      type t = Component.t * TaintEnv.t
      [@@deriving sexp, compare]
    end
    include T
    include Comparator.Make(T)
    module Map = Map.Make(T)
  end

  let analyze (e : Exp.t) =
    let (deps, _store) = ModularAnalysis.analyze e in
    Printf.printf "-------------\n Taint phase starting\n";
    let cache = ref CompEnvPair.Map.empty in
    let rec track_taint (comp : Component.t) (env : TaintEnv.t) : Taint.t =
      Printf.printf "Going through component %s\n" (Component.name comp);
      (* Taint is propagated through:
         1. return values (see example program 1, 2) -> need for taint return values
         2. arguments (see example program 1, 2) -> need for taint environment
         3. control-flow (TODO *)
      (* If we have a tagging of expression locations and a mapping (build from modf)
         Call -> Function
         we can derive the taint:
         var^l x -> use taint store
         let x = e in e' -> bind x in taint store, taint value is same as of e'
         f x -> taint is taint of body of e, with x bound in the taint store (use loc -> value) *)

      let rec eval (env : TaintEnv.t) (e : Exp.t) : Taint.t =
        match e with
        | Exp.Variable x -> TaintEnv.lookup_exn env x
        | Exp.UnOp (Exp.UnaryMinus, e) -> eval env e
        | Exp.UnOp (Exp.Source, e) ->
          let _ = eval env e in
          Taint.Tainted
        | Exp.UnOp (Exp.Sink, e) ->
          Printf.printf "I see a sink!\n";
          Printf.printf "Its arg is %s and it is %s\n" (Sexp.to_string [%sexp (e : Exp.t)]) (Sexp.to_string [%sexp (eval env e : Taint.t)]);
          begin match eval env e with
          | Taint.Tainted -> failwith "TAINT FLOW DETECTED (TODO)"
          | Taint.Untainted -> Taint.Untainted
          end
        | Exp.BinOp (left, _, right) -> Taint.join (eval env left) (eval env right)
        | Exp.Const _ -> Taint.Untainted
        | Exp.Let (x, binding, body) ->
          eval (Map.update env x ~f:(fun _ -> eval env binding)) body
        | Exp.LetRec (name, _args, _fbody, body) ->
          (* We can safely ignore fbody because it will evaluate to a lambda.
             But we do have to extend the environment to mark `name` as untainted *)
          let env' = Map.update env name ~f:(fun _ -> Taint.Untainted) in
          eval env' body
        | Exp.If (condition, consequence, alternative) ->
          let _ = eval env condition in
          Taint.join (eval env consequence) (eval env alternative)
        | Exp.Lambda _ ->
          Taint.Untainted
        | Exp.Call (id, f, args) ->
          let _ = eval env f in (* TODO: control taint *)
          begin match Int.Map.find !(deps.cfa) id with
            | Some callees ->
              Printf.printf "Callees are: ";
              Component.Set.iter callees ~f:(fun c -> Printf.printf "%s " (Component.name c));
              Printf.printf "\n";
              let res = Component.Set.fold callees ~init:Taint.Untainted ~f:(fun acc callee ->
                  Printf.printf "Call of %s to %s\n" (Component.name comp) (Component.name callee);
                  Printf.printf "foo ...\n";
                  let foo = (List.fold (List.zip_exn (Component.args callee) args)
                          ~init:env
                          ~f:(fun env' (arg, e) ->
                              Printf.printf "Argument %s exp is %s\n" arg (Sexp.to_string [%sexp (e : Exp.t)]);
                              Printf.printf "Argument %s is %s\n" arg (Sexp.to_string [%sexp (eval env e : Taint.t)]);
                              Map.update env' arg ~f:(fun _ ->
                                  print_s [%sexp (e : Exp.t)];
                                  eval env e))) in
                  Printf.printf "foo computed\n";
                  let return = (track_taint callee foo) in
                  Printf.printf "Call from %s to %s results in %s\n" (Component.name comp) (Component.name callee) (Sexp.to_string [%sexp (return : Taint.t)]);
                  Taint.join acc return) in
              Printf.printf "Calls done....";
              res
            | None -> failwith "No call" (* no call, probably a mistake *)
          end
      in
      match Map.find !cache (comp, env) with
      | Some res ->
        Printf.printf "[%s] Cached result: %s\n" (Component.name comp) (Sexp.to_string [%sexp (res : Taint.t)]);
        res(* result already computed, return it *)
      | None ->
        Printf.printf "[%s] No cached result, computing\n" (Component.name comp);
        (* First set it to untainted in case of recursion (TODO: make sure this is correct) *)
        cache := Map.update !cache (comp, env) ~f:(fun _ -> Taint.Untainted);
        (* Analyze this component *)
        let taint = eval env (Component.exp comp) in
        (* Update the cache *)
        cache := Map.update !cache (comp, env) ~f:(fun _ -> taint);
        taint
    in
    let rec track_taint_main (comp : Component.t) (env : TaintEnv.t) : unit =
      Printf.printf "Tracking taint in component %s\n" (Component.name comp);
      match track_taint comp env with
      | Untainted ->
        Printf.printf "Untainted, everything is fine in %s\n" (Component.name comp)
      | Tainted ->
        Printf.printf "Tainted, going to callers %s\n" (Component.name comp);
        begin match Component.Map.find !(deps.calls) comp with
          | Some callers -> Component.Set.iter callers ~f:(fun c ->
              Printf.printf "%s calls %s\n" (Component.name c) (Component.name comp);
              (* Note: we use an empty taint environment here, where everything
                 is untainted. That should not be an issue because we are only
                 interested in tracking taint coming from the result of the
                 current component. Taint coming from somewhere else will be
                 analyzed in another call to track_taint_main *)
              track_taint_main c TaintEnv.empty)
          | None -> ()
        end in

    let components = Component.Map.keys !(deps.returns) in
    List.iter
      (List.filter components
         (* Possible optimization: we basically already have a call graph
            computed. We can restrict our search to sources and sinks that are
            connected in the call graph *)
         ~f:(fun comp ->
             let res = Exp.contains_source (Component.exp comp) in
             Printf.printf "Does component %s contains a source? %b\n" (Component.name comp) res;
             res
           ))
      ~f:(fun comp ->
          Printf.printf "Tracking taint in component %s\n" (Component.name comp);
          track_taint_main comp TaintEnv.empty)
  (* TODO: once we found a component whose result is tainted, we have to go to its caller components! *)
end

let () =
  TaintAnalysis.analyze ExpExamples.taint1
(*  let (deps, _store) = ModularAnalysis.analyze ExpExamples.program1 in
  Component.Map.iteri !(deps.returns) ~f:(fun ~key:component ~data:result ->
      Printf.printf "Component %s results in %s\n" (Component.name component) (Sexp.to_string [%sexp (result : Lattice.t)]))
*)
