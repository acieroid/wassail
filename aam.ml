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
      | Call of t * t list (* f x ... *)
    [@@deriving sexp, compare]
  end
  include T
  include Comparator.Make(T)
end

module ExpExamples = struct
  open Exp
  let fact body =
    LetRec
      ("fact", ["n"],
       (If (BinOp (Variable "n", Equals, Const 0),
            Const 1,
            BinOp (Variable "n", Times,
                   Call ((Variable "fact"),
                         [BinOp ((Variable "n"), Minus, Const 1)])))),
       body)

  (* Program 1: let fact (n) = ... in fact(5) *)
  let program1 =
    fact (Call (Variable "fact", [Const 5]))

  (* Program 2: let fact (n) = ... and f(x,y) = x+y in f(fact(5), 1) *)
  let program2 =
    (fact (Let ("f", Lambda (["x"; "y"],
                             BinOp (Variable "x", Plus, Variable "y")),
               Call (Variable "f", [Call (Variable "fact", [Const 5]); Const 1]))))
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
      | Call of Exp.t (* body *) * Env.t (* extended env *) * string option (* function name *)
    [@@deriving sexp, compare]
  end
  include T
  include Comparator.Make(T)
  module Set = Set.Make(T)
  module Map = Map.Make(T)
  let name (component : t) : string =
    match component with
    | Main _ -> "main"
    | Call (_, _, Some name) -> name
    | Call (_, _, None) -> "anonymous"
  let env (component : t) : Env.t =
    match component with
    | Main _ -> Env.empty
    | Call (_, env, _) -> env
  let exp (component : t) : Exp.t =
    match component with
    | Main e -> e
    | Call (e, _, _) -> e
end

module Deps = struct
  module T = struct
    type t = {
      reads: Component.Set.t Addr.Map.t ref; (* address -> components reading it *)
      write: Component.Set.t Addr.Map.t ref; (* address -> components writing to it *)
      calls: Component.Set.t Component.Map.t ref; (* callee -> caler map *)
      returns: Lattice.t Component.Map.t ref; (* component -> result map *)
      to_analyze : Component.Set.t ref; (* list of components to analyze *)
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
  }
  let register_read (deps : t) (component : Component.t) (a : Addr.t) : unit =
    deps.reads := Addr.Map.update !(deps.reads) a ~f:(function
        | Some comps -> Component.Set.add comps component
        | None -> Component.Set.singleton component)
  let register_alloc (_deps : t) (_component : Component.t) (_a : Addr.t) : unit =
    () (* we ignore allocs for now *)
  let register_call (deps : t) (caller : Component.t) (callee : Component.t) : unit =
      deps.calls := Component.Map.update !(deps.calls) callee ~f:(function
        | Some callers ->
          (* It's already been called, add the dependency *)
          Printf.printf "[%s calls %s] no new component\n" (Component.name caller) (Component.name callee);
          Component.Set.add callers caller
        | None ->
          (* It's a new call, add the dependency and track the new component *)
          Printf.printf "[%s calls %s] callee added as new component\n" (Component.name caller) (Component.name callee);
          deps.to_analyze := Component.Set.add !(deps.to_analyze) callee;
          Component.Set.singleton caller
      )
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
            Printf.printf "[result of %s: %s -> %s] retriggers\n" (Component.name component) (Sexp.to_string [%sexp (r : Lattice.t)]) (Sexp.to_string [%sexp (new_result : Lattice.t)]);
            retrigger ();
            new_result
          end
        | None ->
          if Lattice.is_bottom result then
            Lattice.bottom (* also safely ignored *)
          else begin
            (* same as above *)
            Printf.printf "[result of %s: no result -> %s] retriggers\n" (Component.name component) (Sexp.to_string [%sexp (result : Lattice.t)]);
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
    let rec eval (env : Env.t) : Exp.t -> Lattice.t = function
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
      (* We have to extend the env before constructing the closure, to make sure the recursive function can refer to itself *)
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
    | Exp.Call (f, args) as e ->
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
              let component' = Component.Call (body, env'', fname) in
              Deps.register_call deps component component';
              let result = Deps.result_of deps component' in
              Lattice.join acc result
            | None -> acc
          end
        | _ -> acc) in
    let res = eval (Component.env component) (Component.exp component) in
    Deps.register_result deps component res
end

module InterAnalysis = struct
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

let () =
  let (deps, _store) = InterAnalysis.analyze ExpExamples.program1 in
  Component.Map.iteri !(deps.returns) ~f:(fun ~key:component ~data:result ->
      Printf.printf "Component %s results in %s\n" (Component.name component) (Sexp.to_string [%sexp (result : Lattice.t)]))
