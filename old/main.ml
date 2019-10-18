open Core
open Incr_engine
open Incr.Let_syntax

module Symbol = String

module Exp = struct
  type binary =
    | Plus | Minus | Times | Divides
    | Equals (* comparisons are treated as bin ops that return 0 (false) or 1 (true) *)
  type unary =
    | UnaryMinus
    | Source | Sink (* needed for taint analysis *)
  type t = exp Incr.t
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
end

module Value = struct
  type t =
    | Const of int
    | Closure of Symbol.t list * Exp.t * Symbol.t option * env
  and env = t String.Map.t
  let sexp_of_t = function
    | Const n -> Sexp.Atom (string_of_int n)
    | Closure _ -> Sexp.Atom ("clo")
end

let lookup env var =
  match Map.find env var with
  | None -> failwith "unbound variable"
  | Some v -> v

(* let rec eval (env : Value.env) (exp : Exp.t) : Value.t Incr.t =
  Incr.bind exp ~f:(function
      | Exp.Variable x ->
         Incr.const (lookup env x)
      | Exp.Const n -> Incr.const (Value.Const n)
      | Exp.BinOp (left, op, right) ->
          let%map left = eval env left
          and right = eval env right in
          begin match (left, op, right) with
          | (Value.Const x, Exp.Plus, Value.Const y) -> Value.Const (x + y)
          | (Value.Const x, Exp.Minus, Value.Const y) -> Value.Const (x - y)
          | (Value.Const x, Exp.Times, Value.Const y) -> Value.Const (x * y)
          | (Value.Const x, Exp.Divides, Value.Const y) -> Value.Const (x / y)
          | (Value.Const x, Exp.Equals, Value.Const y) -> Value.Const (if x = y then 1 else 0)
          | _ -> failwith "Incorrect binary operation"
          end
      | Exp.UnOp (op, arg) ->
         let%map v = eval env arg in
         begin match (op, v) with
         | (Exp.UnaryMinus, Value.Const x) -> Value.Const (- x)
         | (Exp.UnaryMinus, _) -> failwith "can't minus a closure"
         | (Exp.Source, v) -> v
         | (Exp.Sink, v) -> v
         end
      | Exp.Let (x, binding, body) ->
         let%bind v = eval env binding in
         eval (Map.update env x ~f:(function _ -> v)) body
      | Exp.LetRec (name, args, fbody, body) ->
         let v = Value.Closure (args, fbody, Some name, env) in
         eval (Map.update env name ~f:(function _ -> v)) body
      | Exp.If (condition, consequence, alternative) ->
         let%bind v = eval env condition in
         begin match v with
         | Value.Const 0 -> (* false *) eval env alternative
         | _ -> (* true *) eval env consequence
         end
      | Exp.Lambda (args, body) ->
         Incr.const (Value.Closure (args, body, None, env))
      | Exp.Call (f, args) ->
         let%bind operator = eval env f
         and operands = Incr.all (List.map ~f:(eval env) args) in
         begin match operator with
         | Value.Closure (args, body, fname, env') ->
            begin match (List.zip args operands) with
            | Some argsops ->
               let env'' = List.fold argsops ~init:env' ~f:(fun accenv (name, v) ->
                               Map.update accenv name ~f:(function _ -> v)) in
               begin match fname with
               | None -> eval env'' body (* not a recursive function *)
               | Some f -> eval (Map.update env'' f ~f:(function _ -> operator)) body (* recursive function, bind it *)
               end
            | None -> failwith "invalid call, arity does not match"
            end
         | _ -> failwith "Not a closure"
         end
    )
 *)
module TaintValue = struct
  type taint = Tainted | Untainted
  type value =
    | Const of int
    | Closure of Symbol.t list * Exp.t * Symbol.t option * env
  and t = value * taint
  and env = t String.Map.t
  let sexp_of_t = function
    | (Const n, _) -> Sexp.Atom (string_of_int n)
    | (Closure _, _) -> Sexp.Atom ("clo")
  let const n = (Const n, Untainted)
  let value v = (v, Untainted)
  let join t1 t2 =
    match t1 with
    | Tainted -> Tainted
    | Untainted -> t2
end

module Error = struct
  type t =
    | TaintError of TaintValue.value
    | UnboundVariable of Symbol.t
    | ApplicationError
  let sexp_of_t = function
    | TaintError _ -> Sexp.Atom "TaintError"
    | UnboundVariable _ -> Sexp.Atom "UnboundVariable"
    | ApplicationError -> Sexp.Atom "ApplicationError"
end

let show x =
  Incr.stabilize ();
  print_s [%sexp (Incr.Observer.value_exn x : ((TaintValue.t, Error.t) Either.t))]


let lookup_either (env : TaintValue.env) x : ((TaintValue.t, Error.t) Either.t) =
  match Map.find env x with
  | Some v -> Either.First v
  | None -> Either.Second (Error.UnboundVariable x)

let rec eval (env : TaintValue.env) (exp : Exp.t) : ((TaintValue.t, Error.t) Either.t) Incr.t =
  Incr.bind exp ~f:(function
      | Exp.Variable x ->
         Incr.const (lookup_either env x)
      | Exp.Const n -> Incr.const (Either.First (TaintValue.const n))
      | Exp.BinOp (left, op, right) ->
          let%map left = eval env left
          and right = eval env right in
          begin match (left, op, right) with
          | (Either.First (TaintValue.Const x, t1), Exp.Plus,    Either.First (TaintValue.Const y, t2)) -> Either.First (TaintValue.Const (x + y), (TaintValue.join t1 t2))
          | (Either.First (TaintValue.Const x, t1), Exp.Minus,   Either.First (TaintValue.Const y, t2)) -> Either.First (TaintValue.Const (x - y), (TaintValue.join t1 t2))
          | (Either.First (TaintValue.Const x, t1), Exp.Times,   Either.First (TaintValue.Const y, t2)) -> Either.First (TaintValue.Const (x * y), (TaintValue.join t1 t2))
          | (Either.First (TaintValue.Const x, t1), Exp.Divides, Either.First (TaintValue.Const y, t2)) -> Either.First (TaintValue.Const (x / y), (TaintValue.join t1 t2))
          | (Either.First (TaintValue.Const x, t1), Exp.Equals,  Either.First (TaintValue.Const y, t2)) -> Either.First (TaintValue.Const (if x = y then 1 else 0), (TaintValue.join t1 t2))
          | _ -> Either.Second Error.ApplicationError
          end
      | Exp.UnOp (op, arg) ->
         let%map v = eval env arg in
         begin match (op, v) with
         | (Exp.UnaryMinus, Either.First (TaintValue.Const x, t)) -> Either.First (TaintValue.Const (- x), t)
         | (Exp.UnaryMinus, _) -> Either.Second Error.ApplicationError
         | (Exp.Source, Either.First (v, _)) -> Either.First (v, TaintValue.Tainted)
         | (Exp.Source, err) -> err
         | (Exp.Sink, Either.First (v, Tainted)) -> Either.Second (Error.TaintError v)
         | (Exp.Sink, v) -> v
         end
      | Exp.Let (x, binding, body) ->
         let%bind v = eval env binding in
         begin match v with
         | Either.First v -> eval (Map.update env x ~f:(function _ -> v)) body
         | err -> Incr.const err
         end
      | Exp.LetRec (name, args, fbody, body) ->
         let v = TaintValue.value (TaintValue.Closure (args, fbody, Some name, env)) in
         eval (Map.update env name ~f:(function _ -> v)) body
      | Exp.If (condition, consequence, alternative) ->
         let%bind v = eval env condition in
         begin match v with (* TODO: taint control flow *)
         | Either.First (TaintValue.Const 0, _) -> (* false *) eval env alternative
         | Either.First _ -> (* true *) eval env consequence
         | err -> Incr.const err
         end
      | Exp.Lambda (args, body) ->
         Incr.const (Either.First (TaintValue.value (TaintValue.Closure (args, body, None, env))))
      | Exp.Call (f, args) ->
         let%bind operator = eval env f
         and operands = Incr.all (List.map ~f:(eval env) args) in
         begin match operator with
         | Either.First ((TaintValue.Closure (args, body, fname, env'), _) as opv) -> (* TODO: taint control flow *)
            begin match (List.zip args operands) with
            | Some argsops ->
               let env'' = List.fold argsops ~init:(Either.First env') ~f:(fun accenv (name, v) ->
                               match (accenv, v) with
                               | (Either.First env, Either.First vv) -> Either.First (Map.update env name ~f:(function _ -> vv))
                               | (Either.Second err, _) -> Either.Second err
                               | (_, Either.Second err) -> Either.Second err) in
               begin match (env'', fname) with
               | (Either.First env'', None) -> eval env'' body (* not a recursive function *)
               | (Either.First env'', Some f) -> eval (Map.update env'' f ~f:(function _ -> opv)) body (* recursive function, bind it *)
               | (Either.Second err, _) -> Incr.const (Either.Second err)
               end
            | None -> failwith "invalid call, arity does not match"
            end
         | _ -> failwith "Not a closure"
         end
    )

(* let show x =
  Incr.stabilize ();
  print_s [%sexp (Incr.Observer.value_exn x : Value.t)]
 *)
let exp e = Incr.Var.watch (Incr.Var.create e)

(* let fact (n) = if (n == 0) then 1 else n * fact(n-1) *)
let fact body =
  exp (Exp.LetRec
         ("fact", ["n"],
          exp (Exp.If (exp (Exp.BinOp (exp (Exp.Variable "n"),
                                               Exp.Equals,
                                               exp (Exp.Const 0))),
                           exp (Exp.Const 1),
                           exp (Exp.BinOp (exp (Exp.Variable "n"),
                                               Exp.Times,
                                               exp (Exp.Call
                                                      (exp (Exp.Variable "fact"),
                                                       [exp (Exp.BinOp (exp (Exp.Variable "n"),
                                                                            Exp.Minus,
                                                                            exp (Exp.Const 1)))])))))), body))

(* Program 1: let fact (n) = ... in fact(5)
   where 5 becomes 6. *)
let program1 =
  let diffpoint = Incr.Var.create (Exp.Const 5) in
  let applydiff () =
    diffpoint := Exp.Const 4 in
  (fact (exp (Exp.Call (exp (Exp.Variable "fact"), [Incr.Var.watch diffpoint]))),
   applydiff)

(* Program 2: let fact (n) = ... and f(x,y) = x+y in f(fact(5), 1)
   where 1 becomes 2. *)
let program2 =
  let diffpoint = Incr.Var.create (Exp.Const 1) in
  let applydiff () =
    diffpoint := Exp.Const 0 in
  (fact (exp (Exp.Let ("f", exp (Exp.Lambda (["x"; "y"],
                                                    exp (Exp.BinOp (exp (Exp.Variable "x"),
                                                                        Exp.Plus,
                                                                        exp (Exp.Variable "y"))))),
                           exp (Exp.Call (exp (Exp.Variable "f"),
                                              [exp (Exp.Call (exp (Exp.Variable "fact"),
                                                                  [exp (Exp.Const 5)]));
                                               Incr.Var.watch diffpoint]))))),
  applydiff)

let () =
  (*  let xv = Incr.Var.create (Exp.Const 5) in *)
  let (exp, applydiff) = program2 in
  (*   let empty_env = (IncrMap.Lookup.create (Incr.Var.watch (Incr.Var.create String.Map.empty)) ~comparator:Symbol.comparator) in *)
  Incr.State.set_max_height_allowed Incr.State.t 1024;
  Printf.printf "%d\n" (Incr.State.max_height_allowed Incr.State.t);
  let computation = (eval String.Map.empty exp) in
  let observer = Incr.observe computation in
  report ();
  show observer;
  Incr.save_dot "1.dot";
  report ();
  applydiff ();
  report ();
  show observer;
  Incr.save_dot "2.dot";
  report ();
  ()
