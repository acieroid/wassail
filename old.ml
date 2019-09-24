(*open Core

module Symbol = String
let sym (s : String.t) : Symbol.t = s

module Exp = struct
  type bin =
    | Plus
  type t =
    | Variable of Symbol.t
    | BinOp of t * bin * t
    | Const of int
    | Let of Symbol.t * t * t
end

type env = int Map.M(Symbol).t

let lookup env var =
  match Map.find env var with
  | None -> failwith "unbound variable"
  | Some v -> v

let rec eval (env : env) = function
  | Exp.Variable x -> lookup env x
  | Exp.Const n -> n
  | Exp.BinOp (x, Exp.Plus, y) -> (eval env x) + (eval env y)
  | Exp.Let (x, binding, body) ->
     eval (Map.update env x ~f:(function _ -> eval env binding)) body

let () =
  let exp = Exp.Let ("x", Exp.Const 5, Exp.BinOp (Exp.Const 1, Exp.Plus, Exp.Variable "x")) in
  Printf.printf "result: %d\n" (eval String.Map.empty exp)

module Incr = Incremental_kernel.Incremental.Make ()
open Incr.Let_syntax
(* module IncrMap = Incr_map.Make(Incr) *)

module IncrExp = struct
  type bin =
    | Plus | Minus | Times | Divides
    | Equals (* comparisons are treated as bin ops that return 0 (false) or 1 (true) *)
  type t = exp Incr.t
  and exp =
    | Variable of Symbol.t (* x *)
    | BinOp of t * bin * t (* e + e *)
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
    | Closure of Symbol.t list * (IncrExp.t [@sexp.opaque]) * String.t option * env
  and env = t String.Map.t
  let sexp_of_t = function
    | Const n -> Sexp.Atom (string_of_int n)
    | Closure _ -> Sexp.Atom ("clo")
end

(* Initial version, very inneficient probably *)
let rec eval_inc (env : Value.env) (exp : IncrExp.t) : Value.t Incr.t =
  Incr.bind exp ~f:(function
      | IncrExp.Variable x ->
         Incr.const (lookup env x)
      | IncrExp.Const n -> Incr.const (Value.Const n)
      | IncrExp.BinOp (left, op, right) ->
          let%map left = eval_inc env left
          and right = eval_inc env right in
          begin match (left, op, right) with
          | (Value.Const x, IncrExp.Plus, Value.Const y) -> Value.Const (x + y)
          | (Value.Const x, IncrExp.Minus, Value.Const y) -> Value.Const (x - y)
          | (Value.Const x, IncrExp.Times, Value.Const y) -> Value.Const (x * y)
          | (Value.Const x, IncrExp.Divides, Value.Const y) -> Value.Const (x / y)
          | (Value.Const x, IncrExp.Equals, Value.Const y) -> Value.Const (if x = y then 1 else 0)
          | _ -> failwith "Incorrect binary operation"
          end
      | IncrExp.Let (x, binding, body) ->
         let%bind v = eval_inc env binding in
         eval_inc (Map.update env x ~f:(function _ -> v)) body
      | IncrExp.LetRec (name, args, fbody, body) ->
         let v = Value.Closure (args, fbody, Some name, env) in
         eval_inc (Map.update env name ~f:(function _ -> v)) body
      | IncrExp.If (condition, consequence, alternative) ->
         let%bind v = eval_inc env condition in
         begin match v with
         | Value.Const 0 -> (* false *) eval_inc env alternative
         | _ -> (* true *) eval_inc env consequence
         end
      | IncrExp.Lambda (args, body) ->
         Incr.const (Value.Closure (args, body, None, env))
      | IncrExp.Call (f, args) ->
         let%bind operator = eval_inc env f
         and operands = Incr.all (List.map ~f:(eval_inc env) args) in
         match operator with
         | Value.Closure (args, body, fname, env') ->
            begin match (List.zip args operands) with
            | Some argsops ->
               let env'' = List.fold argsops ~init:env' ~f:(fun accenv (name, v) ->
                               Map.update accenv name ~f:(function _ -> v)) in
               begin match fname with
               | None -> eval_inc env'' body (* not a recursive function *)
               | Some f -> eval_inc (Map.update env'' f ~f:(function _ -> operator)) body (* recursive function, bind it *)
               end
            | None -> failwith "invalid call, arity does not match"
            end
         | _ -> failwith "Not a closure"
    )



let show x =
  Incr.stabilize ();
  print_s [%sexp (Incr.Observer.value_exn x : Value.t)]

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

let exp e = Incr.Var.watch (Incr.Var.create e)

(* let fact (n) = if (n == 0) then 1 else n * fact(n-1) *)
let fact body =
  exp (IncrExp.LetRec
         ("fact", ["n"],
          exp (IncrExp.If (exp (IncrExp.BinOp (exp (IncrExp.Variable "n"),
                                               IncrExp.Equals,
                                               exp (IncrExp.Const 0))),
                           exp (IncrExp.Const 1),
                           exp (IncrExp.BinOp (exp (IncrExp.Variable "n"),
                                               IncrExp.Times,
                                               exp (IncrExp.Call
                                                      (exp (IncrExp.Variable "fact"),
                                                       [exp (IncrExp.BinOp (exp (IncrExp.Variable "n"),
                                                                            IncrExp.Minus,
                                                                            exp (IncrExp.Const 1)))])))))), body))

(* Program 1: let fact (n) = ... in fact(5)
   where 5 becomes 6. *)
let program1 =
  let diffpoint = Incr.Var.create (IncrExp.Const 5) in
  let applydiff () =
    diffpoint := IncrExp.Const 4 in
  (fact (exp (IncrExp.Call (exp (IncrExp.Variable "fact"), [Incr.Var.watch diffpoint]))),
   applydiff)

(* Program 2: let fact (n) = ... and f(x,y) = x+y in f(fact(5), 1)
   where 1 becomes 2. *)
let program2 =
  let diffpoint = Incr.Var.create (IncrExp.Const 1) in
  let applydiff () =
    diffpoint := IncrExp.Const 0 in
  (fact (exp (IncrExp.Let ("f", exp (IncrExp.Lambda (["x"; "y"],
                                                    exp (IncrExp.BinOp (exp (IncrExp.Variable "x"),
                                                                        IncrExp.Plus,
                                                                        exp (IncrExp.Variable "y"))))),
                           exp (IncrExp.Call (exp (IncrExp.Variable "f"),
                                              [exp (IncrExp.Call (exp (IncrExp.Variable "fact"),
                                                                  [exp (IncrExp.Const 5)]));
                                               Incr.Var.watch diffpoint]))))),
  applydiff)

let () =
  (*  let xv = Incr.Var.create (IncrExp.Const 5) in *)
  let (exp, applydiff) = program2 in
  (*   let empty_env = (IncrMap.Lookup.create (Incr.Var.watch (Incr.Var.create String.Map.empty)) ~comparator:Symbol.comparator) in *)
  Incr.State.set_max_height_allowed Incr.State.t 1024;
  Printf.printf "%d\n" (Incr.State.max_height_allowed Incr.State.t);
  let computation = (eval_inc String.Map.empty exp) in
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
  *)
