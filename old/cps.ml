open Core
(* open Incr_engine
open Incr.Let_syntax
 *)
module VarSymbol = String
module KontSymbol = String

type p = string * e [@@deriving sexp, compare]
and e =
  | Call of t * t * c
  | KontCall of c * t
              [@@deriving sexp, compare]
and t =
  | VarSymbol of string
  | KontSymbol of string
  | Lambda of string * string * e
            [@@deriving sexp, compare]
and c =
  | KLambda of string * e
  | Kont of string
              [@@deriving sexp, compare]

type value =
  | Lam of string * string * e * env
  | KLam of string * e * env
  | Stop
  [@@deriving sexp]
and env = value String.Map.t
            [@@deriving sexp]

let lookup env var =
  match Map.find env var with
  | None -> failwith "unbound variable"
  | Some v -> v

let eval_t (t : t) (env : env) : value =
  match t with
  | VarSymbol x -> lookup env x
  | KontSymbol v -> lookup env v
  | Lambda (x, k, e) -> Lam (x, k, e, env)

let eval_c (c : c) (env : env) : value =
  match c with
  | Kont k -> lookup env k
  | KLambda (v, e) -> KLam (v, e, env)

type state = e * env
let step : state -> state = function
  | (Call (t0, t1, c), env) ->
     begin match eval_t t0 env with
     | Lam (x, k, e, env') ->
        (e, Map.update
              (Map.update env' x ~f:(function _ -> eval_t t1 env))
                 k ~f:(function _ -> eval_c c env))
     | _ -> failwith "..."
     end
  | (KontCall (c, t), env) ->
     begin match eval_c c env with
     | KLam (v, e, env') -> (e, Map.update env' v ~f:(function _ -> eval_t t env))
     | _ -> failwith "..."
     end


let eval (k, e) : state =
  let final_state = function
    | (KontCall (Kont "kr", VarSymbol "vr"), env) -> Some (lookup env "vr")
    | _ -> None
  in

  let rec step' state =
    match final_state state with
    | Some _ -> state (* w *)
    | None -> step' (step state)
  in
  step' (e, Map.update String.Map.empty k
              ~f:(function _ ->
                    KLam ("vr", KontCall (Kont "kr", VarSymbol "vr"), Map.update String.Map.empty "kr" ~f:(function _ -> Stop))))

(* let f = (fn x -> x) in f f (fn y -> y) *)
(* which in CPS is: *)
(* lam k0 . (lam f, k2. f f (lam v4 . v4 (lam y, k5 . k5 y) k2)) (lam x, k6. k6 x) k0 *)

let program : string * e =
  ("k0",
   (Call (Lambda ("f", "k2",
                  Call (VarSymbol "f",
                        VarSymbol "f",
                        KLambda ("v4",
                                 Call (VarSymbol "v4",
                                       Lambda ("y", "k5", KontCall (Kont "k5", VarSymbol "y")),
                                       Kont "k2")))),
          Lambda ("x", "k6",
                  KontCall (Kont "k6", VarSymbol "x")),
          Kont "k0")))

let () =
  let (_, env) = eval program in
  print_s [%sexp (env : env)]

module AbsValue = struct
  type t =
    | Lam of string * string * e
    | KLam of string * e
    | Stop
[@@deriving sexp, compare]
end
module AbsValueSet = Set.Make(AbsValue)

type absenv = AbsValueSet.t String.Map.t
                [@@deriving sexp]

let abseval_t (t : t) (env : absenv) : AbsValueSet.t =
  match t with
  | VarSymbol x -> lookup env x
  | KontSymbol v -> lookup env v
  | Lambda (x, k, e) -> AbsValueSet.singleton (Lam (x, k, e))

let abseval_c (c : c) (env : absenv) : AbsValueSet.t =
  match c with
  | Kont k -> lookup env k
  | KLambda (v, e) -> AbsValueSet.singleton (KLam (v, e))


module Exp = struct
  type t = e [@@deriving sexp, compare]
end

module ExpSet = Set.Make(Exp)

let absenv_join (e1 : absenv) (e2 : absenv) : absenv =
  Map.fold e2 ~init:e1 ~f:(fun ~key:k ~data:d acc ->
      Map.update acc k ~f:(function _ ->
                                     match Map.find acc k with
                                     | Some d' -> AbsValueSet.union d d'
                                     | None -> d))

let join (x : ExpSet.t * absenv) (y : ExpSet.t * absenv) : ExpSet.t * absenv =
  let (exps1, env1) = x in
  let (exps2, env2) = y in
  (ExpSet.union exps1 exps2,
   absenv_join env1 env2)

let fix (e : e) (k : string) (es : ExpSet.t) (env : absenv) : (ExpSet.t * absenv) =
  let init = (ExpSet.singleton e, Map.update
                              (Map.update String.Map.empty "kr" ~f:(function _ -> AbsValueSet.singleton Stop))
                              k ~f:(function _ -> AbsValueSet.singleton (KLam ("vr", KontCall (Kont "kr", VarSymbol "vr"))))) in
  ExpSet.fold es ~init:init ~f:(fun acc e ->
      match e with
      | Call (t0, t1, c) ->
         AbsValueSet.fold (abseval_t t0 env) ~init:acc ~f:(fun acc lam ->
             match lam with
             | Lam (x, k', e') ->
                join acc
                  (ExpSet.singleton e',
                   Map.update (Map.update env x ~f:(function _ -> abseval_t t1 env))
                     k' ~f:(function _ -> abseval_c c env))
             | _ -> acc)
      | KontCall (c, t) ->
         AbsValueSet.fold (abseval_c c env) ~init:acc ~f:(fun acc lam ->
             match lam with
             | KLam (v, e') ->
                join acc
                  (ExpSet.singleton e', Map.update env v ~f:(function _ -> abseval_t t env))
             | _ -> acc))

let cfa (program : string * e) : (ExpSet.t * absenv) =
  let (k, e) = program in
  let i = ref 1 in
  let rec loop es env =
    Printf.printf "Iteration %d\n" (!i);
    i := !i + 1;
    let (es', env') = fix e k es env in
    if ExpSet.equal es es' && String.Map.equal AbsValueSet.equal env env' then (es, env)
    else loop es' env'
  in
  loop ExpSet.empty String.Map.empty

let () =
  let (_, env) = cfa program in
  print_s [%sexp ("-------" : string)];
  print_s [%sexp (env : absenv)]
