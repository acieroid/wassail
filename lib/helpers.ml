open Core_kernel

(** Integers *)
module I = struct
  type t = int
  [@@deriving sexp, compare, equal]
end

(** Strings *)
module S = struct
  type t = string
  [@@deriving sexp, compare, equal]
end

(** Sets of integers *)
module IntSet = Set.Make(I)

(** Maps of integers *)
module IntMap = Map.Make(I)

(** Sets of strings *)
module StringSet = Set.Make(S)

(** Maps of strings *)
module StringMap = Map.Make(S)


(** Get the nth element of a list *)
let get_nth (l : 'a list) (n : int) : Var.t = List.nth_exn l n

(** Pop one element from a list *)
let pop (vstack : 'a list) : 'a =
  match vstack with
  | hd :: _ -> hd
  | _ -> failwith "Invalid vstack"

(** Pop two elements from a list *)
let pop2 (vstack : 'a list) : ('a * 'a) =
  match vstack with
  | x :: y :: _ -> (x, y)
  | _ -> failwith "Invalid vstack"

(** Pop 3 elements from a list *)
let pop3 (vstack : 'a list) : ('a * 'a * 'a) =
    match vstack with
    | x :: y :: z :: _ -> (x, y, z)
    | _ -> failwith "Invalid vstack"


let error at category msg =
  failwith (Printf.sprintf "Error: %s" (Wasm.Source.string_of_region at ^ ": " ^ category ^ ": " ^ msg))

let input_from get_script run =
  try
    let script = get_script () in
    run script;
  with
  | Wasm.Decode.Code (at, msg) -> error at "decoding error" msg
  | Wasm.Parse.Syntax (at, msg) -> error at "syntax error" msg
  | Wasm.Valid.Invalid (at, msg) -> error at "invalid module" msg
  | Wasm.Import.Unknown (at, msg) -> error at "link failure" msg
  | Wasm.Eval.Link (at, msg) -> error at "link failure" msg
  | Wasm.Eval.Trap (at, msg) -> error at "runtime trap" msg
  | Wasm.Eval.Exhaustion (at, msg) -> error at "resource exhaustion" msg
  | Wasm.Eval.Crash (at, msg) -> error at "runtime crash" msg
  | Wasm.Encode.Code (at, msg) -> error at "encoding error" msg

let parse_file name run =
  let ic = In_channel.create name in
  try
    let lexbuf = Lexing.from_channel ic in
    let success = input_from (fun _ ->
        let var_opt, def = Wasm.Parse.parse name lexbuf Wasm.Parse.Module in
        [(var_opt, def)]) run in
    In_channel.close ic;
    success
  with exn -> In_channel.close ic; raise exn

let parse_string str run =
  let lexbuf = Lexing.from_string str in
  input_from (fun _ ->
      let var_opt, def = Wasm.Parse.parse "foo.wat" lexbuf Wasm.Parse.Module in
      [(var_opt, def)])
    run

let apply_to_textual (filename : string) (f : Wasm.Ast.module_ -> unit) =
  let extract (l : (Wasm.Script.var option * Wasm.Script.definition) list) =
    List.iter l ~f:(fun (_var_opt, def) ->
        match def.it with
        | Wasm.Script.Textual m -> f m
        | Wasm.Script.Encoded _ -> failwith "unsupported"
        | Wasm.Script.Quoted _ -> failwith "unsupported"
      ) in
  parse_file filename extract

