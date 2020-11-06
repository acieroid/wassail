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
module IntSet = struct
  include Set.Make(I)
  let to_string (s : t) : string = String.concat ~sep:"," (List.map (to_list s) ~f:string_of_int)
end

(** Maps of integers *)
module IntMap = struct
  include Map.Make(I)
  let to_string (m : 'a t) (f : 'a -> string) : string = String.concat ~sep:", " (List.map (to_alist m) ~f:(fun (k, v) -> Printf.sprintf "%d -> %s" k (f v)))
end

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

let parse_from_lexbuf name lexbuf run =
  let extract (l : (Wasm.Script.var option * Wasm.Script.definition) list) =
    match l with
    | (_, { it = Wasm.Script.Textual m; _ }) :: _ -> run m
    | _ -> failwith "unsupported format" in
    input_from (fun _ ->
        let var_opt, def = Wasm.Parse.parse name lexbuf Wasm.Parse.Module in
        [(var_opt, def)])
        extract


let parse_string str run =
  let lexbuf = Lexing.from_string str in
  input_from (fun _ ->
      let var_opt, def = Wasm.Parse.parse "foo.wat" lexbuf Wasm.Parse.Module in
      [(var_opt, def)])
    run

let apply_to_file (filename : string) (f : Wasm.Ast.module_ -> 'a) : 'a =
  In_channel.with_file filename ~f:(fun ic ->
      let lexbuf = Lexing.from_channel ic in
      parse_from_lexbuf filename lexbuf f)

let apply_to_string (string : string) (f : Wasm.Ast.module_ -> 'a) : 'a =
  let lexbuf = Lexing.from_string string in
  parse_from_lexbuf "no-file" lexbuf f
