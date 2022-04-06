open Core

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

module Int32Set = struct
  include Set.Make(Int32)
  let to_string (s : t) : string = String.concat ~sep:"," (List.map (to_list s) ~f:Int32.to_string)
end

(** Sets of pairs of integers *)
module IntPairSet = struct
  include Set.Make(struct
      type t = int * int
      [@@deriving sexp, compare, equal]
    end)
  let to_string (s : t) : string = String.concat ~sep:"," (List.map (to_list s) ~f:(fun (x, y) -> Printf.sprintf "(%d,%d)" x y))
end

(** Maps of integers *)
module IntMap = struct
  include Map.Make(I)
  let to_string (m : 'a t) (f : 'a -> string) : string = String.concat ~sep:", " (List.map (to_alist m) ~f:(fun (k, v) -> Printf.sprintf "%d -> %s" k (f v)))
end

module Int32Map = struct
  include Map.Make(Int32)
  let to_string (m : 'a t) (f : 'a -> string) : string = String.concat ~sep:", " (List.map (to_alist m) ~f:(fun (k, v) -> Printf.sprintf "%s -> %s" (Int32.to_string k) (f v)))
end

(** Sets of strings *)
module StringSet = Set.Make(S)

(** Maps of strings *)
module StringMap = Map.Make(S)

module List32 = struct
  let nth_exn = Wasm.Lib.List32.nth
  let rec nth xs n =
    match n, xs with
    | 0l, x::_ -> Some x
    | n, _::xs' when Int32.(n > 0l) -> nth xs' Int32.(n - 1l)
    | _ -> None
  let mapi (l : 'a list) ~(f : Int32.t -> 'a -> 'b) : 'b list =
    List.mapi l ~f:(fun x -> f (Int32.of_int_exn x))
  let length = Wasm.Lib.List32.length
end

(** Get the nth element of a list *)
let get_nth (l : 'a list) (n : Int32.t) : Var.t =
  match List.nth l (Int32.to_int_exn n) with
  | Some v -> v
  | _ -> failwith "Helpers.get_nth nth exception"

(** Pop one element from a list *)
let pop (vstack : 'a list) : 'a =
  match vstack with
  | hd :: _ -> hd
  | _ -> failwith "Invalid vstack when popping 1 value"

(** Pop two elements from a list *)
let pop2 (vstack : 'a list) : ('a * 'a) =
  match vstack with
  | x :: y :: _ -> (x, y)
  | _ -> failwith "Invalid vstack when popping 2 values"

(** Pop 3 elements from a list *)
let pop3 (vstack : 'a list) : ('a * 'a * 'a) =
    match vstack with
    | x :: y :: z :: _ -> (x, y, z)
    | _ -> failwith "Invalid vstack when popping 3 values"


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

let parse_from_lexbuf_textual name lexbuf run =
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
      let var_opt, def = Wasm.Parse.parse "no-file" lexbuf Wasm.Parse.Module in
      [(var_opt, def)])
    run

let apply_to_textual_file (filename : string) (f : Wasm.Ast.module_ -> 'a) : 'a =
  In_channel.with_file filename ~f:(fun ic ->
      let lexbuf = Lexing.from_channel ic in
      parse_from_lexbuf_textual filename lexbuf f)

let apply_to_binary_file (filename : string) (f : Wasm.Ast.module_ -> 'a) : 'a =
  In_channel.with_file filename ~f:(fun ic ->
      f (Wasm.Decode.decode filename (In_channel.input_all ic)))

let apply_to_file (filename : string) (f : Wasm.Ast.module_ -> 'a) : 'a =
  match Stdlib.Filename.extension filename with
  | ".wat" -> apply_to_textual_file filename f
  | ".wasm" -> apply_to_binary_file filename f
  | ext -> failwith (Printf.sprintf "Invalid extension for WebAssembly module: %s" ext)

let apply_to_string (string : string) (f : Wasm.Ast.module_ -> 'a) : 'a =
  let lexbuf = Lexing.from_string string in
  parse_from_lexbuf_textual "no-file" lexbuf f

module type Abstract_domain = sig
  type t
  val compare : t -> t -> int
  val equal : t -> t -> bool
  val t_of_sexp : Sexp.t -> t
  val sexp_of_t : t -> Sexp.t

  val bottom : t
  val to_string : t -> string
  val join : t -> t -> t
end

module Product_domain = struct
  module Make (Domain1 : Abstract_domain) (Domain2 : Abstract_domain) = struct
    type t = Domain1.t * Domain2.t
    [@@deriving sexp, compare, equal]
    let bottom : t = (Domain1.bottom, Domain2.bottom)
    let to_string (t : t) =
      Printf.sprintf "(%s, %s)"
        (Domain1.to_string (fst t))
        (Domain2.to_string (snd t))
    let join (t1 : t) (t2 : t) : t =
      (Domain1.join (fst t1) (fst t2),
       Domain2.join (snd t1) (snd t2))
  end
end
