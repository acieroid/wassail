open Js_of_ocaml
open Wasmtaint

let js_simple x = x + 42

let js_hello = Js.Unsafe.obj [| ("some_number", Js.Unsafe.inject 42);
                                ("some_string", Js.Unsafe.inject (Js.string "hello")); |]

let array_of_intmap (map : 'a IntMap.t) (f : 'a -> 'b) : (string * Js.Unsafe.any) array =
  match IntMap.max_elt map with
  | Some (n, _) ->
    Array.init (n+1) (fun i -> (string_of_int i, match IntMap.find map i with
    | Some x -> f x
    | None -> Js.Unsafe.inject Js.undefined))
  | None -> [| |]

let array_of_intlist (l : int list) : (string * Js.Unsafe.any) array =
  Array.init (List.length l) (fun i -> (string_of_int i, Js.Unsafe.inject (List.nth l i)))

let array_of_list (l : 'a list) (f : 'a -> Js.Unsafe.any) : (string * Js.Unsafe.any) array =
  Array.init (List.length l) (fun i -> (string_of_int i, f (List.nth l i)))

let js_of_block (block : BasicBlock.t) = Js.Unsafe.obj [| ("idx", Js.Unsafe.inject block.idx);
                                                          ("sort", Js.Unsafe.inject (Js.string (match block.sort with
                                                            | BlockEntry -> "block_entry"
                                                            | BlockExit -> "block_exit"
                                                            | LoopEntry -> "loop_entry"
                                                            | LoopExit -> "loop_exit"
                                                            | Normal -> "normal"
                                                            | Function -> "function"
                                                            | Return -> "return")));
                                                          ("instrs", Js.Unsafe.obj (array_of_list block.instrs (fun x -> Js.Unsafe.inject (Wasmtaint.Instr.to_string x)))) |]

let js_of_cfg (cfg : CFG.t) = Js.Unsafe.obj [| ("blocks", Js.Unsafe.obj (array_of_intmap cfg.basic_blocks js_of_block));
                                               ("edges", Js.Unsafe.inject (Js.Unsafe.obj (array_of_intmap cfg.edges (fun x -> Js.Unsafe.obj (array_of_intlist x))))) |]
let get_cfgs () =
  Js.Unsafe.obj (array_of_intmap !(Wasmtaint.cfgs) js_of_cfg)

let () =
    Js.export "jsbridge"
      (object%js
        method init program = Wasmtaint.initialize (Js.to_string program)
        method nglobals = !(Wasmtaint.nglobals)
        method cfgs = get_cfgs ()
      end)
