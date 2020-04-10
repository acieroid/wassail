open Js_of_ocaml
open Wasmtaint

let js_simple x = x + 42

let js_hello = Js.Unsafe.obj [| ("some_number", Js.Unsafe.inject 42);
                                ("some_string", Js.Unsafe.inject (Js.string "hello")); |]

let array_of_intmap (map : 'a IntMap.t) (f : 'a -> 'b) : 'b array =
  match IntMap.max_elt map with
  | Some (n, _) ->
    Array.init (n+1) (fun i -> match IntMap.find map i with
    | Some x -> f x
    | None -> Js.undefined)
  | None -> [| |]

let array_of_intlist (l : int list) : int array =
  Array.init (List.length l) (fun i -> (List.nth l i))

let array_of_intmap_indices (map : 'a IntMap.t) : int array =
  array_of_intlist (IntMap.keys map)

let array_of_list (l : 'a list) (f : 'a -> 'b) : 'b array =
  Array.init (List.length l) (fun i -> f (List.nth l i))

let js_of_instr (instr : Instr.t) =  Js.string (Instr.to_string instr)

let js_of_block_sort (sort : Basic_block.block_sort) = Js.string (match sort with
    | BlockEntry -> "BlockEntry"
    | BlockExit -> "BlockExit"
    | LoopEntry -> "LoopEntry"
    | LoopExit -> "LoopExit"
    | Normal -> "Normal"
    | Function -> "Function"
    | Return -> "Return")

let js_of_block (block : Basic_block.t) = object%js (self)
  val idx = block.idx
  val sort = js_of_block_sort block.sort
  val instrs = Js.array (array_of_list block.instrs js_of_instr)
end

let js_of_cfg (cfg : Cfg.t) = object%js (self)
  val blocks = Js.array (array_of_intmap cfg.basic_blocks (fun x -> Js.def (js_of_block x)))
  val edges = Js.array (array_of_intmap cfg.edges (fun targets -> Js.def (Js.array (array_of_intlist targets))))
end

let js_of_state (state : Domain.state) = Js.string (Domain.to_string state)

let () =
    Js.export "jsbridge"
      (object%js
        method init program = Wasmtaint.initialize (Js.to_string program)

        method addLogger cb =
          Logging.add_callback (fun opt msg ->
              let jsopt = match opt with
                | Info -> Js.string "INFO"
                | Warn s -> Js.string (Printf.sprintf "WARN:%s" s)
              in
              Js.Unsafe.fun_call cb [| Js.Unsafe.inject jsopt; Js.Unsafe.inject msg |])

        method cfgIndices =
          array_of_intmap_indices !(Wasmtaint.cfgs)

        method functionName idx =
          match IntMap.find (!Wasmtaint.cfgs) idx with
          | Some cfg -> Js.string (cfg.name)
          | None -> failwith "CFG not found"

        method deps =
          match IntMap.max_elt (!Wasmtaint.cfg_deps) with
          | None -> Js.array [| |]
          | Some (maxIdx, _) -> Js.array (Array.init maxIdx (fun idx ->
              match IntMap.find (!Wasmtaint.cfg_deps) idx with
              | Some l -> Js.array (Array.of_list l)
              | None -> Js.array [| |]))

        method getCfg (idx : int) : 'a Js.t =
          match IntMap.find !(Wasmtaint.cfgs) idx with
          | Some cfg -> js_of_cfg cfg
          | None -> failwith "CFG not found"

        method analyze =
          let _ = Wasmtaint.Inter_fixpoint.analyze !(Wasmtaint.cfgs) !(Wasmtaint.nglobals) in
          ()

(*        method initial_state (nargs : int) (nlocals : int) (nglobals : int) =
          Domain.init
            (* locals: [p0, p1, ...., 0, 0, 0] *)
            (List.init nargs ~f:(fun i -> Value.symbolic (Printf.sprintf "p%d" i))
               (List.init nlocals ~f:(fun _ -> Value.zero I32Type)))
            (* globals: [g0, ...] *)
            (List.init nglobals ~f:(fun i -> Value.symbolic (Printf.sprintf "g%d" i)))
            (* memory: M *)
            Memory.top
*)
        method result (cfgidx : int) = match IntMap.find !(Inter_fixpoint.data) cfgidx with
          | Some (Some state) -> Js.Unsafe.inject (js_of_state state)
          | _ -> Js.Unsafe.inject (Js.undefined)
      end)
