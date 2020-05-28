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

let array_of_intlist (l : int list) : int Js.js_array Js.t =
  Js.array (Array.init (List.length l) (fun i -> (List.nth l i)))

let array_of_intmap_indices (map : 'a IntMap.t) : int Js.js_array Js.t =
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
  val sort = match block.content with
    | Nothing -> Js.string "Empty"
    | Control _ -> Js.string "Control"
    | Data _ -> Js.string "Data"
  val name = match block.content with
    | Nothing -> Js.string ""
    | Control i -> Js.string (Instr.control_to_short_string i)
    | Data _ -> Js.string ""
  val shape = match block.content with
    | Data _ -> "rect"
    | Control (Call _)
    | Control (CallIndirect _) -> "circle"
    | Control Return -> "diamond"
    | Control _ -> "ellipse"
    | Nothing -> "circle"
  val instrs = Js.array (array_of_list (match block.content with
      | Nothing -> []
      | Control (Call i) -> [Instr.control_to_string (Call i)]
      | Control _ -> []
      | Data is -> List.map Instr.data_to_string is)
      Js.string)
  val label = Js.string (match block.content with
    | Nothing -> ""
    | Data is -> Printf.sprintf "%s" (String.concat "\n" (List.map Instr.data_to_string is))
    | Control i -> Printf.sprintf "%s" (Instr.control_to_short_string i))
end

let js_of_cfg (cfg : Cfg.t) = object%js (self)
  val blocks = Js.array (array_of_intmap cfg.basic_blocks (fun x -> Js.def (js_of_block x)))
  val edges = Js.array (array_of_intmap cfg.edges (fun targets ->
      Js.def (Js.array (array_of_list targets (fun (target, data) ->
          Js.array (array_of_list [Js.string (string_of_int target); match data with
            | None -> Js.string ""
            | Some true -> Js.string "t"
            | Some false -> Js.string "f"]
            (fun x -> x)))))))
end

let js_of_state (state : Domain.state) = Js.string (Domain.to_string state)

let js_of_result (result : Transfer.result) = Js.string (Transfer.result_to_string result)
let js_of_result_pair (result : (Transfer.result * Transfer.result)) = Js.string (Printf.sprintf "pre:\n%s\npost:\n%s" (Transfer.result_to_string (fst result)) (Transfer.result_to_string (snd result)))

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
          Wasmtaint.Inter_fixpoint.analyze !(Wasmtaint.cfgs) !(Wasmtaint.nglobals) (match !(Wasmtaint.module_) with
              | Some m -> m
              | None -> failwith "Module not loaded")

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
        method result (cfgidx : int) (blockidx : int) = match IntMap.find !(Inter_fixpoint.data) cfgidx with
          | Some (Some results) -> begin match IntMap.find results blockidx with
              | Some st -> Js.Unsafe.inject (js_of_result_pair st)
              | None -> Js.Unsafe.inject (Js.undefined)
            end
          | _ -> Js.Unsafe.inject (Js.undefined)
      end)
