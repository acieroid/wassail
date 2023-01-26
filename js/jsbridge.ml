open Core
open Js_of_ocaml
open Wassail

let create_cfg cfg =
  object%js
    method toDot = Js.string (Cfg.to_dot cfg)
  end

let () =
  Js.export "jsbridge"
    (object%js (self)
       val mutable program = None

       method load (program : Js.js_string Js.t) : unit = self##.program := Some (Js.to_string program)

       method cfgs : 'a (* an object mapping function indices to CFGs *) =
         let m = Wasm_module.of_string (Option.value_exn self##.program) in
         let cfgs = Cfg_builder.build_all m in
         let cfgs_alist = List.map (Int32Map.to_alist cfgs) ~f:(fun (id, cfg) -> (Int32.to_string id, Js.Unsafe.inject (create_cfg cfg)))  in
         Js.Unsafe.obj (Array.of_list cfgs_alist)
     end)
