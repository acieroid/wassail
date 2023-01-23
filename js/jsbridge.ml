open Js_of_ocaml

let js_simple x = x + 42

let () =
  Js.export "jsbridge"
    (object%js (self)
       val mutable program = None

       method load program = self##.program := Some program

       method do_something = self##.program
     end)
