Installing the dependencies
---------------------------
1. Get the right version of the wasm spec:

>  git clone https://github.com/WebAssembly/spec
>  cd spec/
>  opam pin add wasm .

2. Install dependencies for this project

>  opam install core incremental incr_map pps ppx_jane ppx_compare

Running the tests
-----------------

> dune runtest

Running the executable
----------------------
>  dune exec ./bin/main.exe foo.wat

Running the interface
---------------------

>  dune build ./js/jsbridge.bc.js

Then you can open interface.html