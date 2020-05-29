Installing the dependencies
---------------------------
0. Install opam

1. Get the right version of the wasm spec:

> git clone https://github.com/WebAssembly/spec
> cd spec/
> git checkout 92b9ce
> opam pin add wasm .

2. Install dependencies for this project

> opam install - core core_kernel ppx_compare ppx_inline_test ppx_jane wasm

(This list of dependencies may be outdated, you can find it by running `dune external-lib-deps ./bin/main.exe`)

Running the tests
-----------------

> make test

If there is no output, this means all tests successfully passed.

Running the executable
----------------------
> make
> ./main.exe foo.wat

Running the interface
---------------------

> make js

Then you can open `interface/cfg-viz.html`.
