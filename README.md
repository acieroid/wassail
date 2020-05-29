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
-------------------

> make test

If there is no output, this means all tests successfully passed.

Building
---------

> make

Generating CFGs
-----------------
You can either generate a single CFG, for example to generate the CFG of function 1 from file `foo.wat` into `foo.dot`:

> ./main.exe cfg foo.wat 1 foo.dot

Or you can generate all CFGs in a given directory:

> ./main.exe cfgs foo.wat out/

Running the analysis
----------------------

You can either run individual intra-analyses, by listing the functions that have to be analyzed.
They will be analyzed in the given order, and summaries will be updated so that if a function relies on another one, it can use its summary.

For example, to analyze function 1, followed by function 2, and re-analyze function 1:

> ./main.exe intra foo.wat 1 2 1

Or you can use an inter-fixpoint mechanism to analyze all functions until no new results are produced:

> ./main.exe inter foo.wat

All results (i.e., the generated summaries) are printed to the console.

Using the interface
---------------------

> make js

Then you can open `interface/cfg-viz.html`.
