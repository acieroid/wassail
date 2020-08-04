Wassail is a static analyzer for WebAssembly.

# Setup
## Installing the dependencies

0. Install opam

1. Get the right version of the wasm spec:

```sh
git clone https://github.com/WebAssembly/spec
cd spec/
git checkout 92b9ce
opam pin add wasm .
```

2. Install dependencies for this project

```sh
opam install core core_kernel ppx_compare ppx_inline_test ppx_jane wasm sexplib apron
```

(This list of dependencies may be outdated, you can find it by running `dune external-lib-deps ./bin/main.exe`)

## Running the tests

```sh
make test
```

If there is no output, this means all tests successfully passed.

## Building

```sh
make
```

# Constructing Control-Flow Graphs and Call-Graphs

## Generating Control Flow Graphs
You can either generate a single CFG, for example to generate the CFG of function 1 from file `foo.wat` into `foo.dot`:

```sh
./main.exe cfg foo.wat 1 foo.dot
```

Or you can generate all CFGs in a given directory:

```sh
./main.exe cfgs foo.wat out/
```

## Generating Call Graphs
To generate the call graph for a module `foo.wat` into `foo.dot`:

```sh
./main.exe callgraph foo.wat foo.dot
```

# Analyses

## Taint Analysis

TODO


# Web interface

The web interface can be used to generate CFGs, call graphs, and run a naive taint analysis.
It currently cannot perform any relational analysis, as it requires Apron which can't be compiled to JavaScript.

!!! The web interface needs to be refreshed and currently does not work.

```sh
make js
```

Then you can open `interface/cfg-viz.html`.
