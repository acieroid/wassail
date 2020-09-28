Wassail is a static analyzer for WebAssembly.

# Setup
## Installing the dependencies

0. Install [opam](https://opam.ocaml.org/)

1. Install dependencies for this project

```sh
opam install core core_kernel ppx_compare ppx_inline_test ppx_jane wasm sexplib apron
```

(This list of dependencies may be outdated, you can find it by running `dune external-lib-deps ./bin/main.exe`)

## Building

```sh
make
```

## Running the tests

```sh
make test
```

If there is no output, this means all tests successfully passed.

# Constructing Control-Flow Graphs and Call-Graphs of WebAssembly Programs

Wassail works on the textual representation of WebAssembly, i.e., `.wat` files.

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


# References

Wassail has been described in the following publication:
  - [Compositional Information Flow Analysis for WebAssembly Programs](http://soft.vub.ac.be/~qstieven/scam2020wasm/), SCAM 2020. ([pdf](http://soft.vub.ac.be/Publications/2020/vub-tr-soft-20-11.pdf])
