# Wassail
`wassail` (WebAssembly static analyzer and inspection library) is a toolkit to perform both lightweight and heavyweight static analysis of WebAssembly modules.

  - [Installation](#installation)
  - [Usage](#usage)
  - [Contributing](#contributing)

# Installation
## Dependencies

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

# Usage
Wassail can perform a number of tasks on WebAssembly modules, both in their textual representation (`.wat` files) or in their binary representation (`.wasm`)
## Listing imports
```sh
$ ./main.exe imports foo.wasm
0	time	i32 -> i32
1	ctime	i32 -> i32
2	roundf	f32 -> f32
...
```
## Listing exports
```sh
$ ./main.exe exports foo.wasm
11563	atof	i32 -> f64
11586	strlen	i32 -> i32
11483	fopen	i32, i32 -> i32
...
```

## Listing section sizes
Section sizes are reported in bytes.
```sh
$ ./main.exe sizes foo.wasm
1962	type
8724	import
13120	func
0	table
0	memory
23	global
1316	export
0	start
9480	elem
2063620	code
507825	data
```

## Generating Call Graphs
To generate the call graph for a module `foo.dot`:

```sh
$ ./main.exe callgraph benchmarks/benchmarksgame/fankuchredux.wat foo.dot
```

Output graph:

[!DOT call graph](doc/callgraph.png)

## Generating Control-Flow Graphs
You can either generate a single CFG, for example to generate the CFG of function 1 from file `foo.wat` into `foo.dot`:

```sh
$ ./main.exe cfg foo.wat 1 foo.dot
```

Or you can generate all CFGs in a given directory:

```sh
$ ./main.exe cfgs foo.wat out/
```

Example output:

[!DOT CFG](doc/cfg.png)

## Performing Taint Analysis
WIP, more to come.
## Performing Relational Analysis
WIP, more to come.
## Slicing
WIP, more to come.

# Contributing
Contributions are welcome!

## Running the tests

```sh
$ make test
```

# References

Wassail has been described in the following publication:
  - [Compositional Information Flow Analysis for WebAssembly Programs](http://soft.vub.ac.be/~qstieven/scam2020wasm/), SCAM 2020. ([pdf](http://soft.vub.ac.be/Publications/2020/vub-tr-soft-20-11.pdf])
