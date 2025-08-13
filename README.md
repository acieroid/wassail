# Wassail
`wassail` (WebAssembly static analyzer and inspection library) is a toolkit to perform both lightweight and heavyweight static analysis of WebAssembly modules.

  - [Web version](#webversion)
  - [Installation](#installation)
  - [Usage](#usage)
  - [Contributing](#contributing)

# Web version
Parts of Wassail can be run in your web browser [here](https://acieroid.github.io/wassail/).

# Installation
0. Install [opam](https://opam.ocaml.org/)

1. Run `opam install core_unix`

2. Run `opam install .`

Then you should be able to run the `wassail` command. You can also simply run `dune exec -- wassail` if you don't want to install it.

# Usage
Wassail can perform a number of tasks on WebAssembly modules, both in their textual representation (`.wat` files) or in their binary representation (`.wasm`)
## Listing imports
```sh
$ dune exec -- wassail imports ./benchmarks/benchmarksgame/fasta.wat
0       proc_exit       i32 ->
1       fd_close        i32 -> i32
2       fd_seek         i32, i64, i32, i32 -> i32
3       fd_write        i32, i32, i32, i32 -> i32
4       fd_fdstat_get   i32, i32 -> i32
```
## Listing exports
```sh
$ dune exec -- wassail exports ./benchmarks/benchmarksgame/fasta.wat
5       _start   ->
```

## Listing section sizes
Section sizes are reported in bytes.
```sh
$ dune exec -- wassail sizes ./benchmarks/benchmarksgame/fankuchredux.wat
23      type
42      import
12      func
0       table
9       memory
14      global
25      export
0       start
0       elem
882     code
0       data
```

## Generating Call Graphs
To generate the call graph for a module `foo.dot`:

```sh
$ wassail callgraph benchmarks/benchmarksgame/fankuchredux.wat foo.dot
```

Output graph:

![DOT call graph](doc/callgraph.png)

## Generating Control-Flow Graphs
You can either generate a single CFG, for example to generate the CFG of function 4 from benchmark `fankuchredux.wat` into `foo.dot`:

```sh
$ wassail cfg benchmarks/benchmarksgame/fankuchredux.wat 4 foo.dot
```

Or you can generate all CFGs in a given directory:

```sh
$ wassail cfgs foo.wat out/
```

Example output:

![DOT CFG](doc/cfg.png)

## Other entry points

```sh
$ dune exec -- wassail help
Static analysis of WebAssembly

  wassail SUBCOMMAND

=== subcommands ===

  callgraph                  . Generate the call graph for the module from file
                               [in], outputs as DOT to file [out]
  callgraph-adjlist          . Generate the call graph for the module from file
                               [in], outputs in a textual representation to file
                               [out]
  cdg                        . Produce a CDG for a given function
  cfg                        . Generate a DOT file representing the CFG of
                               function [fidx] from the wasm file [in], in file
                               [out]
  cfg-adjlist                . Generate the CFG of function [fidx] from the wasm
                               file [in], in two text files: [out].adjlist and
                               [out].nodes
  cfgs                       . Generate DOT files representing the CFG of each
                               function defined in the wasm file [in], and
                               outputs them in the directory [out_dir]
  count                      . Count instructions in a file
  count-in-slice             . Count the number of instructions in a slice for a
                               specific slicing criterion
  dependencies               . Produce a PDG for a given function
  dump                       . Outputs the entire WebAssembly file without
                               modification
  evaluate-slicing           . Evaluate the slicer on a a benchmark
  exports                    . List functions exported by a WebAssembly module
  find-criterion             . Find the slicing criterion in a program. Prints
                               the address the string ORBS.
  find-indirect-calls        . Find call_indirect instructions and shows the
                               function in which they appear as well as their
                               label
  function-body              . Returns the body of a given function
  function-instruction-labels
                             . Returns the labels of instructions of a given
                               function
  functions                  . Returns the indices of functions of a WebAssembly
                               modules, along with their name if they have one
  gen-slice-specific         . Generate a slice for a specific slicing criterion
  generate                   . Generate a WebAssembly module from a single
                               function
  icfg                       . Generate a DOT file representing an
                               interprocedural CFG (ICFG), starting at function
                               [fidx] from the wasm file [in]
  imports                    . List functions imported by a WebAssembly module
  instructions               . List instructions used by a WebAssembly module,
                               and how many time each instruction appears
  load                       . Load a module and quits
  mem-exports                . Outputs the number of memories exported by this
                               module
  mem-imports                . Outputs the number of memories exported by this
                               module
  postdom                    . Visualize the post-dominator tree of a function
  reduced-callgraph          . Generate the call graph for the module from file
                               [in], only considering functions reachable from
                               [fidx], outputs as DOT to file [out]
  schedule                   . Generate the analysis schedule for the module
                               from file [in]
  sizes                      . Output the size (in bytes) of each section of a
                               WebAssembly module
  slice                      . Produce an executable program after slicing the
                               given function at the given slicing criterion
  spec-inference             . Annotate the CFG with the inferred variables
  spec-inference-inter       . Annotate the ICFG with the inferred variables
  taint-cfg                  . Generate a DOT file representing the
                               taint-annotated CFG of function [fid] from the
                               wasm file [in], in file [out]
  taint-from-exported-to-imported
                             . Detects unsafe flows from exported functions to
                               imported functions
  taint-from-sources-to-sinks
                             . Detects unsafe flows from a list of sources to a
                               list of defined sinks
  taint-inter                . Performs summary-based interprocedural taint
                               analysis of a set of functions in file [file].
                               [funs] is a list of comma-separated function ids,
                               e.g., to analyze function 1, then analyze both
                               function 2 and 3 as part of the same fixpoint
                               computation, [funs] is 1 2,3. The full schedule
                               for any file can be computed using the `schedule`
                               target.
  taint-inter-classical      . Perform classical interprocedural taint analysis
                               from a given entry point
  taint-intra                . Just like `intra`, but only performs the taint
                               analysis
  taintcall-cfg              . Performs a inter-procedural taintcall analysis
                               and displays the results for the given CFGs
  version                    . print version information
  help                       . explain a given subcommand (perhaps recursively)
```

# Contributing
Contributions are welcome! Wassail is licensed under the conditions of the GPLv3.0 license with a non-commercial usage clause.

## Running the tests

```sh
$ make test
```


## Using the web interface

You can access the web interface [here](https://acieroid.github.io/wassail/), or compile it for yourself as follows:

```sh
$ opam install js_of_ocaml js_of_ocaml-ppx
$ make js
$ open js/index.html
```
# References

The following publications use Wassail:

  - [Compositional Information Flow Analysis for WebAssembly Programs](http://soft.vub.ac.be/Publications/2020/vub-tr-soft-20-11.pdf), SCAM 2020. ([presentation video](https://www.youtube.com/watch?v=IX8swyZ4TPI)).
  - [Wassail: A WebAssembly Static Analysis Library](https://soft.vub.ac.be/Publications/2021/vub-tr-soft-21-04.pdf), ProWeb 2021.
  - [Static Stack-Preserving Intra-Procedural Slicing of WebAssembly Binaries](https://soft.vub.ac.be/Publications/2022/vub-tr-soft-22-04.pdf), ICSE 2022.
  - [Dynamic Slicing of WebAssembly Binaries](http://soft.vub.ac.be/Publications/2023/vub-tr-soft-23-11.pdf), ICSME 2023.
  - [An Empirical Evaluation of Static, Dynamic, and Hybrid Slicing of WebAssembly Binaries](https://qstievenart.gitlab.io/pdfs/jss2025.pdf), Journal of Systems and Software, 2025.
  
The following presentations might be useful as extra documentation:
  - [Building Static Analyses for WebAssembly Binaries with Wassail](https://docs.google.com/presentation/d/15J6EuRAz0WFD2TG4hBwAo_GLDhDXcy77ZAUGSzLj6yA/edit?usp=drive_link), PAW 2022.
  - [Wassail: A WebAssembly Static Analysis Library](https://docs.google.com/presentation/d/1ozzSdDTWQnMaSzX2o7wZpSC3m1cmzYfwYY6sy2kIqrE/edit?usp=drive_link), ProWeb 2021.
