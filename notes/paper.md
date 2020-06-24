# Motivation
From papers:
  - GraalVM-Wasm: "WebAssembly, sometimes abbreviated as Wasm, is a com-pact, portable, statically typed stack-based binary format.WebAssembly is easy to parse, specialise, and optimise com-pared to equivalent code fragments in JavaScript [8,19,20]"
  - GraalVM-Wasm: "Nonetheless, recentdevelopment efforts have included WebAssembly targetsfor standalone execution, such as on IoT, embedded, mobiledevices, and servers"


## Why WebAssemply
It is becoming a new big standard
## Why binary analysis
Because we can't have access to the source code when running wasm
It has been shown that wasm has many malicious applications (ref)
## Why Compositional
Wasm is used for libraries, mostly.
Hence, you can't rely on a whole-program analysis.
## Why taint
Again, for security applications, but this paper goes beyond that.
## What is specific about wasm
  - new language
  - mostly used as a library: whole-program analyses can't be used
# Contributions
  - A method to construct wasm CFGs that are fit for analyses, together with an annotation mechanism for the CFG that makes explicit how the vstack is used, reifiyng what is already part of the standard (for validation in their case).
  - On top of this, we design a relational abstract interpretation of wasm that satisfies the necessary requirements: being compositional through the use of summaries.
    We demonstrate the use of our approach to statically infer summaries that model explicit information flow control of wasm program. We evaluate it on a set of synthetic benchmarks for development of future analyses.
# Evaluation
The paper should be focused not on "big benchmarks", but rather on a small set of synthetic examples.
We can then provide this set of examples as a contribution.

Part of the evaluation is actually evaluating the "analysis framework" by applying it for taint analysis.

We could also show how many variables we generate, and show some optimizations to improve that (probably the complexity of a polyhedra depends on the number of vars).

After the "compilation/variable/vstack generation" phase, it would definitely be possible to spit out datalog facts, but probably that should be for another paper.
But it makes sense to present two tools: one that does the parsing/cfg construction/annotation (on which other tools could be built, including symbolic execution), and one that does the compositional analysis.

# Approach
The analysis really is just a typical dataflow analysis.
We can first construct the CFG, and from there on compute a number of pre-analyses.
These include:
  - vstack spec analysis
  - variables generation analysis
  - guards analysis (i.e., local.get 0; i32.const 1; i32.and; if; will infer that l0&1 = 0 holds on the true branch, does not on the false branch)
  - locals/globals
  - memory?
  - constraints

Some of them don't have to be on the CFG actually (e.g., vstack spec), but it's probably easier to express everything on the CFG
