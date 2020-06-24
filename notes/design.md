Wasm-spec parsed program
-of_wasm->
instructions
-cfg_builder->
CFG
-label->
LabeledCFG where instructions have unique labels
-annotate->
AnnotatedCFG (with vars, locals, globals spec, and memory)
in this CFG, instructions have to be annotated so we need a new type of instructions
some blocks also have to be annotated (e.g., entry/exit/merge)

how the annotation works:
  follow CFG edges, map a function over each basic block, which returns the annotated basic block?
instead of annotation, we can just use labels to store the extra info in global maps
-intra->
