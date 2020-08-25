# Next steps:
  - upon memory load/store, apply a backwards analysis or slicing?
  - how to model memory in the summary?
# Non-interference
Non-interference is defined for one input to the program.
In our case, we may need a notion of compositional non-interference?
But probably not: one input to a function always yield the same output (even with different confidential data) is a correct definition for us.

# Interprocedural Static Slicing of Binary Executables (SCAM2003)
If the slice for the statements creating observable behavior is free of statements that processconfidential data, the program is noninterferent [7, 21, 137].
7: M. Abadi, A. Banerjee, N. Heintze, and J. G. Riecke. A core calculus of dependency. InPOPL ’99: Proceedings of the 26th ACM SIGPLAN-SIGACT symposium on Principlesof programming languages, pages 147–160, New York, NY, USA, 1999. ACM
21: ]J.-F. Bergeretti and B. A. Carré.  Information-flow and data-flow analysis of while-programs.ACM Trans. Program. Lang. Syst., 7(1):37–61, 1985.
137: G. Snelting. Combining slicing and constraint solving for validation of measurementsoftware. InStatic Analysis, pages 332–348. Springer-Verlag London, UK, September1996


Basically, a Wasm program has mutliple entry points.
Besides these entry points, we can reason interprocedurally as needed.
# Intraprocedural Static Slicing of Binary Executables (Cifuentes & Fraboulet, 1997)
Could be useful to know which instructions are used to compute memory locations.

# Statically Analyzing Information FlowsAn Abstract Interpretation-based Hyperanalysis for Non-Interference (2019)
